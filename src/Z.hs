{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- A little language called Z.
--
-- Examples:
--
-- fn x y
--    fn p
--       do print p
--          print if = p
--                     42
--                   1
--                   0
--          - * x
--              y
--            p
--    5
--    6
--    42
--
-- →
--
-- 42
-- 1
-- Right (Right -12)
--

module Z where

import           Control.Applicative hiding (many,optional)
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Parsec hiding (runP,(<|>))
import           Text.Parsec.Combinator

--------------------------------------------------------------------------------
-- Main entry points

parseAndRunExpr :: String -> IO (Either RuntimeError (Either ParseError Value))
parseAndRunExpr str =
  runEval emptyEnv $
    bindBuiltins $ do
      source <- runParse "" (spaces *> expr <* spaces <* eof) str
      either (return . Left) (fmap Right . eval) source

parseAndRunBlock :: String -> IO (Either RuntimeError (Either ParseError Value))
parseAndRunBlock str =
  runEval emptyEnv $
    bindBuiltins $ do
      source <- runParse "" (block <* eof) str
      either (return . Left) (fmap Right . evalBlock) source


parseOnly :: Parse e -> String -> IO (Either RuntimeError (Either ParseError e))
parseOnly p str =
  runEval emptyEnv $ runParse "" (p <* eof) str

--------------------------------------------------------------------------------
-- Parser

type Parse a = ParsecT String () Z a

runParse :: SourceName -> Parse a -> String -> Z (Either ParseError a)
runParse name p s = runParserT p () name s

block :: Parse Block
block = do
  xs <- many1 (decl <* newline <* optional newline)
  return (Block xs)

decl :: Parse Decl
decl = try defun <|> try defmacro <|> try stmt

stmt = fmap Stmt expr

defun = offside many1 (string "defun") id $ \_ parts ->
  case parts of
    [ps,body] -> do
      params <- flatten ps
      case params of
        (name:params@(_:_)) -> return (Defun name (makeLambda params body))
        _ -> unexpected "malformed defun name/parameters"
    _ -> unexpected "malformed defun"

defmacro = offside many1 (string "defmacro") id $ \_ parts ->
  case parts of
    [ps,body] -> do
      params <- flatten ps
      case params of
        (name:params@(_:_)) -> return (Defun name (makeLambda params body))
        _ -> unexpected "malformed defun name/parameters"
    _ -> unexpected "malformed defun"

expr :: Parse Exp
expr = try ife <|> try doe <|> try fun <|> try app <|> try lit <|> var

lit :: Parse Exp
lit = fmap Value (integer <|> bool)

var :: Parse Exp
var = fmap Var varname

varname :: Parse String
varname = (++) <$> many1 sym <*> many (sym <|> digit)
  where sym = letter <|> oneOf "!@#$%^&*=-`~{}{}:';./,+_"

integer = fmap (Integer . read) (many1 digit)
bool = fmap (Bool . (=="true")) (string "true" <|> string "false")

app = offside many var (\(Var name) -> name) $ \op args ->
  return (foldl App op args)

fun :: Parse Exp
fun = offside many1 (string "fn") id $ \_ parts ->
  case parts of
    (ps:body:args) -> do
      params <- flatten ps
      let lambda = makeLambda params body
      case args of
        [] -> return lambda
        args -> return (foldl App lambda args)
    _ -> unexpected "malformed fn"

makeLambda params body =
  foldl (\body name -> Value (Fun emptyEnv name body))
        body
        (reverse params)

ife :: Parse Exp
ife = offside (count 2) (string "if") id $ \_ parts ->
  case parts of
    [cond,cons,ant] -> return (If cond cons ant)
    _ -> unexpected "malformed if"

doe :: Parse Exp
doe = offside many (string "do") id $ \_ stmts ->
  case stmts of
    [stmt] -> return stmt
    stmts  -> return (foldr1 Seq stmts)

flatten :: Exp -> Parse [Var]
flatten = go where
  go (App x y) = (++) <$> go x <*> go y
  go (Var x) = return [x]
  go _       = unexpected "incorrect parameter format"

offside :: (Parse Exp -> Parse [Exp])
        -> Parse a
        -> (a -> String)
        -> (a -> [Exp] -> Parse b)
        -> Parse b
offside numbered p getName cont = do
  pos <- getPosition
  op <- p
  char ' '
  arg <- expr
  let side = sourceColumn pos + length (getName op)
  args <- numbered (try (do newline; string (replicate side ' '); expr))
  cont op (arg:args)

--------------------------------------------------------------------------------
-- Evaluator

runEval :: Env -> Z a -> IO (Either RuntimeError a)
runEval env z = runReaderT (runErrorT (runZ z)) env

emptyEnv :: Map Var Value
emptyEnv = M.empty

evalBlock (Block decls) = go decls where
  go [] = return Unit
  go (d:ds) =
    case d of
      Defun name fun -> evalDefun name fun (go ds)
      Defmacro name fun -> evalDefmacro name fun (go ds)
      Stmt e -> do eval e; go ds

evalDefun name fun cont = do
  f <- eval fun
  bind name f cont

evalDefmacro name fun cont = do
  f <- eval fun
  return Unit
  cont

eval :: Exp -> Z Value
eval ex = do
  go ex
  where
    go (Var var) = do
      e <- resolve var
      eval (Value e)
    go (Value (Fun _ p b)) = do
      env <- ask
      return (Fun env p b)
    go (Value v) = return v
    go (App ope arge) = apply ope arge
    go (Seq a b) = do
      _ <- eval a
      eval b
    go (If c t e) = do
      cond <- eval c
      case cond of
        Bool b -> eval $ if b then t else e
        _ -> throwError NotABool

apply :: Exp -> Exp -> Z Value
apply ope arge = do
  op <- eval ope
  case op of
    BuiltIn f -> do
      arg <- eval arge
      f arg
    Fun env param body -> do
      local (const env) $ do
        arg <- eval arge
        local (M.insert param arg)
              (eval body)
    _ -> throwError (NotAFunction ope arge)

resolve :: Var -> Z Value
resolve key = do
  env <- ask
  case M.lookup key env of
    Just v -> return v
    _ -> throwError (NotInScope key)

bind :: Var -> Value -> Z a -> Z a
bind name value m = local (M.insert name value) m

--------------------------------------------------------------------------------
-- Built-ins

bindBuiltins :: Z a -> Z a
bindBuiltins m = go builtins where
  go ((name,var):bs) = bind name var (go bs)
  go [] = m

builtins = [("show",show')
           ,("print",print')
           ,("+",arith (+))
           ,("-",arith (-))
           ,("/",arith div)
           ,("*",arith (*))
           ,("=",logic (==))
           ,("cons",bi Cons)
           ,("unit",Unit)]

  where arith = biInt Integer
        logic = biInt Bool

show' = BuiltIn $ \v ->
  return (String (T.pack (show v)))

print' = BuiltIn $ \value -> unit $ ffi $
  case value of
    String text -> T.putStrLn text
    Integer i   -> print i
    Bool b      -> print b
    Unit        -> print ()
    Cons a b    -> print [a,b]
    Fun _ _ _   -> putStrLn "<function>"
    BuiltIn _   -> putStrLn "<built-in>"

  where unit m = m >> return Unit
        ffi = liftIO

biInt cons op = BuiltIn $ \value ->
  case value of
    Integer x -> return $ BuiltIn $ \value ->
      case value of
        Integer y -> return $ cons (x `op` y)

bi cons = BuiltIn $ \x -> return $ BuiltIn $ \y ->
  return (cons x y)

--------------------------------------------------------------------------------
-- Types

newtype Z a = Z { runZ :: ErrorT RuntimeError (ReaderT Env IO) a }
  deriving (Monad
           ,Functor
           ,MonadReader Env
           ,MonadError RuntimeError
           ,MonadIO
           ,Alternative
           ,Applicative)

data RuntimeError
  = NotInScope Name
  | NotAFunction Exp Exp
  | NotABool
    deriving Show
instance Error RuntimeError

data Block = Block [Decl]
  deriving Show

data Decl
  = Defun Var Exp
  | Defmacro Var Exp
  | Stmt Exp
    deriving Show

data Exp
  = Var Var
  | Value Value
  | App Exp Exp
  | Seq Exp Exp
  | If Exp Exp Exp
  deriving Show

data Value
  = String Text
  | Integer Integer
  | Bool Bool
  | Unit
  | Cons Value Value
  | Fun Env Var Exp
  | BuiltIn (Value -> Z Value)

instance Show Value where
  show value =
    case value of
      String text -> show (T.unpack text)
      Integer i   -> show i
      Bool b      -> show b
      Unit        -> "<unit>"
      Fun _ x e   -> "(fn " ++ x ++ " " ++ show e ++ ")"
      BuiltIn _   -> "<built-in>"
      Cons a b    -> "(cons " ++ show a ++ " " ++ show b ++ ")"

type Var = String
type Env = Map Var Value
type Name = String
