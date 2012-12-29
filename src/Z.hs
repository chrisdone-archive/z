{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- A little language called Z.
--
-- Examples:
--
-- λ> parseAndRun "(cons 1 (cons 2 unit))"
-- Right (Right (cons 1 (cons 2 <unit>)))
-- λ> parseAndRun "((fn x y (fn p (do (print p) (print (if (= p 42) 1 0)) (- (* x y) p)))) 5 6 42)"
-- 42
-- 1
-- Right (Right -12)
--

module Z where

import           Control.Applicative hiding ((<|>),many)
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Parsec hiding (runP)
import           Text.Parsec.Combinator

--------------------------------------------------------------------------------
-- Main entry points

parseAndRun :: String -> IO (Either RuntimeError (Either ParseError Value))
parseAndRun str =
  runEval emptyEnv $
    bindBuiltins $ do
      source <- runParse "" (spaces *> expr <* spaces <* eof) str
      either (return . Left) (fmap Right . eval) source

parseOnly :: Parse e -> String -> IO (Either RuntimeError (Either ParseError e))
parseOnly p str =
  runEval emptyEnv $ runParse "" (p <* eof) str

--------------------------------------------------------------------------------
-- Parser

type Parse a = ParsecT String () Z a

runParse :: SourceName -> Parse a -> String -> Z (Either ParseError a)
runParse name p s = runParserT p () name s

expr :: Parse Exp
expr = try fun <|> try doe <|> try ife <|> try app <|> lit <|> var

ife :: Parse Exp
ife = parens $ do
  string "if "
  cond <- expr
  string " "
  then' <- expr
  string " "
  else' <- expr
  return (If cond then' else')

doe :: Parse Exp
doe = parens $ do
  string "do"
  es <- many1 (string " " *> expr)
  case es of
    [e] -> return e
    es  -> return (foldr1 Seq es)

fun :: Parse Exp
fun = parens $ do
  string "fn"
  params <- lookAhead (many1 (string " " *> expr))
  vs <- count (length params - 1) (string " " *> varname)
  string " "
  exp <- expr
  return (foldl (\body name -> Value (Fun emptyEnv name body)) exp vs)

lit :: Parse Exp
lit = fmap Value (integer <|> bool)

var :: Parse Exp
var = fmap Var varname

varname :: Parse String
varname = (++) <$> many1 sym <*> many sym
  where sym = letter <|> digit <|> oneOf "!@#$%^&*=-`~{}{}:';./,+_"

app :: Parse Exp
app = parens $ do
  op <- expr
  string " "
  args <- sepBy1 expr (string " ")
  return (foldl App op args)

integer = fmap (Integer . read) (many1 digit)
bool = fmap (Bool . (=="true")) (string "true" <|> string "false")

parens :: Parse a -> Parse a
parens = between (char '(') (char ')')

--------------------------------------------------------------------------------
-- Evaluator

runEval :: Env -> Z a -> IO (Either RuntimeError a)
runEval env z = runReaderT (runErrorT (runZ z)) env

emptyEnv :: Map Var Value
emptyEnv = M.empty

eval :: Exp -> Z Value
eval (Var var) = do
  e <- resolve var
  return e
eval (Value (Fun _ p b)) = do
  env <- ask
  return (Fun env p b)
eval (Value v) = return v
eval (App ope arge) = apply ope arge
eval (Seq a b) = do
  _ <- eval a
  eval b
eval (If c t e) = do
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

bind :: Var -> Value -> Z a -> Z a
bind name value m = local (M.insert name value) m

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
           ,MonadIO)

data RuntimeError
  = NotInScope Name
  | NotAFunction Exp Exp
  | NotABool
    deriving Show
instance Error RuntimeError

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
