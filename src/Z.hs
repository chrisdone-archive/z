{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- A little language called Z.

module Z where

import           Control.Applicative hiding (many,optional)
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.Dynamic
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Text.Parsec hiding (runP,(<|>))
import           Text.Parsec.Combinator
import           Text.Regex

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
  runEval emptyEnv $
    bindBuiltins $ do
      runParse "" (p <* eof) str

--------------------------------------------------------------------------------
-- Parser

type Parse a = ParsecT String () Z a

runParse :: SourceName -> Parse a -> String -> Z (Either ParseError a)
runParse name p s = runParserT p () name s

block :: Parse Block
block = fmap Block (go []) where
  go acc = do
    mdecl <- may (decl <* many newline)
    case mdecl of
      Nothing -> return acc
      Just decl ->
        case decl of
          Defun name fun -> do
            f <- lift (eval fun)
            bind name f $ go (acc ++ [decl])
          Defmacro name fun -> do
            f <- lift (eval fun)
            bind name f (go acc)
          x -> go (acc ++ [x])

may x = fmap Just (try x) <|> pure Nothing

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
        (ValueSym name:params@(_:_)) -> do
          let macro = (Defmacro (MacroSym name) (makeLambda params body))
--          debug $ show macro
          return macro
        _ -> unexpected "malformed defun name/parameters"
    _ -> unexpected "malformed defun"

expr :: Parse Exp
expr =
  -- Keywords
  try ife <|> try doe <|> try fun <|>
  -- Macro
  try macro <|>
  -- Everything else
  try lit <|> try app <|> var

macro = do
  Var name <- lookAhead macroVar
  result <- lift (resolveMaybe name)
  case result of
    Nothing -> unexpected "not a macro, don't bother"
    Just{} -> do
      _ <- var
      pos <- getPosition
      input <- getMacroInput pos
      posAfter <- getPosition
      -- debug $ "Position before consuming macro's input: " ++ show pos
      -- debug $ "Position AFTER consuming macro's input: " ++ show posAfter
      debug $ "Macro:\n" ++ symToString name ++ indentBefore (length (symToString name)+1) input
      value <- lift (eval (applyMacro name input))
      case value of
        Quote ex -> return ex
        String (addnewline -> out) -> do
          debug $ "→\n" ++ out ++ "\n—"
          result <- lift (runParse "" (expr <* many newline <* eof) out)
          case result of
            Left balls -> error $ "Re-parsing macro's output, got: \n" ++
                                  out ++ "\nbut:\n" ++
                                  show balls
            Right ok -> return ok
        _ -> lift (throwError (ImproperMacroReturn value))

  where addnewline = unlines . lines

indent i = intercalate "\n" . map (\l -> replicate i ' ' ++ l) . lines
indentBefore i s =
  intercalate "\n" ((" " ++ line) : map indent ls)
   where indent line = replicate (fromIntegral i) ' ' ++ line
         ls = drop 1 (lines s)
         line = concat (take 1 (lines s))

debug :: String -> Parse ()
debug _ = return ()
-- debug s = lift . liftIO . putStrLn$s

applyMacro name input =
  App (Var name) (Value (String input))

getMacroInput pos = do
  space
  currentLine <- manyTill anyChar newline
  let side = sourceColumn pos
  rest <- many (try (string (replicate side ' ') *> manyTill anyChar newline))
  let input = intercalate "\n" (currentLine : rest)
  return input

lit :: Parse Exp
lit = fmap Value (str <|> integer <|> bool)

var :: Parse Exp
var = fmap (Var . ValueSym) varname

macroVar = fmap (Var . MacroSym) varname

varname :: Parse String
varname = go where
  go = do
    c <- fmap Just (lookAhead anyChar) <|> pure Nothing
    case c of
      Nothing  -> unexpected "empty varname"
      Just ' ' -> return ""
      Just '\n' -> return ""
      Just c   -> do anyChar
                     cs <- fmap Just go <|> pure Nothing
                     case cs of
                       Nothing -> return [c]
                       Just cs -> return (c : cs)

str = fmap (String . read . quote . concat)
           (char '"' *> manyTill (try escaped <|> ch) (char '"'))
   where ch = fmap return anyChar
         escaped = string "\\\""
         quote x = "\"" ++ x ++ "\""

integer = fmap (Integer . read) (many1 digit)
bool = fmap (Bool . (=="true")) (string "true" <|> string "false")

app = offside many var (\(Var name) -> symToString name) $ \op args ->
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

makeLambda :: [Sym] -> Exp -> Exp
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

flatten :: Exp -> Parse [Sym]
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
  args <- numbered (try (do optional newline; string (replicate side ' '); expr))
  cont op (arg:args)

symToString (MacroSym s) = s
symToString (ValueSym s) = s

--------------------------------------------------------------------------------
-- Evaluator

runEval :: Env -> Z a -> IO (Either RuntimeError a)
runEval env z = runReaderT (runErrorT (runZ z)) env

emptyEnv :: Map Sym Value
emptyEnv = M.empty

evalBlock (Block decls) = go decls where
  go [] = return Unit
  go (d:ds) =
    case d of
      Defun name fun -> evalDefun name fun (go ds)
      Stmt e -> do eval e; go ds

evalDefun name fun cont = do
  f <- eval fun
  bind name f cont

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
      arg <- eval arge
      local (const env) $ do
        local (M.insert param arg)
              (eval body)
    _ -> throwError (NotAFunction ope arge)

resolve :: Sym -> Z Value
resolve key =
  resolveMaybe key
    >>= maybe (throwError (NotInScope key))
              return

resolveMaybe :: Sym -> Z (Maybe Value)
resolveMaybe key = do
  env <- ask
  return (M.lookup key env)

bind :: MonadReader Env m => Sym -> Value -> m a -> m a
bind name value m = local (M.insert name value) m

--------------------------------------------------------------------------------
-- Built-ins

bindBuiltins :: Z a -> Z a
bindBuiltins m = go builtins where
  go ((name,var):bs) = bind (ValueSym name) var (go bs)
  go [] = m

builtins = [
           -- Built-in macro-writing functions.
            ("z:string",string')
           ,("z:blocks",blocks')
           ,("z:indent",indent')
           ,("z:indent-before",indentBefore')
           -- Standard functions.
           ,("lines",lines')
           ,("show",show')
           ,("print",print')
           ,("+",arith (+))
           ,("-",arith (-))
           ,("/",arith div)
           ,("*",arith (*))
           ,("=",logic (==))
           ,("cons",bi Cons)
           ,("car",car')
           ,("cdr",cdr')
           ,("unit?",nil')
           ,("unit",Unit)
           ,("++",concat')
           -- Regex functions.
           ,("regex:new",regex_new)
           ,("regex:match",regex_match)
           ]

  where arith = biInt Integer
        logic = biInt Bool

deriving instance Typeable Regex
regex_new = BuiltIn $ \(String s) ->
  return $ Foreign $ toDyn $ mkRegex s
regex_match = BuiltIn $ \(Foreign regex) ->
  return $ BuiltIn $ \(String s) ->
    case fromDynamic regex of
      Nothing -> error "not a regex!"
      Just regex ->
        return $ maybe Unit
                       (list . map String)
                       (matchRegex regex s)

nil' = BuiltIn $ \i ->
  return $ case i of
    Unit -> Bool True
    _ -> Bool False

indent' = BuiltIn $ \(Integer n) ->
  return $ BuiltIn $ \(String a) ->
    case lines a of
      [] -> error "nothing to indent"
      lines -> return $ String $ intercalate "\n" (map indent lines)
        where indent line = replicate (fromIntegral n) ' ' ++ line

indentBefore' = BuiltIn $ \(Integer n) ->
  return $ BuiltIn $ \(String a) ->
    case lines a of
      [] -> error "nothing to indent"
      (line:lines) -> return $ String $ intercalate "\n" ((" " ++ line) : map indent lines)
        where indent line = replicate (fromIntegral n) ' ' ++ line

string' = BuiltIn $ \(String a) ->
  return $ (String (show a))

concat' = BuiltIn $ \(String a) ->
  return $ BuiltIn $ \(String b) ->
    return (String (a ++ b))

car' = BuiltIn $ \(Cons a b) -> return a
cdr' = BuiltIn $ \(Cons a b) -> return b

list :: [Value] -> Value
list = foldr Cons Unit

blocks' = BuiltIn $ \(String s) -> return (list (map String (go (lines s)))) where
  go [] = []
  go (line:lines) =
    case span (isPrefixOf " ") lines of
      (these,next) -> (intercalate "\n" (line : these)) : go next

lines' =
  BuiltIn $ \(String str) ->
    return (list (map String (lines str)))

if' =
  BuiltIn $ \(Quote cond) -> do
    return $ BuiltIn $ \(Quote con) -> do
      return $ BuiltIn $ \(Quote ant) -> do
        return (Quote (If cond con ant))

show' = BuiltIn $ \v ->
  return (String (show v))

print' = BuiltIn $ \value -> unit $ ffi $
  case value of
    String text -> putStrLn text
    Integer i   -> print i
    Bool b      -> print b
    Unit        -> print ()
    Cons a b    -> print [a,b]
    Fun _ _ _   -> putStrLn "<function>"
    BuiltIn _   -> putStrLn "<built-in>"
    Quote e     -> print e

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
  = NotInScope Sym
  | NotAFunction Exp Exp
  | NotABool
  | ImproperMacroReturn Value
    deriving Show
instance Error RuntimeError

data Block = Block [Decl]
  deriving Show

data Decl
  = Defun Sym Exp
  | Defmacro Sym Exp
  | Stmt Exp
    deriving Show

data Exp
  = Var Sym
  | Value Value
  | App Exp Exp
  | Seq Exp Exp
  | If Exp Exp Exp
  deriving Show

data Value
  = String String
  | Integer Integer
  | Bool Bool
  | Unit
  | Cons Value Value
  | Fun Env Sym Exp
  | BuiltIn (Value -> Z Value)
  | Quote Exp
  | Foreign Dynamic

instance Show Value where
  show value =
    case value of
      String text      -> show text
      Integer i        -> show i
      Bool b           -> show b
      Unit             -> "<unit>"
      Fun _ x e        -> "(fn " ++ symToString x ++ " " ++ show e ++ ")"
      BuiltIn _        -> "<built-in>"
      Cons a b         -> "(cons " ++ show a ++ " " ++ show b ++ ")"
      Quote e          -> "(quote " ++ show e ++ ")"

type Env = Map Sym Value
type Name = String

data Sym = MacroSym String
         | ValueSym String
  deriving (Show,Eq,Ord)
