{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module STLCC.Parse where

import qualified Control.Exception          as Except
import           Control.Monad.Reader
import           Data.Char
import           Data.Functor.Identity
import           Data.List                  as List (foldl')
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Set                   as Set
import           Data.Text
import           Data.Void
import qualified Debug.Trace                as T
import           STLCC.AST.Unchecked
import           STLCC.Util.HReader
import           STLCC.Util.Nat
import           STLCC.Util.Vec
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- [Note: Basic Expression Grammar]
--
-- The basic grammar for expressions looks something like this:
--
-- expr     ::= lamda | app | term
-- lambda   ::= binder expr
-- binder   ::= '\' var '.' | 'λ' var '.'
-- app      ::= term app_tail
-- app_tail ::= lambda | term app_tail | term
-- term     ::= var | literal | '(' expr ')'
--
-- The "var" non-terminal denotes any legal variable or top level
-- definition name.
--
-- Note that the parser won't parse "app" in this way exactly, as we
-- want applications to be left associative. Thus we instead treat
-- "app" more like this in practice:
--
-- app ::= term lambda | term term [term ...] [lambda]
--
-- That is a "term" followed by a "lambda" or a "term" followed by a
-- "term", optionally followed by 1 or more "terms", optionally
-- followed by a lambda. In the parser, this second alternative is
-- really treated as a term followed by a nonempty list of terms,
-- followed by an optional lambda.


type Parser n x = ParsecT Void Text (Reader (Vec Text n)) x

----------------------------------------------------------------------
---                       Special Combinators                      ---
----------------------------------------------------------------------

-- | Introduce a new variable into the current scope.
bindVar :: Text -> Parser ('S n) x -> Parser n x
bindVar var_name p = hlocal (var_name :::) p

-- | Try to lookup the de Bruijn index of a variable in the current
-- scope.
--
-- Note: There may be several variables in scope with the same
-- name. This function will return the index of the most recently
-- introduced one. The others are effectively shadowed.
lookupVar :: MonadReader (Vec Text n) m => Text -> m (Maybe (Fin n))
lookupVar var_name = asks (findInVec var_name)


lexeme :: Parser n x -> Parser n x
lexeme = L.lexeme spaceConsumer
  where spaceConsumer = L.space space1 lineComm blockComm
        lineComm = L.skipLineComment "-- "
        -- No block comments
        blockComm = fail "block comments not supported"

elabel :: String -> ErrorItem t
elabel s = Label (NonEmpty.fromList s)

----------------------------------------------------------------------
---                             Basics                             ---
----------------------------------------------------------------------

--- Variables --------------------------------------------------------


variableName :: Parser n Text
variableName = label "variable name" . lexeme $ do
  c <- satisfy okPrefix
  cs <- takeWhileP Nothing isAlphaNum
  pure (cons c cs)

  where okPrefix x = isLetter x && x /= '\\' && x /= 'λ'


variable :: Parser n (UExp n)
variable = label "variable or top level name" $ do
  n <- variableName
  v <- lookupVar n
  case v of
    Just fn -> pure (UVar fn)
    Nothing -> pure (UGlobal n)

--- Literals ---------------------------------------------------------

boolLiteral :: Parser n (UExp n)
boolLiteral = label "bool literal" . lexeme $
  try trueLit <|> falseLit
  where trueLit = do
          string "True"
          pure (UBoolE True)
        falseLit = do
          string "False"
          pure (UBoolE False)

literal :: Parser n (UExp n)
literal = label "literal" $ boolLiteral


--- Terms ------------------------------------------------------------


term :: Parser n (UExp n)
term = label "term" $
            try literal
        <|> try variable
        <|> parenExpression

parenExpression :: Parser n (UExp n)
parenExpression =
  label "parenthesised expression" $
  between (try lpar) rpar expression

  where lpar = lexeme (char '(')
        rpar = lexeme (char ')')

--- Expressions ------------------------------------------------------


expression :: Parser n (UExp n)
expression = label "expression" $
      lambda
  <|> application
  <|> term



closedExpression :: Parser 'Z (UExp 'Z)
closedExpression = expression


--- Lambda Abstraction -----------------------------------------------

lambdaBinder :: Parser n Text
lambdaBinder = do
  try sob                <?> "lambda token"
  vv <- try variableName <?> "name to bind"
  -- TODO: Parse type annot, if present
  try eob                <?> "end of binding marker"
  pure vv

  where eob = lexeme (char '.')
        sob = lexeme (string "\\" <|> string "λ")
lambda :: Parser n (UExp n)
lambda = label "lambda" $ do
  vv <- lambdaBinder <?> "lambda binder"
  -- TODO: Report shadowing: lookupVar vv
  expr <- bindVar vv expression       <?> "lambda body"
  pure (ULam vv Nothing expr)


--- Application ------------------------------------------------------


application :: Parser n (UExp n)
application =  label "application" $ do
  a <- try term
  b <- appTail
  pure (List.foldl' UApp a b)

  where appTail =  do
          ts <- try (many term)
          l <- optional lambda
          case l of
            Just ll -> pure (ts ++ [ll])
            Nothing -> pure ts



----------------------------------------------------------------------
---                             Parser                             ---
----------------------------------------------------------------------




parseClosedExpr :: Text -> IO (Maybe (UExp 'Z))
parseClosedExpr s =
  case runEnv (runParserT (closedExpression <* eof) "(input)" s) of
    Left x  -> putStr (errorBundlePretty x) >> pure Nothing
    Right r -> pure (Just r)

  where runEnv = runIdentity . flip runReaderT VNil

mkpretty :: Maybe (UExp 'Z) -> IO ()
mkpretty (Just x) = putStrLn (pretty x)
mkpretty Nothing  = pure ()
