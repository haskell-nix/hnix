{-# LANGUAGE CPP #-}

module Nix.Parser (parseNixFile, Result(..)) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Foldable
import           Data.List (foldl1')
import qualified Data.Map as Map
import           Data.Text hiding (head, map, foldl1')
import qualified Data.Text as T
import           Nix.Types
import           Nix.Internal
import           Nix.Parser.Library
import           Prelude hiding (elem)
import qualified Prelude

nixApp :: Parser NExpr
nixApp = go <$>
    someTill (whiteSpace *> nixExpr True)
             (try (lookAhead (() <$ oneOf "=,;])}" <|> eof)))
  where
    go []     = error "some has failed us"
    go [x]    = x
    go (f:xs) = Fix (NApp f (go xs))

nixExpr :: Bool -> Parser NExpr
nixExpr = buildExpressionParser table . nixTerm
  where
    table =
        [ [ prefix "-"  NNeg ]
        , [ binary "++" NConcat AssocRight ]
        , [ binary "*"  NMult   AssocLeft,
            binary "/"  NDiv    AssocLeft ]
        , [ binary "+"  NPlus   AssocLeft,
            binary "-"  NMinus  AssocLeft ]
        ]

    binary  name fun = Infix ((\x y -> Fix (NOper (fun x y))) <$ symbol name)
    prefix  name fun = Prefix (Fix . NOper . fun <$ symbol name)
    -- postfix name fun = Postfix (Fix . NOper . fun <$ symbol name)

nixTerm :: Bool -> Parser NExpr
nixTerm allowLambdas = choice
    [ nixInt
    , nixBool
    , nixNull
    , nixParens
    , nixList
    , nixPath
    , setLambdaStringOrSym allowLambdas
    ]

nixInt :: Parser NExpr
nixInt = mkInt <$> decimal <?> "integer"

nixBool :: Parser NExpr
nixBool = try (mkBool . (== "true") <$> string "true") <?> "bool"

nixNull :: Parser NExpr
nixNull = try (mkNull <$ string "null") <?> "null"

nixParens :: Parser NExpr
nixParens = parens nixApp <?> "parens"

nixList :: Parser NExpr
nixList = brackets (Fix . NList <$> many (nixTerm False)) <?> "list"

nixPath :: Parser NExpr
nixPath = try $ fmap mkPath $ mfilter ('/' `elem`) $ some (oneOf "A-Za-z_0-9.:/")

-- | This is a bit tricky because we don't know whether we're looking at a set
--   or a lambda until we've looked ahead a bit.  And then it may be neither,
--   in which case we fall back to expected a plain string or identifier.
setLambdaStringOrSym :: Bool -> Parser NExpr
setLambdaStringOrSym allowLambdas = do
    trace "setLambdaStringOrSym" $ return ()
    x <- try (lookAhead symName)
        <|> try (lookAhead (singleton <$> char '{'))
        <|> return ""
    if x == "rec" || x == "{"
        then setOrArgs
        else do
            trace "might still have a lambda" $ return ()
            y <- try (lookAhead (True <$ (symName *> whiteSpace *> symbolic ':')))
                <|> return False
            trace ("results are = " ++ show y) $ return ()
            if y
                then if allowLambdas
                    then setOrArgs
                    else error "Unexpected lambda"
                else keyName <?> "string"

symName :: Parser Text
symName = pack <$> ((:) <$> letter <*> many (alphaNum <|> char '.'))

stringish :: Parser NExpr
stringish =  (char '"' *> (merge <$> manyTill stringChar (char '"')))
         <|> (char '$' *> braces nixApp)
  where
    merge = foldl1' (\x y -> Fix (NOper (NConcat x y)))

    stringChar =  char '\\' *> (mkStr . singleton <$> anyChar)
              <|> (string "${" *> nixApp <* char '}')
              <|> (mkStr . pack <$> many (noneOf "\"\\"))

argExpr :: Parser NExpr
argExpr =  (Fix . NArgSet . Map.fromList <$> argList)
       <|> ((mkSym <$> symName) <?> "argname")
  where
    argList =  braces ((argName <* whiteSpace) `sepBy` symbolic ',')
           <?> "arglist"

    argName = (,) <$> (symName <* whiteSpace)
                  <*> optional (symbolic '?' *> nixExpr False)

nvPair :: Parser (NExpr, NExpr)
nvPair = (,) <$> keyName <*> (symbolic '=' *> nixApp)

keyName :: Parser NExpr
keyName = (stringish <|> (mkSym <$> symName)) <* whiteSpace

setOrArgs :: Parser NExpr
setOrArgs = do
    trace "setOrArgs" $ return ()
    sawRec <- try (symbol "rec" *> pure True) <|> pure False
    trace ("Do we have sawRec: " ++ show sawRec) $ return ()
    haveSet <-
        if sawRec
        then return True
        else try (lookAhead lookaheadForSet)
    trace ("Do we have a set: " ++ show haveSet) $ return ()
    if haveSet
        then braces (Fix . NSet sawRec <$> nvPair `endBy` symbolic ';')
            <?> "set"
        else do
            trace "parsing arguments" $ return ()
            args <- argExpr <?> "arguments"
            trace ("args: " ++ show args) $ return ()
            symbolic ':' *> ((Fix .) . NAbs <$> pure args <*> nixApp)
                <|> pure args

lookaheadForSet :: Parser Bool
lookaheadForSet = do
    trace "lookaheadForSet" $ return ()
    x <- (symbolic '{' *> return True) <|> return False
    if not x then return x else do
        y <- (keyName *> return True) <|> return False
        if not y then return y else do
            trace "still in lookaheadForSet" $ return ()
            (symbolic '=' *> return True) <|> return False

parseNixFile :: MonadIO m => FilePath -> m (Result NExpr)
parseNixFile = parseFromFileEx nixApp

{-

Grammar of the Nix language (LL(n)).  I conditionalize terms in the grammar
with a predicate suffix in square brackets.  If the predicate fails, we
back-track.  WS is used to indicate where arbitrary whitespace is allowed.

top ::= app

Applied expressions, or "expr expr", express function application.  Since they
do not mean this within lists, we must call it out as a separate grammar rule so
that we can make clear when it is allowed.

app ::= expr WS+ app | (epsilon)

expr ::= atom
       | '(' app ')'
       | '[' list_members ']'
       | "rec"[opt] '{' set_members[one kv_pair exists] '}'
       | argspec ':' app

atom ::= INTEGER
       | "true" | "false"
       | "null"
       | CHAR(0-9A-Za-z_./)+[elem '/']
       | '"' string '"'

Strings are a bit special in that not only do they observe escaping conventions,
but they allow for interpolation of arbitrary Nix expressions.  This means
they form a sub-grammar, so we assume a lexical context switch here.

string ::= string_elem string | (epsilon)

string_elem ::= '\' ANYCHAR | subexpr | ANYCHAR+

subexpr ::= "${" WS* app "}"

list_members ::= expr WS+ list_members | (epsilon)

set_members ::= kv_pair WS* ';' WS* set_members | (epsilon)

kv_pair ::= stringish WS* '=' WS* app

stringish ::= string | CHAR(0-9A-Za-z_.)+ | subexpr

argspec ::= CHAR(0-9A-Za-z_)+ | '{' arg_list '}'

arg_list ::= arg_specifier | arg_specifier ',' arg_list

arg_specifier ::= CHAR(0-9A-Za-z_)+ default_value[opt]

default_value ::= '?' app

-}
