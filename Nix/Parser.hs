{-# LANGUAGE CPP #-}

module Nix.Parser (parseNixFile, Result(..)) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.List (foldl1')
import qualified Data.Map as Map
import           Data.Text hiding (head, map, foldl1')
import           Nix.Types
import           Nix.Internal
import           Nix.Parser.Library
import           Prelude hiding (elem)

-- | The lexer for this parser is defined in 'Nix.Parser.Library'.
nixApp :: Parser NExpr
nixApp = go <$> someTill (whiteSpace *> nixExpr True) (try (lookAhead stop))
  where
    go []     = error "some has failed us"
    go [x]    = x
    go (f:xs) = Fix (NApp f (go xs))

    stop = () <$ oneOf "=,;])}" <|> reservedWords <|> eof

nixExpr :: Bool -> Parser NExpr
nixExpr = buildExpressionParser table . nixTerm
  where
    table =
        [ [ binary "."  NAttr    AssocNone ]
        , [ prefix "-"  NNeg ]
        -- , [ prefix "~"  NSubpath ]  -- deprecated
        , [ binary "?"  NHasAttr AssocNone ]
        , [ binary "++" NConcat  AssocRight ]
        , [ binary "*"  NMult    AssocLeft, binary "/"  NDiv    AssocLeft ]
        , [ binary "+"  NPlus    AssocLeft, binary "-"  NMinus  AssocLeft ]
        , [ prefix "!"  NNot ]
        , [ binary "//" NUpdate  AssocRight ]
        , [ binary "<"  NLt      AssocLeft, binary ">"  NGt     AssocLeft
          , binary "<=" NLte     AssocLeft, binary ">=" NGte    AssocLeft ]
        , [ binary "==" NEq      AssocNone, binary "!=" NNEq    AssocNone ]
        , [ binary "&&" NAnd     AssocLeft ]
        , [ binary "||" NOr      AssocLeft ]
        , [ binary "->" NImpl    AssocNone ]
        ]

    binary  name fun = Infix ((\x y -> Fix (NOper (fun x y))) <$ symbol name)
    prefix  name fun = Prefix (Fix . NOper . fun <$ symbol name)
    -- postfix name fun = Postfix (Fix . NOper . fun <$ symbol name)

nixTerm :: Bool -> Parser NExpr
nixTerm allowLambdas = choice
    [ nixInt
    , nixParens
    , nixList
    , nixLet
    , nixIf
    , nixBool
    , nixNull
    , nixPath                   -- can be expensive due to back-tracking
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

nixLet :: Parser NExpr
nixLet =  fmap Fix $ NLet
      <$> (reserved "let" *> nixBinders)
      <*> (whiteSpace *> reserved "in" *> nixApp)

nixIf :: Parser NExpr
nixIf =  fmap Fix $ NIf
     <$> (reserved "if" *> nixApp)
     <*> (whiteSpace *> reserved "then" *> nixApp)
     <*> (whiteSpace *> reserved "else" *> nixApp)

-- | This is a bit tricky because we don't know whether we're looking at a set
--   or a lambda until we've looked ahead a bit.  And then it may be neither,
--   in which case we fall back to expected a plain string or identifier.
setLambdaStringOrSym :: Bool -> Parser NExpr
setLambdaStringOrSym allowLambdas = do
    trace "setLambdaStringOrSym" $ return ()
    isSetOrArgs <- try (lookAhead (reserved "rec") *> pure True)
        <|> try (lookAhead (singleton <$> char '{') *> pure True)
        <|> pure False
    if isSetOrArgs
        then setOrArgs
        else do
            trace "might still have a lambda" $ return ()
            y <- try (lookAhead (True <$ (identifier *> whiteSpace
                                          *> symbolic ':')))
                <|> return False
            trace ("results are = " ++ show y) $ return ()
            if y
                then if allowLambdas
                    then setOrArgs
                    else error "Unexpected lambda"
                else keyName <?> "string"

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
       <|> ((mkSym <$> identifier) <?> "argname")
  where
    argList =  braces ((argName <* whiteSpace) `sepBy` symbolic ',')
           <?> "arglist"

    argName = (,) <$> (identifier <* whiteSpace)
                  <*> optional (symbolic '?' *> nixExpr False)

nvPair :: Parser (NExpr, NExpr)
nvPair = (,) <$> keyName <*> (symbolic '=' *> nixApp)

nixBinders :: Parser [(NExpr, NExpr)]
nixBinders = nvPair `endBy` symbolic ';'

keyName :: Parser NExpr
keyName = (stringish <|> (mkSym <$> identifier)) <* whiteSpace

setOrArgs :: Parser NExpr
setOrArgs = do
    trace "setOrArgs" $ return ()
    sawRec <- try (reserved "rec" *> pure True) <|> pure False
    trace ("Do we have sawRec: " ++ show sawRec) $ return ()
    haveSet <-
        if sawRec
        then return True
        else try (lookAhead lookaheadForSet)
    trace ("Do we have a set: " ++ show haveSet) $ return ()
    if haveSet
        then braces (Fix . NSet sawRec <$> nixBinders) <?> "set"
        else do
            trace "parsing arguments" $ return ()
            args <- argExpr <?> "arguments"
            trace ("args: " ++ show args) $ return ()
            symbolic ':' *> fmap Fix (NAbs <$> pure args <*> nixApp)
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
