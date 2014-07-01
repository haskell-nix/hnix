{-# LANGUAGE CPP #-}

module Nix.Parser (parseNixFile, Result(..)) where

import           Control.Applicative
import           Control.Monad hiding (forM_, mapM, sequence)
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Foldable
import qualified Data.Map as Map
import           Data.Text hiding (concat, concatMap, head, map)
import           Nix.Types
import           Nix.Internal
import qualified Prelude
import           Prelude hiding (readFile, concat, concatMap, elem, mapM,
                                 sequence)

#if USE_PARSEC
import           Data.Text.IO
import           Text.Parsec hiding ((<|>), many, optional)
import           Text.Parsec.Text
import           Text.PrettyPrint.ANSI.Leijen (Doc, text)

symbol :: String -> Parser String
symbol str = string str <* whiteSpace

symbolic :: Char -> Parser Char
symbolic c = char c <* whiteSpace

decimal :: Parser Integer
decimal = read <$> some digit

whiteSpace :: Parser ()
whiteSpace = spaces

data Result a = Success a
              | Failure Doc

parseFromFileEx :: MonadIO m => Parser a -> FilePath -> m (Result a)
parseFromFileEx p path =
    (either (Failure . text . show) Success . parse p path)
        `liftM` liftIO (readFile path)
#else
import           Text.Trifecta
import           Text.Parser.LookAhead
#endif

parseNixFile :: MonadIO m => FilePath -> m (Result NExpr)
parseNixFile = parseFromFileEx nixApp

nixApp :: Parser NExpr
nixApp = go <$> some (whiteSpace *> nixTerm True)
  where
    go []     = error "some has failed us"
    go [x]    = x
    go (f:xs) = Fix (NApp f (go xs))

nixTerm :: Bool -> Parser NExpr
nixTerm allowLambdas = choice
    [ mkInt <$> decimal                       <?> "integer"

    , (string "true" *> pure (mkBool True))   <?> "bool"
    , (string "false" *> pure (mkBool False)) <?> "bool"
    , (string "null" *> pure mkNull)          <?> "null"

    , between (symbolic '(') (symbolic ')') nixApp
          <?> "parens"

    , between (symbolic '[') (symbolic ']')
          (Fix . NList <$> many (nixTerm False))
          <?> "list"

    , try (do chars <- some (satisfy isPathChar)
              trace ("Path chars: " ++ show chars) $ return ()
              guard ('/' `elem` chars)
              return $ mkPath chars)

    , maybeSetOrLambda allowLambdas
    ]

maybeSetOrLambda :: Bool -> Parser NExpr
maybeSetOrLambda allowLambdas = do
    trace "maybeSetOrLambda" $ return ()
    x <- try (lookAhead symName)
        <|> try (lookAhead (singleton <$> char '{'))
        <|> return ""
    if x == "rec" || x == "{"
        then setOrArgs
        else do
            trace "might still have a lambda" $ return ()
            y <- try (lookAhead (symName *> whiteSpace *> symbolic ':'
                                     *> return True))
                <|> return False
            trace ("results are = " ++ show y) $ return ()
            if y
                then if allowLambdas
                    then setOrArgs
                    else error "Unexpected lambda"
                else keyName <?> "string"

isPathChar :: Char -> Bool
isPathChar c = isAlpha c || c `Prelude.elem` ".:/"

oneChar :: Parser NExpr
oneChar = mkStr . singleton <$> anyChar

stringChar :: Parser NExpr
stringChar = char '\\' *> oneChar
         <|> (string "${" *> nixApp <* char '}')
         <|> (mkStr . pack <$> many (noneOf "\"\\"))

symName :: Parser Text
symName = do
    chars <- some (satisfy (\c -> isAlpha c || c == '.'))
    trace ("chars = " ++ show chars) $ return ()
    guard (isLower (head chars))
    return $ pack (trace ("chars: " ++ show chars) chars)

stringish :: Parser NExpr
stringish
     =  (char '"' *>
         (Fix . NConcat <$> manyTill stringChar (char '"')))
    <|> (char '$' *> between (symbolic '{') (symbolic '}') nixApp)

keyName :: Parser NExpr
keyName = (stringish <|> (mkSym <$> symName)) <* whiteSpace

nvPair :: Parser (NExpr, NExpr)
nvPair = (,) <$> keyName
             <*> (symbolic '=' *> nixApp)

argExpr :: Parser NExpr
argExpr =  (Fix . NArgSet . Map.fromList <$> argList)
       <|> ((mkSym <$> symName) <?> "argname")

argList :: Parser [(Text, Maybe NExpr)]
argList = between (symbolic '{') (symbolic '}')
              ((argName <* whiteSpace) `sepBy` symbolic ',')
              <?> "arglist"

argName :: Parser (Text, Maybe NExpr)
argName = (,) <$> (symName <* whiteSpace)
              <*> optional (try (symbolic '?' *> nixApp))

-- whiteSymbolic :: Char -> Parser Char
-- whiteSymbolic c = whiteSpace *> symbolic c

lookaheadForSet :: Parser Bool
lookaheadForSet = do
    trace "lookaheadForSet" $ return ()
    x <- (symbolic '{' *> return True) <|> return False
    if not x then return x else do
        y <- (keyName *> return True) <|> return False
        if not y then return y else
            (symbolic '=' *> return True) <|> return False

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
        then between (symbolic '{') (symbolic '}')
                 (Fix . NSet sawRec <$> nvPair `endBy` symbolic ';')
                 <?> "set"
        else do
            trace "parsing arguments" $ return ()
            args <- argExpr <?> "arguments"
            trace ("args: " ++ show args) $ return ()
            symbolic ':' *> ((Fix .) . NAbs <$> pure args <*> nixApp)
                <|> pure args
