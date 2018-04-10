{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Parser.Library
  ( module Nix.Parser.Library
  , module X
  ) where

import           Control.Applicative hiding (many)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char (isAlpha, isDigit, isSpace)
import           Data.Functor.Identity
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Text
import qualified Data.Text.IO as T
import           Data.Void
import           Text.Megaparsec as X
import           Text.Megaparsec.Char as X
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.PrettyPrint.ANSI.Leijen as X (Doc, text)

whiteSpace :: Parser ()
whiteSpace = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace
{-# INLINEABLE lexeme #-}

symbol = lexeme . string

reserved :: Text -> Parser ()
reserved n = lexeme $ try $ do
    _ <- string n <* lookAhead (satisfy endMarker)
    return ()
  where
    endMarker x = isSpace x || x == '{' || x == '(' || x == ';'

opStart :: Parser Char
opStart = satisfy $ \x ->
    -- jww (2018-04-09): Could this be faster?
    x `elem` (".+-*/=<>&|!?" :: String)

{-
opLetter :: CharParsing m => m Char
opLetter = oneOf ">+/&|="
-}

identStart :: Parser Char
identStart = letterChar <|> char '_'

identLetter :: Parser Char
identLetter = satisfy $ \x ->
    isAlpha x || isDigit x || x == '"' || x == '_' || x == '\'' || x == '-'

identifier = lexeme $ try $ do
    ident <- pack <$> ((:) <$> identStart <*> many identLetter)
    guard (not (ident `HashSet.member` reservedNames))
    return ident

parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")
semi      = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."
equals    = symbol "="
question  = symbol "?"

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

reservedNames :: HashSet Text
reservedNames = HashSet.fromList
    [ "let", "in"
    , "if", "then", "else"
    , "assert"
    , "with"
    , "rec"
    , "inherit"
    , "true", "false" ]

type Parser = ParsecT Void Text Identity

data Result a = Success a | Failure Doc deriving Show

parseFromFileEx :: MonadIO m => Parser a -> FilePath -> m (Result a)
parseFromFileEx p path = do
    txt <- liftIO (T.readFile path)
    return $ either (Failure . text . parseErrorPretty' txt) Success
           $ parse p path txt

parseFromText :: Parser a -> Text -> Result a
parseFromText p txt =
    either (Failure . text . parseErrorPretty' txt) Success $
        parse p "<string>" txt
