{-# LANGUAGE CPP #-}

module Nix.Parser.Library ( module Nix.Parser.Library, module X ) where

import           Control.Applicative

#if USE_PARSEC

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text.IO as T
import           Text.Parsec as X hiding ((<|>), many, optional)
import           Text.Parsec.Expr as X
import           Text.Parsec.Text as X
import qualified Text.Parsec.Token as P
import           Text.PrettyPrint.ANSI.Leijen as X (Doc, text)

lexer :: Stream s m Char => P.GenTokenParser s u m
lexer = P.makeTokenParser P.LanguageDef
    { P.commentStart    = "/*"
    , P.commentEnd      = "*/"
    , P.commentLine     = "#"
    , P.nestedComments  = True
    , P.identStart      = letter <|> char '_'
    , P.identLetter     = alphaNum <|> char '_'
    , P.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.opLetter        = oneOf "@"
    , P.reservedNames   =
        [ "let", "in"
        , "if", "then", "else"
        , "true", "false"
        , "null"
        , "assert"
        , "with"
        , "rec"
        , "inherit"
        , "or"
        ]
    , P.reservedOpNames = []
    , P.caseSensitive   = True
    }

parens :: Parser a -> Parser a
parens = P.parens lexer

brackets :: Parser a -> Parser a
brackets = P.brackets lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

symbol :: String -> Parser String
symbol str = string str <* whiteSpace

symbolic :: Char -> Parser Char
symbolic c = char c <* whiteSpace

decimal :: Parser Integer
decimal = read <$> some digit

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

data Result a = Success a
              | Failure Doc

parseFromFileEx :: MonadIO m => Parser a -> FilePath -> m (Result a)
parseFromFileEx p path =
    (either (Failure . text . show) Success . parse p path)
        `liftM` liftIO (T.readFile path)

#else

import Text.Parser.Expression as X
import Text.Parser.LookAhead as X
import Text.Parser.Token as X
import Text.Trifecta as X

#endif

someTill :: Parser a -> Parser end -> Parser [a]
someTill p end = go
  where
    go   = (:) <$> p <*> scan
    scan = (end *> return []) <|>  go
