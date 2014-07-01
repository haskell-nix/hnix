{-# LANGUAGE CPP #-}

module Nix.Parser.Library
    (
#if USE_PARSEC
      module Text.Parsec
    , module Text.Parsec.Expr
    , module Text.Parsec.Text
#else
      module Text.Trifecta
    , module Text.Parser.Expression
    , module Text.Parser.LookAhead
#endif
    )
    where

#if USE_PARSEC

import Control.Applicative
import Data.Text.IO
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.Expr
import Text.Parsec.Text
import Text.PrettyPrint.ANSI.Leijen (Doc, text)

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

import Text.Parser.Expression
import Text.Parser.LookAhead
import Text.Trifecta

#endif
