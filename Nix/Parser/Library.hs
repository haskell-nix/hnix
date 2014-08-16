{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nix.Parser.Library ( module Nix.Parser.Library, module X) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Text hiding (map)
import Text.Parser.Expression as X
import Text.Parser.LookAhead as X
import Text.Parser.Token as X
import Text.Parser.Char as X hiding (text)
import Text.Parser.Combinators as X
import Text.PrettyPrint.ANSI.Leijen as X (Doc, text)
import Text.Parser.Token.Highlight

import qualified Data.HashSet as HashSet

#if USE_PARSEC
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Text as Parsec
import qualified Data.Text.IO as T
#else
import qualified Text.Trifecta as Trifecta
import qualified Text.Trifecta.Delta as Trifecta

import Text.Trifecta as X (Result(..))
#endif

newtype NixParser p a = NixParser { runNixParser :: p a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, LookAheadParsing)

instance TokenParsing p => TokenParsing (NixParser p) where
  someSpace = skipSome (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
  nesting = NixParser . nesting . runNixParser
  highlight h = NixParser . highlight h . runNixParser
  semi = token $ char ';' <?> ";"
  token p = p <* whiteSpace

simpleSpace :: CharParsing m => m ()
simpleSpace = skipSome (satisfy isSpace)

oneLineComment :: CharParsing m => m ()
oneLineComment = char '#' *> skipMany (notChar '\n')

multiLineComment :: CharParsing m => m ()
multiLineComment = try (string "/*") *> inComment

inComment :: CharParsing m => m ()
inComment = choice
  [ () <$ try (string "*/")
  , multiLineComment       *> inComment
  , skipSome (noneOf "*/") *> inComment
  , oneOf "*/"             *> inComment
  ] <?> "end of comment"

identStyle :: CharParsing m => IdentifierStyle m
identStyle = IdentifierStyle
  { _styleName = "nix identifier"
  , _styleStart = identStart
  , _styleLetter = identLetter
  , _styleReserved = HashSet.fromList reservedNames
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

identifier :: (TokenParsing m, Monad m) => m Text
identifier = ident identStyle

reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved = reserve identStyle

reservedOp :: TokenParsing m => String -> m ()
reservedOp o = token $ try $ () <$
  highlight ReservedOperator (string o) <* (notFollowedBy opLetter <?> "end of " ++ o)

opStart :: CharParsing m => m Char
opStart = oneOf ".+-*/=<>&|!?"

opLetter :: CharParsing m => m Char
opLetter = oneOf ">-+/&|="

identStart :: CharParsing m => m Char
identStart = letter <|> char '_'

identLetter :: CharParsing m => m Char
identLetter = alphaNum <|> oneOf "_'-"

reservedNames :: [String]
reservedNames =
    [ "let", "in"
    , "if", "then", "else"
    , "null"
    , "assert"
    , "with"
    , "rec"
    , "inherit"
    , "or"
    ]

stopWords :: (TokenParsing m, Monad m) => m ()
stopWords = () <$
    (whiteSpace *> (reserved "in" <|> reserved "then" <|> reserved "else"))

someTill :: Alternative f => f a -> f end -> f [a]
someTill p end = go
  where
    go   = (:) <$> p <*> scan
    scan = (end *> pure []) <|>  go

--------------------------------------------------------------------------------
parseFromFileEx :: MonadIO m => Parser a -> FilePath -> m (Result a)
parseFromString :: Parser a -> String -> Result a

#if USE_PARSEC
data Result a = Success a
              | Failure Doc
  deriving Show

type Parser = NixParser Parsec.Parser

parseFromFileEx p path =
    (either (Failure . text . show) Success . Parsec.parse (runNixParser p) path)
        `liftM` liftIO (T.readFile path)

parseFromString p = either (Failure . text . show) Success . Parsec.parse (runNixParser p) "<string>" . pack

#else

type Parser = NixParser Trifecta.Parser

parseFromFileEx p = Trifecta.parseFromFileEx (runNixParser p)

parseFromString p = Trifecta.parseString (runNixParser p) (Trifecta.Directed "<string>" 0 0 0 0)
#endif
