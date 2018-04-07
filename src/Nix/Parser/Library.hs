{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Parser.Library
  ( module Nix.Parser.Library
  , module X
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Data
import           Data.Functor
import qualified Data.HashSet as HashSet
import           Data.Int (Int64)
import           Data.List (nub)
import           Data.Text
import           Data.Text.Encoding
import           GHC.Generics
import           Text.Parser.Char as X hiding (text)
import           Text.Parser.Combinators as X
import           Text.Parser.Expression as X
import           Text.Parser.LookAhead as X
import           Text.Parser.Token as X
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.PrettyPrint.ANSI.Leijen as X (Doc, text)
#if USE_PARSEC
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Text as Parsec
import qualified Data.Text.IO as T
#else
import qualified Text.Trifecta as Trifecta
import qualified Text.Trifecta.Delta as Trifecta

import           Text.Trifecta as X (Result(..))
#endif

newtype NixParser p a = NixParser { runNixParser :: p a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, LookAheadParsing, Trifecta.DeltaParsing)

instance TokenParsing p => TokenParsing (NixParser p) where
  someSpace = NixParser $ buildSomeSpaceParser' someSpace commentStyle
  nesting = NixParser . nesting . runNixParser
  highlight h = NixParser . highlight h . runNixParser
  semi = token $ char ';' <?> ";"
  token p = p <* whiteSpace

buildSomeSpaceParser' :: forall m. CharParsing m => m () -> CommentStyle -> m ()
buildSomeSpaceParser' simpleSpace
    (CommentStyle startStyle endStyle lineStyle nestingStyle)
  | noLine && noMulti = skipSome (simpleSpace <?> "")
  | noLine           = skipSome (simpleSpace <|> multiLineComment <?> "")
  | noMulti          = skipSome (simpleSpace <|> oneLineComment <?> "")
  | otherwise =
    skipSome (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
  where
    noLine  = Prelude.null lineStyle
    noMulti = Prelude.null startStyle

    oneLineComment, multiLineComment, inComment, inCommentMulti :: m ()
    oneLineComment = try (string lineStyle) *> skipMany (satisfy (\x -> x `notElem` ['\r', '\n']))
    multiLineComment = try (string startStyle) *> inComment
    inComment = if nestingStyle then inCommentMulti else inCommentSingle
    inCommentMulti
      =   () <$ try (string endStyle)
      <|> multiLineComment *> inCommentMulti
      <|> skipSome (noneOf startEnd) *> inCommentMulti
      <|> oneOf startEnd *> inCommentMulti
      <?> "end of comment"

    startEnd = nub (endStyle ++ startStyle)

    inCommentSingle :: m ()
    inCommentSingle
      =   () <$ try (string endStyle)
      <|> skipSome (noneOf startEnd) *> inCommentSingle
      <|> oneOf startEnd *> inCommentSingle
      <?> "end of comment"

commentStyle :: CommentStyle
commentStyle = CommentStyle
  { _commentStart = "/*"
  , _commentEnd   = "*/"
  , _commentLine  = "#"
  , _commentNesting = False
  }

identStyle :: CharParsing m => IdentifierStyle m
identStyle = IdentifierStyle
  { _styleName = "identifier"
  , _styleStart = identStart
  , _styleLetter = identLetter
  , _styleReserved = reservedNames
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

identifier :: (TokenParsing m, Monad m) => m Text
identifier = ident identStyle <?> "identifier"

reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved = reserve identStyle

reservedOp :: TokenParsing m => String -> m ()
reservedOp o = token $ try $ void $
  highlight ReservedOperator (string o)
      <* (notFollowedBy opLetter <?> "end of " ++ o)

opStart :: CharParsing m => m Char
opStart = oneOf ".+-*/=<>&|!?"

opLetter :: CharParsing m => m Char
opLetter = oneOf ">+/&|="

identStart :: CharParsing m => m Char
identStart = letter <|> char '_'

identLetter :: CharParsing m => m Char
identLetter = alphaNum <|> oneOf "_'-"

reservedNames :: HashSet.HashSet String
reservedNames = HashSet.fromList
    [ "let", "in"
    , "if", "then", "else"
    , "assert"
    , "with"
    , "rec"
    , "inherit"
    , "true"
    , "false"
    ]

stopWords :: (TokenParsing m, Monad m) => m ()
stopWords = () <$
    (whiteSpace *> (reserved "in" <|> reserved "then" <|> reserved "else"))

someTill :: Alternative f => f a -> f end -> f [a]
someTill p end = go
  where
    go   = (:) <$> p <*> scan
    scan = (end $> []) <|>  go

--------------------------------------------------------------------------------
-- | Like Text.Trifecta.Delta.Delta, but with FilePath instead of ByteString
data Delta
   = Columns !Int64 !Int64
   | Tab !Int64 !Int64 !Int64
   | Lines !Int64 !Int64 !Int64 !Int64
   | Directed !FilePath !Int64 !Int64 !Int64 !Int64
   deriving (Generic, Data, Eq, Ord, Show, Read)

deltaFromTrifecta :: Trifecta.Delta -> Delta
deltaFromTrifecta = \case
  Trifecta.Columns a b -> Columns a b
  Trifecta.Tab a b c -> Tab a b c
  Trifecta.Lines a b c d -> Lines a b c d
  Trifecta.Directed a b c d e -> Directed (unpack $ decodeUtf8 a) b c d e

deltaToTrifecta :: Delta -> Trifecta.Delta
deltaToTrifecta = \case
  Columns a b -> Trifecta.Columns a b
  Tab a b c -> Trifecta.Tab a b c
  Lines a b c d -> Trifecta.Lines a b c d
  Directed a b c d e -> Trifecta.Directed (encodeUtf8 $ pack a) b c d e

parseFromFileEx :: MonadIO m => Parser a -> FilePath -> m (Result a)
parseFromString :: Parser a -> String -> Result a
position :: Parser Delta

#if USE_PARSEC
data Result a = Success a
              | Failure Doc
  deriving Show

type Parser = NixParser Parsec.Parser

parseFromFileEx p path =
    (either (Failure . text . show) Success . Parsec.parse (runNixParser p) path)
        `liftM` liftIO (T.readFile path)

parseFromString p = either (Failure . text . show) Success . Parsec.parse (runNixParser p) "<string>" . pack

position = error "position not implemented for Parsec parser"

#else

type Parser = NixParser Trifecta.Parser

parseFromFileEx p = Trifecta.parseFromFileEx (runNixParser p)

parseFromString p = Trifecta.parseString (runNixParser p) (Trifecta.Directed "<string>" 0 0 0 0)

position = deltaFromTrifecta <$> Trifecta.position

#endif
