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
import           Data.Char (isAlpha, isDigit)
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

{-
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
-}

{-
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

opLetter :: CharParsing m => m Char
opLetter = oneOf ">+/&|="
-}

opStart :: Parser Char
opStart = satisfy $ \x -> x `elem` (".+-*/=<>&|!?" :: String)

identStart :: Parser Char
identStart = letterChar <|> char '_'

identLetter :: Parser Char
identLetter = satisfy $ \x ->
    isAlpha x || isDigit x || x == '"' || x == '_' || x == '\'' || x == '-'

symbol     = L.symbol whiteSpace
lexeme     = L.lexeme whiteSpace
reservedOp = symbol
identifier = pack <$> ((:) <$> identStart <*> many identLetter)
reserved   = symbol

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

-- number :: Parser Scientific
-- number = lexeme L.scientific -- similar to ‘naturalOrFloat’ in Parsec

reservedNames :: HashSet Text
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

{-
stopWords :: (TokenParsing m, Monad m) => m ()
stopWords = () <$
    (whiteSpace *> (reserved "in" <|> reserved "then" <|> reserved "else"))
-}

whiteSpace :: Parser ()
whiteSpace = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

type Parser = ParsecT Void Text Identity

data Result a = Success a | Failure Doc deriving Show

parseFromFileEx :: MonadIO m => Parser a -> FilePath -> m (Result a)
parseFromFileEx p path =
    (either (Failure . text . parseErrorPretty) Success . parse p path)
        `liftM` liftIO (T.readFile path)

parseFromText :: Parser a -> Text -> Result a
parseFromText p =
    either (Failure . text . parseErrorPretty) Success . parse p "<string>"
