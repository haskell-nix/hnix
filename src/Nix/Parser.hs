{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Parser (
  parseNixFile,
  parseNixFileLoc,
  parseNixText,
  parseNixTextLoc,
  Result(..)
  ) where

import           Control.Applicative hiding (many, some)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char (isAlpha, isDigit, isSpace)
import           Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict.InsOrd as M
import           Data.Text hiding (map)
import           Nix.Expr hiding (($>))
import           Nix.Parser.Library
import           Nix.Parser.Operators
import           Nix.StringOperations
import           Text.Megaparsec.Expr

--------------------------------------------------------------------------------

nixExpr :: Parser NExpr
nixExpr = stripAnnotation <$> nixExprLoc

-- | The lexer for this parser is defined in 'Nix.Parser.Library'.
nixExprLoc :: Parser NExprLoc
nixExprLoc = whiteSpace *> (nixToplevelForm <|> exprParser)

exprParser :: Parser NExprLoc
exprParser = makeExprParser nixTerm $ map (map snd) (nixOperators nixSelector)

antiStart :: Parser Text
antiStart = try (symbol "${") <?> show ("${" :: String)

nixAntiquoted :: Parser a -> Parser (Antiquoted a NExprLoc)
nixAntiquoted p =
        Antiquoted <$> (antiStart *> nixExprLoc <* symbol "}")
    <|> Plain <$> p
    <?> "anti-quotation"

selDot :: Parser ()
selDot = try (symbol "." *> notFollowedBy nixPath) <?> "."

nixSelect :: Parser NExprLoc -> Parser NExprLoc
nixSelect term = build
  <$> term
  <*> optional ((,) <$> (selDot *> nixSelector)
                    <*> optional (reserved "or" *> nixTerm))
 where
  build :: NExprLoc
        -> Maybe (Ann SrcSpan (NAttrPath NExprLoc), Maybe NExprLoc)
        -> NExprLoc
  build t Nothing = t
  build t (Just (s,o)) = nSelectLoc t s o

nixSelector :: Parser (Ann SrcSpan (NAttrPath NExprLoc))
nixSelector = annotateLocation $ keyName `sepBy1` selDot

-- #define DEBUG_PARSER 1
#if DEBUG_PARSER
-- | A self-contained unit.
nixTerm :: Parser NExprLoc
nixTerm = nixSelect $ choice
    [ dbg "Parens"     nixParens
    , dbg "Set"        nixSet
    , dbg "List"       nixList
    , dbg "SPath"      nixSPath
    , dbg "StringExpr" nixStringExpr
    , dbg "Path"       nixPath
    , dbg "Uri"        nixUri
    , dbg "Float"      nixFloat
    , dbg "Int"        nixInt
    , dbg "Bool"       nixBool
    , dbg "Null"       nixNull
    , dbg "Sym"        nixSym ]

nixToplevelForm :: Parser NExprLoc
nixToplevelForm = choice
    [ dbg "Let"    nixLet
    , dbg "If"     nixIf
    , dbg "Assert" nixAssert
    , dbg "With"   nixWith
    , dbg "Lambda" nixLambda ]
#else
nixTerm :: Parser NExprLoc
nixTerm = nixSelect $ choice
    [ nixParens
    , nixSet
    , nixList
    , nixSPath
    , nixStringExpr
    , nixPath
    , nixUri
    , nixFloat
    , nixInt
    , nixBool
    , nixNull
    , nixSym ]

nixToplevelForm :: Parser NExprLoc
nixToplevelForm = choice
    [ nixLet
    , nixIf
    , nixAssert
    , nixWith
    , nixLambda ]
#endif

nixSym :: Parser NExprLoc
nixSym = annotateLocation1 $ mkSymF <$> identifier

nixInt :: Parser NExprLoc
nixInt = annotateLocation1 (mkIntF <$> integer <?> "integer")

nixFloat :: Parser NExprLoc
nixFloat = annotateLocation1 (try (mkFloatF . realToFrac <$> float) <?> "float")

nixBool :: Parser NExprLoc
nixBool = annotateLocation1 (try (bool "true" True <|>
                                  bool "false" False) <?> "bool") where
  bool str b = mkBoolF b <$ lexeme (string str <* notFollowedBy pathChar)

nixNull :: Parser NExprLoc
nixNull = annotateLocation1
    (mkNullF <$ try (lexeme (string "null" <* notFollowedBy pathChar))
         <?> "null")

nixParens :: Parser NExprLoc
nixParens = parens nixExprLoc <?> "parens"

nixList :: Parser NExprLoc
nixList = annotateLocation1 (brackets (NList <$> many nixTerm) <?> "list")

pathChar :: Parser Char
pathChar = satisfy $ \x ->
    isAlpha x || isDigit x || x == '.' || x == '_' || x == '-' || x == '+'

slash :: Parser Char
slash = try (char '/' <* notFollowedBy (satisfy (\x -> x == '/' || x == '*' || isSpace x)))
    <?> "slash"

-- | A path surrounded by angle brackets, indicating that it should be
-- looked up in the NIX_PATH environment variable at evaluation.
nixSPath :: Parser NExprLoc
nixSPath = annotateLocation1
    (mkPathF True <$> try (char '<' *> many (pathChar <|> slash) <* symbol ">")
         <?> "spath")

pathStr :: Parser FilePath
pathStr = lexeme $ liftM2 (++) (many pathChar)
    (Prelude.concat <$> some (liftM2 (:) slash (some pathChar)))

nixPath :: Parser NExprLoc
nixPath = annotateLocation1 (try (mkPathF False <$> pathStr) <?> "path")

nixLet :: Parser NExprLoc
nixLet = annotateLocation1 (reserved "let"
    *> (letBody <|> letBinders)
    <?> "let block")
  where
    letBinders = NLet
        <$> nixBinders
        <*> (reserved "in" *> nixExprLoc)
    -- Let expressions `let {..., body = ...}' are just desugared
    -- into `(rec {..., body = ...}).body'.
    letBody = (\x pos -> NSelect x [StaticKey "body" (Just pos)] Nothing)
        <$> aset <*> getPosition
    aset = annotateLocation1 $ NRecSet <$> braces nixBinders

nixIf :: Parser NExprLoc
nixIf = annotateLocation1 (NIf
     <$> (reserved "if" *> nixExprLoc)
     <*> (reserved "then" *> nixExprLoc)
     <*> (reserved "else" *> nixExprLoc)
     <?> "if")

nixAssert :: Parser NExprLoc
nixAssert = annotateLocation1 (NAssert
  <$> (reserved "assert" *> nixExprLoc)
  <*> (semi *> nixExprLoc)
  <?> "assert")

nixWith :: Parser NExprLoc
nixWith = annotateLocation1 (NWith
  <$> (reserved "with" *> nixExprLoc)
  <*> (semi *> nixExprLoc)
  <?> "with")

nixLambda :: Parser NExprLoc
nixLambda = nAbs <$> annotateLocation (try argExpr) <*> nixExprLoc

nixStringExpr :: Parser NExprLoc
nixStringExpr = nStr <$> annotateLocation nixString

nixUri :: Parser NExprLoc
nixUri = annotateLocation1 $ lexeme $ try $ do
    start <- letterChar
    protocol <- many $ satisfy $ \x ->
        isAlpha x || isDigit x || x `elem` ("+-." :: String)
    _ <- string ":"
    address  <- some $ satisfy $ \x ->
        isAlpha x || isDigit x || x `elem` ("%/?:@&=+$,-_.!~*'" :: String)
    return $ mkUriF $ pack $ start : protocol ++ ':' : address

nixString :: Parser (NString NExprLoc)
nixString = lexeme (doubleQuoted <|> indented <?> "string")
  where
    doubleQuoted :: Parser (NString NExprLoc)
    doubleQuoted = DoubleQuoted . removePlainEmpty . mergePlain
                <$> (doubleQ *> many (stringChar doubleQ (void $ char '\\') doubleEscape)
                             <* doubleQ)
                <?> "double quoted string"

    doubleQ = void (char '"')
    doubleEscape = Plain . singleton <$> (char '\\' *> escapeCode)

    indented :: Parser (NString NExprLoc)
    indented = stripIndent
            <$> (indentedQ *> many (stringChar indentedQ indentedQ indentedEscape)
                           <* indentedQ)
            <?> "indented string"

    indentedQ = void (try (string "''") <?> "\"''\"")
    indentedEscape = fmap Plain
              $  try (indentedQ *> char '\\') *> fmap singleton escapeCode
             <|> try (indentedQ *> ("''" <$ char '\'' <|> "$"  <$ char '$'))

    stringChar end escStart esc = esc
        <|> Antiquoted <$> (antiStart *> nixExprLoc <* char '}')
            -- ^ don't skip trailing space
        <|> Plain . singleton <$> char '$'
        <|> Plain . pack <$> some plainChar
     where
       plainChar =
           notFollowedBy (end <|> void (char '$') <|> escStart) *> anyChar

    escapeCode = choice [ c <$ char e | (c,e) <- escapeCodes ] <|> anyChar

-- | Gets all of the arguments for a function.
argExpr :: Parser (Params NExprLoc)
argExpr = choice [atLeft, onlyname, atRight] <* symbol ":" where
  -- An argument not in curly braces. There's some potential ambiguity
  -- in the case of, for example `x:y`. Is it a lambda function `x: y`, or
  -- a URI `x:y`? Nix syntax says it's the latter. So we need to fail if
  -- there's a valid URI parse here.
  onlyname = choice [nixUri >> unexpected (Label ('v' NE.:| "alid uri")),
                     Param <$> identifier]

  -- Parameters named by an identifier on the left (`args @ {x, y}`)
  atLeft = try $ do
    name <- identifier <* symbol "@"
    (variadic, params) <- params
    return $ ParamSet params variadic (Just name)

  -- Parameters named by an identifier on the right, or none (`{x, y} @ args`)
  atRight = do
    (variadic, params) <- params
    name <- optional $ symbol "@" *> identifier
    return $ ParamSet params variadic name

  -- Return the parameters set.
  params = do
    (args, dotdots) <- braces getParams
    return (dotdots, M.fromList args)

  -- Collects the parameters within curly braces. Returns the parameters and
  -- a boolean indicating if the parameters are variadic.
  getParams :: Parser ([(Text, Maybe NExprLoc)], Bool)
  getParams = go [] where
    -- Attempt to parse `...`. If this succeeds, stop and return True.
    -- Otherwise, attempt to parse an argument, optionally with a
    -- default. If this fails, then return what has been accumulated
    -- so far.
    go acc = ((acc, True) <$ symbol "...") <|> getMore acc
    getMore acc =
      -- Could be nothing, in which just return what we have so far.
      option (acc, False) $ do
        -- Get an argument name and an optional default.
        pair <- liftA2 (,) identifier (optional $ question *> nixExprLoc)
        -- Either return this, or attempt to get a comma and restart.
        option (acc ++ [pair], False) $ comma >> go (acc ++ [pair])

nixBinders :: Parser [Binding NExprLoc]
nixBinders = (inherit <|> namedVar) `endBy` semi where
  inherit = Inherit <$> (reserved "inherit" *> optional scope)
                    <*> many keyName
                    <?> "inherited binding"
  namedVar = NamedVar <$> (annotated <$> nixSelector)
                      <*> (equals *> nixExprLoc)
                      <?> "variable binding"
  scope = parens nixExprLoc <?> "inherit scope"

keyName :: Parser (NKeyName NExprLoc)
keyName = dynamicKey <|> staticKey where
  staticKey = do
      beg <- getPosition
      StaticKey <$> identifier <*> pure (Just beg)
  dynamicKey = DynamicKey <$> nixAntiquoted nixString

nixSet :: Parser NExprLoc
nixSet = annotateLocation1 ((isRec <*> braces nixBinders) <?> "set") where
  isRec = (try (reserved "rec" $> NRecSet) <?> "recursive set")
       <|> pure NSet

parseNixFile :: MonadIO m => FilePath -> m (Result NExpr)
parseNixFile = parseFromFileEx $ nixExpr <* eof

parseNixFileLoc :: MonadIO m => FilePath -> m (Result NExprLoc)
parseNixFileLoc = parseFromFileEx $ nixExprLoc <* eof

parseNixText :: Text -> Result NExpr
parseNixText = parseFromText $ nixExpr <* eof

parseNixTextLoc :: Text -> Result NExprLoc
parseNixTextLoc = parseFromText $ nixExprLoc <* eof
