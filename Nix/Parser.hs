{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Parser (
  parseNixFile,
  parseNixFileLoc,
  parseNixString,
  parseNixStringLoc,
  parseNixText,
  parseNixTextLoc,
  Result(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable hiding (concat)
import           Data.Functor
import qualified Data.HashMap.Strict.InsOrd as M
import           Data.Text hiding (map, foldl', concat)
import           Nix.Expr hiding (($>))
import           Nix.Parser.Library
import           Nix.Parser.Operators
import           Nix.StringOperations

--------------------------------------------------------------------------------

annotateLocation :: Parser a -> Parser (Ann SrcSpan a)
annotateLocation p = do
  begin <- position
  res   <- p
  end   <- position
  let span = SrcSpan begin end
  pure $ Ann span res

annotateLocation1 :: Parser (NExprF NExprLoc) -> Parser NExprLoc
annotateLocation1 = fmap annToAnnF . annotateLocation

--------------------------------------------------------------------------------

nixExpr :: Parser NExpr
nixExpr = stripAnnotation <$> nixExprLoc

-- | The lexer for this parser is defined in 'Nix.Parser.Library'.
nixExprLoc :: Parser NExprLoc
nixExprLoc =
    whiteSpace *> (nixToplevelForm <|> foldl' makeParser nixTerm nixOperators)
 where
    makeParser :: Parser NExprLoc -> Either NSpecialOp NOperatorDef
               -> Parser NExprLoc
    makeParser term (Left NSelectOp) = nixSelect term
    makeParser term (Left NAppOp) = chainl1 term (pure nApp)
    makeParser term (Left NHasAttrOp) = nixHasAttr term
    makeParser term (Right (NUnaryDef name op)) =
        build <$> many (annotateLocation (void $ symbol name)) <*> term
      where
        build :: [Ann SrcSpan ()] -> NExprLoc -> NExprLoc
        build = flip $ foldl' (\t' (Ann s ()) -> nUnary (Ann s op) t')

    makeParser term (Right (NBinaryDef assoc ops)) = case assoc of
        NAssocLeft  -> chainl1 term op
        NAssocRight -> chainr1 term op
        NAssocNone  -> term <**> (flip <$> op <*> term <|> pure id)
      where
        op :: Parser (NExprLoc -> NExprLoc -> NExprLoc)
        op = choice . map (\(n,o) -> (\(Ann a ()) -> nBinary (Ann a o))
                              <$> annotateLocation (reservedOp n)) $ ops

antiStart :: Parser String
antiStart = try (string "${") <?> show ("${" :: String)

nixAntiquoted :: Parser a -> Parser (Antiquoted a NExprLoc)
nixAntiquoted p = Antiquoted <$> (antiStart *> nixExprLoc <* symbolic '}') <|> Plain <$> p

selDot :: Parser ()
selDot = try (char '.' *> notFollowedBy (("path" :: String) <$ nixPath)) *> whiteSpace
      <?> "."

nixSelector :: Parser (Ann SrcSpan (NAttrPath NExprLoc))
nixSelector = annotateLocation $ keyName `sepBy1` selDot

nixSelect :: Parser NExprLoc -> Parser NExprLoc
nixSelect term = build
  <$> term
  <*> optional ((,) <$> (selDot *> nixSelector) <*> optional (reserved "or" *> nixTerm))
 where
  build :: NExprLoc -> Maybe (Ann SrcSpan (NAttrPath NExprLoc), Maybe NExprLoc) -> NExprLoc
  build t Nothing = t
  build t (Just (s,o)) = nSelectLoc t s o

nixHasAttr :: Parser NExprLoc -> Parser NExprLoc
nixHasAttr term = build <$> term <*> optional (reservedOp "?" *> nixSelector) where
  build :: NExprLoc -> Maybe (Ann SrcSpan (NAttrPath NExprLoc)) -> NExprLoc
  build t Nothing = t
  build t (Just s) = nHasAttr t s

-- | A self-contained unit.
nixTerm :: Parser NExprLoc
nixTerm = nixSelect $ choice
  [ nixPath, nixSPath, nixFloat, nixInt, nixBool, nixNull, nixParens, nixList, nixUri
  , nixStringExpr, nixSet, nixSym ]

nixToplevelForm :: Parser NExprLoc
nixToplevelForm = choice [nixLambda, nixLet, nixIf, nixAssert, nixWith]

nixSym :: Parser NExprLoc
nixSym = annotateLocation1 $ mkSymF <$> identifier

nixInt :: Parser NExprLoc
nixInt = annotateLocation1 $ mkIntF <$> token decimal <?> "integer"

nixFloat :: Parser NExprLoc
nixFloat = annotateLocation1 $ try (mkFloatF . realToFrac <$> token double) <?> "float"

nixBool :: Parser NExprLoc
nixBool = annotateLocation1 $ try (true <|> false) <?> "bool" where
  true = mkBoolF True <$ reserved "true"
  false = mkBoolF False <$ reserved "false"

nixNull :: Parser NExprLoc
nixNull = annotateLocation1 $ mkNullF <$ try (reserved "null") <?> "null"

nixParens :: Parser NExprLoc
nixParens = parens nixExprLoc <?> "parens"

nixList :: Parser NExprLoc
nixList = annotateLocation1 $ brackets (NList <$> many nixTerm) <?> "list"

pathChars :: String
pathChars = ['A'..'Z'] ++ ['a'..'z'] ++ "._-+" ++ ['0'..'9']

slash :: Parser Char
slash = try (char '/' <* notFollowedBy (void (char '/') <|> someSpace)) <?> "slash"

-- | A path surrounded by angle brackets, indicating that it should be
-- looked up in the NIX_PATH environment variable at evaluation.
nixSPath :: Parser NExprLoc
nixSPath = annotateLocation1 $ mkPathF True <$> try (char '<' *> some (oneOf pathChars <|> slash) <* symbolic '>')
        <?> "spath"

nixPath :: Parser NExprLoc
nixPath = annotateLocation1 $ token $ fmap (mkPathF False) $ ((++)
    <$> (try ((++) <$> many (oneOf pathChars) <*> fmap (:[]) slash) <?> "path")
    <*> fmap concat
      (  some (some (oneOf pathChars)
     <|> liftA2 (:) slash (some (oneOf pathChars)))
      )
    )
    <?> "path"

nixLet :: Parser NExprLoc
nixLet = annotateLocation1 $ reserved "let"
    *> whiteSpace
    *> (letBody <|> letBinders)
    <?> "let block"
  where
    letBinders = NLet
        <$> nixBinders
        <*> (whiteSpace *> reserved "in" *> nixExprLoc)
    -- Let expressions `let {..., body = ...}' are just desugared
    -- into `(rec {..., body = ...}).body'.
    letBody = (\x -> NSelect x [StaticKey "body"] Nothing) <$> aset
    aset = annotateLocation1 $ NRecSet <$> braces nixBinders


nixIf :: Parser NExprLoc
nixIf = annotateLocation1 $ NIf
     <$> (reserved "if" *> nixExprLoc)
     <*> (whiteSpace *> reserved "then" *> nixExprLoc)
     <*> (whiteSpace *> reserved "else" *> nixExprLoc)
     <?> "if"

nixAssert :: Parser NExprLoc
nixAssert = annotateLocation1 $ NAssert
  <$> (reserved "assert" *> nixExprLoc)
  <*> (semi *> nixExprLoc)

nixWith :: Parser NExprLoc
nixWith = annotateLocation1 $ NWith
  <$> (reserved "with" *> nixExprLoc)
  <*> (semi *> nixExprLoc)

nixLambda :: Parser NExprLoc
nixLambda = (nAbs <$> annotateLocation (try argExpr <?> "lambda arguments") <*> nixExprLoc) <?> "lambda"

nixStringExpr :: Parser NExprLoc
nixStringExpr = nStr <$> annotateLocation nixString

uriAfterColonC :: Parser Char
uriAfterColonC = alphaNum <|> oneOf "%/?:@&=+$,-_.!~*'"

nixUri :: Parser NExprLoc
nixUri = annotateLocation1 $ token $ fmap (mkUriF . pack) $ (++)
  <$> try ((++) <$> (scheme <* char ':') <*> fmap (\x -> [':',x]) uriAfterColonC)
  <*> many uriAfterColonC
 where
  scheme = (:) <$> letter <*> many (alphaNum <|> oneOf "+-.")

nixString :: Parser (NString NExprLoc)
nixString = doubleQuoted <|> indented <?> "string"
  where
    doubleQuoted :: Parser (NString NExprLoc)
    doubleQuoted = DoubleQuoted . removePlainEmpty . mergePlain
                <$> (doubleQ *> many (stringChar doubleQ (void $ char '\\') doubleEscape)
                             <* token doubleQ)
                <?> "double quoted string"

    doubleQ = void $ char '"'
    doubleEscape = Plain . singleton <$> (char '\\' *> escapeCode)

    indented :: Parser (NString NExprLoc)
    indented = stripIndent
            <$> (indentedQ *> many (stringChar indentedQ indentedQ indentedEscape)
                           <* token indentedQ)
            <?> "indented string"

    indentedQ = void $ try (string "''") <?> "\"''\""
    indentedEscape = fmap Plain
              $  try (indentedQ *> char '\\') *> fmap singleton escapeCode
             <|> try (indentedQ *> ("''" <$ char '\'' <|> "$"  <$ char '$'))

    stringChar end escStart esc
       =  esc
      <|> Antiquoted <$> (antiStart *> nixExprLoc <* char '}') -- don't skip trailing space
      <|> Plain . singleton <$> char '$'
      <|> Plain . pack <$> some plainChar
     where plainChar = notFollowedBy (end <|> void (char '$') <|> escStart) *> anyChar

    escapeCode = choice [ c <$ char e | (c,e) <- escapeCodes ] <|> anyChar

-- | Gets all of the arguments for a function.
argExpr :: Parser (Params NExprLoc)
argExpr = choice [atLeft, onlyname, atRight] <* symbolic ':' where
  -- An argument not in curly braces. There's some potential ambiguity
  -- in the case of, for example `x:y`. Is it a lambda function `x: y`, or
  -- a URI `x:y`? Nix syntax says it's the latter. So we need to fail if
  -- there's a valid URI parse here.
  onlyname = choice [nixUri >> unexpected "valid uri",
                     Param <$> identifier]

  -- Parameters named by an identifier on the left (`args @ {x, y}`)
  atLeft = try $ do
    name <- identifier <* symbolic '@'
    (variadic, params) <- params
    return $ ParamSet params variadic (Just name)

  -- Parameters named by an identifier on the right, or none (`{x, y} @ args`)
  atRight = do
    (variadic, params) <- params
    name <- optional $ symbolic '@' *> identifier
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
    go acc = (token (string "...") >> return (acc, True)) <|> getMore acc
    getMore acc =
      -- Could be nothing, in which just return what we have so far.
      option (acc, False) $ do
        -- Get an argument name and an optional default.
        pair <- liftA2 (,) identifier (optional $ symbolic '?' *> nixExprLoc)
        -- Either return this, or attempt to get a comma and restart.
        option (acc ++ [pair], False) $ symbolic ',' >> go (acc ++ [pair])

nixBinders :: Parser [Binding NExprLoc]
nixBinders = (inherit <|> namedVar) `endBy` symbolic ';' where
  inherit = Inherit <$> (reserved "inherit" *> optional scope)
                    <*> many keyName
                    <?> "inherited binding"
  namedVar = NamedVar <$> (annotated <$> nixSelector)
                      <*> (symbolic '=' *> nixExprLoc)
                      <?> "variable binding"
  scope = parens nixExprLoc <?> "inherit scope"

keyName :: Parser (NKeyName NExprLoc)
keyName = dynamicKey <|> staticKey where
  staticKey = StaticKey <$> identifier
  dynamicKey = DynamicKey <$> nixAntiquoted nixString

nixSet :: Parser NExprLoc
nixSet = annotateLocation1 $ (isRec <*> braces nixBinders) <?> "set" where
  isRec = (try (reserved "rec" $> NRecSet) <?> "recursive set")
       <|> pure NSet

parseNixFile :: MonadIO m => FilePath -> m (Result NExpr)
parseNixFile = parseFromFileEx $ nixExpr <* eof

parseNixFileLoc :: MonadIO m => FilePath -> m (Result NExprLoc)
parseNixFileLoc = parseFromFileEx $ nixExprLoc <* eof

parseNixString :: String -> Result NExpr
parseNixString = parseFromString $ nixExpr <* eof

parseNixStringLoc :: String -> Result NExprLoc
parseNixStringLoc = parseFromString $ nixExprLoc <* eof

parseNixText :: Text -> Result NExpr
parseNixText = parseNixString . unpack

parseNixTextLoc :: Text -> Result NExprLoc
parseNixTextLoc = parseNixStringLoc . unpack
