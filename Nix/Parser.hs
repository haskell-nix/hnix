{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Parser (
  parseNixFile,
  parseNixString,
  parseNixText,
  Result(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Fix
import           Data.Foldable hiding (concat)
import qualified Data.Map as Map
import           Data.Text hiding (head, map, foldl1', foldl', concat)
import           Nix.Parser.Library
import           Nix.Types
import           Prelude hiding (elem)

-- | The lexer for this parser is defined in 'Nix.Parser.Library'.
nixExpr :: Parser NExpr
nixExpr = whiteSpace *> (nixToplevelForm <|> foldl' makeParser nixTerm nixOperators)
 where
  makeParser term (Left NSelectOp) = nixSelect term
  makeParser term (Left NAppOp) = chainl1 term $ pure $ \a b -> Fix (NApp a b)
  makeParser term (Left NHasAttrOp) = nixHasAttr term
  makeParser term (Right (NUnaryDef name op))
    = build <$> many (void $ symbol name) <*> term
   where build = flip $ foldl' (\t' () -> mkOper op t')
  makeParser term (Right (NBinaryDef assoc ops)) = case assoc of
    NAssocLeft  -> chainl1 term op
    NAssocRight -> chainr1 term op
    NAssocNone  -> term <**> (flip <$> op <*> term <|> pure id)
   where op = choice . map (\(n,o) -> mkOper2 o <$ reservedOp n) $ ops

antiStart :: Parser String
antiStart = try (string "${") <?> show ("${" :: String)

nixAntiquoted :: Parser a -> Parser (Antiquoted a NExpr)
nixAntiquoted p = Antiquoted <$> (antiStart *> nixExpr <* symbolic '}') <|> Plain <$> p

selDot :: Parser ()
selDot = try (char '.' *> notFollowedBy (("path" :: String) <$ nixPath)) *> whiteSpace
      <?> "."

nixSelector :: Parser (NSelector NExpr)
nixSelector = keyName `sepBy1` selDot where

nixSelect :: Parser NExpr -> Parser NExpr
nixSelect term = build
  <$> term
  <*> optional ((,) <$> (selDot *> nixSelector) <*> optional (reserved "or" *> nixExpr))
 where
  build t Nothing = t
  build t (Just (s,o)) = Fix $ NSelect t s o

nixHasAttr :: Parser NExpr -> Parser NExpr
nixHasAttr term = build <$> term <*> optional (reservedOp "?" *> nixSelector) where
  build t Nothing = t
  build t (Just s) = Fix $ NHasAttr t s

-- | A self-contained unit.
nixTerm :: Parser NExpr
nixTerm = nixSelect $ choice
  [ nixInt, nixBool, nixNull, nixParens, nixList, nixPath, nixSPath, nixUri
  , nixStringExpr, nixSet, nixSym ]

nixToplevelForm :: Parser NExpr
nixToplevelForm = choice [nixLambda, nixLet, nixIf, nixAssert, nixWith]

nixSym :: Parser NExpr
nixSym = mkSym <$> identifier

nixInt :: Parser NExpr
nixInt = mkInt <$> token decimal <?> "integer"

nixBool :: Parser NExpr
nixBool = try (true <|> false) <?> "bool" where
  true = mkBool True <$ symbol "true"
  false = mkBool False <$ symbol "false"

nixNull :: Parser NExpr
nixNull = mkNull <$ try (symbol "null") <?> "null"

nixParens :: Parser NExpr
nixParens = parens nixExpr <?> "parens"

nixList :: Parser NExpr
nixList = brackets (Fix . NList <$> many nixTerm) <?> "list"

pathChars :: String
pathChars = ['A'..'Z'] ++ ['a'..'z'] ++ "._-+" ++ ['0'..'9']

slash :: Parser Char
slash = try (char '/' <* notFollowedBy (char '/')) <?> "slash"

-- | A path surrounded by angle brackets, indicating that it should be
-- looked up in the NIX_PATH environment variable at evaluation.
nixSPath :: Parser NExpr
nixSPath = mkPath True <$> try (char '<' *> some (oneOf pathChars <|> slash) <* symbolic '>')
        <?> "spath"

nixPath :: Parser NExpr
nixPath = token $ fmap (mkPath False) $ ((++)
    <$> (try ((++) <$> many (oneOf pathChars) <*> fmap (:[]) slash) <?> "path")
    <*> fmap concat
      (  some (some (oneOf pathChars)
     <|> liftA2 (:) slash (some (oneOf pathChars)))
      )
    )
    <?> "path"

nixLet :: Parser NExpr
nixLet =  fmap Fix $ NLet
      <$> (reserved "let" *> nixBinders)
      <*> (whiteSpace *> reserved "in" *> nixExpr)
      <?> "let"

nixIf :: Parser NExpr
nixIf =  fmap Fix $ NIf
     <$> (reserved "if" *> nixExpr)
     <*> (whiteSpace *> reserved "then" *> nixExpr)
     <*> (whiteSpace *> reserved "else" *> nixExpr)
     <?> "if"

nixAssert :: Parser NExpr
nixAssert = fmap Fix $ NAssert
  <$> (reserved "assert" *> nixExpr)
  <*> (semi *> nixExpr)

nixWith :: Parser NExpr
nixWith = fmap Fix $ NWith
  <$> (reserved "with" *> nixExpr)
  <*> (semi *> nixExpr)

nixLambda :: Parser NExpr
nixLambda = Fix <$> (NAbs <$> (try argExpr <?> "lambda arguments") <*> nixExpr) <?> "lambda"

nixStringExpr :: Parser NExpr
nixStringExpr = Fix . NStr <$> nixString

uriAfterColonC :: Parser Char
uriAfterColonC = alphaNum <|> oneOf "%/?:@&=+$,-_.!~*'"

nixUri :: Parser NExpr
nixUri = token $ fmap (mkUri . pack) $ (++)
  <$> try ((++) <$> (scheme <* char ':') <*> fmap (\x -> [':',x]) uriAfterColonC)
  <*> many uriAfterColonC
 where
  scheme = (:) <$> letter <*> many (alphaNum <|> oneOf "+-.")

nixString :: Parser (NString NExpr)
nixString = doubleQuoted <|> indented <?> "string"
  where
    doubleQuoted = NString DoubleQuoted . removePlainEmpty . mergePlain
                <$> (doubleQ *> many (stringChar doubleQ (void $ char '\\') doubleEscape)
                             <* token doubleQ)
                <?> "double quoted string"

    doubleQ = void $ char '"'
    doubleEscape = Plain . singleton <$> (char '\\' *> escapeCode)

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
      <|> Antiquoted <$> (antiStart *> nixExpr <* char '}') -- don't skip trailing space
      <|> Plain . singleton <$> char '$'
      <|> Plain . pack <$> some plainChar
     where plainChar = notFollowedBy (end <|> void (char '$') <|> escStart) *> anyChar

    escapeCode = choice [ c <$ char e | (c,e) <- escapeCodes ] <|> anyChar

-- | Gets all of the arguments for a function.
argExpr :: Parser (Formals NExpr)
argExpr = choice [atLeft, onlyname, atRight] <* symbolic ':' where
  -- An argument not in curly braces. There's some potential ambiguity
  -- in the case of, for example `x:y`. Is it a lambda function `x: y`, or
  -- a URI `x:y`? Nix syntax says it's the latter. So we need to fail if
  -- there's a valid URI parse here.
  onlyname = choice [nixUri >> unexpected "valid uri",
                     FormalName <$> identifier]

  -- Parameters named by an identifier on the left (`args @ {x, y}`)
  atLeft = try $ do
    name <- identifier <* symbolic '@'
    ps <- params
    return $ FormalSet ps (Just name)

  -- Parameters named by an identifier on the right, or none (`{x, y} @ args`)
  atRight = do
    ps <- params
    name <- optional $ symbolic '@' *> identifier
    return $ FormalSet ps name

  -- Return the parameters set.
  params = do
    (args, dotdots) <- braces getParams
    let pset = if dotdots then VariadicParamSet else FixedParamSet
    return $ pset $ Map.fromList args

  -- Collects the parameters within curly braces. Returns the parameters and
  -- a boolean indicating if the parameters are variadic.
  getParams :: Parser ([(Text, Maybe NExpr)], Bool)
  getParams = go [] where
    -- Attempt to parse `...`. If this succeeds, stop and return True.
    -- Otherwise, attempt to parse an argument, optionally with a
    -- default. If this fails, then return what has been accumulated
    -- so far.
    go acc = (token (string "...") >> return (acc, True)) <|> getMore acc
    getMore acc = do
      -- Could be nothing, in which just return what we have so far.
      option (acc, False) $ do
        -- Get an argument name and an optional default.
        pair <- liftA2 (,) identifier (optional $ symbolic '?' *> nixExpr)
        -- Either return this, or attempt to get a comma and restart.
        option (acc ++ [pair], False) $ symbolic ',' >> go (acc ++ [pair])

nixBinders :: Parser [Binding NExpr]
nixBinders = (inherit <|> namedVar) `endBy` symbolic ';' where
  inherit = Inherit <$> (reserved "inherit" *> optional scope) <*> many ((:[]) <$> keyName)
         <?> "inherited binding"
  namedVar = NamedVar <$> nixSelector <*> (symbolic '=' *> nixExpr)
          <?> "variable binding"
  scope = parens nixExpr <?> "inherit scope"

keyName :: Parser (NKeyName NExpr)
keyName = dynamicKey <|> staticKey where
  staticKey = StaticKey <$> identifier
  dynamicKey = DynamicKey <$> nixAntiquoted nixString

nixSet :: Parser NExpr
nixSet = Fix <$> (NSet <$> isRec <*> braces nixBinders) <?> "set" where
  isRec = (try (reserved "rec" *> pure Rec) <?> "recursive set")
       <|> pure NonRec

parseNixFile :: MonadIO m => FilePath -> m (Result NExpr)
parseNixFile = parseFromFileEx $ nixExpr <* eof

parseNixString :: String -> Result NExpr
parseNixString = parseFromString $ nixExpr <* eof

parseNixText :: Text -> Result NExpr
parseNixText = parseNixString . unpack
