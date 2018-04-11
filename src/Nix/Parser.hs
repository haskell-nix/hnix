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
import           Data.Text hiding (map, empty)
import           Nix.Expr hiding (($>))
import           Nix.Parser.Library
import           Nix.Parser.Operators
import           Nix.StringOperations
import           Text.Megaparsec.Expr

--------------------------------------------------------------------------------

nixExprLoc :: Parser NExprLoc
nixExprLoc = makeExprParser nixTerm $ map (map snd) (nixOperators nixSelector)

antiStart :: Parser Text
antiStart = symbol "${" <?> show ("${" :: String)

nixAntiquoted :: Parser a -> Parser (Antiquoted a NExprLoc)
nixAntiquoted p =
        Antiquoted <$> (antiStart *> nixToplevelForm <* symbol "}")
    <|> Plain <$> p
    <?> "anti-quotation"

selDot :: Parser ()
selDot = try (symbol "." *> notFollowedBy nixPath) <?> "."

nixSelect :: Parser NExprLoc -> Parser NExprLoc
nixSelect term = do
    res <- build
        <$> term
        <*> optional ((,) <$> (selDot *> nixSelector)
                          <*> optional (reserved "or" *> nixTerm))
    continues <- optional $ lookAhead selDot
    case continues of
        Nothing -> pure res
        Just _  -> nixSelect (pure res)
 where
  build :: NExprLoc
        -> Maybe (Ann SrcSpan (NAttrPath NExprLoc), Maybe NExprLoc)
        -> NExprLoc
  build t Nothing = t
  build t (Just (s,o)) = nSelectLoc t s o

nixSelector :: Parser (Ann SrcSpan (NAttrPath NExprLoc))
nixSelector = annotateLocation $ keyName `sepBy1` selDot

nixTerm :: Parser NExprLoc
nixTerm = do
    c <- try $ lookAhead $ satisfy $ \x ->
        pathChar x ||
        x == '(' ||
        x == '{' ||
        x == '[' ||
        x == '<' ||
        x == '/' ||
        x == '"' ||
        x == '\''
    case c of
        '('  -> nixSelect nixParens
        '{'  -> nixSelect nixSet
        '['  -> nixList
        '<'  -> nixSPath
        '/'  -> nixPath
        '"'  -> nixStringExpr
        '\'' -> nixStringExpr
        _    -> choice $
            [ nixSelect nixSet | c == 'r' ] ++
            [ nixPath | pathChar c ] ++
            [ nixUri | isAlpha c ] ++
            (if isDigit c then [ nixFloat
                               , nixInt ] else []) ++
            [ nixBool | c == 't' || c == 'f' ] ++
            [ nixNull | c == 'n' ] ++
            [ nixSelect nixSym ]

nixToplevelForm :: Parser NExprLoc
nixToplevelForm = keywords <|> nixLambda <|> nixExprLoc
  where
    keywords = do
        word <- try $ lookAhead $ some letterChar <* satisfy reservedEnd
        case word of
            "let"    -> nixLet
            "if"     -> nixIf
            "assert" -> nixAssert
            "with"   -> nixWith
            _        -> empty

nixSym :: Parser NExprLoc
nixSym = annotateLocation1 $ mkSymF <$> identifier

nixInt :: Parser NExprLoc
nixInt = annotateLocation1 (mkIntF <$> integer <?> "integer")

nixFloat :: Parser NExprLoc
nixFloat = annotateLocation1 (try (mkFloatF . realToFrac <$> float) <?> "float")

nixBool :: Parser NExprLoc
nixBool = annotateLocation1 (bool "true" True <|>
                             bool "false" False) <?> "bool" where
  bool str b = mkBoolF b <$ reserved str

nixNull :: Parser NExprLoc
nixNull = annotateLocation1 (mkNullF <$ reserved "null" <?> "null")

nixParens :: Parser NExprLoc
nixParens = parens nixToplevelForm <?> "parens"

nixList :: Parser NExprLoc
nixList = annotateLocation1 (brackets (NList <$> many nixTerm) <?> "list")

pathChar :: Char -> Bool
pathChar x = isAlpha x || isDigit x || x == '.' || x == '_' || x == '-' || x == '+'

slash :: Parser Char
slash = try (char '/' <* notFollowedBy (satisfy (\x -> x == '/' || x == '*' || isSpace x)))
    <?> "slash"

-- | A path surrounded by angle brackets, indicating that it should be
-- looked up in the NIX_PATH environment variable at evaluation.
nixSPath :: Parser NExprLoc
nixSPath = annotateLocation1
    (mkPathF True <$> try (char '<' *> many (satisfy pathChar <|> slash) <* symbol ">")
         <?> "spath")

pathStr :: Parser FilePath
pathStr = lexeme $ liftM2 (++) (many (satisfy pathChar))
    (Prelude.concat <$> some (liftM2 (:) slash (some (satisfy pathChar))))

nixPath :: Parser NExprLoc
nixPath = annotateLocation1 (try (mkPathF False <$> pathStr) <?> "path")

nixLet :: Parser NExprLoc
nixLet = annotateLocation1 (reserved "let"
    *> (letBody <|> letBinders)
    <?> "let block")
  where
    letBinders = NLet
        <$> nixBinders
        <*> (reserved "in" *> nixToplevelForm)
    -- Let expressions `let {..., body = ...}' are just desugared
    -- into `(rec {..., body = ...}).body'.
    letBody = (\x pos -> NSelect x [StaticKey "body" (Just pos)] Nothing)
        <$> aset <*> getPosition
    aset = annotateLocation1 $ NRecSet <$> braces nixBinders

nixIf :: Parser NExprLoc
nixIf = annotateLocation1 (NIf
     <$> (reserved "if" *> nixExprLoc)
     <*> (reserved "then" *> nixToplevelForm)
     <*> (reserved "else" *> nixToplevelForm)
     <?> "if")

nixAssert :: Parser NExprLoc
nixAssert = annotateLocation1 (NAssert
  <$> (reserved "assert" *> nixExprLoc)
  <*> (semi *> nixToplevelForm)
  <?> "assert")

nixWith :: Parser NExprLoc
nixWith = annotateLocation1 (NWith
  <$> (reserved "with" *> nixToplevelForm)
  <*> (semi *> nixToplevelForm)
  <?> "with")

nixLambda :: Parser NExprLoc
nixLambda = nAbs <$> annotateLocation (try argExpr) <*> nixToplevelForm

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
                <$> (doubleQ *> many (stringChar doubleQ (void $ char '\\')
                                                 doubleEscape)
                             <* doubleQ)
                <?> "double quoted string"

    doubleQ = void (char '"')
    doubleEscape = Plain . singleton <$> (char '\\' *> escapeCode)

    indented :: Parser (NString NExprLoc)
    indented = stripIndent
            <$> (indentedQ *> many (stringChar indentedQ indentedQ
                                               indentedEscape)
                           <* indentedQ)
            <?> "indented string"

    indentedQ = void (string "''" <?> "\"''\"")
    indentedEscape = try $ do
        indentedQ
        (Plain <$> ("''" <$ char '\'' <|> "$"  <$ char '$')) <|> do
            _ <- char '\\'
            c <- escapeCode
            pure $ if c == '\n'
                   then EscapedNewline
                   else Plain $ singleton c

    stringChar end escStart esc =
            Antiquoted <$> (antiStart *> nixToplevelForm <* char '}')
        <|> Plain . singleton <$> char '$'
        <|> esc
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
    return (dotdots, args)

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
        pair <- liftA2 (,) identifier (optional $ question *> nixToplevelForm)
        -- Either return this, or attempt to get a comma and restart.
        option (acc ++ [pair], False) $ comma >> go (acc ++ [pair])

nixBinders :: Parser [Binding NExprLoc]
nixBinders = (inherit <|> namedVar) `endBy` semi where
  inherit = Inherit <$> (reserved "inherit" *> optional scope)
                    <*> many keyName
                    <?> "inherited binding"
  namedVar = NamedVar <$> (annotated <$> nixSelector)
                      <*> (equals *> nixToplevelForm)
                      <?> "variable binding"
  scope = parens nixToplevelForm <?> "inherit scope"

keyName :: Parser (NKeyName NExprLoc)
keyName = dynamicKey <|> staticKey where
  staticKey = do
      beg <- getPosition
      StaticKey <$> identifier <*> pure (Just beg)
  dynamicKey = DynamicKey <$> nixAntiquoted nixString

nixSet :: Parser NExprLoc
nixSet = annotateLocation1 ((isRec <*> braces nixBinders) <?> "set") where
  isRec = (reserved "rec" $> NRecSet <?> "recursive set")
       <|> pure NSet

parseNixFile :: MonadIO m => FilePath -> m (Result NExpr)
parseNixFile =
    parseFromFileEx $ stripAnnotation <$> (whiteSpace *> nixToplevelForm <* eof)

parseNixFileLoc :: MonadIO m => FilePath -> m (Result NExprLoc)
parseNixFileLoc = parseFromFileEx (whiteSpace *> nixToplevelForm <* eof)

parseNixText :: Text -> Result NExpr
parseNixText =
    parseFromText $ stripAnnotation <$> (whiteSpace *> nixToplevelForm <* eof)

parseNixTextLoc :: Text -> Result NExprLoc
parseNixTextLoc = parseFromText (whiteSpace *> nixToplevelForm <* eof)
