{-# language CPP #-}
{-# language DeriveAnyClass #-}

{-# options_ghc -fno-warn-name-shadowing #-}

-- | Main module for parsing Nix expressions.
module Nix.Parser
  ( parseNixFile
  , parseNixFileLoc
  , parseNixText
  , parseNixTextLoc
  , parseExpr
  , parseFromFileEx
  , Parser
  , parseFromText
  , Result
  , reservedNames
  , NAssoc(..)
  , NOpPrecedence(..)
  , NOpName(..)
  , NSpecialOp(..)
  , NOperatorDef(..)
  , nixExpr
  , nixExprAlgebra
  , nixSet
  , nixBinders
  , nixSelector
  , nixSym
  , nixPath
  , nixString
  , nixUri
  , nixSearchPath
  , nixFloat
  , nixInt
  , nixBool
  , nixNull
  , whiteSpace

  --  2022-01-26: NOTE: Try to hide it after OperatorInfo is removed
  , NOp(..)
  , appOpDef
  )
where

import           Nix.Prelude             hiding ( (<|>)
                                                , some
                                                , many
                                                )
import           Data.Foldable                  ( foldr1 )

import           Control.Monad                  ( msum )
import           Control.Monad.Combinators.Expr ( makeExprParser
                                                , Operator( Postfix
                                                          , InfixN
                                                          , InfixR
                                                          , Prefix
                                                          , InfixL
                                                          )
                                                )
import           Data.Char                      ( isAlpha
                                                , isDigit
                                                , isSpace
                                                )
import           Data.Data                      ( Data(..) )
import           Data.Foldable                  ( minimum )
import           Data.List.Extra                ( groupSort )
import           Data.Fix                       ( Fix(..) )
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import qualified Data.Text                     as Text
import           Nix.Expr.Types
import           Nix.Expr.Shorthands     hiding ( ($>) )
import           Nix.Expr.Types.Annotated
import           Nix.Expr.Strings               ( escapeCodes
                                                , stripIndent
                                                , mergePlain
                                                , removeEmptyPlains
                                                )
import           Nix.Render                     ( MonadFile() )
import           Prettyprinter                  ( Doc
                                                , pretty
                                                )
-- `parser-combinators` ships performance enhanced & MonadPlus-aware combinators.
-- For example `some` and `many` impoted here.
import           Text.Megaparsec         hiding ( (<|>)
                                                , State
                                                )
import           Text.Megaparsec.Char           ( space1
                                                , letterChar
                                                , char
                                                )
import qualified Text.Megaparsec.Char.Lexer    as Lexer


type Parser = ParsecT Void Text (State SourcePos)

-- * Utils

-- | Different to @isAlphaNum@
isAlphanumeric :: Char -> Bool
isAlphanumeric x = isAlpha x || isDigit x
{-# INLINABLE isAlphanumeric #-}

-- | Alternative "<|>" with additional preservation of 'MonadPlus' constraint.
infixl 3 <|>
(<|>) :: MonadPlus m => m a -> m a -> m a
(<|>) = mplus

-- ** Annotated

annotateLocation1 :: Parser a -> Parser (AnnUnit SrcSpan a)
annotateLocation1 p =
  do
    begin <- getSourcePos
    res   <- p
    end   <- get -- The state set before the last whitespace

    pure $ AnnUnit (SrcSpan (toNSourcePos begin) (toNSourcePos end)) res

annotateLocation :: Parser (NExprF NExprLoc) -> Parser NExprLoc
annotateLocation = (annUnitToAnn <$>) . annotateLocation1

annotateNamedLocation :: String -> Parser (NExprF NExprLoc) -> Parser NExprLoc
annotateNamedLocation name = annotateLocation . label name


-- ** Grammar

reservedNames :: HashSet VarName
reservedNames =
  HS.fromList
    ["let", "in", "if", "then", "else", "assert", "with", "rec", "inherit"]

reservedEnd :: Char -> Bool
reservedEnd x =
  isSpace x || (`elem` ("{([})];:.\"'," :: String)) x
{-# INLINABLE reservedEnd #-}

reserved :: Text -> Parser ()
reserved n =
  lexeme $ try $ chunk n *> lookAhead (void (satisfy reservedEnd) <|> eof)

exprAfterSymbol :: Char -> Parser NExprLoc
exprAfterSymbol p = symbol p *> nixExpr

exprAfterReservedWord :: Text -> Parser NExprLoc
exprAfterReservedWord word = reserved word *> nixExpr

-- | A literal copy of @megaparsec@ one but with addition of the @\r@ for Windows EOL case (@\r\n@).
-- Overall, parser should simply @\r\n -> \n@.
skipLineComment' :: Tokens Text -> Parser ()
skipLineComment' prefix =
  chunk prefix *> void (takeWhileP (pure "character") $ \x -> x /= '\n' && x /= '\r')

whiteSpace :: Parser ()
whiteSpace =
  do
    put =<< getSourcePos
    Lexer.space space1 lineCmnt blockCmnt
 where
  lineCmnt  = skipLineComment' "#"
  blockCmnt = Lexer.skipBlockComment "/*" "*/"

-- | Lexeme is a unit of the language.
-- Convention is that after lexeme an arbitrary amount of empty entities (space, comments, line breaks) are allowed.
-- This lexeme definition just skips over superflous @megaparsec: lexeme@ abstraction.
lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

symbol :: Char -> Parser Char
symbol = lexeme . char

symbols :: Text -> Parser Text
symbols = lexeme . chunk

-- We restrict the type of 'parens' and 'brackets' here because if they were to
-- take a 'Parser NExprLoc' argument they would parse additional text which
-- wouldn't be captured in the source location annotation.
--
-- Braces and angles in hnix don't enclose a single expression so this type
-- restriction would not be useful.
parens :: Parser (NExprF f) -> Parser (NExprF f)
parens   = on between symbol '(' ')'

braces :: Parser a -> Parser a
braces   = on between symbol '{' '}'

brackets :: Parser (NExprF f) -> Parser (NExprF f)
brackets = on between symbol '[' ']'

antiquotedIsHungryForTrailingSpaces :: Bool -> Parser (Antiquoted v NExprLoc)
antiquotedIsHungryForTrailingSpaces hungry = Antiquoted <$> (antiStart *> nixExpr <* antiEnd)
 where
  antiStart :: Parser Text
  antiStart = label "${" $ symbols "${"

  antiEnd :: Parser Char
  antiEnd = label "}" $
    if hungry
      then lexeme (char '}')
      else char '}'

antiquotedLexeme :: Parser (Antiquoted v NExprLoc)
antiquotedLexeme = antiquotedIsHungryForTrailingSpaces True

antiquoted :: Parser (Antiquoted v NExprLoc)
antiquoted = antiquotedIsHungryForTrailingSpaces False

---------------------------------------------------------------------------------

-- * Parser parts

-- ** Constrants

nixNull :: Parser NExprLoc
nixNull =
  annotateNamedLocation "null" $
    mkNullF <$ reserved "null"

nixBool :: Parser NExprLoc
nixBool =
  annotateNamedLocation "bool" $
    on (<|>) lmkBool (True, "true") (False, "false")
 where
  lmkBool (b, txt) = mkBoolF b <$ reserved txt

integer :: Parser Integer
integer = lexeme Lexer.decimal

-- | Parse an integer literal and validate it fits in Int64 bounds.
-- Nix uses 64-bit signed integers and errors on out-of-range literals.
int64 :: Parser Int64
int64 = do
  n <- integer
  if n > fromIntegral (maxBound :: Int64) || n < fromIntegral (minBound :: Int64)
    then fail $ "invalid integer '" <> show n <> "'"
    else pure (fromIntegral n)

nixInt :: Parser NExprLoc
nixInt =
  annotateNamedLocation "integer" $
    mkIntF <$> int64

float :: Parser Double
float = lexeme Lexer.float

nixFloat :: Parser NExprLoc
nixFloat =
  annotateNamedLocation "float" $
    try $
      mkFloatF . realToFrac <$> float

nixUri :: Parser NExprLoc
nixUri =
  lexeme $
    annotateLocation $
      try $
        do
          start    <- letterChar
          protocol <-
            takeWhileP mempty $
              \ x ->
                isAlphanumeric x
                || (`elem` ("+-." :: String)) x
          _       <- single ':'
          address <-
            takeWhile1P mempty $
                \ x ->
                  isAlphanumeric x
                  || (`elem` ("%/?:@&=+$,-_.!~*'" :: String)) x
          pure . NStr . DoubleQuoted . one . Plain $ start `Text.cons` protocol <> ":" <> address


-- ** Strings

nixAntiquoted :: Parser a -> Parser (Antiquoted a NExprLoc)
nixAntiquoted p =
  label "anti-quotation" $
    antiquotedLexeme
    <|> Plain <$> p

escapeCode :: Parser Char
escapeCode =
  msum
    [ c <$ char e | (c, e) <- escapeCodes ]
  <|> anySingle

stringChar
  :: Parser ()
  -> Parser ()
  -> Parser (Antiquoted Text NExprLoc)
  -> Parser (Antiquoted Text NExprLoc)
stringChar end escStart esc =
  antiquoted
  <|> Plain . one <$> char '$'
  <|> esc
  <|> Plain . fromString <$> some plainChar
  where
  plainChar :: Parser Char
  plainChar =
    notFollowedBy (end <|> void (char '$') <|> escStart) *> anySingle

doubleQuoted :: Parser (NString NExprLoc)
doubleQuoted =
  label "double quoted string" $
    DoubleQuoted . removeEmptyPlains . mergePlain <$>
      inQuotationMarks (many $ stringChar quotationMark (void $ char '\\') doubleEscape)
  where
  inQuotationMarks :: Parser a -> Parser a
  inQuotationMarks expr = quotationMark *> expr <* quotationMark

  quotationMark :: Parser ()
  quotationMark = void $ char '"'

  doubleEscape :: Parser (Antiquoted Text r)
  doubleEscape = Plain . one <$> (char '\\' *> escapeCode)


indented :: Parser (NString NExprLoc)
indented =
  label "indented string" $
    stripIndent <$>
      inIndentedQuotation (many $ join stringChar indentedQuotationMark indentedEscape)
 where
  -- | Read escaping inside of the "'' <expr> ''"
  indentedEscape :: Parser (Antiquoted Text r)
  indentedEscape =
    try $
      do
        indentedQuotationMark
        Plain <$> ("''" <$ char '\'' <|> "$" <$ char '$')
          <|>
            do
              c <- char '\\' *> escapeCode

              pure $
                if c /= '\n'
                  then Plain $ one c
                  else EscapedNewline

  -- | Enclosed into indented quatation "'' <expr> ''"
  inIndentedQuotation :: Parser a -> Parser a
  inIndentedQuotation expr = indentedQuotationMark *> expr <* indentedQuotationMark

  -- | Symbol "''"
  indentedQuotationMark :: Parser ()
  indentedQuotationMark = label "\"''\"" . void $ chunk "''"


nixString' :: Parser (NString NExprLoc)
nixString' = label "string" $ lexeme $ doubleQuoted <|> indented

nixString :: Parser NExprLoc
nixString = annNStr <$> annotateLocation1 nixString'


-- ** Names (variables aka symbols)

identifier :: Parser VarName
identifier =
  lexeme $
    try $
      do
        iD <-
          liftA2 Text.cons
            (satisfy (\x -> isAlpha x || x == '_'))
            (takeWhileP mempty identLetter)
        let varName = mkVarName iD
        guard $ not $ varName `HS.member` reservedNames
        pure varName
 where
  identLetter x = isAlphanumeric x || x == '_' || x == '\'' || x == '-'

nixSym :: Parser NExprLoc
nixSym = annotateLocation $ mkSymF . varNameText <$> identifier


-- ** ( ) parens

-- | 'nixExpr' returns an expression annotated with a source position,
-- however this position doesn't include the parsed parentheses, so remove the
-- "inner" location annotateion and annotate again, including the parentheses.
nixParens :: Parser NExprLoc
nixParens =
  annotateNamedLocation "parens" $
    parens $ stripAnnF . unFix <$> nixExpr


-- ** [ ] list

nixList :: Parser NExprLoc
nixList =
  annotateNamedLocation "list" $
    brackets $ NList <$> many nixTerm


-- ** { } set

-- | Merge duplicate attribute bindings when both are attrsets (like Nix).
-- Throws parse error for non-mergeable duplicates.
--
-- For example:
--   { a.x = 1; a = { y = 2; }; }  ->  { a = { x = 1; y = 2; }; }
--   { a.x = 1; a = 2; }           ->  error: attribute 'a' already defined
mergeBindings :: [Binding NExprLoc] -> Parser [Binding NExprLoc]
mergeBindings binds = do
  let -- Tag each binding with its index to preserve order
      indexed = zip [0::Int ..] binds
      -- Separate static-key bindings (that might need merging) from others
      (staticBinds, otherBinds) = partitionBindings indexed
      -- Group static bindings by top-level key
      grouped = groupByTopKey staticBinds
  -- Process each group (merge if needed)
  mergedWithIdx <- concat <$> traverse (uncurry mergeGroup) (HM.toList grouped)
  -- Combine with other bindings and sort by original index to preserve order
  let allBindings = mergedWithIdx <> otherBinds
  pure $ map snd $ sortBy (comparing fst) allBindings
 where
  -- Partition into static key bindings and others (dynamic keys, inherits)
  -- Keep the original index for ordering
  -- Note: Plain quoted strings like "foo" are treated as static keys for merging purposes
  partitionBindings :: [(Int, Binding NExprLoc)]
                    -> ([(Int, VarName, [NKeyName NExprLoc], NExprLoc, NSourcePos)], [(Int, Binding NExprLoc)])
  partitionBindings = foldr go ([], [])
   where
    go (idx, b) (statics, others) = case b of
      NamedVar (StaticKey k :| rest) val pos -> ((idx, k, rest, val, pos) : statics, others)
      -- Plain quoted strings (no interpolation) should be treated as static keys
      NamedVar (DynamicKey dk :| rest) val pos
        | Just k <- staticStringFromDynamic dk -> ((idx, k, rest, val, pos) : statics, others)
      _ -> (statics, (idx, b) : others)

  -- Extract a static string from a DynamicKey if it's a plain string (no interpolation)
  -- "foo" -> Just "foo", ${x} -> Nothing, "${x}" -> Nothing
  staticStringFromDynamic :: Antiquoted (NString NExprLoc) NExprLoc -> Maybe VarName
  staticStringFromDynamic (Plain (DoubleQuoted parts)) = extractPlainText parts
  staticStringFromDynamic (Plain (Indented _ parts))   = extractPlainText parts
  staticStringFromDynamic _ = Nothing  -- Antiquoted expressions are truly dynamic

  -- Extract plain text from string parts, returning Nothing if any part is interpolated
  -- or if the result would be empty (empty strings remain dynamic keys)
  extractPlainText :: [Antiquoted Text NExprLoc] -> Maybe VarName
  extractPlainText parts =
    case traverse getPlain parts of
      Just texts
        | let t = mconcat texts
        , not (Text.null t) -> Just $ mkVarName t
      _ -> Nothing
   where
    getPlain (Plain t) = Just t
    getPlain _         = Nothing  -- Antiquoted or EscapedNewline

  -- Group static bindings by their top-level key, keeping indices
  groupByTopKey :: [(Int, VarName, [NKeyName NExprLoc], NExprLoc, NSourcePos)]
                -> HM.HashMap VarName [(Int, [NKeyName NExprLoc], NExprLoc, NSourcePos)]
  groupByTopKey = HM.fromListWith (<>) . fmap (\(idx, k, rest, val, pos) -> (k, [(idx, rest, val, pos)]))

  -- Merge a group of bindings with the same top-level key
  -- Returns bindings paired with their minimum index (for ordering)
  mergeGroup :: VarName -> [(Int, [NKeyName NExprLoc], NExprLoc, NSourcePos)] -> Parser [(Int, Binding NExprLoc)]
  mergeGroup key [(idx, rest, val, pos)] =
    -- Single binding: reconstruct it
    pure [(idx, NamedVar (StaticKey key :| rest) val pos)]
  mergeGroup key bindings
    -- Error: multiple direct non-attrset bindings (e.g., { a = 1; a = 2; })
    | (p1:_:_) <- directNonAttrsetPositions =
        fail $ "attribute '" <> toString (varNameText key) <> "' already defined at " <> show p1
    -- Error: direct non-attrset mixed with nested paths (e.g., { a.b = 1; a = 2; })
    | (pos:_) <- directNonAttrsetPositions
    , any hasNestedPath bindings =
        fail $ "attribute '" <> toString (varNameText key) <> "' already defined at " <> show pos
    -- Merge: at least one direct attrset (e.g., { a = { x = 1; }; a.y = 2; })
    | any isDirectAttrset bindings = do
        merged <- mergeMultiple key bindings
        -- Use minimum index for the merged binding's position in output
        let minIdx = minimum $ map (\(idx, _, _, _) -> idx) bindings
        pure [(minIdx, merged)]
    -- No merge: all are nested paths (e.g., { a.b = 1; a.c = 2; })
    | otherwise =
        pure [(idx, NamedVar (StaticKey key :| rest) val pos) | (idx, rest, val, pos) <- bindings]
   where
    isDirectAttrset (_, [], val, _) = case unFix val of
      AnnF _ (NSet _ _) -> True
      _ -> False
    isDirectAttrset _ = False

    directNonAttrsetPositions :: [NSourcePos]
    directNonAttrsetPositions =
      [ pos | (_, [], val, pos) <- bindings
      , case unFix val of
          AnnF _ (NSet _ _) -> False
          _ -> True
      ]

    hasNestedPath :: (Int, [NKeyName NExprLoc], NExprLoc, NSourcePos) -> Bool
    hasNestedPath (_, rest, _, _) = not (null rest)

  -- Merge multiple bindings for the same key
  mergeMultiple :: VarName -> [(Int, [NKeyName NExprLoc], NExprLoc, NSourcePos)] -> Parser (Binding NExprLoc)
  mergeMultiple key bindings = do
    -- Collect all nested bindings (strip indices, they were only for ordering)
    let bindingsNoIdx = [(rest, val, pos) | (_, rest, val, pos) <- bindings]
    nestedBindsWithRec <- traverse (extractNestedBinding key) bindingsNoIdx
    let allBindings = concatMap snd nestedBindsWithRec
        -- If ANY direct attrset is recursive, the merged result is recursive
        mergedRec = case [r | (r, _) <- nestedBindsWithRec, r == Recursive] of
                      (_:_) -> Recursive
                      []    -> NonRecursive
        -- Use the first position for the merged binding
        firstPos = case bindingsNoIdx of
          ((_, _, pos) : _) -> pos
          [] -> error "mergeMultiple: impossible empty list"
    -- Recursively merge the collected bindings (handles nested conflicts)
    mergedInner <- mergeBindings allBindings
    -- Construct the merged NSet, preserving recursivity from direct attrsets
    let mergedSet = mkNSet mergedRec mergedInner
    pure $ NamedVar (one $ StaticKey key) mergedSet firstPos

  -- Extract bindings from a single entry (either nested path or direct NSet)
  -- Returns (Recursivity, bindings) - Recursive if from a rec attrset, NonRecursive otherwise
  extractNestedBinding :: VarName -> ([NKeyName NExprLoc], NExprLoc, NSourcePos) -> Parser (Recursivity, [Binding NExprLoc])
  extractNestedBinding key ([], val, pos) =
    -- Direct binding: value must be an NSet to merge
    case unFix val of
      AnnF _ (NSet rec innerBinds) -> pure (rec, innerBinds)  -- Preserve recursivity!
      _ -> fail $ "attribute '" <> toString (varNameText key) <> "' already defined at " <> show pos
  extractNestedBinding _key (rest, val, pos) =
    -- Nested path: convert to a binding (not from a direct attrset, so NonRecursive)
    pure (NonRecursive, [NamedVar (fromList rest) val pos])

  -- Helper to construct an NSet with source span
  mkNSet :: Recursivity -> [Binding NExprLoc] -> NExprLoc
  mkNSet rec binds' =
    -- Use a dummy source span; this will be overwritten by annotation
    Fix $ AnnF dummySpan $ NSet rec binds'

  dummySpan :: SrcSpan
  dummySpan = SrcSpan (toNSourcePos dummyPos) (toNSourcePos dummyPos)
   where
    dummyPos = SourcePos "" (mkPos 1) (mkPos 1)

nixBinders :: Parser [Binding NExprLoc]
nixBinders = mergeBindings =<< (inherit <|> namedVar) `endBy` symbol ';' where
  inherit =
    do
      -- We can't use 'reserved' here because it would consume the whitespace
      -- after the keyword, which is not exactly the semantics of C++ Nix.
      try $ chunk "inherit" *> lookAhead (void $ satisfy reservedEnd)
      p <- getSourcePos
      x <- whiteSpace *> optional scope
      label "inherited binding" $
        liftA2 (Inherit x)
          (many inheritedIdentifier)
          (pure (toNSourcePos p))
  -- | Parse an identifier for inherit - either a regular identifier or a quoted string
  -- (for reserved keywords like "or"). Quoted strings must be static (no interpolation).
  inheritedIdentifier = identifier <|> quotedIdentifier
  quotedIdentifier = lexeme $ do
    str <- doubleQuoted
    case str of
      DoubleQuoted [Plain t] -> pure $ mkVarName t
      _ -> fail "dynamic attributes not allowed in inherit"
  namedVar =
    do
      p <- getSourcePos
      label "variable binding" $
        liftA3 NamedVar
          (annotated <$> nixSelector)
          (exprAfterSymbol '=')
          (pure (toNSourcePos p))
  scope = label "inherit scope" nixParens

nixSet :: Parser NExprLoc
nixSet =
  annotateNamedLocation "set" $
    isRec <*> braces nixBinders
 where
  isRec =
    label "recursive set" (reserved "rec" $> NSet Recursive)
    <|> pure (NSet mempty)

-- ** /x/y/z literal Path

pathChar :: Char -> Bool
pathChar x =
  isAlphanumeric x || (`elem` ("._-+~" :: String)) x

slash :: Parser Char
slash =
  label "slash " $
    try $
      char '/' <* notFollowedBy (satisfy $ \x -> x == '/' || x == '*' || isSpace x)

pathPieces :: Parser ([Antiquoted Text NExprLoc], Bool)
pathPieces =
  lexeme $
    do
      pieces <- some pathPiece
      let hasSlash = or $ snd <$> pieces
      let parts = removeEmptyPlains . mergePlain $ fst <$> pieces
      pure (parts, hasSlash)
 where
  pathPiece :: Parser (Antiquoted Text NExprLoc, Bool)
  pathPiece =
    (,(False)) <$> antiquoted
      <|> (,(True)) . Plain . one <$> slash
      <|> (,(False)) . Plain . fromString <$> some (satisfy pathChar)

nixPath :: Parser NExprLoc
nixPath =
  annotateNamedLocation "path" $
    try $
      do
        (parts, hasSlash) <- pathPieces
        guard hasSlash
        let hasAntiquote =
              any
                (\case
                  Antiquoted{} -> True
                  _ -> False
                )
                parts
        if hasAntiquote
          then do
            -- For paths with antiquotes, check the last part isn't just a slash
            guard $ isValidPathParts parts
            pure $ mkPathStrF $ DoubleQuoted parts
          else
            case traverse getPlain parts of
              Just texts -> do
                let pathStr = Text.concat texts
                guard $ isValidPathStr pathStr
                pure $ mkPathF False $ toString pathStr
              Nothing -> empty
  where
    getPlain :: Antiquoted Text NExprLoc -> Maybe Text
    getPlain = \case
      Plain t -> Just t
      _ -> Nothing

    -- | Check that a path string doesn't end with "/" and isn't just a root/prefix
    isValidPathStr :: Text -> Bool
    isValidPathStr t =
      not (Text.isSuffixOf "/" t) &&
      -- Reject paths that are just "/", "~/", "./", "../" etc. with no content
      t `notElem` ["/", "~", "~/", ".", "./", "..", "../"]

    -- | Check that path parts don't end with just a slash
    isValidPathParts :: [Antiquoted Text r] -> Bool
    isValidPathParts parts = case reverse parts of
      [] -> False
      (Plain t : _) -> not (Text.isSuffixOf "/" t) && t /= "/"
      _ -> True  -- Ends with antiquote, which is OK


-- ** <<x>> environment path

-- | A path surrounded by angle brackets, indicating that it should be
-- looked up in the NIX_PATH environment variable at evaluation.
nixSearchPath :: Parser NExprLoc
nixSearchPath =
  annotateNamedLocation "spath" $
    mkPathF True <$> try (lexeme $ char '<' *> many (satisfy pathChar <|> slash) <* char '>')


-- ** Operators

--  2022-01-26: NOTE: Rename to 'literal'
newtype NOpName = NOpName Text
  deriving
    (Eq, Ord, Generic, Typeable, Data, Show, NFData)

instance IsString NOpName where
  fromString = coerce . fromString @Text

instance ToString NOpName where
  toString = toString @Text . coerce

operator :: NOpName -> Parser Text
operator (coerce -> op) =
  case op of
    c@"-" -> c `without` '>'
    c@"/" -> c `without` '/'
    c@"<" -> c `without` '='
    c@">" -> c `without` '='
    n   -> symbols n
 where
  without :: Text -> Char -> Parser Text
  without opChar noNextChar =
    lexeme . try $ chunk opChar <* notFollowedBy (char noNextChar)

opWithLoc :: (AnnUnit SrcSpan o -> a) -> o -> NOpName -> Parser a
opWithLoc f op name = f . (op <$) <$> annotateLocation1 (operator name)

--  2022-01-26: NOTE: Make presedence free and type safe by moving it into type level:
--  https://youtu.be/qaPdg0mZavM?t=1757
--  https://wiki.haskell.org/The_Monad.Reader/Issue5/Number_Param_Types
newtype NOpPrecedence = NOpPrecedence Int
  deriving (Eq, Ord, Generic, Bounded, Typeable, Data, Show, NFData)

instance Enum NOpPrecedence where
  toEnum = coerce
  fromEnum = coerce

instance Num NOpPrecedence where
  (+) = coerce ((+) @Int)
  (*) = coerce ((*) @Int)
  abs = coerce (abs @Int)
  signum = coerce (signum @Int)
  fromInteger = coerce (fromInteger @Int)
  negate = coerce (negate @Int)

--  2022-01-26: NOTE: This type belongs into 'Type.Expr' & be used in NExprF.
data NAppOp = NAppOp
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

--  2022-01-26: NOTE: This type belongs into 'Type.Expr' & be used in NExprF.
data NSpecialOp
  = NHasAttrOp
  | NSelectOp
  | NTerm -- ^ For special handling of internal special cases.
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

data NAssoc
  = NAssocLeft
  -- Nota bene: @parser-combinators@ named "associative property" as 'InfixN' stating it as "non-associative property".
  -- Binary operators having some associativity is a basis property in mathematical algebras in use (for example, in Category theory). Having no associativity in operators makes theory mostly impossible in use and so non-associativity is not encountered in notations, therefore under 'InfixN' @parser-combinators@ meant "associative".
  -- | Bidirectional associativity, or simply: associative property.
  | NAssoc
  | NAssocRight
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

--  2022-01-31: NOTE: This type and related typeclasses & their design, probably need a refinement.
--
-- In the "Nix.Pretty", the code probably should be well-typed to the type of operations its processes.
-- Therefor splitting operation types into separate types there is probably needed.
--
-- After that:
--
-- > { NAssoc, NOpPrecedence, NOpName }
--
-- Can be formed into a type.
--
-- Also 'NAppDef' really has only 1 implementation, @{ NAssoc, NOpPrecedence, NOpName }@
-- were added there only to make type uniformal.
-- All impossible cases ideally should be unrepresentable.
-- | Single operator grammar entries.
data NOperatorDef
  = NAppDef     NAppOp     NAssoc NOpPrecedence NOpName
  | NUnaryDef   NUnaryOp   NAssoc NOpPrecedence NOpName
  | NBinaryDef  NBinaryOp  NAssoc NOpPrecedence NOpName
  | NSpecialDef NSpecialOp NAssoc NOpPrecedence NOpName
  --  2022-01-26: NOTE: Ord can be the order of evaluation of precedence (which 'Pretty' printing also accounts for).
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

-- Supplied since its definition gets called/used frequently.
-- | Functional application operator definition, left associative, high precedence.
appOpDef :: NOperatorDef
appOpDef = NAppDef NAppOp NAssocLeft 1 " " -- This defined as "2" in Nix lang spec.

-- | Extract associativity from any NOperatorDef
opDefAssoc :: NOperatorDef -> NAssoc
opDefAssoc = \case
  NAppDef     _ assoc _ _ -> assoc
  NUnaryDef   _ assoc _ _ -> assoc
  NBinaryDef  _ assoc _ _ -> assoc
  NSpecialDef _ assoc _ _ -> assoc

-- | Extract precedence from any NOperatorDef
opDefPrecedence :: NOperatorDef -> NOpPrecedence
opDefPrecedence = \case
  NAppDef     _ _ prec _ -> prec
  NUnaryDef   _ _ prec _ -> prec
  NBinaryDef  _ _ prec _ -> prec
  NSpecialDef _ _ prec _ -> prec

-- | Extract name from any NOperatorDef
opDefName :: NOperatorDef -> NOpName
opDefName = \case
  NAppDef     _ _ _ name -> name
  NUnaryDef   _ _ _ name -> name
  NBinaryDef  _ _ _ name -> name
  NSpecialDef _ _ _ name -> name

-- | Class to get a private free construction to abstract away the gap between the Nix operation types
-- 'NUnaryOp', 'NBinaryOp', 'NSpecialOp'.
class NOp a where
  {-# MINIMAL getOpDef #-}

  getOpDef :: a -> NOperatorDef

  getOpAssoc :: a -> NAssoc
  getOpAssoc = opDefAssoc . getOpDef

  getOpPrecedence :: a -> NOpPrecedence
  getOpPrecedence = opDefPrecedence . getOpDef

  getOpName :: a -> NOpName
  getOpName = opDefName . getOpDef

instance NOp NAppOp where
  getOpDef NAppOp = appOpDef

instance NOp NUnaryOp where
  getOpDef = \case
    NNeg -> NUnaryDef NNeg NAssocRight 3 "-"
    NNot -> NUnaryDef NNot NAssocRight 8 "!"

instance NOp NBinaryOp where
  getOpDef = \case
    NConcat -> NBinaryDef NConcat NAssocRight  5 "++"
    NMult   -> NBinaryDef NMult   NAssocLeft   6 "*"
    NDiv    -> NBinaryDef NDiv    NAssocLeft   6 "/"
    NPlus   -> NBinaryDef NPlus   NAssocLeft   7 "+"
    NMinus  -> NBinaryDef NMinus  NAssocLeft   7 "-"
    NUpdate -> NBinaryDef NUpdate NAssocRight  9 "//"
    NLt     -> NBinaryDef NLt     NAssocLeft  10 "<"
    NLte    -> NBinaryDef NLte    NAssocLeft  10 "<="
    NGt     -> NBinaryDef NGt     NAssocLeft  10 ">"
    NGte    -> NBinaryDef NGte    NAssocLeft  10 ">="
    NEq     -> NBinaryDef NEq     NAssoc      11 "=="
    NNEq    -> NBinaryDef NNEq    NAssoc      11 "!="
    NAnd    -> NBinaryDef NAnd    NAssocLeft  12 "&&"
    NOr     -> NBinaryDef NOr     NAssocLeft  13 "||"
    NImpl   -> NBinaryDef NImpl   NAssocRight 14 "->"

instance NOp NSpecialOp where
  getOpDef = \case
    NSelectOp  -> NSpecialDef NSelectOp  NAssocLeft 1 "."
    NHasAttrOp -> NSpecialDef NHasAttrOp NAssocLeft 4 "?"
    NTerm      -> NSpecialDef NTerm      NAssocLeft 1 "???"

instance NOp NOperatorDef where
  getOpDef = id

prefix :: NUnaryOp -> Operator Parser NExprLoc
prefix op =
  Prefix $ manyUnaryOp $ opWithLoc annNUnary op $ getOpName op
-- postfix name op = (NUnaryDef name op,
--                    Postfix (opWithLoc annNUnary op name))

manyUnaryOp :: MonadPlus f => f (a -> a) -> f (a -> a)
manyUnaryOp f = foldr1 (.) <$> some f

binary
  :: NBinaryOp
  -> Operator Parser NExprLoc
binary op =
  mapAssocToInfix (getOpAssoc op) $ opWithLoc annNBinary op (getOpName op)

mapAssocToInfix :: NAssoc -> m (a -> a -> a) -> Operator m a
mapAssocToInfix NAssocLeft  = InfixL
mapAssocToInfix NAssoc      = InfixN
mapAssocToInfix NAssocRight = InfixR

-- ** x: y lambda function

-- | Gets all of the arguments for a function.
argExpr :: Parser (Params NExprLoc)
argExpr =
  msum
    [ atLeft
    , onlyname
    , atRight
    ]
  <* symbol ':'
 where
  -- An argument not in curly braces. There's some potential ambiguity
  -- in the case of, for example `x:y`. Is it a lambda function `x: y`, or
  -- a URI `x:y`? Nix syntax says it's the latter. So we need to fail if
  -- there's a valid URI parse here.
  onlyname =
    msum
      [ nixUri *> unexpected (Label $ fromList "valid uri" )
      , Param <$> identifier
      ]

  -- Parameters named by an identifier on the left (`args @ {x, y}`)
  atLeft =
    try $
      do
        name             <- identifier <* symbol '@'
        (variadic, pset) <- params
        pure $ ParamSet (pure name) variadic (HM.fromList pset)

  -- Parameters named by an identifier on the right, or none (`{x, y} @ args`)
  atRight =
    do
      (variadic, pset) <- params
      name             <- optional $ symbol '@' *> identifier
      pure $ ParamSet name variadic (HM.fromList pset)

  -- Return the parameters set.
  params = braces getParams

  -- Collects the parameters within curly braces. Returns the parameters and
  -- an flag indication if the parameters are variadic.
  getParams :: Parser (Variadic, [(VarName, Maybe NExprLoc)])
  getParams = go mempty
   where
    -- Attempt to parse `...`. If this succeeds, stop and return True.
    -- Otherwise, attempt to parse an argument, optionally with a
    -- default. If this fails, then return what has been accumulated
    -- so far.
    go :: [(VarName, Maybe NExprLoc)] -> Parser (Variadic, [(VarName, Maybe NExprLoc)])
    go acc = ((Variadic, acc) <$ symbols "...") <|> getMore
     where
      getMore :: Parser (Variadic, [(VarName, Maybe NExprLoc)])
      getMore =
        -- Could be nothing, in which just return what we have so far.
        option (mempty, acc) $
          do
            -- Get an argument name and an optional default.
            pair <-
              liftA2 (,)
                identifier
                (optional $ exprAfterSymbol '?')

            let args = acc <> one pair

            -- Either return this, or attempt to get a comma and restart.
            option (mempty, args) $ symbol ',' *> go args

nixLambda :: Parser NExprLoc
nixLambda =
  liftA2 annNAbs
    (annotateLocation1 $ try argExpr)
    nixExpr


-- ** let expression

nixLet :: Parser NExprLoc
nixLet =
  annotateNamedLocation "let block" $
    reserved "let" *> (letBody <|> letBinders)
 where
  -- | Expressions `let {..., body = ...}' are just desugared
  -- into `(rec {..., body = ...}).body'.
  letBody    = (\ expr -> NSelect Nothing expr (one $ StaticKey "body")) <$> attrset
   where
    attrset       = annotateLocation $ NSet Recursive <$> braces nixBinders
  -- | Regular `let`
  letBinders =
    liftA2 NLet
      nixBinders
      (exprAfterReservedWord "in")

-- ** if then else

nixIf :: Parser NExprLoc
nixIf =
  annotateNamedLocation "if" $
    liftA3 NIf
      (reserved "if"   *> nixExpr)
      (exprAfterReservedWord "then")
      (exprAfterReservedWord "else")

-- ** with

nixWith :: Parser NExprLoc
nixWith =
  annotateNamedLocation "with" $
    liftA2 NWith
      (exprAfterReservedWord "with")
      (exprAfterSymbol       ';'   )


-- ** assert

nixAssert :: Parser NExprLoc
nixAssert =
  annotateNamedLocation "assert" $
    liftA2 NAssert
      (exprAfterReservedWord "assert")
      (exprAfterSymbol       ';'     )

-- ** . - reference (selector) into attr

selectorDot :: Parser ()
selectorDot = label "." $ try (symbol '.' *> notFollowedBy nixPath)

keyName :: Parser (NKeyName NExprLoc)
keyName = dynamicKey <|> staticKey
 where
  staticKey  = StaticKey <$> identifier
  dynamicKey = DynamicKey <$> nixAntiquoted nixString'

nixSelector :: Parser (AnnUnit SrcSpan (NAttrPath NExprLoc))
nixSelector =
  annotateLocation1 $ fromList <$> keyName `sepBy1` selectorDot

nixSelect :: Parser NExprLoc -> Parser NExprLoc
nixSelect term =
  do
    res <-
      liftA2 builder
        term
        (optional $
          liftA2 (flip (,))
            (selectorDot *> nixSelector)
            (optional $ reserved "or" *> nixTerm)
        )
    continues <- optional $ lookAhead selectorDot

    maybe
      id
      (const nixSelect)
      continues
      (pure res)
 where
  builder
    :: NExprLoc
    -> Maybe
      ( Maybe NExprLoc
      , AnnUnit SrcSpan (NAttrPath NExprLoc)
      )
    -> NExprLoc
  builder t =
    maybe
      t
      (uncurry (`annNSelect` t))


-- ** _ - syntax hole

nixSynHole :: Parser NExprLoc
nixSynHole =
  annotateLocation $ mkSynHoleF . varNameText <$> (char '^' *> identifier)

-- List of Nix operation parsers with their precedence.
opParsers :: [(NOpPrecedence, Operator Parser NExprLoc)]
opParsers =
  -- This is not parsed here, even though technically it's part of the
  -- expression table. The problem is that in some cases, such as list
  -- membership, it's also a term. And since terms are effectively the
  -- highest precedence entities parsed by the expression parser, it ends up
  -- working out that we parse them as a kind of "meta-term".

  -- {-  1 -}
  -- [ ( NSpecialDef "." NSelectOp NAssocLeft
  --   , Postfix $
  --       do
  --         sel <- seldot *> selector
  --         mor <- optional (reserved "or" *> term)
  --         pure $ \x -> annNSelect x sel mor)
  -- ]

  -- NApp is left associative
  -- 2018-05-07: jwiegley: Thanks to Brent Yorgey for showing me this trick!
  specialBuilder NAppOp (InfixL $ annNApp <$ symbols mempty) <>
  specialBuilder NHasAttrOp (Postfix $ symbol '?' *> (flip annNHasAttr <$> nixSelector)) <>
  builder prefix <>
  builder binary
 where
  specialBuilder :: NOp t => t -> b -> [(NOpPrecedence, b)]
  specialBuilder op parser = one (entry op (const parser))

  builder :: (Enum t, Bounded t, NOp t) => (t -> b) -> [(NOpPrecedence, b)]
  builder tp = fmap (`entry` tp) universe

  entry :: NOp t => t -> (t -> b) -> (NOpPrecedence, b)
  entry op parser = (getOpPrecedence op, parser op)


-- ** Expr & its constituents (Language term, expr algebra)

nixTerm :: Parser NExprLoc
nixTerm =
  do
    c <- try . lookAhead . satisfy $
      \x -> (`elem` ("({[</\"'^" :: String)) x || pathChar x
    case c of
      '('  -> nixSelect nixParens
      '{'  -> nixSelect nixSet
      '['  -> nixList
      '<'  -> nixSearchPath
      '/'  -> nixPath
      '"'  -> nixString
      '\'' -> nixString
      '^'  -> nixSynHole
      _ ->
        msum
          $  [ nixSelect nixSet | c == 'r' ]
          <> [ nixPath | pathChar c ]
          <> if isDigit c
              then [ nixFloat, nixInt ]
              else
                [ nixUri | isAlpha c ]
                <> [ nixBool | c == 't' || c == 'f' ]
                <> [ nixNull | c == 'n' ]
                <> one (nixSelect nixSym)

-- | Bundles parsers into @[[]]@ based on precedence (form is required for `megaparsec`).
nixOperators :: [[ Operator Parser NExprLoc ]]
nixOperators =
  snd <$>
    groupSort opParsers

-- | Nix expression algebra parser.
-- "Expression algebra" is to explain @megaparsec@ use of the term "Expression" (parser for language algebraic coperators without any statements (without @let@ etc.)), which is essentially an algebra inside the language.
nixExprAlgebra :: Parser NExprLoc
nixExprAlgebra =
  makeExprParser
    nixTerm
    nixOperators

nixExpr :: Parser NExprLoc
nixExpr = keywords <|> nixLambda <|> nixExprAlgebra
 where
  keywords = nixLet <|> nixIf <|> nixAssert <|> nixWith


-- * Parse

type Result a = Either (Doc Void) a


parseWith
  :: Parser a
  -> Path
  -> Text
  -> Either (Doc Void) a
parseWith parser file input =
  either
    (Left . pretty . errorBundlePretty)
    pure
    $ (`evalState` initialPos (coerce file)) $ (`runParserT` coerce file) parser input


parseFromFileEx :: MonadFile m => Parser a -> Path -> m (Result a)
parseFromFileEx parser file = parseWith parser file <$> readFile file

parseFromText :: Parser a -> Text -> Result a
parseFromText = (`parseWith` "<string>")

fullContent :: Parser NExprLoc
fullContent = whiteSpace *> nixExpr <* eof

parseNixFile' :: MonadFile m => (Parser NExprLoc -> Parser a) -> Path -> m (Result a)
parseNixFile' f =
  parseFromFileEx $ f fullContent

parseNixFile :: MonadFile m => Path -> m (Result NExpr)
parseNixFile =
  parseNixFile' (stripAnnotation <$>)

parseNixFileLoc :: MonadFile m => Path -> m (Result NExprLoc)
parseNixFileLoc =
  parseNixFile' id

parseNixText' :: (Parser NExprLoc -> Parser a) -> Text -> Result a
parseNixText' f =
  parseFromText $ f fullContent

parseNixText :: Text -> Result NExpr
parseNixText =
  parseNixText' (stripAnnotation <$>)

parseNixTextLoc :: Text -> Result NExprLoc
parseNixTextLoc =
  parseNixText' id

parseExpr :: MonadFail m => Text -> m NExpr
parseExpr =
  either
    (fail . show)
    pure
    . parseNixText
