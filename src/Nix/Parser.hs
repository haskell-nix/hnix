{-# language CPP #-}
{-# language DeriveAnyClass #-}

{-# options_ghc -fno-warn-name-shadowing #-}

-- | Main module for parsing Nix expressions.
module Nix.Parser
  ( parseNixFile
  , parseNixFileLoc
  , parseNixText
  , parseNixTextLoc
  , parseFromFileEx
  , Parser
  , parseFromText
  , Result
  , reservedNames
  , OperatorInfo(..)
  , NSpecialOp(..)
  , NAssoc(..)
  , NOperatorDef
  , getUnaryOperator
  , getBinaryOperator
  , getSpecialOperator
  , nixToplevelForm
  , nixExpr
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
  , symbol
  , whiteSpace
  )
where

import           Prelude                 hiding ( (<|>)
                                                , some
                                                , many
                                                , readFile
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
import           Data.Fix                       ( Fix(..) )
import qualified Data.HashSet                  as HashSet
import qualified Data.Map                      as Map
import           Data.Text                      ( cons )
import           Nix.Utils
import           Nix.Expr.Types
import           Nix.Expr.Shorthands     hiding ( ($>) )
import           Nix.Expr.Types.Annotated
import           Nix.Expr.Strings               ( escapeCodes
                                                , stripIndent
                                                , mergePlain
                                                , removePlainEmpty
                                                )
import           Nix.Render                     ( MonadFile(readFile) )
import           Prettyprinter                  ( Doc
                                                , pretty
                                                )
-- `parser-combinators` ships performance enhanced & MonadPlus-aware combinators.
-- For example `smome` and `many` impoted here.
import           Text.Megaparsec         hiding ( (<|>)
                                                , State
                                                )
import           Text.Megaparsec.Char           ( space1
                                                , string
                                                , letterChar
                                                , char
                                                )
import qualified Text.Megaparsec.Char.Lexer    as Lexer

-- | Different to @isAlphaNum@
isAlphanumeric :: Char -> Bool
isAlphanumeric x = isAlpha x || isDigit x
{-# inline isAlphanumeric #-}

-- | @<|>@ with additional preservation of @MonadPlus@ constraint.
infixl 3 <|>
(<|>) :: MonadPlus m => m a -> m a -> m a
(<|>) = mplus

---------------------------------------------------------------------------------

nixExpr :: Parser NExprLoc
nixExpr =
  makeExprParser
    nixTerm $
      snd <<$>>
        nixOperators nixSelector

antiStart :: Parser Text
antiStart = label "${" $ symbol "${"

nixAntiquoted :: Parser a -> Parser (Antiquoted a NExprLoc)
nixAntiquoted p =
  label "anti-quotation" $
    Antiquoted <$>
      (antiStart *> nixToplevelForm <* symbol "}")
        <|> Plain <$> p

selDot :: Parser ()
selDot = label "." $ try (symbol "." *> notFollowedBy nixPath)

nixSelect :: Parser NExprLoc -> Parser NExprLoc
nixSelect term =
  do
    res <-
      liftA2 build
        term
        (optional $
          liftA2 (,)
            (selDot *> nixSelector)
            (optional $ reserved "or" *> nixTerm)
        )
    continues <- optional $ lookAhead selDot

    maybe
      id
      (const nixSelect)
      continues
      (pure res)
 where
  build
    :: NExprLoc
    -> Maybe
      ( AnnUnit SrcSpan (NAttrPath NExprLoc)
      , Maybe NExprLoc
      )
    -> NExprLoc
  build t mexpr =
    maybe
      t
      (\ (a, m) -> (`annNSelect` t) m a)
      mexpr

nixSelector :: Parser (AnnUnit SrcSpan (NAttrPath NExprLoc))
nixSelector =
  annotateLocation1 $
    do
      (x : xs) <- keyName `sepBy1` selDot
      pure $ x :| xs

nixTerm :: Parser NExprLoc
nixTerm = do
  c <- try $ lookAhead $ satisfy $ \x ->
    pathChar x || (`elem` ("({[</\"'^" :: String)) x
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
               <> [ nixSelect nixSym ]

nixToplevelForm :: Parser NExprLoc
nixToplevelForm = keywords <|> nixLambda <|> nixExpr
 where
  keywords = nixLet <|> nixIf <|> nixAssert <|> nixWith

nixSym :: Parser NExprLoc
nixSym = annotateLocation $ mkSymF . coerce <$> identifier

nixSynHole :: Parser NExprLoc
nixSynHole = annotateLocation $ mkSynHoleF . coerce <$> (char '^' *> identifier)

nixInt :: Parser NExprLoc
nixInt =
  annotateNamedLocation "integer" $
    mkIntF <$> integer

nixFloat :: Parser NExprLoc
nixFloat =
  annotateNamedLocation "float" $
    try $
      mkFloatF . realToFrac <$> float

nixBool :: Parser NExprLoc
nixBool =
  annotateNamedLocation "bool" $
    on (<|>) lmkBool (True, "true") (False, "false")
 where
  lmkBool (b, txt) = mkBoolF b <$ reserved txt

nixNull :: Parser NExprLoc
nixNull =
  annotateNamedLocation "null" $
    mkNullF <$ reserved "null"

-- | 'nixTopLevelForm' returns an expression annotated with a source position,
-- however this position doesn't include the parsed parentheses, so remove the
-- "inner" location annotateion and annotate again, including the parentheses.
nixParens :: Parser NExprLoc
nixParens =
  annotateNamedLocation "parens" $
    parens $ stripAnnF . unFix <$> nixToplevelForm

nixList :: Parser NExprLoc
nixList =
  annotateNamedLocation "list" $
    brackets $ NList <$> many nixTerm

pathChar :: Char -> Bool
pathChar x =
  isAlphanumeric x || (`elem` ("._-+~" :: String)) x

slash :: Parser Char
slash =
  label "slash " $
    try $
      char '/' <* notFollowedBy (satisfy $ \x -> x == '/' || x == '*' || isSpace x)

-- | A path surrounded by angle brackets, indicating that it should be
-- looked up in the NIX_PATH environment variable at evaluation.
nixSearchPath :: Parser NExprLoc
nixSearchPath =
  annotateNamedLocation "spath" $
    mkPathF True <$> try (char '<' *> many (satisfy pathChar <|> slash) <* symbol ">")

pathStr :: Parser Path
pathStr =
  lexeme . coerce $
    liftA2 (<>)
      (many $ satisfy pathChar)
      (concat <$>
        some
          (liftA2 (:)
            slash
            (some $ satisfy pathChar)
          )
      )

nixPath :: Parser NExprLoc
nixPath =
  annotateNamedLocation "path" $
    try $ mkPathF False <$> coerce pathStr

nixLet :: Parser NExprLoc
nixLet =
  annotateNamedLocation "let block" $
    reserved "let" *> (letBody <|> letBinders)
 where
  letBinders =
    liftA2 NLet
      nixBinders
      (reserved "in" *> nixToplevelForm)
  -- Let expressions `let {..., body = ...}' are just desugared
  -- into `(rec {..., body = ...}).body'.
  letBody    = (\x -> NSelect Nothing x (StaticKey "body" :| mempty)) <$> aset
  aset       = annotateLocation $ NSet Recursive <$> braces nixBinders

nixIf :: Parser NExprLoc
nixIf =
  annotateNamedLocation "if" $
    liftA3 NIf
      (reserved "if"   *> nixExpr        )
      (reserved "then" *> nixToplevelForm)
      (reserved "else" *> nixToplevelForm)

nixAssert :: Parser NExprLoc
nixAssert =
  annotateNamedLocation "assert" $
    liftA2 NAssert
      (reserved "assert" *> nixToplevelForm)
      (semi              *> nixToplevelForm)

nixWith :: Parser NExprLoc
nixWith =
  annotateNamedLocation "with" $
    liftA2 NWith
      (reserved "with" *> nixToplevelForm)
      (semi            *> nixToplevelForm)

nixLambda :: Parser NExprLoc
nixLambda =
  liftA2 annNAbs
    (annotateLocation1 $ try argExpr)
    nixToplevelForm

nixString :: Parser NExprLoc
nixString = annNStr <$> annotateLocation1 nixString'

nixUri :: Parser NExprLoc
nixUri = lexeme $ annotateLocation $ try $ do
  start    <- letterChar
  protocol <- many $
    satisfy $
      \ x ->
        isAlphanumeric x
        || (`elem` ("+-." :: String)) x
  _       <- string ":"
  address <-
    some $
      satisfy $
        \ x ->
          isAlphanumeric x
          || (`elem` ("%/?:@&=+$,-_.!~*'" :: String)) x
  pure $ NStr $ DoubleQuoted $ one $ Plain $ toText $ one start <> protocol <> ":" <> address

nixString' :: Parser (NString NExprLoc)
nixString' = lexeme $ label "string" $ doubleQuoted <|> indented
 where
  doubleQuoted :: Parser (NString NExprLoc)
  doubleQuoted =
    label "double quoted string" $
      DoubleQuoted . removePlainEmpty . mergePlain <$>
        ( doubleQ *>
            many (stringChar doubleQ (void $ char '\\') doubleEscape)
          <* doubleQ
        )

  doubleQ      = void $ char '"'
  doubleEscape = Plain . one <$> (char '\\' *> escapeCode)

  indented :: Parser (NString NExprLoc)
  indented =
    label "indented string" $
      stripIndent <$>
        ( indentedQ *>
            many (stringChar indentedQ indentedQ indentedEscape)
          <* indentedQ
        )

  indentedQ :: Parser ()
  indentedQ = void . label "\"''\"" $ string "''"

  indentedEscape =
    try $
      do
        indentedQ
        (Plain <$> ("''" <$ char '\'' <|> "$" <$ char '$')) <|>
          do
            _ <- char '\\'
            c <- escapeCode

            pure $
              bool
                EscapedNewline
                (Plain $ one c)
                (c /= '\n')

  stringChar end escStart esc =
    Antiquoted <$>
      (antiStart *> nixToplevelForm <* char '}')
        <|> Plain . one <$>
          char '$' <|> esc <|> Plain . toText <$>
            some plainChar
   where
    plainChar =
      notFollowedBy (end <|> void (char '$') <|> escStart) *> anySingle

  escapeCode =
    msum
      [ c <$ char e | (c, e) <- escapeCodes ]
    <|> anySingle

-- | Gets all of the arguments for a function.
argExpr :: Parser (Params NExprLoc)
argExpr =
  msum
    [ atLeft
    , onlyname
    , atRight
    ]
  <* symbol ":"
 where
  -- An argument not in curly braces. There's some potential ambiguity
  -- in the case of, for example `x:y`. Is it a lambda function `x: y`, or
  -- a URI `x:y`? Nix syntax says it's the latter. So we need to fail if
  -- there's a valid URI parse here.
  onlyname =
    msum
      [ nixUri *> unexpected (Label ('v' :| "alid uri"))
      , Param <$> identifier
      ]

  -- Parameters named by an identifier on the left (`args @ {x, y}`)
  atLeft =
    try $
      do
        name             <- identifier <* symbol "@"
        (pset, variadic) <- params
        pure $ ParamSet (pure name) variadic pset

  -- Parameters named by an identifier on the right, or none (`{x, y} @ args`)
  atRight =
    do
      (pset, variadic) <- params
      name             <- optional $ symbol "@" *> identifier
      pure $ ParamSet name variadic pset

  -- Return the parameters set.
  params = braces getParams

  -- Collects the parameters within curly braces. Returns the parameters and
  -- an flag indication if the parameters are variadic.
  getParams = go mempty
   where
    -- Attempt to parse `...`. If this succeeds, stop and return True.
    -- Otherwise, attempt to parse an argument, optionally with a
    -- default. If this fails, then return what has been accumulated
    -- so far.
    go acc = ((acc, Variadic) <$ symbol "...") <|> getMore
     where
      getMore :: ParsecT  Void Text (State SourcePos) ([(VarName, Maybe NExprLoc)], Variadic)
      getMore =
        -- Could be nothing, in which just return what we have so far.
        option (acc, mempty) $
          do
            -- Get an argument name and an optional default.
            pair <-
              liftA2 (,)
                identifier
                (optional $ question *> nixToplevelForm)

            let args = acc <> [pair]

            -- Either return this, or attempt to get a comma and restart.
            option (args, mempty) $ comma *> go args

nixBinders :: Parser [Binding NExprLoc]
nixBinders = (inherit <|> namedVar) `endBy` semi where
  inherit =
    do
      -- We can't use 'reserved' here because it would consume the whitespace
      -- after the keyword, which is not exactly the semantics of C++ Nix.
      try $ string "inherit" *> lookAhead (void (satisfy reservedEnd))
      p <- getSourcePos
      x <- whiteSpace *> optional scope
      label "inherited binding" $
        liftA2 (Inherit x)
          (many identifier)
          (pure p)
  namedVar =
    do
      p <- getSourcePos
      label "variable binding" $
        liftA3 NamedVar
          (annotated <$> nixSelector)
          (equals *> nixToplevelForm)
          (pure p)
  scope = label "inherit scope" nixParens

keyName :: Parser (NKeyName NExprLoc)
keyName = dynamicKey <|> staticKey
 where
  staticKey  = StaticKey <$> identifier
  dynamicKey = DynamicKey <$> nixAntiquoted nixString'

nixSet :: Parser NExprLoc
nixSet =
  annotateNamedLocation "set" $
    isRec <*> braces nixBinders
 where
  isRec =
    label "recursive set" (reserved "rec" $> NSet Recursive)
    <|> pure (NSet NonRecursive)

parseNixFile :: MonadFile m => Path -> m (Result NExpr)
parseNixFile =
  parseFromFileEx $ stripAnnotation <$> (whiteSpace *> nixToplevelForm <* eof)

parseNixFileLoc :: MonadFile m => Path -> m (Result NExprLoc)
parseNixFileLoc = parseFromFileEx (whiteSpace *> nixToplevelForm <* eof)

parseNixText :: Text -> Result NExpr
parseNixText =
  parseFromText $ stripAnnotation <$> (whiteSpace *> nixToplevelForm <* eof)

parseNixTextLoc :: Text -> Result NExprLoc
parseNixTextLoc = parseFromText (whiteSpace *> nixToplevelForm <* eof)

{- Parser.Library -}

skipLineComment' :: Tokens Text -> Parser ()
skipLineComment' prefix =
  string prefix *> void (takeWhileP (pure "character") (\x -> x /= '\n' && x /= '\r'))

whiteSpace :: Parser ()
whiteSpace = do
  put =<< getSourcePos
  Lexer.space space1 lineCmnt blockCmnt
 where
  lineCmnt  = skipLineComment' "#"
  blockCmnt = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

symbol :: Text -> Parser Text
symbol = lexeme . string

reservedEnd :: Char -> Bool
reservedEnd x =
  isSpace x || (`elem` ("{([})];:.\"'," :: String)) x
{-# inline reservedEnd #-}

reserved :: Text -> Parser ()
reserved n =
  lexeme $ try $ string n *> lookAhead (void (satisfy reservedEnd) <|> eof)

identifier :: Parser VarName
identifier = lexeme $ try $ do
  (coerce -> ident) <-
    liftA2 cons
      (satisfy (\x -> isAlpha x || x == '_'))
      (takeWhileP mempty identLetter)
  guard $ not $ ident `HashSet.member` reservedNames
  pure ident
 where
  identLetter x = isAlphanumeric x || x == '_' || x == '\'' || x == '-'

-- We restrict the type of 'parens' and 'brackets' here because if they were to
-- take a @Parser NExprLoc@ argument they would parse additional text which
-- wouldn't be captured in the source location annotation.
--
-- Braces and angles in hnix don't enclose a single expression so this type
-- restriction would not be useful.
parens :: Parser (NExprF f) -> Parser (NExprF f)
parens   = between (symbol "(") (symbol ")")
braces :: ParsecT Void Text (State SourcePos) a -> ParsecT Void Text (State SourcePos) a
braces   = between (symbol "{") (symbol "}")
-- angles    = between (symbol "<") (symbol ">")
brackets :: Parser (NExprF f) -> Parser (NExprF f)
brackets = between (symbol "[") (symbol "]")
semi :: Parser Text
semi     = symbol ";"
comma :: Parser Text
comma    = symbol ","
-- colon     = symbol ":"
-- dot       = symbol "."
equals :: Parser Text
equals   = symbol "="
question :: Parser Text
question = symbol "?"

integer :: Parser Integer
integer = lexeme Lexer.decimal

float :: Parser Double
float = lexeme Lexer.float

reservedNames :: HashSet VarName
reservedNames =
  HashSet.fromList
    ["let", "in", "if", "then", "else", "assert", "with", "rec", "inherit"]

type Parser = ParsecT Void Text (State SourcePos)

type Result a = Either (Doc Void) a

parseFromFileEx :: MonadFile m => Parser a -> Path -> m (Result a)
parseFromFileEx parser file =
  do
    input <- decodeUtf8 <$> readFile file

    pure $
      either
        (Left . pretty . errorBundlePretty)
        pure
        $ (`evalState` initialPos (coerce file)) $ runParserT parser (coerce file) input

parseFromText :: Parser a -> Text -> Result a
parseFromText parser input =
  let stub = "<string>" in
  either
    (Left . pretty . errorBundlePretty)
    pure
    $ (`evalState` initialPos stub) $ (`runParserT` stub) parser input

{- Parser.Operators -}

data NSpecialOp = NHasAttrOp | NSelectOp
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

data NAssoc = NAssocNone | NAssocLeft | NAssocRight
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

data NOperatorDef
  = NUnaryDef   Text NUnaryOp
  | NBinaryDef  Text NBinaryOp  NAssoc
  | NSpecialDef Text NSpecialOp NAssoc
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

annotateLocation1 :: Parser a -> Parser (AnnUnit SrcSpan a)
annotateLocation1 p =
  do
    begin <- getSourcePos
    res <- p
    end   <- get -- The state set before the last whitespace

    pure $ AnnUnit (SrcSpan begin end) res

annotateLocation :: Parser (NExprF NExprLoc) -> Parser NExprLoc
annotateLocation = (annUnitToAnn <$>) . annotateLocation1

annotateNamedLocation :: String -> Parser (NExprF NExprLoc) -> Parser NExprLoc
annotateNamedLocation name = annotateLocation . label name

manyUnaryOp :: MonadPlus f => f (a -> a) -> f (a -> a)
manyUnaryOp f = foldr1 (.) <$> some f

operator :: Text -> Parser Text
operator op =
  case op of
    c@"-" -> c `without` '>'
    c@"/" -> c `without` '/'
    c@"<" -> c `without` '='
    c@">" -> c `without` '='
    n   -> symbol n
 where
  without :: Text -> Char -> Parser Text
  without opChar noNextChar =
    lexeme . try $ string opChar <* notFollowedBy (char noNextChar)

opWithLoc :: Text -> o -> (AnnUnit SrcSpan o -> a) -> Parser a
opWithLoc name op f =
  do
    AnnUnit ann _ <-
      annotateLocation1 $
        {- dbg (toString name) $ -}
        operator name

    pure $ f $ AnnUnit ann op

binaryN :: Text -> NBinaryOp -> (NOperatorDef, Operator (ParsecT Void Text (State SourcePos)) NExprLoc)
binaryN name op =
  (NBinaryDef name op NAssocNone, InfixN $ opWithLoc name op annNBinary)
binaryL :: Text -> NBinaryOp -> (NOperatorDef, Operator (ParsecT Void Text (State SourcePos)) NExprLoc)
binaryL name op =
  (NBinaryDef name op NAssocLeft, InfixL $ opWithLoc name op annNBinary)
binaryR :: Text -> NBinaryOp -> (NOperatorDef, Operator (ParsecT Void Text (State SourcePos)) NExprLoc)
binaryR name op =
  (NBinaryDef name op NAssocRight, InfixR $ opWithLoc name op annNBinary)
prefix :: Text -> NUnaryOp -> (NOperatorDef, Operator (ParsecT Void Text (State SourcePos)) NExprLoc)
prefix name op =
  (NUnaryDef name op, Prefix $ manyUnaryOp $ opWithLoc name op annNUnary)
-- postfix name op = (NUnaryDef name op,
--                    Postfix (opWithLoc name op annNUnary))

nixOperators
  :: Parser (AnnUnit SrcSpan (NAttrPath NExprLoc))
  -> [[(NOperatorDef, Operator Parser NExprLoc)]]
nixOperators selector =
  [ -- This is not parsed here, even though technically it's part of the
    -- expression table. The problem is that in some cases, such as list
    -- membership, it's also a term. And since terms are effectively the
    -- highest precedence entities parsed by the expression parser, it ends up
    -- working out that we parse them as a kind of "meta-term".

    -- {-  1 -} [ (NSpecialDef "." NSelectOp NAssocLeft,
    --             Postfix $ do
    --                    sel <- seldot *> selector
    --                    mor <- optional (reserved "or" *> term)
    --                    pure $ \x -> annNSelect x sel mor) ]

    {-  2 -}
    [ ( NBinaryDef " " NApp NAssocLeft
      ,
        -- Thanks to Brent Yorgey for showing me this trick!
        InfixL $ annNApp <$ symbol ""
      )
    ]
  , {-  3 -}
    [ prefix  "-"  NNeg ]
  , {-  4 -}
    [ ( NSpecialDef "?" NHasAttrOp NAssocLeft
      , Postfix $ symbol "?" *> (flip annNHasAttr <$> selector)
      )
    ]
  , {-  5 -}
    [ binaryR "++" NConcat ]
  , {-  6 -}
    [ binaryL "*"  NMult
    , binaryL "/"  NDiv
    ]
  , {-  7 -}
    [ binaryL "+"  NPlus
    , binaryL "-"  NMinus
    ]
  , {-  8 -}
    [ prefix  "!"  NNot ]
  , {-  9 -}
    [ binaryR "//" NUpdate ]
  , {- 10 -}
    [ binaryL "<"  NLt
    , binaryL ">"  NGt
    , binaryL "<=" NLte
    , binaryL ">=" NGte
    ]
  , {- 11 -}
    [ binaryN "==" NEq
    , binaryN "!=" NNEq
    ]
  , {- 12 -}
    [ binaryL "&&" NAnd ]
  , {- 13 -}
    [ binaryL "||" NOr ]
  , {- 14 -}
    [ binaryR "->" NImpl ]
  ]

data OperatorInfo = OperatorInfo
  { precedence    :: Int
  , associativity :: NAssoc
  , operatorName  :: Text
  } deriving (Eq, Ord, Generic, Typeable, Data, Show)

getUnaryOperator :: NUnaryOp -> OperatorInfo
getUnaryOperator = (m Map.!)
 where
  m =
    Map.fromList $
      concat $
        zipWith
          buildEntry
          [1 ..]
          (nixOperators $ fail "unused")

  buildEntry i =
    concatMap $
      \case
        (NUnaryDef name op, _) -> [(op, OperatorInfo i NAssocNone name)]
        _                      -> mempty

getBinaryOperator :: NBinaryOp -> OperatorInfo
getBinaryOperator = (m Map.!)
 where
  m =
    Map.fromList $
      concat $
        zipWith
          buildEntry
          [1 ..]
          (nixOperators $ fail "unused")

  buildEntry i =
    concatMap $
      \case
        (NBinaryDef name op assoc, _) -> [(op, OperatorInfo i assoc name)]
        _                             -> mempty

getSpecialOperator :: NSpecialOp -> OperatorInfo
getSpecialOperator NSelectOp = OperatorInfo 1 NAssocLeft "."
getSpecialOperator o         = m Map.! o
 where
  m =
    Map.fromList $
      concat $
        zipWith
          buildEntry
          [1 ..]
          (nixOperators $ fail "unused")

  buildEntry i =
    concatMap $
      \case
        (NSpecialDef name op assoc, _) -> [(op, OperatorInfo i assoc name)]
        _                              -> mempty
