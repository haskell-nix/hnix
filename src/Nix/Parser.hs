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
import           Data.Fix                       ( Fix(..) )
import qualified Data.HashSet                  as HashSet
import qualified Data.Text                     as Text
import qualified Data.Map.Strict               as M
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
{-# inline isAlphanumeric #-}

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
  HashSet.fromList
    ["let", "in", "if", "then", "else", "assert", "with", "rec", "inherit"]

reservedEnd :: Char -> Bool
reservedEnd x =
  isSpace x || (`elem` ("{([})];:.\"'," :: String)) x
{-# inline reservedEnd #-}

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
    bool
      id
      lexeme
      hungry
      (char '}')

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

nixInt :: Parser NExprLoc
nixInt =
  annotateNamedLocation "integer" $
    mkIntF <$> integer

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
                bool
                  EscapedNewline
                  (Plain $ one c)
                  ('\n' /= c)

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
        (coerce -> iD) <-
          liftA2 Text.cons
            (satisfy (\x -> isAlpha x || x == '_'))
            (takeWhileP mempty identLetter)
        guard $ not $ iD `HashSet.member` reservedNames
        pure iD
 where
  identLetter x = isAlphanumeric x || x == '_' || x == '\'' || x == '-'

nixSym :: Parser NExprLoc
nixSym = annotateLocation $ mkSymF <$> coerce identifier


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

nixBinders :: Parser [Binding NExprLoc]
nixBinders = (inherit <|> namedVar) `endBy` symbol ';' where
  inherit =
    do
      -- We can't use 'reserved' here because it would consume the whitespace
      -- after the keyword, which is not exactly the semantics of C++ Nix.
      try $ chunk "inherit" *> lookAhead (void $ satisfy reservedEnd)
      p <- getSourcePos
      x <- whiteSpace *> optional scope
      label "inherited binding" $
        liftA2 (Inherit x)
          (many identifier)
          (pure (toNSourcePos p))
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

pathStr :: Parser Path
pathStr =
  lexeme $ coerce . toString <$>
    liftA2 (<>)
      (takeWhileP mempty pathChar)
      (Text.concat <$>
        some
          (liftA2 Text.cons
            slash
            (takeWhile1P mempty pathChar)
          )
      )

nixPath :: Parser NExprLoc
nixPath =
  annotateNamedLocation "path" $
    try $ mkPathF False <$> coerce pathStr


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
data NSpecialOp
  = NHasAttrOp
  | NSelectOp
  | NTerm -- ^ For special handling of internal print cases.
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

data NAssoc
  = NAssocLeft
  -- Nota bene: @parser-combinators@ named "associative property" as 'InfixN' stating it as "non-associative property".
  -- Binary operators having some associativity is a basis property in mathematical algebras in use (for example, in Category theory). Having no associativity in operators makes theory mostly impossible in use and so non-associativity is not encountered in notations, therefore under 'InfixN' @parser-combinators@ meant "associative".
  -- | Bidirectional associativity, or simply: associative property.
  | NAssoc
  | NAssocRight
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

--  2022-01-26: NOTE: Maybe split up this type into according set? Would make NOp class total.
-- | Single operator grammar entries.
data NOperatorDef
  = NAppDef                       NOpPrecedence NOpName
  | NUnaryDef   NUnaryOp          NOpPrecedence NOpName
  | NBinaryDef  NBinaryOp  NAssoc NOpPrecedence NOpName
  | NSpecialDef NSpecialOp NAssoc NOpPrecedence NOpName
  --  2022-01-26: NOTE: Ord can be the order of evaluation of precedence (which 'Pretty' printing also accounts for).
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

appOpDef :: NOperatorDef
appOpDef = NAppDef 1 " " -- This defined as "2" in Nix lang spec.

--  2022-01-26: NOTE: After `OperatorInfo` type is removed from code base
-- , think to remove these maps in favour of direct pattern matching in instances.
-- That would make those instances total.
unaryOpDefMap :: Map NUnaryOp NOperatorDef
unaryOpDefMap = fromList
  [ (NNeg, NUnaryDef NNeg 3 "-")
  , (NNot, NUnaryDef NNot 8 "!")
  ]

binaryOpDefMap :: Map NBinaryOp NOperatorDef
binaryOpDefMap = fromList
  [ (NConcat, NBinaryDef NConcat NAssocRight  5 "++")
  , (NMult  , NBinaryDef NMult   NAssocLeft   6 "*" )
  , (NDiv   , NBinaryDef NDiv    NAssocLeft   6 "/" )
  , (NPlus  , NBinaryDef NPlus   NAssocLeft   7 "+" )
  , (NMinus , NBinaryDef NMinus  NAssocLeft   7 "-" )
  , (NUpdate, NBinaryDef NUpdate NAssocRight  9 "//")
  , (NLt    , NBinaryDef NLt     NAssocLeft  10 "<" )
  , (NLte   , NBinaryDef NLte    NAssocLeft  10 "<=")
  , (NGt    , NBinaryDef NGt     NAssocLeft  10 ">" )
  , (NGte   , NBinaryDef NGte    NAssocLeft  10 ">=")
  , (NEq    , NBinaryDef NEq     NAssoc      11 "==")
  , (NNEq   , NBinaryDef NNEq    NAssoc      11 "!=")
  , (NAnd   , NBinaryDef NAnd    NAssocLeft  12 "&&")
  , (NOr    , NBinaryDef NOr     NAssocLeft  13 "||")
  , (NImpl  , NBinaryDef NImpl   NAssocRight 14 "->")
  ]

specOpDefMap :: Map NSpecialOp NOperatorDef
specOpDefMap = fromList
  [ (NSelectOp , NSpecialDef NSelectOp  NAssocLeft 1 ".")
  , (NHasAttrOp, NSpecialDef NHasAttrOp NAssocLeft 4 "?")
  ]

--  2022-01-26: NOTE: When total - make sure to hide & inline all these instances to get free solution.
-- | Class to get a private free construction to abstract away the gap between the Nix operation types
-- 'NUnaryOp', 'NBinaryOp', 'NSpecialOp'.
-- And in doing remove 'OperatorInfo' from existance.
class NOp a where
  getOpDef :: a -> NOperatorDef
  getOpAssoc :: a -> NAssoc
  getOpPrecedence :: a -> NOpPrecedence
  getOpName :: a -> NOpName

instance NOp NUnaryOp where
  getOpDef op =
    M.findWithDefault
      (error "Impossible happened: unary operation should be includded into the definition map.")
      op
      unaryOpDefMap
  --  2022-01-26: NOTE: This instance is a lie, - remove it after `OperatorInfo` is removed from the module.
  getOpAssoc = fun . getOpDef
   where
    fun (NUnaryDef _op _prec _name) = NAssoc
    fun _ = error "Impossible happened, unary operation should been matched."
  getOpPrecedence = fun . getOpDef
   where
    fun (NUnaryDef _op prec _name) = prec
    fun _ = error "Impossible happened, unary operation should been matched."
  getOpName = fun . getOpDef
   where
    fun (NUnaryDef _op _prec name) = name
    fun _ = error "Impossible happened, unary operation should been matched."

instance NOp NBinaryOp where
  getOpDef op =
    M.findWithDefault
      (error "Impossible, binary operation should be includded into the definition map.")
      op
      binaryOpDefMap
  getOpAssoc = fun . getOpDef
   where
    fun (NBinaryDef _op assoc _prec _name) = assoc
    fun _ = error "Impossible happened, binary operation should been matched."
  getOpPrecedence = fun . getOpDef
   where
    fun (NBinaryDef _op _assoc prec _name) = prec
    fun _ = error "Impossible happened, binary operation should been matched."
  getOpName = fun . getOpDef
   where
    fun (NBinaryDef _op _assoc _prec name) = name
    fun _ = error "Impossible happened, binary operation should been matched."

instance NOp NSpecialOp where
  getOpDef op =
    M.findWithDefault
      (error "Impossible, special operation should be includded into the definition map.")
      op
      specOpDefMap
  getOpAssoc = fun . getOpDef
   where
    fun (NSpecialDef _op assoc _prec _name) = assoc
    fun _ = error "Impossible happened, special operation should been matched."
  getOpPrecedence = fun . getOpDef
   where
    fun (NSpecialDef _op _assoc prec _name) = prec
    fun _ = error "Impossible happened, special operation should been matched."
  getOpName = fun . getOpDef
   where
    fun (NSpecialDef _op _assoc _prec name) = name
    fun _ = error "Impossible happened, special operation should been matched."

instance NOp NOperatorDef where
  getOpDef op = op
  getOpAssoc op = fun op
   where
    fun (NAppDef _prec _name) = NAssocLeft
    fun (NBinaryDef _op assoc _prec _name) = assoc
    fun (NSpecialDef _op assoc _prec _name) = assoc
    fun _ = error "Impossible happened, operator seems to have no associativity getter defined."
  getOpPrecedence = fun . getOpDef
   where
    fun (NAppDef prec _name) = prec
    fun (NBinaryDef _op _assoc prec _name) = prec
    fun (NSpecialDef _op _assoc prec _name) = prec
    fun _ = error "Impossible happened, operator seems to have no precedence getter defined."
  getOpName = fun . getOpDef
   where
    fun (NAppDef _prec name) = name
    fun (NBinaryDef _op _assoc _prec name) = name
    fun (NSpecialDef _op _assoc _prec name) = name
    fun _ = error "Impossible happened, operator seems to have no name getter defined."

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
        pure $ ParamSet (pure name) variadic pset

  -- Parameters named by an identifier on the right, or none (`{x, y} @ args`)
  atRight =
    do
      (variadic, pset) <- params
      name             <- optional $ symbol '@' *> identifier
      pure $ ParamSet name variadic pset

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
      (reserved "if"   *> nixExprAlgebra)
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
      liftA2 build
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
  build
    :: NExprLoc
    -> Maybe
      ( Maybe NExprLoc
      , AnnUnit SrcSpan (NAttrPath NExprLoc)
      )
    -> NExprLoc
  build t =
    maybe
      t
      (uncurry (`annNSelect` t))


-- ** _ - syntax hole

nixSynHole :: Parser NExprLoc
nixSynHole = annotateLocation $ mkSynHoleF <$> coerce (char '^' *> identifier)


-- ** Expr & its constituents (Language term, expr algebra)

-- | Bundles operators with parsers for them, since @megaparsec@ requires the @[[op]]@ form.
nixOperators
  :: [[ Operator Parser NExprLoc ]]
nixOperators =
  [ -- This is not parsed here, even though technically it's part of the
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

    {-  2 -}
  -- 2018-05-07: jwiegley: Thanks to Brent Yorgey for showing me this trick!
    one (InfixL $ annNApp <$ symbols mempty) -- NApp is left associative
  , {-  3 -}
    one $ prefix NNeg
  , {-  4 -}
    one ( Postfix $ symbol '?' *> (flip annNHasAttr <$> nixSelector) )
  , {-  5 -}
    one $ binary NConcat
  , {-  6 -}
    [ binary NMult
    , binary NDiv
    ]
  , {-  7 -}
    [ binary NPlus
    , binary NMinus
    ]
  , {-  8 -}
    one $ prefix NNot
  , {-  9 -}
    one $ binary NUpdate
  , {- 10 -}
    [ binary NLt
    , binary NGt
    , binary NLte
    , binary NGte
    ]
  , {- 11 -}
    [ binary NEq
    , binary NNEq
    ]
  , {- 12 -}
    one $ binary NAnd
  , {- 13 -}
    one $ binary NOr
  , {- 14 -}
    one $ binary NImpl
  ]

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

-- | Nix expression algebra parser.
-- "Expression algebra" is to explain @megaparsec@ use of the term "Expression" (parser for language algebraic coperators without any statements (without @let@ etc.)), which is essentially an algebra inside the language.
nixExprAlgebra :: Parser NExprLoc
nixExprAlgebra =
  makeExprParser -- This requires to convert precedence to [[op]]
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
