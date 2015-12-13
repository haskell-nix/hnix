{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Types where

import           Control.Applicative
import           Control.Monad hiding (forM_, mapM, sequence)
import           Data.Data
import           Data.Fix
import           Data.Foldable
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Data.Traversable
import           Data.Tuple (swap)
import           GHC.Exts
import           GHC.Generics
import           Prelude hiding (readFile, concat, concatMap, elem, mapM,
                                 sequence, minimum, foldr)

-- | Atoms are values that evaluate to themselves. This means that they appear in both
-- the parsed AST (in the form of literals) and the evaluated form.
data NAtom
  -- | An integer. The c nix implementation currently only supports integers that
  -- fit in the range of 'Int64'.
  = NInt Integer

  -- | The first argument of 'NPath' is 'True' if the path must be looked up in the Nix
  -- search path.
  -- For example, @<nixpkgs/pkgs>@ is represented by @NPath True "nixpkgs/pkgs"@,
  -- while @foo/bar@ is represented by @NPath False "foo/bar@.
  | NPath Bool FilePath

  | NBool Bool
  | NNull
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

atomText :: NAtom -> Text
atomText (NInt i)  = pack (show i)
atomText (NBool b) = if b then "true" else "false"
atomText NNull     = "null"
atomText (NPath isFromEnv p)
  | isFromEnv = pack ("<" ++ p ++ ">")
  -- If it's not an absolute path, we need to prefix with ./
  | otherwise = case pack p of
    "./" -> "./."
    "../" -> "../."
    ".." -> "../."
    txt | "/" `T.isPrefixOf` txt -> txt
        | "./" `T.isPrefixOf` txt -> txt
        | "../" `T.isPrefixOf` txt -> txt
        | otherwise -> "./" <> txt

-- | 'Antiquoted' represents an expression that is either
-- antiquoted (surrounded by ${...}) or plain (not antiquoted).
data Antiquoted v r = Plain v | Antiquoted r
  deriving (Ord, Eq, Generic, Typeable, Data, Functor, Show)

-- | Merge adjacent 'Plain' values with 'mappend'.
mergePlain :: Monoid v => [Antiquoted v r] -> [Antiquoted v r]
mergePlain [] = []
mergePlain (Plain a: Plain b: xs) = mergePlain (Plain (a <> b) : xs)
mergePlain (x:xs) = x : mergePlain xs

-- | Remove 'Plain' values equal to 'mempty'.
removePlainEmpty :: (Eq v, Monoid v) => [Antiquoted v r] -> [Antiquoted v r]
removePlainEmpty = filter f where
  f (Plain x) = x /= mempty
  f _ = True

runAntiquoted :: (v -> a) -> (r -> a) -> Antiquoted v r -> a
runAntiquoted f _ (Plain v) = f v
runAntiquoted _ f (Antiquoted r) = f r

data StringKind = DoubleQuoted | Indented
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

-- | A 'NixString' is a list of things that are either a plain string
-- or an antiquoted expression. After the antiquotes have been evaluated,
-- the final string is constructed by concating all the parts.
data NString r = NString StringKind [Antiquoted Text r] | NUri Text
  deriving (Eq, Ord, Generic, Typeable, Data, Functor, Show)

-- | Split a stream representing a string with antiquotes on line breaks.
splitLines :: [Antiquoted Text r] -> [[Antiquoted Text r]]
splitLines = uncurry (flip (:)) . go where
  go (Plain t : xs) = (Plain l :) <$> foldr f (go xs) ls where
    (l : ls) = T.split (=='\n') t
    f prefix (finished, current) = ((Plain prefix : current) : finished, [])
  go (Antiquoted a : xs) = (Antiquoted a :) <$> go xs
  go [] = ([],[])

-- | Join a stream of strings containing antiquotes again. This is the inverse
-- of 'splitLines'.
unsplitLines :: [[Antiquoted Text r]] -> [Antiquoted Text r]
unsplitLines = intercalate [Plain "\n"]

-- | Form an indented string by stripping spaces equal to the minimal indent.
stripIndent :: [Antiquoted Text r] -> NString r
stripIndent [] = NString Indented []
stripIndent xs = NString Indented . removePlainEmpty . mergePlain . unsplitLines $ ls'
 where
  ls = stripEmptyOpening $ splitLines xs
  ls' = map (dropSpaces minIndent) ls

  minIndent = case stripEmptyLines ls of
    [] -> 0
    nonEmptyLs -> minimum $ map (countSpaces . mergePlain) nonEmptyLs

  stripEmptyLines = filter f where
    f [Plain t] = not $ T.null $ T.strip t
    f _ = True

  stripEmptyOpening ([Plain t]:ts) | T.null (T.strip t) = ts
  stripEmptyOpening ts = ts

  countSpaces (Antiquoted _:_) = 0
  countSpaces (Plain t : _) = T.length . T.takeWhile (== ' ') $ t
  countSpaces [] = 0

  dropSpaces 0 x = x
  dropSpaces n (Plain t : cs) = Plain (T.drop n t) : cs
  dropSpaces _ _ = error "stripIndent: impossible"

escapeCodes :: [(Char, Char)]
escapeCodes =
  [ ('\n', 'n' )
  , ('\r', 'r' )
  , ('\t', 't' )
  , ('\\', '\\')
  , ('$' , '$' )
  , ('"', '"')
  ]

fromEscapeCode :: Char -> Maybe Char
fromEscapeCode = (`lookup` map swap escapeCodes)

toEscapeCode :: Char -> Maybe Char
toEscapeCode = (`lookup` escapeCodes)

instance IsString (NString r) where
  fromString "" = NString DoubleQuoted []
  fromString x = NString DoubleQuoted . (:[]) . Plain . pack $ x

-- | A 'KeyName' is something that can appear on the left side of an equals sign.
-- For example, @a@ is a 'KeyName' in @{ a = 3; }@, @let a = 3; in ...@, @{}.a@ or @{} ? a@.
--
-- Nix supports both static keynames (just an identifier) and dynamic identifiers.
-- Dynamic identifiers can be either a string (e.g.: @{ "a" = 3; }@) or an antiquotation
-- (e.g.: @let a = "example"; in { ${a} = 3; }.example@).
--
-- Note: There are some places where a dynamic keyname is not allowed. In particular, those include:
--
--   * the RHS of a @binding@ inside @let@: @let ${"a"} = 3; in ...@ produces a syntax error.
--   * the attribute names of an 'inherit': @inherit ${"a"};@ is forbidden.
--
-- Note: In Nix, a simple string without antiquotes such as @"foo"@ is allowed even if
-- the context requires a static keyname, but the parser still considers it a
-- 'DynamicKey' for simplicity.
data NKeyName r
  = DynamicKey (Antiquoted (NString r) r)
  | StaticKey Text
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

-- deriving this instance automatically is not possible
-- because r occurs not only as last argument in Antiquoted (NString r) r
instance Functor NKeyName where
  fmap f (DynamicKey (Plain str)) = DynamicKey . Plain $ fmap f str
  fmap f (DynamicKey (Antiquoted e)) = DynamicKey . Antiquoted $ f e
  fmap _ (StaticKey key) = StaticKey key

type NSelector r = [NKeyName r]

data NOperF r
  = NUnary NUnaryOp r
  | NBinary NBinaryOp r r
  deriving (Eq, Ord, Generic, Typeable, Data, Functor, Show)

data NUnaryOp = NNeg | NNot deriving (Eq, Ord, Generic, Typeable, Data, Show)
data NSpecialOp = NHasAttrOp | NSelectOp | NAppOp
  deriving (Eq, Ord, Generic, Typeable, Data, Show)
data NBinaryOp
  = NEq
  | NNEq
  | NLt
  | NLte
  | NGt
  | NGte
  | NAnd
  | NOr
  | NImpl
  | NUpdate

  | NPlus
  | NMinus
  | NMult
  | NDiv
  | NConcat
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

data NAssoc = NAssocNone | NAssocLeft | NAssocRight
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

data NOperatorDef
  = NUnaryDef String NUnaryOp
  | NBinaryDef NAssoc [(String, NBinaryOp)]
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

nixOperators :: [Either NSpecialOp NOperatorDef]
nixOperators =
  [ Left NSelectOp
  , Left NAppOp
  , Right $ NUnaryDef  "-"  NNeg
  , Left NHasAttrOp
  ] ++ map Right
  [ NBinaryDef NAssocRight [("++", NConcat)]
  , NBinaryDef NAssocLeft [("*", NMult), ("/", NDiv)]
  , NBinaryDef NAssocLeft [("+", NPlus), ("-", NMinus)]
  , NUnaryDef  "!"  NNot
  , NBinaryDef NAssocRight [("//", NUpdate)]
  , NBinaryDef NAssocLeft [("<", NLt), (">", NGt), ("<=", NLte), (">=", NGte)]
  , NBinaryDef NAssocNone [("==", NEq), ("!=", NNEq)]
  , NBinaryDef NAssocLeft [("&&", NAnd)]
  , NBinaryDef NAssocLeft [("||", NOr)]
  , NBinaryDef NAssocNone [("->", NImpl)]
  ]

data OperatorInfo = OperatorInfo
  { precedence    :: Int
  , associativity :: NAssoc
  , operatorName  :: String
  } deriving (Eq, Ord, Generic, Typeable, Data, Show)

getUnaryOperator :: NUnaryOp -> OperatorInfo
getUnaryOperator = (m Map.!) where
  m = Map.fromList . concat . zipWith buildEntry [1..] . reverse $ nixOperators
  buildEntry i (Right (NUnaryDef name op)) = [(op, OperatorInfo i NAssocNone name)]
  buildEntry _ _                           = []

getBinaryOperator :: NBinaryOp -> OperatorInfo
getBinaryOperator = (m Map.!) where
  m = Map.fromList . concat . zipWith buildEntry [1..] . reverse $ nixOperators
  buildEntry i (Right (NBinaryDef assoc ops)) =
    [ (op, OperatorInfo i assoc name) | (name,op) <- ops ]
  buildEntry _ _                              = []

getSpecialOperatorPrec :: NSpecialOp -> Int
getSpecialOperatorPrec = (m Map.!) where
  m = Map.fromList . catMaybes . zipWith buildEntry [1..] . reverse $ nixOperators
  buildEntry _ (Right _) = Nothing
  buildEntry i (Left op) = Just (op, i)

selectOp :: OperatorInfo
selectOp = OperatorInfo (getSpecialOperatorPrec NSelectOp) NAssocLeft "."

hasAttrOp :: OperatorInfo
hasAttrOp = OperatorInfo (getSpecialOperatorPrec NHasAttrOp) NAssocLeft "?"

appOp :: OperatorInfo
appOp = OperatorInfo (getSpecialOperatorPrec NAppOp) NAssocLeft " "

data NSetBind = Rec | NonRec
  deriving (Ord, Eq, Generic, Typeable, Data, Show)

-- | A single line of the bindings section of a let expression or of
-- a set.
data Binding r
  = NamedVar (NSelector r) r
  | Inherit (Maybe r) [NSelector r]
  deriving (Typeable, Data, Ord, Eq, Functor, Show)

-- | For functions which are called with a set as an argument.
data FormalParamSet r
  = FixedParamSet (Map Text (Maybe r))    -- ^ E.g. `{foo, bar}`
  | VariadicParamSet (Map Text (Maybe r)) -- ^ E.g. `{foo, bar, ...}`
  deriving (Eq, Ord, Generic, Typeable, Data, Functor, Show, Foldable, Traversable)

-- | @Formals@ represents all the ways the formal parameters to a
-- function can be represented.
data Formals r
  = FormalName Text
  | FormalSet (FormalParamSet r) (Maybe Text)
  deriving (Ord, Eq, Generic, Typeable, Data, Functor, Show, Foldable, Traversable)

-- | A functor-ized nix expression type, which lets us do things like traverse
-- expressions and map functions over them. The actual NExpr type is defined
-- below.
data NExprF r
    -- value types
    = NConstant NAtom
    | NStr (NString r)
    | NList [r]
    | NSet NSetBind [Binding r]
    | NAbs (Formals r) r

    -- operators
    | NOper (NOperF r)
    | NSelect r (NSelector r) (Maybe r)
    | NHasAttr r (NSelector r)
    | NApp r r

    -- language constructs
    -- | A 'NSym' is a reference to a variable. For example, @f@ is represented as
    -- @NSym "f"@ and @a@ as @NSym "a" in @f a@.
    | NSym Text
    | NLet [Binding r] r
    | NIf r r r
    | NWith r r
    | NAssert r r
    deriving (Ord, Eq, Generic, Typeable, Data, Functor, Show)

type NExpr = Fix NExprF

mkInt :: Integer -> NExpr
mkInt = Fix . NConstant . NInt

mkStr :: StringKind -> Text -> NExpr
mkStr kind x = Fix . NStr . NString kind $ if x == ""
  then []
  else [Plain x]

mkUri :: Text -> NExpr
mkUri = Fix . NStr . NUri

mkPath :: Bool -> FilePath -> NExpr
mkPath b = Fix . NConstant . NPath b

mkSym :: Text -> NExpr
mkSym = Fix . NSym

mkSelector :: Text -> NSelector NExpr
mkSelector = (:[]) . StaticKey

mkBool :: Bool -> NExpr
mkBool = Fix . NConstant . NBool

mkNull :: NExpr
mkNull = Fix (NConstant NNull)

mkOper :: NUnaryOp -> NExpr -> NExpr
mkOper op = Fix . NOper . NUnary op

mkOper2 :: NBinaryOp -> NExpr -> NExpr -> NExpr
mkOper2 op a = Fix . NOper . NBinary op a

mkFormalSet :: [(Text, Maybe NExpr)] -> Formals NExpr
mkFormalSet = mkFixedParamSet

mkFixedParamSet :: [(Text, Maybe NExpr)] -> Formals NExpr
mkFixedParamSet ps = FormalSet (FixedParamSet $ Map.fromList ps) Nothing

mkVariadicParamSet :: [(Text, Maybe NExpr)] -> Formals NExpr
mkVariadicParamSet ps = FormalSet (VariadicParamSet $ Map.fromList ps) Nothing

mkApp :: NExpr -> NExpr -> NExpr
mkApp e = Fix . NApp e

mkRecSet :: [Binding NExpr] -> NExpr
mkRecSet = Fix . NSet Rec

mkNonRecSet :: [Binding NExpr] -> NExpr
mkNonRecSet = Fix . NSet NonRec

mkLet :: [Binding NExpr] -> NExpr -> NExpr
mkLet bs = Fix . NLet bs

mkList :: [NExpr] -> NExpr
mkList = Fix . NList

mkWith :: NExpr -> NExpr -> NExpr
mkWith e = Fix . NWith e

mkAssert :: NExpr -> NExpr -> NExpr
mkAssert e = Fix . NWith e

mkIf :: NExpr -> NExpr -> NExpr -> NExpr
mkIf e1 e2 = Fix . NIf e1 e2

mkFunction :: Formals NExpr -> NExpr -> NExpr
mkFunction params = Fix . NAbs params

-- | Shorthand for producing a binding of a name to an expression.
bindTo :: Text -> NExpr -> Binding NExpr
bindTo name val = NamedVar (mkSelector name) val

-- | Append a list of bindings to a set or let expression.
-- For example, adding `[a = 1, b = 2]` to `let c = 3; in 4` produces
-- `let a = 1; b = 2; c = 3; in 4`.
appendBindings :: [Binding NExpr] -> NExpr -> NExpr
appendBindings newBindings (Fix e) = case e of
  NLet bindings e' -> Fix $ NLet (bindings <> newBindings) e'
  NSet bindType bindings -> Fix $ NSet bindType (bindings <> newBindings)
  _ -> error "Can only append bindings to a set or a let"

-- | Applies a transformation to the body of a nix function.
modifyFunctionBody :: (NExpr -> NExpr) -> NExpr -> NExpr
modifyFunctionBody f (Fix e) = case e of
  NAbs params body -> Fix $ NAbs params (f body)
  _ -> error "Not a function"

-- | An 'NValue' is the most reduced form of an 'NExpr' after evaluation
-- is completed.
data NValueF r
    = NVConstant NAtom
    | NVStr Text
    | NVList [r]
    | NVSet (Map Text r)
    | NVFunction (Formals r) (NValue -> IO r)
    deriving (Generic, Typeable, Functor)

instance Show f => Show (NValueF f) where
    showsPrec = flip go where
      go (NVConstant atom) = showsCon1 "NVConstant" atom
      go (NVStr      text) = showsCon1 "NVStr"      text
      go (NVList     list) = showsCon1 "NVList"     list
      go (NVSet     attrs) = showsCon1 "NVSet"      attrs
      go (NVFunction r _)  = showsCon1 "NVFunction" r

      showsCon1 :: Show a => String -> a -> Int -> String -> String
      showsCon1 con a d = showParen (d > 10) $ showString (con ++ " ") . showsPrec 11 a

type NValue = Fix NValueF

valueText :: NValue -> Text
valueText = cata phi where
    phi (NVConstant a)   = atomText a
    phi (NVStr t)        = t
    phi (NVList _)       = error "Cannot coerce a list to a string"
    phi (NVSet _)        = error "Cannot coerce a set to a string"
    phi (NVFunction _ _) = error "Cannot coerce a function to a string"
