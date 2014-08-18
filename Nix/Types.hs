{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Types where

import           Control.Applicative
import           Control.Monad hiding (forM_, mapM, sequence)
import           Data.Data
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

newtype Fix (f :: * -> *) = Fix { outF :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . outF

cataM :: (Traversable f, Monad m) => (f a -> m a) -> Fix f -> m a
cataM f = f <=< mapM (cataM f) . outF

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
atomText (NPath s p)
  | s = pack ("<" ++ p ++ ">")
  | otherwise = pack p

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

  minIndent = minimum . map (countSpaces . mergePlain) . stripEmptyLines $ ls

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
  ]

fromEscapeCode :: Char -> Maybe Char
fromEscapeCode = (`lookup` map swap escapeCodes)

toEscapeCode :: Char -> Maybe Char
toEscapeCode = (`lookup` escapeCodes)

instance IsString (NString r) where
  fromString "" = NString DoubleQuoted []
  fromString x = NString DoubleQuoted . (:[]) . Plain . pack $ x

-- | A 'KeyName' is something that can appear at the right side of an equals sign.
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
  | NBinaryDef String NBinaryOp NAssoc
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

nixOperators :: [Either NSpecialOp [NOperatorDef]]
nixOperators =
  [ Left NSelectOp
  , Left NAppOp
  , Right [ NUnaryDef  "-"  NNeg           ]
  , Left NHasAttrOp
  ] ++ map Right
  [ [ NBinaryDef "++" NConcat  NAssocRight ]
  , [ NBinaryDef "*"  NMult    NAssocLeft  , NBinaryDef "/" NDiv   NAssocLeft ]
  , [ NBinaryDef "+"  NPlus    NAssocLeft  , NBinaryDef "-" NMinus NAssocLeft ]
  , [ NUnaryDef  "!"  NNot                 ]
  , [ NBinaryDef "//" NUpdate NAssocRight  ]
  , [ NBinaryDef "<"  NLt     NAssocLeft   , NBinaryDef ">"  NGt  NAssocLeft
    , NBinaryDef "<=" NLte    NAssocLeft   , NBinaryDef ">=" NGte NAssocLeft
    ]
  , [ NBinaryDef "==" NEq     NAssocNone   , NBinaryDef "!=" NNEq NAssocNone  ]
  , [ NBinaryDef "&&" NAnd    NAssocLeft   ]
  , [ NBinaryDef "||" NOr     NAssocLeft   ]
  , [ NBinaryDef "->" NImpl   NAssocNone   ]
  ]

data OperatorInfo = OperatorInfo
  { precedence    :: Int
  , associativity :: NAssoc
  , operatorName  :: String
  } deriving (Eq, Ord, Generic, Typeable, Data, Show)

getUnaryOperator :: NUnaryOp -> OperatorInfo
getUnaryOperator = (m Map.!) where
  m = Map.fromList . concat . zipWith buildEntry [1..] . reverse $ nixOperators
  buildEntry _ (Left _) = []
  buildEntry i (Right ops) =
    [ (op, OperatorInfo i NAssocNone name) | NUnaryDef name op <- ops ]

getBinaryOperator :: NBinaryOp -> OperatorInfo
getBinaryOperator = (m Map.!) where
  m = Map.fromList . concat . zipWith buildEntry [1..] . reverse $ nixOperators
  buildEntry _ (Left _) = []
  buildEntry i (Right ops) =
    [ (op, OperatorInfo i assoc name) | NBinaryDef name op assoc <- ops ]

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

data FormalParamSet r = FormalParamSet (Map Text (Maybe r))
  deriving (Eq, Ord, Generic, Typeable, Data, Functor, Show, Foldable, Traversable)

-- | @Formals@ represents all the ways the formal parameters to a
-- function can be represented.
data Formals r
  = FormalName Text
  | FormalSet (FormalParamSet r)
  | FormalLeftAt Text (FormalParamSet r)
  | FormalRightAt (FormalParamSet r) Text
  deriving (Ord, Eq, Generic, Typeable, Data, Functor, Show, Foldable, Traversable)

-- | @formalsAsMap@ combines the outer and inner name bindings of
-- 'Formals'
formalsAsMap :: Formals r -> Map Text (Maybe r)
formalsAsMap (FormalName n) = Map.singleton n Nothing
formalsAsMap (FormalSet (FormalParamSet s)) = s
formalsAsMap (FormalLeftAt n (FormalParamSet s)) = Map.insert n Nothing s
formalsAsMap (FormalRightAt (FormalParamSet s) n) = Map.insert n Nothing s

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

instance Show (Fix NExprF) where showsPrec p (Fix f) = showsPrec p f
instance Eq (Fix NExprF)   where Fix x == Fix y = x == y
instance Ord (Fix NExprF)  where compare (Fix x) (Fix y) = compare x y

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
instance Show (Fix NValueF) where showsPrec p (Fix f) = showsPrec p f

valueText :: NValue -> Text
valueText = cata phi where
    phi (NVConstant a)   = atomText a
    phi (NVStr t)        = t
    phi (NVList _)       = error "Cannot coerce a list to a string"
    phi (NVSet _)        = error "Cannot coerce a set to a string"
    phi (NVFunction _ _) = error "Cannot coerce a function to a string"
