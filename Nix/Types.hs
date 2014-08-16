{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Types where

import           Control.Monad hiding (forM_, mapM, sequence)
import           Data.Data
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Text hiding (concat, concatMap, head, map, zipWith, reverse, intercalate)
import           Data.Traversable
import           Data.Tuple (swap)
import           GHC.Exts
import           GHC.Generics
import           Prelude hiding (readFile, concat, concatMap, elem, mapM,
                                 sequence)

newtype Fix (f :: * -> *) = Fix { outF :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . outF

cataM :: (Traversable f, Monad m) => (f a -> m a) -> Fix f -> m a
cataM f = f <=< mapM (cataM f) . outF

data NAtom
  = NInt Integer
  | NPath FilePath
  | NBool Bool
  | NSym Text
  | NNull
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

atomText :: NAtom -> Text
atomText (NInt i)  = pack (show i)
atomText (NPath p) = pack p
atomText (NBool b) = if b then "true" else "false"
atomText (NSym s)  = s
atomText NNull     = "null"

-- | 'Antiquoted' represents an expression that is either
-- antiquoted (surrounded by ${...}) or plain (not antiquoted).
data Antiquoted v r = Plain v | Antiquoted r
  deriving (Ord, Eq, Generic, Typeable, Data, Functor, Show)

runAntiquoted :: (v -> a) -> (r -> a) -> Antiquoted v r -> a
runAntiquoted f _ (Plain v) = f v
runAntiquoted _ f (Antiquoted r) = f r

-- | A 'NixString' is a list of things that are either a plain string
-- or an antiquoted expression. After the antiquotes have been evaluated,
-- the final string is constructed by concating all the parts.
newtype NString r = NString [Antiquoted Text r]
  deriving (Eq, Ord, Generic, Typeable, Data, Functor, Show)

escapeCodes :: [(Char, Char)]
escapeCodes =
  [ ('\n', 'n' )
  , ('\r', 'r' )
  , ('\t', 't' )
  , ('\\', '\\')
  , ('$' , '$' )
  , ('"' , '"' )
  , ('\'', '\'')
  ]

fromEscapeCode :: Char -> Maybe Char
fromEscapeCode = (`lookup` map swap escapeCodes)

toEscapeCode :: Char -> Maybe Char
toEscapeCode = (`lookup` escapeCodes)

instance IsString (NString r) where
  fromString = NString . (:[]) . Plain . pack

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
  m = Map.fromList . catMaybes . zipWith buildEntry [1..] $ nixOperators
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

mkStr :: Text -> NExpr
mkStr = Fix . NStr . NString . (:[]) . Plain

mkPath :: FilePath -> NExpr
mkPath = Fix . NConstant . NPath

mkSym :: Text -> NExpr
mkSym = Fix . NConstant . NSym

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
