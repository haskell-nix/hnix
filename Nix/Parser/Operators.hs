module Nix.Parser.Operators where

import           Data.Data (Data(..))
import           Data.Foldable (concat)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Typeable (Typeable)
import           GHC.Generics
import           Nix.Expr

data NSpecialOp = NHasAttrOp | NSelectOp | NAppOp
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
  , Right $ NUnaryDef "-" NNeg
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
  m = Map.fromList . concat . zipWith buildEntry [1..] . reverse $
        nixOperators
  buildEntry i = \case
    Right (NUnaryDef name op) -> [(op, OperatorInfo i NAssocNone name)]
    _ -> []

getBinaryOperator :: NBinaryOp -> OperatorInfo
getBinaryOperator = (m Map.!) where
  m = Map.fromList . concat . zipWith buildEntry [1..] . reverse $
        nixOperators
  buildEntry i = \case
    Right (NBinaryDef assoc ops) ->
      [(op, OperatorInfo i assoc name) | (name,op) <- ops]
    _ -> []

getSpecialOperatorPrec :: NSpecialOp -> Int
getSpecialOperatorPrec = (m Map.!) where
  m = Map.fromList . catMaybes . zipWith buildEntry [1..] . reverse $
        nixOperators
  buildEntry _ (Right _) = Nothing
  buildEntry i (Left op) = Just (op, i)

selectOp :: OperatorInfo
selectOp = OperatorInfo (getSpecialOperatorPrec NSelectOp) NAssocLeft "."

hasAttrOp :: OperatorInfo
hasAttrOp = OperatorInfo (getSpecialOperatorPrec NHasAttrOp) NAssocLeft "?"

appOp :: OperatorInfo
appOp = OperatorInfo (getSpecialOperatorPrec NAppOp) NAssocLeft " "
