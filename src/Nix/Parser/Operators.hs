{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Parser.Operators where

import           Data.Data (Data(..))
import           Data.Foldable (concat)
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           GHC.Generics hiding (Prefix)
import           Nix.Expr
import           Nix.Parser.Library
import           Text.Megaparsec.Expr

data NSpecialOp = NHasAttrOp | NSelectOp | NAppOp
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

data NAssoc = NAssocNone | NAssocLeft | NAssocRight
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

data NOperatorDef
  = NUnaryDef Text NUnaryOp
  | NBinaryDef NAssoc [(Text, NBinaryOp)]
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

annotateLocation :: Parser a -> Parser (Ann SrcSpan a)
annotateLocation p = do
  begin <- getPosition
  res   <- p
  end   <- getPosition
  pure $ Ann (SrcSpan begin end) res

annotateLocation1 :: Parser (NExprF NExprLoc) -> Parser NExprLoc
annotateLocation1 = fmap annToAnnF . annotateLocation

opWithLoc :: Text -> o -> (Ann SrcSpan o -> a) -> Parser a
opWithLoc name op f = do
    Ann ann _ <- annotateLocation (symbol name)
    return $ f (Ann ann op)

binaryN name op = (Right (op, NAssocNone),
                   name, InfixN  (opWithLoc name op nBinary))
binaryL name op = (Right (op, NAssocLeft),
                   name, InfixL  (opWithLoc name op nBinary))
binaryR name op = (Right (op, NAssocRight),
                   name, InfixR  (opWithLoc name op nBinary))
prefix  name op = (Left  op, name, Prefix  (opWithLoc name op nUnary))
postfix name op = (Left  op, name, Postfix (opWithLoc name op nUnary))

nixOperators
    :: [[(Either NUnaryOp (NBinaryOp, NAssoc),
         Text, Operator Parser NExprLoc)]]
nixOperators =
  [ {-  1 -} [ binaryL "."  NSelect ]
  , {-  2 -} [ binaryL " "  NApp ]
  , {-  3 -} [ prefix  "-"  NNeg ]
  , {-  4 -} [ binaryL "?"  NHasAttr ]
  , {-  5 -} [ binaryR "++" NConcat ]
  , {-  6 -} [ binaryL "*"  NMult
             , binaryL "/"  NDiv ]
  , {-  7 -} [ binaryL "+"  NPlus
             , binaryL "-"  NMinus ]
  , {-  8 -} [ prefix  "!"  NNot ]
  , {-  9 -} [ binaryR "//" NUpdate ]
  , {- 10 -} [ binaryL "<"  NLt
             , binaryL ">"  NGt
             , binaryL "<=" NLte
             , binaryL ">=" NGte ]
  , {- 11 -} [ binaryN "==" NEq
             , binaryN "!=" NNEq ]
  , {- 12 -} [ binaryL "&&" NAnd ]
  , {- 13 -} [ binaryL "||" NOr ]
  , {- 14 -} [ binaryN "->" NImpl ]
  ]

data OperatorInfo = OperatorInfo
  { precedence    :: Int
  , associativity :: NAssoc
  , operatorName  :: Text
  } deriving (Eq, Ord, Generic, Typeable, Data, Show)

getUnaryOperator :: NUnaryOp -> OperatorInfo
getUnaryOperator = (m Map.!) where
  m = Map.fromList $ concat $ zipWith buildEntry [1..] nixOperators
  buildEntry i = concatMap $ \case
    (Left op, name, _) -> [(op, OperatorInfo i NAssocNone name)]
    _ -> []

getBinaryOperator :: NBinaryOp -> OperatorInfo
getBinaryOperator = (m Map.!) where
  m = Map.fromList $ concat $ zipWith buildEntry [1..] nixOperators
  buildEntry i = concatMap $ \case
    (Right (op, assoc), name, _) -> [(op, OperatorInfo i assoc name)]
    _ -> []
