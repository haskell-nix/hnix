{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Parser.Operators where

import           Control.DeepSeq
import           Data.Data (Data(..))
import           Data.Foldable (concat)
import qualified Data.Map as Map
import           Data.Text (Text, unpack)
import           Data.Typeable (Typeable)
import           GHC.Generics hiding (Prefix)
import           Nix.Expr
import           Nix.Parser.Library
import           Text.Megaparsec.Expr

data NSpecialOp = NHasAttrOp | NSelectOp
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

data NAssoc = NAssocNone | NAssocLeft | NAssocRight
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

data NOperatorDef
  = NUnaryDef Text NUnaryOp
  | NBinaryDef Text NBinaryOp NAssoc
  | NSpecialDef Text NSpecialOp NAssoc
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

annotateLocation :: Parser a -> Parser (Ann SrcSpan a)
annotateLocation p = do
  begin <- getPosition
  res   <- p
  end   <- getPosition
  pure $ Ann (SrcSpan begin end) res

annotateLocation1 :: Parser (NExprF NExprLoc) -> Parser NExprLoc
annotateLocation1 = fmap annToAnnF . annotateLocation

manyUnaryOp f = foldr1 (.) <$> some f

operator "/" = lexeme . try $ string "/" <* notFollowedBy (char '/')
operator n   = symbol n

opWithLoc :: Text -> o -> (Ann SrcSpan o -> a) -> Parser a
opWithLoc name op f = do
    Ann ann _ <- annotateLocation $ {- dbg (unpack name) $ -} operator name
    return $ f (Ann ann op)

binaryN name op = (NBinaryDef name op NAssocNone,
                   InfixN  (opWithLoc name op nBinary))
binaryL name op = (NBinaryDef name op NAssocLeft,
                   InfixL  (opWithLoc name op nBinary))
binaryR name op = (NBinaryDef name op NAssocRight,
                   InfixR  (opWithLoc name op nBinary))
prefix  name op = (NUnaryDef name op,
                   Prefix  (manyUnaryOp (opWithLoc name op nUnary)))
postfix name op = (NUnaryDef name op,
                   Postfix (opWithLoc name op nUnary))

nixOperators
    :: Parser (Ann SrcSpan (NAttrPath NExprLoc))
    -> [[(NOperatorDef, Operator Parser NExprLoc)]]
nixOperators selector =
  [ -- This is not parsed here, even though technically it's part of the
    -- expression table. The problem is that in same cases, such as list
    -- membership, it's also a term. And since terms are effectively the
    -- highest precedence entities parsed by the expression parser, it ends up
    -- working out that we parse them as a kind of "meta-term".

    -- {-  1 -} [ (NSpecialDef "." NSelectOp NAssocLeft,
    --             Postfix $ do
    --                    sel <- seldot *> selector
    --                    mor <- optional (reserved "or" *> term)
    --                    return $ \x -> nSelectLoc x sel mor) ]

    {-  2 -} [ (NBinaryDef " " NApp NAssocLeft,
                -- Thanks to Brent Yorgey for showing me this trick!
                InfixL $ nApp <$ symbol "") ]
  , {-  3 -} [ prefix  "-"  NNeg ]
  , {-  4 -} [ (NSpecialDef "?" NHasAttrOp NAssocLeft,
                Postfix $ symbol "?" *> (flip nHasAttr <$> selector)) ]
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
  m = Map.fromList $ concat $ zipWith buildEntry [1..]
          (nixOperators (error "unused"))
  buildEntry i = concatMap $ \case
    (NUnaryDef name op, _) -> [(op, OperatorInfo i NAssocNone name)]
    _ -> []

getBinaryOperator :: NBinaryOp -> OperatorInfo
getBinaryOperator = (m Map.!) where
  m = Map.fromList $ concat $ zipWith buildEntry [1..]
          (nixOperators (error "unused"))
  buildEntry i = concatMap $ \case
    (NBinaryDef name op assoc, _) -> [(op, OperatorInfo i assoc name)]
    _ -> []

getSpecialOperator :: NSpecialOp -> OperatorInfo
getSpecialOperator NSelectOp = OperatorInfo 1 NAssocLeft "."
getSpecialOperator o = m Map.! o where
  m = Map.fromList $ concat $ zipWith buildEntry [1..]
          (nixOperators (error "unused"))
  buildEntry i = concatMap $ \case
    (NSpecialDef name op assoc, _) -> [(op, OperatorInfo i assoc name)]
    _ -> []
