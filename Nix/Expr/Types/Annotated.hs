{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | The source location annotated nix expression type and supporting types.
--
module Nix.Expr.Types.Annotated where

import           Control.Monad        hiding (forM_, mapM, sequence)
import           Data.Data
import           Data.Fix
import           Data.Functor.Compose
import           GHC.Exts
import           GHC.Generics
import           Nix.Expr.Types
import           Prelude              hiding (concat, concatMap, elem, foldr,
                                       mapM, minimum, readFile, sequence)

-- | A location in a source file
data SrcLoc = SrcLoc{ line   :: Int
                    , column :: Int
                    }
  deriving (Ord, Eq, Generic, Typeable, Data, Read, Show)

-- | A type constructor applied to a type along with an annotation
--
-- Intended to be used with 'Fix':
-- @type MyType = Fix (Compose (Ann Annotation) F)@
data Ann ann a = Ann{ annotation :: ann
                    , annotated  :: a
                    }
  deriving (Ord, Eq, Data, Generic, Functor, Read, Show)

type AnnF ann f = Compose (Ann ann) f

annFToAnn :: Fix (AnnF ann f) -> Ann ann (Fix (AnnF ann f))
annFToAnn = undefined

annToAnnF :: Ann ann (f (Fix (AnnF ann f))) -> Fix (AnnF ann f)
annToAnnF (Ann ann a) = Fix (Compose (Ann ann a))

type NExprLocF = AnnF SrcLoc NExprF

-- | A nix expression with source location at each subexpression.
type NExprLoc = Fix NExprLocF

stripAnnotation :: Functor f => Fix (AnnF ann f) -> Fix f
stripAnnotation = ana (annotated . getCompose . unFix)

-- mergeSpans2 :: (a -> b -> c)
--             -> (Ann SrcLoc a -> Ann SrcLoc b -> Ann SrcLoc c)
-- mergeSpans2 = undefined

-- mergeSpansE2 :: (NExpr -> NExpr -> NExprF NExpr)
--              -> (NExprLoc -> NExprLoc -> NExprLoc)
-- mergeSpansE2 = undefined

-- mergeSpans3 :: (a -> b -> c -> d)
--             -> (Ann SrcLoc a -> Ann SrcLoc b -> Ann SrcLoc c -> Ann SrcLoc d)
-- mergeSpans3 = undefined

nApp :: NExprLoc -> NExprLoc -> NExprLoc
nApp = undefined

nUnary :: Ann SrcLoc NUnaryOp -> NExprLoc -> NExprLoc
nUnary = undefined

nBinary :: Ann SrcLoc NBinaryOp -> NExprLoc -> NExprLoc -> NExprLoc
nBinary = undefined

nSelectLoc :: NExprLoc -> Ann SrcLoc (NAttrPath NExprLoc) -> Maybe NExprLoc -> NExprLoc
nSelectLoc = undefined

nHasAttr :: NExprLoc -> Ann SrcLoc (NAttrPath NExprLoc) -> NExprLoc
nHasAttr = undefined

nAbs :: Ann SrcLoc (Params  NExprLoc) -> NExprLoc -> NExprLoc
nAbs = undefined

nStr :: NString NExprLoc -> NExprLoc
nStr = undefined
