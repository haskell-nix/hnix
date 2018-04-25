{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | The source location annotated nix expression type and supporting types.
--
module Nix.Expr.Types.Annotated
  ( module Nix.Expr.Types.Annotated
  , SourcePos(..), unPos
  )where

import Codec.Serialise
import Control.DeepSeq
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.TH
import Data.Binary (Binary(..))
import Data.Data
import Data.Eq.Deriving
import Data.Fix
import Data.Function (on)
import Data.Functor.Compose
import Data.Hashable
import Data.Hashable.Lifted
import Data.Ord.Deriving
import Data.Semigroup
import Data.Text (Text, pack)
import GHC.Generics
import Nix.Atoms
import Nix.Expr.Types
import Nix.Parser.Library (SourcePos(..))
import Text.Megaparsec (unPos, mkPos)
import Text.Read.Deriving
import Text.Show.Deriving

-- | A location in a source file
data SrcSpan = SrcSpan
    { spanBegin :: SourcePos
    , spanEnd   :: SourcePos
    }
    deriving (Ord, Eq, Generic, Typeable, Data, Show, NFData, Serialise,
              Hashable)

-- | A type constructor applied to a type along with an annotation
--
-- Intended to be used with 'Fix':
-- @type MyType = Fix (Compose (Ann Annotation) F)@
data Ann ann a = Ann
    { annotation :: ann
    , annotated  :: a
    }
    deriving (Ord, Eq, Data, Generic, Generic1, Typeable, Functor, Foldable,
              Traversable, Read, Show, NFData, NFData1, Serialise,
              Hashable, Hashable1)

$(deriveEq1   ''Ann)
$(deriveEq2   ''Ann)
$(deriveOrd1  ''Ann)
$(deriveOrd2  ''Ann)
$(deriveRead1 ''Ann)
$(deriveRead2 ''Ann)
$(deriveShow1 ''Ann)
$(deriveShow2 ''Ann)
$(deriveJSON1 defaultOptions ''Ann)
$(deriveJSON2 defaultOptions ''Ann)

instance Semigroup SrcSpan where
  s1 <> s2 = SrcSpan ((min `on` spanBegin) s1 s2)
                     ((max `on` spanEnd) s1 s2)

type AnnF ann f = Compose (Ann ann) f

annToAnnF :: Ann ann (f (Fix (AnnF ann f))) -> Fix (AnnF ann f)
annToAnnF (Ann ann a) = AnnE ann a

type NExprLocF = AnnF SrcSpan NExprF

-- | A nix expression with source location at each subexpression.
type NExprLoc = Fix NExprLocF

instance NFData NExprLoc
instance Serialise NExprLoc
instance Hashable NExprLoc

instance Binary SrcSpan
instance (Binary ann, Binary a) => Binary (Ann ann a)
instance Binary r => Binary (NExprLocF r)
instance Binary NExprLoc

instance ToJSON SrcSpan
instance FromJSON SrcSpan

instance Serialise r => Serialise (Compose (Ann SrcSpan) NExprF r) where
    encode (Compose (Ann ann a)) = encode ann <> encode a
    decode = (Compose .) . Ann <$> decode <*> decode

pattern AnnE :: forall ann (g :: * -> *). ann
             -> g (Fix (Compose (Ann ann) g)) -> Fix (Compose (Ann ann) g)
pattern AnnE ann a = Fix (Compose (Ann ann a))

stripAnnotation :: Functor f => Fix (AnnF ann f) -> Fix f
stripAnnotation = ana (annotated . getCompose . unFix)

stripAnn :: AnnF ann f r -> f r
stripAnn = annotated . getCompose

nUnary :: Ann SrcSpan NUnaryOp -> NExprLoc -> NExprLoc
nUnary (Ann s1 u) e1@(AnnE s2 _) = AnnE (s1 <> s2) (NUnary u e1)
nUnary _ _ = error "nUnary: unexpected"

nBinary :: Ann SrcSpan NBinaryOp -> NExprLoc -> NExprLoc -> NExprLoc
nBinary (Ann s1 b) e1@(AnnE s2 _) e2@(AnnE s3 _) =
  AnnE (s1 <> s2 <> s3) (NBinary b e1 e2)
nBinary _ _ _ = error "nBinary: unexpected"

nSelectLoc :: NExprLoc -> Ann SrcSpan (NAttrPath NExprLoc) -> Maybe NExprLoc
           -> NExprLoc
nSelectLoc e1@(AnnE s1 _) (Ann s2 ats) d = case d of
  Nothing               -> AnnE (s1 <> s2) (NSelect e1 ats Nothing)
  Just (e2@(AnnE s3 _)) -> AnnE (s1 <> s2 <> s3) (NSelect e1 ats (Just e2))
  _ -> error "nSelectLoc: unexpected"
nSelectLoc _ _ _ = error "nSelectLoc: unexpected"

nHasAttr :: NExprLoc -> Ann SrcSpan (NAttrPath NExprLoc) -> NExprLoc
nHasAttr e1@(AnnE s1 _) (Ann s2 ats) = AnnE (s1 <> s2) (NHasAttr e1 ats)
nHasAttr _ _ = error "nHasAttr: unexpected"

nApp :: NExprLoc -> NExprLoc -> NExprLoc
nApp e1@(AnnE s1 _) e2@(AnnE s2 _) = AnnE (s1 <> s2) (NBinary NApp e1 e2)
nApp _ _ = error "nApp: unexpected"

nAbs :: Ann SrcSpan (Params NExprLoc) -> NExprLoc -> NExprLoc
nAbs (Ann s1 ps) e1@(AnnE s2 _) = AnnE (s1 <> s2) (NAbs ps e1)
nAbs _ _ = error "nAbs: unexpected"

nStr :: Ann SrcSpan (NString NExprLoc) -> NExprLoc
nStr (Ann s1 s) = AnnE s1 (NStr s)

deltaInfo :: SourcePos -> (Text, Int, Int)
deltaInfo (SourcePos fp l c) = (pack fp, unPos l, unPos c)

nNull :: NExprLoc
nNull = Fix (Compose (Ann nullAnn (NConstant NNull)))

nullAnn :: SrcSpan
nullAnn = SrcSpan nullPos nullPos

nullPos :: SourcePos
nullPos = SourcePos "<unknown>" (mkPos 1) (mkPos 1)

-- | Pattern systems for matching on NExprLocF constructions.

pattern NSym_ :: SrcSpan -> VarName -> NExprLocF r
pattern NSym_ ann x = Compose (Ann ann (NSym x))

pattern NConstant_ :: SrcSpan -> NAtom -> NExprLocF r
pattern NConstant_ ann x = Compose (Ann ann (NConstant x))

pattern NStr_ :: SrcSpan -> NString r -> NExprLocF r
pattern NStr_ ann x = Compose (Ann ann (NStr x))

pattern NList_ :: SrcSpan -> [r] -> NExprLocF r
pattern NList_ ann x = Compose (Ann ann (NList x))

pattern NSet_ :: SrcSpan -> [Binding r] -> NExprLocF r
pattern NSet_ ann x = Compose (Ann ann (NSet x))

pattern NRecSet_ :: SrcSpan -> [Binding r] -> NExprLocF r
pattern NRecSet_ ann x = Compose (Ann ann (NRecSet x))

pattern NLiteralPath_ :: SrcSpan -> FilePath -> NExprLocF r
pattern NLiteralPath_ ann x = Compose (Ann ann (NLiteralPath x))

pattern NEnvPath_ :: SrcSpan -> FilePath -> NExprLocF r
pattern NEnvPath_ ann x = Compose (Ann ann (NEnvPath x))

pattern NSelect_ :: SrcSpan -> r -> NAttrPath r -> Maybe r -> NExprLocF r
pattern NSelect_ ann x p v = Compose (Ann ann (NSelect x p v))

pattern NHasAttr_ :: SrcSpan -> r -> NAttrPath r -> NExprLocF r
pattern NHasAttr_ ann x p = Compose (Ann ann (NHasAttr x p))

pattern NAbs_ :: SrcSpan -> Params r-> r -> NExprLocF r
pattern NAbs_ ann x b = Compose (Ann ann (NAbs x b))

pattern NLet_ :: SrcSpan -> [Binding r] -> r -> NExprLocF r
pattern NLet_ ann x b = Compose (Ann ann (NLet x b))

pattern NIf_ :: SrcSpan -> r -> r -> r -> NExprLocF r
pattern NIf_ ann c t e = Compose (Ann ann (NIf c t e))

pattern NWith_ :: SrcSpan -> r -> r -> NExprLocF r
pattern NWith_ ann x y = Compose (Ann ann (NWith x y))

pattern NAssert_ :: SrcSpan -> r -> r -> NExprLocF r
pattern NAssert_ ann x y = Compose (Ann ann (NAssert x y))

pattern NUnary_ :: SrcSpan -> NUnaryOp -> r -> NExprLocF r
pattern NUnary_ ann op x = Compose (Ann ann (NUnary op x))

pattern NBinary_ :: SrcSpan -> NBinaryOp -> r -> r -> NExprLocF r
pattern NBinary_ ann op x y = Compose (Ann ann (NBinary op x y))
