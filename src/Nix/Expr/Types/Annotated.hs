{-# language CPP                #-}
{-# language DeriveAnyClass     #-}
{-# language KindSignatures     #-}
{-# language PatternSynonyms    #-}
{-# language RankNTypes         #-}
{-# language TemplateHaskell    #-}

-- | The source location annotated nix expression type and supporting types.
--
module Nix.Expr.Types.Annotated
  ( module Nix.Expr.Types.Annotated
  , module Data.Functor.Compose
  , SourcePos(..)
  , unPos
  , mkPos
  )
where

import           Codec.Serialise
import           Control.DeepSeq
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                )
import           Data.Aeson.TH
import           Data.Binary                    ( Binary(..) )
import           Data.Data
import           Data.Eq.Deriving
import           Data.Fix                       ( Fix(..)
                                                , unfoldFix )
import           Data.Functor.Compose
import           Data.Hashable.Lifted
import           Data.Ord.Deriving
import           GHC.Generics
import           Nix.Utils
import           Nix.Atoms
import           Nix.Expr.Types
import           Text.Megaparsec                ( unPos
                                                , mkPos
                                                )
import           Text.Megaparsec.Pos            ( SourcePos(..) )
import           Text.Read.Deriving
import           Text.Show.Deriving

-- * data type @SrcSpan@ - a zone in a source file

-- | Demarcation of a chunk in a source file.
data SrcSpan = SrcSpan
  { spanBegin :: SourcePos
  , spanEnd   :: SourcePos
  }
  deriving (Ord, Eq, Generic, Typeable, Data, Show, NFData, Hashable)

-- ** Instances

instance Semigroup SrcSpan where
  s1 <> s2 =
    SrcSpan
      ((min `on` spanBegin) s1 s2)
      ((max `on` spanEnd  ) s1 s2)

instance Binary SrcSpan
instance ToJSON SrcSpan
instance FromJSON SrcSpan

instance Serialise SrcSpan

-- * data type @Ann@

-- | A type constructor applied to a type along with an annotation
--
-- Intended to be used with 'Fix':
-- @type MyType = Fix (Compose (AnnUnit Annotation) F)@
data AnnUnit ann expr = AnnUnit
  { annotation :: ann
  , annotated  :: expr
  }
 deriving
  ( Eq, Ord, Data, Typeable, Hashable
  , Generic, Generic1, NFData
  , Functor, Foldable, Traversable
  , Show, Read
  )

type AnnF ann f = Compose (AnnUnit ann) f

-- | Pattern: @(Compose (AnnUnit _ _))@.
pattern AnnF
  :: ann
  -> f a
  -> Compose (AnnUnit ann) f a
pattern AnnF ann f = Compose (AnnUnit ann f)
{-# complete AnnF #-}


type Ann ann f = Fix (AnnF ann f)

-- | Pattern: @Fix (Compose (AnnUnit _ _))@.
-- Fix composes units of (annotations & the annotated) into one object.
-- Giving annotated expression.
pattern Ann
  :: forall ann (f :: Type -> Type)
  . ann
  -> f (Ann ann f)
  -> Ann ann f
pattern Ann ann a = Fix (AnnF ann a)
{-# complete Ann #-}

annUnitToAnn :: AnnUnit ann (f (Ann ann f)) -> Ann ann f
annUnitToAnn (AnnUnit ann a) = Ann ann a

-- ** Instances

instance Hashable ann => Hashable1 (AnnUnit ann)

instance NFData ann => NFData1 (AnnUnit ann)

instance (Binary ann, Binary a) => Binary (AnnUnit ann a)

$(deriveEq1   ''AnnUnit)
$(deriveEq2   ''AnnUnit)
$(deriveOrd1  ''AnnUnit)
$(deriveOrd2  ''AnnUnit)
$(deriveRead1 ''AnnUnit)
$(deriveRead2 ''AnnUnit)
$(deriveShow1 ''AnnUnit)
$(deriveShow2 ''AnnUnit)
$(deriveJSON1 defaultOptions ''AnnUnit)
$(deriveJSON2 defaultOptions ''AnnUnit)

instance (Serialise ann, Serialise a) => Serialise (AnnUnit ann a)

-- ** @NExprLoc{,F}@ - annotated Nix expression

type NExprLocF = AnnF SrcSpan NExprF

instance Serialise r => Serialise (NExprLocF r) where
  encode (AnnF ann a) = encode ann <> encode a
  decode =
    liftA2 AnnF
      decode
      decode

instance Binary r => Binary (NExprLocF r)

-- | Annotated Nix expression (each subexpression direct to its source location).
type NExprLoc = Fix NExprLocF

instance Serialise NExprLoc

instance Binary NExprLoc

-- * Other

stripAnnF :: AnnF ann f r -> f r
stripAnnF = annotated . getCompose

stripAnnotation :: Functor f => Ann ann f -> Fix f
stripAnnotation = unfoldFix (stripAnnF . unFix)

annNUnary :: AnnUnit SrcSpan NUnaryOp -> NExprLoc -> NExprLoc
annNUnary (AnnUnit s1 u) e1@(Ann s2 _) = Ann (s1 <> s2) $ NUnary u e1
{-# inline annNUnary #-}

annNBinary :: AnnUnit SrcSpan NBinaryOp -> NExprLoc -> NExprLoc -> NExprLoc
annNBinary (AnnUnit s1 b) e1@(Ann s2 _) e2@(Ann s3 _) =
  Ann (s1 <> s2 <> s3) $ NBinary b e1 e2

annNSelect
  :: Maybe NExprLoc -> NExprLoc -> AnnUnit SrcSpan (NAttrPath NExprLoc) -> NExprLoc
annNSelect Nothing e1@(Ann s2 _) (AnnUnit s1 ats) = Ann (s2 <> s1) $ NSelect Nothing e1 ats
annNSelect (Just e3@(Ann s3 _)) e2@(Ann s2 _) (AnnUnit s1 ats) = Ann (s3 <> s2 <> s1) $ NSelect (pure e3) e2 ats

annNHasAttr :: NExprLoc -> AnnUnit SrcSpan (NAttrPath NExprLoc) -> NExprLoc
annNHasAttr e1@(Ann s1 _) (AnnUnit s2 ats) = Ann (s1 <> s2) $ NHasAttr e1 ats

annNApp :: NExprLoc -> NExprLoc -> NExprLoc
annNApp e1@(Ann s1 _) e2@(Ann s2 _) = Ann (s1 <> s2) $ NBinary NApp e1 e2

annNAbs :: AnnUnit SrcSpan (Params NExprLoc) -> NExprLoc -> NExprLoc
annNAbs (AnnUnit s1 ps) e1@(Ann s2 _) = Ann (s1 <> s2) $ NAbs ps e1

annNStr :: AnnUnit SrcSpan (NString NExprLoc) -> NExprLoc
annNStr (AnnUnit s1 s) = Ann s1 $ NStr s

deltaInfo :: SourcePos -> (Text, Int, Int)
deltaInfo (SourcePos fp l c) = (toText fp, unPos l, unPos c)

annNNull :: NExprLoc
annNNull = Ann nullSpan $ NConstant NNull
{-# inline annNNull #-}

nullSpan :: SrcSpan
nullSpan = SrcSpan nullPos nullPos
{-# inline nullSpan #-}

-- | Pattern systems for matching on @NExprLocF@ constructions.

pattern NConstantAnnF :: SrcSpan -> NAtom -> NExprLocF r
pattern NConstantAnnF ann x = AnnF ann (NConstant x)

pattern NStrAnnF :: SrcSpan -> NString r -> NExprLocF r
pattern NStrAnnF ann x = AnnF ann (NStr x)

pattern NSymAnnF :: SrcSpan -> VarName -> NExprLocF r
pattern NSymAnnF ann x = AnnF ann (NSym x)

pattern NListAnnF :: SrcSpan -> [r] -> NExprLocF r
pattern NListAnnF ann x = AnnF ann (NList x)

pattern NSetAnnF :: SrcSpan -> Recursivity -> [Binding r] -> NExprLocF r
pattern NSetAnnF ann recur x = AnnF ann (NSet recur x)

pattern NLiteralPathAnnF :: SrcSpan -> Path -> NExprLocF r
pattern NLiteralPathAnnF ann x = AnnF ann (NLiteralPath x)

pattern NEnvPathAnnF :: SrcSpan -> Path -> NExprLocF r
pattern NEnvPathAnnF ann x = AnnF ann (NEnvPath x)

pattern NUnaryAnnF :: SrcSpan -> NUnaryOp -> r -> NExprLocF r
pattern NUnaryAnnF ann op x = AnnF ann (NUnary op x)

pattern NBinaryAnnF :: SrcSpan -> NBinaryOp -> r -> r -> NExprLocF r
pattern NBinaryAnnF ann op x y = AnnF ann (NBinary op x y)

pattern NSelectAnnF :: SrcSpan ->  Maybe r -> r -> NAttrPath r -> NExprLocF r
pattern NSelectAnnF ann v x p = AnnF ann (NSelect v x p)

pattern NHasAttr_ :: SrcSpan -> r -> NAttrPath r -> NExprLocF r
pattern NHasAttr_ ann x p = AnnF ann (NHasAttr x p)

pattern NAbs_ :: SrcSpan -> Params r-> r -> NExprLocF r
pattern NAbs_ ann x b = AnnF ann (NAbs x b)

pattern NLet_ :: SrcSpan -> [Binding r] -> r -> NExprLocF r
pattern NLet_ ann x b = AnnF ann (NLet x b)

pattern NIf_ :: SrcSpan -> r -> r -> r -> NExprLocF r
pattern NIf_ ann c t e = AnnF ann (NIf c t e)

pattern NWith_ :: SrcSpan -> r -> r -> NExprLocF r
pattern NWith_ ann x y = AnnF ann (NWith x y)

pattern NAssert_ :: SrcSpan -> r -> r -> NExprLocF r
pattern NAssert_ ann x y = AnnF ann (NAssert x y)

pattern NSynHole_ :: SrcSpan -> VarName -> NExprLocF r
pattern NSynHole_ ann x = AnnF ann (NSynHole x)
{-# complete NConstantAnnF, NStrAnnF, NSymAnnF, NListAnnF, NSetAnnF, NLiteralPathAnnF, NEnvPathAnnF, NUnaryAnnF, NBinaryAnnF, NSelectAnnF, NHasAttr_, NAbs_, NLet_, NIf_, NWith_, NAssert_, NSynHole_ #-}


pattern PNConstant :: SrcSpan -> NAtom -> NExprLoc
pattern PNConstant ann x = Ann ann (NConstant x)

pattern PNStr :: SrcSpan -> NString NExprLoc -> NExprLoc
pattern PNStr ann x = Ann ann (NStr x)

pattern PNSym :: SrcSpan -> VarName -> NExprLoc
pattern PNSym ann x = Ann ann (NSym x)

pattern PNList :: SrcSpan -> [NExprLoc] -> NExprLoc
pattern PNList ann x = Ann ann (NList x)

pattern PNSet :: SrcSpan -> Recursivity -> [Binding NExprLoc] -> NExprLoc
pattern PNSet ann recur x = Ann ann (NSet recur x)

pattern PNLiteralPath :: SrcSpan -> Path -> NExprLoc
pattern PNLiteralPath ann x = Ann ann (NLiteralPath x)

pattern PNEnvPath :: SrcSpan -> Path -> NExprLoc
pattern PNEnvPath ann x = Ann ann (NEnvPath x)

pattern PNUnary :: SrcSpan -> NUnaryOp -> NExprLoc -> NExprLoc
pattern PNUnary ann op x = Ann ann (NUnary op x)

pattern PNBinary :: SrcSpan -> NBinaryOp -> NExprLoc -> NExprLoc -> NExprLoc
pattern PNBinary ann op x y = Ann ann (NBinary op x y)

pattern PNSelect :: SrcSpan ->  Maybe NExprLoc -> NExprLoc -> NAttrPath NExprLoc -> NExprLoc
pattern PNSelect ann v x p = Ann ann (NSelect v x p)

pattern PNHasAttr :: SrcSpan -> NExprLoc -> NAttrPath NExprLoc -> NExprLoc
pattern PNHasAttr ann x p = Ann ann (NHasAttr x p)

pattern PNAbs :: SrcSpan -> Params NExprLoc -> NExprLoc -> NExprLoc
pattern PNAbs ann x b = Ann ann (NAbs x b)

pattern PNLet :: SrcSpan -> [Binding NExprLoc] -> NExprLoc -> NExprLoc
pattern PNLet ann x b = Ann ann (NLet x b)

pattern PNIf :: SrcSpan -> NExprLoc -> NExprLoc -> NExprLoc -> NExprLoc
pattern PNIf ann c t e = Ann ann (NIf c t e)

pattern PNWith :: SrcSpan -> NExprLoc -> NExprLoc -> NExprLoc
pattern PNWith ann x y = Ann ann (NWith x y)

pattern PNAssert :: SrcSpan -> NExprLoc -> NExprLoc -> NExprLoc
pattern PNAssert ann x y = Ann ann (NAssert x y)

pattern PNSynHole :: SrcSpan -> VarName -> NExprLoc
pattern PNSynHole ann x = Ann ann (NSynHole x)
{-# complete PNConstant, PNStr, PNSym, PNList, PNSet, PNLiteralPath, PNEnvPath, PNUnary, PNBinary, PNSelect, PNHasAttr, PNAbs, PNLet, PNIf, PNWith, PNAssert, PNSynHole #-}
