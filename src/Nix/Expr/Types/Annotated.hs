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
  )
where

import           Nix.Prelude
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
                                                , unfoldFix
                                                )
import           Data.Functor.Compose
import           Data.Hashable.Lifted
import           Data.Ord.Deriving
import           GHC.Generics
import           Nix.Atoms
import           Nix.Expr.Types
import           Text.Read.Deriving
import           Text.Show.Deriving

-- * data type @SrcSpan@ - a zone in a source file

-- | Demarcation of a chunk in a source file.
data SrcSpan = SrcSpan
  { getSpanBegin :: NSourcePos
  , getSpanEnd   :: NSourcePos
  }
 deriving (Ord, Eq, Generic, Typeable, Data, Show, NFData, Hashable)

-- ** Instances

instance Semigroup SrcSpan where
  s1 <> s2 =
    SrcSpan
      (on min getSpanBegin s1 s2)
      (on max getSpanEnd   s1 s2)

instance Binary SrcSpan
instance ToJSON SrcSpan
instance FromJSON SrcSpan

instance Serialise SrcSpan

-- * data type @Ann@

--  2021-08-02: NOTE: Annotation needs to be after what is annotated.
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

instance Hashable ann => Hashable1 (AnnUnit ann)

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
annNUnary (AnnUnit s1 u) e1@(Ann s2 _) = NUnaryAnn (s1 <> s2) u e1
{-# inline annNUnary #-}

annNBinary :: AnnUnit SrcSpan NBinaryOp -> NExprLoc -> NExprLoc -> NExprLoc
annNBinary (AnnUnit s1 b) e1@(Ann s2 _) e2@(Ann s3 _) = NBinaryAnn (s1 <> s2 <> s3) b e1 e2

annNSelect
  :: Maybe NExprLoc -> NExprLoc -> AnnUnit SrcSpan (NAttrPath NExprLoc) -> NExprLoc
annNSelect  Nothing             e2@(Ann s2 _) (AnnUnit s1 ats) = NSelectAnn (      s2 <> s1)  Nothing  e2 ats
annNSelect (Just e3@(Ann s3 _)) e2@(Ann s2 _) (AnnUnit s1 ats) = NSelectAnn (s3 <> s2 <> s1) (pure e3) e2 ats

annNHasAttr :: NExprLoc -> AnnUnit SrcSpan (NAttrPath NExprLoc) -> NExprLoc
annNHasAttr e1@(Ann s1 _) (AnnUnit s2 ats) = NHasAttrAnn (s1 <> s2) e1 ats

annNApp :: NExprLoc -> NExprLoc -> NExprLoc
annNApp e1@(Ann s1 _) e2@(Ann s2 _) = NAppAnn (s1 <> s2) e1 e2

annNAbs :: AnnUnit SrcSpan (Params NExprLoc) -> NExprLoc -> NExprLoc
annNAbs (AnnUnit s1 ps) e1@(Ann s2 _) = NAbsAnn (s1 <> s2) ps e1

annNStr :: AnnUnit SrcSpan (NString NExprLoc) -> NExprLoc
annNStr (AnnUnit s1 s) = NStrAnn s1 s

deltaInfo :: NSourcePos -> (Text, Int, Int)
deltaInfo (NSourcePos fp l c) = (fromString $ coerce fp, unPos $ coerce l, unPos $ coerce c)

annNNull :: NExprLoc
annNNull = NConstantAnn nullSpan NNull
{-# inline annNNull #-}

nullSpan :: SrcSpan
nullSpan = SrcSpan nullPos nullPos
{-# inline nullSpan #-}

-- ** Patterns

-- *** Patterns to match on 'NExprLocF' constructions (for 'SrcSpan'-based annotations).

pattern NConstantAnnF    :: SrcSpan -> NAtom -> NExprLocF r
pattern NConstantAnnF    ann x      = AnnF ann (NConstant x)

pattern NStrAnnF         :: SrcSpan -> NString r -> NExprLocF r
pattern NStrAnnF         ann x      = AnnF ann (NStr x)

pattern NSymAnnF         :: SrcSpan -> VarOffset -> VarName -> NExprLocF r
pattern NSymAnnF         ann x y    = AnnF ann (NSym x y)

pattern NListAnnF        :: SrcSpan -> [r] -> NExprLocF r
pattern NListAnnF        ann x      = AnnF ann (NList x)

pattern NSetAnnF         :: SrcSpan -> Recursivity -> [Binding r] -> NExprLocF r
pattern NSetAnnF         ann rec x  = AnnF ann (NSet rec x)

pattern NLiteralPathAnnF :: SrcSpan -> Path -> NExprLocF r
pattern NLiteralPathAnnF ann x      = AnnF ann (NLiteralPath x)

pattern NEnvPathAnnF     :: SrcSpan -> Path -> NExprLocF r
pattern NEnvPathAnnF     ann x      = AnnF ann (NEnvPath x)

pattern NUnaryAnnF       :: SrcSpan -> NUnaryOp -> r -> NExprLocF r
pattern NUnaryAnnF       ann op x   = AnnF ann (NUnary op x)

pattern NAppAnnF         :: SrcSpan -> r -> r -> NExprLocF r
pattern NAppAnnF         ann x y    = AnnF ann (NApp x y)

pattern NBinaryAnnF      :: SrcSpan -> NBinaryOp -> r -> r -> NExprLocF r
pattern NBinaryAnnF      ann op x y = AnnF ann (NBinary op x y)

pattern NSelectAnnF      :: SrcSpan ->  Maybe r -> r -> NAttrPath r -> NExprLocF r
pattern NSelectAnnF      ann v x p  = AnnF ann (NSelect v x p)

pattern NHasAttrAnnF     :: SrcSpan -> r -> NAttrPath r -> NExprLocF r
pattern NHasAttrAnnF     ann x p    = AnnF ann (NHasAttr x p)

pattern NAbsAnnF         :: SrcSpan -> Params r-> r -> NExprLocF r
pattern NAbsAnnF         ann x b    = AnnF ann (NAbs x b)

pattern NLetAnnF         :: SrcSpan -> [Binding r] -> r -> NExprLocF r
pattern NLetAnnF         ann x b    = AnnF ann (NLet x b)

pattern NIfAnnF          :: SrcSpan -> r -> r -> r -> NExprLocF r
pattern NIfAnnF          ann c t e  = AnnF ann (NIf c t e)

pattern NWithAnnF        :: SrcSpan -> r -> r -> NExprLocF r
pattern NWithAnnF        ann x y    = AnnF ann (NWith x y)

pattern NAssertAnnF      :: SrcSpan -> r -> r -> NExprLocF r
pattern NAssertAnnF      ann x y    = AnnF ann (NAssert x y)

pattern NSynHoleAnnF     :: SrcSpan -> VarName -> NExprLocF r
pattern NSynHoleAnnF     ann x      = AnnF ann (NSynHole x)
{-# complete NConstantAnnF, NStrAnnF, NSymAnnF, NListAnnF, NSetAnnF, NLiteralPathAnnF, NEnvPathAnnF, NUnaryAnnF, NBinaryAnnF, NSelectAnnF, NHasAttrAnnF, NAbsAnnF, NLetAnnF, NIfAnnF, NWithAnnF, NAssertAnnF, NSynHoleAnnF #-}


-- *** Patterns to match on 'NExprLoc' constructions (for 'SrcSpan'-based annotations).

pattern NConstantAnn    :: SrcSpan -> NAtom -> NExprLoc
pattern NConstantAnn    ann x      = Ann ann (NConstant x)

pattern NStrAnn         :: SrcSpan -> NString NExprLoc -> NExprLoc
pattern NStrAnn         ann x      = Ann ann (NStr x)

pattern NSymAnn         :: SrcSpan -> VarOffset -> VarName -> NExprLoc
pattern NSymAnn         ann x y    = Ann ann (NSym x y)

pattern NListAnn        :: SrcSpan -> [NExprLoc] -> NExprLoc
pattern NListAnn        ann x      = Ann ann (NList x)

pattern NSetAnn         :: SrcSpan -> Recursivity -> [Binding NExprLoc] -> NExprLoc
pattern NSetAnn         ann rec x  = Ann ann (NSet rec x)

pattern NLiteralPathAnn :: SrcSpan -> Path -> NExprLoc
pattern NLiteralPathAnn ann x      = Ann ann (NLiteralPath x)

pattern NEnvPathAnn     :: SrcSpan -> Path -> NExprLoc
pattern NEnvPathAnn     ann x      = Ann ann (NEnvPath x)

pattern NUnaryAnn       :: SrcSpan -> NUnaryOp -> NExprLoc -> NExprLoc
pattern NUnaryAnn       ann op x   = Ann ann (NUnary op x)

pattern NAppAnn         :: SrcSpan -> NExprLoc -> NExprLoc -> NExprLoc
pattern NAppAnn         ann x y    = Ann ann (NApp x y)

pattern NBinaryAnn      :: SrcSpan -> NBinaryOp -> NExprLoc -> NExprLoc -> NExprLoc
pattern NBinaryAnn      ann op x y = Ann ann (NBinary op x y)

pattern NSelectAnn      :: SrcSpan ->  Maybe NExprLoc -> NExprLoc -> NAttrPath NExprLoc -> NExprLoc
pattern NSelectAnn      ann v x p  = Ann ann (NSelect v x p)

pattern NHasAttrAnn     :: SrcSpan -> NExprLoc -> NAttrPath NExprLoc -> NExprLoc
pattern NHasAttrAnn     ann x p    = Ann ann (NHasAttr x p)

pattern NAbsAnn         :: SrcSpan -> Params NExprLoc -> NExprLoc -> NExprLoc
pattern NAbsAnn         ann x b    = Ann ann (NAbs x b)

pattern NLetAnn         :: SrcSpan -> [Binding NExprLoc] -> NExprLoc -> NExprLoc
pattern NLetAnn         ann x b    = Ann ann (NLet x b)

pattern NIfAnn          :: SrcSpan -> NExprLoc -> NExprLoc -> NExprLoc -> NExprLoc
pattern NIfAnn          ann c t e  = Ann ann (NIf c t e)

pattern NWithAnn        :: SrcSpan -> NExprLoc -> NExprLoc -> NExprLoc
pattern NWithAnn        ann x y    = Ann ann (NWith x y)

pattern NAssertAnn      :: SrcSpan -> NExprLoc -> NExprLoc -> NExprLoc
pattern NAssertAnn      ann x y    = Ann ann (NAssert x y)

pattern NSynHoleAnn     :: SrcSpan -> VarName -> NExprLoc
pattern NSynHoleAnn     ann x      = Ann ann (NSynHole x)
{-# complete NConstantAnn, NStrAnn, NSymAnn, NListAnn, NSetAnn, NLiteralPathAnn, NEnvPathAnn, NUnaryAnn, NBinaryAnn, NSelectAnn, NHasAttrAnn, NAbsAnn, NLetAnn, NIfAnn, NWithAnn, NAssertAnn, NSynHoleAnn #-}
