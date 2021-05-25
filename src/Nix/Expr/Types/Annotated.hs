{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}

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

#ifdef MIN_VERSION_serialise
import           Codec.Serialise
#endif
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

#ifdef MIN_VERSION_serialise
instance Serialise SrcSpan
#endif

-- * data type @Ann@

-- | A type constructor applied to a type along with an annotation
--
-- Intended to be used with 'Fix':
-- @type MyType = Fix (Compose (Ann Annotation) F)@
data Ann ann a = Ann
    { annotation :: ann
    , annotated  :: a
    }
    deriving (Ord, Eq, Data, Generic, Generic1, Typeable, Functor, Foldable,
              Traversable, Read, Show, NFData, Hashable)

type AnnF ann f = Compose (Ann ann) f

-- | Pattern: @Fix (Compose (Ann _ _))@.
-- Fix composes units of (annotations & the annotated) into one object.
-- Giving annotated expression.
pattern AnnE
  :: forall ann (g :: * -> *)
  . ann
  -> g (Fix (AnnF ann g))
  -> Fix (AnnF ann g)
pattern AnnE ann a = Fix (Compose (Ann ann a))
{-# complete AnnE #-}

annToAnnF :: Ann ann (f (Fix (AnnF ann f))) -> Fix (AnnF ann f)
annToAnnF (Ann ann a) = AnnE ann a

-- ** Instances

instance Hashable ann => Hashable1 (Ann ann)

instance NFData ann => NFData1 (Ann ann)

instance (Binary ann, Binary a) => Binary (Ann ann a)

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

#ifdef MIN_VERSION_serialise
instance (Serialise ann, Serialise a) => Serialise (Ann ann a)
#endif

-- ** @NExprLoc{,F}@ - annotated Nix expression

type NExprLocF = AnnF SrcSpan NExprF

#ifdef MIN_VERSION_serialise
instance Serialise r => Serialise (NExprLocF r) where
  encode (Compose (Ann ann a)) = encode ann <> encode a
  decode =
    liftA2 ((Compose .) . Ann)
      decode
      decode
#endif

instance Binary r => Binary (NExprLocF r)

-- | Annotated Nix expression (each subexpression direct to its source location).
type NExprLoc = Fix NExprLocF

#ifdef MIN_VERSION_serialise
instance Serialise NExprLoc
#endif

instance Binary NExprLoc

-- * Other

stripAnnotation :: Functor f => Fix (AnnF ann f) -> Fix f
stripAnnotation = unfoldFix (stripAnn . unFix)

stripAnn :: AnnF ann f r -> f r
stripAnn = annotated . getCompose

nUnary :: Ann SrcSpan NUnaryOp -> NExprLoc -> NExprLoc
nUnary (Ann s1 u) e1@(AnnE s2 _) = AnnE (s1 <> s2) $ NUnary u e1
{-# inline nUnary #-}

nBinary :: Ann SrcSpan NBinaryOp -> NExprLoc -> NExprLoc -> NExprLoc
nBinary (Ann s1 b) e1@(AnnE s2 _) e2@(AnnE s3 _) =
  AnnE (s1 <> s2 <> s3) $ NBinary b e1 e2

nSelectLoc
  :: NExprLoc -> Ann SrcSpan (NAttrPath NExprLoc) -> Maybe NExprLoc -> NExprLoc
nSelectLoc e1@(AnnE s1 _) (Ann s2 ats) =
  --  2021-05-16: NOTE: This could been rewritten into function application of @(s3, pure e2)@
  -- if @SrcSpan@ was Monoid, which requires @SorcePos@ to be a Monoid, and upstream code prevents it.
  -- Question upstream: https://github.com/mrkkrp/megaparsec/issues/450
  maybe
    (                    AnnE  s1s2        $ NSelect e1 ats   Nothing)
    (\ e2@(AnnE s3 _) -> AnnE (s1s2 <> s3) $ NSelect e1 ats $ pure e2)
 where
  s1s2 = s1 <> s2

nHasAttr :: NExprLoc -> Ann SrcSpan (NAttrPath NExprLoc) -> NExprLoc
nHasAttr e1@(AnnE s1 _) (Ann s2 ats) = AnnE (s1 <> s2) $ NHasAttr e1 ats

nApp :: NExprLoc -> NExprLoc -> NExprLoc
nApp e1@(AnnE s1 _) e2@(AnnE s2 _) = AnnE (s1 <> s2) $ NBinary NApp e1 e2

nAbs :: Ann SrcSpan (Params NExprLoc) -> NExprLoc -> NExprLoc
nAbs (Ann s1 ps) e1@(AnnE s2 _) = AnnE (s1 <> s2) $ NAbs ps e1

nStr :: Ann SrcSpan (NString NExprLoc) -> NExprLoc
nStr (Ann s1 s) = AnnE s1 $ NStr s

deltaInfo :: SourcePos -> (Text, Int, Int)
deltaInfo (SourcePos fp l c) = (toText fp, unPos l, unPos c)

nNull :: NExprLoc
nNull = AnnE nullSpan $ NConstant NNull
{-# inline nNull #-}

nullSpan :: SrcSpan
nullSpan = SrcSpan nullPos nullPos
{-# inline nullSpan #-}

-- | Pattern systems for matching on NExprLocF constructions.

pattern NConstant_ :: SrcSpan -> NAtom -> NExprLocF r
pattern NConstant_ ann x = Compose (Ann ann (NConstant x))

pattern NStr_ :: SrcSpan -> NString r -> NExprLocF r
pattern NStr_ ann x = Compose (Ann ann (NStr x))

pattern NSym_ :: SrcSpan -> VarName -> NExprLocF r
pattern NSym_ ann x = Compose (Ann ann (NSym x))

pattern NList_ :: SrcSpan -> [r] -> NExprLocF r
pattern NList_ ann x = Compose (Ann ann (NList x))

pattern NSet_ :: SrcSpan -> NRecordType -> [Binding r] -> NExprLocF r
pattern NSet_ ann recur x = Compose (Ann ann (NSet recur x))

pattern NLiteralPath_ :: SrcSpan -> FilePath -> NExprLocF r
pattern NLiteralPath_ ann x = Compose (Ann ann (NLiteralPath x))

pattern NEnvPath_ :: SrcSpan -> FilePath -> NExprLocF r
pattern NEnvPath_ ann x = Compose (Ann ann (NEnvPath x))

pattern NUnary_ :: SrcSpan -> NUnaryOp -> r -> NExprLocF r
pattern NUnary_ ann op x = Compose (Ann ann (NUnary op x))

pattern NBinary_ :: SrcSpan -> NBinaryOp -> r -> r -> NExprLocF r
pattern NBinary_ ann op x y = Compose (Ann ann (NBinary op x y))

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

pattern NSynHole_ :: SrcSpan -> Text -> NExprLocF r
pattern NSynHole_ ann x = Compose (Ann ann (NSynHole x))
{-# complete NConstant_, NStr_, NSym_, NList_, NSet_, NLiteralPath_, NEnvPath_, NUnary_, NBinary_, NSelect_, NHasAttr_, NAbs_, NLet_, NIf_, NWith_, NAssert_, NSynHole_ #-}
