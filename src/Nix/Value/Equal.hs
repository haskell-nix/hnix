{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}

{-# options_ghc -Wno-missing-pattern-synonym-signatures #-}

module Nix.Value.Equal where

import           Nix.Prelude             hiding ( Comparison )
import           Control.Comonad                ( Comonad(extract))
import           Control.Monad.Free             ( Free(Pure,Free) )
import           Control.Monad.Trans.Except     ( throwE )
import           Data.Semialign                 ( Align
                                                , Semialign(align)
                                                )
import qualified Data.HashMap.Lazy             as HashMap.Lazy
import           Data.These                     ( These(These) )
import           Nix.Atoms
import           Nix.Frames
import           Nix.String
import           Nix.Thunk
import           Nix.Value
import           Nix.Expr.Types                 ( AttrSet )

checkComparable
  :: ( Framed e m
     , MonadDataErrorContext t f m
     )
  => NValue t f m
  -> NValue t f m
  -> m ()
checkComparable x y =
  case (x, y) of
    (NVConstant (NInt   _), NVConstant (NInt   _)) -> stub
    (NVConstant (NInt   _), NVConstant (NFloat _)) -> stub
    (NVConstant (NFloat _), NVConstant (NInt   _)) -> stub
    (NVConstant (NFloat _), NVConstant (NFloat _)) -> stub
    (NVStr       _        , NVStr       _        ) -> stub
    (NVPath      _        , NVPath      _        ) -> stub
    _                                              -> throwError $ Comparison x y

-- | Checks whether two containers are equal, using the given item equality
--   predicate. If there are any item slots that don't match between the two
--   containers, the result will be @False@.
alignEqM
  :: (Align f, Traversable f, Monad m)
  => (a -> b -> m Bool)
  -> f a
  -> f b
  -> m Bool
alignEqM eq fa fb =
  fmap
    (isRight @() @())
    $ runExceptT $
      traverse_
        (guard <=< lift . uncurry eq)
        =<< traverse
            (\case
              These a b -> pure (a, b)
              _         -> throwE mempty
            )
            (Data.Semialign.align fa fb)

alignEq :: (Align f, Traversable f) => (a -> b -> Bool) -> f a -> f b -> Bool
alignEq eq fa fb =
  runIdentity $ alignEqM ((Identity .) . eq) fa fb

isDerivationM
  :: Monad m
  => ( t
     -> m (Maybe NixString)
     )
  -> AttrSet t
  -> m Bool
isDerivationM f m =
  maybe
    False
    -- (2019-03-18):
    -- We should probably really make sure the context is empty here
    -- but the C++ implementation ignores it.
    ((==) "derivation" . ignoreContext)
    . join <$> traverse f (HashMap.Lazy.lookup "type" m)


isDerivation
  :: Monad m
  => ( t
     -> Maybe NixString
     )
  -> AttrSet t
  -> Bool
isDerivation f = runIdentity . isDerivationM (Identity . f)

valueFEqM
  :: Monad n
  => (  AttrSet a
     -> AttrSet a
     -> n Bool
     )
  -> (  a
     -> a
     -> n Bool
     )
  -> NValueF p m a
  -> NValueF p m a
  -> n Bool
valueFEqM attrsEq eq =
  curry $
    \case
      (NVConstantF (NFloat x), NVConstantF (NInt   y)) -> pure $             x == fromInteger y
      (NVConstantF (NInt   x), NVConstantF (NFloat y)) -> pure $ fromInteger x == y
      (NVConstantF lc        , NVConstantF rc        ) -> pure $            lc == rc
      (NVStrF      ls        , NVStrF      rs        ) -> pure $  (\ i -> i ls == i rs) ignoreContext
      (NVListF     ls        , NVListF     rs        ) ->          alignEqM eq ls rs
      (NVSetF      _      lm , NVSetF      _      rm ) ->          attrsEq lm rm
      (NVPathF     lp        , NVPathF     rp        ) ->             pure $ lp == rp
      _                                                -> pure False

valueFEq
  :: (AttrSet a -> AttrSet a -> Bool)
  -> (a -> a -> Bool)
  -> NValueF p m a
  -> NValueF p m a
  -> Bool
valueFEq attrsEq eq x y =
  runIdentity $
    valueFEqM
      ((Identity .) . attrsEq)
      ((Identity .) . eq)
      x
      y

compareAttrSetsM
  :: Monad m
  => (t -> m (Maybe NixString))
  -> (t -> t -> m Bool)
  -> AttrSet t
  -> AttrSet t
  -> m Bool
compareAttrSetsM f eq lm rm =
  do
    b <- on (liftA2 (&&)) (isDerivationM f) lm rm
    bool
      compareAttrs
      (maybe
        compareAttrs
        (uncurry eq)
        outPaths
      )
      b
 where
  compareAttrs = alignEqM eq lm rm

  outPaths = on (liftA2 (,)) (HashMap.Lazy.lookup "outPath") lm rm

compareAttrSets
  :: (t -> Maybe NixString)
  -> (t -> t -> Bool)
  -> AttrSet t
  -> AttrSet t
  -> Bool
compareAttrSets f eq lm rm =
  runIdentity $ compareAttrSetsM (Identity . f) ((Identity .) . eq) lm rm

valueEqM
  :: forall t f m
   . (MonadThunk t m (NValue t f m), NVConstraint f)
  => NValue t f m
  -> NValue t f m
  -> m Bool
valueEqM (  Pure x) (  Pure y) = thunkEqM x y
valueEqM (  Pure x) y@(Free _) = thunkEqM x =<< thunk (pure y)
valueEqM x@(Free _) (  Pure y) = (`thunkEqM` y) =<< thunk (pure x)
valueEqM (Free (NValue' (extract -> x))) (Free (NValue' (extract -> y))) =
  valueFEqM
    (compareAttrSetsM findNVStr valueEqM)
    valueEqM
    x
    y
 where
  findNVStr :: NValue t f m -> m (Maybe NixString)
  findNVStr =
    free
      (pure .
        (\case
          NVStr s -> pure s
          _       -> mempty
        ) <=< force
      )
      (pure .
        \case
          NVStr' s -> pure s
          _        -> mempty
      )

-- This function has mutual recursion with `valueEqM`, and this function so far is not used across the project,
-- but that one is.
thunkEqM :: (MonadThunk t m (NValue t f m), NVConstraint f) => t -> t -> m Bool
thunkEqM lt rt =
  do
    lv <- force lt
    rv <- force rt

    let
      unsafePtrEq =
        bool
          (valueEqM lv rv)
          (pure True)
          $ on (==) thunkId lt rt

    case (lv, rv) of
      (NVClosure _ _, NVClosure _ _) -> unsafePtrEq
      (NVList _     , NVList _     ) -> unsafePtrEq
      (NVSet _ _    , NVSet _ _    ) -> unsafePtrEq
      _                              -> valueEqM lv rv
