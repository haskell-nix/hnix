{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module Nix.Var
  ( Var
  , MonadVar
  , eqVar
  , newVar
  , readVar
  , writeVar
  , atomicModifyVar
  )
where

import           Control.Monad.Ref
import           Data.GADT.Compare  ( GEq(..) )
import           Data.STRef         ( STRef )
import           Type.Reflection    ( (:~:)(Refl) )

import           Unsafe.Coerce      ( unsafeCoerce )

type Var m = Ref m

type MonadVar m = MonadAtomicRef m

eqVar :: GEq (Ref m) => Ref m a -> Ref m a -> Bool
eqVar a b = isJust $ geq a b

newVar :: MonadRef m => a -> m (Ref m a)
newVar = newRef

readVar :: MonadRef m => Ref m a -> m a
readVar = readRef

writeVar :: MonadRef m => Ref m a -> a -> m ()
writeVar = writeRef

atomicModifyVar :: MonadAtomicRef m => Ref m a -> (a -> (a, b)) -> m b
atomicModifyVar = atomicModifyRef

--TODO: Upstream GEq instances
-- Upstream thread: https://github.com/haskellari/some/pull/34
instance GEq IORef where
  geq = gEqual

instance GEq (STRef s) where
  geq = gEqual

-- | Simply a helper function
gEqual :: Eq a => a -> b -> Maybe c
gEqual a b =
  bool
    Nothing
    (pure $ unsafeCoerce Refl)
    (a == unsafeCoerce b)
