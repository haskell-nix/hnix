{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Nix.Var where

import           Control.Monad.Ref
import           Data.GADT.Compare
import           Data.IORef
import           Data.Maybe
import           Data.STRef
import           Type.Reflection ((:~:)(Refl))

import           Unsafe.Coerce
import           Data.Bool          ( bool )

type Var m = Ref m

type MonadVar m = MonadAtomicRef m

eqVar :: forall m a . GEq (Ref m) => Ref m a -> Ref m a -> Bool
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
instance GEq IORef where
  a `geq` b =
    bool
      Nothing
      (pure $ unsafeCoerce Refl)
      (a == unsafeCoerce b )

instance GEq (STRef s) where
  a `geq` b =
    bool
      Nothing
      (pure $ unsafeCoerce Refl )
      (a == unsafeCoerce b)
