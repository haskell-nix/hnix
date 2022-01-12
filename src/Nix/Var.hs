{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}

{-# options_ghc -Wno-orphans #-}
{-# options_ghc -Wno-unused-top-binds #-}

module Nix.Var ()
where

import           Nix.Prelude
import           Control.Monad.Ref
import           Data.GADT.Compare  ( GEq(..) )
import           Data.STRef         ( STRef )
import           Type.Reflection    ( (:~:)(Refl) )

import           Unsafe.Coerce      ( unsafeCoerce )

eqVar :: GEq (Ref m) => Ref m a -> Ref m a -> Bool
eqVar a b = isJust $ geq a b

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
