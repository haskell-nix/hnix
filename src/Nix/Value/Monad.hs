{-# LANGUAGE MultiParamTypeClasses #-}

module Nix.Value.Monad where

-- * @MonadValue@ - a main implementation class

class MonadValue v m where
  defer :: m v -> m v
  demand :: v -> m v
  -- | If 'v' is a thunk, 'inform' allows us to modify the action to be
  --   performed by the thunk, perhaps by enriching it with scope info, for
  --   example.
  inform :: (m v -> m v) -> v -> m v


-- * @MonadValueF@ - a Kleisli-able customization class

class MonadValueF v m where
  demandF :: (v -> m r) -> v -> m r
  informF :: (m v -> m v) -> v -> m v
