{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nix.Value.Monad where

class MonadValue v m where
  defer :: m v -> m v
  demand :: v -> (v -> m r) -> m r
  -- | If 'v' is a thunk, 'inform' allows us to modify the action to be
  --   peformed by the thunk, perhaps by enriching it with scpoe info, for
  --   example.
  inform :: v -> (m v -> m v) -> m v
