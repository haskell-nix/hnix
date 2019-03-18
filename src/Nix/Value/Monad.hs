{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nix.Value.Monad where

class MonadValue v m where
  defer :: m v -> m v
  demand :: v -> (v -> m r) -> m r
