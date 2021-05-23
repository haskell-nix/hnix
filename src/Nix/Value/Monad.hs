
module Nix.Value.Monad where

-- * @MonadValue@ - a main implementation class

class MonadValue v m where
  -- | Wrap value into a thunk.
  defer :: m v -> m v
  -- | Force the evaluation of the value.
  demand :: v -> m v
  -- | If 'v' is a thunk, 'inform' allows us to modify the action to be
  --   performed by the thunk, perhaps by enriching it with scope info, for
  --   example.
  inform :: v -> m v


-- * @MonadValueF@ - a Kleisli-able customization class

class MonadValueF v m where
  demandF :: (v -> m r) -> v -> m r
  informF :: (m v -> m v) -> v -> m v
