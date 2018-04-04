{-# LANGUAGE TypeFamilies #-}

module Nix.Thunk where

-- | Rather than encoding laziness ourselves, using a datatype containing
--   deferred actions or computed values in an IORef, we can leverage
--   Haskell's own laziness by deferring actions until forced. For monads that
--   are already non-strict in their binds, this type class is just the
--   identity; but for monads like IO, we need support from the runtime such
--   as 'unsafeInterleaveIO'. So what we're doing is making use of an already
--   existing implementation of thunks, rather than duplicating it here.
--
--   See issue #75

class MonadInterleave m where
    unsafeInterleave :: m a -> m a

type Thunk (m :: * -> *) v = v

valueRef :: Applicative m => v -> m (Thunk m v)
valueRef = pure

buildThunk :: MonadInterleave m => m v -> m (Thunk m v)
buildThunk = unsafeInterleave

forceThunk :: Applicative m => Thunk m v -> m v
forceThunk x = pure $! x
