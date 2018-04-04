{-# LANGUAGE TypeFamilies #-}

module Nix.Thunk where

import           Control.Exception.Base (evaluate)
import           System.IO.Unsafe (unsafeInterleaveIO)

-- | Rather than encoding laziness ourselves, using a datatype containing
--   deferred actions or computed values in an IORef, we can leverage
--   Haskell's own laziness by deferring actions until forced. For monads that
--   are already non-strict in their binds, this type class is just the
--   identity; but for monads like IO, we need support from the runtime such
--   as 'unsafeInterleaveIO'. So what we're doing is making use of an already
--   existing implementation of thunks, rather than duplicating it here.
--
--   See issue #75
--
--   Unlike 'unsafeInterleaveIO', we have a "doubly-monadic" return type so that
--   no effects happen during the forcing of pure-seeming values. They instead
--   happen with the binding of the inner monad action.
--
--   Many thanks to Theo Giannakopolous (@tgiannak) for clarifying this
--   situation, and suggesting the use of MonadInterleave.

class MonadInterleave m where
    unsafeInterleave :: m a -> m (m a)

data Thunk m v = Pure v | Effect (m v)

valueRef :: Applicative m => v -> Thunk m v
valueRef = Pure

buildThunk :: (Functor m, MonadInterleave m) => m v -> m (Thunk m v)
buildThunk = fmap Effect . unsafeInterleave

forceThunk :: Applicative m => Thunk m v -> m v
forceThunk (Pure v) = pure v
forceThunk (Effect m) = m

instance MonadInterleave IO where
    unsafeInterleave = evaluate <$> unsafeInterleaveIO
