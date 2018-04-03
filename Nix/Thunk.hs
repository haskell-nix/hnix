{-# LANGUAGE TypeFamilies #-}

module Nix.Thunk where

data Deferred m v
    = DeferredAction (m v)
    -- ^ This is closure over the environment where it was created.
    | ComputedValue v

class Monad m => MonadVar m where
    type Var m :: * -> *

    newVar :: a -> m (Var m a)
    readVar :: Var m a -> m a
    writeVar :: Var m a -> a -> m ()

newtype MonadVar m => Thunk m v =
    Thunk { getThunk :: Either v (Var m (Deferred m v)) }

valueRef :: MonadVar m => v -> m (Thunk m v)
valueRef  = pure . Thunk . Left

buildThunk :: MonadVar m => m v -> m (Thunk m v)
buildThunk action =
    Thunk . Right <$> newVar (DeferredAction action)

forceThunk :: MonadVar m => Thunk m v -> m v
forceThunk (Thunk (Left ref)) = pure ref
forceThunk (Thunk (Right ref)) = do
    eres <- readVar ref
    case eres of
        ComputedValue value -> return value
        DeferredAction action -> do
            value <- action
            writeVar ref (ComputedValue value)
            return value
