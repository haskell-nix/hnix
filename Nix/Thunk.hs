{-# LANGUAGE TypeFamilies #-}

module Nix.Thunk where

data Deferred m v
    = DeferredAction (m v)
    | ComputedValue v

class Monad m => MonadVar m where
    type Var m :: * -> *

    newVar :: a -> m (Var m a)
    readVar :: Var m a -> m a
    writeVar :: Var m a -> a -> m ()

type Thunk m v = Either v (Var m (Deferred m v))

valueRef :: v -> Thunk m v
valueRef  = Left

buildThunk :: MonadVar m => m v -> m (Thunk m v)
buildThunk action =
    Right <$> newVar (DeferredAction action)

forceThunk :: MonadVar m => Thunk m v -> m v
forceThunk (Left ref) = pure ref
forceThunk (Right ref) = do
    eres <- readVar ref
    case eres of
        ComputedValue value -> return value
        DeferredAction action -> do
            value <- action
            writeVar ref (ComputedValue value)
            return value
