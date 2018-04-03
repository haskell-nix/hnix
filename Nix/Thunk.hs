module Nix.Thunk where

import Control.Monad.IO.Class
import Data.IORef

data Deferred m v
    = DeferredAction (m v)
    -- ^ This is closure over the environment where it was created.
    | ComputedValue v

newtype Thunk m v = Thunk { getThunk :: Either v (IORef (Deferred m v)) }

valueRef :: MonadIO m => v -> m (Thunk m v)
valueRef  = pure . Thunk . Left

buildThunk :: MonadIO m => m v -> m (Thunk m v)
buildThunk action =
    liftIO $ Thunk . Right <$> newIORef (DeferredAction action)

forceThunk :: MonadIO m => Thunk m v -> m v
forceThunk (Thunk (Left ref)) = pure ref
forceThunk (Thunk (Right ref)) = do
    eres <- liftIO $ readIORef ref
    case eres of
        ComputedValue value -> return value
        DeferredAction action -> do
            value <- action
            liftIO $ writeIORef ref (ComputedValue value)
            return value
