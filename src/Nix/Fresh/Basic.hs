{-# language CPP #-}
{-# language ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module Nix.Fresh.Basic where

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail ( MonadFail )
#endif
import           Nix.Effects
import           Nix.Render
import           Nix.Fresh
import           Nix.Value

type StdIdT = FreshIdT Int

-- NOTE: These would be removed by: https://github.com/haskell-nix/hnix/pull/804
instance (MonadFail m, MonadFile m) => MonadFile (StdIdT m)
instance MonadIntrospect m => MonadIntrospect (StdIdT m)
instance MonadStore m => MonadStore (StdIdT m)
instance MonadPutStr m => MonadPutStr (StdIdT m)
instance MonadHttp m => MonadHttp (StdIdT m)
instance MonadEnv m => MonadEnv (StdIdT m)
instance MonadPaths m => MonadPaths (StdIdT m)
instance MonadInstantiate m => MonadInstantiate (StdIdT m)
instance MonadExec m => MonadExec (StdIdT m)

instance (MonadEffects t f m, MonadDataContext f m)
  => MonadEffects t f (StdIdT m) where
  makeAbsolutePath = lift . makeAbsolutePath @t @f @m
  findEnvPath      = lift . findEnvPath @t @f @m
  findPath vs path = do
    i <- FreshIdT ask
    let vs' = unliftNValue (runFreshIdT i) <$> vs
    lift $ findPath @t @f @m vs' path
  importPath path = do
    i <- FreshIdT ask
    p <- lift $ importPath @t @f @m path
    pure $ liftNValue (runFreshIdT i) p
  pathToDefaultNix = lift . pathToDefaultNix @t @f @m
  derivationStrict v = do
    i <- FreshIdT ask
    p <- lift $ derivationStrict @t @f @m $ unliftNValue (runFreshIdT i) v
    pure $ liftNValue (runFreshIdT i) p
  traceEffect = lift . traceEffect @t @f @m
