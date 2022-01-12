{-# language CPP #-}

{-# options_ghc -Wno-orphans #-}


module Nix.Fresh.Basic where

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail ( MonadFail )
#endif
import           Nix.Prelude
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

  toAbsolutePath :: Path -> StdIdT m Path
  toAbsolutePath = lift . toAbsolutePath @t @f @m

  findEnvPath :: String -> StdIdT m Path
  findEnvPath      = lift . findEnvPath @t @f @m

  findPath :: [NValue t f (StdIdT m)] -> Path -> StdIdT m Path
  findPath vs path =
    do
      i <- FreshIdT ask
      lift $ findPath @t @f @m (unliftNValue (`runFreshIdT` i) <$> vs) path

  importPath :: Path -> StdIdT m (NValue t f (StdIdT m))
  importPath path =
    do
      i <- FreshIdT ask
      lift $ liftNValue (`runFreshIdT` i) <$> (importPath @t @f @m $ path)

  pathToDefaultNix :: Path -> StdIdT m Path
  pathToDefaultNix = lift . pathToDefaultNix @t @f @m

  derivationStrict :: NValue t f (StdIdT m) -> StdIdT m (NValue t f (StdIdT m))
  derivationStrict v =
    do
      i <- FreshIdT ask
      let
        fresh :: FreshIdT Int m a -> m a
        fresh = (`runFreshIdT` i)
      lift $ liftNValue fresh <$> (derivationStrict @t @f @m . unliftNValue fresh $ v)

  traceEffect :: String -> StdIdT m ()
  traceEffect = lift . traceEffect @t @f @m
