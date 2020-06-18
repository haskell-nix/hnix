{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Fresh.Basic where

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail ( MonadFail )
#endif
import           Control.Monad.Reader
import           Nix.Effects
import           Nix.Render
import           Nix.Fresh
import           Nix.Value

type StdIdT = FreshIdT Int

instance (MonadFail m, MonadFile m) => MonadFile (StdIdT m)
instance MonadIntrospect m => MonadIntrospect (StdIdT m)
instance MonadStore m => MonadStore (StdIdT m) where
  addPath' = lift . addPath'
  toFile_' = (lift .) . toFile_'
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
    let vs' = map (unliftNValue (runFreshIdT i)) vs
    lift $ findPath @t @f @m vs' path
  importPath path = do
    i <- FreshIdT ask
    p <- lift $ importPath @t @f @m path
    return $ liftNValue (runFreshIdT i) p
  pathToDefaultNix = lift . pathToDefaultNix @t @f @m
  derivationStrict v = do
    i <- FreshIdT ask
    p <- lift $ derivationStrict @t @f @m (unliftNValue (runFreshIdT i) v)
    return $ liftNValue (runFreshIdT i) p
  traceEffect = lift . traceEffect @t @f @m
