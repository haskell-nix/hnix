{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Nix.Fresh.Stable (FreshStableIdT, runFreshStableIdT, freshId) where

import Nix.Effects
import Nix.Value
import Nix.Render
import Nix.Thunk
import Nix.Thunk.StableId
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Ref
import Control.Monad.Catch
import Control.Applicative
#ifdef MIN_VERSION_haskeline
import System.Console.Haskeline.MonadException (MonadException)
#endif

newtype FreshStableIdT m a = FreshStableIdT (ReaderT StableId (StateT Int m) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadRef
    , MonadAtomicRef
    , MonadCatch
    , MonadThrow
    , MonadIO
    , MonadFix
    , MonadPlus
    , Alternative
#ifdef MIN_VERSION_haskeline
    , MonadException
#endif
    )

instance MonadState s m => MonadState s (FreshStableIdT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadTrans FreshStableIdT where
  lift = FreshStableIdT . lift . lift

instance MonadTransWrap (StateT s) where
  liftWrap f a = do
    old <- get
    (result, new) <- lift $ f $ runStateT a old
    put new
    pure result

instance MonadTransWrap FreshStableIdT where
  liftWrap f (FreshStableIdT a) = FreshStableIdT $ liftWrap (liftWrap f) a

runFreshStableIdT :: Monad m => StableId -> FreshStableIdT m a -> m a
runFreshStableIdT root (FreshStableIdT a) = evalStateT (runReaderT a root) 0

freshId :: Monad m => FreshStableIdT m StableId
freshId = FreshStableIdT $ do
  root <- ask
  n <- get
  put $ succ n
  return $ cons n root

withRootId root a =
  lift $ runFreshStableIdT root a

instance MonadFile m => MonadFile (FreshStableIdT m)
instance MonadIntrospect m => MonadIntrospect (FreshStableIdT m)
instance MonadStore m => MonadStore (FreshStableIdT m) where
  addPath' = lift . addPath'
  toFile_' = (lift .) . toFile_'
instance MonadPutStr m => MonadPutStr (FreshStableIdT m)
instance MonadHttp m => MonadHttp (FreshStableIdT m)
instance MonadEnv m => MonadEnv (FreshStableIdT m)
instance MonadInstantiate m => MonadInstantiate (FreshStableIdT m)
instance MonadExec m => MonadExec (FreshStableIdT m)

{-
instance (MonadEffects t f m, MonadDataContext f m)
  => MonadEffects t f (FreshStableIdT m) where
  makeAbsolutePath = lift . makeAbsolutePath @t @f @m
  findEnvPath      = lift . findEnvPath @t @f @m
  findPath vs path = do
    root <- freshId
    let vs' = map (unliftNValue (runFreshStableIdT root)) vs
    lift $ findPath @t @f @m vs' path
  importPath path = do
    root <- freshId
    p <- lift $ importPath @t @f @m path
    return $ liftNValue (runFreshStableIdT root) p
  pathToDefaultNix = lift . pathToDefaultNix @t @f @m
  derivationStrict v = do
    root <- freshId
    p <- lift $ derivationStrict @t @f @m (unliftNValue (runFreshStableIdT root) v)
    return $ liftNValue (runFreshStableIdT root) p
  traceEffect = lift . traceEffect @t @f @m
-}
