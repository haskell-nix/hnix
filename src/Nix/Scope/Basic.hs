{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Nix.Scope.Basic where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch
import Nix.Thunk
import Nix.Scope
#ifdef MIN_VERSION_haskeline
import           System.Console.Haskeline.MonadException hiding(catch)
#endif

-- `binding` is the information associated with a variable name in the scope
newtype ScopeT binding m a = ScopeT { unScopeT :: ReaderT (Scopes m binding) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadPlus
    , MonadFix
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadException
    )

deriving instance MonadState s m => MonadState s (ScopeT binding m)

instance MonadReader r m => MonadReader r (ScopeT binding m) where
  ask = lift ask
  local f = liftWrap $ local f
  reader = lift . reader

runScopeT :: ScopeT binding m a -> Scopes m binding -> m a
runScopeT = runReaderT . unScopeT

instance MonadTrans (ScopeT t) where
  lift = ScopeT . lift

instance Monad m => Scoped t (ScopeT t m) where
  currentScopes = ScopeT $ hoistDynamicScopes lift <$> ask
  clearScopes = ScopeT . local (const mempty) . unScopeT
  pushScopes added =
    ScopeT .
    local (\old -> hoistDynamicScopes (`runScopeT` old) added <> old) .
    unScopeT
  lookupVar name = ScopeT $ lift . lookupVarScopes name =<< ask

instance MonadThunk m => MonadThunk (ScopeT binding m) where
  type Thunk (ScopeT binding m) = Thunk m
  type ThunkValue (ScopeT binding m) = ThunkValue m
  thunk a = ScopeT $ do
    scopes <- ask
    lift $ thunk $ runScopeT a scopes
  queryM t n k = ScopeT $ do
    scopes <- ask
    lift $ queryM t (runScopeT n scopes) ((`runScopeT` scopes) . k)
  force t k = ScopeT $ do
    scopes <- ask
    lift $ force t $ (`runScopeT` scopes) . k
  forceEff t k = ScopeT $ do
    scopes <- ask
    lift $ forceEff t $ (`runScopeT` scopes) . k
  further t k = ScopeT $ do
    scopes <- ask
    lift $ further t $ (`runScopeT` scopes) . k . lift

instance MonadTransWrap (ScopeT binding) where
  liftWrap f a = ScopeT $ liftWrap f (unScopeT a)
