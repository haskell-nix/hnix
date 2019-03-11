{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Thunk where

import Control.Exception hiding (catch)
import Control.Monad.Ref
import Data.Typeable
import Data.GADT.Compare

import Nix.Fresh

class (Monad m, MonadAtomicRef m, GEq (Ref m), MonadFreshId Int m)
  => MonadThunk v t m | m -> t, t -> m, t -> v where
    thunk :: m v -> m t
    force :: t -> (v -> m r) -> m r
    forceEff :: t -> (v -> m r) -> m r
    wrapValue :: v -> t
    getValue :: t -> Maybe v

newtype ThunkLoop = ThunkLoop (Maybe Int)
    deriving (Show, Typeable)

instance Exception ThunkLoop
