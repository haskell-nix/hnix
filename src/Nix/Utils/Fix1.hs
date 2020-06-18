{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nix.Utils.Fix1 where

import           Control.Applicative
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail
#endif
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State

-- | The fixpoint combinator, courtesy of Gregory Malecha.
--   https://gist.github.com/gmalecha/ceb3778b9fdaa4374976e325ac8feced
newtype Fix1 (t :: (k -> *) -> k -> *) (a :: k) = Fix1 { unFix1 :: t (Fix1 t) a }

deriving instance Functor (t (Fix1 t)) => Functor (Fix1 t)
deriving instance Applicative (t (Fix1 t)) => Applicative (Fix1 t)
deriving instance Alternative (t (Fix1 t)) => Alternative (Fix1 t)
deriving instance Monad (t (Fix1 t)) => Monad (Fix1 t)
deriving instance MonadPlus (t (Fix1 t)) => MonadPlus (Fix1 t)
deriving instance MonadFix (t (Fix1 t)) => MonadFix (Fix1 t)
deriving instance MonadIO (t (Fix1 t)) => MonadIO (Fix1 t)
deriving instance MonadCatch (t (Fix1 t)) => MonadCatch (Fix1 t)
deriving instance MonadThrow (t (Fix1 t)) => MonadThrow (Fix1 t)

deriving instance MonadReader e (t (Fix1 t)) => MonadReader e (Fix1 t)
deriving instance MonadState s (t (Fix1 t)) => MonadState s (Fix1 t)

newtype Fix1T (t :: (k -> *) -> (* -> *) -> k -> *) (m :: * -> *) (a :: k)
  = Fix1T { unFix1T :: t (Fix1T t m) m a }

deriving instance Functor (t (Fix1T t m) m) => Functor (Fix1T t m)
deriving instance Applicative (t (Fix1T t m) m) => Applicative (Fix1T t m)
deriving instance Alternative (t (Fix1T t m) m) => Alternative (Fix1T t m)
deriving instance Monad (t (Fix1T t m) m) => Monad (Fix1T t m)
deriving instance MonadFail (t (Fix1T t m) m) => MonadFail (Fix1T t m)
deriving instance MonadPlus (t (Fix1T t m) m) => MonadPlus (Fix1T t m)
deriving instance MonadFix (t (Fix1T t m) m) => MonadFix (Fix1T t m)
deriving instance MonadIO (t (Fix1T t m) m) => MonadIO (Fix1T t m)
deriving instance MonadCatch (t (Fix1T t m) m) => MonadCatch (Fix1T t m)
deriving instance MonadThrow (t (Fix1T t m) m) => MonadThrow (Fix1T t m)
deriving instance MonadMask (t (Fix1T t m) m) => MonadMask (Fix1T t m)

deriving instance MonadReader e (t (Fix1T t m) m) => MonadReader e (Fix1T t m)
deriving instance MonadState s (t (Fix1T t m) m) => MonadState s (Fix1T t m)

{-

newtype Flip (f :: i -> j -> *) (a :: j) (b :: i) = Flip { unFlip :: f b a }

-- | Natural Transformations (Included from
--   [compdata](https://hackage.haskell.org/package/compdata))
type (:->) f g = forall a. f a -> g a

class HFunctor f where
  hfmap :: a :-> b -> f a :-> f b

-}
