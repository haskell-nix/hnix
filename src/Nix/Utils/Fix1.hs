{-# language TypeFamilies #-}
{-# language ConstraintKinds #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PolyKinds #-}
{-# language UndecidableInstances #-}

module Nix.Utils.Fix1
  ( Fix1(..)
  , Fix1T(..)
  , MonadFix1T
  )
where

import           Nix.Prelude
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Ref              ( MonadAtomicRef(..)
                                                , MonadRef(..)
                                                )
import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )

-- | The fixpoint combinator.
-- Courtesy of Gregory Malecha.
-- https://gist.github.com/gmalecha/ceb3778b9fdaa4374976e325ac8feced
newtype Fix1 (t :: (k -> Type) -> k -> Type) (a :: k) = Fix1 { unFix1 :: t (Fix1 t) a }

deriving instance Generic (Fix1 t a)
deriving instance Functor (t (Fix1 t))
  => Functor (Fix1 t)
deriving instance Applicative (t (Fix1 t))
  => Applicative (Fix1 t)
deriving instance Alternative (t (Fix1 t))
  => Alternative (Fix1 t)
deriving instance Monad (t (Fix1 t))
  => Monad (Fix1 t)
deriving instance MonadPlus (t (Fix1 t))
  => MonadPlus (Fix1 t)
deriving instance MonadFix (t (Fix1 t))
  => MonadFix (Fix1 t)
deriving instance MonadIO (t (Fix1 t))
  => MonadIO (Fix1 t)
deriving instance MonadCatch (t (Fix1 t))
  => MonadCatch (Fix1 t)
deriving instance MonadThrow (t (Fix1 t))
  => MonadThrow (Fix1 t)

deriving instance MonadReader e (t (Fix1 t))
  => MonadReader e (Fix1 t)
deriving instance MonadState s (t (Fix1 t))
  => MonadState s (Fix1 t)

newtype Fix1T (t :: (k -> Type) -> (Type -> Type) -> k -> Type) (m :: Type -> Type) (a :: k)
  = Fix1T { unFix1T :: t (Fix1T t m) m a }

deriving instance Generic (Fix1T t m m)
deriving instance Functor (t (Fix1T t m) m)
  => Functor (Fix1T t m)
deriving instance Applicative (t (Fix1T t m) m)
  => Applicative (Fix1T t m)
deriving instance Alternative (t (Fix1T t m) m)
  => Alternative (Fix1T t m)
deriving instance Monad (t (Fix1T t m) m)
  => Monad (Fix1T t m)
deriving instance MonadFail (t (Fix1T t m) m)
  => MonadFail (Fix1T t m)
deriving instance MonadPlus (t (Fix1T t m) m)
  => MonadPlus (Fix1T t m)
deriving instance MonadFix (t (Fix1T t m) m)
  => MonadFix (Fix1T t m)
deriving instance MonadIO (t (Fix1T t m) m)
  => MonadIO (Fix1T t m)
deriving instance MonadCatch (t (Fix1T t m) m)
  => MonadCatch (Fix1T t m)
deriving instance MonadThrow (t (Fix1T t m) m)
  => MonadThrow (Fix1T t m)
deriving instance MonadMask (t (Fix1T t m) m)
  => MonadMask (Fix1T t m)

deriving instance MonadReader e (t (Fix1T t m) m)
  => MonadReader e (Fix1T t m)
deriving instance MonadState s (t (Fix1T t m) m)
  => MonadState s (Fix1T t m)


type MonadFix1T t m = (MonadTrans (Fix1T t), Monad (t (Fix1T t m) m))

instance
  ( MonadFix1T t m
  , MonadRef m
  )
  => MonadRef (Fix1T t m)
 where
  type Ref (Fix1T t m) = Ref m

  newRef  = lift . newRef
  {-# inline newRef #-}
  readRef = lift . readRef
  {-# inline readRef #-}
  writeRef r = lift . writeRef r
  {-# inline writeRef #-}

instance
  ( MonadFix1T t m
  , MonadAtomicRef m
  )
  => MonadAtomicRef (Fix1T t m)
 where
  atomicModifyRef r = lift . atomicModifyRef r
  {-# inline atomicModifyRef #-}

{-

newtype Flip (f :: i -> j -> *) (a :: j) (b :: i) = Flip { unFlip :: f b a }

-- | Natural Transformations
--  ( Included from
--   [compdata](https://hackage.haskell.org/package/compdata)
--  )
type (:->) f g = forall a. f a -> g a

class HFunctor f where
  hfmap :: a :-> b -> f a :-> f b

-}
