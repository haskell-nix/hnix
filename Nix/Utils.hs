{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Utils (module Nix.Utils, module X) where

import Control.Monad
import Control.Monad.Fix
import Data.Fix

#define ENABLE_TRACING 1
#if ENABLE_TRACING
import Debug.Trace as X
#else
import Prelude as X
trace :: String -> a -> a
trace = const id
traceM :: Monad m => String -> m ()
traceM = const (return ())
#endif

(&) :: a -> (a -> c) -> c
(&) = flip ($)

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

loebM :: (MonadFix m, Traversable t) => t (t a -> m a) -> m (t a)
loebM f = mfix $ \a -> mapM ($ a) f

para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para f base = h where
    h []     = base
    h (x:xs) = f x xs (h xs)

-- | adi is Abstracting Definitional Interpreters:
--
--     https://arxiv.org/abs/1707.04755
--
--   Essentially, it does for evaluation what recursion schemes do for
--   representation: allows threading layers through existing structure, only
--   in this case through behavior.
adi :: (Monoid b, Applicative s, Traversable t)
    => (t a -> a)
    -> ((Fix t -> (b, s a)) -> Fix t -> (b, s a))
    -> Fix t -> (b, s a)
adi f g = g (go . traverse (adi f g) . unFix)
  where
    go = fmap (fmap f . sequenceA)

adiM :: (Monoid b, Applicative s, Traversable s, Traversable t, Monad m)
     => (t a -> m a)
     -> ((Fix t -> m (b, s a)) -> Fix t -> m (b, s a))
     -> Fix t -> m (b, s a)
adiM f g = g ((go <=< traverse (adiM f g)) . unFix)
  where
    go = traverse (traverse f . sequenceA) . sequenceA

adiT :: forall s t m a. (Traversable t, Monad m, Monad s)
     => (t a -> m a)
     -> ((Fix t -> s (m a)) -> Fix t -> s (m a))
     -> Fix t -> s (m a)
adiT f g = g (go . fmap (adiT f g) . unFix)
  where
    go :: t (s (m a)) -> s (m a)
    go = fmap ((f =<<) . sequenceA) . sequenceA
