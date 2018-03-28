module Nix.Utils where

import Control.Monad
import Data.Fix

(&) :: a -> (a -> c) -> c
(&) = flip ($)

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

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
