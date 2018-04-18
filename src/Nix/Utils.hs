{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Utils (module Nix.Utils, module X) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import           Data.Fix
import           Data.Functor.Identity
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.List (sortOn)
import           Data.Monoid (Endo)
import           Data.Text (Text)
import qualified Data.Vector as V

#if ENABLE_TRACING
import           Debug.Trace as X
#else
import           Prelude as X
trace :: String -> a -> a
trace = const id
traceM :: Monad m => String -> m ()
traceM = const (return ())
#endif

type DList a = Endo [a]

type AttrSet = HashMap Text

infixr 0 &
(&) :: a -> (a -> c) -> c
(&) = flip ($)

(<&>) :: Functor f => f a -> (a -> c) -> f c
(<&>) = flip (<$>)

(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

loebM :: (MonadFix m, Traversable t) => t (t a -> m a) -> m (t a)
loebM f = mfix $ \a -> mapM ($ a) f

para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para f base = h where
    h []     = base
    h (x:xs) = f x xs (h xs)

paraM :: Monad m => (a -> [a] -> b -> m b) -> b -> [a] -> m b
paraM f base = h where
    h []     = return base
    h (x:xs) = f x xs =<< h xs

transport :: Functor g => (forall x. f x -> g x) -> Fix f -> Fix g
transport f (Fix x) = Fix $ fmap (transport f) (f x)

-- | adi is Abstracting Definitional Interpreters:
--
--     https://arxiv.org/abs/1707.04755
--
--   Essentially, it does for evaluation what recursion schemes do for
--   representation: allows threading layers through existing structure, only
--   in this case through behavior.
adi :: Traversable t
    => (t a -> a)
    -> ((Fix t -> a) -> Fix t -> a)
    -> Fix t -> a
adi f g = g (f . fmap (adi f g) . unFix)

adiM :: (Traversable t, Monad m)
     => (t a -> m a)
     -> ((Fix t -> m a) -> Fix t -> m a)
     -> Fix t -> m a
adiM f g = g ((f <=< traverse (adiM f g)) . unFix)

type MonoLens a b = forall f. Functor f => (b -> f b) -> a -> f a

view :: MonoLens a b -> a -> b
view l = getConst . l Const

set :: MonoLens a b -> b -> a -> a
set l b = runIdentity . l (\_ -> Identity b)

over :: MonoLens a b -> (b -> b) -> a -> a
over l f = runIdentity . l (Identity . f)

class Has a b where
    hasLens :: MonoLens a b

toEncodingSorted :: A.Value -> A.Encoding
toEncodingSorted = \case
    A.Object m ->
        A.pairs . mconcat
                . fmap (\(k, v) -> A.pair k $ toEncodingSorted v)
                . sortOn fst
                $ M.toList m
    A.Array l -> A.list toEncodingSorted $ V.toList l
    v -> A.toEncoding v
