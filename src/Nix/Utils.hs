{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Nix.Utils (module Nix.Utils, module X) where

import           Control.Applicative
import           Control.Arrow ((&&&))
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

-- | An f-algebra defines how to reduced the fixed-point of a functor to a
--   value.
type Alg f a = f a -> a

type AlgM f m a = f a -> m a

-- | An "transform" here is a modification of a catamorphism.
type Transform f a = (Fix f -> a) -> Fix f -> a

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

para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para f = f . fmap (id &&& para f) . unFix

paraM :: (Traversable f, Monad m) => (f (Fix f, a) -> m a) -> Fix f -> m a
paraM f = f <=< traverse (\x -> (x,) <$> paraM f x) . unFix

cataP :: Functor f => (Fix f -> f a -> a) -> Fix f -> a
cataP f x = f x . fmap (cataP f) . unFix $ x

cataPM :: (Traversable f, Monad m) => (Fix f -> f a -> m a) -> Fix f -> m a
cataPM f x = f x <=< traverse (cataPM f) . unFix $ x

transport :: Functor g => (forall x. f x -> g x) -> Fix f -> Fix g
transport f (Fix x) = Fix $ fmap (transport f) (f x)

-- | adi is Abstracting Definitional Interpreters:
--
--     https://arxiv.org/abs/1707.04755
--
--   Essentially, it does for evaluation what recursion schemes do for
--   representation: allows threading layers through existing structure, only
--   in this case through behavior.
adi :: Functor f => (f a -> a) -> ((Fix f -> a) -> Fix f -> a) -> Fix f -> a
adi f g = g (f . fmap (adi f g) . unFix)

adiM :: (Traversable t, Monad m)
     => (t a -> m a) -> ((Fix t -> m a) -> Fix t -> m a) -> Fix t -> m a
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

instance Has a a where
    hasLens f = f

instance Has (a, b) a where
    hasLens f (x, y) = (, y) <$> f x

instance Has (a, b) b where
    hasLens f (x, y) = (x,) <$> f y

toEncodingSorted :: A.Value -> A.Encoding
toEncodingSorted = \case
    A.Object m ->
        A.pairs . mconcat
                . fmap (\(k, v) -> A.pair k $ toEncodingSorted v)
                . sortOn fst
                $ M.toList m
    A.Array l -> A.list toEncodingSorted $ V.toList l
    v -> A.toEncoding v
