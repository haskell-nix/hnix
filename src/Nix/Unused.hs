{-# language FunctionalDependencies #-}
{-# language TemplateHaskell #-}

{-# options_ghc -Wno-missing-signatures #-}

-- | This module holds unused code.
-- So, if someone wants something - look here, use it & move to appropriate place.
module Nix.Unused
 where

import           Control.Monad.Free             ( Free(..) )
import           Data.Fix                       ( Fix(..) )
import           Lens.Family2.TH                ( makeLensesBy )

-- * From "Nix.Utils"

-- | > type AlgM f m a = f a -> m a
type AlgM f m a = f a -> m a

whenFree :: (Monoid b)
  => (f (Free f a) -> b) -> Free f a -> b
whenFree =
  free
    mempty
{-# inline whenFree #-}

whenPure :: (Monoid b)
  => (a -> b) -> Free f a -> b
whenPure f =
  free
    f
    mempty
{-# inline whenPure #-}

-- | Replace:
--  @Pure a -> a@
--  @Free -> Fix@
freeToFix :: Functor f => (a -> Fix f) -> Free f a -> Fix f
freeToFix f = go
 where
  go =
    free
      f
      $ Fix . (go <$>)

-- | Replace:
--  @a -> Pure a@
--  @Fix -> Free@
fixToFree :: Functor f => Fix f -> Free f a
fixToFree = Free . go
 where
  go (Fix f) = Free . go <$> f


loeb :: Functor f => f (f a -> a) -> f a
loeb x = go
 where
  go = ($ go) <$> x

adiM
  :: ( Traversable t
     , Monad m
     )
  => Transform t (m a)
  -> AlgM t m a
  -> Fix t
  -> m a
adiM g f = g $ f <=< traverse (adiM g f) . unFix

para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para f = f . fmap (id &&& para f) . unFix

paraM :: (Traversable f, Monad m) => (f (Fix f, a) -> m a) -> Fix f -> m a
paraM f = f <=< traverse (\x -> (x, ) <$> paraM f x) . unFix

cataP :: Functor f => (Fix f -> f a -> a) -> Fix f -> a
cataP f x = f x . fmap (cataP f) . unFix $ x

cataPM :: (Traversable f, Monad m) => (Fix f -> f a -> m a) -> Fix f -> m a
cataPM f x = f x <=< traverse (cataPM f) . unFix $ x

$(makeLensesBy (\n -> pure $ "_" <> n) ''Fix)
