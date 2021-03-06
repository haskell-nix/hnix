{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Utils (module Nix.Utils, module X) where

import           Control.Arrow                  ( (&&&) )
import           Control.Monad                  ( (<=<) )
import           Control.Monad.Fix              ( MonadFix(..) )
import           Control.Monad.Free             ( Free(..) )
import           Control.Monad.Trans.Control    ( MonadTransControl(..) )
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Encoding           as A
import           Data.Fix                       ( Fix(..) )
import           Data.Hashable                  ( Hashable )
import           Data.HashMap.Lazy              ( HashMap )
import qualified Data.HashMap.Lazy             as M
import           Data.List                      ( sortOn )
import           Data.Monoid                    ( Endo )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Vector                   as V
import           Lens.Family2                  as X
import           Lens.Family2.Stock             ( _1
                                                , _2
                                                )
import           Lens.Family2.TH                ( makeLensesBy )

#if ENABLE_TRACING
import           Debug.Trace as X
#else
import           Prelude                       as X
                                         hiding ( putStr
                                                , putStrLn
                                                , print
                                                )
trace :: String -> a -> a
trace = const id
traceM :: Monad m => String -> m ()
traceM = const (pure ())
#endif

$(makeLensesBy (\n -> pure ("_" <> n)) ''Fix)

type DList a = Endo [a]

type AttrSet = HashMap Text

-- | F-algebra defines how to reduce the fixed-point of a functor to a
--   value.
type Alg f a = f a -> a

type AlgM f m a = f a -> m a

-- | "Transform" here means a modification of a catamorphism.
type Transform f a = (Fix f -> a) -> Fix f -> a

{-# inline (<&>)#-}
(<&>) :: Functor f => f a -> (a -> c) -> f c
(<&>) = flip (<$>)

{-# inline (??)#-}
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

loebM :: (MonadFix m, Traversable t) => t (t a -> m a) -> m (t a)
loebM f = mfix $ \a -> traverse ($ a) f

para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para f = f . fmap (id &&& para f) . unFix

paraM :: (Traversable f, Monad m) => (f (Fix f, a) -> m a) -> Fix f -> m a
paraM f = f <=< traverse (\x -> (x, ) <$> paraM f x) . unFix

cataP :: Functor f => (Fix f -> f a -> a) -> Fix f -> a
cataP f x = f x . fmap (cataP f) . unFix $ x

cataPM :: (Traversable f, Monad m) => (Fix f -> f a -> m a) -> Fix f -> m a
cataPM f x = f x <=< traverse (cataPM f) . unFix $ x

transport :: Functor g => (forall x . f x -> g x) -> Fix f -> Fix g
transport f (Fix x) = Fix $ fmap (transport f) (f x)

lifted
  :: (MonadTransControl u, Monad (u m), Monad m)
  => ((a -> m (StT u b)) -> m (StT u b))
  -> (a -> u m b)
  -> u m b
lifted f k = restoreT . pure =<< liftWith (\run -> f (run . k))

freeToFix :: Functor f => (a -> Fix f) -> Free f a -> Fix f
freeToFix f = go
 where
  go =
    free
      f
      (Fix . fmap go)

fixToFree :: Functor f => Fix f -> Free f a
fixToFree = Free . go where go (Fix f) = fmap (Free . go) f

-- | adi is Abstracting Definitional Interpreters:
--
--     https://arxiv.org/abs/1707.04755
--
--   Essentially, it does for evaluation what recursion schemes do for
--   representation: allows threading layers through existing structure, only
--   in this case through behavior.
adi :: Functor f => (f a -> a) -> ((Fix f -> a) -> Fix f -> a) -> Fix f -> a
adi f g = g (f . fmap (adi f g) . unFix)

adiM
  :: (Traversable t, Monad m)
  => (t a -> m a)
  -> ((Fix t -> m a) -> Fix t -> m a)
  -> Fix t
  -> m a
adiM f g = g ((f <=< traverse (adiM f g)) . unFix)

class Has a b where
    hasLens :: Lens' a b

instance Has a a where
  hasLens f = f

instance Has (a, b) a where
  hasLens = _1

instance Has (a, b) b where
  hasLens = _2

toEncodingSorted :: A.Value -> A.Encoding
toEncodingSorted = \case
  A.Object m ->
    A.pairs
      . mconcat
      . fmap (\(k, v) -> A.pair k $ toEncodingSorted v)
      . sortOn fst
      $ M.toList m
  A.Array l -> A.list toEncodingSorted $ V.toList l
  v         -> A.toEncoding v

data NixPathEntryType = PathEntryPath | PathEntryURI deriving (Show, Eq)

-- | @NIX_PATH@ is colon-separated, but can also contain URLs, which have a colon
-- (i.e. @https://...@)
uriAwareSplit :: Text -> [(Text, NixPathEntryType)]
uriAwareSplit = go where
  go str = case Text.break (== ':') str of
    (e1, e2)
      | Text.null e2                              -> [(e1, PathEntryPath)]
      | Text.pack "://" `Text.isPrefixOf` e2      ->
        let ((suffix, _) : path) = go (Text.drop 3 e2) in
        (e1 <> Text.pack "://" <> suffix, PathEntryURI) : path
      | otherwise                                 -> (e1, PathEntryPath) : go (Text.drop 1 e2)

alterF
  :: (Eq k, Hashable k, Functor f)
  => (Maybe v -> f (Maybe v))
  -> k
  -> HashMap k v
  -> f (HashMap k v)
alterF f k m =
  fmap
    (maybe
      (M.delete k m)
      (\ v -> M.insert k v m)
    )
    $ f $ M.lookup k m

{-# inline bool #-}
-- | From @Data.Bool ( bool )@.
bool :: a -> a -> Bool -> a
bool f t b =
  if b
    then t
    else f

{-# inline list #-}
-- | Analog for @bool@ or @maybe@, for list-like cons structures.
list
  :: Foldable t
  => b -> (t a -> b) -> t a -> b
list e f l =
  bool
    (f l)
    e
    (null l)

{-# inline free #-}
-- | Lambda analog of @maybe@ or @either@ for Free monad.
free :: (a -> b) -> (f (Free f a) -> b) -> Free f a -> b
free fP fF fr =
  case fr of
    Pure a -> fP a
    Free fa -> fF fa

{-# inline ifTrue #-}
ifTrue :: (Monoid a)
  => a -> Bool -> a
ifTrue =
  bool
    mempty

{-# inline ifFalse #-}
ifFalse :: (Monoid a)
  => a  -> Bool  -> a
ifFalse f =
  bool
    f
    mempty

{-# inline ifJust #-}
ifJust :: (Monoid b)
  => (a -> b)  -> Maybe a  -> b
ifJust =
  maybe
    mempty

{-# inline ifNothing #-}
ifNothing  :: (Monoid b)
  => b  -> Maybe a  -> b
ifNothing f =
  maybe
    f
    mempty

{-# inline ifRight #-}
ifRight :: (Monoid c)
  => (b -> c) -> Either a b -> c
ifRight =
  either
    mempty

{-# inline ifLeft #-}
ifLeft :: (Monoid c)
  => (a -> c) -> Either a b -> c
ifLeft f =
  either
    f
    mempty

{-# inline ifFree #-}
ifFree :: (Monoid b)
  => (f (Free f a) -> b) -> Free f a -> b
ifFree =
  free
    mempty

{-# inline ifPure #-}
ifPure :: (Monoid b)
  => (a -> b) -> Free f a -> b
ifPure f =
  free
    f
    mempty
