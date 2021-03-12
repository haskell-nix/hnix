{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

(<&>) :: Functor f => f a -> (a -> c) -> f c
(<&>) = flip (<$>)
{-# inline (<&>)#-}

(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# inline (??)#-}

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

-- | From @Data.Bool ( bool )@.
bool :: a -> a -> Bool -> a
bool f t b =
  if b
    then t
    else f
{-# inline bool #-}

-- | Analog for @bool@ or @maybe@, for list-like cons structures.
list
  :: Foldable t
  => b -> (t a -> b) -> t a -> b
list e f l =
  bool
    (f l)
    e
    (null l)
{-# inline list #-}

-- | Lambda analog of @maybe@ or @either@ for Free monad.
free :: (a -> b) -> (f (Free f a) -> b) -> Free f a -> b
free fP fF fr =
  case fr of
    Pure a -> fP a
    Free fa -> fF fa
{-# inline free #-}


whenTrue :: (Monoid a)
  => a -> Bool -> a
whenTrue =
  bool
    mempty
{-# inline whenTrue #-}

whenFalse :: (Monoid a)
  => a  -> Bool  -> a
whenFalse f =
  bool
    f
    mempty
{-# inline whenFalse #-}

whenJust :: (Monoid b)
  => (a -> b)  -> Maybe a  -> b
whenJust =
  maybe
    mempty
{-# inline whenJust #-}

whenNothing  :: (Monoid b)
  => b  -> Maybe a  -> b
whenNothing f =
  maybe
    f
    mempty
{-# inline whenNothing #-}

whenRight :: (Monoid c)
  => (b -> c) -> Either a b -> c
whenRight =
  either
    mempty
{-# inline whenRight #-}

whenLeft :: (Monoid c)
  => (a -> c) -> Either a b -> c
whenLeft f =
  either
    f
    mempty
{-# inline whenLeft #-}

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

-- | From @base@ @Data.Foldable@
traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr c (pure ())
  -- See Note [List fusion and continuations in 'c']
  where c x k = f x *> k
        {-# inline c #-}

-- From @base@ @Data.Foldable@
for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
for_ = flip traverse_
{-# inline for_ #-}

-- | Apply a single function to both components of a pair.
--
-- > both succ (1,2) == (2,3)
--
-- Taken From package @extra@
both :: (a -> b) -> (a, a) -> (b, b)
both f (x,y) = (f x, f y)
{-# inline both #-}
