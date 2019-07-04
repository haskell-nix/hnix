{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Scope where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.HashMap.Lazy             as M
import           Data.Text                      ( Text )
import           Lens.Family2
import           Nix.Utils

class Scoped a m | m -> a where
  currentScopes :: m (Scopes m a)
  clearScopes :: m r -> m r
  pushScopes :: Scopes m a -> m r -> m r
  lookupVar :: Text -> m (Maybe a)

newtype Scope a = Scope { getScope :: AttrSet a }
    deriving (Functor, Foldable, Traversable, Eq)

instance Show (Scope a) where
  show (Scope m) = show (M.keys m)

newScope :: AttrSet a -> Scope a
newScope = Scope

scopeLookup :: Text -> [Scope a] -> Maybe a
scopeLookup key = foldr go Nothing
  where go (Scope m) rest = M.lookup key m <|> rest

data Scopes m a = Scopes
    { lexicalScopes :: [Scope a]
    , dynamicScopes :: [m (Scope a)]
    }

instance Show (Scopes m a) where
  show (Scopes m a) =
    "Scopes: " ++ show m ++ ", and " ++ show (length a) ++ " with-scopes"

instance Semigroup (Scopes m a) where
  Scopes ls lw <> Scopes rs rw = Scopes (ls <> rs) (lw <> rw)

instance Monoid (Scopes m a) where
  mempty  = emptyScopes
  mappend = (<>)

instance Functor m => Functor (Scopes m) where
  fmap f (Scopes l d) = Scopes (fmap (fmap f) l) (fmap (fmap (fmap f)) d)

emptyScopes :: forall m a . Scopes m a
emptyScopes = Scopes [] []

currentScopesReader
  :: forall m a e . (MonadReader e m, Has e (Scopes m a)) => m (Scopes m a)
currentScopesReader = asks (view hasLens)

clearScopesReader
  :: forall m a e r . (MonadReader e m, Has e (Scopes m a)) => m r -> m r
clearScopesReader = local (set hasLens (emptyScopes @m @a))

strongScope :: AttrSet a -> Scopes m a
strongScope a = Scopes [Scope a] []

pushScope :: Scoped a m => AttrSet a -> m r -> m r
pushScope s = pushScopes (Scopes [Scope s] [])

pushWeakScope :: (Functor m, Scoped a m) => m (AttrSet a) -> m r -> m r
pushWeakScope s = pushScopes (Scopes [] [Scope <$> s])

pushScopesReader
  :: (MonadReader e m, Has e (Scopes m a)) => Scopes m a -> m r -> m r
pushScopesReader s = local (over hasLens (s <>))

lookupVarReader
  :: forall m a e . (MonadReader e m, Has e (Scopes m a)) => Text -> m (Maybe a)
lookupVarReader k = do
  s <- asks $ view hasLens
  lookupVarScopes k s

lookupVarScopes
  :: forall m a . Monad m => Text -> Scopes m a -> m (Maybe a)
lookupVarScopes k s = do
  let mres = scopeLookup k $ lexicalScopes @m s
  case mres of
    Just sym -> return $ Just sym
    Nothing  -> do
      let ws = dynamicScopes s
      foldr
        (\x rest -> do
          mres' <- M.lookup k . getScope <$> x
          case mres' of
            Just sym -> return $ Just sym
            Nothing  -> rest
        )
        (return Nothing)
        ws

withScopes :: Scoped a m => Scopes m a -> m r -> m r
withScopes scope = clearScopes . pushScopes scope

hoistDynamicScopes :: (m (Scope a) -> n (Scope a)) -> Scopes m a -> Scopes n a
hoistDynamicScopes f (Scopes s d) = Scopes s $ fmap f d
