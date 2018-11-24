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
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Lens.Family2
import           Nix.Utils

newtype Scope a = Scope { getScope :: AttrSet a }
    deriving (Functor, Foldable, Traversable)

instance Show (Scope a) where
    show (Scope m) = show (M.keys m)

newScope :: AttrSet a -> Scope a
newScope = Scope

scopeLookup :: Text -> [Scope v] -> Maybe v
scopeLookup key = foldr go Nothing
  where
    go (Scope m) rest = M.lookup key m <|> rest

data Scopes m v = Scopes
    { lexicalScopes :: [Scope v]
    , dynamicScopes :: [m (Scope v)]
    }

instance Show (Scopes m v) where
    show (Scopes m v) =
        "Scopes: " ++ show m ++ ", and "
            ++ show (length v) ++ " with-scopes"

instance Semigroup (Scopes m v) where
    Scopes ls lw <> Scopes rs rw = Scopes (ls <> rs) (lw <> rw)

instance Monoid (Scopes m v) where
    mempty  = emptyScopes
    mappend = (<>)

emptyScopes :: forall m v. Scopes m v
emptyScopes = Scopes [] []

class Scoped t m | m -> t where
  currentScopes :: m (Scopes m t)
  clearScopes :: m a -> m a
  pushScopes :: Scopes m t -> m a -> m a
  lookupVar :: Text -> m (Maybe t)

currentScopesReader :: forall m t e. (MonadReader e m, Has e (Scopes m t)) => m (Scopes m t)
currentScopesReader = asks (view hasLens)

clearScopesReader :: forall m t e a. (MonadReader e m, Has e (Scopes m t)) => m a -> m a
clearScopesReader = local (set hasLens (emptyScopes @m @t))

pushScope :: Scoped t m => AttrSet t -> m a -> m a
pushScope s = pushScopes (Scopes [Scope s] [])

pushWeakScope :: (Functor m, Scoped t m) => m (AttrSet t) -> m a -> m a
pushWeakScope s = pushScopes (Scopes [] [Scope <$> s])

pushScopesReader :: (MonadReader e m, Has e (Scopes m t)) => Scopes m t -> m a -> m a
pushScopesReader s = local (over hasLens (s <>))

lookupVarReader :: forall m t e. (MonadReader e m, Has e (Scopes m t)) => Text -> m (Maybe t)
lookupVarReader k = do
    mres <- asks (scopeLookup k . lexicalScopes @m . view hasLens)
    case mres of
        Just sym -> return $ Just sym
        Nothing -> do
            ws <- asks (dynamicScopes . view hasLens)
            foldr (\x rest -> do
                          mres' <- M.lookup k . getScope <$> x
                          case mres' of
                              Just sym -> return $ Just sym
                              Nothing  -> rest)
                  (return Nothing) ws

withScopes :: Scoped t m => Scopes m t -> m a -> m a
withScopes scope = clearScopes . pushScopes scope
