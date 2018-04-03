{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Scope where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Nix.Utils

data Scope m a
    = Scope (HashMap Text a)
    | WeakScope a (a -> m (HashMap Text a))
      -- ^ Weak scopes (used by 'with') are delayed until first needed.

instance Show (Scope m a) where
    show (Scope m) = show (M.keys m)
    show (WeakScope _ _) = "<weak scope>"

newScope :: HashMap Text a -> Scope m a
newScope = Scope

newWeakScope :: a -> (a -> m (HashMap Text a)) -> Scope m a
newWeakScope = WeakScope

isWeakScope :: Scope m a -> Bool
isWeakScope (WeakScope _ _) = True
isWeakScope _ = False

scopeLookup :: MonadIO m => Text -> [Scope m v] -> m (Maybe v)
scopeLookup key = paraM go Nothing
  where
    go (Scope m) _ rest = return $ M.lookup key m <|> rest
    go (WeakScope m f) ms rest = do
        -- If the symbol lookup is in a weak scope, first see if there are any
        -- matching symbols from the *non-weak* scopes after this one. If so,
        -- prefer that, otherwise perform the lookup here. This way, if there
        -- are several weaks scopes in a row, followed by non-weak scopes,
        -- we'll first prefer the symbol from the non-weak scopes, and then
        -- prefer it from the first weak scope that matched.
        mres <- scopeLookup key (filter (not . isWeakScope) ms)
        case mres of
            Nothing ->
                f m >>= \m' ->
                    return $ M.lookup key m' <|> rest
            _ -> return mres

type Scopes m v = [Scope m v]

type Scoped e v m = (MonadReader e m, Has e (Scopes m v))

emptyScopes :: Scopes m v
emptyScopes = []

currentScopes :: Scoped e v m => m (Scopes m v)
currentScopes = asks (view hasLens)

clearScopes :: forall v m e r. Scoped e v m => m r -> m r
clearScopes = local (set hasLens ([] :: [Scope m v]))

pushScope :: forall v m e r. Scoped e v m => HashMap Text v -> m r -> m r
pushScope s = local (over hasLens (Scope @m s :))

pushWeakScope :: forall v m e r. Scoped e v m
              => v -> (v -> m (HashMap Text v)) -> m r -> m r
pushWeakScope s f = local (over hasLens (WeakScope s f :))

pushScopes :: Scoped e v m => Scopes m v -> m r -> m r
pushScopes s = local (over hasLens (s ++))

lookupVar :: forall e v m. (Scoped e v m, MonadIO m) => Text -> m (Maybe v)
lookupVar k = join $ asks (scopeLookup @m k . view hasLens)

withScopes :: forall v m e a. Scoped e v m => Scopes m v -> m a -> m a
withScopes scope = clearScopes @v . pushScopes scope
