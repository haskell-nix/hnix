{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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

data Scope a = Scope
    { _scopeMap  :: HashMap Text a
    , scopeWeak :: Bool
    }
    deriving Functor

instance Show (Scope a) where
    show (Scope xs _) = show $ M.keys xs

newScope :: HashMap Text a -> Scope a
newScope m = Scope m False

newWeakScope :: HashMap Text a -> Scope a
newWeakScope m = Scope m True

scopeLookup :: Text -> [Scope v] -> Maybe v
scopeLookup key = para go Nothing
  where
    go (Scope m False) _  rest = M.lookup key m <|> rest
    go (Scope m True)  ms rest =
        -- If the symbol lookup is in a weak scope, first see if there are any
        -- matching symbols from the *non-weak* scopes after this one. If so,
        -- prefer that, otherwise perform the lookup here. This way, if there
        -- are several weaks scopes in a row, followed by non-weak scopes,
        -- we'll first prefer the symbol from the non-weak scopes, and then
        -- prefer it from the first weak scope that matched.
        scopeLookup key (filter (not . scopeWeak) ms)
            <|> M.lookup key m <|> rest

type Scopes v = [Scope v]

type Scoped e v m = (MonadReader e m, Has e (Scopes v))

emptyScopes :: Scopes v
emptyScopes = []

currentScopes :: Scoped e v m => m (Scopes v)
currentScopes = asks (view hasLens)

clearScopes :: forall v m e r. Scoped e v m => m r -> m r
clearScopes = local (set hasLens ([] :: [Scope v]))

pushScope :: forall v m e r. Scoped e v m => HashMap Text v -> m r -> m r
pushScope s = local (over hasLens (Scope s False :))

pushWeakScope :: Scoped e v m => HashMap Text v -> m r -> m r
pushWeakScope s = local (over hasLens (Scope s True :))

pushScopes :: Scoped e v m => Scopes v -> m r -> m r
pushScopes s = local (over hasLens (s ++))

lookupVar :: Scoped e v m => Text -> m (Maybe v)
lookupVar k = asks (scopeLookup k . view hasLens)

withScopes :: forall v m e a. Scoped e v m => Scopes v -> m a -> m a
withScopes scope = clearScopes @v . pushScopes scope
