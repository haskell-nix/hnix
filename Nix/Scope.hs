module Nix.Scope where

import           Control.Applicative
import qualified Data.Map.Lazy as Map
import           Data.Text (Text)
import           Nix.Utils

data Scope a = Scope
    { _scopeMap  :: Map.Map Text a
    , scopeWeak :: Bool
    }
    deriving Functor

instance Show (Scope a) where
    show (Scope xs _) = show $ Map.keys xs

newScope :: Map.Map Text a -> Scope a
newScope m = Scope m False

newWeakScope :: Map.Map Text a -> Scope a
newWeakScope m = Scope m True

newtype NestedScopes a = NestedScopes { getNestedScopes :: [Scope a] }
    deriving Functor

instance Show (NestedScopes a) where
    show (NestedScopes xs) = show xs

emptyScopes :: NestedScopes a
emptyScopes = NestedScopes []

scopeLookup :: Text -> NestedScopes a -> Maybe a
scopeLookup key = para go Nothing . getNestedScopes
  where
    go (Scope m True)  ms rest =
        -- If the symbol lookup is in a weak scope, first see if there are any
        -- matching symbols from the *non-weak* scopes after this one. If so,
        -- prefer that, otherwise perform the lookup here. This way, if there
        -- are several weaks scopes in a row, followed by non-weak scopes,
        -- we'll first prefer the symbol from the non-weak scopes, and then
        -- prefer it from the first weak scope that matched.
        scopeLookup key (NestedScopes (filter (not . scopeWeak) ms))
            <|> Map.lookup key m <|> rest
    go (Scope m False) _ rest = Map.lookup key m <|> rest

combineScopes :: NestedScopes a -> NestedScopes a -> NestedScopes a
combineScopes (NestedScopes xs) (NestedScopes ys) = NestedScopes (xs ++ ys)

extendScope :: Map.Map Text a -> NestedScopes a -> NestedScopes a
extendScope x (NestedScopes xs) = NestedScopes (newScope x:xs)
