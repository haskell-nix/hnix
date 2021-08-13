{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Nix.Scope where

import qualified Data.HashMap.Lazy             as M
import qualified Text.Show
import           Lens.Family2
import           Nix.Expr.Types

--  2021-07-19: NOTE: Scopes can gain from sequentiality, HashMap (aka AttrSet) may not be proper to it.
newtype Scope a = Scope (AttrSet a)
  deriving
    ( Eq, Ord, Generic
    , Typeable, NFData
    , Read, Hashable
    , Semigroup, Monoid
    , Functor, Foldable, Traversable
    )

instance Show (Scope a) where
  show (Scope m) = show $ M.keys m

newScope :: AttrSet a -> Scope a
newScope = coerce

scopeLookup :: VarName -> [Scope a] -> Maybe a
scopeLookup key = foldr go Nothing
 where
  go
    :: Scope a
    -> Maybe a
    -> Maybe a
  go (Scope m) rest = M.lookup key m <|> rest

data Scopes m a =
  Scopes
    { lexicalScopes :: [Scope a]
    , dynamicScopes :: [m (Scope a)]
    }

instance Show (Scopes m a) where
  show (Scopes m a) =
    "Scopes: " <> show m <> ", and " <> show (length a) <> " with-scopes"

instance Semigroup (Scopes m a) where
  Scopes ls lw <> Scopes rs rw = Scopes (ls <> rs) (lw <> rw)

instance Monoid (Scopes m a) where
  mempty = emptyScopes

emptyScopes :: Scopes m a
emptyScopes = Scopes mempty mempty

class Scoped a m | m -> a where
  currentScopes :: m (Scopes m a)
  clearScopes   :: m r -> m r
  pushScopes    :: Scopes m a -> m r -> m r
  lookupVar     :: VarName -> m (Maybe a)

currentScopesReader
  :: forall m a e
  . ( MonadReader e m
    , Has e (Scopes m a)
    )
  => m (Scopes m a)
currentScopesReader = asks $ view hasLens

clearScopesReader
  :: forall m a e r
  . ( MonadReader e m
    , Has e (Scopes m a)
    )
  => m r
  -> m r
clearScopesReader = local $ set hasLens $ emptyScopes @m @a

pushScope
  :: Scoped a m
  => Scope a
  -> m r
  -> m r
pushScope scope = pushScopes $ Scopes [scope] mempty

pushWeakScope
  :: ( Functor m
     , Scoped a m
     )
  => m (Scope a)
  -> m r
  -> m r
pushWeakScope scope = pushScopes $ Scopes mempty [scope]

pushScopesReader
  :: ( MonadReader e m
     , Has e (Scopes m a)
     )
  => Scopes m a
  -> m r
  -> m r
pushScopesReader s = local $ over hasLens (s <>)

lookupVarReader
  :: forall m a e
  . ( MonadReader e m
    , Has e (Scopes m a)
    )
  => VarName
  -> m (Maybe a)
lookupVarReader k =
  do
    mres <- asks $ scopeLookup k . lexicalScopes @m . view hasLens

    maybe
      (do
        ws <- asks $ dynamicScopes . view hasLens

        foldr
          (\ weakscope rest ->
            do
              mres' <- M.lookup k . coerce @(Scope a) <$> weakscope

              maybe
                rest
                (pure . pure)
                mres'
          )
          (pure Nothing)
          ws
      )
      (pure . pure)
      mres

withScopes
  :: Scoped a m
  => Scopes m a
  -> m r
  -> m r
withScopes scopes = clearScopes . pushScopes scopes
