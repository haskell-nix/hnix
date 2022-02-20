{-# language UndecidableInstances #-}
{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Nix.Scope where

import           Nix.Prelude
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
    , One
    )

instance Show (Scope a) where
  show (Scope m) = show $ M.keys m

scopeLookup :: VarName -> [Scope a] -> Maybe a
scopeLookup key = foldr fun Nothing
 where
  fun
    :: Scope a
    -> Maybe a
    -> Maybe a
  fun (Scope m) rest = M.lookup key m <|> rest

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
  askScopes :: m (Scopes m a)
  clearScopes   :: m r -> m r
  pushScopes    :: Scopes m a -> m r -> m r
  lookupVar     :: VarOffset -> VarName -> m (Maybe a)

askScopesReader
  :: forall m a e
  . ( MonadReader e m
    , Has e (Scopes m a)
    )
  => m (Scopes m a)
askScopesReader = askLocal

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
pushScope scope = pushScopes $ Scopes (one scope) mempty

pushWeakScope
  :: ( Functor m
     , Scoped a m
     )
  => m (Scope a)
  -> m r
  -> m r
pushWeakScope scope = pushScopes $ Scopes mempty $ one scope

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
  => VarOffset
  -> VarName
  -> m (Maybe a)
lookupVarReader offset k = case offset of
  Static (StaticOffset lvl _) -> do
    mres <- asks $ M.lookup k . unscope <=< (!!? lvl) . lexicalScopes @m . view hasLens
    maybe (error "binding analysis error") (pure . Just) mres
  Dynamic -> dynamicLookup
  Unknown -> do
    mres <- asks $ scopeLookup k . lexicalScopes @m . view hasLens
    maybe dynamicLookup (pure . Just) mres
 where
  unscope (Scope s) = s
  dynamicLookup =
    do
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

withScopes
  :: Scoped a m
  => Scopes m a
  -> m r
  -> m r
withScopes scopes = clearScopes . pushScopes scopes
