{-# language UndecidableInstances #-}
{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Nix.Scope where

import           Nix.Prelude
import qualified Data.HashMap.Strict           as HM
import qualified Text.Show
import           Lens.Family2
import           Nix.Expr.Types
import qualified GHC.Clock                     as Clock

-- | Performance note on scope representation (2025-01):
--
-- We evaluated flattening lexicalScopes from @[Scope a]@ (list of HashMaps)
-- into a single merged @AttrSet a@ to achieve O(1) lookups instead of O(d)
-- where d = scope depth.
--
-- Results: This made performance WORSE (~9s slower on nixpkgs evaluation).
--
-- Why flattening failed:
--   - HashMap union on every pushScope is O(min(n,m))
--   - Scopes grew to 25k+ bindings, making union expensive
--   - The cost of merging on every push exceeded savings from O(1) lookups
--
-- Why the current design is better:
--   - Push is O(1) - just prepend to list
--   - Lookup averages 2.5 scope searches (found quickly in inner scopes)
--   - Small HashMaps are cache-friendly
--   - VarName uses interned Symbols, so individual lookups are already O(1)
--
-- Alternative approaches NOT YET TRIED:
--   - Cache hot variables (most lookups hit same few vars)
--   - Vector of scopes instead of list (better cache locality)
--   - Lazy flattening (only flatten when depth exceeds threshold)
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
  show (Scope m) = show $ HM.keys m

scopeLookup :: VarName -> [Scope a] -> Maybe a
scopeLookup key = foldr fun Nothing
 where
  fun
    :: Scope a
    -> Maybe a
    -> Maybe a
  fun (Scope m) rest = HM.lookup key m <|> rest

-- | Like scopeLookup but also returns (total depth, scopes searched before finding)
--   If not found, scopes searched = total depth
--   Computes depth during single traversal to avoid extra pass.
scopeLookupWithDepth :: VarName -> [Scope a] -> (Maybe a, Int, Int)
scopeLookupWithDepth key = go 0
 where
  go !depth [] = (Nothing, depth, depth)
  go !depth (Scope m : rest) =
    case HM.lookup key m of
      Just v  -> (Just v, depth + 1 + length rest, depth + 1)
      Nothing -> go (depth + 1) rest

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
  -- | Set scopes directly. More efficient than clearScopes . pushScopes
  -- Default implementation uses clearScopes + pushScopes for compatibility.
  setScopes     :: Scopes m a -> m r -> m r
  setScopes scopes = clearScopes . pushScopes scopes
  lookupVar     :: VarName -> m (Maybe a)

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

-- | Set scopes directly (single operation, more efficient than clear+push)
setScopesReader
  :: forall m a e r
  . ( MonadReader e m
    , Has e (Scopes m a)
    )
  => Scopes m a
  -> m r
  -> m r
setScopesReader scopes = local $ set hasLens scopes

pushScope
  :: Scoped a m
  => Scope a
  -> m r
  -> m r
pushScope ~scope = pushScopes $ Scopes (one scope) mempty  -- ~scope: lazy for loebM knot-tying

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
  => VarName
  -> m (Maybe a)
lookupVarReader k =
  do
    mres <- asks $ scopeLookup k . lexicalScopes @m . view hasLens

    case mres of
      Just res -> pure $ pure res
      Nothing -> do
        ws <- asks $ dynamicScopes . view hasLens

        foldr
          (\ weakscope rest ->
            do
              mres' <- HM.lookup k . coerce @(Scope a) <$> weakscope
              case mres' of
                Just res -> pure $ pure res
                Nothing -> rest
          )
          (pure Nothing)
          ws

withScopes
  :: Scoped a m
  => Scopes m a
  -> m r
  -> m r
withScopes = setScopes

-- | Scope lookup result for profiling
data ScopeLookupInfo
  = LexicalHit !Int !Int   -- ^ Found in lexical scope: (depth, scopes searched)
  | DynamicHit !Int !Int   -- ^ Found in dynamic scope: (depth, scopes searched)
  | LookupMiss !Int        -- ^ Not found: depth
  deriving (Show, Eq)

-- | Instrumented version of lookupVarReader that returns lookup info for profiling
lookupVarReaderWithInfo
  :: forall m a e
  . ( MonadReader e m
    , Has e (Scopes m a)
    , MonadIO m
    )
  => VarName
  -> m (Maybe a, ScopeLookupInfo, Word64)  -- ^ (result, info, elapsed nanoseconds)
lookupVarReaderWithInfo k = do
  start <- liftIO Clock.getMonotonicTimeNSec
  lexScopes <- asks $ lexicalScopes @m . view hasLens
  let (mres, lexDepth, searched) = scopeLookupWithDepth k lexScopes

  result <- case mres of
    Just v -> pure (Just v, LexicalHit lexDepth searched)
    Nothing -> do
      ws <- asks $ dynamicScopes . view hasLens
      let totalDepth = lexDepth + length ws
      searchDynamic lexDepth 0 totalDepth ws
  end <- liftIO Clock.getMonotonicTimeNSec
  let (val, info) = result
  pure (val, info, end - start)
 where
  searchDynamic _lexCount _dynSearched totalDepth [] =
    pure (Nothing, LookupMiss totalDepth)
  searchDynamic lexCount dynSearched totalDepth (weakscope : rest) = do
    mres' <- HM.lookup k . coerce @(Scope a) <$> weakscope
    case mres' of
      Just v  -> pure (Just v, DynamicHit totalDepth (lexCount + dynSearched + 1))
      Nothing -> searchDynamic lexCount (dynSearched + 1) totalDepth rest
