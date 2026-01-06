{-# LANGUAGE StrictData #-}

-- | Statistics collection for evaluation profiling
module Nix.EvalStats
  ( EvalStats(..)
  , ExprStats(..)
  , ThunkStats(..)
  , BuiltinStats(..)
  , ScopeStats(..)
  , ScopeLookupResult(..)
  , newEvalStats
  , recordExprExclusive
  , withExprTiming
  , recordThunkForce
  , recordThunkCreate
  , recordBuiltin
  , recordScopeLookup
  , printEvalStats
  ) where

import           Nix.Prelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import           Text.Printf (printf)
import qualified GHC.Clock as Clock

-- | Statistics for a single expression type
data ExprStats = ExprStats
  { exprCount      :: !Word64  -- ^ Number of evaluations
  , exprTimeNs     :: !Word64  -- ^ Total (inclusive) time in nanoseconds
  , exprExclTimeNs :: !Word64  -- ^ Exclusive time (excluding children) in nanoseconds
  } deriving (Show, Eq)

instance Semigroup ExprStats where
  ExprStats c1 t1 e1 <> ExprStats c2 t2 e2 = ExprStats (c1 + c2) (t1 + t2) (e1 + e2)

instance Monoid ExprStats where
  mempty = ExprStats 0 0 0

-- | Statistics for thunk operations
data ThunkStats = ThunkStats
  { thunkCreated   :: !Word64  -- ^ Number of thunks created
  , thunkForced    :: !Word64  -- ^ Number of force operations
  , thunkHit       :: !Word64  -- ^ Cache hits (already computed)
  , thunkMiss      :: !Word64  -- ^ Cache misses (needed computation)
  , thunkTimeNs    :: !Word64  -- ^ Time spent in thunk computation (misses only)
  , thunkHitTimeNs :: !Word64  -- ^ Time spent in cache hits (overhead only)
  } deriving (Show, Eq)

instance Semigroup ThunkStats where
  ThunkStats c1 f1 h1 m1 t1 ht1 <> ThunkStats c2 f2 h2 m2 t2 ht2 =
    ThunkStats (c1+c2) (f1+f2) (h1+h2) (m1+m2) (t1+t2) (ht1+ht2)

instance Monoid ThunkStats where
  mempty = ThunkStats 0 0 0 0 0 0

-- | Statistics for a single builtin function
data BuiltinStats = BuiltinStats
  { builtinCount  :: !Word64  -- ^ Number of calls
  , builtinTimeNs :: !Word64  -- ^ Total time in nanoseconds
  } deriving (Show, Eq)

instance Semigroup BuiltinStats where
  BuiltinStats c1 t1 <> BuiltinStats c2 t2 = BuiltinStats (c1 + c2) (t1 + t2)

instance Monoid BuiltinStats where
  mempty = BuiltinStats 0 0

-- | Statistics for scope lookups
data ScopeStats = ScopeStats
  { scopeLookups       :: !Word64  -- ^ Total number of lookups
  , scopeHits          :: !Word64  -- ^ Found in lexical scope
  , scopeDynamicHits   :: !Word64  -- ^ Found in dynamic (with) scope
  , scopeMisses        :: !Word64  -- ^ Not found
  , scopeTotalDepth    :: !Word64  -- ^ Sum of scope depths at lookup time
  , scopeTotalSearched :: !Word64  -- ^ Sum of scopes searched before finding
  , scopeMaxDepth      :: !Word64  -- ^ Maximum scope depth seen
  , scopeTimeNs        :: !Word64  -- ^ Total time spent in lookups
  } deriving (Show, Eq)

instance Semigroup ScopeStats where
  ScopeStats l1 h1 d1 m1 td1 ts1 md1 t1 <> ScopeStats l2 h2 d2 m2 td2 ts2 md2 t2 =
    ScopeStats (l1+l2) (h1+h2) (d1+d2) (m1+m2) (td1+td2) (ts1+ts2) (max md1 md2) (t1+t2)

instance Monoid ScopeStats where
  mempty = ScopeStats 0 0 0 0 0 0 0 0

-- | All evaluation statistics
data EvalStats = EvalStats
  { statsExprs     :: !(IORef (HashMap Text ExprStats))
  , statsThunks    :: !(IORef ThunkStats)
  , statsBuiltins  :: !(IORef (HashMap Text BuiltinStats))
  , statsScopes    :: !(IORef ScopeStats)
  , statsChildTime :: !(IORef Word64)  -- ^ Accumulated child time for exclusive timing
  }

-- | Create a new empty stats collector
newEvalStats :: MonadIO m => m EvalStats
newEvalStats = liftIO $ EvalStats
  <$> newIORef mempty
  <*> newIORef mempty
  <*> newIORef mempty
  <*> newIORef mempty
  <*> newIORef 0

-- | Record an expression evaluation with exclusive timing
recordExprExclusive :: MonadIO m => EvalStats -> Text -> Word64 -> Word64 -> m ()
recordExprExclusive stats name totalNs exclNs = liftIO $
  modifyIORef' (statsExprs stats) $
    HM.insertWith (<>) name (ExprStats 1 totalNs exclNs)
{-# INLINABLE recordExprExclusive #-}

-- | Execute an action with timing, tracking exclusive time
--   Returns the result and adds the total time to parent's child time
withExprTiming :: MonadIO m => EvalStats -> Text -> m a -> m a
withExprTiming stats name action = do
  -- Save parent's accumulated child time and reset for our children
  parentChildTime <- liftIO $ readIORef (statsChildTime stats)
  liftIO $ writeIORef (statsChildTime stats) 0

  -- Time the action
  start <- liftIO Clock.getMonotonicTimeNSec
  result <- action
  end <- liftIO Clock.getMonotonicTimeNSec

  -- Get time spent in our children
  ourChildTime <- liftIO $ readIORef (statsChildTime stats)

  let totalTime = end - start
      exclTime = totalTime - ourChildTime

  -- Record our stats
  recordExprExclusive stats name totalTime exclTime

  -- Add our total time to parent's child time accumulator
  liftIO $ writeIORef (statsChildTime stats) (parentChildTime + totalTime)

  pure result
{-# INLINABLE withExprTiming #-}

-- | Record a thunk creation
recordThunkCreate :: MonadIO m => EvalStats -> m ()
recordThunkCreate stats = liftIO $
  modifyIORef' (statsThunks stats) $ \s ->
    s { thunkCreated = thunkCreated s + 1 }
{-# INLINABLE recordThunkCreate #-}

-- | Record a thunk force operation
--   hit = True means the value was already computed
--   elapsedNs = time spent (measures overhead for hits, overhead+compute for misses)
recordThunkForce :: MonadIO m => EvalStats -> Bool -> Word64 -> m ()
recordThunkForce stats hit elapsedNs = liftIO $
  modifyIORef' (statsThunks stats) $ \s ->
    if hit
      then s { thunkForced = thunkForced s + 1
             , thunkHit = thunkHit s + 1
             , thunkHitTimeNs = thunkHitTimeNs s + elapsedNs }
      else s { thunkForced = thunkForced s + 1
             , thunkMiss = thunkMiss s + 1
             , thunkTimeNs = thunkTimeNs s + elapsedNs }
{-# INLINABLE recordThunkForce #-}

-- | Record a builtin function call with timing
recordBuiltin :: MonadIO m => EvalStats -> Text -> Word64 -> m ()
recordBuiltin stats name elapsedNs = liftIO $
  modifyIORef' (statsBuiltins stats) $
    HM.insertWith (<>) name (BuiltinStats 1 elapsedNs)
{-# INLINABLE recordBuiltin #-}

-- | Result of a scope lookup for profiling
data ScopeLookupResult
  = ScopeLexicalHit !Int !Int   -- ^ Found in lexical scope: depth, scopes searched
  | ScopeDynamicHit !Int !Int   -- ^ Found in dynamic scope: depth, scopes searched
  | ScopeMiss !Int              -- ^ Not found: depth searched

-- | Record a scope lookup
recordScopeLookup :: MonadIO m => EvalStats -> ScopeLookupResult -> Word64 -> m ()
recordScopeLookup stats result elapsedNs = liftIO $
  modifyIORef' (statsScopes stats) $ \s ->
    case result of
      ScopeLexicalHit depth searched -> s
        { scopeLookups = scopeLookups s + 1
        , scopeHits = scopeHits s + 1
        , scopeTotalDepth = scopeTotalDepth s + fromIntegral depth
        , scopeTotalSearched = scopeTotalSearched s + fromIntegral searched
        , scopeMaxDepth = max (scopeMaxDepth s) (fromIntegral depth)
        , scopeTimeNs = scopeTimeNs s + elapsedNs
        }
      ScopeDynamicHit depth searched -> s
        { scopeLookups = scopeLookups s + 1
        , scopeDynamicHits = scopeDynamicHits s + 1
        , scopeTotalDepth = scopeTotalDepth s + fromIntegral depth
        , scopeTotalSearched = scopeTotalSearched s + fromIntegral searched
        , scopeMaxDepth = max (scopeMaxDepth s) (fromIntegral depth)
        , scopeTimeNs = scopeTimeNs s + elapsedNs
        }
      ScopeMiss depth -> s
        { scopeLookups = scopeLookups s + 1
        , scopeMisses = scopeMisses s + 1
        , scopeTotalDepth = scopeTotalDepth s + fromIntegral depth
        , scopeTotalSearched = scopeTotalSearched s + fromIntegral depth
        , scopeMaxDepth = max (scopeMaxDepth s) (fromIntegral depth)
        , scopeTimeNs = scopeTimeNs s + elapsedNs
        }
{-# INLINABLE recordScopeLookup #-}

-- | Print statistics summary to stdout
printEvalStats :: MonadIO m => EvalStats -> m ()
printEvalStats stats = liftIO $ do
  putStrLn "\n===== Evaluation Statistics ====="

  -- Expression stats
  exprMap <- readIORef (statsExprs stats)
  unless (HM.null exprMap) $ do
    putStrLn "\n-- Expression Evaluation (sorted by exclusive time) --"
    putStrLn $ printf "%-16s %10s %14s %14s %10s"
      ("Type" :: String) ("Count" :: String) ("Excl (ms)" :: String) ("Incl (ms)" :: String) ("Avg Excl" :: String)
    putStrLn $ replicate 68 '-'
    -- Sort by exclusive time (descending)
    let sorted = List.sortOn (negate . exprExclTimeNs . snd) $ HM.toList exprMap
        totalCount = sum $ map (exprCount . snd) sorted
        totalExcl = sum $ map (exprExclTimeNs . snd) sorted
        totalIncl = sum $ map (exprTimeNs . snd) sorted
    forM_ sorted $ \(name, ExprStats count inclNs exclNs) -> do
      let exclMs = fromIntegral exclNs / 1e6 :: Double
          inclMs = fromIntegral inclNs / 1e6 :: Double
          avgExclUs = if count > 0 then fromIntegral exclNs / fromIntegral count / 1e3 else 0 :: Double
      putStrLn $ printf "%-16s %10d %14.1f %14.1f %10.2f" (toString name) count exclMs inclMs avgExclUs
    putStrLn $ replicate 68 '-'
    putStrLn $ printf "%-16s %10d %14.1f %14.1f"
      ("TOTAL" :: String) totalCount
      (fromIntegral totalExcl / 1e6 :: Double)
      (fromIntegral totalIncl / 1e6 :: Double)

  -- Thunk stats
  thunks <- readIORef (statsThunks stats)
  when (thunkForced thunks > 0 || thunkCreated thunks > 0) $ do
    putStrLn "\n-- Thunk Operations --"
    putStrLn $ printf "Created:     %12d" (thunkCreated thunks)
    putStrLn $ printf "Forced:      %12d" (thunkForced thunks)
    putStrLn $ printf "  Cache hit: %12d (%.1f%%)"
      (thunkHit thunks)
      (if thunkForced thunks > 0
        then 100 * fromIntegral (thunkHit thunks) / fromIntegral (thunkForced thunks) :: Double
        else 0)
    putStrLn $ printf "  Computed:  %12d (%.1f%%)"
      (thunkMiss thunks)
      (if thunkForced thunks > 0
        then 100 * fromIntegral (thunkMiss thunks) / fromIntegral (thunkForced thunks) :: Double
        else 0)
    putStrLn $ printf "Cache hit time: %9.1f ms (avg %.2f us)"
      (fromIntegral (thunkHitTimeNs thunks) / 1e6 :: Double)
      (if thunkHit thunks > 0
        then fromIntegral (thunkHitTimeNs thunks) / fromIntegral (thunkHit thunks) / 1e3 :: Double
        else 0)
    putStrLn $ printf "Compute time: %11.1f ms (avg %.2f us)"
      (fromIntegral (thunkTimeNs thunks) / 1e6 :: Double)
      (if thunkMiss thunks > 0
        then fromIntegral (thunkTimeNs thunks) / fromIntegral (thunkMiss thunks) / 1e3 :: Double
        else 0)

  -- Builtin stats
  builtinMap <- readIORef (statsBuiltins stats)
  unless (HM.null builtinMap) $ do
    putStrLn "\n-- Builtin Functions --"
    putStrLn $ printf "%-30s %12s %12s %12s" ("Name" :: String) ("Count" :: String) ("Time (ms)" :: String) ("Avg (us)" :: String)
    putStrLn $ replicate 70 '-'
    let sorted = List.sortOn (negate . builtinTimeNs . snd) $ HM.toList builtinMap
        totalCount = sum $ map (builtinCount . snd) sorted
        totalTime = sum $ map (builtinTimeNs . snd) sorted
    -- Only show top 20 builtins by time
    forM_ (take 20 sorted) $ \(name, BuiltinStats count timeNs) -> do
      let timeMs = fromIntegral timeNs / 1e6 :: Double
          avgUs = if count > 0 then fromIntegral timeNs / fromIntegral count / 1e3 else 0 :: Double
      putStrLn $ printf "%-30s %12d %12.1f %12.2f" (toString name) count timeMs avgUs
    when (length sorted > 20) $
      putStrLn $ printf "... and %d more builtins" (length sorted - 20)
    putStrLn $ replicate 70 '-'
    putStrLn $ printf "%-30s %12d %12.1f" ("TOTAL" :: String) totalCount (fromIntegral totalTime / 1e6 :: Double)

  -- Scope stats
  scopes <- readIORef (statsScopes stats)
  when (scopeLookups scopes > 0) $ do
    putStrLn "\n-- Scope Lookups --"
    putStrLn $ printf "Total lookups:     %12d" (scopeLookups scopes)
    putStrLn $ printf "  Lexical hits:    %12d (%.1f%%)"
      (scopeHits scopes)
      (100 * fromIntegral (scopeHits scopes) / fromIntegral (scopeLookups scopes) :: Double)
    putStrLn $ printf "  Dynamic hits:    %12d (%.1f%%)"
      (scopeDynamicHits scopes)
      (100 * fromIntegral (scopeDynamicHits scopes) / fromIntegral (scopeLookups scopes) :: Double)
    putStrLn $ printf "  Misses:          %12d (%.1f%%)"
      (scopeMisses scopes)
      (100 * fromIntegral (scopeMisses scopes) / fromIntegral (scopeLookups scopes) :: Double)
    putStrLn $ printf "Avg scope depth:   %12.1f"
      (fromIntegral (scopeTotalDepth scopes) / fromIntegral (scopeLookups scopes) :: Double)
    putStrLn $ printf "Avg scopes searched: %10.1f"
      (fromIntegral (scopeTotalSearched scopes) / fromIntegral (scopeLookups scopes) :: Double)
    putStrLn $ printf "Max scope depth:   %12d" (scopeMaxDepth scopes)
    putStrLn $ printf "Total time:        %12.1f ms" (fromIntegral (scopeTimeNs scopes) / 1e6 :: Double)
    putStrLn $ printf "Avg time per lookup: %10.2f us"
      (fromIntegral (scopeTimeNs scopes) / fromIntegral (scopeLookups scopes) / 1e3 :: Double)

  putStrLn "\n================================="
