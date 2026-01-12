{-# LANGUAGE DeriveGeneric #-}

-- | Statistics collection for evaluation profiling
module Nix.EvalStats
  ( EvalStats(..)
  , ExprStats(..)
  , ThunkStats(..)
  , BuiltinStats(..)
  , ScopeStats(..)
  , IOStats(..)
  , ScopeLookupResult(..)
  , newEvalStats
  , recordExprExclusive
  , withExprTiming
  , recordThunkForce
  , recordThunkCreate
  , withBuiltinTiming
  , recordScopeLookup
  , recordFileRead
  , recordDrvRead
  , recordHttpFetch
  , recordStoreAdd
  , printEvalStats
  ) where

import           Nix.Prelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified GHC.Clock as Clock

-- | Statistics for a single expression type
data ExprStats = ExprStats
  { exprCount      :: Word64  -- ^ Number of evaluations
  , exprTimeNs     :: Word64  -- ^ Total (inclusive) time in nanoseconds
  , exprExclTimeNs :: Word64  -- ^ Exclusive time (excluding children) in nanoseconds
  } deriving (Show, Eq)

instance Semigroup ExprStats where
  ExprStats c1 t1 e1 <> ExprStats c2 t2 e2 = ExprStats (c1 + c2) (t1 + t2) (e1 + e2)

instance Monoid ExprStats where
  mempty = ExprStats 0 0 0

-- | Statistics for thunk operations
data ThunkStats = ThunkStats
  { thunkCreated   :: Word64  -- ^ Number of thunks created
  , thunkForced    :: Word64  -- ^ Number of force operations
  , thunkHit       :: Word64  -- ^ Cache hits (already computed)
  , thunkMiss      :: Word64  -- ^ Cache misses (needed computation)
  , thunkTimeNs    :: Word64  -- ^ Time spent in thunk computation (misses only)
  , thunkHitTimeNs :: Word64  -- ^ Time spent in cache hits (overhead only)
  } deriving (Show, Eq)

instance Semigroup ThunkStats where
  ThunkStats c1 f1 h1 m1 t1 ht1 <> ThunkStats c2 f2 h2 m2 t2 ht2 =
    ThunkStats (c1+c2) (f1+f2) (h1+h2) (m1+m2) (t1+t2) (ht1+ht2)

instance Monoid ThunkStats where
  mempty = ThunkStats 0 0 0 0 0 0

-- | Statistics for a single builtin function
data BuiltinStats = BuiltinStats
  { builtinCount  :: Word64  -- ^ Number of calls
  , builtinTimeNs :: Word64  -- ^ Total time in nanoseconds
  } deriving (Show, Eq)

instance Semigroup BuiltinStats where
  BuiltinStats c1 t1 <> BuiltinStats c2 t2 = BuiltinStats (c1 + c2) (t1 + t2)

instance Monoid BuiltinStats where
  mempty = BuiltinStats 0 0

-- | Statistics for scope lookups
data ScopeStats = ScopeStats
  { scopeLookups       :: Word64  -- ^ Total number of lookups
  , scopeHits          :: Word64  -- ^ Found in lexical scope
  , scopeDynamicHits   :: Word64  -- ^ Found in dynamic (with) scope
  , scopeMisses        :: Word64  -- ^ Not found
  , scopeTotalDepth    :: Word64  -- ^ Sum of scope depths at lookup time
  , scopeTotalSearched :: Word64  -- ^ Sum of scopes searched before finding
  , scopeMaxDepth      :: Word64  -- ^ Maximum scope depth seen
  , scopeTimeNs        :: Word64  -- ^ Total time spent in lookups
  } deriving (Show, Eq)

instance Semigroup ScopeStats where
  ScopeStats l1 h1 d1 m1 td1 ts1 md1 t1 <> ScopeStats l2 h2 d2 m2 td2 ts2 md2 t2 =
    ScopeStats (l1+l2) (h1+h2) (d1+d2) (m1+m2) (td1+td2) (ts1+ts2) (max md1 md2) (t1+t2)

instance Monoid ScopeStats where
  mempty = ScopeStats 0 0 0 0 0 0 0 0

-- | Statistics for IO operations (file reads, HTTP fetches, store operations)
data IOStats = IOStats
  { ioFileReads      :: Word64  -- ^ Number of file reads
  , ioFileReadTimeNs :: Word64  -- ^ Total time in file reads (nanoseconds)
  , ioDrvReads       :: Word64  -- ^ Number of .drv file reads specifically
  , ioDrvReadTimeNs  :: Word64  -- ^ Time in .drv file reads (nanoseconds)
  , ioHttpFetches    :: Word64  -- ^ Number of HTTP fetches
  , ioHttpTimeNs     :: Word64  -- ^ Time in HTTP operations (nanoseconds)
  , ioStoreAdds      :: Word64  -- ^ Number of store add operations
  , ioStoreAddTimeNs :: Word64  -- ^ Time in store add operations (nanoseconds)
  } deriving (Show, Eq)

instance Semigroup IOStats where
  IOStats fr1 frt1 dr1 drt1 hf1 ht1 sa1 sat1 <> IOStats fr2 frt2 dr2 drt2 hf2 ht2 sa2 sat2 =
    IOStats (fr1+fr2) (frt1+frt2) (dr1+dr2) (drt1+drt2) (hf1+hf2) (ht1+ht2) (sa1+sa2) (sat1+sat2)

instance Monoid IOStats where
  mempty = IOStats 0 0 0 0 0 0 0 0

-- | All evaluation statistics
data EvalStats = EvalStats
  { statsExprs     :: IORef (HashMap Text ExprStats)
  , statsThunks    :: IORef ThunkStats
  , statsBuiltins  :: IORef (HashMap Text BuiltinStats)
  , statsScopes    :: IORef ScopeStats
  , statsChildTime :: IORef Word64  -- ^ Accumulated child time for exclusive timing
  , statsIO        :: IORef IOStats -- ^ Aggregate IO statistics
  , statsIOTime    :: IORef Word64  -- ^ Accumulated IO time for exclusive timing (like statsChildTime)
  , statsThunkChildTime :: IORef Word64  -- ^ Accumulated child thunk time for exclusive thunk timing
  , statsBuiltinChildTime :: IORef Word64  -- ^ Accumulated child builtin time for exclusive builtin timing
  }

-- | Create a new empty stats collector
newEvalStats :: MonadIO m => m EvalStats
newEvalStats = liftIO $ EvalStats
  <$> newIORef mempty
  <*> newIORef mempty
  <*> newIORef mempty
  <*> newIORef mempty
  <*> newIORef 0
  <*> newIORef mempty
  <*> newIORef 0
  <*> newIORef 0
  <*> newIORef 0

-- | Record an expression evaluation with exclusive timing
recordExprExclusive :: MonadIO m => EvalStats -> Text -> Word64 -> Word64 -> m ()
recordExprExclusive stats name totalNs exclNs = liftIO $
  modifyIORef' (statsExprs stats) $
    HM.insertWith (<>) name (ExprStats 1 totalNs exclNs)
{-# INLINABLE recordExprExclusive #-}

-- | Execute an action with timing, tracking exclusive time (excluding children AND IO)
--   Returns the result and adds the total time to parent's child time
withExprTiming :: MonadIO m => EvalStats -> Text -> m a -> m a
withExprTiming stats name action = do
  -- Save parent's accumulated child time and reset for our children
  parentChildTime <- liftIO $ readIORef (statsChildTime stats)
  liftIO $ writeIORef (statsChildTime stats) 0

  -- Save parent's accumulated IO time and reset for our IO operations
  parentIOTime <- liftIO $ readIORef (statsIOTime stats)
  liftIO $ writeIORef (statsIOTime stats) 0

  -- Time the action
  start <- liftIO Clock.getMonotonicTimeNSec
  result <- action
  end <- liftIO Clock.getMonotonicTimeNSec

  -- Get time spent in our children
  ourChildTime <- liftIO $ readIORef (statsChildTime stats)
  -- Get time spent in IO during this expression
  ourIOTime <- liftIO $ readIORef (statsIOTime stats)

  let totalTime = end - start
      -- Exclusive time excludes both child expressions AND IO operations
      exclTime = if totalTime > ourChildTime + ourIOTime
                   then totalTime - ourChildTime - ourIOTime
                   else 0

  -- Record our stats
  recordExprExclusive stats name totalTime exclTime

  -- Add our total time to parent's child time accumulator
  liftIO $ writeIORef (statsChildTime stats) (parentChildTime + totalTime)
  -- Add our IO time to parent's IO time accumulator (IO bubbles up)
  liftIO $ writeIORef (statsIOTime stats) (parentIOTime + ourIOTime)

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

-- | Record a builtin function call with exclusive timing
recordBuiltinExclusive :: MonadIO m => EvalStats -> Text -> Word64 -> m ()
recordBuiltinExclusive stats name exclNs = liftIO $
  modifyIORef' (statsBuiltins stats) $
    HM.insertWith (<>) name (BuiltinStats 1 exclNs)
{-# INLINABLE recordBuiltinExclusive #-}

-- | Execute a builtin with timing, tracking exclusive time (excluding nested builtins)
--   Returns the result and adds the total time to parent's child builtin time
withBuiltinTiming :: MonadIO m => EvalStats -> Text -> m a -> m a
withBuiltinTiming stats name action = do
  -- Save parent's accumulated child builtin time and reset for our children
  parentChildTime <- liftIO $ readIORef (statsBuiltinChildTime stats)
  liftIO $ writeIORef (statsBuiltinChildTime stats) 0

  -- Time the action
  start <- liftIO Clock.getMonotonicTimeNSec
  result <- action
  end <- liftIO Clock.getMonotonicTimeNSec

  -- Get time spent in child builtins
  ourChildTime <- liftIO $ readIORef (statsBuiltinChildTime stats)

  let totalTime = end - start
      -- Exclusive time excludes child builtins
      exclTime = if totalTime > ourChildTime
                   then totalTime - ourChildTime
                   else 0

  -- Record our stats with exclusive time
  recordBuiltinExclusive stats name exclTime

  -- Add our total time to parent's child builtin time accumulator
  liftIO $ writeIORef (statsBuiltinChildTime stats) (parentChildTime + totalTime)

  pure result
{-# INLINABLE withBuiltinTiming #-}

-- | Result of a scope lookup for profiling
data ScopeLookupResult
  = ScopeLexicalHit Int Int   -- ^ Found in lexical scope: depth, scopes searched
  | ScopeDynamicHit Int Int   -- ^ Found in dynamic scope: depth, scopes searched
  | ScopeMiss Int             -- ^ Not found: depth searched

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

-- | Record a general file read (e.g., .nix files during import)
--   Also updates the IO time accumulator so this time is excluded from expression exclusive time
recordFileRead :: MonadIO m => EvalStats -> Word64 -> m ()
recordFileRead stats elapsedNs = liftIO $ do
  modifyIORef' (statsIO stats) $ \s -> s
    { ioFileReads = ioFileReads s + 1
    , ioFileReadTimeNs = ioFileReadTimeNs s + elapsedNs
    }
  modifyIORef' (statsIOTime stats) (+ elapsedNs)
{-# INLINABLE recordFileRead #-}

-- | Record a .drv file read
--   Also updates the IO time accumulator so this time is excluded from expression exclusive time
recordDrvRead :: MonadIO m => EvalStats -> Word64 -> m ()
recordDrvRead stats elapsedNs = liftIO $ do
  modifyIORef' (statsIO stats) $ \s -> s
    { ioDrvReads = ioDrvReads s + 1
    , ioDrvReadTimeNs = ioDrvReadTimeNs s + elapsedNs
    , ioFileReads = ioFileReads s + 1
    , ioFileReadTimeNs = ioFileReadTimeNs s + elapsedNs
    }
  modifyIORef' (statsIOTime stats) (+ elapsedNs)
{-# INLINABLE recordDrvRead #-}

-- | Record an HTTP fetch operation
--   Also updates the IO time accumulator so this time is excluded from expression exclusive time
recordHttpFetch :: MonadIO m => EvalStats -> Word64 -> m ()
recordHttpFetch stats elapsedNs = liftIO $ do
  modifyIORef' (statsIO stats) $ \s -> s
    { ioHttpFetches = ioHttpFetches s + 1
    , ioHttpTimeNs = ioHttpTimeNs s + elapsedNs
    }
  modifyIORef' (statsIOTime stats) (+ elapsedNs)
{-# INLINABLE recordHttpFetch #-}

-- | Record a store add operation
--   Also updates the IO time accumulator so this time is excluded from expression exclusive time
recordStoreAdd :: MonadIO m => EvalStats -> Word64 -> m ()
recordStoreAdd stats elapsedNs = liftIO $ do
  modifyIORef' (statsIO stats) $ \s -> s
    { ioStoreAdds = ioStoreAdds s + 1
    , ioStoreAddTimeNs = ioStoreAddTimeNs s + elapsedNs
    }
  modifyIORef' (statsIOTime stats) (+ elapsedNs)
{-# INLINABLE recordStoreAdd #-}

-- | Print statistics summary to stdout as JSON
printEvalStats :: MonadIO m => EvalStats -> m ()
printEvalStats stats = liftIO $ do
  -- Expression stats
  exprMap <- readIORef (statsExprs stats)
  let exprList = List.sortOn (negate . exprExclTimeNs . snd) $ HM.toList exprMap
      exprJson = A.object
        [ "byType" A..= A.object
            [ AK.fromText name A..= A.object
                [ "count" A..= exprCount s
                , "exclusiveTimeNs" A..= exprExclTimeNs s
                , "inclusiveTimeNs" A..= exprTimeNs s
                ]
            | (name, s) <- exprList
            ]
        , "total" A..= A.object
            [ "count" A..= sum (map (exprCount . snd) exprList)
            , "exclusiveTimeNs" A..= sum (map (exprExclTimeNs . snd) exprList)
            , "inclusiveTimeNs" A..= sum (map (exprTimeNs . snd) exprList)
            ]
        ]

  -- Thunk stats
  thunks <- readIORef (statsThunks stats)
  let thunkJson = A.object
        [ "created" A..= thunkCreated thunks
        , "forced" A..= thunkForced thunks
        , "cacheHits" A..= thunkHit thunks
        , "cacheMisses" A..= thunkMiss thunks
        , "cacheHitTimeNs" A..= thunkHitTimeNs thunks
        , "computeTimeNs" A..= thunkTimeNs thunks
        ]

  -- Builtin stats
  builtinMap <- readIORef (statsBuiltins stats)
  let builtinList = List.sortOn (negate . builtinTimeNs . snd) $ HM.toList builtinMap
      builtinJson = A.object
        [ "byName" A..= A.object
            [ AK.fromText name A..= A.object
                [ "count" A..= builtinCount s
                , "timeNs" A..= builtinTimeNs s
                ]
            | (name, s) <- builtinList
            ]
        , "total" A..= A.object
            [ "count" A..= sum (map (builtinCount . snd) builtinList)
            , "timeNs" A..= sum (map (builtinTimeNs . snd) builtinList)
            ]
        ]

  -- Scope stats
  scopes <- readIORef (statsScopes stats)
  let scopeJson = A.object
        [ "totalLookups" A..= scopeLookups scopes
        , "lexicalHits" A..= scopeHits scopes
        , "dynamicHits" A..= scopeDynamicHits scopes
        , "misses" A..= scopeMisses scopes
        , "totalDepth" A..= scopeTotalDepth scopes
        , "totalSearched" A..= scopeTotalSearched scopes
        , "maxDepth" A..= scopeMaxDepth scopes
        , "timeNs" A..= scopeTimeNs scopes
        ]

  -- IO stats
  ioStats <- readIORef (statsIO stats)
  let ioJson = A.object
        [ "fileReads" A..= A.object
            [ "count" A..= ioFileReads ioStats
            , "timeNs" A..= ioFileReadTimeNs ioStats
            ]
        , "drvReads" A..= A.object
            [ "count" A..= ioDrvReads ioStats
            , "timeNs" A..= ioDrvReadTimeNs ioStats
            ]
        , "httpFetches" A..= A.object
            [ "count" A..= ioHttpFetches ioStats
            , "timeNs" A..= ioHttpTimeNs ioStats
            ]
        , "storeAdds" A..= A.object
            [ "count" A..= ioStoreAdds ioStats
            , "timeNs" A..= ioStoreAddTimeNs ioStats
            ]
        , "totalTimeNs" A..= (ioFileReadTimeNs ioStats + ioHttpTimeNs ioStats + ioStoreAddTimeNs ioStats)
        ]

  -- Combined JSON output
  let fullJson = A.object
        [ "expressions" A..= exprJson
        , "thunks" A..= thunkJson
        , "builtins" A..= builtinJson
        , "scopes" A..= scopeJson
        , "io" A..= ioJson
        ]

  BL.putStrLn $ A.encode fullJson
