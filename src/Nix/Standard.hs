{-# language AllowAmbiguousTypes #-}
{-# language TypeFamilies #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language ConstraintKinds #-}
{-# language PatternSynonyms #-}

{-# options_ghc -Wno-orphans #-}


module Nix.Standard where

import           Nix.Prelude                   hiding ( StateT
                                                , runStateT
                                                , evalStateT
                                                , execStateT
                                                )
import           Control.Monad.Trans.State.Strict
                                                ( StateT
                                                , evalStateT
                                                )
import           Control.Comonad                ( Comonad )
import           Control.Comonad.Env            ( ComonadEnv )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                , MonadMask
                                                )
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Free             ( Free(Free) )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Ref              ( MonadRef(newRef)
                                                , MonadAtomicRef
                                                )
import qualified Text.Show
import           Nix.Cited
import           Nix.Cited.Basic
import           Nix.Config.Singleton
import           Nix.Context
import           Nix.EvalStats                  ( EvalStats(..)
                                                , newEvalStats
                                                , printEvalStats
                                                , recordThunkCreate
                                                , recordThunkForce
                                                , recordScopeLookup
                                                , ScopeLookupResult(..)
                                                )
import qualified GHC.Clock                     as Clock
import           Nix.Effects
import           Nix.Effects.Basic
import           Nix.Effects.Derivation
import           Nix.Expr.Types.Annotated
import           Nix.Fresh
import           Nix.Fresh.Basic
import           Nix.Options
import           Nix.Render
import           Nix.Scope
import           Nix.Expr.Types                 ( VarName )
import           Nix.Store.Overlay
import           Nix.Thunk
import           Nix.Thunk.Basic                ( NThunkF(..)
                                                , isComputed
                                                )
import           Nix.Utils.Fix1                 ( Fix1T(Fix1T) )
import           Nix.Value
import           Nix.Value.Monad
import qualified System.IO                     as IO
import qualified System.Nix.StorePath          as Store


newtype StdCited m a =
  StdCited
    (Cited (StdThunk m) (StdCited m) m a)
  deriving
    ( Generic, Typeable
    , Functor, Applicative, Comonad, ComonadEnv [Provenance m (StdValue m)]
    , Foldable, Traversable
    )

newtype StdThunk m =
  StdThunk
    (StdCited m (NThunkF m (StdValue m)))
type StdValue' m = NValue' (StdThunk m) (StdCited m) m (StdValue m)
type StdValue m = NValue (StdThunk m) (StdCited m) m

-- | Standard evaluation monad with compile-time configuration.
--
-- The @cfg@ parameter enables zero-cost conditional execution.
-- Use 'withEvalCfg' at program startup to bridge runtime options to @cfg@.
type StdM (cfg :: EvalCfg) m = StandardT cfg (StdIdT m)

-- | Value type in the standard monad.
type StdValM (cfg :: EvalCfg) m = StdValue (StdM cfg m)

-- | Thunk type in the standard monad.
type StdThunM (cfg :: EvalCfg) m = StdThunk (StdM cfg m)

type StdBase m =
  ( MonadFix m
  , MonadFile m
  , MonadCatch m
  , MonadThrow m
  , MonadMask m
  , MonadEnv m
  , MonadPaths m
  , MonadExec m
  , MonadHttp m
  , MonadInstantiate m
  , MonadIntrospect m
  , MonadPlus m
  , MonadPutStr m
  , MonadStore m
  , MonadStoreRead m
  , MonadAtomicRef m
  , Typeable m
  )

-- | Type alias:
--
-- > Cited (StdThunk m) (StdCited m) m (NThunkF m (StdValue m))
type CitedStdThunk m = Cited (StdThunk m) (StdCited m) m (NThunkF m (StdValue m))

instance Show (StdThunk m) where
  show _ = toString thunkStubText

instance HasCitations1 m (StdValue m) (StdCited m) where
  citations1 (StdCited c) = citations1 c
  addProvenance1 x (StdCited c) = StdCited $ addProvenance1 x c

instance HasCitations m (StdValue m) (StdThunk m) where
  citations (StdThunk c) = citations1 c
  addProvenance x (StdThunk c) = StdThunk $ addProvenance1 x c

instance (MonadReader (Context cfg m (StdValue m)) m, MonadIO m) => Scoped (StdValue m) m where
  askScopes   = askScopesReader
  clearScopes = clearScopesReader @m @(StdValue m)
  pushScopes  = pushScopesReader
  setScopes   = setScopesReader   -- Single operation, more efficient than clear+push
  lookupVar   = lookupVarWithStats

-- | Instrumented lookupVar that records scope stats when enabled
lookupVarWithStats
  :: forall cfg m
  . ( MonadReader (Context cfg m (StdValue m)) m
    , MonadIO m
    )
  => VarName
  -> m (Maybe (StdValue m))
lookupVarWithStats k = do
  mstats <- askEvalStats
  case mstats of
    Nothing -> lookupVarReader k
    Just stats -> do
      (result, info, elapsed) <- lookupVarReaderWithInfo @m @(StdValue m) k
      let scopeResult = case info of
            LexicalHit depth searched -> ScopeLexicalHit depth searched
            DynamicHit depth searched -> ScopeDynamicHit depth searched
            LookupMiss depth -> ScopeMiss depth
      recordScopeLookup stats scopeResult elapsed
      pure result

instance
  ( MonadFix m
  , MonadFile m
  , MonadCatch m
  , MonadEnv m
  , MonadPaths m
  , MonadExec m
  , MonadHttp m
  , MonadInstantiate m
  , MonadIntrospect m
  , MonadPlus m
  , MonadPutStr m
  , MonadStore m
  , MonadStoreRead m
  , MonadAtomicRef m
  , Typeable m
  , Scoped (StdValue m) m
  , MonadReader (Context cfg m (StdValue m)) m
  , MonadState (HashMap Path NExprLoc, HashMap Text Text) m
  , MonadDataErrorContext (StdThunk m) (StdCited m) m
  , MonadThunk (StdThunk m) m (StdValue m)
  , MonadValue (StdValue m) m
  , HasProvCfg cfg
  )
  => MonadEffects (StdThunk m) (StdCited m) m where
  toAbsolutePath   = defaultToAbsolutePath
  findEnvPath      = defaultFindEnvPath
  findPath         = defaultFindPath
  importPath       = defaultImportPath
  pathToDefaultNix = defaultPathToDefaultNix
  derivationStrict = defaultDerivationStrict
  traceEffect      = defaultTraceEffect

-- 2021-07-24:
-- This instance currently is to satisfy @MonadThunk@ requirements for @normalForm@ function.
-- As it is seen from the instance - it does superficial type class jump.
-- It is just a type boundary for thunking.
instance
  ( Typeable       m
  , MonadThunkId   m
  , MonadAtomicRef m
  , MonadCatch     m
  , MonadIO        m
  , MonadReader (Context cfg m (StdValue m)) m
  )
  => MonadThunk (StdThunk m) m (StdValue m) where

  thunkId
    :: StdThunk m
    -> ThunkId  m
  thunkId = thunkId @(CitedStdThunk m) . coerce
  {-# INLINABLE thunkId #-}

  thunk
    :: m (StdValue m)
    -> m (StdThunk m)
  thunk action = do
    mstats <- askEvalStats
    traverse_ recordThunkCreate mstats
    coerce <$> thunk @(CitedStdThunk m) action
  {-# INLINABLE thunk #-}

  query
    :: m (StdValue m)
    ->    StdThunk m
    -> m (StdValue m)
  query b = query @(CitedStdThunk m) b . coerce
  {-# INLINABLE query #-}

  force
    ::    StdThunk m
    -> m (StdValue m)
  force t = do
    mstats <- askEvalStats
    case mstats of
      Nothing -> force @(CitedStdThunk m) (coerce t)
      Just stats -> do
        -- Only check computed state when profiling (for accurate stats)
        let CitedP _ innerThunk = coerce t :: CitedStdThunk m
        wasComputed <- isComputed innerThunk

        -- Save parent's accumulated child thunk time and reset for our children
        parentThunkChildTime <- liftIO $ readIORef (statsThunkChildTime stats)
        liftIO $ writeIORef (statsThunkChildTime stats) 0

        -- Save IO time before forcing (to exclude IO from pure compute time)
        ioTimeBefore <- liftIO $ readIORef (statsIOTime stats)

        start <- liftIO Clock.getMonotonicTimeNSec
        result <- force @(CitedStdThunk m) (coerce t)
        end <- liftIO Clock.getMonotonicTimeNSec

        -- Get IO time after forcing
        ioTimeAfter <- liftIO $ readIORef (statsIOTime stats)
        let ioTimeDuring = ioTimeAfter - ioTimeBefore

        -- Get time spent in child thunk forces
        ourThunkChildTime <- liftIO $ readIORef (statsThunkChildTime stats)

        let elapsed = end - start
            -- For cache misses: exclusive time = total - child thunks - IO
            exclTime = if elapsed > ourThunkChildTime + ioTimeDuring
                         then elapsed - ourThunkChildTime - ioTimeDuring
                         else 0

        -- For hits, record full elapsed (it's just overhead); for misses, record exclusive time
        recordThunkForce stats wasComputed (if wasComputed then elapsed else exclTime)

        -- Add our total time to parent's child thunk time accumulator
        liftIO $ writeIORef (statsThunkChildTime stats) (parentThunkChildTime + elapsed)

        pure result
  {-# INLINABLE force #-}

  forceEff
    ::    StdThunk m
    -> m (StdValue m)
  forceEff = forceEff @(CitedStdThunk m) . coerce
  {-# INLINABLE forceEff #-}

  further
    ::    StdThunk m
    -> m (StdThunk m)
  further = fmap coerce . further @(CitedStdThunk m) . coerce
  {-# INLINABLE further #-}


-- * @instance MonadValue (StdValue m) m@

instance
  ( MonadAtomicRef m
  , MonadCatch m
  , MonadIO m
  , Typeable m
  , MonadReader (Context cfg m (StdValue m)) m
  , MonadThunkId m
  )
  => MonadValue (StdValue m) m where

  defer
    :: m (StdValue m)
    -> m (StdValue m)
  defer action = pure . coerce <$> thunk @(StdThunk m) action
  {-# INLINABLE defer #-}

  demand
    :: StdValue m
    -> m (StdValue m)
  demand = go -- lock to ensure no type class jumps.
   where
    go :: StdValue m -> m (StdValue m)
    go =
      free
        (go <=< force @(StdThunk m) . coerce)
        (pure . Free)
  {-# INLINABLE demand #-}

  inform
    :: StdValue m
    -> m (StdValue m)
  inform = go -- lock to ensure no type class jumps.
   where
    go :: StdValue m -> m (StdValue m)
    go =
      free
        ((pure . coerce <$>) . (further @(CitedStdThunk m) . coerce))
        ((Free <$>) . bindNValue' id go)
  {-# INLINABLE inform #-}


-- | The core evaluation transformer, parameterized by compile-time config.
--
-- The @cfg@ parameter enables zero-cost conditional execution in the
-- 'MonadEval' instance. When @cfg@ is known at compile time (via 'withEvalCfg'),
-- GHC eliminates unused branches for disabled features.
newtype StandardTF (cfg :: EvalCfg) r m a
  = StandardTF
      (ReaderT
        (Context cfg r (StdValue r))
        (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
        a
      )
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadFail
    , MonadPlus
    , MonadFix
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadMask
    , MonadReader (Context cfg r (StdValue r))
    , MonadState (HashMap Path NExprLoc, HashMap Text Text)
    )

instance MonadTrans (StandardTF cfg r) where
  lift = StandardTF . lift . lift
  {-# INLINABLE lift #-}

instance (MonadPutStr r, MonadPutStr m)
  => MonadPutStr (StandardTF cfg r m)
instance (MonadHttp r, MonadHttp m)
  => MonadHttp (StandardTF cfg r m)
instance (MonadEnv r, MonadEnv m)
  => MonadEnv (StandardTF cfg r m)
instance (MonadPaths r, MonadPaths m)
  => MonadPaths (StandardTF cfg r m)
instance (MonadInstantiate r, MonadInstantiate m)
  => MonadInstantiate (StandardTF cfg r m)
instance (MonadExec r, MonadExec m)
  => MonadExec (StandardTF cfg r m)
instance (MonadIntrospect r, MonadIntrospect m)
  => MonadIntrospect (StandardTF cfg r m)

---------------------------------------------------------------------------------

-- | Standard evaluation monad, parameterized by compile-time config.
--
-- When @cfg@ is known at compile time, GHC eliminates unused branches
-- for disabled features (stats, provenance, tracing).
type StandardT (cfg :: EvalCfg) m = Fix1T (StandardTF cfg) m

instance MonadTrans (Fix1T (StandardTF cfg)) where
  lift = Fix1T . lift
  {-# INLINABLE lift #-}

instance MonadThunkId m
  => MonadThunkId (StandardT cfg m) where

  type ThunkId (StandardT cfg m) = ThunkId m

mkStandardT
  :: ReaderT
      (Context cfg (StandardT cfg m) (StdValue (StandardT cfg m)))
      (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
      a
  -> StandardT cfg m a
mkStandardT = coerce
{-# INLINABLE mkStandardT #-}

runStandardT
  :: StandardT cfg m a
  -> ReaderT
      (Context cfg (StandardT cfg m) (StdValue (StandardT cfg m)))
      (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
      a
runStandardT = coerce
{-# INLINABLE runStandardT #-}

runWithBasicEffectsAndStats
  :: (MonadIO m, MonadAtomicRef m)
  => Options
  -> Maybe EvalStats
  -> StandardT cfg (StdIdT m) a
  -> m a
runWithBasicEffectsAndStats opts mstats =
  fun . (`evalStateT` mempty) . (`runReaderT` newContextWithStats opts mstats) . runStandardT
 where
  fun action =
    runFreshIdT action =<< newRef (1 :: Int)

runWithBasicEffects
  :: (MonadIO m, MonadAtomicRef m)
  => Options
  -> StandardT cfg (StdIdT m) a
  -> m a
runWithBasicEffects opts = runWithBasicEffectsAndStats opts Nothing

-- | Type-parameterized runner with compile-time configuration dispatch.
--
-- When configuration flags are known at compile time (established via 'withEvalCfg'),
-- this function enables zero-cost conditional execution. The type parameters flow through
-- to the action, allowing evaluation functions like 'evalExprLocT' to use compile-time
-- dispatch.
--
-- Example usage:
--
-- @
-- main' opts = withEvalCfg (isEvalStats opts) (isValues opts) (isTrace opts) $
--   \\(_ :: Proxy cfg) ->
--     runWithStoreEffectsIOT @cfg opts myAction
-- @
runWithStoreEffectsIOT
  :: forall (cfg :: EvalCfg) a
   . KnownEvalCfg cfg
  => Options
  -> (forall m. (StdBase m, KnownEvalCfg cfg) => StdM cfg m a)
  -> IO a
runWithStoreEffectsIOT opts action = do
  -- Create stats collector only when type-level says it's needed
  -- When CfgStats cfg ~ 'False, GHC eliminates the Just branch
  mstats <- ifStats @cfg (Just <$> newEvalStats) (pure Nothing)

  -- Warn about invalid option combinations
  when (getStoreMode opts == StoreRemote && getStoreDir opts /= "/nix/store") $
    IO.hPutStrLn IO.stderr "Warning: --store-dir is ignored in remote mode (nix-daemon always uses /nix/store)"

  -- Run the action
  result <- case getStoreMode opts of
    StoreRemote ->
      runWithBasicEffectsAndStats opts mstats (action :: StdM cfg IO a)
    StoreOverlay ->
      let
        storeDir = Store.StoreDir $ encodeUtf8 $ toText $ getStoreDir opts
        storeCfg = OverlayStoreConfig
          { overlayStoreDir = storeDir
          , overlayReadThrough = isStoreReadThrough opts
          }
      in
        evalOverlayStoreT storeCfg defaultOverlayStoreState $
          runWithBasicEffectsAndStats opts mstats (action :: StdM cfg (OverlayStoreT IO) a)

  -- Print stats only when enabled at type level
  -- When CfgStats cfg ~ 'False, GHC eliminates this branch
  whenStatsM @cfg $ traverse_ printEvalStats mstats

  pure result
{-# INLINABLE runWithStoreEffectsIOT #-}
