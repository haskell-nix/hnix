{-# language TypeFamilies #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language UndecidableInstances #-}

{-# options_ghc -Wno-orphans #-}


module Nix.Standard where

import           Nix.Prelude
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
import           Control.Monad.Reader           ( MonadFix )
import           Control.Monad.Ref              ( MonadRef(newRef)
                                                , MonadAtomicRef
                                                )
import qualified Text.Show
import           Nix.Cited
import           Nix.Cited.Basic
import           Nix.Context
import           Nix.Effects
import           Nix.Effects.Basic
import           Nix.Effects.Derivation
import           Nix.Expr.Types.Annotated
import           Nix.Fresh
import           Nix.Fresh.Basic
import           Nix.Options
import           Nix.Render
import           Nix.Scope
import           Nix.Thunk
import           Nix.Thunk.Basic
import           Nix.Utils.Fix1                 ( Fix1T(Fix1T) )
import           Nix.Value
import           Nix.Value.Monad


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
type StandardIO = StandardT (StdIdT IO)
type StdVal = StdValue StandardIO
type StdThun = StdThunk StandardIO
type StdIO = StandardIO ()

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

instance MonadReader (Context m (StdValue m)) m => Scoped (StdValue m) m where
  askScopes   = askScopesReader
  clearScopes = clearScopesReader @m @(StdValue m)
  pushScopes  = pushScopesReader
  lookupVar   = lookupVarReader

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
  , MonadAtomicRef m
  , Typeable m
  , Scoped (StdValue m) m
  , MonadReader (Context m (StdValue m)) m
  , MonadState (HashMap Path NExprLoc, HashMap Text Text) m
  , MonadDataErrorContext (StdThunk m) (StdCited m) m
  , MonadThunk (StdThunk m) m (StdValue m)
  , MonadValue (StdValue m) m
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
  , MonadReader (Context m (StdValue m)) m
  )
  => MonadThunk (StdThunk m) m (StdValue m) where

  thunkId
    :: StdThunk m
    -> ThunkId  m
  thunkId = thunkId @(CitedStdThunk m) . coerce
  {-# inline thunkId #-}

  thunk
    :: m (StdValue m)
    -> m (StdThunk m)
  thunk = fmap coerce . thunk @(CitedStdThunk m)
  {-# inline thunk #-}

  query
    :: m (StdValue m)
    ->    StdThunk m
    -> m (StdValue m)
  query b = query @(CitedStdThunk m) b . coerce
  {-# inline query #-}

  force
    ::    StdThunk m
    -> m (StdValue m)
  force = force @(CitedStdThunk m) . coerce
  {-# inline force #-}

  forceEff
    ::    StdThunk m
    -> m (StdValue m)
  forceEff = forceEff @(CitedStdThunk m) . coerce
  {-# inline forceEff #-}

  further
    ::    StdThunk m
    -> m (StdThunk m)
  further = fmap coerce . further @(CitedStdThunk m) . coerce
  {-# inline further #-}


-- * @instance MonadThunkF@ (Kleisli functor HOFs)

-- | This is a functorized version in CPS.

-- Please do not use MonadThunkF instances to define MonadThunk. as MonadThunk uses specialized functions.
instance
  ( Typeable       m
  , MonadThunkId   m
  , MonadAtomicRef m
  , MonadCatch     m
  , MonadReader (Context m (StdValue m)) m
  )
  => MonadThunkF (StdThunk m) m (StdValue m) where

  queryF
    :: ( StdValue m
       -> m r
       )
    -> m r
    -> StdThunk m
    -> m r
  queryF k b = queryF @(CitedStdThunk m) k b . coerce

  forceF
    :: ( StdValue m
       -> m r
       )
    -> StdThunk m
    -> m r
  forceF k = forceF @(CitedStdThunk m) k . coerce

  forceEffF
    :: ( StdValue m
       -> m r
       )
    -> StdThunk m
    -> m r
  forceEffF k = forceEffF @(CitedStdThunk m) k . coerce

  furtherF
    :: ( m (StdValue m)
       -> m (StdValue m)
       )
    ->    StdThunk m
    -> m (StdThunk m)
  furtherF k = fmap coerce . furtherF @(CitedStdThunk m) k . coerce


-- * @instance MonadValue (StdValue m) m@

instance
  ( MonadAtomicRef m
  , MonadCatch m
  , Typeable m
  , MonadReader (Context m (StdValue m)) m
  , MonadThunkId m
  )
  => MonadValue (StdValue m) m where

  defer
    :: m (StdValue m)
    -> m (StdValue m)
  defer = fmap (pure . coerce) . thunk @(CitedStdThunk m)

  demand
    :: StdValue m
    -> m (StdValue m)
  demand = go -- lock to ensure no type class jumps.
   where
    go :: StdValue m -> m (StdValue m)
    go =
      free
        (go <=< force @(CitedStdThunk m) . coerce)
        (pure . Free)

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


-- * @instance MonadValueF (StdValue m) m@

instance
  ( MonadAtomicRef m
  , MonadCatch m
  , Typeable m
  , MonadReader (Context m (StdValue m)) m
  , MonadThunkId m
  )
  => MonadValueF (StdValue m) m where

  demandF
    :: ( StdValue m
      -> m r
      )
    -> StdValue m
    -> m r
  demandF f = f <=< demand

  informF
    :: ( m (StdValue m)
      -> m (StdValue m)
      )
    -> StdValue m
    -> m (StdValue m)
  informF f = f . inform


{------------------------------------------------------------------------}

-- jww (2019-03-22): NYI
-- whileForcingThunk
--   :: forall t f m s e r . (Exception s, Convertible e t f m) => s -> m r -> m r
-- whileForcingThunk frame =
--   withFrame Debug (ForcingThunk @t @f @m) . withFrame Debug frame

newtype StandardTF r m a
  = StandardTF
      (ReaderT
        (Context r (StdValue r))
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
    , MonadReader (Context r (StdValue r))
    , MonadState (HashMap Path NExprLoc, HashMap Text Text)
    )

instance MonadTrans (StandardTF r) where
  lift = StandardTF . lift . lift
  {-# inline lift #-}

instance (MonadPutStr r, MonadPutStr m)
  => MonadPutStr (StandardTF r m)
instance (MonadHttp r, MonadHttp m)
  => MonadHttp (StandardTF r m)
instance (MonadEnv r, MonadEnv m)
  => MonadEnv (StandardTF r m)
instance (MonadPaths r, MonadPaths m)
  => MonadPaths (StandardTF r m)
instance (MonadInstantiate r, MonadInstantiate m)
  => MonadInstantiate (StandardTF r m)
instance (MonadExec r, MonadExec m)
  => MonadExec (StandardTF r m)
instance (MonadIntrospect r, MonadIntrospect m)
  => MonadIntrospect (StandardTF r m)

---------------------------------------------------------------------------------

type StandardT m = Fix1T StandardTF m

instance MonadTrans (Fix1T StandardTF) where
  lift = Fix1T . lift
  {-# inline lift #-}

instance MonadThunkId m
  => MonadThunkId (StandardT m) where

  type ThunkId (StandardT m) = ThunkId m

mkStandardT
  :: ReaderT
      (Context (StandardT m) (StdValue (StandardT m)))
      (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
      a
  -> StandardT m a
mkStandardT = coerce

runStandardT
  :: StandardT m a
  -> ReaderT
      (Context (StandardT m) (StdValue (StandardT m)))
      (StateT (HashMap Path NExprLoc, HashMap Text Text) m)
      a
runStandardT = coerce

runWithBasicEffects
  :: (MonadIO m, MonadAtomicRef m)
  => Options
  -> StandardT (StdIdT m) a
  -> m a
runWithBasicEffects opts =
  fun . (`evalStateT` mempty) . (`runReaderT` newContext opts) . runStandardT
 where
  fun action =
    runFreshIdT action =<< newRef (1 :: Int)

runWithBasicEffectsIO :: Options -> StandardIO a -> IO a
runWithBasicEffectsIO = runWithBasicEffects
