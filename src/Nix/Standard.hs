{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Standard where

import           Control.Applicative
import           Control.Monad.Catch     hiding ( catchJust )
import           Control.Monad.Free
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Control.Monad.State
import           Data.Coerce
import           Data.Functor.Identity
import           Data.HashMap.Lazy              ( HashMap )
import           Data.Typeable
import           Nix.Cited
import           Nix.Context
import           Nix.Effects
import           Nix.Effects.Basic
import           Nix.Expr.Types.Annotated
import           Nix.Thunk.StableId
import           Nix.Thunk.Basic
import           Nix.Options
import           Nix.Render
import           Nix.Scope
import           Nix.Scope.Basic
import           Nix.Thunk
import           Nix.Utils.Fix1
import           Nix.Value
import           Nix.Value.Monad
#ifdef MIN_VERSION_haskeline
import           System.Console.Haskeline.MonadException hiding(catch)
#endif
--TODO: Put citations back in

-- All of the following type classes defer to the underlying 'm'.

deriving instance MonadPutStr (t (Fix1 t)) => MonadPutStr (Fix1 t)
deriving instance MonadHttp (t (Fix1 t)) => MonadHttp (Fix1 t)
deriving instance MonadEnv (t (Fix1 t)) => MonadEnv (Fix1 t)
deriving instance MonadInstantiate (t (Fix1 t)) => MonadInstantiate (Fix1 t)
deriving instance MonadExec (t (Fix1 t)) => MonadExec (Fix1 t)
deriving instance MonadIntrospect (t (Fix1 t)) => MonadIntrospect (Fix1 t)

deriving instance MonadPutStr (t (Fix1T t m) m) => MonadPutStr (Fix1T t m)
deriving instance MonadHttp (t (Fix1T t m) m) => MonadHttp (Fix1T t m)
deriving instance MonadEnv (t (Fix1T t m) m) => MonadEnv (Fix1T t m)
deriving instance MonadInstantiate (t (Fix1T t m) m) => MonadInstantiate (Fix1T t m)
deriving instance MonadExec (t (Fix1T t m) m) => MonadExec (Fix1T t m)
deriving instance MonadIntrospect (t (Fix1T t m) m) => MonadIntrospect (Fix1T t m)

#ifdef MIN_VERSION_haskeline
-- For whatever reason, using the default StateT instance provided by
-- haskeline does not work.
instance MonadException m
  => MonadException (StateT (HashMap FilePath NExprLoc) m) where
  controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
    run' = RunIO(fmap(StateT . const) . run . flip runStateT s)
    in fmap(flip runStateT s) $ f run'

instance MonadException m => MonadException (Fix1T StandardTF m) where
  controlIO f = mkStandardT $ controlIO $ \(RunIO run) ->
    let run' = RunIO (fmap mkStandardT . run . runStandardT)
    in runStandardT <$> f run'
#endif

type MonadFix1T t m = (MonadTrans (Fix1T t), Monad (t (Fix1T t m) m))

instance (MonadFix1T t m, MonadRef m) => MonadRef (Fix1T t m) where
  type Ref (Fix1T t m) = Ref m
  newRef  = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance (MonadFix1T t m, MonadAtomicRef m) => MonadAtomicRef (Fix1T t m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance (MonadFix1T t m, MonadFile m) => MonadFile (Fix1T t m)

instance (MonadFix1T t m, MonadStore m) => MonadStore (Fix1T t m) where
  addPath' = lift . addPath'
  toFile_' n = lift . toFile_' n

deriving instance MonadThunk (t (Fix1T t m) m) => MonadThunk (Fix1T t m)

{------------------------------------------------------------------------}

{-
newtype StdCited (t :: *) (m :: * -> *) a = StdCited
  { _stdCited :: Cited (StdThunk t m) (StdCited t m) m a }
  deriving
    ( Generic
    , Typeable
    , Functor
    , Applicative
    , Foldable
    , Traversable
    , Comonad
    , ComonadEnv [Provenance m (StdValue t m)]
    )

newtype StdThunk t (m :: * -> *) = StdThunk
  { _stdThunk :: StdCited t m t }
-}

--type StdValue m a = NValue m a

instance ( MonadFix m
         , MonadFile m
         , MonadCatch m
         , MonadEnv m
         , MonadExec m
         , MonadHttp m
         , MonadInstantiate m
         , MonadIntrospect m
         , MonadPlus m
         , MonadPutStr m
         , MonadStore m
         , MonadAtomicRef m
         , Typeable m
         )
  => MonadEffects Identity (StandardT m) where
  makeAbsolutePath = defaultMakeAbsolutePath
  findEnvPath      = defaultFindEnvPath
  findPath         = defaultFindPath
  importPath       = defaultImportPath
  pathToDefaultNix = defaultPathToDefaultNix
  derivationStrict = defaultDerivationStrict
  traceEffect      = defaultTraceEffect

{-
instance Show (StdThunk t m) where
  show _ = "<thunk>"

instance HasCitations1 m (StdValue t m) (StdCited t m) where
  citations1 (StdCited c) = citations1 c
  addProvenance1 x (StdCited c) = StdCited (addProvenance1 x c)

instance HasCitations m (StdValue t m) (StdThunk t m) where
  citations (StdThunk c) = citations1 c
  addProvenance x (StdThunk c) = StdThunk (addProvenance1 x c)

instance MonadReader (Context m (StdValue t m)) m => Scoped (StdValue t m) m where
  currentScopes = currentScopesReader
  clearScopes   = clearScopesReader @m @(StdValue t m)
  pushScopes    = pushScopesReader
  lookupVar     = lookupVarReader

instance ( MonadFix m
         , MonadFile m
         , MonadCatch m
         , MonadEnv m
         , MonadExec m
         , MonadHttp m
         , MonadInstantiate m
         , MonadIntrospect m
         , MonadPlus m
         , MonadPutStr m
         , MonadStore m
         , MonadAtomicRef m
         , Typeable m
         , Scoped (StdValue t m) m
         , MonadReader (Context m (StdValue t m)) m
         , MonadState (HashMap FilePath NExprLoc) m
         , MonadDataErrorContext (StdThunk t m) (StdCited t m) m
         , MonadThunk (StdThunk t m) m (StdValue t m)
         , MonadValue (StdValue t m) m
         )
  => MonadEffects (StdCited t m) m where
  makeAbsolutePath = defaultMakeAbsolutePath
  findEnvPath      = defaultFindEnvPath
  findPath         = defaultFindPath
  importPath       = defaultImportPath
  pathToDefaultNix = defaultPathToDefaultNix
  derivationStrict = defaultDerivationStrict
  traceEffect      = defaultTraceEffect

instance ( MonadAtomicRef m
         , MonadCatch m
         , Typeable m
         , MonadReader (Context m (StdValue t m)) m
         , MonadThunkId m
         , MonadThunk t m (Free (NValue' (StdThunk t m) (StdCited t m) m) (StdThunk t m))
         , Typeable t
         )
  => MonadThunk (StdThunk t m) m (StdValue t m) where
  thunk   = fmap (StdThunk . StdCited) . thunk
  thunkId = thunkId . _stdCited . _stdThunk
  queryM x b f = queryM (_stdCited (_stdThunk x)) b f
  force    = force . _stdCited . _stdThunk
  forceEff = forceEff . _stdCited . _stdThunk
  further  = (fmap (StdThunk . StdCited) .) . further . _stdCited . _stdThunk

instance ( MonadAtomicRef m
         , MonadCatch m
         , Typeable m
         , MonadReader (Context m (StdValue t m)) m
         , MonadThunk t m (Free (NValue' (StdThunk t m) (StdCited t m) m) (StdThunk t m))
         , Typeable t
         )
  => MonadValue (StdValue t m) m where
  defer = fmap Pure . thunk

  demand (Pure v) f = force v (flip demand f)
  demand (Free v) f = f (Free v)

  inform (Pure t) f = Pure <$> further t f
  inform (Free v) f = Free <$> bindNValue' id (flip inform f) v
-}

{------------------------------------------------------------------------}

-- jww (2019-03-22): NYI
-- whileForcingThunk
--   :: forall t f m s e r . (Exception s, Convertible e t f m) => s -> m r -> m r
-- whileForcingThunk frame =
--   withFrame Debug (ForcingThunk @t @f @m) . withFrame Debug frame

type StandardTFInner r m = ScopeT (NValue Identity r)
  (ThunkT (NValue Identity r) --TODO: What should this `Identity` be? Probably (StdCited ...)
    (ReaderT Context
      (StateT (HashMap FilePath NExprLoc) m)))

newtype StandardTF r m a
  = StandardTF { unStandardTF :: StandardTFInner r m a }
  deriving
    ( Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadFix
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadReader Context
    , MonadState (HashMap FilePath NExprLoc)
    )

deriving instance Functor m => Functor (StandardTF r m)
instance Monad m => Scoped (Free (NValue' Identity (StandardT m)) (StdThunk (StandardT m) m)) (StandardT m) where
  currentScopes = mkStandardT $ hoistDynamicScopes mkStandardT <$> currentScopes
  clearScopes = mkStandardT . clearScopes . runStandardT
  pushScopes s = mkStandardT . pushScopes (hoistDynamicScopes runStandardT s) . runStandardT
  lookupVar = mkStandardT . lookupVar

instance MonadTrans (StandardTF r) where
  lift = StandardTF . lift . lift . lift . lift

instance MonadTransWrap (StandardTF r) where
  liftWrap f (StandardTF a) = StandardTF $ liftWrap (liftWrap (liftWrap (liftWrap f))) a

instance (MonadPutStr m) => MonadPutStr (StandardTF r m)
instance (MonadHttp m) => MonadHttp (StandardTF r m)
instance (MonadEnv m) => MonadEnv (StandardTF r m)
instance (MonadInstantiate m) => MonadInstantiate (StandardTF r m)
instance (MonadExec m) => MonadExec (StandardTF r m)
instance (MonadIntrospect m) => MonadIntrospect (StandardTF r m)

instance ( Monad m
         , Typeable r
         , Typeable (Thunk r)
         , Typeable m
         , MonadAtomicRef m
         , MonadCatch m
         ) => MonadThunk (StandardTF r m) where
  type Thunk (StandardTF r m) = StdThunk r m
  type ThunkValue (StandardTF r m) = StdValue r
  thunk v = StandardTF $ StdThunk <$> thunk (unStandardTF v)
  queryM = coerce $ queryM @(StandardTFInner r m)
  force = coerce $ force @(StandardTFInner r m)
  forceEff = coerce $ forceEff @(StandardTFInner r m)
  further t f = fmap StdThunk $ StandardTF $ further (unStdThunk t) $ unStandardTF . f . StandardTF

newtype StdThunk r m = StdThunk { unStdThunk :: Thunk (StandardTFInner r m) }
  deriving (Eq, Ord, Show, Typeable)

type StdValue r = NValue Identity r

instance ( Monad m
         , Typeable m
         , MonadAtomicRef m
         , MonadCatch m
         ) => MonadValue (Free (NValue' Identity (StandardT m)) (StdThunk (StandardT m) m)) (StandardT m) where
  defer = fmap pure . thunk

  demand (Pure v) f = force v (flip demand f)
  demand (Free v) f = f (Free v)

  inform (Pure t) f = Pure <$> further t f
  inform (Free v) f = Free <$> bindNValue' id (flip inform f) v

--TODO
instance HasCitations m' v (StdThunk r m) where
  citations _ = []
  addProvenance _ = id

instance HasCitations1 m v Identity where
  citations1 _ = []
  addProvenance1 _ = id

{------------------------------------------------------------------------}

type StandardT m = Fix1T StandardTF m

instance MonadTrans (Fix1T StandardTF) where
  lift = Fix1T . lift

mkStandardT
  :: StandardTFInner (Fix1T StandardTF m) m a
  -> StandardT m a
mkStandardT = Fix1T . StandardTF

runStandardT
  :: StandardT m a
  -> StandardTFInner (Fix1T StandardTF m) m a
runStandardT (Fix1T (StandardTF m)) = m

runWithBasicEffects
  :: (MonadIO m, MonadAtomicRef m) => Options -> StandardT m a -> m a
runWithBasicEffects opts =
  (`evalStateT` mempty) . (`runReaderT` newContext opts) . (`runThunkT` nil) . (`runScopeT` mempty) . runStandardT

runWithBasicEffectsIO :: Options -> StandardT IO a -> IO a
runWithBasicEffectsIO = runWithBasicEffects
