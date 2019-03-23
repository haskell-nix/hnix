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
import           Control.Comonad                ( Comonad )
import           Control.Comonad.Env            ( ComonadEnv )
import           Control.Monad.Catch     hiding ( catchJust )
import           Control.Monad.Free
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Control.Monad.State
import           Data.HashMap.Lazy              ( HashMap )
import           Data.Typeable
import           GHC.Generics
import           Nix.Cited
import           Nix.Cited.Basic
import           Nix.Context
import           Nix.Effects
import           Nix.Effects.Basic
import           Nix.Expr.Types.Annotated
import           Nix.Fresh
import           Nix.Fresh.Basic
import           Nix.Options
import           Nix.Render
import           Nix.Scope
import           Nix.Thunk
import           Nix.Thunk.Basic
import           Nix.Utils.Fix1
import           Nix.Value
import           Nix.Value.Monad
import           Nix.Var
#ifdef MIN_VERSION_haskeline
import           System.Console.Haskeline.MonadException hiding(catch)
#endif

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
    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
    in fmap (flip runStateT s) $ f run'

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

{------------------------------------------------------------------------}

newtype StdCited m a = StdCited
  { _stdCited :: Cited (StdThunk m) (StdCited m) m a }
  deriving
    ( Generic
    , Typeable
    , Functor
    , Applicative
    , Foldable
    , Traversable
    , Comonad
    , ComonadEnv [Provenance m (StdValue m)]
    )

newtype StdThunk (m :: * -> *) = StdThunk
  { _stdThunk :: StdCited m (NThunkF m (StdValue m)) }

type StdValue   m = NValue   (StdThunk m) (StdCited m) m
type StdValueNF m = NValueNF (StdThunk m) (StdCited m) m

instance Show (StdThunk m) where
  show _ = "<thunk>"

instance HasCitations1 m (StdValue m) (StdCited m) where
  citations1 (StdCited c) = citations1 c
  addProvenance1 x (StdCited c) = StdCited (addProvenance1 x c)

instance HasCitations m (StdValue m) (StdThunk m) where
  citations (StdThunk c) = citations1 c
  addProvenance x (StdThunk c) = StdThunk (addProvenance1 x c)

instance MonadReader (Context m (StdValue m)) m => Scoped (StdValue m) m where
  currentScopes = currentScopesReader
  clearScopes   = clearScopesReader @m @(StdValue m)
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
         , Scoped (StdValue m) m
         , MonadReader (Context m (StdValue m)) m
         , MonadState (HashMap FilePath NExprLoc) m
         , MonadDataErrorContext (StdThunk m) (StdCited m) m
         , MonadThunk (StdThunk m) m (StdValue m)
         , MonadValue (StdValue m) m
         )
  => MonadEffects (StdThunk m) (StdCited m) m where
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
         , MonadReader (Context m (StdValue m)) m
         , MonadThunkId m
         )
  => MonadThunk (StdThunk m) m (StdValue m) where
  thunk   = fmap (StdThunk . StdCited) . thunk
  thunkId = thunkId . _stdCited . _stdThunk
  queryM x b f = queryM (_stdCited (_stdThunk x)) b f
  force    = force . _stdCited . _stdThunk
  forceEff = forceEff . _stdCited . _stdThunk
  further  = (fmap (StdThunk . StdCited) .) . further . _stdCited . _stdThunk

instance ( MonadAtomicRef m
         , MonadCatch m
         , Typeable m
         , MonadReader (Context m (StdValue m)) m
         , MonadThunkId m
         )
  => MonadValue (StdValue m) m where
  defer = fmap Pure . thunk

  demand (Pure v) f = force v (flip demand f)
  demand (Free v) f = f (Free v)

  inform (Pure t) f = Pure <$> further t f
  inform (Free v) f = Free <$> bindNValue' id (flip inform f) v

{------------------------------------------------------------------------}

-- jww (2019-03-22): NYI
-- whileForcingThunk
--   :: forall t f m s e r . (Exception s, Convertible e t f m) => s -> m r -> m r
-- whileForcingThunk frame =
--   withFrame Debug (ForcingThunk @t @f @m) . withFrame Debug frame

newtype StandardTF r m a
  = StandardTF (ReaderT (Context r (StdValue r))
                        (StateT (HashMap FilePath NExprLoc) m) a)
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadFix
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadReader (Context r (StdValue r))
    , MonadState (HashMap FilePath NExprLoc)
    )

instance MonadTrans (StandardTF r) where
  lift = StandardTF . lift . lift

instance (MonadPutStr r, MonadPutStr m) => MonadPutStr (StandardTF r m)
instance (MonadHttp r, MonadHttp m) => MonadHttp (StandardTF r m)
instance (MonadEnv r, MonadEnv m) => MonadEnv (StandardTF r m)
instance (MonadInstantiate r, MonadInstantiate m) => MonadInstantiate (StandardTF r m)
instance (MonadExec r, MonadExec m) => MonadExec (StandardTF r m)
instance (MonadIntrospect r, MonadIntrospect m) => MonadIntrospect (StandardTF r m)

{------------------------------------------------------------------------}

type StandardT m = Fix1T StandardTF m

instance MonadTrans (Fix1T StandardTF) where
  lift = Fix1T . lift

-- | This instance is based on the 'StdIdT' layer of 'StandardTF m'.
instance MonadThunkId m => MonadThunkId (Fix1T StandardTF m) where
  type ThunkId (Fix1T StandardTF m) = ThunkId m

mkStandardT
  :: ReaderT (Context (StandardT m) (StdValue (StandardT m)))
            (StateT (HashMap FilePath NExprLoc)
                    m) a
  -> StandardT m a
mkStandardT = Fix1T . StandardTF

runStandardT
  :: StandardT m a
  -> ReaderT (Context (StandardT m) (StdValue (StandardT m)))
            (StateT (HashMap FilePath NExprLoc)
                    m) a
runStandardT (Fix1T (StandardTF m)) = m

runWithBasicEffects :: (MonadIO m, MonadAtomicRef m)
                    => Options -> StandardT (StdIdT m) a -> m a
runWithBasicEffects opts =
  go . (`evalStateT` mempty)
     . (`runReaderT` newContext opts)
     . runStandardT
 where
  go action = do
    i <- newVar (1 :: Int)
    runFreshIdT i action

runWithBasicEffectsIO :: Options -> StandardT (StdIdT IO) a -> IO a
runWithBasicEffectsIO = runWithBasicEffects
