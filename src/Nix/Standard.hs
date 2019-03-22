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
import           Nix.Value
import           Nix.Value.Monad
import           Nix.Var
#ifdef MIN_VERSION_haskeline
import           System.Console.Haskeline.MonadException hiding(catch)
#endif

newtype StdThunk (m :: * -> *) = StdThunk
  { _stdThunk :: StdCited m (NThunkF m (StdValue m)) }

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

type StdValue m = NValue (StdThunk m) (StdCited m) m
type StdValueNF m = NValueNF (StdThunk m) (StdCited m) m
-- type StdIdT m     = FreshIdT Int m

instance Show (StdThunk m) where
  show _ = "<thunk>"          -- jww (2019-03-15): NYI

type MonadStdThunk m
  = (MonadVar m, MonadCatch m, MonadThrow m, Typeable m, MonadAtomicRef m)

instance HasCitations m (StdValue m) (StdThunk m) where
  citations (StdThunk c) = citations1 c
  addProvenance x (StdThunk c) = StdThunk (addProvenance1 x c)

instance HasCitations1 m (StdValue m) (StdCited m) where
  citations1 (StdCited c) = citations1 c
  addProvenance1 x (StdCited c) = StdCited (addProvenance1 x c)

-- jww (2019-03-22): NYI
-- whileForcingThunk
--   :: forall t f m s e r . (Exception s, Convertible e t f m) => s -> m r -> m r
-- whileForcingThunk frame =
--   withFrame Debug (ForcingThunk @t @f @m) . withFrame Debug frame

newtype StandardT m a = StandardT
  { runStandardT
      :: ReaderT (Context (StandardT m) (StdValue (StandardT m)))
                (StateT (HashMap FilePath NExprLoc)
                        (StdIdT m)) a }
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
    , MonadReader (Context (StandardT m) (StdValue (StandardT m)))
    , MonadState (HashMap FilePath NExprLoc)
    )

#ifdef MIN_VERSION_haskeline
-- For whatever reason, using the default StateT instance provided by
-- haskeline does not work.
instance MonadException m
  => MonadException (StateT (HashMap FilePath NExprLoc) m) where
  controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
    in fmap (flip runStateT s) $ f run'

instance MonadException m => MonadException (StandardT m) where
  controlIO f = StandardT $ controlIO $ \(RunIO run) ->
    let run' = RunIO (fmap StandardT . run . runStandardT)
    in runStandardT <$> f run'
#endif

instance MonadTrans StandardT where
  lift = StandardT . lift . lift . lift

instance MonadRef m => MonadRef (StandardT m) where
  type Ref (StandardT m) = Ref m
  newRef  = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (StandardT m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance (MonadFile m, Monad m) => MonadFile (StandardT m)

instance MonadStore m => MonadStore (StandardT m) where
  addPath' = lift . addPath'
  toFile_' n = lift . toFile_' n

instance MonadPutStr m => MonadPutStr (StandardT m)
instance MonadHttp m => MonadHttp (StandardT m)
instance MonadEnv m => MonadEnv (StandardT m)
instance MonadInstantiate m => MonadInstantiate (StandardT m)
instance MonadExec m => MonadExec (StandardT m)
instance MonadIntrospect m => MonadIntrospect (StandardT m)

instance MonadAtomicRef m => MonadThunkId (StandardT m) where
  type ThunkId (StandardT m) = Int
  freshId = StandardT $ lift $ lift freshId

instance ( MonadAtomicRef m
         , MonadCatch m
         , Typeable m
         )
  => MonadThunk (StdThunk (StandardT m))
               (StandardT m)
               (StdValue (StandardT m)) where
  thunk   = fmap (StdThunk . StdCited) . thunk
  thunkId = thunkId . _stdCited . _stdThunk
  queryM x b f = queryM (_stdCited (_stdThunk x)) b f
  force    = force . _stdCited . _stdThunk
  forceEff = forceEff . _stdCited . _stdThunk
  further  = (fmap (StdThunk . StdCited) .) . further . _stdCited . _stdThunk

instance ( MonadAtomicRef m
         , MonadCatch m
         , Typeable m
         )
  => MonadValue (StdValue (StandardT m)) (StandardT m) where
  defer = fmap Pure . thunk

  demand (Pure v) f = force v (flip demand f)
  demand (Free v) f = f (Free v)

  inform (Pure t) f = Pure <$> further t f
  inform (Free v) f = Free <$> bindNValue' id (flip inform f) v

instance Monad m => Scoped (StdValue (StandardT m)) (StandardT m) where
  currentScopes = currentScopesReader
  clearScopes   = clearScopesReader @(StandardT m) @(StdValue (StandardT m))
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
         )
  => MonadEffects (StdThunk (StandardT m))
                 (StdCited (StandardT m))
                 (StandardT m) where
  makeAbsolutePath = defaultMakeAbsolutePath
  findEnvPath      = defaultFindEnvPath
  findPath         = defaultFindPath
  importPath       = defaultImportPath
  pathToDefaultNix = defaultPathToDefaultNix
  derivationStrict = defaultDerivationStrict
  traceEffect      = defaultTraceEffect

runWithBasicEffects
  :: (MonadIO m, MonadVar m) => Options -> StandardT m a -> m a
runWithBasicEffects opts =
  go . (`evalStateT` mempty) . (`runReaderT` newContext opts) . runStandardT
 where
  go action = do
    i <- newVar (1 :: Int)
    runFreshIdT i action

runWithBasicEffectsIO :: Options -> StandardT IO a -> IO a
runWithBasicEffectsIO = runWithBasicEffects
