{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module Nix.Standard where

import           Control.Applicative
import           Control.Comonad                ( Comonad )
import           Control.Comonad.Env            ( ComonadEnv )
import           Control.Monad.Catch     hiding ( catchJust )
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Free
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Control.Monad.State
import           Data.HashMap.Lazy              ( HashMap )
import qualified Data.HashMap.Strict
import           Data.Text                      ( Text )
import           Data.Typeable
import           GHC.Generics
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
import           Nix.Utils.Fix1
import           Nix.Value
import           Nix.Value.Monad
import           Nix.Var


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

type StdValue m = NValue (StdThunk m) (StdCited m) m

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
         , MonadState (HashMap FilePath NExprLoc, Data.HashMap.Strict.HashMap Text Text) m
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
  defer
    :: m (StdValue m)
    -> m (StdValue m)
  defer = fmap Pure . thunk

  demand
    :: StdValue m
    -> ( StdValue m
      -> m r
      )
    -> m r
  demand (Pure v) f = force v (flip demand f)
  demand (Free v) f = f (Free v)

  inform
    :: StdValue m
    -> ( m (StdValue m)
      -> m (StdValue m)
      )
    -> m (StdValue m)
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
                        (StateT (HashMap FilePath NExprLoc, HashMap Text Text) m) a)
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
    , MonadState (HashMap FilePath NExprLoc, HashMap Text Text)
    )

instance MonadTrans (StandardTF r) where
  lift = StandardTF . lift . lift

instance (MonadPutStr r, MonadPutStr m) => MonadPutStr (StandardTF r m)
instance (MonadHttp r, MonadHttp m) => MonadHttp (StandardTF r m)
instance (MonadEnv r, MonadEnv m) => MonadEnv (StandardTF r m)
instance (MonadPaths r, MonadPaths m) => MonadPaths (StandardTF r m)
instance (MonadInstantiate r, MonadInstantiate m) => MonadInstantiate (StandardTF r m)
instance (MonadExec r, MonadExec m) => MonadExec (StandardTF r m)
instance (MonadIntrospect r, MonadIntrospect m) => MonadIntrospect (StandardTF r m)

---------------------------------------------------------------------------------

type StandardT m = Fix1T StandardTF m

instance MonadTrans (Fix1T StandardTF) where
  lift = Fix1T . lift

instance MonadThunkId m => MonadThunkId (Fix1T StandardTF m) where
  type ThunkId (Fix1T StandardTF m) = ThunkId m

mkStandardT
  :: ReaderT
       (Context (StandardT m) (StdValue (StandardT m)))
       (StateT (HashMap FilePath NExprLoc, Data.HashMap.Strict.HashMap Text Text) m)
       a
  -> StandardT m a
mkStandardT = Fix1T . StandardTF

runStandardT
  :: StandardT m a
  -> ReaderT
       (Context (StandardT m) (StdValue (StandardT m)))
       (StateT (HashMap FilePath NExprLoc, Data.HashMap.Strict.HashMap Text Text) m)
       a
runStandardT (Fix1T (StandardTF m)) = m

runWithBasicEffects
  :: (MonadIO m, MonadAtomicRef m) => Options -> StandardT (StdIdT m) a -> m a
runWithBasicEffects opts =
  go . (`evalStateT` mempty) . (`runReaderT` newContext opts) . runStandardT
 where
  go action = do
    i <- newVar (1 :: Int)
    runFreshIdT i action

runWithBasicEffectsIO :: Options -> StandardT (StdIdT IO) a -> IO a
runWithBasicEffectsIO = runWithBasicEffects
