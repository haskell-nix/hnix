{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module Nix.Standard where

import           Control.Applicative
import           Control.Monad.Catch     hiding ( catchJust )
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Free
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Control.Monad.State
import           Data.Coerce
import           Data.Constraint                ( (\\) )
import           Data.Constraint.Forall         ( Forall, inst )
import           Data.Functor.Identity
import           Data.HashMap.Lazy              ( HashMap )
import           Data.Text                      ( Text )
import           Data.Typeable
import           Nix.Cited
import           Nix.Context
import           Nix.Effects
import           Nix.Effects.Basic
import           Nix.Effects.Derivation
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


instance MonadFile m => MonadFile (StandardTF r m)
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
         , MonadPaths m
         )
  => MonadEffects Identity (StandardT m) where
  makeAbsolutePath = defaultMakeAbsolutePath
  findEnvPath      = defaultFindEnvPath
  findPath         = defaultFindPath
  importPath       = defaultImportPath
  pathToDefaultNix = defaultPathToDefaultNix
  derivationStrict = defaultDerivationStrict
  traceEffect      = defaultTraceEffect

{------------------------------------------------------------------------}

-- jww (2019-03-22): NYI
-- whileForcingThunk
--   :: forall t f m s e r . (Exception s, Convertible e t f m) => s -> m r -> m r
-- whileForcingThunk frame =
--   withFrame Debug (ForcingThunk @t @f @m) . withFrame Debug frame

type StandardTFInner r m = ScopeT (NValue Identity r) r
  (ThunkT (NValue Identity r) --TODO: What should this `Identity` be? Probably (StdCited ...)
    (ReaderT Context
      (StateT (HashMap FilePath NExprLoc, HashMap Text Text) m)))

newtype StandardTF r m a
  = StandardTF { unStandardTF :: StandardTFInner r m a }
  deriving
    ( Applicative
    , Alternative
    , Monad
    , MonadFail
    , MonadPlus
    , MonadFix
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadMask
    , MonadReader Context
    , MonadState (HashMap FilePath NExprLoc, HashMap Text Text)
    , MonadStore
    )

deriving instance (Monad m, Monad r, Thunk r ~ StdThunk r m) => Scoped r (Free (NValue' Identity r) (StdThunk r m)) (StandardTF r m)

deriving instance Functor m => Functor (StandardTF r m)

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

instance MonadPaths m => MonadPaths (StandardTF r m)

instance ( Monad m
         , Typeable m
         , MonadAtomicRef m
         , MonadCatch m
         ) => MonadValue (Free (NValue' Identity (StandardT m)) (StdThunk (StandardT m) m)) (StandardT m) where
  defer = fmap pure . thunk

  demand (Pure v) f = force v (`demand` f)
  demand (Free v) f = f (Free v)

  inform (Pure t) f = Pure <$> further t f
  inform (Free v) f = Free <$> bindNValue' id (`inform` f) v

--TODO
instance HasCitations m' v (StdThunk r m) where
  citations _ = []
  addProvenance _ = id

instance HasCitations1 m v Identity where
  citations1 _ = []
  addProvenance1 _ = id

---------------------------------------------------------------------------------

type StandardT m = Fix1T StandardTF m

class MonadTrans (t (Fix1T t m)) => TransAtFix1T t m

instance MonadTrans (t (Fix1T t m)) => TransAtFix1T t m

instance Forall (TransAtFix1T t) => MonadTrans (Fix1T t) where
  lift (x :: m a) = Fix1T $ (lift \\ inst @(TransAtFix1T t) @m) x

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
