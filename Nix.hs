{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix (eval, evalLoc, tracingEvalLoc, lint, runLintM) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as M
import           Data.IORef
import           Data.Text (Text)
import           Nix.Builtins
import qualified Nix.Eval as Eval
import           Nix.Eval hiding (eval)
import           Nix.Expr.Types (NExpr)
import           Nix.Expr.Types.Annotated (NExprLoc)
import qualified Nix.Lint as Lint
import           Nix.Lint hiding (lint)
import           Nix.Monad
import           Nix.Monad.Instance
import           Nix.Scope
import           Nix.Stack
import           Nix.Thunk
import           Nix.Utils

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: forall e m. MonadBuiltins e m
                 => Maybe FilePath -> NExpr -> m (NValueNF m)
evalTopLevelExpr mpath expr = do
    base <- baseEnv
    (normalForm =<<) $ pushScopes base $ case mpath of
        Nothing -> Eval.evalExpr expr
        Just path -> do
            traceM $ "Setting __cur_file = " ++ show path
            let ref = valueThunk @m $ NVLiteralPath path
            pushScope (M.singleton "__cur_file" ref)
                      (Eval.evalExpr expr)

eval :: (MonadFix m, MonadIO m)
     => Maybe FilePath -> NExpr -> m (NValueNF (Lazy m))
eval mpath = runLazyM . evalTopLevelExpr mpath

-- | Evaluate a nix expression in the default context
evalTopLevelExprLoc :: forall e m. MonadBuiltins e m
                    => Maybe FilePath -> NExprLoc -> m (NValueNF m)
evalTopLevelExprLoc mpath expr = do
    base <- baseEnv
    (normalForm =<<) $ pushScopes base $ case mpath of
        Nothing -> framedEvalExpr Eval.eval expr
        Just path -> do
            traceM $ "Setting __cur_file = " ++ show path
            let ref = valueThunk @m $ NVLiteralPath path
            pushScope (M.singleton "__cur_file" ref)
                      (framedEvalExpr Eval.eval expr)

evalLoc :: (MonadFix m, MonadIO m)
        => Maybe FilePath -> NExprLoc -> m (NValueNF (Lazy m))
evalLoc mpath = runLazyM . evalTopLevelExprLoc mpath

tracingEvalLoc :: forall m. (MonadFix m, MonadIO m, Alternative m)
    => Maybe FilePath -> NExprLoc -> m (NValueNF (Lazy m))
tracingEvalLoc mpath expr = do
    traced <- tracingEvalExpr Eval.eval expr
    case mpath of
        Nothing ->
            runLazyM (normalForm =<< (`pushScopes` traced) =<< baseEnv)
        Just path -> do
            traceM $ "Setting __cur_file = " ++ show path
            let ref = valueThunk @(Lazy m) $ NVLiteralPath path
            let m = M.singleton "__cur_file" ref
            runLazyM (baseEnv >>= (`pushScopes` pushScope m traced)
                                 >>= normalForm)

newtype Lint m a = Lint
    { runLint :: ReaderT (Context (Lint m) (SThunk (Lint m))) m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO,
              MonadReader (Context (Lint m) (SThunk (Lint m))))

instance MonadIO m => MonadVar (Lint m) where
    type Var (Lint m) = IORef

    newVar   = liftIO . newIORef
    readVar  = liftIO . readIORef
    writeVar = (liftIO .) . writeIORef

instance MonadIO m => MonadFile (Lint m) where
    readFile = liftIO . BS.readFile

instance MonadIO m =>
      Eval.MonadExpr (SThunk (Lint m))
          (IORef (NSymbolicF (NTypeF (Lint m) (SThunk (Lint m)))))
          (Lint m) where
    embedSet s = mkSymbolic [TSet (Just s)]
    projectSet = unpackSymbolic >=> \case
        NMany [TSet s] -> return s
        _ -> return Nothing

    type MText (Lint m) = Text

    wrapText   = return
    unwrapText = return

    embedText   = const $ mkSymbolic [TStr]
    projectText = const $ return Nothing

runLintM :: Lint m a -> m a
runLintM = flip runReaderT (Context emptyScopes []) . runLint

symbolicBaseEnv :: Monad m => m (Scopes m (SThunk m))
symbolicBaseEnv = return []     -- jww (2018-04-02): TODO

lint :: (MonadFix m, MonadIO m) => NExpr -> m (Symbolic (Lint m))
lint expr = runLintM $ symbolicBaseEnv
    >>= (`pushScopes` Lint.lintExpr expr)
