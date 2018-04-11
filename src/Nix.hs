{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix (eval, evalLoc, tracingEvalLoc, lint, runLintM) where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import qualified Data.HashMap.Lazy as M
import           Nix.Builtins
import qualified Nix.Eval as Eval
import           Nix.Eval hiding (eval)
import           Nix.Exec
import           Nix.Expr.Types (NExpr)
import           Nix.Expr.Types.Annotated (NExprLoc)
import           Nix.Lint (lint, runLintM)
import           Nix.Normal
import           Nix.Scope
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: forall e m. MonadBuiltins e m
                 => Maybe FilePath -> NExpr -> m (NValueNF m)
evalTopLevelExpr mpath expr = do
    base <- baseEnv
    (normalForm =<<) $ pushScopes base $ case mpath of
        Nothing -> Eval.evalExpr
            @_ @(NValue m) @(NThunk m) @m expr
        Just path -> do
            traceM $ "Setting __cur_file = " ++ show path
            let ref = value @(NValue m) @(NThunk m) @m $ NVPath path
            pushScope (M.singleton "__cur_file" ref)
                      (Eval.evalExpr @_ @(NValue m) @(NThunk m) @m expr)

eval :: (MonadFix m, MonadThrow m, MonadCatch m, MonadIO m)
     => Maybe FilePath -> NExpr -> m (NValueNF (Lazy m))
eval mpath = runLazyM . evalTopLevelExpr mpath

-- | Evaluate a nix expression in the default context
evalTopLevelExprLoc :: forall e m. MonadBuiltins e m
                    => Maybe FilePath -> NExprLoc -> m (NValueNF m)
evalTopLevelExprLoc mpath expr = do
    base <- baseEnv
    (normalForm =<<) $ pushScopes base $ case mpath of
        Nothing ->
            framedEvalExpr (Eval.eval @_ @(NValue m) @(NThunk m) @m) expr
        Just path -> do
            traceM $ "Setting __cur_file = " ++ show path
            let ref = value @(NValue m) @(NThunk m) @m $ NVPath path
            pushScope (M.singleton "__cur_file" ref)
                      (framedEvalExpr
                           (Eval.eval @_ @(NValue m) @(NThunk m) @m) expr)

evalLoc :: (MonadFix m, MonadThrow m, MonadCatch m, MonadIO m)
        => Maybe FilePath -> NExprLoc -> m (NValueNF (Lazy m))
evalLoc mpath = runLazyM . evalTopLevelExprLoc mpath

tracingEvalLoc
    :: forall m. (MonadFix m, MonadThrow m, MonadCatch m,
            Alternative m, MonadIO m)
    => Maybe FilePath -> NExprLoc -> m (NValueNF (Lazy m))
tracingEvalLoc mpath expr = do
    traced <- tracingEvalExpr @_ @(Lazy m) @_ @(NValue (Lazy m))
        (Eval.eval @_ @(NValue (Lazy m)) @(NThunk (Lazy m)) @(Lazy m)) expr
    case mpath of
        Nothing ->
            runLazyM (normalForm =<< (`pushScopes` traced) =<< baseEnv)
        Just path -> do
            traceM $ "Setting __cur_file = " ++ show path
            let ref = value @(NValue (Lazy m)) @(NThunk (Lazy m)) @(Lazy m)
                    $ NVPath path
            let m = M.singleton "__cur_file" ref
            runLazyM (baseEnv >>= (`pushScopes` pushScope m traced)
                                 >>= normalForm)
