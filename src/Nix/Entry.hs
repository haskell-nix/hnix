{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Entry where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as Text
import           Nix.Builtins
import           Nix.Effects
import qualified Nix.Eval as Eval
import           Nix.Expr.Types (NExpr)
import           Nix.Expr.Types.Annotated (NExprLoc)
import           Nix.Scope
import           Nix.Stack
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

type MonadNix e m =
    (Scoped e (NThunk m) m, Framed e m, MonadVar m, MonadFile m,
     MonadEffects m, MonadFix m, MonadCatch m)

-- | Evaluate a nix expression in the default context
evalTopLevelExprGen
    :: forall e m a. MonadNix e m
    => (a -> m (NValue m)) -> Maybe FilePath -> [String] -> a
    -> m (NValue m)
evalTopLevelExprGen cont mpath incls expr = do
    base <- baseEnv
    let i = value @(NValue m) @(NThunk m) @m $ NVList $
            map (value @(NValue m) @(NThunk m) @m
                     . flip NVStr mempty . Text.pack) incls
    pushScope (M.singleton "__includes" i) $
        pushScopes base $ case mpath of
            Nothing -> cont expr
            Just path -> do
                traceM $ "Setting __cur_file = " ++ show path
                let ref = value @(NValue m) @(NThunk m) @m $ NVPath path
                pushScope (M.singleton "__cur_file" ref) $ cont expr

-- | Evaluate a nix expression in the default context
eval :: forall e m. MonadNix e m
     => Maybe FilePath -> [String] -> NExpr -> m (NValue m)
eval = evalTopLevelExprGen $
    Eval.evalExpr @_ @(NValue m) @(NThunk m) @m

-- | Evaluate a nix expression in the default context
evalLoc :: forall e m. MonadNix e m
        => Maybe FilePath -> [String] -> NExprLoc -> m (NValue m)
evalLoc = evalTopLevelExprGen $
    Eval.framedEvalExpr (Eval.eval @_ @(NValue m) @(NThunk m) @m)

tracingEvalLoc
    :: forall e m. (MonadNix e m, Alternative m, MonadIO m)
    => Maybe FilePath -> [String] -> NExprLoc -> m (NValue m)
tracingEvalLoc mpath incls expr =
    evalTopLevelExprGen id mpath incls
        =<< Eval.tracingEvalExpr @_ @m @_ @(NValue m)
                (Eval.eval @_ @(NValue m)
                           @(NThunk m) @m) expr
