{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as Text
import           Nix.Builtins
import qualified Nix.Eval as Eval
import           Nix.Exec
import           Nix.Expr.Types (NExpr)
import           Nix.Expr.Types.Annotated (NExprLoc)
import           Nix.Normal
import           Nix.Scope
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

-- | Evaluate a nix expression in the default context
evalTopLevelExprGen
    :: forall e m a. MonadBuiltins e m
    => (a -> m (NValue m)) -> Maybe FilePath -> [String] -> a
    -> m (NValueNF m)
evalTopLevelExprGen cont mpath incls expr = do
    base <- baseEnv
    let i = value @(NValue m) @(NThunk m) @m $ NVList $
            map (value @(NValue m) @(NThunk m) @m
                     . flip NVStr mempty . Text.pack) incls
    pushScope (M.singleton "__includes" i) $
        (normalForm =<<) $ pushScopes base $ case mpath of
            Nothing -> cont expr
            Just path -> do
                traceM $ "Setting __cur_file = " ++ show path
                let ref = value @(NValue m) @(NThunk m) @m $ NVPath path
                pushScope (M.singleton "__cur_file" ref) $ cont expr

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: forall e m. MonadBuiltins e m
                 => Maybe FilePath -> [String] -> NExpr -> m (NValueNF m)
evalTopLevelExpr = evalTopLevelExprGen $
    Eval.evalExpr @_ @(NValue m) @(NThunk m) @m

eval :: (MonadFix m, MonadThrow m, MonadCatch m, MonadIO m)
     => Maybe FilePath -> [String] -> NExpr -> m (NValueNF (Lazy m))
eval mpath incls = runLazyM . evalTopLevelExpr mpath incls

-- | Evaluate a nix expression in the default context
evalTopLevelExprLoc :: forall e m. MonadBuiltins e m
                    => Maybe FilePath -> [String] -> NExprLoc -> m (NValueNF m)
evalTopLevelExprLoc = evalTopLevelExprGen $
    Eval.framedEvalExpr (Eval.eval @_ @(NValue m) @(NThunk m) @m)

evalLoc :: (MonadFix m, MonadThrow m, MonadCatch m, MonadIO m)
        => Maybe FilePath -> [String] -> NExprLoc -> m (NValueNF (Lazy m))
evalLoc mpath incls = runLazyM . evalTopLevelExprLoc mpath incls

tracingEvalLoc
    :: forall m. (MonadFix m, MonadThrow m, MonadCatch m,
            Alternative m, MonadIO m)
    => Maybe FilePath -> [String] -> NExprLoc -> m (NValueNF (Lazy m))
tracingEvalLoc mpath incls expr =
    runLazyM . evalTopLevelExprGen id mpath incls
        =<< Eval.tracingEvalExpr @_ @(Lazy m) @_ @(NValue (Lazy m))
                (Eval.eval @_ @(NValue (Lazy m))
                           @(NThunk (Lazy m)) @(Lazy m)) expr
