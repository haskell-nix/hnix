{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix where

import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Trans.Reader
import qualified Data.HashMap.Lazy as M
import           Nix.Builtins
import           Nix.Eval
import           Nix.Expr.Types.Annotated (NExprLoc, stripAnnotation)
import           Nix.Lint
import           Nix.Monad
import           Nix.Monad.Instance
import           Nix.Scope
import           Nix.Utils

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: MonadBuiltins e m
                 => Maybe FilePath -> NExprLoc -> m (NValueNF m)
evalTopLevelExpr mdir expr = do
    base <- baseEnv
    (normalForm =<<) $ pushScopes base $ case mdir of
        Nothing -> contextualExprEval expr
        Just dir -> do
            traceM $ "Setting __cwd = " ++ show dir
            ref <- valueThunk $ NVLiteralPath dir
            pushScope (M.singleton "__cwd" ref) (contextualExprEval expr)

evalTopLevelExprIO :: Maybe FilePath -> NExprLoc -> IO (NValueNF (Lazy IO))
evalTopLevelExprIO mdir = runLazyIO . evalTopLevelExpr mdir

-- informativeEvalTopLevelExprIO :: Maybe FilePath -> NExpr
--                               -> IO (NValueNF (Lazy IO))
-- informativeEvalTopLevelExprIO mdir expr =
--     runReaderT (runLazy (evalTopLevelExpr mdir expr)) []

tracingEvalTopLevelExprIO :: Maybe FilePath -> NExprLoc
                          -> IO (NValueNF (Lazy IO))
tracingEvalTopLevelExprIO mdir expr = do
    traced <- tracingExprEval expr
    case mdir of
        Nothing ->
            runLazyIO (normalForm =<< (`pushScopes` traced) =<< baseEnv)
        Just dir -> do
            traceM $ "Setting __cwd = " ++ show dir
            ref <- runLazyIO (valueThunk $ NVLiteralPath dir)
            let m = M.singleton "__cwd" ref
            runLazyIO (baseEnv >>= (`pushScopes` pushScope m traced)
                                 >>= normalForm)

newtype Lint m a = Lint
    { runLint :: ReaderT (Context (Symbolic (Lint m))) m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO,
              MonadReader (Context (Symbolic (Lint m))))

runLintIO :: Lint IO a -> IO a
runLintIO = flip runReaderT (Context emptyScopes []) . runLint

symbolicBaseEnv :: Monad m => m (Scopes (Symbolic m))
symbolicBaseEnv = return [Scope M.empty False]

lintExprIO :: NExprLoc -> IO (Symbolic (Lint IO))
lintExprIO expr =
    runLintIO (symbolicBaseEnv
                   >>= (`pushScopes` lintExpr (stripAnnotation expr)))

tracingLintExprIO :: NExprLoc -> IO (Symbolic (Lint IO))
tracingLintExprIO expr = do
    traced <- tracingExprLint expr
    ref <- runLintIO $ mkSymbolic [TPath]
    let m = M.singleton "__cwd" ref
    runLintIO (symbolicBaseEnv >>= (`pushScopes` pushScope m traced))
