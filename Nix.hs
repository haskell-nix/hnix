module Nix where

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
            ref <- valueRef $ NVLiteralPath dir
            pushScope (M.singleton "__cwd" ref) (contextualExprEval expr)

evalTopLevelExprIO :: Maybe FilePath -> NExprLoc -> IO (NValueNF (Cyclic IO))
evalTopLevelExprIO mdir = runCyclicIO . evalTopLevelExpr mdir

-- informativeEvalTopLevelExprIO :: Maybe FilePath -> NExpr
--                               -> IO (NValueNF (Cyclic IO))
-- informativeEvalTopLevelExprIO mdir expr =
--     runReaderT (runCyclic (evalTopLevelExpr mdir expr)) []

tracingEvalTopLevelExprIO :: Maybe FilePath -> NExprLoc
                          -> IO (NValueNF (Cyclic IO))
tracingEvalTopLevelExprIO mdir expr = do
    traced <- tracingExprEval expr
    case mdir of
        Nothing ->
            runCyclicIO (normalForm =<< (`pushScopes` traced) =<< baseEnv)
        Just dir -> do
            traceM $ "Setting __cwd = " ++ show dir
            ref <- runCyclicIO (valueRef $ NVLiteralPath dir)
            let m = M.singleton "__cwd" ref
            runCyclicIO (baseEnv >>= (`pushScopes` pushScope m traced)
                                 >>= normalForm)

lintExpr :: NExprLoc -> IO ()
lintExpr expr =
    runCyclicIO (baseEnv >>= (`pushScopes` checkExpr (stripAnnotation expr)))
