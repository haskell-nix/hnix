module Nix where

import           Control.Monad.Trans.Reader
import qualified Data.Map.Lazy as Map
import           Nix.Builtins
import           Nix.Eval
import           Nix.Expr (NExpr)
import           Nix.Lint
import           Nix.Monad
import           Nix.Monad.Instance
import           Nix.Utils

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: MonadNixEnv m => Maybe FilePath -> NExpr -> m (NValueNF m)
evalTopLevelExpr mdir expr = do
    base <- baseEnv
    (normalForm =<<) $ pushScopes base $ case mdir of
        Nothing -> evalExpr expr
        Just dir -> do
            traceM $ "Setting __cwd = " ++ show dir
            ref <- valueRef $ NVLiteralPath dir
            pushScope (Map.singleton "__cwd" ref) (evalExpr expr)

evalTopLevelExprIO :: Maybe FilePath -> NExpr -> IO (NValueNF (Cyclic IO))
evalTopLevelExprIO mdir expr =
    runReaderT (runCyclic (evalTopLevelExpr mdir expr)) []

tracingEvalTopLevelExprIO :: Maybe FilePath -> NExpr
                          -> IO (NValueNF (Cyclic IO))
tracingEvalTopLevelExprIO mdir expr = do
    traced <- tracingExprEval expr
    case mdir of
        Nothing ->
            runCyclicIO (normalForm =<< (`pushScopes` traced) =<< baseEnv)
        Just dir -> do
            traceM $ "Setting __cwd = " ++ show dir
            ref <- runCyclicIO (valueRef $ NVLiteralPath dir)
            let m = Map.singleton "__cwd" ref
            runCyclicIO (baseEnv >>= (`pushScopes` pushScope m traced)
                                 >>= normalForm)

lintExpr :: NExpr -> IO ()
lintExpr expr = runCyclicIO (baseEnv >>= (`pushScopes` checkExpr expr))
