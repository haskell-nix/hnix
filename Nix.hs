module Nix where

import           Control.Monad.Trans.Reader
import qualified Data.Map.Lazy as Map
import           Nix.Builtins
import           Nix.Eval
import           Nix.Expr (NExpr)
import           Nix.Lint
import           Nix.Monad
import           Nix.Monad.Instance
import           Nix.Scope
import           Nix.Utils

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: MonadNixEnv m => Maybe FilePath -> NExpr -> m (NValueNF m)
evalTopLevelExpr mdir expr = do
    base <- do
        base <- baseEnv
        case mdir of
            Nothing -> return base
            Just dir -> do
                ref <- buildThunk $ return $ NVLiteralPath dir
                let m = Map.singleton "__cwd" ref
                traceM $ "Setting __cwd = " ++ show dir
                return $ extendScope m base
    normalForm =<< pushScopes base (evalExpr expr)

evalTopLevelExprIO :: Maybe FilePath -> NExpr -> IO (NValueNF (Cyclic IO))
evalTopLevelExprIO mdir expr =
    runReaderT (runCyclic (evalTopLevelExpr mdir expr)) emptyScopes

tracingEvalTopLevelExprIO :: Maybe FilePath -> NExpr
                          -> IO (NValueNF (Cyclic IO))
tracingEvalTopLevelExprIO mdir expr = do
    base <- case mdir of
        Nothing -> run baseEnv emptyScopes
        Just dir -> do
            ref   <- run (buildThunk $ return $ NVLiteralPath dir) emptyScopes
            let m = Map.singleton "__cwd" ref
            traceM $ "Setting __cwd = " ++ show dir
            base <- run baseEnv emptyScopes
            return $ extendScope m base
    expr' <- tracingExprEval expr
    thnk  <- run expr' base
    run (normalForm thnk) base
  where
    run = runReaderT . runCyclic

lintExpr :: NExpr -> IO ()
lintExpr expr = run (checkExpr expr) =<< run baseEnv emptyScopes
  where
    run = runReaderT . runCyclic
