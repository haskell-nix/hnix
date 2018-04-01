{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

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
            ref <- valueRef $ NVLiteralPath dir
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
            ref <- runLazyIO (valueRef $ NVLiteralPath dir)
            let m = M.singleton "__cwd" ref
            runLazyIO (baseEnv >>= (`pushScopes` pushScope m traced)
                                 >>= normalForm)

newtype Lint m a = Lint
    { runLint :: ReaderT (Context Symbolic) m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO,
              MonadReader (Context Symbolic))

runLintIO :: Lint IO a -> IO a
runLintIO = flip runReaderT (Context emptyScopes []) . runLint

symbolicBaseEnv :: Monad m => m (Scopes Symbolic)
symbolicBaseEnv = return [Scope M.empty False]

lintExprIO :: NExprLoc -> IO Symbolic
lintExprIO expr =
    runLintIO (symbolicBaseEnv
                   >>= (`pushScopes` lintExpr (stripAnnotation expr)))
