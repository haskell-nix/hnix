{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Nix where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Trans.Reader
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
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
        Nothing -> framedEvalExpr eval expr
        Just dir -> do
            traceM $ "Setting __cwd = " ++ show dir
            ref <- valueThunk $ NVLiteralPath dir
            pushScope (M.singleton "__cwd" ref)
                      (framedEvalExpr eval expr)

evalTopLevelExprIO :: Maybe FilePath -> NExprLoc -> IO (NValueNF (Lazy IO))
evalTopLevelExprIO mdir = runLazyIO . evalTopLevelExpr mdir

-- informativeEvalTopLevelExprIO :: Maybe FilePath -> NExpr
--                               -> IO (NValueNF (Lazy IO))
-- informativeEvalTopLevelExprIO mdir expr =
--     runReaderT (runLazy (evalTopLevelExpr mdir expr)) []

tracingEvalTopLevelExprIO :: Maybe FilePath -> NExprLoc
                          -> IO (NValueNF (Lazy IO))
tracingEvalTopLevelExprIO mdir expr = do
    traced <- tracingEvalExpr eval expr
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
    { runLint :: ReaderT (Context (SThunk (Lint m))) m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO,
              MonadReader (Context (SThunk (Lint m))))

instance (MonadFix m, MonadIO m)
      => MonadEval (SThunk (Lint m)) (Symbolic (Lint m)) (Lint m) where
    wrapThunk   = SThunk
    unwrapThunk = getSThunk

    embedSet s = mkSymbolic [TSet (Just s)]
    projectSet = unpackSymbolic >=> \case
        NMany [TSet s] -> return s
        _ -> return Nothing

    type MText (Lint m) = Text

    wrapText   = return
    unwrapText = return

    embedText   = const $ mkSymbolic [TStr]
    projectText = const $ return Nothing

runLintIO :: Lint IO a -> IO a
runLintIO = flip runReaderT (Context emptyScopes []) . runLint

symbolicBaseEnv :: Monad m => m (Scopes (SThunk m))
symbolicBaseEnv = return [Scope M.empty False]

lintExprIO :: NExprLoc -> IO (Symbolic (Lint IO))
lintExprIO expr =
    runLintIO (symbolicBaseEnv
                   >>= (`pushScopes` lintExpr (stripAnnotation expr)))

tracingLintExprIO :: NExprLoc -> IO (Symbolic (Lint IO))
tracingLintExprIO expr = do
    traced <- tracingEvalExpr lint expr
    ref <- runLintIO $ sthunk $ mkSymbolic [TPath]
    let m = M.singleton "__cwd" ref
    runLintIO (symbolicBaseEnv >>= (`pushScopes` pushScope m traced))
