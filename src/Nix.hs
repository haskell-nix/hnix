{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix (eval, evalLoc, tracingEvalLoc, lint, runLintM) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as M
import           Data.STRef
import           Data.Text (Text)
import           Nix.Builtins
import qualified Nix.Eval as Eval
import           Nix.Eval hiding (eval)
import           Nix.Expr.Types (NExpr)
import           Nix.Expr.Types.Annotated (NExprLoc)
import qualified Nix.Lint as Lint
import           Nix.Lint hiding (lint)
import           Nix.Monad.Instance
import           Nix.Scope
import           Nix.Stack
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: forall e m. MonadBuiltins e m
                 => Maybe FilePath -> NExpr -> m (NValueNF m)
evalTopLevelExpr mpath expr = do
    base <- baseEnv
    (normalForm =<<) $ pushScopes base $ case mpath of
        Nothing -> Eval.evalExpr expr
        Just path -> do
            traceM $ "Setting __cur_file = " ++ show path
            let ref = valueThunk @m $ NVPath path
            pushScope (M.singleton "__cur_file" ref)
                      (Eval.evalExpr expr)

eval :: (MonadFix m, MonadThrow m, MonadCatch m, MonadIO m)
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
            let ref = valueThunk @m $ NVPath path
            pushScope (M.singleton "__cur_file" ref)
                      (framedEvalExpr Eval.eval expr)

evalLoc :: (MonadFix m, MonadThrow m, MonadCatch m, MonadIO m)
        => Maybe FilePath -> NExprLoc -> m (NValueNF (Lazy m))
evalLoc mpath = runLazyM . evalTopLevelExprLoc mpath

tracingEvalLoc
    :: forall m. (MonadFix m, MonadThrow m, MonadCatch m, MonadIO m, Alternative m)
    => Maybe FilePath -> NExprLoc -> m (NValueNF (Lazy m))
tracingEvalLoc mpath expr = do
    traced <- tracingEvalExpr Eval.eval expr
    case mpath of
        Nothing ->
            runLazyM (normalForm =<< (`pushScopes` traced) =<< baseEnv)
        Just path -> do
            traceM $ "Setting __cur_file = " ++ show path
            let ref = valueThunk @(Lazy m) $ NVPath path
            let m = M.singleton "__cur_file" ref
            runLazyM (baseEnv >>= (`pushScopes` pushScope m traced)
                                 >>= normalForm)

newtype Lint s a = Lint
    { runLint :: ReaderT (Context (Lint s) (SThunk (Lint s))) (ST s) a }
    deriving (Functor, Applicative, Monad, MonadFix,
              MonadReader (Context (Lint s) (SThunk (Lint s))))

instance MonadVar (Lint s) where
    type Var (Lint s) = STRef s

    newVar x     = Lint $ ReaderT $ \_ -> newSTRef x
    readVar x    = Lint $ ReaderT $ \_ -> readSTRef x
    writeVar x y = Lint $ ReaderT $ \_ -> writeSTRef x y
    atomicModifyVar x f = Lint $ ReaderT $ \_ -> do
        res <- snd . f <$> readSTRef x
        _ <- modifySTRef x (fst . f)
        return res

instance MonadFile (Lint s) where
    readFile x = Lint $ ReaderT $ \_ -> unsafeIOToST $ BS.readFile x

instance MonadThrow (Lint s) where
    throwM e = Lint $ ReaderT $ \_ -> unsafeIOToST $ throw e

instance Eval.MonadExpr (SThunk (Lint s))
             (STRef s (NSymbolicF (NTypeF (Lint s) (SThunk (Lint s)))))
             (Lint s) where
    embedSet s = mkSymbolic [TSet (Just s)]
    projectSet = unpackSymbolic >=> \case
        NMany [TSet s] -> return s
        _ -> return Nothing
    projectSetWithPos = unpackSymbolic >=> \case
        NMany [TSet s] -> return $ (, M.empty) <$> s
        _ -> return Nothing

    type MText (Lint s) = Text

    wrapText   = return
    unwrapText = return

    embedText   = const $ mkSymbolic [TStr]
    projectText = const $ return Nothing

runLintM :: Lint s a -> ST s a
runLintM = flip runReaderT (Context emptyScopes []) . runLint

symbolicBaseEnv :: Monad m => m (Scopes m (SThunk m))
symbolicBaseEnv = return []     -- jww (2018-04-02): TODO

lint :: NExpr -> ST s (Symbolic (Lint s))
lint expr = runLintM $ symbolicBaseEnv
    >>= (`pushScopes` Lint.lintExpr expr)
