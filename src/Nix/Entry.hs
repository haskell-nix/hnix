{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Nix.Entry where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Fix
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import           Nix.Builtins
import           Nix.Effects
import qualified Nix.Eval as Eval
import           Nix.Expr.Shorthands
import           Nix.Expr.Types (NExpr)
import           Nix.Expr.Types.Annotated (NExprLoc, stripAnnotation)
import           Nix.Normal
import           Nix.Options
import           Nix.Parser
import           Nix.Parser.Library (Result(..))
import           Nix.Pretty
import           Nix.Scope
import           Nix.Stack
import           Nix.Thunk
import qualified Nix.Trace as Trace
import           Nix.Utils
import           Nix.Value

type MonadNix e m =
    (Scoped e (NThunk m) m, Framed e m, MonadVar m, MonadFile m,
     MonadEffects m, MonadFix m, MonadCatch m)

-- | Evaluate a nix expression in the default context
evalTopLevelExprGen :: forall e m a r. MonadNix e m
                    => (a -> m r) -> Maybe FilePath -> a -> m r
evalTopLevelExprGen cont mpath expr = do
    base <- baseEnv
    opts :: Options <- asks (view hasLens)
    let i = value @(NValue m) @(NThunk m) @m $ NVList $
            map (value @(NValue m) @(NThunk m) @m
                     . flip NVStr mempty . Text.pack) (include opts)
    pushScope (M.singleton "__includes" i) $
        pushScopes base $ case mpath of
            Nothing -> cont expr
            Just path -> do
                traceM $ "Setting __cur_file = " ++ show path
                let ref = value @(NValue m) @(NThunk m) @m $ NVPath path
                pushScope (M.singleton "__cur_file" ref) $ cont expr

-- | Evaluate a nix expression in the default context
eval :: forall e m. MonadNix e m
     => Maybe FilePath -> NExpr -> m (NValue m)
eval = evalTopLevelExprGen $
    Eval.evalExpr @_ @(NValue m) @(NThunk m) @m

-- | Evaluate a nix expression in the default context
evalLoc :: forall e m. MonadNix e m
        => Maybe FilePath -> NExprLoc -> m (NValue m)
evalLoc = evalTopLevelExprGen $
    Eval.framedEvalExpr (Eval.eval @_ @(NValue m) @(NThunk m) @m)

tracingEvalLoc
    :: forall e m. (MonadNix e m, Alternative m, MonadIO m)
    => Maybe FilePath -> NExprLoc -> m (NValue m)
tracingEvalLoc mpath expr = do
    (expr', eres) <- evalTopLevelExprGen id mpath
        =<< Trace.tracingEvalExpr @_ @m @SomeException @_ @(NValue m)
                (Eval.eval @_ @(NValue m)
                           @(NThunk m) @m) mpath expr
    liftIO $ do
        putStrLn "Expression tree before winnowing:"
        putStrLn "--------"
        print $ prettyNix (stripAnnotation expr)
        putStrLn "--------"
        putStrLn "Expression tree after winnowing:"
        putStrLn "--------"
        print $ prettyNix (stripAnnotation expr')
        putStrLn "--------"
    case eres of
        Left err -> throwM err
        Right v  -> return v

evaluateExpression
    :: forall e m a. MonadNix e m
    => Maybe FilePath
    -> (Maybe FilePath -> NExprLoc -> m (NValue m))
    -> (NValue m -> m a)
    -> NExprLoc
    -> m a
evaluateExpression mpath evaluator handler expr = do
    opts :: Options <- asks (view hasLens)
    args <- traverse (traverse eval') $
        map (second parseArg) (arg opts) ++
        map (second mkStr) (argstr opts)
    compute evaluator expr (argmap args) handler
  where
    parseArg s = case parseNixText s of
        Success x -> x
        Failure err -> errorWithoutStackTrace (show err)

    eval' = (normalForm =<<) . eval mpath

    argmap args = embed $ Fix $ NVSet (M.fromList args) mempty

    compute ev x args p = do
         f <- ev mpath x
         processResult p =<< case f of
             NVClosure _ g -> g args
             _ -> pure f

processResult :: forall e m a. MonadNix e m
              => (NValue m -> m a) -> NValue m -> m a
processResult h val = do
    opts :: Options <- asks (view hasLens)
    case attr opts of
        Nothing -> h val
        Just (Text.splitOn "." -> keys) -> go keys val
  where
    go :: [Text.Text] -> NValue m -> m a
    go [] v = h v
    go ((Text.decimal -> Right (n,"")):ks) v = case v of
        NVList xs -> case ks of
            [] -> force @(NValue m) @(NThunk m) (xs !! n) h
            _  -> force (xs !! n) (go ks)
        _ -> errorWithoutStackTrace $
                "Expected a list for selector '" ++ show n
                    ++ "', but got: " ++ show v
    go (k:ks) v = case v of
        NVSet xs _ -> case M.lookup k xs of
            Nothing ->
                errorWithoutStackTrace $
                    "Set does not contain key '"
                        ++ Text.unpack k ++ "'"
            Just v' -> case ks of
                [] -> force v' h
                _  -> force v' (go ks)
        _ -> errorWithoutStackTrace $
            "Expected a set for selector '" ++ Text.unpack k
                ++ "', but got: " ++ show v
