{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Builtins
    (baseEnv, builtins, Cyclic(..), NestedMap(..),
     evalTopLevelExpr, evalTopLevelExprIO,
     tracingEvalTopLevelExprIO, lintExpr)
    where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Fix
import           Data.IORef
import qualified Data.Map.Lazy as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (mapM)
import           Nix.Atoms
import           Nix.Eval
import           Nix.Expr (NExpr)
import           Nix.Parser
import           Nix.Utils
import           System.Process (readProcessWithExitCode)
import           System.Exit (ExitCode (ExitSuccess))

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: MonadNix m => NExpr -> m (NValueNF m)
evalTopLevelExpr expr = do
    base <- baseEnv
    normalForm =<< pushScopes base (evalExpr expr)

evalTopLevelExprIO :: NExpr -> IO (NValueNF (Cyclic IO))
evalTopLevelExprIO expr =
    runReaderT (runCyclic (evalTopLevelExpr expr)) emptyMap

tracingEvalTopLevelExprIO :: NExpr -> IO (NValueNF (Cyclic IO))
tracingEvalTopLevelExprIO expr = do
    base <- run baseEnv emptyMap
    expr' <- tracingExprEval expr
    thnk  <- run expr' base
    run (normalForm thnk) base
  where
    run = runReaderT . runCyclic

lintExpr :: NExpr -> IO ()
lintExpr expr = run (checkExpr expr) =<< run baseEnv emptyMap
  where
    run = runReaderT . runCyclic

baseEnv :: MonadNix m => m (NestedMap (NThunk m))
baseEnv = do
    ref <- buildThunk . NVSet =<< builtins
    lst <- (("builtins", ref) :) <$> topLevelBuiltins
    return . NestedMap . (:[]) $ Map.fromList lst
  where
    topLevelBuiltins = map mapping . filter isTopLevel <$> builtinsList

newtype Cyclic m a = Cyclic
    { runCyclic :: ReaderT (NestedMap (NThunk (Cyclic m))) m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

data Deferred m
    = DeferredAction (NestedMap (NThunk m)) (m (NThunk m))
    -- ^ This is closure over the environment where it was created.
    | ComputedValue (NValue m)

instance MonadNix (Cyclic IO) where
    -- jww (2018-03-29): We should use actually stacked scopes here, rather
    -- than constantly merging maps. The number of scope levels will usually
    -- be manageable, but the number of attributes within scopes can be
    -- enormous, making this one of the worst implementations.
    pushScopes s k = Cyclic $ local (combineMaps s) $ do
        scope <- runCyclic currentScope
        traceM $ "scope: " ++ show (() <$ scope)
        runCyclic k

    clearScopes  = Cyclic . local (const (NestedMap [])) . runCyclic
    currentScope = Cyclic ask
    lookupVar k  = Cyclic $ nestedLookup k <$> ask

    -- jww (2018-03-29): Cache which files have been read in.
    importFile path = forceThunk path >>= \case
        NVLiteralPath path -> Cyclic $ do
            traceM $ "Importing file " ++ path
            eres <- parseNixFile path
            case eres of
                Failure err  -> error $ "Parse failed: " ++ show err
                Success expr -> runCyclic $ evalExpr expr
        p -> error $ "Unexpected argument to import: " ++ show (() <$ p)

    addPath path = liftIO $ do
        (exitCode, out, _) <-
            readProcessWithExitCode "nix-store" ["--add", path] ""
        case exitCode of
          ExitSuccess -> return $ StorePath out
          _ -> error $ "No such file or directory: " ++ show path

    data NThunk (Cyclic IO) =
        NThunkIO (Either (NValueNF (Cyclic IO))
                         (IORef (Deferred (Cyclic IO))))

    valueRef = return . NThunkIO . Left

    buildThunk action =
        liftIO $ NThunkIO . Right <$> newIORef (ComputedValue action)

    defer scope action = do
        traceM $ "Deferring action in scope: " ++ show (() <$ scope)
        liftIO $ NThunkIO . Right <$> newIORef (DeferredAction scope action)

    forceThunk (NThunkIO (Left value)) =
        return $ NThunkIO . Left <$> unFix value

    forceThunk (NThunkIO (Right ref)) = do
        eres <- liftIO $ readIORef ref
        case eres of
            ComputedValue value -> return value
            DeferredAction scope action -> do
                traceM $ "Forcing thunk in scope: " ++ show scope
                value <- Cyclic
                    $ local (`combineMaps` scope)
                    $ runCyclic
                    $ forceThunk =<< action
                traceM $ "Forcing thunk computed: " ++ show (() <$ value)
                liftIO $ writeIORef ref (ComputedValue value)
                return value

builtins :: MonadNix m => m (ValueSet m)
builtins = Map.fromList . map mapping <$> builtinsList

data BuiltinType = Normal | TopLevel
data Builtin m = Builtin
    { kind    :: BuiltinType
    , mapping :: (Text, NThunk m)
    }

isTopLevel :: Builtin m -> Bool
isTopLevel b = case kind b of Normal -> False; TopLevel -> True

builtinsList :: MonadNix m => m [ Builtin m ]
builtinsList = sequence [
      add  TopLevel "toString" toString
    , add  TopLevel "import"   import_
    , add2 Normal   "hasAttr"  hasAttr
    , add2 Normal   "getAttr"  getAttr
    , add2 Normal   "any"      any_
    , add2 Normal   "all"      all_
  ]
  where
    add  t n v = (\f -> Builtin t (n, f)) <$> builtin (Text.unpack n) v
    add2 t n v = (\f -> Builtin t (n, f)) <$> builtin2 (Text.unpack n) v

-- Helpers

mkBool :: MonadNix m => Bool -> m (NThunk m)
mkBool = valueRef . Fix . NVConstant . NBool

extractBool :: MonadNix m => NThunk m -> m Bool
extractBool arg = forceThunk arg >>= \case
    NVConstant (NBool b) -> return b
    _ -> error "Not a boolean constant"

evalPred :: MonadNix m => NThunk m -> NThunk m -> m (NThunk m)
evalPred f arg = forceThunk f >>= \case
    NVFunction params pred ->
        (`pushScope` pred) =<< buildArgument params arg
    x -> error $ "Trying to call a " ++ show (() <$ x)

-- Primops

toString :: MonadNix m => NThunk m -> m (NThunk m)
toString = valueRef . uncurry ((Fix .) . NVStr) <=< valueText <=< normalForm

import_ :: MonadNix m => NThunk m -> m (NThunk m)
import_ = importFile

hasAttr :: MonadNix m => NThunk m -> NThunk m -> m (NThunk m)
hasAttr x y = (,) <$> forceThunk x <*> forceThunk y >>= \case
    (NVStr key _, NVSet aset) ->
        valueRef $ Fix . NVConstant . NBool $ Map.member key aset
    (x, y) -> error $ "Invalid types for builtin.hasAttr: "
                 ++ show (() <$ x, () <$ y)

getAttr :: MonadNix m => NThunk m -> NThunk m -> m (NThunk m)
getAttr x y = (,) <$> forceThunk x <*> forceThunk y >>= \case
    (NVStr key _, NVSet aset) ->
        return $ Map.findWithDefault _err key aset
          where _err = error ("Field does not exist " ++ Text.unpack key)
    (x, y) -> error $ "Invalid types for builtin.hasAttr: "
                 ++ show (() <$ x, () <$ y)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = return False
anyM p (x:xs)   = do
        q <- p x
        if q then return True
             else anyM p xs

any_ :: MonadNix m => NThunk m -> NThunk m -> m (NThunk m)
any_ pred arg = forceThunk arg >>= \case
    NVList l ->
        mkBool =<< anyM extractBool =<< mapM (evalPred pred) l
    arg -> error $ "builtins.any takes a list as second argument, not a "
              ++ show (() <$ arg)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x:xs)   = do
        q <- p x
        if q then allM p xs
             else return False

all_ :: MonadNix m => NThunk m -> NThunk m -> m (NThunk m)
all_ pred arg = forceThunk arg >>= \case
    NVList l ->
        mkBool =<< allM extractBool =<< mapM (evalPred pred) l
    arg -> error $ "builtins.all takes a list as second argument, not a "
              ++ show (() <$ arg)
