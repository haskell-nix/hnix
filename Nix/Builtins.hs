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
import           Data.Foldable (foldlM)
import           Data.Traversable (mapM)
import           Nix.Atoms
import           Nix.Eval
import           Nix.Expr (NExpr)
import           Nix.Parser
import           Nix.Utils
import           System.Environment
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath
import           System.Process (readProcessWithExitCode)

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: MonadNix m => Maybe FilePath -> NExpr -> m (NValueNF m)
evalTopLevelExpr mdir expr = do
    base <- do
        base <- baseEnv
        case mdir of
            Nothing -> return base
            Just dir -> do
                ref <- valueRef $ Fix $ NVLiteralPath dir
                let m = Map.singleton "__cwd" ref
                traceM $ "Setting __cwd = " ++ show dir
                return $ extendMap m base
    normalForm =<< pushScopes base (evalExpr expr)

evalTopLevelExprIO :: Maybe FilePath -> NExpr -> IO (NValueNF (Cyclic IO))
evalTopLevelExprIO mdir expr =
    runReaderT (runCyclic (evalTopLevelExpr mdir expr)) emptyMap

tracingEvalTopLevelExprIO :: Maybe FilePath -> NExpr
                          -> IO (NValueNF (Cyclic IO))
tracingEvalTopLevelExprIO mdir expr = do
    base <- case mdir of
        Nothing -> run baseEnv emptyMap
        Just dir -> do
            ref   <- run (valueRef $ Fix $ NVLiteralPath dir) emptyMap
            let m = Map.singleton "__cwd" ref
            traceM $ "Setting __cwd = " ++ show dir
            base <- run baseEnv emptyMap
            return $ extendMap m base
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
    = DeferredAction (m (NThunk m))
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
    importFile path = normalForm path >>= \case
        Fix (NVLiteralPath path) -> do
            mres <- lookupVar "__cwd"
            path' <- case mres of
                Nothing  -> do
                    traceM "No known current directory"
                    return path
                Just dir -> normalForm dir >>= \case
                    Fix (NVLiteralPath dir') -> do
                        traceM $ "Current directory for import is: "
                            ++ show dir'
                        return $ dir' </> path
                    x -> error $ "How can the current directory be: " ++ show x
            traceM $ "Importing file " ++ path'
            eres <- Cyclic $ parseNixFile path'
            case eres of
                Failure err  -> error $ "Parse failed: " ++ show err
                Success expr -> do
                    ref <- valueRef $ Fix $ NVLiteralPath $ takeDirectory path'
                    -- Use this cookie so that when we evaluate the next
                    -- import, we'll remember which directory its containing
                    -- file was in.
                    pushScope (Map.singleton "__cwd" ref)
                              (evalExpr expr)
        p -> error $ "Unexpected argument to import: " ++ show p

    addPath path = liftIO $ do
        (exitCode, out, _) <-
            readProcessWithExitCode "nix-store" ["--add", path] ""
        case exitCode of
          ExitSuccess -> return $ StorePath out
          _ -> error $ "No such file or directory: " ++ show path

    getEnvVar name = normalForm name >>= \case
        Fix (NVStr s _) -> do
            mres <- liftIO $ lookupEnv (Text.unpack s)
            case mres of
                Nothing -> valueRef $ Fix $ NVStr "" mempty
                Just v  -> valueRef $ Fix $ NVStr (Text.pack v) mempty
        p -> error $ "Unexpected argument to getEnv: " ++ show p

    data NThunk (Cyclic IO) =
        NThunkIO (Either (NValueNF (Cyclic IO))
                         (IORef (Deferred (Cyclic IO))))

    valueRef = return . NThunkIO . Left

    buildThunk action =
        liftIO $ NThunkIO . Right <$> newIORef (ComputedValue action)

    defer action =
        liftIO $ NThunkIO . Right <$> newIORef (DeferredAction action)

    forceThunk (NThunkIO (Left value)) =
        return $ NThunkIO . Left <$> unFix value

    forceThunk (NThunkIO (Right ref)) = do
        eres <- liftIO $ readIORef ref
        case eres of
            ComputedValue value -> return value
            DeferredAction action -> do
                scope <- currentScope
                traceM $ "Forcing thunk in scope: " ++ show scope
                value <- forceThunk =<< action
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

    , add  Normal   "getEnv"   getEnv_
    , add2 Normal   "hasAttr"  hasAttr
    , add2 Normal   "getAttr"  getAttr
    , add2 Normal   "any"      any_
    , add2 Normal   "all"      all_
    , add3 Normal   "foldl'"   foldl'_
    , add  Normal   "head"     head_
  ]
  where
    add  t n v = (\f -> Builtin t (n, f)) <$> builtin (Text.unpack n) v
    add2 t n v = (\f -> Builtin t (n, f)) <$> builtin2 (Text.unpack n) v
    add3 t n v = (\f -> Builtin t (n, f)) <$> builtin3 (Text.unpack n) v

-- Helpers

mkBool :: MonadNix m => Bool -> m (NThunk m)
mkBool = valueRef . Fix . NVConstant . NBool

extractBool :: MonadNix m => NThunk m -> m Bool
extractBool arg = forceThunk arg >>= \case
    NVConstant (NBool b) -> return b
    _ -> error "Not a boolean constant"

apply :: MonadNix m => NThunk m -> NThunk m -> m (NThunk m)
apply f arg = forceThunk f >>= \case
    NVFunction params pred ->
        (`pushScope` pred) =<< buildArgument params arg
    x -> error $ "Trying to call a " ++ show (() <$ x)

-- Primops

toString :: MonadNix m => NThunk m -> m (NThunk m)
toString = valueRef . uncurry ((Fix .) . NVStr) <=< valueText <=< normalForm

import_ :: MonadNix m => NThunk m -> m (NThunk m)
import_ = importFile

getEnv_ :: MonadNix m => NThunk m -> m (NThunk m)
getEnv_ = getEnvVar

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
        mkBool =<< anyM extractBool =<< mapM (apply pred) l
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
        mkBool =<< allM extractBool =<< mapM (apply pred) l
    arg -> error $ "builtins.all takes a list as second argument, not a "
              ++ show (() <$ arg)

--TODO: Strictness
foldl'_ :: MonadNix m => NThunk m -> NThunk m -> NThunk m -> m (NThunk m)
foldl'_ f z l = forceThunk l >>= \case
    NVList vals ->
        foldlM (\b a -> (f `apply` b) >>= (`apply` a)) z vals
    arg -> error $ "builtins.foldl' takes a list as third argument, not a "
              ++ show (() <$ arg)

head_ :: MonadNix m => NThunk m -> m (NThunk m)
head_ arg = forceThunk arg >>= \case
    NVList vals -> case vals of
        [] -> error "builtins.head: empty list"
        h:_ -> return h
    _ -> error "builtins.head: not a list"
