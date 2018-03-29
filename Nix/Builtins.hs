{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Builtins
    (baseEnv, builtins, Cyclic(..), evalTopLevelExpr, evalTopLevelExprIO)
    where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
-- import           Data.Fix
-- import           Data.Functor.Identity
import           Data.IORef
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (mapM)
import           Nix.Atoms
import           Nix.Eval
import           Nix.Expr (NExpr)
import           Nix.Parser
-- import           System.IO.Unsafe

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: MonadNix m => NExpr -> m (NValueNF m)
evalTopLevelExpr = normalForm <=< pushScope baseEnv . evalExpr

evalTopLevelExprIO :: NExpr -> IO (NValueNF (Cyclic IO))
evalTopLevelExprIO expr =
    evalStateT (runCyclic (evalTopLevelExpr expr)) Map.empty

baseEnv :: MonadNix m => ValueSet m
baseEnv = Map.fromList
    $ ("builtins", valueRef $ NVSet builtins) : topLevelBuiltins
  where
    topLevelBuiltins = map mapping (filter isTopLevel builtinsList)

newtype Cyclic m a = Cyclic { runCyclic :: StateT (ValueSet (Cyclic m)) m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadNix (Cyclic IO) where
    -- jww (2018-03-29): We should use actually stacked scopes here, rather
    -- than constantly merging maps. The number of scope levels will usually
    -- be manageable, but the number of attributes within scopes can be
    -- enormous, making this one of the worst implementations.
    pushScope s k = Cyclic $ modify (s `Map.union`) >> runCyclic k

    lookupVar k = Cyclic $ do
        s <- get
        case Map.lookup k s of
            Nothing -> return Nothing
            Just v -> Just <$> runCyclic v

    -- jww (2018-03-29): Cache which files have been read in.
    importFile path = forceThunk path >>= \case
        NVLiteralPath path -> Cyclic $ do
            liftIO $ putStrLn $ "Importing file " ++ path
            eres <- parseNixFile path
            case eres of
                Failure err  -> error $ "Parse failed: " ++ show err
                Success expr -> runCyclic $ evalExpr expr
        p -> error $ "Unexpected argument to import: " ++ show (() <$ p)

    data NThunk (Cyclic IO) =
        NThunkIO (IORef (Either (Cyclic IO (NValue (Cyclic IO)))
                                (NValue (Cyclic IO))))

    buildThunk action = liftIO $ NThunkIO <$> newIORef (Left action)
    valueRef   value  = liftIO $ NThunkIO <$> newIORef (Right value)

    forceThunk (NThunkIO ref) = do
        eres <- liftIO $ readIORef ref
        case eres of
            Right value -> return value
            Left action -> do
                value <- action
                liftIO $ writeIORef ref (Right value)
                return value

builtins :: MonadNix m => ValueSet m
builtins = Map.fromList $ map mapping builtinsList

data BuiltinType = Normal | TopLevel
data Builtin m = Builtin
    { kind    :: BuiltinType
    , mapping :: (Text, m (NThunk m))
    }

isTopLevel :: Builtin m -> Bool
isTopLevel b = case kind b of Normal -> False; TopLevel -> True

builtinsList :: MonadNix m => [ Builtin m ]
builtinsList = [
      add  TopLevel "toString" toString
    , add  TopLevel "import"   import_
    , add2 Normal   "hasAttr"  hasAttr
    , add2 Normal   "getAttr"  getAttr
    , add2 Normal   "any"      any_
    , add2 Normal   "all"      all_
  ]
  where
    add  t n v = Builtin t (n, builtin  (Text.unpack n) v)
    add2 t n v = Builtin t (n, builtin2 (Text.unpack n) v)

-- Helpers

mkBool :: MonadNix m => Bool -> m (NThunk m)
mkBool = valueRef . NVConstant . NBool

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
toString = valueRef . uncurry NVStr . valueText <=< normalForm

import_ :: MonadNix m => NThunk m -> m (NThunk m)
import_ = importFile

hasAttr :: MonadNix m => NThunk m -> NThunk m -> m (NThunk m)
hasAttr x y = (,) <$> forceThunk x <*> forceThunk y >>= \case
    (NVStr key _, NVSet aset) ->
        valueRef $ NVConstant . NBool $ Map.member key aset
    (x, y) -> error $ "Invalid types for builtin.hasAttr: "
                 ++ show (() <$ x, () <$ y)

getAttr :: MonadNix m => NThunk m -> NThunk m -> m (NThunk m)
getAttr x y = (,) <$> forceThunk x <*> forceThunk y >>= \case
    (NVStr key _, NVSet aset) ->
        Map.findWithDefault _err key aset
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
        mkBool =<< anyM extractBool =<< mapM (evalPred pred) =<< sequence l
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
        mkBool =<< allM extractBool =<< mapM (evalPred pred) =<< sequence l
    arg -> error $ "builtins.all takes a list as second argument, not a "
              ++ show (() <$ arg)
