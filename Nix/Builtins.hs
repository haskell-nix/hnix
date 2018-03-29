{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nix.Builtins (baseEnv, builtins,
                     Cyclic(..), evalTopLevelExpr, evalTopLevelExprIO) where

import           Control.Monad.Trans.State
import           Data.Fix
import           Data.Functor.Identity
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (mapM)
import           Nix.Atoms
import           Nix.Eval
import           Nix.Expr (NExpr)
import           Nix.Parser
import           System.IO.Unsafe

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: MonadNix m => NExpr -> m (NValue m)
evalTopLevelExpr = pushScope baseEnv . evalExpr

baseEnv :: MonadNix m => ValueSet m
baseEnv = fmap pure
        . Map.fromList
        $ ("builtins", Fix $ NVSet builtins) : topLevelBuiltins
  where
    topLevelBuiltins = map mapping (filter isTopLevel builtinsList)

newtype Cyclic m a = Cyclic { runCyclic :: StateT (ValueSet (Cyclic m)) m a }
    deriving (Functor, Applicative, Monad)

instance MonadNix (Cyclic Identity) where
    -- currentScope = Cyclic get
    -- newScope s k = Cyclic $ put s >> runCyclic k
    pushScope s k = Cyclic $ modify (s `Map.union`) >> runCyclic k
    lookupVar k = Cyclic $ do
        s <- get
        case Map.lookup k s of
            Nothing -> return Nothing
            Just v -> Just <$> runCyclic v
    importFile path = Cyclic $ case path of
        Fix (NVLiteralPath path) ->
            let eres = unsafePerformIO $ parseNixFile path
            in case eres of
                Failure err  -> error $ "Parse failed: " ++ show err
                Success expr -> runCyclic $ evalExpr expr
        _ -> error $ "Unexpected argument to import: " ++ show path

instance MonadNix (Cyclic IO) where
    -- currentScope = Cyclic get
    -- newScope s k = Cyclic $ put s >> runCyclic k
    pushScope s k = Cyclic $ modify (s `Map.union`) >> runCyclic k
    lookupVar k = Cyclic $ do
        s <- get
        case Map.lookup k s of
            Nothing -> return Nothing
            Just v -> Just <$> runCyclic v
    importFile path = Cyclic $ case path of
        Fix (NVLiteralPath path) -> do
            eres <- parseNixFile path
            case eres of
                Failure err  -> error $ "Parse failed: " ++ show err
                Success expr -> runCyclic $ evalExpr expr
        _ -> error $ "Unexpected argument to import: " ++ show path

evalTopLevelExprIO :: NExpr -> IO (NValue (Cyclic IO))
evalTopLevelExprIO expr =
    evalStateT (runCyclic (evalTopLevelExpr expr)) Map.empty

builtins :: MonadNix m => Map.Map Text (NValue m)
builtins = Map.fromList $ map mapping builtinsList

data BuiltinType = Normal | TopLevel
data Builtin m = Builtin {kind :: BuiltinType, mapping :: (Text, NValue m) }

isTopLevel :: Builtin m -> Bool
isTopLevel b = case kind b of
                    Normal -> False
                    TopLevel -> True

builtinsList :: MonadNix m => [ Builtin m ]
builtinsList = [
      topLevel ("toString", prim_toString)
    , topLevel ("import"  , prim_import)
    , basic    ("hasAttr" , prim_hasAttr)
    , basic    ("getAttr" , prim_getAttr)
    , basic    ("any"     , prim_any )
    , basic    ("all"     , prim_all )
  ]
  where
    basic = Builtin Normal
    topLevel = Builtin TopLevel

-- Helpers

mkBool :: Bool -> NValue m
mkBool = Fix . NVConstant . NBool

extractBool :: NValue m -> Bool
extractBool (Fix (NVConstant (NBool b))) = b
extractBool _                            = error "Not a bool constant"

evalPred :: MonadNix m => NValue m -> NValue m -> m (NValue m)
evalPred (Fix (NVFunction params pred)) arg =
    (`pushScope` pred) =<< buildArgument params arg
evalPred pred _ = error $ "Trying to call a " ++ show pred

-- Primops

prim_toString :: MonadNix m => Functor m => NValue m
prim_toString = builtin "toString" toString
toString :: MonadNix m => NValue m -> m (NValue m)
toString s = return $ Fix $ uncurry NVStr $ valueText s

prim_import :: MonadNix m => Functor m => NValue m
prim_import = builtin "import" import_
import_ :: MonadNix m => NValue m -> m (NValue m)
import_ = importFile

prim_hasAttr :: MonadNix m => NValue m
prim_hasAttr = builtin2 "hasAttr" hasAttr
hasAttr :: MonadNix m => NValue m -> NValue m -> m (NValue m)
hasAttr (Fix (NVStr key _)) (Fix (NVSet aset)) =
    return $ Fix $ NVConstant $ NBool $ Map.member key aset
hasAttr key aset =
    error $ "Invalid types for builtin.hasAttr: " ++ show (key, aset)

prim_getAttr :: MonadNix m => NValue m
prim_getAttr = builtin2 "getAttr" getAttr
getAttr :: MonadNix m => NValue m -> NValue m -> m (NValue m)
getAttr (Fix (NVStr key _)) (Fix (NVSet aset)) =
    return $ Map.findWithDefault _err key aset
  where _err = error ("Field does not exist " ++ Text.unpack key)
getAttr key aset =
    error $ "Invalid types for builtin.getAttr: " ++ show (key, aset)

prim_any :: MonadNix m => NValue m
prim_any = builtin2 "any" _any
_any :: MonadNix m => NValue m -> NValue m -> m (NValue m)
_any pred (Fix (NVList l)) =
    mkBool . any extractBool <$> mapM (evalPred pred) l
_any _ list =
    error $ "builtins.any takes a list as second argument, not a " ++ show list

prim_all :: MonadNix m => NValue m
prim_all = builtin2 "all" _all
_all :: MonadNix m => NValue m -> NValue m -> m (NValue m)
_all pred (Fix (NVList l)) =
    mkBool . all extractBool <$> mapM (evalPred pred) l
_all _ list =
    error $ "builtins.all takes a list as second argument, not a " ++ show list
