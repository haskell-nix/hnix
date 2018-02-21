
module Nix.Builtins (baseEnv, builtins, evalTopLevelExpr) where

import           Control.Applicative
import           Control.Monad hiding (mapM, sequence)
import           Control.Monad.Fix
import           Data.Fix
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (mapM)
import           Nix.Atoms
import           Nix.Eval
import           Nix.Expr (NExpr)
import           Prelude hiding (mapM, sequence)

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: MonadFix m => NExpr -> m (NValue m)
evalTopLevelExpr val = evalExpr val baseEnv

baseEnv :: MonadFix m => ValueSet m
baseEnv = Map.fromList $ [ ("builtins", Fix $ NVSet builtins) ] ++ topLevelBuiltins
  where
    topLevelBuiltins = map mapping $ filter isTopLevel builtinsList
    -- builtins = Map.fromList $ map mapping $ builtinsList

builtins :: MonadFix m => ValueSet m
builtins = Map.fromList $ map mapping $ builtinsList

data BuiltinType = Normal | TopLevel
data Builtin m = Builtin {kind :: BuiltinType, mapping :: (Text, NValue m) }

isTopLevel :: Builtin m -> Bool
isTopLevel b = case kind b of
                    Normal -> False
                    TopLevel -> True

builtinsList :: MonadFix m => [ Builtin m ]
builtinsList = [
      topLevel ("toString", prim_toString)
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

evalPred :: NValue m -> NValue m -> m (NValue m)
evalPred (Fix (NVFunction argset pred)) = pred . buildArgument argset
evalPred pred = error $ "Trying to call a " ++ show pred


-- Primops

prim_toString :: MonadFix m => Functor m => NValue m
prim_toString = Fix $ NVBuiltin1 "toString" $ toString
toString :: MonadFix m => NValue m -> m (NValue m)
toString s = return $ Fix $ uncurry NVStr $ valueText s

prim_hasAttr :: MonadFix m => NValue m
prim_hasAttr = Fix $ NVBuiltin2 "hasAttr" [] hasAttr
hasAttr :: MonadFix m => NValue m -> NValue m -> m (NValue m)
hasAttr (Fix (NVStr key _)) (Fix (NVSet aset)) = return $ Fix $ NVConstant $ NBool $ Map.member key aset
hasAttr key aset = error $ "Invalid types for builtin.hasAttr: " ++ show (key, aset)

prim_getAttr :: MonadFix m => NValue m
prim_getAttr = Fix $ NVBuiltin2 "getAttr" [] getAttr
getAttr :: MonadFix m => NValue m -> NValue m -> m (NValue m)
getAttr (Fix (NVStr key _)) (Fix (NVSet aset)) = return $ Map.findWithDefault _err key aset
  where _err = error ("Field does not exist " ++ Text.unpack key)
getAttr key aset = error $ "Invalid types for builtin.getAttr: " ++ show (key, aset)


prim_any :: MonadFix m => NValue m
prim_any = Fix $ NVBuiltin2 "any" [] _any
_any :: MonadFix m => NValue m -> NValue m -> m (NValue m)
_any pred (Fix (NVList l)) = mkBool . any extractBool <$> mapM (evalPred pred) l
_any _ list = error $ "builtins.any takes a list as second argument, not a " ++ show list

prim_all :: MonadFix m => NValue m
prim_all = Fix $ NVBuiltin2 "all" [] _all
_all :: MonadFix m => NValue m -> NValue m -> m (NValue m)
_all pred (Fix (NVList l)) = mkBool . all extractBool <$> mapM (evalPred pred) l
_all _ list = error $ "builtins.all takes a list as second argument, not a " ++ show list

