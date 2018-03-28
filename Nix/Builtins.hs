module Nix.Builtins (baseEnv, builtins, evalTopLevelExpr) where

import           Data.Fix
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Eval
import           Nix.Expr (NExpr)
import           Nix.Parser
import           System.IO.Unsafe

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: NExpr -> NValue
evalTopLevelExpr val = evalExpr val baseEnv

baseEnv :: ValueSet
baseEnv =
    Map.fromList $ [ ("builtins", Fix $ NVSet builtins) ] ++ topLevelBuiltins
  where
    topLevelBuiltins = map mapping $ filter isTopLevel builtinsList
    -- builtins = Map.fromList $ map mapping $ builtinsList

builtins :: ValueSet
builtins = Map.fromList $ map mapping $ builtinsList

data BuiltinType = Normal | TopLevel
data Builtin = Builtin
    { kind    :: BuiltinType
    , mapping :: (Text, NValue)
    }

isTopLevel :: Builtin -> Bool
isTopLevel b = case kind b of
    Normal -> False
    TopLevel -> True

builtinsList :: [ Builtin ]
builtinsList =
    [ topLevel ("toString", prim_toString)
    , topLevel ("import",   prim_import)
    , basic    ("hasAttr" , prim_hasAttr)
    , basic    ("getAttr" , prim_getAttr)
    , basic    ("any"     , prim_any )
    , basic    ("all"     , prim_all )
    ]
  where
    basic = Builtin Normal
    topLevel = Builtin TopLevel

-- Helpers

mkBool :: Bool -> NValue
mkBool = Fix . NVConstant . NBool

extractBool :: NValue -> Bool
extractBool (Fix (NVConstant (NBool b))) = b
extractBool _ = error "Not a bool constant"

evalPred :: NValue -> NValue -> NValue
evalPred (Fix (NVFunction params pred)) = pred . buildArgument params
evalPred pred = error $ "Trying to call a " ++ show pred

-- Primops

prim_toString :: NValue
prim_toString = builtin "toString" $ toString

toString :: NValue -> NValue
toString s = Fix $ uncurry NVStr $ valueText s

prim_import :: NValue
prim_import = builtin "import" $ import_

import_ :: NValue -> NValue
import_ (Fix (NVLiteralPath path)) = doImport path
import_ (Fix (NVEnvPath path)) = doImport path -- jww (2018-03-27): ?
import_ val = error $ "Invalid argument type for import: " ++ show val

-- Since the files we are importing can be considered immutable during parsing
-- and evaluation, this use of unsafePerformIO should be fine.
doImport :: FilePath -> NValue
doImport path = case unsafePerformIO (parseNixFile path) of
    Failure err ->
        error $ "Import of " ++ path ++ " failed: " ++ show err
    Success expr -> evalExpr expr baseEnv

prim_hasAttr :: NValue
prim_hasAttr = builtin2 "hasAttr" hasAttr

hasAttr :: NValue -> NValue -> NValue
hasAttr (Fix (NVStr key _)) (Fix (NVSet aset)) =
    Fix $ NVConstant $ NBool $ Map.member key aset
hasAttr key aset =
    error $ "Invalid types for builtin.hasAttr: " ++ show (key, aset)

prim_getAttr :: NValue
prim_getAttr = builtin2 "getAttr" getAttr

getAttr :: NValue -> NValue -> NValue
getAttr (Fix (NVStr key _)) (Fix (NVSet aset)) =
    Map.findWithDefault _err key aset
  where
    _err = error ("Field does not exist " ++ Text.unpack key)
getAttr key aset =
    error $ "Invalid types for builtin.getAttr: " ++ show (key, aset)

prim_any :: NValue
prim_any = builtin2 "any" _any

_any :: NValue -> NValue -> NValue
_any pred (Fix (NVList l)) = mkBool . any extractBool $ map (evalPred pred) l
_any _ list =
    error $ "builtins.any takes a list as second argument, not a " ++ show list

prim_all :: NValue
prim_all = builtin2 "all" _all

_all :: NValue -> NValue -> NValue
_all pred (Fix (NVList l)) = mkBool . all extractBool $ map (evalPred pred) l
_all _ list =
    error $ "builtins.all takes a list as second argument, not a " ++ show list
