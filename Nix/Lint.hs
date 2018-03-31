{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Lint (checkExpr) where

import           Control.Monad
import           Data.Fix
-- import           Data.Functor.Compose
-- import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
-- import           Data.Set (Set)
-- import qualified Data.Set as Set
-- import           Data.Text (Text)
-- import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Eval
import           Nix.Expr
import           Nix.Monad
import           Nix.Scope

nullVal :: MonadNix m => m (NValue m)
nullVal = return $ NVConstant NNull

-- | Evaluate an nix expression, with a given ValueSet as environment
checkExpr :: (Scoped e (NThunk m) m, Framed e m, MonadNix m) => NExpr -> m ()
checkExpr = cata check

check :: forall e m. (Scoped e (NThunk m) m, Framed e m, MonadNix m)
     => NExprF (m ()) -> m ()

check (NSym var) = lookupVar @_ @(NThunk m) var >>= \case
    Nothing -> error $ "lint: Undefined variable: " ++ show var
    Just _ -> return ()

check (NSet binds) =
    void $ evalBinds True False (fmap (fmap (const nullVal)) binds)

check (NRecSet binds) =
    void $ evalBinds True True (fmap (fmap (const nullVal)) binds)

check (NLet binds e) =
    (`pushScope` e)
        =<< evalBinds True True (fmap (fmap (const nullVal)) binds)

-- check (NWith _scope e) = do
--     env <- currentScope
--     pushScope env e

check (NAbs a b) = do
    nv <- buildThunk nullVal
    case a of
        Param name ->
            pushScope (M.singleton name nv) b
        ParamSet (FixedParamSet s) Nothing ->
            pushScope (nv <$ s) b
        ParamSet (FixedParamSet s) (Just m) ->
            pushScope (M.insert m nv (nv <$ s)) b
        ParamSet (VariadicParamSet s) Nothing ->
            pushScope (nv <$ s) b
        ParamSet (VariadicParamSet s) (Just m) ->
            pushScope (M.insert m nv (nv <$ s)) b

-- In order to check some of the other operations properly, we'd need static
-- typing
check _ = return ()
