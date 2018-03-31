{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Lint (checkExpr) where

import           Control.Monad
import           Data.Fix
import           Data.Functor.Compose
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Eval
import           Nix.Expr
import           Nix.Monad
import           Nix.Scope

data NSymbolicF r
    = NAny
    | NMany [r]
    deriving (Show, Eq, Ord, Functor)

type Symbolic m = Fix (Compose NSymbolicF (NValueF m))

everyPossible :: Symbolic m
everyPossible = Fix $ Compose NAny

symbolicEvalExpr :: MonadNix m => NExpr -> m (Symbolic m)
symbolicEvalExpr = cataM eval

eval :: MonadNix m => NExprF (Symbolic m) -> m (Symbolic m)

eval (NSym var) = do
    mres <- lookupVar var
    case mres of
        Nothing -> throwError $ "Undefined variable: " ++ show var
        Just v  -> return v

eval v@(NConstant _)    = Compose $ NMany [v]
eval v@(NStr _)         = Compose $ NMany [v]
eval v@(NLiteralPath _) = Compose $ NMany [v]
eval v@(NEnvPath _)     = Compose $ NMany [v]

eval e@(NUnary _op arg) = unify e arg [NVConstant NInt, NVConstant NBool]

eval (NBinary op larg rarg) = do
    let unsupportedTypes =
            "unsupported argument types for binary operator "
                ++ show (lval, op, rval)
    case (lval, rval) of
        (NVConstant lc, NVConstant rc) -> case (op, lc, rc) of
            (NEq,  _, _) ->
                -- TODO: Refactor so that eval (NBinary ..) dispatches based
                -- on operator first
                valueRefBool =<< valueEq lval rval
            (NNEq, _, _) -> valueRefBool . not =<< valueEq lval rval
            (NLt,  l, r) -> valueRefBool $ l <  r
            (NLte, l, r) -> valueRefBool $ l <= r
            (NGt,  l, r) -> valueRefBool $ l >  r
            (NGte, l, r) -> valueRefBool $ l >= r
            (NAnd,  NBool l, NBool r) -> valueRefBool $ l && r
            (NOr,   NBool l, NBool r) -> valueRefBool $ l || r
            (NImpl, NBool l, NBool r) -> valueRefBool $ not l || r
            (NPlus,  NInt l, NInt r) -> valueRefInt $ l + r
            (NMinus, NInt l, NInt r) -> valueRefInt $ l - r
            (NMult,  NInt l, NInt r) -> valueRefInt $ l * r
            (NDiv,   NInt l, NInt r) -> valueRefInt $ l `div` r
            _ -> throwError unsupportedTypes

        (NVStr ls lc, NVStr rs rc) -> case op of
            NPlus -> return $ NVStr (ls `mappend` rs) (lc `mappend` rc)
            NEq  -> valueRefBool =<< valueEq lval rval
            NNEq -> valueRefBool . not =<< valueEq lval rval
            _    -> throwError unsupportedTypes

        (NVSet ls, NVSet rs) -> case op of
            NUpdate -> return $ NVSet $ rs `Map.union` ls
            _ -> throwError unsupportedTypes

        (NVList ls, NVList rs) -> case op of
            NConcat -> return $ NVList $ ls ++ rs
            NEq -> valueRefBool =<< valueEq lval rval
            _ -> throwError unsupportedTypes

        (NVLiteralPath ls, NVLiteralPath rs) -> case op of
            -- TODO: Canonicalise path
            NPlus -> return $ NVLiteralPath $ ls ++ rs
            _ -> throwError unsupportedTypes

        (NVLiteralPath ls, NVStr rs rc) -> case op of
            -- TODO: Canonicalise path
            NPlus -> return $ NVStr (Text.pack ls `mappend` rs) rc
            _ -> throwError unsupportedTypes

        _ -> throwError unsupportedTypes

eval (NSelect aset attr alternative) = do
    aset' <- aset
    ks    <- evalSelector True attr
    mres  <- extract aset' ks
    case mres of
        Just v -> do
            traceM $ "Wrapping a selector: " ++ show (() <$ v)
            pure v
        Nothing -> fromMaybe err alternative
          where
            err = throwError $ "could not look up attribute "
                ++ intercalate "." (map Text.unpack ks)
                ++ " in " ++ show (() <$ aset')
  where
    extract (NVSet s) (k:ks) = case Map.lookup k s of
        Just v  -> do
            s' <- forceThunk v
            extract s' ks
        Nothing -> return Nothing
    extract _ (_:_) = return Nothing
    extract v [] = return $ Just v

eval (NHasAttr aset attr) = aset >>= \case
    NVSet s -> evalSelector True attr >>= \case
        [keyName] ->
            return $ NVConstant $ NBool $ keyName `Map.member` s
        _ -> throwError "attr name argument to hasAttr is not a single-part name"
    _ -> throwError "argument to hasAttr has wrong type"

eval (NList l) = do
    scope <- currentScope
    NVList <$> traverse (deferInScope scope) l

eval (NSet binds) = do
    traceM "NSet..1"
    s <- evalBinds True False binds
    traceM $ "NSet..2: s = " ++ show (() <$ s)
    return $ NVSet s

eval (NRecSet binds) = do
    traceM "NRecSet..1"
    s <- evalBinds True True binds
    traceM $ "NRecSet..2: s = " ++ show (() <$ s)
    return $ NVSet s

eval (NLet binds e) = do
    traceM "Let..1"
    s <- evalBinds True True binds
    traceM $ "Let..2: s = " ++ show (() <$ s)
    pushScope s e

eval (NIf cond t f) = cond >>= \case
    NVConstant (NBool True) -> t
    NVConstant (NBool False) -> f
    _ -> throwError "condition must be a boolean"

eval (NWith scope e) = scope >>= \case
    NVSet s -> pushWeakScope s e
    _ -> throwError "scope must be a set in with statement"

eval (NAssert cond e) = cond >>= \case
    NVConstant (NBool True) -> e
    NVConstant (NBool False) -> throwError "assertion failed"
    _ -> throwError "assertion condition must be boolean"

eval (NApp fun arg) = fun >>= \case
    NVFunction params f -> do
        args <- buildArgument params =<< buildThunk arg
        traceM $ "Evaluating function application with args: "
            ++ show (newScope args)
        clearScopes (pushScope args (forceThunk =<< f))
    NVBuiltin _ f -> f =<< buildThunk arg
    x -> throwError $ "Attempt to call non-function: " ++ show (() <$ x)

eval (NAbs params body) = do
    -- It is the environment at the definition site, not the call site, that
    -- needs to be used when evaluating the body and default arguments, hence
    -- we defer here so the present scope is restored when the parameters and
    -- body are forced during application.
    scope <- currentScope
    traceM $ "Creating lambda abstraction in scope: " ++ show scope
    return $ NVFunction (buildThunk . pushScopes scope <$> params)
                        (buildThunk (pushScopes scope body))

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
