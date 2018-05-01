{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Nix.Type.Infer (
  Constraint(..),
  TypeError(..),
  Subst(..),
  inferTop
) where

import           Nix.Atoms
import           Nix.Convert
import           Nix.Eval (MonadEval(..))
import qualified Nix.Eval as Eval
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames (Frame)
import           Nix.Scope
import           Nix.Thunk
import qualified Nix.Type.Assumption as As
import           Nix.Type.Env
import qualified Nix.Type.Env as Env
import           Nix.Type.Type
import           Nix.Utils

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Fix
import           Data.Foldable
import           Data.List (delete, find, nub, intersect, (\\))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Semigroup
import qualified Data.HashMap.Lazy as M
import qualified Data.Set as Set
import           Data.Text (Text)

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Inference monad
newtype Infer a = Infer
    { getInfer ::
        ReaderT (Set.Set TVar, Scopes Infer Judgment)  -- Monomorphic set
                (StateT InferState                     -- Inference state
                        (Except TypeError))            -- Inference errors
                a                                      -- Result
    }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix,
              MonadReader (Set.Set TVar, Scopes Infer Judgment),
              MonadState InferState, MonadError TypeError)

-- | Inference state
newtype InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

data Constraint
    = EqConst Type Type
    | EqConstOneOf Type [Type]
      -- ^ The first type must unify with the second. For example, integer
      -- could unify with integer, or a type variable.
    | ExpInstConst Type Scheme
    | ImpInstConst Type (Set.Set TVar) Type
    deriving (Show, Eq, Ord)

newtype Subst = Subst (Map TVar Type)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable TVar where
  apply (Subst s) a = tv
    where t = TVar a
          (TVar tv) = Map.findWithDefault t a s

instance Substitutable Type where
  apply _ (TCon a)           = TCon a
  apply s (TSet a)           = TSet (M.map (apply s) a)
  apply s (TSubSet a)        = TSubSet (M.map (apply s) a)
  apply s (TList a)          = TList (map (apply s) a)
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2)     = apply s t1 `TArr` apply s t2

instance Substitutable Scheme where
  apply (Subst s) (Forall as t) = Forall as $ apply s' t
    where s' = Subst $ foldr Map.delete s as

instance Substitutable Constraint where
   apply s (EqConst t1 t2)         = EqConst (apply s t1) (apply s t2)
   apply s (EqConstOneOf t1 t2)    = EqConstOneOf (apply s t1) (apply s t2)
   apply s (ExpInstConst t sc)     = ExpInstConst (apply s t) (apply s sc)
   apply s (ImpInstConst t1 ms t2) = ImpInstConst (apply s t1) (apply s ms) (apply s t2)

instance Substitutable a => Substitutable [a] where
  apply = map . apply

instance (Ord a, Substitutable a) => Substitutable (Set.Set a) where
  apply = Set.map . apply


class FreeTypeVars a where
  ftv :: a -> Set.Set TVar

instance FreeTypeVars Type where
  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (TSet a)       = Set.unions (map ftv (M.elems a))
  ftv (TSubSet a)    = Set.unions (map ftv (M.elems a))
  ftv (TList a)      = Set.unions (map ftv a)
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance FreeTypeVars TVar where
  ftv = Set.singleton

instance FreeTypeVars Scheme where
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance FreeTypeVars a => FreeTypeVars [a] where
  ftv   = foldr (Set.union . ftv) Set.empty

instance (Ord a, FreeTypeVars a) => FreeTypeVars (Set.Set a) where
  ftv   = foldr (Set.union . ftv) Set.empty


class ActiveTypeVars a where
  atv :: a -> Set.Set TVar

instance ActiveTypeVars Constraint where
  atv (EqConst t1 t2)         = ftv t1 `Set.union` ftv t2
  atv (EqConstOneOf t1 t2)    = ftv t1 `Set.union` ftv t2
  atv (ImpInstConst t1 ms t2) = ftv t1 `Set.union` (ftv ms `Set.intersection` ftv t2)
  atv (ExpInstConst t s)      = ftv t `Set.union` ftv s

instance ActiveTypeVars a => ActiveTypeVars [a] where
  atv = foldr (Set.union . atv) Set.empty


data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable Text
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  | forall s. Frame s => EvaluationError s
  | InferenceAborted

deriving instance Show TypeError

instance Semigroup TypeError where
    x <> _ = x

instance Monoid TypeError where
    mempty  = InferenceAborted
    mappend = (<>)

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Infer a -> Either TypeError a
runInfer m =
    runExcept $ evalStateT (runReaderT (getInfer m) (Set.empty, emptyScopes)) initInfer

inferType :: Env -> NExpr -> Infer (Subst, Type)
inferType env ex = do
  Judgment as cs t <- infer ex
  let unbounds = Set.fromList (As.keys as) `Set.difference` Set.fromList (Env.keys env)
  unless (Set.null unbounds) $ throwError $ UnboundVariable (Set.findMin unbounds)
  let cs' = [ExpInstConst t s | (x, s) <- Env.toList env, t <- As.lookup x as]
  subst <- solve (cs ++ cs')
  return (subst, apply subst t)

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Env -> NExpr -> Either TypeError Scheme
inferExpr env ex = case runInfer (inferType env ex) of
  Left err -> Left err
  Right (subst, ty) -> Right $ closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize Set.empty

extendMSet :: TVar -> Infer a -> Infer a
extendMSet x = Infer . local (first (Set.insert x)) . getInfer

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = Infer $ do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ apply s t

generalize :: Set.Set TVar -> Type -> Scheme
generalize free t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` free

unops :: Type -> NUnaryOp -> [Constraint]
unops u1 = \case
    NNot -> [ EqConst      u1 ( typeFun [typeBool, typeBool] ) ]
    NNeg -> [ EqConstOneOf u1 [ typeFun [typeInt,   typeInt]
                             , typeFun [typeFloat, typeFloat] ] ]

binops :: Type -> NBinaryOp -> [Constraint]
binops u1 = \case
    NApp    -> []                -- this is handled separately

    -- Equality tells you nothing about the types, because any two types are
    -- allowed.
    NEq     -> []
    NNEq    -> []

    NGt     -> inequality
    NGte    -> inequality
    NLt     -> inequality
    NLte    -> inequality

    NAnd    -> [ EqConst      u1 ( typeFun [typeBool,   typeBool,   typeBool]) ]
    NOr     -> [ EqConst      u1 ( typeFun [typeBool,   typeBool,   typeBool]) ]
    NImpl   -> [ EqConst      u1 ( typeFun [typeBool,   typeBool,   typeBool]) ]

    NConcat -> [ EqConstOneOf u1 [ typeFun [typeList,   typeList,   typeList]
                                , typeFun [typeList,   typeNull,   typeList]
                                , typeFun [typeNull,   typeList,   typeList]
                                ] ]

    NUpdate -> [ EqConstOneOf u1 [ typeFun [typeSet,    typeSet,    typeSet]
                                , typeFun [typeSet,    typeNull,   typeSet]
                                , typeFun [typeNull,   typeSet,    typeSet]
                                ] ]

    NPlus   -> [ EqConstOneOf u1 [ typeFun [typeInt,    typeInt,    typeInt]
                                , typeFun [typeFloat,  typeFloat,  typeFloat]
                                , typeFun [typeInt,    typeFloat,  typeFloat]
                                , typeFun [typeFloat,  typeInt,    typeFloat]
                                , typeFun [typeString, typeString, typeString]
                                , typeFun [typePath,   typePath,   typePath]
                                , typeFun [typeString, typeString, typePath]
                                ] ]
    NMinus  -> arithmetic
    NMult   -> arithmetic
    NDiv    -> arithmetic
  where
    inequality =
        [ EqConstOneOf u1 [ typeFun [typeInt,    typeInt,    typeBool]
                          , typeFun [typeFloat,  typeFloat,  typeBool]
                          , typeFun [typeInt,    typeFloat,  typeBool]
                          , typeFun [typeFloat,  typeInt,    typeBool]
                          ] ]

    arithmetic =
        [ EqConstOneOf u1 [ typeFun [typeInt,    typeInt,    typeInt]
                          , typeFun [typeFloat,  typeFloat,  typeFloat]
                          , typeFun [typeInt,    typeFloat,  typeFloat]
                          , typeFun [typeFloat,  typeInt,    typeFloat]
                          ] ]

instance MonadThunk Judgment Judgment Infer where
    thunk     = id
    force v f = f v
    value     = id

instance MonadEval Judgment Infer where
    freeVariable var = do
        tv <- fresh
        return $ Judgment (As.singleton var tv) [] tv

    evaledSym _ = pure

    evalCurPos =
        return $ Judgment As.empty [] $ TSet $ M.fromList
            [ ("file", typePath)
            , ("line", typeInt)
            , ("col",  typeInt) ]

    evalConstant c  = return $ Judgment As.empty [] (go c)
      where
        go = \case
          NInt _   -> typeInt
          NFloat _ -> typeFloat
          NBool _  -> typeBool
          NNull    -> typeNull
          NUri _   -> typeUri

    evalString      = const $ return $ Judgment As.empty [] typeString
    evalLiteralPath = const $ return $ Judgment As.empty [] typePath
    evalEnvPath     = const $ return $ Judgment As.empty [] typePath

    evalUnary op (Judgment as1 cs1 t1) = do
        tv <- fresh
        return $ Judgment as1 (cs1 ++ unops (t1 `TArr` tv) op) tv

    evalBinary op (Judgment as1 cs1 t1) e2 = do
        Judgment as2 cs2 t2 <- e2
        tv <- fresh
        return $ Judgment
            (as1 `As.merge` as2)
            (cs1 ++ cs2 ++ binops (t1 `TArr` (t2 `TArr` tv)) op)
            tv

    evalWith _scope _body = undefined-- pushWeakScope undefined body

    evalIf (Judgment as1 cs1 t1) t f = do
        Judgment as2 cs2 t2 <- t
        Judgment as3 cs3 t3 <- f
        return $ Judgment
            (as1 `As.merge` as2 `As.merge` as3)
            (cs1 ++ cs2 ++ cs3 ++ [EqConst t1 typeBool, EqConst t2 t3])
            t2

    evalAssert (Judgment as1 cs1 t1) body = do
        Judgment as2 cs2 t2 <- body
        return $ Judgment
            (as1 `As.merge` as2)
            (cs1 ++ cs2 ++ [EqConst t1 typeBool])
            t2

    evalApp (Judgment as1 cs1 t1) e2 = do
        Judgment as2 cs2 t2 <- e2
        tv <- fresh
        return $ Judgment
            (as1 `As.merge` as2)
            (cs1 ++ cs2 ++ [EqConst t1 (t2 `TArr` tv)])
            tv

    evalAbs (Param x) e = do
        tv@(TVar a) <- fresh
        Judgment as cs t <-
            extendMSet a (e (pure (Judgment (As.singleton x tv) [] tv)))
        return $ Judgment
            (as `As.remove` x)
            (cs ++ [EqConst t' tv | t' <- As.lookup x as])
            (tv `TArr` t)

    evalAbs (ParamSet _x _variadic _mname) _e = undefined

    evalError = throwError . EvaluationError

data Judgment = Judgment
    { assumptions     :: As.Assumption
    , typeConstraints :: [Constraint]
    , inferredType    :: Type
    }
    deriving Show

instance FromValue (Text, DList Text) Infer Judgment where
    fromValueMay _ = return Nothing
    fromValue _ = error "Unused"

instance FromValue (AttrSet Judgment, AttrSet SourcePos) Infer Judgment where
    -- jww (2018-04-30): How can we do this? TSet doesn't record enough information
    fromValueMay (Judgment _ _ (TSet xs)) =
        pure $ Just (M.mapWithKey (\k v -> Judgment (As.singleton k v) [] v) xs, M.empty)
    fromValueMay _ = pure Nothing
    fromValue = fromValueMay >=> \case
        Just v  -> pure v
        Nothing -> pure (M.empty, M.empty)

instance ToValue (AttrSet Judgment, AttrSet SourcePos) Infer Judgment where
    toValue (xs, _) = pure $ Judgment
        (foldr (As.merge . assumptions) As.empty xs)
        (concatMap typeConstraints xs)
        (TSet (M.map inferredType xs))

instance ToValue [Judgment] Infer Judgment where
    toValue xs = pure $ Judgment
        (foldr (As.merge . assumptions) As.empty xs)
        (concatMap typeConstraints xs)
        (TList (map inferredType xs))

instance ToValue Bool Infer Judgment where
    toValue _ = pure $ Judgment As.empty [] typeBool

infer :: NExpr -> Infer Judgment
infer = cata Eval.eval

inferTop :: Env -> [(Text, NExpr)] -> Either TypeError Env
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)    = [a]
    fv (TArr a b)  = fv a ++ fv b
    fv (TCon _)    = []
    fv (TSet a)    = concatMap fv (M.elems a)
    fv (TSubSet a) = concatMap fv (M.elems a)
    fv (TList a)   = concatMap fv a

    normtype (TArr a b)  = TArr (normtype a) (normtype b)
    normtype (TCon a)    = TCon a
    normtype (TSet a)    = TSet (M.map normtype a)
    normtype (TSubSet a) = TSubSet (M.map normtype a)
    normtype (TList a)   = TList (map normtype a)
    normtype (TVar a)    =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

unifyMany :: [Type] -> [Type] -> Infer Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Infer Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TList _) (TList _) = return emptySubst
unifies (TSet b) (TSubSet s)
    | M.keys b `intersect` M.keys s == M.keys s = return emptySubst
unifies (TSubSet s) (TSet b)
    | M.keys b `intersect` M.keys s == M.keys s = return emptySubst
unifies (TSet s) (TSet b)
    | null (M.keys b \\ M.keys s) = return emptySubst
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

bind ::  TVar -> Type -> Infer Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ Map.singleton a t)

occursCheck ::  FreeTypeVars a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable xs = fromJust (find solvable (chooseOne xs))
  where
    chooseOne xs = [(x, ys) | x <- xs, let ys = delete x xs]
    solvable (EqConst{}, _)      = True
    solvable (EqConstOneOf{}, _) = True
    solvable (ExpInstConst{}, _) = True
    solvable (ImpInstConst _t1 ms t2, cs) =
        Set.null ((ftv t2 `Set.difference` ms) `Set.intersection` atv cs)

solve :: [Constraint] -> Infer Subst
solve [] = return emptySubst
solve cs = solve' (nextSolvable cs)

solve' :: (Constraint, [Constraint]) -> Infer Subst
solve' (EqConst t1 t2, cs) = do
  su1 <- unifies t1 t2
  su2 <- solve (apply su1 cs)
  return (su2 `compose` su1)

solve' (EqConstOneOf t1 t2, cs) = do
  -- jww (2018-04-30): Instead of picking the first that matches, collect all
  -- that match into a 'TVariant [Type]' type, so that we can report that a
  -- function like 'x: y: x + y' has type: forall a b. a one of integer,
  -- float, string, b the same as a, or compatible, result is determined by
  -- the finally decided type of the function (in this case, one of int,
  -- float, string or path, based on the types of a and b).
  su1 <- asum (map (unifies t1) t2)
  su2 <- solve (apply su1 cs)
  return (su2 `compose` su1)

solve' (ImpInstConst t1 ms t2, cs) =
  solve (ExpInstConst t1 (generalize ms t2) : cs)

solve' (ExpInstConst t s, cs) = do
  s' <- instantiate s
  solve (EqConst t s' : cs)
