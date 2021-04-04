{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Nix.Type.Infer
  ( Constraint(..)
  , TypeError(..)
  , InferError(..)
  , Subst(..)
  , inferTop
  )
where

import           Control.Monad.Catch            ( MonadThrow(..)
                                                , MonadCatch(..)
                                                )
import           Control.Monad.Except           ( MonadError(..)
                                                )
import           Prelude                 hiding ( Type
                                                , TVar
                                                , Constraint
                                                )
import           Nix.Utils
import           Control.Monad.Logic     hiding ( fail )
import           Control.Monad.Reader           ( MonadFix
                                                )
import           Control.Monad.Ref
import           Control.Monad.ST               ( ST
                                                , runST
                                                )
import           Data.Fix                       ( foldFix )
import           Data.Foldable                  ( foldrM )
import qualified Data.HashMap.Lazy             as M
import           Data.List                      ( delete
                                                , intersect
                                                , (\\)
                                                , (!!)
                                                )
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )
import qualified Data.Set                      as Set
import           Nix.Atoms
import           Nix.Convert
import           Nix.Eval                       ( MonadEval(..) )
import qualified Nix.Eval                      as Eval
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Fresh
import           Nix.String
import           Nix.Scope
import qualified Nix.Type.Assumption           as As
import           Nix.Type.Env            hiding ( empty )
import qualified Nix.Type.Env                  as Env
import           Nix.Type.Type
import           Nix.Value.Monad
import           Nix.Var


-- * Classes

-- | Inference monad
newtype InferT s m a =
  InferT
    { getInfer ::
        ReaderT
          (Set.Set TVar, Scopes (InferT s m) (Judgment s))
          (StateT InferState (ExceptT InferError m))
          a
    }
    deriving
      ( Functor
      , Applicative
      , Alternative
      , Monad
      , MonadPlus
      , MonadFix
      , MonadReader (Set.Set TVar, Scopes (InferT s m) (Judgment s))
      , MonadFail
      , MonadState InferState
      , MonadError InferError
      )

instance MonadTrans (InferT s) where
  lift = InferT . lift . lift . lift

-- instance MonadThunkId m => MonadThunkId (InferT s m) where
--   type ThunkId (InferT s m) = ThunkId m

-- | Inference state
newtype InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

data Constraint
  = EqConst Type Type
  | ExpInstConst Type Scheme
  | ImpInstConst Type (Set.Set TVar) Type
  deriving (Show, Eq, Ord)

newtype Subst = Subst (Map TVar Type)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable TVar where
  apply (Subst s) a = tv
   where
    t         = TVar a
    (TVar tv) = Map.findWithDefault t a s

instance Substitutable Type where
  apply _         (  TCon a   ) = TCon a
  apply s         (  TSet b a ) = TSet b (M.map (apply s) a)
  apply s         (  TList a  ) = TList (fmap (apply s) a)
  apply (Subst s) t@(TVar  a  ) = Map.findWithDefault t a s
  apply s         (  t1 :~> t2) = apply s t1 :~> apply s t2
  apply s         (  TMany ts ) = TMany (fmap (apply s) ts)

instance Substitutable Scheme where
  apply (Subst s) (Forall as t) = Forall as $ apply s' t
    where s' = Subst $ foldr Map.delete s as

instance Substitutable Constraint where
  apply s (EqConst      t1 t2) = EqConst (apply s t1) (apply s t2)
  apply s (ExpInstConst t  sc) = ExpInstConst (apply s t) (apply s sc)
  apply s (ImpInstConst t1 ms t2) =
    ImpInstConst (apply s t1) (apply s ms) (apply s t2)

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply

instance (Ord a, Substitutable a) => Substitutable (Set.Set a) where
  apply = Set.map . apply


class FreeTypeVars a where
  ftv :: a -> Set.Set TVar

instance FreeTypeVars Type where
  ftv TCon{}      = mempty
  ftv (TVar a   ) = Set.singleton a
  ftv (TSet _ a ) = Set.unions (fmap ftv (M.elems a))
  ftv (TList a  ) = Set.unions (fmap ftv a)
  ftv (t1 :~> t2) = ftv t1 `Set.union` ftv t2
  ftv (TMany ts ) = Set.unions (fmap ftv ts)

instance FreeTypeVars TVar where
  ftv = Set.singleton

instance FreeTypeVars Scheme where
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance FreeTypeVars a => FreeTypeVars [a] where
  ftv = foldr (Set.union . ftv) mempty

instance (Ord a, FreeTypeVars a) => FreeTypeVars (Set.Set a) where
  ftv = foldr (Set.union . ftv) mempty


class ActiveTypeVars a where
  atv :: a -> Set.Set TVar

instance ActiveTypeVars Constraint where
  atv (EqConst      t1 t2   ) = ftv t1 `Set.union` ftv t2
  atv (ImpInstConst t1 ms t2) = ftv t1 `Set.union` (ftv ms `Set.intersection` ftv t2)
  atv (ExpInstConst t  s     ) = ftv t  `Set.union` ftv s

instance ActiveTypeVars a => ActiveTypeVars [a] where
  atv = foldr (Set.union . atv) mempty

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariables [Text]
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  deriving (Eq, Show, Ord)

data InferError
  = TypeInferenceErrors [TypeError]
  | TypeInferenceAborted
  | forall s. Exception s => EvaluationError s

typeError :: MonadError InferError m => TypeError -> m ()
typeError err = throwError $ TypeInferenceErrors [err]

deriving instance Show InferError
instance Exception InferError

instance Semigroup InferError where
  x <> _ = x

instance Monoid InferError where
  mempty  = TypeInferenceAborted
  mappend = (<>)


-- * Inference

-- | Run the inference monad
runInfer' :: MonadInfer m => InferT s m a -> m (Either InferError a)
runInfer' =
  runExceptT
    . (`evalStateT` initInfer)
    . (`runReaderT` (mempty, emptyScopes))
    . getInfer

runInfer :: (forall s . InferT s (FreshIdT Int (ST s)) a) -> Either InferError a
runInfer m = runST $ do
  i <- newVar (1 :: Int)
  runFreshIdT i (runInfer' m)

inferType
  :: forall s m . MonadInfer m => Env -> NExpr -> InferT s m [(Subst, Type)]
inferType env ex = do
  Judgment as cs t <- infer ex
  let unbounds =
        Set.fromList (As.keys as) `Set.difference` Set.fromList (Env.keys env)
  unless (Set.null unbounds) $ typeError $ UnboundVariables
    (ordNub (Set.toList unbounds))
  let
    cs' =
      [ ExpInstConst t s
          | (x, ss) <- Env.toList env
          , s       <- ss
          , t       <- As.lookup x as
      ]
  inferState <- get
  let
    eres = (`evalState` inferState) $ runSolver $
      do
        subst <- solve (cs <> cs')
        pure (subst, subst `apply` t)
  either
    (throwError . TypeInferenceErrors)
    pure
    eres

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Env -> NExpr -> Either InferError [Scheme]
inferExpr env ex =
  (\ (subst, ty) -> closeOver $ subst `apply` ty) <<$>> runInfer (inferType env ex)

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalizeScheme . generalize mempty

extendMSet :: Monad m => TVar -> InferT s m a -> InferT s m a
extendMSet x = InferT . local (first (Set.insert x)) . getInfer

letters :: [String]
letters =
  do
    l <- [1 ..]
    replicateM
      l
      ['a' .. 'z']

freshTVar :: MonadState InferState m => m TVar
freshTVar =
  do
    s <- get
    put s { count = count s + 1 }
    pure $ TV (toText (letters !! count s))

fresh :: MonadState InferState m => m Type
fresh = TVar <$> freshTVar

instantiate :: MonadState InferState m => Scheme -> m Type
instantiate (Forall as t) =
  do
    as' <- traverse (const fresh) as
    let s = Subst $ Map.fromList $ zip as as'
    pure $ apply s t

generalize :: Set.Set TVar -> Type -> Scheme
generalize free t = Forall as t
 where
  as = Set.toList $ ftv t `Set.difference` free

unops :: Type -> NUnaryOp -> [Constraint]
unops u1 op =
  [ EqConst u1
   (case op of
      NNot -> typeFun [typeBool                   , typeBool                       ]
      NNeg -> TMany   [typeFun  [typeInt, typeInt], typeFun  [typeFloat, typeFloat]]
    )
  ]

binops :: Type -> NBinaryOp -> [Constraint]
binops u1 op =
  if
    -- NApp in fact is handled separately
    -- Equality tells nothing about the types, because any two types are allowed.
    | op `elem` [ NApp  , NEq  , NNEq        ] -> mempty
    | op `elem` [ NGt   , NGte , NLt  , NLte ] -> inequality
    | op `elem` [ NAnd  , NOr  , NImpl       ] -> gate
    | op ==       NConcat                      -> concatenation
    | op `elem` [ NMinus, NMult, NDiv        ] -> arithmetic
    | op ==       NUpdate                      -> rUnion
    | op ==       NPlus                        -> addition
    | otherwise -> fail "GHC so far can not infer that this pattern match is full, so make it happy."

 where

  gate          = eqCnst [typeBool, typeBool, typeBool]
  concatenation = eqCnst [typeList, typeList, typeList]

  eqCnst l = [EqConst u1 (typeFun l)]

  inequality =
    eqCnstMtx
      [ [typeInt  , typeInt  , typeBool]
      , [typeFloat, typeFloat, typeBool]
      , [typeInt  , typeFloat, typeBool]
      , [typeFloat, typeInt  , typeBool]
      ]

  arithmetic =
    eqCnstMtx
      [ [typeInt  , typeInt  , typeInt  ]
      , [typeFloat, typeFloat, typeFloat]
      , [typeInt  , typeFloat, typeFloat]
      , [typeFloat, typeInt  , typeFloat]
      ]

  rUnion =
    eqCnstMtx
      [ [typeSet , typeSet , typeSet]
      , [typeSet , typeNull, typeSet]
      , [typeNull, typeSet , typeSet]
      ]

  addition =
    eqCnstMtx
      [ [typeInt   , typeInt   , typeInt   ]
      , [typeFloat , typeFloat , typeFloat ]
      , [typeInt   , typeFloat , typeFloat ]
      , [typeFloat , typeInt   , typeFloat ]
      , [typeString, typeString, typeString]
      , [typePath  , typePath  , typePath  ]
      , [typeString, typeString, typePath  ]
      ]

  eqCnstMtx mtx = [EqConst u1 (TMany (typeFun <$> mtx))]

liftInfer :: Monad m => m a -> InferT s m a
liftInfer = InferT . lift . lift . lift

instance MonadRef m => MonadRef (InferT s m) where
  type Ref (InferT s m) = Ref m
  newRef x = liftInfer $ newRef x
  readRef x = liftInfer $ readRef x
  writeRef x y = liftInfer $ writeRef x y

instance MonadAtomicRef m => MonadAtomicRef (InferT s m) where
  atomicModifyRef x f =
    liftInfer $
      do
        res <- snd . f <$> readRef x
        _   <- modifyRef x (fst . f)
        pure res

-- newtype JThunkT s m = JThunk (NThunkF (InferT s m) (Judgment s))

instance Monad m => MonadThrow (InferT s m) where
  throwM = throwError . EvaluationError

instance Monad m => MonadCatch (InferT s m) where
  catch m h =
    catchError m $
      \case
        EvaluationError e ->
          maybe
            (error $ "Exception was not an exception: " <> show e)
            h
            (fromException (toException e))
        err -> error $ "Unexpected error: " <> show err

type MonadInfer m
  = ({- MonadThunkId m,-}
     MonadVar m, MonadFix m)

--  2021-02-22: NOTE: Seems like suporflous instance
instance Monad m => MonadValue (Judgment s) (InferT s m) where
  defer
    :: InferT s m (Judgment s)
    -> InferT s m (Judgment s)
  defer  = id

  demand
    :: Judgment s
    -> InferT s m (Judgment s)
  demand = pure

  inform
    :: Judgment s
    -> InferT s m (Judgment s)
  inform = pure


--  2021-02-22: NOTE: Seems like suporflous instance
instance Monad m => MonadValueF (Judgment s) (InferT s m) where

  demandF
    :: ( Judgment s
      -> InferT s m r)
    -> Judgment s
    -> InferT s m r
  demandF f a = f a

  informF
    :: ( InferT s m (Judgment s)
      -> InferT s m (Judgment s)
      )
    -> Judgment s
    -> InferT s m (Judgment s)
  informF f = f . pure

{-
instance MonadInfer m
  => MonadThunk (JThunkT s m) (InferT s m) (Judgment s) where

  thunkId (JThunk x) = thunkId x

  thunk = fmap JThunk . thunk

  queryM b (JThunk x) = queryM b x

  -- If we have a thunk loop, we just don't know the type.
  force (JThunk t) = catch (force t)
    $ \(_ :: ThunkLoop) ->
                           f =<< Judgment As.empty mempty <$> fresh

  -- If we have a thunk loop, we just don't know the type.
  forceEff (JThunk t) = catch (forceEff t)
    $ \(_ :: ThunkLoop) ->
                           f =<< Judgment As.empty mempty <$> fresh
-}

instance MonadInfer m => MonadEval (Judgment s) (InferT s m) where
  freeVariable var = do
    tv <- fresh
    pure $ Judgment (As.singleton var tv) mempty tv

  synHole var = do
    tv <- fresh
    pure $ Judgment (As.singleton var tv) mempty tv

-- If we fail to look up an attribute, we just don't know the type.
  attrMissing _ _ = Judgment As.empty mempty <$> fresh

  evaledSym _ = pure

  evalCurPos = pure $ Judgment As.empty mempty $ TSet False $ M.fromList
    [("file", typePath), ("line", typeInt), ("col", typeInt)]

  evalConstant c = pure $ Judgment As.empty mempty (go c)
   where
    go = \case
      NURI   _ -> typeString
      NInt   _ -> typeInt
      NFloat _ -> typeFloat
      NBool  _ -> typeBool
      NNull    -> typeNull

  evalString      = const $ pure $ Judgment As.empty mempty typeString
  evalLiteralPath = const $ pure $ Judgment As.empty mempty typePath
  evalEnvPath     = const $ pure $ Judgment As.empty mempty typePath

  evalUnary op (Judgment as1 cs1 t1) = do
    tv <- fresh
    pure $ Judgment as1 (cs1 <> unops (t1 :~> tv) op) tv

  evalBinary op (Judgment as1 cs1 t1) e2 = do
    Judgment as2 cs2 t2 <- e2
    tv                  <- fresh
    pure $ Judgment (as1 `As.merge` as2)
                      (cs1 <> cs2 <> binops (t1 :~> t2 :~> tv) op)
                      tv

  evalWith = Eval.evalWithAttrSet

  evalIf (Judgment as1 cs1 t1) t f = do
    Judgment as2 cs2 t2 <- t
    Judgment as3 cs3 t3 <- f
    pure $ Judgment
      (as1 `As.merge` as2 `As.merge` as3)
      (cs1 <> cs2 <> cs3 <> [EqConst t1 typeBool, EqConst t2 t3])
      t2

  evalAssert (Judgment as1 cs1 t1) body = do
    Judgment as2 cs2 t2 <- body
    pure
      $ Judgment (as1 `As.merge` as2) (cs1 <> cs2 <> [EqConst t1 typeBool]) t2

  evalApp (Judgment as1 cs1 t1) e2 = do
    Judgment as2 cs2 t2 <- e2
    tv                  <- fresh
    pure $ Judgment (as1 `As.merge` as2)
                      (cs1 <> cs2 <> [EqConst t1 (t2 :~> tv)])
                      tv

  evalAbs (Param x) k = do
    a <- freshTVar
    let tv = TVar a
    ((), Judgment as cs t) <- extendMSet
      a
      (k (pure (Judgment (As.singleton x tv) mempty tv)) (\_ b -> ((), ) <$> b))
    pure $ Judgment (as `As.remove` x)
                      (cs <> [ EqConst t' tv | t' <- As.lookup x as ])
                      (tv :~> t)

  evalAbs (ParamSet ps variadic _mname) k = do
    js <-
      concat <$>
        traverse
          (\(name, _) ->
            do
              tv <- fresh
              pure [(name, tv)]
          )
          ps

    let
      (env, tys) =
        (\f -> foldl' f (As.empty, mempty) js) $ \(as1, t1) (k, t) ->
          (as1 `As.merge` As.singleton k t, M.insert k t t1)
      arg   = pure $ Judgment env mempty (TSet True tys)
      call  = k arg $ \args b -> (args, ) <$> b
      names = fmap fst js

    (args, Judgment as cs t) <- foldr (\(_, TVar a) -> extendMSet a) call js

    ty <- TSet variadic <$> traverse (inferredType <$>) args

    pure $
      Judgment
        (foldl' As.remove as names)
        (cs <> [ EqConst t' (tys M.! x) | x <- names, t' <- As.lookup x as ])
        (ty :~> t)

  evalError = throwError . EvaluationError

data Judgment s =
  Judgment
    { assumptions     :: As.Assumption
    , typeConstraints :: [Constraint]
    , inferredType    :: Type
    }
    deriving Show

instance
  Monad m
  => FromValue NixString (InferT s m) (Judgment s)
 where
  fromValueMay _ = pure mempty
  fromValue _ = error "Unused"

instance
  MonadInfer m
  => FromValue ( AttrSet (Judgment s)
              , AttrSet SourcePos
              ) (InferT s m) (Judgment s)
 where
  fromValueMay (Judgment _ _ (TSet _ xs)) =
    do
      let sing _ = Judgment As.empty mempty
      pure $ pure (M.mapWithKey sing xs, mempty)
  fromValueMay _ = pure mempty
  fromValue =
    pure .
      fromMaybe
      (mempty, mempty)
      <=< fromValueMay

instance MonadInfer m
  => ToValue (AttrSet (Judgment s), AttrSet SourcePos)
            (InferT s m) (Judgment s) where
  toValue (xs, _) =
    Judgment
      <$> foldrM go As.empty xs
      <*> (concat <$> traverse ((pure . typeConstraints) <=< demand) xs)
      <*> (TSet True <$> traverse ((pure . inferredType) <=< demand) xs)
   where
    go x rest =
      do
        x' <- demand x
        pure $ As.merge (assumptions x') rest

instance MonadInfer m => ToValue [Judgment s] (InferT s m) (Judgment s) where
  toValue xs =
    Judgment
      <$> foldrM go As.empty xs
      <*> (concat <$> traverse ((pure . typeConstraints) <=< demand) xs)
      <*> (TList <$> traverse ((pure . inferredType) <=< demand) xs)
   where
    go x rest =
      do
        x' <- demand x
        pure $ As.merge (assumptions x') rest

instance MonadInfer m => ToValue Bool (InferT s m) (Judgment s) where
  toValue _ = pure $ Judgment As.empty mempty typeBool

infer :: MonadInfer m => NExpr -> InferT s m (Judgment s)
infer = foldFix Eval.eval

inferTop :: Env -> [(Text, NExpr)] -> Either InferError Env
inferTop env []                = pure env
inferTop env ((name, ex) : xs) =
  either
    Left
    (\ ty -> inferTop (extend env (name, ty)) xs)
    (inferExpr env ex)

normalizeScheme :: Scheme -> Scheme
normalizeScheme (Forall _ body) = Forall (fmap snd ord) (normtype body)
 where
  ord = zip (ordNub $ fv body) (fmap (TV . toText) letters)

  fv (TVar a  ) = [a]
  fv (a :~> b ) = fv a <> fv b
  fv (TCon _  ) = mempty
  fv (TSet _ a) = concatMap fv (M.elems a)
  fv (TList a ) = concatMap fv a
  fv (TMany ts) = concatMap fv ts

  normtype (a :~> b ) = normtype a :~> normtype b
  normtype (TCon a  ) = TCon a
  normtype (TSet b a) = TSet b (M.map normtype a)
  normtype (TList a ) = TList (fmap normtype a)
  normtype (TMany ts) = TMany (fmap normtype ts)
  normtype (TVar  a ) =
    maybe
      (error "type variable not in signature")
      TVar
      (List.lookup a ord)


-- * Constraint Solver

newtype Solver m a = Solver (LogicT (StateT [TypeError] m) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus,
              MonadLogic, MonadState [TypeError])

instance MonadTrans Solver where
  lift = Solver . lift . lift

instance Monad m => MonadError TypeError (Solver m) where
  throwError err = Solver $ lift (modify (err :)) *> empty
  catchError _ _ = error "This is never used"

runSolver :: Monad m => Solver m a -> m (Either [TypeError] [a])
runSolver (Solver s) = do
  res <- runStateT (observeAllT s) mempty
  pure $ case res of
    (x : xs, _ ) -> pure (x : xs)
    (_     , es) -> Left (ordNub es)

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
Subst s1 `compose` Subst s2 =
  Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

unifyMany :: Monad m => [Type] -> [Type] -> Solver m Subst
unifyMany []         []         = pure emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  pure (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

allSameType :: [Type] -> Bool
allSameType []           = True
allSameType [_         ] = True
allSameType (x : y : ys) = x == y && allSameType (y : ys)

unifies :: Monad m => Type -> Type -> Solver m Subst
unifies t1 t2 | t1 == t2  = pure emptySubst
unifies (TVar v) t        = v `bind` t
unifies t        (TVar v) = v `bind` t
unifies (TList xs) (TList ys)
  | allSameType xs && allSameType ys = case (xs, ys) of
    (x : _, y : _) -> unifies x y
    _              -> pure emptySubst
  | length xs == length ys = unifyMany xs ys
-- We assume that lists of different lengths containing various types cannot
-- be unified.
unifies t1@(TList _    ) t2@(TList _    ) = throwError $ UnificationFail t1 t2
unifies (   TSet True _) (   TSet True _) = pure emptySubst
unifies (TSet False b) (TSet True s)
  | M.keys b `intersect` M.keys s == M.keys s = pure emptySubst
unifies (TSet True s) (TSet False b)
  | M.keys b `intersect` M.keys s == M.keys b = pure emptySubst
unifies (TSet False s) (TSet False b) | null (M.keys b \\ M.keys s) =
  pure emptySubst
unifies (t1 :~> t2) (t3 :~> t4) = unifyMany [t1, t2] [t3, t4]
unifies (TMany t1s) t2          = considering t1s >>- unifies ?? t2
unifies t1          (TMany t2s) = considering t2s >>- unifies t1
unifies t1          t2          = throwError $ UnificationFail t1 t2

bind :: Monad m => TVar -> Type -> Solver m Subst
bind a t | t == TVar a     = pure emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = pure (Subst $ Map.singleton a t)

occursCheck :: FreeTypeVars a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable xs = fromJust (find solvable (chooseOne xs))
 where
  chooseOne xs = [ (x, ys) | x <- xs, let ys = delete x xs ]

  solvable (EqConst{}     , _) = True
  solvable (ExpInstConst{}, _) = True
  solvable (ImpInstConst _t1 ms t2, cs) =
    Set.null ((ftv t2 `Set.difference` ms) `Set.intersection` atv cs)

considering :: [a] -> Solver m a
considering xs = Solver $ LogicT $ \c n -> foldr c n xs

solve :: MonadState InferState m => [Constraint] -> Solver m Subst
solve [] = pure emptySubst
solve cs = solve' (nextSolvable cs)
 where
  solve' (EqConst t1 t2, cs) = unifies t1 t2
    >>- \su1 -> solve (apply su1 cs) >>- \su2 -> pure (su2 `compose` su1)

  solve' (ImpInstConst t1 ms t2, cs) =
    solve (ExpInstConst t1 (generalize ms t2) : cs)

  solve' (ExpInstConst t s, cs) = do
    s' <- lift $ instantiate s
    solve (EqConst t s' : cs)

instance
  Monad m
  => Scoped (Judgment s) (InferT s m) where
  currentScopes = currentScopesReader
  clearScopes   = clearScopesReader @(InferT s m) @(Judgment s)
  pushScopes    = pushScopesReader
  lookupVar     = lookupVarReader
