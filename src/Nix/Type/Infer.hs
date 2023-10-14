{-# language MultiWayIf #-}
{-# language CPP #-}
{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language ExistentialQuantification #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

{-# options_ghc -Wno-name-shadowing #-}

module Nix.Type.Infer
  ( Constraint(..)
  , TypeError(..)
  , InferError(..)
  , Subst(..)
  , inferTop
  )
where

import           Nix.Prelude             hiding ( Constraint
                                                , Type
                                                , TVar
                                                )
import           Control.Monad.Catch            ( MonadThrow(..)
                                                , MonadCatch(..)
                                                )
import           Control.Monad.Except           ( MonadError(throwError,catchError) )
import           Control.Monad.Logic
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Ref              ( MonadAtomicRef(..)
                                                , MonadRef(..)
                                                )
import           Control.Monad.ST               ( ST
                                                , runST
                                                )
import           Data.Fix                       ( foldFix )
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
                                                ( eval
                                                , evalWithAttrSet
                                                )
import           Nix.Expr.Types
import           Nix.Fresh
import           Nix.String
import           Nix.Scope
import           Nix.Type.Assumption     hiding ( extend )
import qualified Nix.Type.Assumption           as Assumption
import           Nix.Type.Env
import qualified Nix.Type.Env                  as Env
import           Nix.Type.Type
import           Nix.Value.Monad


normalizeScheme :: Scheme -> Scheme
normalizeScheme (Forall _ body) = Forall (snd <$> ord) (normtype body)
 where
  ord =
    zip
      (ordNub $ fv body)
      (TV . fromString <$> letters)

  fv (TVar a  ) = one a
  fv (a :~> b ) = on (<>) fv a b
  fv (TCon _  ) = mempty
  fv (TSet _ a) = foldMap fv $ M.elems a
  fv (TList a ) = foldMap fv a
  fv (TMany ts) = foldMap fv ts

  normtype (a :~> b ) = normtype a :~> normtype b
  normtype (TCon a  ) = TCon a
  normtype (TSet b a) = TSet b $ normtype <$> a
  normtype (TList a ) = TList $ normtype <$> a
  normtype (TMany ts) = TMany $ normtype <$> ts
  normtype (TVar  a ) =
    maybe
      (error "type variable not in signature")
      TVar
      (List.lookup a ord)

generalize :: Set.Set TVar -> Type -> Scheme
generalize free t = Forall as t
 where
  as = Set.toList $ free `Set.difference` ftv t

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalizeScheme . generalize mempty

-- When `[]` becomes `NonEmpty` - function becomes just `all`
-- | Check if all elements are of the same type.
allSameType :: [Type] -> Bool
allSameType = allSame
 where
  allSame :: Eq a => [a] -> Bool
  allSame [] = True
  allSame (x:xs) = all (x ==) xs

-- * data type @TypeError@

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariables [VarName]
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  deriving (Eq, Show, Ord)

-- * @InferError@

data InferError
  = TypeInferenceErrors [TypeError]
  | TypeInferenceAborted
  | forall s. Exception s => EvaluationError s

typeError :: MonadError InferError m => TypeError -> m ()
typeError err = throwError $ TypeInferenceErrors $ one err

-- ** Instances

deriving instance Show InferError
instance Exception InferError

instance Semigroup InferError where
  (<>) = const

instance Monoid InferError where
  mempty  = TypeInferenceAborted

-- * @InferState@: inference state

-- | Inference state (stage).
newtype InferState = InferState Int
 deriving
  (Eq, Num, Enum, Ord)

instance Semigroup InferState where
  (<>) = (+)

instance Monoid InferState where
  mempty = 0

-- | Initial inference state
initInfer :: InferState
initInfer = InferState 0

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
    put $ succ s
    pure $ TV $ fromString $ letters !! coerce s

fresh :: MonadState InferState m => m Type
fresh = TVar <$> freshTVar

intoFresh :: (Traversable t, MonadState InferState f) => t a -> f (t Type)
intoFresh =
  traverse (const fresh)

instantiate :: MonadState InferState m => Scheme -> m Type
instantiate (Forall as t) =
  fmap ((`apply` t) . coerce . Map.fromList . zip as) (intoFresh as)

-- * @Constraint@ data type

data Constraint
  = EqConst Type Type
  | ExpInstConst Type Scheme
  | ImpInstConst Type (Set.Set TVar) Type
  deriving (Show, Eq, Ord)

-- * @Subst@ data type

-- | Substitution of the basic type definition by a type variable.
newtype Subst = Subst (Map TVar Type)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
compose a@(Subst s2) (Subst s1) =
  coerce $ --
    apply a <$>
      (s2 <> s1)

-- * class @Substitutable@

class Substitutable a where
  apply :: Subst -> a -> a

-- ** Instances

instance Substitutable TVar where
  apply (Subst s) a = tv
   where
    (TVar tv) = Map.findWithDefault (TVar a) a s

instance Substitutable Type where
  apply _         (  TCon a   ) = TCon a
  apply s         (  TSet b a ) = TSet b $ apply s <$> a
  apply s         (  TList a  ) = TList  $ apply s <$> a
  apply (Subst s) t@(TVar  a  ) = Map.findWithDefault t a s
  apply s         (  t1 :~> t2) = ((:~>) `on` apply s) t1 t2
  apply s         (  TMany ts ) = TMany  $ apply s <$> ts

instance Substitutable Scheme where
  apply (Subst s) (Forall as t) = Forall as $ apply s' t
   where
    s' = Subst $ foldr Map.delete s as

instance Substitutable Constraint where
  apply s (EqConst      t1 t2) = on EqConst (apply s) t1 t2
  apply s (ExpInstConst t  sc) =
    ExpInstConst
      (apply s t)
      (apply s sc)
  apply s (ImpInstConst t1 ms t2) =
    ImpInstConst
      (apply s t1)
      (apply s ms)
      (apply s t2)

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply

instance (Ord a, Substitutable a) => Substitutable (Set.Set a) where
  apply = Set.map . apply


-- * data type @Judgment@

data Judgment s =
  Judgment
    { assumptions     :: Assumption
    , typeConstraints :: [Constraint]
    , inferredType    :: Type
    }
    deriving Show

inferred :: Type -> Judgment s
inferred = Judgment mempty mempty

-- * @InferT@: inference monad

type InferTInternals s m a =
  ReaderT
    (Set.Set TVar, Scopes (InferT s m) (Judgment s))
    (StateT InferState (ExceptT InferError m))
    a

-- | Inference monad
newtype InferT s m a =
  InferT
    { getInfer ::
        InferTInternals s m a
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

extendMSet :: forall s m a . Monad m => TVar -> InferT s m a -> InferT s m a
extendMSet x = coerce putSetElementM
 where
  putSetElementM :: InferTInternals s m a -> InferTInternals s m a
  putSetElementM = local (first . Set.insert $ x)

-- ** Instances

instance MonadTrans (InferT s) where
  lift = InferT . lift . lift . lift

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
        _   <- modifyRef x $ fst . f
        pure res

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
            (fromException $ toException e)
        err -> error $ "Unexpected error: " <> show err

-- instance MonadThunkId m => MonadThunkId (InferT s m) where
--   type ThunkId (InferT s m) = ThunkId m

instance
  Monad m
  => FromValue NixString (InferT s m) (Judgment s)
 where
  fromValueMay _ = stub
  fromValue _ = error "Unused"

instance
  MonadInfer m
  => FromValue ( AttrSet (Judgment s)
              , PositionSet
              ) (InferT s m) (Judgment s)
 where
  fromValueMay (Judgment _ _ (TSet _ xs)) =
    do
      let sing = const inferred
      pure $ pure (M.mapWithKey sing xs, mempty)
  fromValueMay _ = stub
  fromValue =
    pure .
      maybeToMonoid
      <=< fromValueMay

foldInitializedWith :: (Traversable t, Applicative f) => (t c -> c) -> (b -> c) -> (a -> f b) -> t a -> f c
foldInitializedWith fld getter init =
  -- maybe here is some law?
  fmap fld . traverse (fmap getter . init)

toJudgment :: forall t m s . (Traversable t, Monad m) => (t Type -> Type) -> t (Judgment s) -> InferT s m (Judgment s)
toJudgment c xs =
  liftA3 Judgment
    (foldWith fold assumptions    )
    (foldWith fold typeConstraints)
    (foldWith c    inferredType   )
   where
    foldWith :: (t a -> a) -> (Judgment s -> a) -> InferT s m a
    foldWith g f = foldInitializedWith g f demand xs

instance MonadInfer m
  => ToValue (AttrSet (Judgment s), PositionSet)
            (InferT s m) (Judgment s) where
  toValue :: (AttrSet (Judgment s), PositionSet) -> InferT s m (Judgment s)
  toValue (xs, _) = toJudgment (TSet Variadic) xs -- why variadic? Probably `Closed` (`mempty`)?

instance MonadInfer m => ToValue [Judgment s] (InferT s m) (Judgment s) where
  toValue = toJudgment TList

instance MonadInfer m => ToValue Bool (InferT s m) (Judgment s) where
  toValue _ = pure $ inferred typeBool

instance
  Monad m
  => Scoped (Judgment s) (InferT s m) where
  askScopes   = askScopesReader
  clearScopes = clearScopesReader @(InferT s m) @(Judgment s)
  pushScopes  = pushScopesReader
  lookupVar   = lookupVarReader

-- newtype JThunkT s m = JThunk (NThunkF (InferT s m) (Judgment s))

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
  demandF f = f

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

  query b (JThunk x) = query b x

  -- If we have a thunk loop, we just don't know the type.
  force (JThunk t) = catch (force t)
    $ \(_ :: ThunkLoop) ->
                           f =<< Judgment mempty mempty <$> fresh

  -- If we have a thunk loop, we just don't know the type.
  forceEff (JThunk t) = catch (forceEff t)
    $ \(_ :: ThunkLoop) ->
                           f =<< Judgment mempty mempty <$> fresh
-}

polymorphicVar :: MonadInfer m => VarName -> InferT s m (Judgment s)
polymorphicVar var =
  fmap
    (join $ (`Judgment` mempty) . curry one var)
    fresh

constInfer :: Applicative f => Type -> b -> f (Judgment s)
constInfer x = const $ pure $ inferred x

instance MonadInfer m => MonadEval (Judgment s) (InferT s m) where
  freeVariable = polymorphicVar

  synHole = polymorphicVar

  -- If we fail to look up an attribute, we just don't know the type.
  attrMissing _ _ = inferred <$> fresh

  evaledSym _ = pure

  evalCurPos =
    pure $
      inferred $
        TSet mempty $
          M.fromList
            [ ("file", typePath)
            , ("line", typeInt )
            , ("col" , typeInt )
            ]

  evalConstant c = pure $ inferred $ fun c
   where
    fun = \case
      NURI   _ -> typeString
      NInt   _ -> typeInt
      NFloat _ -> typeFloat
      NBool  _ -> typeBool
      NNull    -> typeNull

  evalString      = constInfer typeString
  evalLiteralPath = constInfer typePath
  evalEnvPath     = constInfer typePath

  evalUnary op (Judgment as1 cs1 t1) =
    (Judgment as1 =<< (cs1 <>) . (`unops` op) . (t1 :~>)) <$> fresh

  evalBinary op (Judgment as1 cs1 t1) e2 =
    do
      Judgment as2 cs2 t2 <- e2
      (Judgment (as1 <> as2) =<< ((cs1 <> cs2) <>) . (`binops` op) . ((t1 :~> t2) :~>)) <$> fresh

  evalWith = Eval.evalWithAttrSet

  evalIf (Judgment as1 cs1 t1) t f = do
    Judgment as2 cs2 t2 <- t
    Judgment as3 cs3 t3 <- f
    pure $
      Judgment
        (as1 <> as2 <> as3)
        (cs1 <> cs2 <> cs3 <> [EqConst t1 typeBool, EqConst t2 t3])
        t2

  evalAssert (Judgment as1 cs1 t1) body = do
    Judgment as2 cs2 t2 <- body
    pure $
      Judgment
        (as1 <> as2)
        (cs1 <> cs2 <> one (EqConst t1 typeBool))
        t2

  evalApp (Judgment as1 cs1 t1) e2 = do
    Judgment as2 cs2 t2 <- e2
    tv                  <- fresh
    pure $
      Judgment
        (as1 <> as2)
        (cs1 <> cs2 <> one (EqConst t1 (t2 :~> tv)))
        tv

  evalAbs (Param x) k = do
    a <- freshTVar
    let tv = TVar a
    ((), Judgment as cs t) <-
      extendMSet
        a
        $ k
          (pure (join ((`Judgment` mempty) . curry one x ) tv))
          $ const $ fmap (mempty,)
    pure $
      Judgment
        (as `Assumption.remove` x)
        (cs <> [ EqConst t' tv | t' <- Assumption.lookup x as ])
        (tv :~> t)

  evalAbs (ParamSet _mname variadic pset) k = do
    js <- foldInitializedWith fold one intoFresh pset

    let
      f (as1, t1) (k, t) = (as1 <> one (k, t), M.insert k t t1)
      (env, tys) = foldl' f mempty js
      arg   = pure $ Judgment env mempty $ TSet Variadic tys
      call  = k arg $ \args b -> (args, ) <$> b
      names = fst <$> js

    (args, Judgment as cs t) <- foldr (extendMSet . (\ (TVar a) -> a) . snd) call js

    ty <- foldInitializedWith (TSet variadic) inferredType id args

    pure $
      Judgment
        (foldl' Assumption.remove as names)
        (cs <> [ EqConst t' (tys M.! x) | x <- names, t' <- Assumption.lookup x as ])
        (ty :~> t)

  evalError = throwError . EvaluationError

-- * class @FreeTypeVars@

class FreeTypeVars a where
  ftv :: a -> Set.Set TVar

occursCheck :: FreeTypeVars a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- ** Instances

instance FreeTypeVars Type where
  ftv TCon{}      = mempty
  ftv (TVar a   ) = one a
  ftv (TSet _ a ) = Set.unions $ ftv <$> M.elems a
  ftv (TList a  ) = Set.unions $ ftv <$> a
  ftv (t1 :~> t2) = ftv t1 <> ftv t2
  ftv (TMany ts ) = Set.unions $ ftv <$> ts

instance FreeTypeVars TVar where
  ftv = one

instance FreeTypeVars Scheme where
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance FreeTypeVars a => FreeTypeVars [a] where
  ftv = foldr ((<>) . ftv) mempty

instance (Ord a, FreeTypeVars a) => FreeTypeVars (Set.Set a) where
  ftv = foldr ((<>) . ftv) mempty

-- * class @ActiveTypeVars@

class ActiveTypeVars a where
  atv :: a -> Set.Set TVar

-- ** Instances

instance ActiveTypeVars Constraint where
  atv (EqConst      t1 t2   ) = ftv t1 <> ftv t2
  atv (ImpInstConst t1 ms t2) = ftv t1 <> (ftv ms `Set.intersection` ftv t2)
  atv (ExpInstConst t  s    ) = ftv t  <> ftv s

instance ActiveTypeVars a => ActiveTypeVars [a] where
  atv = foldr ((<>) . atv) mempty

-- * Other

type MonadInfer m
  = ({- MonadThunkId m,-}
     MonadAtomicRef m, MonadFix m)

-- | Run the inference monad
runInfer' :: MonadInfer m => InferT s m a -> m (Either InferError a)
runInfer' =
  runExceptT
    . (`evalStateT` initInfer)
    . (`runReaderT` mempty)
    . getInfer

runInfer :: (forall s . InferT s (FreshIdT Int (ST s)) a) -> Either InferError a
runInfer m =
  runST $ runFreshIdT (runInfer' m) =<< newRef (1 :: Int)

inferType
  :: forall s m . MonadInfer m => Env -> NExpr -> InferT s m [(Subst, Type)]
inferType env ex =
  do
    Judgment as cs t <- infer ex
    let
      unbounds :: Set VarName
      unbounds =
        (Set.difference `on` Set.fromList)
          (Assumption.keys as )
          (       Env.keys env)
    when
      (isPresent unbounds)
      $ typeError $ UnboundVariables $ ordNub $ Set.toList unbounds

    inferState <- get
    let
      cs' =
        [ ExpInstConst t s
            | (x, ss) <- Env.toList env
            , s       <- ss
            , t       <- Assumption.lookup x as
        ]
      evalResult =
        (`evalState` inferState) . runSolver $ second (`apply` t) . join (,) <$> solve (cs <> cs')

    either
      (throwError . TypeInferenceErrors)
      pure
      evalResult

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Env -> NExpr -> Either InferError [Scheme]
inferExpr env ex =
  closeOver . uncurry apply <<$>> runInfer (inferType env ex)

unops :: Type -> NUnaryOp -> [Constraint]
unops u1 op =
  one $
    EqConst u1 $
      case op of
        NNot -> mkUnaryConstr typeBool
        NNeg -> TMany $ mkUnaryConstr <$> [typeInt, typeFloat]
 where
  mkUnaryConstr :: Type -> Type
  mkUnaryConstr = typeFun . mk2same
   where
    mk2same :: a -> NonEmpty a
    mk2same a = a :| one a

binops :: Type -> NBinaryOp -> [Constraint]
binops u1 op =
  if
    -- Equality tells nothing about the types, because any two types are allowed.
    | op `elem` [ NEq   , NNEq               ] -> mempty
    | op `elem` [ NGt   , NGte , NLt  , NLte ] -> inequality
    | op `elem` [ NAnd  , NOr  , NImpl       ] -> gate
    | op ==       NConcat                      -> concatenation
    | op `elem` [ NMinus, NMult, NDiv        ] -> arithmetic
    | op ==       NUpdate                      -> rUnion
    | op ==       NPlus                        -> addition
    | otherwise -> fail "GHC so far can not infer that this pattern match is full, so make it happy."

 where

  mk3 :: a -> a -> a -> NonEmpty a
  mk3 a b c = a :| [b, c]

  mk3same :: a -> NonEmpty a
  mk3same a = a :| [a, a]

  allConst :: Type -> [Constraint]
  allConst = one . EqConst u1 . typeFun . mk3same

  gate          = allConst typeBool
  concatenation = allConst typeList

  eqConstrMtx :: [NonEmpty Type] -> [Constraint]
  eqConstrMtx = one . EqConst u1 . TMany . fmap typeFun

  inequality =
    eqConstrMtx
      [ mk3 typeInt   typeInt   typeBool
      , mk3 typeFloat typeFloat typeBool
      , mk3 typeInt   typeFloat typeBool
      , mk3 typeFloat typeInt   typeBool
      ]

  arithmetic =
    eqConstrMtx
      [ mk3same typeInt
      , mk3same typeFloat
      , mk3 typeInt   typeFloat typeFloat
      , mk3 typeFloat typeInt   typeFloat
      ]

  rUnion =
    eqConstrMtx
      [ mk3same typeSet
      , mk3 typeSet  typeNull typeSet
      , mk3 typeNull typeSet  typeSet
      ]

  addition =
    eqConstrMtx
      [ mk3same typeInt
      , mk3same typeFloat
      , mk3 typeInt    typeFloat  typeFloat
      , mk3 typeFloat  typeInt    typeFloat
      , mk3same typeString
      , mk3same typePath
      , mk3 typeString typeString typePath
      ]

liftInfer :: Monad m => m a -> InferT s m a
liftInfer = InferT . lift . lift . lift

-- * Other

infer :: MonadInfer m => NExpr -> InferT s m (Judgment s)
infer = foldFix Eval.eval

inferTop :: Env -> [(VarName, NExpr)] -> Either InferError Env
inferTop env []                = pure env
inferTop env ((name, ex) : xs) =
  (\ ty -> inferTop (extend env (name, ty)) xs)
    =<< inferExpr env ex

-- * Other

newtype Solver m a = Solver (LogicT (StateT [TypeError] m) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus,
              MonadLogic, MonadState [TypeError])

runSolver :: forall m a . Monad m => Solver m a -> m (Either [TypeError] [a])
runSolver (Solver s) =
  uncurry report <$> runStateT (observeAllT s) mempty
 where
  report :: [a] -> [TypeError] -> Either [TypeError] [a]
  report xs e =
    handlePresence
      (Left $ ordNub e)
      pure
      xs

-- ** Instances

instance MonadTrans Solver where
  lift = Solver . lift . lift

instance Monad m => MonadError TypeError (Solver m) where
  throwError err = Solver $ lift (modify (err :)) *> mempty
  catchError _ _ = error "This is never used"

-- * Other

bind :: Monad m => TVar -> Type -> Solver m Subst
bind a t | t == TVar a     = stub
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = pure $ Subst $ one (a, t)

considering :: [a] -> Solver m a
considering xs = Solver $ LogicT $ \c n -> foldr c n xs

unifies :: Monad m => Type -> Type -> Solver m Subst
unifies t1 t2 | t1 == t2  = stub
unifies (TVar v) t        = v `bind` t
unifies t        (TVar v) = v `bind` t
unifies (TList xs) (TList ys)
  | allSameType xs && allSameType ys =
      case (xs, ys) of
        (x : _, y : _) -> unifies x y
        _              -> stub
  | length xs == length ys = unifyMany xs ys
-- Putting a statement that lists of different lengths containing various types would not
-- be unified.
unifies t1@(TList _    ) t2@(TList _    ) = throwError $ UnificationFail t1 t2
unifies (TSet Variadic _) (TSet Variadic _)                                 = stub
unifies (TSet Closed   s) (TSet Closed   b) | null (M.keys b \\ M.keys s)   = stub
unifies (TSet _ a) (TSet _ b) | (M.keys a `intersect` M.keys b) == M.keys b = stub
unifies (t1 :~> t2) (t3 :~> t4) = unifyMany [t1, t2] [t3, t4]
unifies (TMany t1s) t2          = considering t1s >>- (`unifies` t2)
unifies t1          (TMany t2s) = considering t2s >>- unifies t1
unifies t1          t2          = throwError $ UnificationFail t1 t2

unifyMany :: Monad m => [Type] -> [Type] -> Solver m Subst
unifyMany []         []         = stub
unifyMany (t1 : ts1) (t2 : ts2) =
  do
    su1 <- unifies t1 t2
    su2 <-
      (unifyMany `on` apply su1) ts1 ts2
    pure $ compose su1 su2
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable = fromJust . find solvable . pickFirstOne
 where
  pickFirstOne :: Eq a => [a] -> [(a, [a])]
  pickFirstOne xs = [ (x, ys) | x <- xs, let ys = delete x xs ]

  solvable :: (Constraint, [Constraint]) -> Bool
  solvable (EqConst{}     , _) = True
  solvable (ExpInstConst{}, _) = True
  solvable (ImpInstConst _t1 ms t2, cs) =
    null $ (ms `Set.difference` ftv t2) `Set.intersection` atv cs

solve :: forall m . MonadState InferState m => [Constraint] -> Solver m Subst
solve [] = stub
solve cs = solve' $ nextSolvable cs
 where
  solve' (ImpInstConst t1 ms t2, cs) =
    solve (ExpInstConst t1 (generalize ms t2) : cs)
  solve' (ExpInstConst t s, cs) =
    do
      s' <- lift $ instantiate s
      solve (EqConst t s' : cs)
  solve' (EqConst t1 t2, cs) =
    (\ su1 ->
      (pure . compose su1) -<< solve ((`apply` cs) su1)
    ) -<<
    unifies t1 t2

infixr 1 -<<
-- | @LogicT@ fair conjunction, since library has only @>>-@
(-<<) :: Monad m => (a -> Solver m b) -> Solver m a -> Solver m b
(-<<) = flip (>>-)
