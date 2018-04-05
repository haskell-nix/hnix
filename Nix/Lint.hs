{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Lint where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Coerce
import           Data.Fix
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashMap.Strict.InsOrd as OM
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import           Nix.Atoms
import           Nix.Eval
import           Nix.Expr
import           Nix.Scope
import           Nix.Stack
import           Nix.Thunk
import           Nix.Utils

data TAtom
  = TInt
  | TFloat
  | TBool
  | TNull
  | TUri
  deriving (Show, Eq, Ord)

data NTypeF (m :: * -> *) r
    = TConstant [TAtom]
    | TStr
    | TList r
    | TSet (Maybe (HashMap Text r))
    | TClosure (Scopes m r) (Params (m r)) (m r)
    | TPath
    | TBuiltin String (Symbolic m -> m r)
    deriving Functor

compareTypes :: NTypeF m r -> NTypeF m r -> Ordering
compareTypes (TConstant _)   (TConstant _)   = EQ
compareTypes (TConstant _)   _               = LT
compareTypes _               (TConstant _)   = GT
compareTypes TStr            TStr            = EQ
compareTypes TStr            _               = LT
compareTypes _               TStr            = GT
compareTypes (TList _)       (TList _)       = EQ
compareTypes (TList _)       _               = LT
compareTypes _               (TList _)       = GT
compareTypes (TSet _)        (TSet _)        = EQ
compareTypes (TSet _)        _               = LT
compareTypes _               (TSet _)        = GT
compareTypes TClosure {}     TClosure {}     = EQ
compareTypes TClosure {}     _               = LT
compareTypes _               TClosure {}     = GT
compareTypes TPath           TPath           = EQ
compareTypes TPath            _              = LT
compareTypes _               TPath           = GT
compareTypes (TBuiltin _ _)  (TBuiltin _ _)  = EQ

data NSymbolicF r
    = NAny
    | NMany [r]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

newtype SThunk m = SThunk { getSThunk :: Thunk m (Symbolic m) }

sthunk :: MonadVar m => m (Symbolic m) -> m (SThunk m)
sthunk = fmap coerce . buildThunk

sforce :: (Framed e m, MonadFile m, MonadVar m)
       => SThunk m -> (Symbolic m -> m r) -> m r
sforce = forceThunk . coerce

svalueThunk :: forall m. Symbolic m -> SThunk m
svalueThunk = coerce . valueRef @_ @m

type Symbolic m = Var m (NSymbolicF (NTypeF m (SThunk m)))

everyPossible :: MonadVar m => m (Symbolic m)
everyPossible = packSymbolic NAny

mkSymbolic :: MonadVar m => [NTypeF m (SThunk m)] -> m (Symbolic m)
mkSymbolic xs = packSymbolic (NMany xs)

packSymbolic :: MonadVar m
             => NSymbolicF (NTypeF m (SThunk m)) -> m (Symbolic m)
packSymbolic = newVar

unpackSymbolic :: MonadVar m
               => Symbolic m -> m (NSymbolicF (NTypeF m (SThunk m)))
unpackSymbolic = readVar

renderSymbolic :: MonadLint e m
               => Symbolic m -> m String
renderSymbolic = unpackSymbolic >=> \case
    NAny -> return "<any>"
    NMany xs -> fmap (intercalate ", ") $ forM xs $ \case
        TConstant ys  -> fmap (intercalate ", ") $ forM ys $ \case
            TInt   -> return "int"
            TFloat -> return "float"
            TBool  -> return "bool"
            TNull  -> return "null"
            TUri   -> return "uri"
        TStr            -> return "string"
        TList r         -> do
            x <- sforce r renderSymbolic
            return $ "[" ++ x ++ "]"
        TSet Nothing    -> return "<any set>"
        TSet (Just s)   -> do
            x <- traverse (`sforce` renderSymbolic) s
            return $ "{" ++ show x ++ "}"
        f@(TClosure s p _) -> do
            (args, sym) <-
                lintApp (NAbs (void p) ()) (mkSymbolic [f]) everyPossible
            args' <- traverse renderSymbolic args
            sym'  <- renderSymbolic sym
            return $ "(" ++ show s ++ " over " ++ show args'
                ++ " -> " ++ sym' ++ ")"
        TPath           -> return "path"
        TBuiltin _n _f    -> return "<builtin function>"

-- This function is order and uniqueness preserving (of types).
merge :: forall e m. MonadLint e m
      => NExprF () -> [NTypeF m (SThunk m)] -> [NTypeF m (SThunk m)]
      -> m [NTypeF m (SThunk m)]
merge context = go
  where
    go :: [NTypeF m (SThunk m)] -> [NTypeF m (SThunk m)]
       -> m [NTypeF m (SThunk m)]
    go [] _ = return []
    go _ [] = return []
    go (x:xs) (y:ys) = case (x, y) of
        (TStr,  TStr)  -> (TStr :)  <$> go xs ys
        (TPath, TPath) -> (TPath :) <$> go xs ys
        (TConstant ls, TConstant rs) ->
            (TConstant (ls `intersect` rs) :) <$> go xs ys
        (TList l, TList r) -> sforce l $ \l' -> sforce r $ \r' -> do
            m <- sthunk $ unify context l' r'
            (TList m :) <$> go xs ys
        (TSet x, TSet Nothing) -> (TSet x :) <$> go xs ys
        (TSet Nothing, TSet x) -> (TSet x :) <$> go xs ys
        (TSet (Just l), TSet (Just r)) -> do
            m <- sequenceA $ M.intersectionWith
                (\i j -> i >>= \i' -> j >>= \j' ->
                        sforce i' $ \i'' -> sforce j' $ \j'' ->
                            sthunk $ unify context i'' j'')
                (return <$> l) (return <$> r)
            if M.null m
                then go xs ys
                else (TSet (Just m) :) <$> go xs ys
        (TClosure {}, TClosure {}) ->
            throwError "Cannot unify functions"
        (TBuiltin _ _, TBuiltin _ _) ->
            throwError "Cannot unify builtin functions"
        _ | compareTypes x y == LT -> go xs (y:ys)
          | compareTypes x y == GT -> go (x:xs) ys
          | otherwise              -> error "impossible"

{-
    mergeFunctions pl nl fl pr fr xs ys = do
        m <- sequenceA $ M.intersectionWith
            (\i j -> i >>= \i' -> j >>= \j' -> case (i', j') of
                    (Nothing, Nothing) -> return $ Just Nothing
                    (_, Nothing) -> return Nothing
                    (Nothing, _) -> return Nothing
                    (Just i'', Just j'') ->
                        Just . Just <$> unify context i'' j'')
            (return <$> pl) (return <$> pr)
        let Just m' = sequenceA $ M.filter isJust m
        if M.null m'
            then go xs ys
            else do
                g <- unify context fl fr
                (TClosure (ParamSet m' False nl) g :)
                    <$> go xs ys
-}

type MonadLint e m =
    ( Scoped e (SThunk m) m
    , Framed e m
    , MonadExpr (SThunk m) (Symbolic m) m
    , MonadFix m
    , MonadFile m
    , MonadVar m
    )

-- | unify raises an error if the result is would be 'NMany []'.
unify :: MonadLint e m
      => NExprF () -> Symbolic m -> Symbolic m -> m (Symbolic m)
unify context x y = do
    x' <- readVar x
    y' <- readVar y
    case (x', y') of
        (NAny, _) -> do
            writeVar x y'
            return y
        (_, NAny) -> do
            writeVar y x'
            return x
        (NMany xs, NMany ys) -> do
            m <- merge context xs ys
            if null m
                then do
                    x' <- renderSymbolic x
                    y' <- renderSymbolic y
                    throwError $ "Cannot unify "
                        ++ show x' ++ " with " ++ show y'
                         ++ " in context: " ++ show context
                else do
                    writeVar x (NMany m)
                    writeVar y (NMany m)
                    packSymbolic (NMany m)

lintExpr :: MonadLint e m
         => NExpr -> m (Symbolic m)
lintExpr = cata lint

lint :: forall e m. MonadLint e m
     => NExprF (m (Symbolic m)) -> m (Symbolic m)

lint (NSym var) = do
    mres <- lookupVar var
    case mres of
        Nothing -> throwError $ "Undefined variable: " ++ show var
        Just v  -> sforce v pure

lint (NConstant c) = mkSymbolic [TConstant [t]]
  where
      t = case c of
          NInt _   -> TInt
          NFloat _ -> TFloat
          NBool _  -> TBool
          NNull    -> TNull
          NUri _   -> TUri

lint (NStr _)         = mkSymbolic [TStr]
lint (NLiteralPath _) = mkSymbolic [TPath]
lint (NEnvPath _)     = mkSymbolic [TPath]

lint e@(NUnary _op arg) =
    join $ unify (void e) <$> arg <*> mkSymbolic [TConstant [TInt, TBool]]

lint e@(NBinary op larg rarg) = do
    lsym <- larg
    rsym <- rarg
    y <- sthunk everyPossible
    case op of
        NEq    -> check lsym rsym [ TConstant [TInt, TBool, TNull, TUri]
                                 , TStr
                                 , TList y ]
        NNEq   -> check lsym rsym [ TConstant [TInt, TBool, TNull, TUri]
                                 , TStr
                                 , TList y ]

        NLt    -> check lsym rsym [ TConstant [TInt, TBool, TNull, TUri] ]
        NLte   -> check lsym rsym [ TConstant [TInt, TBool, TNull, TUri] ]
        NGt    -> check lsym rsym [ TConstant [TInt, TBool, TNull, TUri] ]
        NGte   -> check lsym rsym [ TConstant [TInt, TBool, TNull, TUri] ]

        NAnd   -> check lsym rsym [ TConstant [TBool] ]
        NOr    -> check lsym rsym [ TConstant [TBool] ]
        NImpl  -> check lsym rsym [ TConstant [TBool] ]

        -- jww (2018-04-01): NYI: Allow Path + Str
        NPlus  -> check lsym rsym [ TConstant [TInt], TStr, TPath ]
        NMinus -> check lsym rsym [ TConstant [TInt] ]
        NMult  -> check lsym rsym [ TConstant [TInt] ]
        NDiv   -> check lsym rsym [ TConstant [TInt] ]

        NUpdate -> check lsym rsym [ TSet Nothing ]

        NConcat -> check lsym rsym [ TList y ]
  where
    check lsym rsym xs = do
        m <- mkSymbolic xs
        _ <- unify (void e) lsym m
        _ <- unify (void e) rsym m
        unify (void e) lsym rsym

lint (NSelect aset attr alternative) = do
    aset' <- unpackSymbolic =<< aset
    ks    <- evalSelector True attr
    mres  <- extract aset' ks
    case mres of
        Just v -> return v
        Nothing -> fromMaybe err alternative
          where
            err = throwError $ "could not look up attribute "
                ++ intercalate "." (map show ks)
                ++ " in " ++ show (void aset')
  where
    extract NAny (_:_) = Just <$> everyPossible
    extract (NMany [TSet Nothing]) (_:_ks) =
        error "NYI: Selection in unknown set"
    extract (NMany [TSet (Just s)]) (k:ks) = case M.lookup k s of
        Just v  -> sforce v $ unpackSymbolic >=> extract ?? ks
        Nothing -> return Nothing
    extract _ (_:_) = return Nothing
    extract v [] = Just <$> packSymbolic v

lint (NHasAttr aset attr) = aset >>= unpackSymbolic >>= \case
    NMany [TSet _] -> evalSelector True attr >>= \case
        [_] -> mkSymbolic [TConstant [TBool]]
        _ -> -- jww (2018-04-05): Need to repeat the logic above
            throwError $ "attr name argument to hasAttr"
                ++ " is not a single-part name"
    _ -> throwError "argument to hasAttr has wrong type"

lint e@(NList l) = do
    scope <- currentScopes
    y <- everyPossible
    traverse (withScopes @(SThunk m) scope) l
        >>= foldM (unify (void e)) y
        >>= (\t -> mkSymbolic [TList (svalueThunk t)])

lint (NSet binds) = do
    s <- evalBinds True False binds
    mkSymbolic [TSet (Just s)]

lint (NRecSet binds) = do
    s <- evalBinds True True binds
    mkSymbolic [TSet (Just s)]

lint (NLet binds body) = do
    s <- evalBinds True True binds
    pushScope s body

lint e@(NIf cond t f) = do
    _ <- join $ unify (void e) <$> cond <*> mkSymbolic [TConstant [TBool]]
    join $ unify (void e) <$> t <*> f

lint (NWith scope body) = do
    s <- sthunk scope
    pushWeakScope ?? body $ sforce s $ unpackSymbolic >=> \case
        NMany [TSet (Just s')] -> return s'
        NMany [TSet Nothing] -> error "with unknown set"
        _ -> throwError "scope must be a set in with statement"

lint e@(NAssert cond body) = do
    _ <- join $ unify (void e) <$> cond <*> mkSymbolic [TConstant [TBool]]
    body

lint e@(NApp fun arg) = snd <$> lintApp (void e) fun arg

lint (NAbs params body) = do
    scope <- currentScopes @_ @(SThunk m)
    mkSymbolic [TClosure scope (sthunk <$> params) (sthunk body)]

infixl 1 `lintApp`
lintApp :: forall e m. MonadLint e m
        => NExprF () -> m (Symbolic m) -> m (Symbolic m)
        -> m (HashMap Text (Symbolic m), Symbolic m)
lintApp context fun arg = fun >>= unpackSymbolic >>= \case
    NAny -> throwError "Cannot apply something not known to be a function"
    NMany xs -> do
        (args:_, ys) <- fmap unzip $ forM xs $ \case
            TClosure scope params f -> arg >>= unpackSymbolic >>= \case
                NAny -> do
                    pset <- case params of
                       Param name ->
                           M.singleton name <$> everyPossible
                       ParamSet _s _ (Just _) -> error "NYI"
                       ParamSet s _ Nothing ->
                           traverse (const everyPossible) (OM.toHashMap s)
                    pset' <- traverse (sthunk . pure) pset
                    arg'  <- sthunk $ mkSymbolic [TSet (Just pset')]
                    args  <- buildArgument params arg'
                    res   <- withScopes @(SThunk m) scope $
                        pushScope args $ sforce ?? pure =<< f
                    return (pset, res)

                NMany [TSet (Just _)] -> do
                    args <- buildArgument params =<< sthunk arg
                    res <- clearScopes @(SThunk m) $
                        pushScope args $ sforce ?? pure =<< f
                    args' <- traverse (sforce ?? pure) args
                    return (args', res)

                NMany _ -> throwError "NYI: lintApp NMany not set"
            TBuiltin _ _f -> throwError "NYI: lintApp builtin"
            TSet _m -> throwError "NYI: lintApp Set"
            _x -> throwError "Attempt to call non-function"

        y <- everyPossible
        (args,) <$> foldM (unify context) y ys
