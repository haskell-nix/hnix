{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Lint (Symbolic, lintExpr, renderSymbolic) where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
-- import           Data.Align
import           Data.Align.Key
import           Data.Fix
import           Data.Functor.Compose
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.IORef
import           Data.List
import           Data.Maybe
-- import           Data.Set (Set)
-- import qualified Data.Set as Set
import           Data.Text (Text)
-- import qualified Data.Text as Text
import           Data.These
import           Nix.Atoms
import           Nix.Expr
import           Nix.Scope
import           Nix.Stack
import           Nix.StringOperations (runAntiquoted)
import           Nix.Utils
import           Text.Show.Deriving

data TAtom
  -- | An integer. The c nix implementation currently only supports
  -- integers that fit in the range of 'Int64'.
  = TInt
  -- | Booleans.
  | TBool
  -- | Null values. There's only one of this variant.
  | TNull
  -- | URIs, which are just string literals, but do not need quotes.
  | TUri
  deriving (Show, Eq, Ord)

data NTypeF r
    = TConstant [TAtom]
    | TStr
    | TList r
    | TSet (Maybe (HashMap Text r))
    | TFunction (Params r) r
    | TPath
    | TBuiltin [r] r
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

$(deriveShow1 ''NTypeF)

compareTypes :: NTypeF r -> NTypeF r -> Ordering
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
compareTypes (TFunction _ _) (TFunction _ _) = EQ
compareTypes (TFunction _ _) _               = LT
compareTypes _               (TFunction _ _) = GT
compareTypes TPath           TPath           = EQ
compareTypes TPath            _              = LT
compareTypes _               TPath           = GT
compareTypes (TBuiltin _ _)  (TBuiltin _ _)  = EQ

data NSymbolicF r
    = NAny
    | NMany [r]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

$(deriveShow1 ''NSymbolicF)

type Symbolic = Fix (Compose IORef (Compose NSymbolicF NTypeF))

everyPossible :: MonadIO m => m Symbolic
everyPossible = Fix . Compose <$> liftIO (newIORef (Compose NAny))

isEmpty :: MonadIO m => Symbolic -> m Bool
isEmpty (Fix (Compose v)) = liftIO (readIORef v) <&> \case
    Compose (NMany []) -> True
    _ -> False

mkSymbolic :: MonadIO m => [NTypeF Symbolic] -> m Symbolic
mkSymbolic xs = do
    r <- liftIO $ newIORef $ Compose (NMany xs)
    return $ Fix $ Compose r

packSymbolic :: MonadIO m => NSymbolicF (NTypeF Symbolic) -> m Symbolic
packSymbolic x = do
    r <- liftIO $ newIORef $ Compose x
    return $ Fix $ Compose r

unpackSymbolic :: MonadIO m => Symbolic -> m (NSymbolicF (NTypeF Symbolic))
unpackSymbolic (Fix (Compose x)) = getCompose <$> liftIO (readIORef x)

renderSymbolic :: MonadIO m => Symbolic -> m (Fix (Compose NSymbolicF NTypeF))
renderSymbolic =
    fmap (Fix . Compose) . traverse (traverse renderSymbolic) <=< unpackSymbolic

-- This function is order and uniqueness preserving (of types).
merge :: MonadNixLint e m
      => NExprF () -> [NTypeF Symbolic] -> [NTypeF Symbolic]
      -> m [NTypeF Symbolic]
merge context = go
  where
    go [] _ = return []
    go _ [] = return []
    go (x:xs) (y:ys) = case (x, y) of
        (TStr,  TStr)  -> (TStr :)  <$> go xs ys
        (TPath, TPath) -> (TPath :) <$> go xs ys
        (TConstant ls, TConstant rs) ->
            (TConstant (ls `intersect` rs) :) <$> go xs ys
        (TList l, TList r) -> do
            m <- unify context l r
            (TList m :) <$> go xs ys
        (TSet x, TSet Nothing) -> (TSet x :) <$> go xs ys
        (TSet Nothing, TSet x) -> (TSet x :) <$> go xs ys
        (TSet (Just l), TSet (Just r)) -> do
            m <- sequenceA $ M.intersectionWith
                (\i j -> i >>= \i' -> j >>= \j' -> unify context i' j')
                (return <$> l) (return <$> r)
            if M.null m
                then go xs ys
                else (TSet (Just m) :) <$> go xs ys
        (TFunction (Param pl) fl, TFunction (Param pr) fr)
            | pl /= pr -> go xs ys
            | otherwise -> do
                  g <- unify context fl fr
                  (TFunction (Param pl) g :) <$> go xs ys
        (TFunction (ParamSet (FixedParamSet pl) nl) fl,
         TFunction (ParamSet (FixedParamSet pr) nr) fr)
            | nl /= nr -> go xs ys
            | otherwise -> mergeFunctions pl nl fl pr fr xs ys
        (TFunction (ParamSet (VariadicParamSet pl) nl) fl,
         TFunction (ParamSet (VariadicParamSet pr) nr) fr)
            | nl /= nr -> go xs ys
            | otherwise -> mergeFunctions pl nl fl pr fr xs ys
        (TBuiltin pl fl, TBuiltin pr fr)
            | length pl /= length pr -> go xs ys
            | otherwise -> do
                  m <- zipWithM (unify context) pl pr
                  g <- unify context fl fr
                  (TBuiltin m g :) <$> go xs ys
        _ | compareTypes x y == LT -> go xs (y:ys)
          | compareTypes x y == GT -> go (x:xs) ys
          | otherwise              -> error "impossible"

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
                (TFunction (ParamSet (FixedParamSet m') nl) g :)
                    <$> go xs ys

type MonadNixLint e m =
    (Scoped e Symbolic m, Framed e m, MonadFix m, MonadIO m)

-- | unify raises an error if the result is would be 'NMany []'.
unify :: MonadNixLint e m => NExprF () -> Symbolic -> Symbolic -> m Symbolic
unify context l@(Fix (Compose x)) r@(Fix (Compose y)) = do
    Compose x' <- liftIO $ readIORef x
    Compose y' <- liftIO $ readIORef y
    case (x', y') of
        (NAny, _) -> return r
        (_, NAny) -> return l
        (NMany xs, NMany ys) -> do
            m <- merge context xs ys
            if null m
                then do
                    l' <- renderSymbolic l
                    r' <- renderSymbolic r
                    throwError $ "Cannot unify "
                        ++ show l' ++ " with " ++ show r'
                         ++ " in context: " ++ show context
                else do
                    r <- liftIO $ newIORef $ Compose (NMany m)
                    return $ Fix $ Compose r

lintExpr :: MonadNixLint e m => NExpr -> m Symbolic
lintExpr = cata lint

lint :: MonadNixLint e m => NExprF (m Symbolic) -> m Symbolic

lint (NSym var) = do
    mres <- lookupVar var
    case mres of
        Nothing -> throwError $ "Undefined variable: " ++ show var
        Just v  -> return v

lint (NConstant c)    = mkSymbolic [TConstant [t]]
  where
      t = case c of
          NInt _  -> TInt
          NBool _ -> TBool
          NNull   -> TNull
          NUri _  -> TUri

lint (NStr _)         = mkSymbolic [TStr]
lint (NLiteralPath _) = mkSymbolic [TPath]
lint (NEnvPath _)     = mkSymbolic [TPath]

lint e@(NUnary _op arg) =
    join $ unify (void e) <$> arg <*> mkSymbolic [TConstant [TInt, TBool]]

lint e@(NBinary op larg rarg) = do
    lsym <- larg
    rsym <- rarg
    y <- everyPossible
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
        m <- unify (void e) lsym rsym
        unify (void e) m =<< mkSymbolic xs

lint e@(NSelect aset attr alternative) = do
    aset' <- unpackSymbolic =<< aset
    ks    <- lintSelector (void e) True attr
    mres  <- extract aset' ks
    case mres of
        Just v -> return v
        Nothing -> fromMaybe err alternative
          where
            err = throwError $ "could not look up attribute "
                ++ intercalate "." (map show ks)
                ++ " in " ++ show (void aset')
  where
    extract (NMany [TSet s]) (Nothing:ks) = error "NYI: Dynamic selection"
    extract (NMany [TSet Nothing]) (_:ks) = error "NYI: Selection in unknown set"
    extract (NMany [TSet (Just s)]) (Just k:ks) = case M.lookup k s of
        Just v  -> unpackSymbolic v >>= flip extract ks
        Nothing -> return Nothing
    extract _ (_:_) = return Nothing
    extract v [] = Just <$> packSymbolic v

lint e@(NHasAttr aset attr) = aset >>= unpackSymbolic >>= \case
    NMany [TSet _] -> lintSelector (void e) True attr >>= \case
        [_] -> mkSymbolic [TConstant [TBool]]
        _ -> throwError $ "attr name argument to hasAttr"
                ++ " is not a single-part name"
    _ -> throwError "argument to hasAttr has wrong type"

lint e@(NList l) = do
    scope <- currentScopes
    y <- everyPossible
    traverse (withScopes @Symbolic scope) l
        >>= foldM (unify (void e)) y
        >>= (\t -> mkSymbolic [TList t])

lint e@(NSet binds) = do
    s <- lintBinds (void e) True False binds
    mkSymbolic [TSet (Just s)]

lint e@(NRecSet binds) = do
    s <- lintBinds (void e) True True binds
    mkSymbolic [TSet (Just s)]

lint e@(NLet binds body) = do
    s <- lintBinds (void e) True True binds
    pushScope s body

lint e@(NIf cond t f) = do
    _ <- join $ unify (void e) <$> cond <*> mkSymbolic [TConstant [TBool]]
    join $ unify (void e) <$> t <*> f

lint (NWith scope body) = scope >>= unpackSymbolic >>= \case
    NMany [TSet (Just s')] -> pushWeakScope s' body
    NMany [TSet Nothing] -> error "with unknown set"
    _ -> throwError "scope must be a set in with statement"

lint e@(NAssert cond body) = do
    _ <- join $ unify (void e) <$> cond <*> mkSymbolic [TConstant [TBool]]
    body

lint (NApp fun arg) = fun >>= unpackSymbolic >>= \case
    NMany xs -> (mkSymbolic =<<) $ forM xs $ \case
        TFunction params f -> do
            error "NYI: NApp TFunction"
            -- args <- buildArgument params =<< arg
            -- return f
        TBuiltin name f ->
            -- jww (2018-04-01): Lookup the builtin's type by name
            error "NYI: NApp TBuiltin"
        TSet m -> error "NYI: NApp TSet"
    x -> throwError $ "Attempt to call non-function: " ++ show (() <$ x)

lint (NAbs params body) = do
    -- It is the environment at the definition site, not the call site, that
    -- needs to be used when lintuating the body and default arguments. This
    -- is reflected here by demanding the monadic action immediately; whereas
    -- in the lazy evaluator (Eval.hs), we must capture the current scope in
    -- order to restore it when the function is later applied.
    p <- sequence params
    -- jww (2018-03-31): Need to establish function parameters here, so that
    -- when we symbolically evaluate body it will see them.
    -- jww (2018-04-01): -- This needs to be deferred, just as in Eval.hs,
    -- since the parameters may -- take on various types at different call
    -- sites.
    b <- body
    mkSymbolic [TFunction p b]

buildArgument :: forall e m. MonadNixLint e m
              => Params (m Symbolic) -> Symbolic -> m (HashMap Text Symbolic)
buildArgument params arg = case params of
    Param name -> return $ M.singleton name arg
    ParamSet ps m -> go ps m
  where
    go ps m = unpackSymbolic arg >>= \case
        NMany [TSet Nothing] -> error "NYI"
        NMany [TSet (Just args)] -> do
            let (s, isVariadic) = case ps of
                  FixedParamSet    s' -> (s', False)
                  VariadicParamSet s' -> (s', True)
            res <- loebM (alignWithKey (assemble isVariadic) args s)
            maybe (pure res) (selfInject res) m

        x -> throwError $ "Expected set in function call, received: "
                ++ show (() <$ x)

    selfInject :: HashMap Text Symbolic -> Text -> m (HashMap Text Symbolic)
    selfInject res n = do
        ref <- mkSymbolic [TSet (Just res)]
        return $ M.insert n ref res

    assemble :: Bool
             -> Text
             -> These Symbolic (Maybe (m Symbolic))
             -> HashMap Text Symbolic
             -> m Symbolic
    assemble isVariadic k = \case
        That Nothing  ->
            const $ throwError $ "Missing value for parameter: " ++ show k
        That (Just f) -> \args -> do
            scope <- currentScopes @_ @Symbolic
            clearScopes @Symbolic $
                pushScopes scope $ pushScope args f
        This x | isVariadic -> const (pure x)
               | otherwise  ->
                 const $ throwError $ "Unexpected parameter: " ++ show k
        These x _ -> const (pure x)

attrSetAlter :: forall e m. MonadNixLint e m
             => [Maybe Text]
             -> HashMap Text (m Symbolic)
             -> m Symbolic
             -> m (HashMap Text (m Symbolic))
attrSetAlter [] _ _ = throwError "invalid selector with no components"
attrSetAlter (Nothing:ps) m val =
    -- In the case where the only thing we know about a dynamic key is that it
    -- must unify with a string, we have to consider the possibility that it
    -- might select any one of the members of 'm'.
    error "Not yet implemented: dynamic keys"
attrSetAlter (Just p:ps) m val = case M.lookup p m of
    Nothing | null ps   -> go
            | otherwise -> recurse M.empty
    Just v  | null ps   -> go
            | otherwise -> v >>= unpackSymbolic >>= \case
                  NMany [TSet Nothing] -> error "NYI"
                  NMany [TSet (Just s)] -> recurse (pure <$> s)
                  --TODO: Keep a stack of attributes we've already traversed, so
                  --that we can report that to the user
                  x -> throwError $ "attribute " ++ show p
                    ++ " is not a set; its value is " ++ show (void x)
  where
    go = return $ M.insert p val m

    recurse s = attrSetAlter ps s val >>= \m' ->
        if | M.null m' -> return m
           | otherwise -> do
             scope <- currentScopes @_ @Symbolic
             return $ M.insert p (embed scope m') m
      where
        embed scope m' = traverse (withScopes scope) m'
            >>= (\t -> mkSymbolic [TSet (Just t)])

lintBinds :: forall e m. MonadNixLint e m
          => NExprF ()
          -> Bool
          -> Bool
          -> [Binding (m Symbolic)]
          -> m (HashMap Text Symbolic)
lintBinds context allowDynamic recursive = buildResult . concat <=< mapM go
  where
    go :: Binding (m Symbolic) -> m [([Maybe Text], m Symbolic)]
    go (NamedVar x y) =
        sequence [liftM2 (,) (lintSelector context allowDynamic x) (pure y)]
    go (Inherit ms names) = forM names $ \name -> do
        mkey <- lintKeyName context allowDynamic name
        return ([mkey], case mkey of
            Nothing -> error "NYI"
            Just key -> do
                mv <- case ms of
                    Nothing -> lookupVar key
                    Just s -> s >>= unpackSymbolic >>= \case
                        NMany [TSet Nothing] -> error "NYI"
                        NMany [TSet (Just s)] -> pushScope s (lookupVar key)
                        x -> throwError
                            $ "First argument to inherit should be a set, saw: "
                            ++ show (() <$ x)
                case mv of
                    Nothing -> throwError $ "Inheriting unknown attribute: "
                        ++ show (() <$ name)
                    Just v -> return v)

    buildResult :: [([Maybe Text], m Symbolic)]
                -> m (HashMap Text Symbolic)
    buildResult bindings = do
        s <- foldM insert M.empty bindings
        scope <- currentScopes @_ @Symbolic
        if recursive
            then loebM (encapsulate scope <$> s)
            else traverse (withScopes scope) s

    encapsulate scope f attrs =
        withScopes scope . pushScope attrs $ f

    insert m (path, value) = attrSetAlter path m value

lintString :: MonadNixLint e m => NString (m Symbolic) -> m Symbolic
lintString _nstr = mkSymbolic [TStr]

lintSelector :: MonadNixLint e m
             => NExprF () -> Bool -> NAttrPath (m Symbolic)
             -> m [Maybe Text]
lintSelector context = mapM . lintKeyName context

-- We know keys must be result in strings, but we can't know which member in
-- the set they'll reference. Since sets have heterogenous membership, when we
-- see a reference such as 'a.b.c', we can only know there's an error if we've
-- inferred that no member of 'a' is a set.
lintKeyName :: MonadNixLint e m
            => NExprF () -> Bool -> NKeyName (m Symbolic)
            -> m (Maybe Text)
lintKeyName _ _ (StaticKey k) = return $ Just k
lintKeyName context dyn (DynamicKey k)
    | dyn = do
          -- This is done only as a check.
          _ <- runAntiquoted lintString
              (\x -> join $ unify context <$> x <*> mkSymbolic [TStr]) k
          return Nothing
    | otherwise =
      throwError "dynamic attribute not allowed in this context"
