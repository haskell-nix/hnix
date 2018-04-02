{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Lint (Symbolic, NTypeF(..), TAtom(..),
                 lintExpr, tracingExprLint, mkSymbolic,
                 packSymbolic, unpackSymbolic, renderSymbolic) where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Align.Key
import           Data.Fix
import           Data.Functor.Compose
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import           Data.These
import           Nix.Atoms
import           Nix.Expr
import           Nix.Scope
import           Nix.Stack
import           Nix.StringOperations (runAntiquoted)
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
    | TFunction (Params (m r)) (m r)
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

type Symbolic m = Fix (Compose IORef (Compose NSymbolicF (NTypeF m)))

everyPossible :: MonadIO m => m (Symbolic m)
everyPossible = Fix . Compose <$> liftIO (newIORef (Compose NAny))

mkSymbolic :: MonadIO m => [NTypeF m (Symbolic m)] -> m (Symbolic m)
mkSymbolic xs = do
    r <- liftIO $ newIORef $ Compose (NMany xs)
    return $ Fix $ Compose r

packSymbolic :: MonadIO m
             => NSymbolicF (NTypeF m (Symbolic m)) -> m (Symbolic m)
packSymbolic = fmap (Fix . Compose) . liftIO . newIORef . Compose

unpackSymbolic :: MonadIO m
               => Symbolic m -> m (NSymbolicF (NTypeF m (Symbolic m)))
unpackSymbolic (Fix (Compose x)) = getCompose <$> liftIO (readIORef x)

renderSymbolic :: MonadIO m => Symbolic m -> m String
renderSymbolic = unpackSymbolic >=> \case
    NAny -> return "<any>"
    NMany xs -> fmap (intercalate ", ") $ forM xs $ \case
        TConstant ys  -> fmap (intercalate ", ") $ forM ys $ \case
            TInt  -> return "int"
            TBool -> return "bool"
            TNull -> return "null"
            TUri  -> return "uri"
        TStr            -> return "string"
        TList r         -> (\x -> "[" ++ x ++ "]") <$> renderSymbolic r
        TSet Nothing    -> return "<any set>"
        TSet (Just s)   -> (\x -> "{" ++ show x ++ "}")
            <$> traverse renderSymbolic s
        TFunction p f   -> return "<function>"
        TPath           -> return "path"
        TBuiltin n f    -> return "<builtin function>"

-- This function is order and uniqueness preserving (of types).
merge :: MonadNixLint e m
      => NExprF () -> [NTypeF m (Symbolic m)] -> [NTypeF m (Symbolic m)]
      -> m [NTypeF m (Symbolic m)]
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
        (TFunction _ _, TFunction _ _) ->
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
                (TFunction (ParamSet m' False nl) g :)
                    <$> go xs ys
-}

type MonadNixLint e m =
    (Scoped e (Symbolic m) m, Framed e m, MonadFix m, MonadIO m)

-- | unify raises an error if the result is would be 'NMany []'.
unify :: MonadNixLint e m
      => NExprF () -> Symbolic m -> Symbolic m -> m (Symbolic m)
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

lintExpr :: MonadNixLint e m => NExpr -> m (Symbolic m)
lintExpr = cata lint

lint :: forall e m. MonadNixLint e m
     => NExprF (m (Symbolic m)) -> m (Symbolic m)

lint (NSym var) = do
    traceM $ "NSym: " ++ show var
    mres <- lookupVar var
    traceM $ "NSym: " ++ show var ++ "...done"
    case mres of
        Nothing -> throwError $ "Undefined variable: " ++ show var
        Just v  -> return v

lint (NConstant c) = mkSymbolic [TConstant [t]]
  where
      t = case c of
          NInt _  -> TInt
          NBool _ -> TBool
          NNull   -> TNull
          NUri _  -> TUri

lint (NStr _)         = mkSymbolic [TStr]
lint (NLiteralPath _) = mkSymbolic [TPath]
lint (NEnvPath _)     = mkSymbolic [TPath]

lint e@(NUnary _op arg) = do
    traceM "NUnary"
    join $ unify (void e) <$> arg <*> mkSymbolic [TConstant [TInt, TBool]]

lint e@(NBinary op larg rarg) = do
    traceM "NBinary"
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
    traceM "NSelect"
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
    extract (NMany [TSet s]) (Nothing:ks) =
        error "NYI: Dynamic selection"
    extract (NMany [TSet Nothing]) (_:ks) =
        error "NYI: Selection in unknown set"
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
    traceM "NList"
    scope <- currentScopes
    y <- everyPossible
    traverse (withScopes @(Symbolic m) scope) l
        >>= foldM (unify (void e)) y
        >>= (\t -> mkSymbolic [TList t])

lint e@(NSet binds) = do
    traceM "NSet"
    s <- lintBinds (void e) True False binds
    mkSymbolic [TSet (Just s)]

lint e@(NRecSet binds) = do
    traceM "NRecSet"
    s <- lintBinds (void e) True True binds
    mkSymbolic [TSet (Just s)]

lint e@(NLet binds body) = do
    traceM "NLet"
    s <- lintBinds (void e) True True binds
    pushScope s body

lint e@(NIf cond t f) = do
    traceM "NIf"
    _ <- join $ unify (void e) <$> cond <*> mkSymbolic [TConstant [TBool]]
    join $ unify (void e) <$> t <*> f

lint (NWith scope body) = scope >>= unpackSymbolic >>= \case
    NMany [TSet (Just s')] -> pushWeakScope s' body
    NMany [TSet Nothing] -> error "with unknown set"
    _ -> throwError "scope must be a set in with statement"

lint e@(NAssert cond body) = do
    traceM "NAssert"
    _ <- join $ unify (void e) <$> cond <*> mkSymbolic [TConstant [TBool]]
    body

lint e@(NApp fun arg) = traceM "NApp" >> lintApp (void e) fun arg

lint (NAbs params body) = do
    traceM "NAbs"
    scope <- currentScopes @_ @(Symbolic m)
    mkSymbolic [TFunction (pushScopes scope <$> params)
                          (pushScopes scope body)]

infixl 1 `lintApp`
lintApp :: forall e m. MonadNixLint e m
        => NExprF () -> m (Symbolic m) -> m (Symbolic m) -> m (Symbolic m)
lintApp context fun arg = fun >>= unpackSymbolic >>= \case
    NAny -> throwError "Cannot apply something not known to be a function"
    NMany xs -> do
        ys <- forM xs $ \case
            TFunction params f -> do
                traceM "Building arguments..."
                args <- buildArgument params =<< arg
                traceM "Building arguments...done"
                traceM $ "Linting function application with args: "
                    ++ show (newScope args)
                clearScopes @(Symbolic m) (pushScope args f)
            TBuiltin _ f -> error "NYI: lintApp builtin"
            TSet m -> error "NYI: lintApp Set"
            x -> throwError "Attempt to call non-function"
        y <- everyPossible
        foldM (unify context) y ys

buildArgument :: forall e m. MonadNixLint e m
              => Params (m (Symbolic m)) -> Symbolic m
              -> m (HashMap Text (Symbolic m))
buildArgument params arg = case params of
    Param name -> return $ M.singleton name arg
    ParamSet s isVariadic m -> unpackSymbolic arg >>= \case
        NMany [TSet Nothing] -> error "NYI"
        NMany [TSet (Just args)] -> do
            traceM "buildArgument..1"
            res <- loebM (alignWithKey (assemble isVariadic) args s)
            traceM "buildArgument..2"
            maybe (pure res) (selfInject res) m

        x -> throwError $ "Expected set in function call, received: "
                ++ show (() <$ x)
  where
    selfInject :: HashMap Text (Symbolic m) -> Text
               -> m (HashMap Text (Symbolic m))
    selfInject res n = do
        traceM "buildArgument..3"
        ref <- mkSymbolic [TSet (Just res)]
        return $ M.insert n ref res

    assemble :: Bool
             -> Text
             -> These (Symbolic m) (Maybe (m (Symbolic m)))
             -> HashMap Text (Symbolic m)
             -> m (Symbolic m)
    assemble isVariadic k = \case
        That Nothing  ->
            const $ throwError $ "Missing value for parameter: " ++ show k
        That (Just f) -> \args -> do
            traceM "buildArgument..4"
            res <- pushScope args f
            traceM "buildArgument..5"
            return res
        This x | isVariadic -> const (pure x)
               | otherwise  ->
                 const $ throwError $ "Unexpected parameter: " ++ show k
        These x _ -> const (pure x)

attrSetAlter :: forall e m. MonadNixLint e m
             => [Maybe Text]
             -> HashMap Text (m (Symbolic m))
             -> m (Symbolic m)
             -> m (HashMap Text (m (Symbolic m)))
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
             scope <- currentScopes @_ @(Symbolic m)
             return $ M.insert p (embed scope m') m
      where
        embed scope m' = traverse (withScopes scope) m'
            >>= (\t -> mkSymbolic [TSet (Just t)])

lintBinds :: forall e m. MonadNixLint e m
          => NExprF ()
          -> Bool
          -> Bool
          -> [Binding (m (Symbolic m))]
          -> m (HashMap Text (Symbolic m))
lintBinds context allowDynamic recursive = buildResult . concat <=< mapM go
  where
    go :: Binding (m (Symbolic m)) -> m [([Maybe Text], m (Symbolic m))]
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

    buildResult :: [([Maybe Text], m (Symbolic m))]
                -> m (HashMap Text (Symbolic m))
    buildResult bindings = do
        s <- foldM insert M.empty bindings
        scope <- currentScopes @_ @(Symbolic m)
        if recursive
            then loebM (encapsulate scope <$> s)
            else traverse (withScopes scope) s

    encapsulate scope f attrs =
        withScopes scope . pushScope attrs $ f

    insert m (path, value) = attrSetAlter path m value

lintString :: MonadNixLint e m => NString (m (Symbolic m)) -> m (Symbolic m)
lintString _nstr = mkSymbolic [TStr]

lintSelector :: MonadNixLint e m
             => NExprF () -> Bool -> NAttrPath (m (Symbolic m))
             -> m [Maybe Text]
lintSelector context = mapM . lintKeyName context

-- We know keys must be result in strings, but we can't know which member in
-- the set they'll reference. Since sets have heterogenous membership, when we
-- see a reference such as 'a.b.c', we can only know there's an error if we've
-- inferred that no member of 'a' is a set.
lintKeyName :: MonadNixLint e m
            => NExprF () -> Bool -> NKeyName (m (Symbolic m))
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

tracingExprLint :: MonadNixLint e m => NExprLoc -> IO (m (Symbolic m))
tracingExprLint =
    flip runReaderT (0 :: Int)
        . adiM (pure <$> lint . annotated . getCompose) psi
  where
    psi k v@(Fix x) = do
        depth <- ask
        liftIO $ putStrLn $ "lint: " ++ replicate (depth * 2) ' '
            ++ show (stripAnnotation v)
        res <- local succ $
            fmap (withExprContext (() <$ x)) (k v)
        liftIO $ putStrLn $ "lint: " ++ replicate (depth * 2) ' ' ++ "."
        return res
