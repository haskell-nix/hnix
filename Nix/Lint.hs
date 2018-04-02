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

module Nix.Lint (Symbolic, SThunk(..), NTypeF(..), TAtom(..),
                 lintExpr, tracingExprLint, mkSymbolic,
                 packSymbolic, unpackSymbolic, renderSymbolic,
                 sforce, sthunk, svalueThunk) where

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

newtype SThunk m = SThunk { getSThunk :: Thunk m (Symbolic m) }

sthunk :: MonadIO m => m (Symbolic m) -> m (SThunk m)
sthunk = fmap SThunk . buildThunk

sforce :: MonadIO m => SThunk m -> m (Symbolic m)
sforce = forceThunk . getSThunk

svalueThunk :: MonadIO m => Symbolic m -> m (SThunk m)
svalueThunk = fmap SThunk . valueRef

type Symbolic m = IORef (NSymbolicF (NTypeF m (SThunk m)))

everyPossible :: MonadIO m => m (Symbolic m)
everyPossible = packSymbolic NAny

mkSymbolic :: MonadIO m => [NTypeF m (SThunk m)] -> m (Symbolic m)
mkSymbolic xs = packSymbolic (NMany xs)

packSymbolic :: MonadIO m
             => NSymbolicF (NTypeF m (SThunk m)) -> m (Symbolic m)
packSymbolic = liftIO . newIORef

unpackSymbolic :: MonadIO m
               => Symbolic m -> m (NSymbolicF (NTypeF m (SThunk m)))
unpackSymbolic = liftIO . readIORef

renderSymbolic :: MonadIO m => Symbolic m -> m String
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
            x <- renderSymbolic =<< sforce r
            return $ "[" ++ x ++ "]"
        TSet Nothing    -> return "<any set>"
        TSet (Just s)   -> do
            x <- traverse (renderSymbolic <=< sforce) s
            return $ "{" ++ show x ++ "}"
        TFunction _p _f   -> return "<function>"
        TPath           -> return "path"
        TBuiltin _n _f    -> return "<builtin function>"

-- This function is order and uniqueness preserving (of types).
merge :: forall e m. MonadNixLint e m
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
        (TList l, TList r) -> do
            m <- sthunk $ join $ unify context <$> sforce l <*> sforce r
            (TList m :) <$> go xs ys
        (TSet x, TSet Nothing) -> (TSet x :) <$> go xs ys
        (TSet Nothing, TSet x) -> (TSet x :) <$> go xs ys
        (TSet (Just l), TSet (Just r)) -> do
            m <- sequenceA $ M.intersectionWith
                (\i j -> i >>= \i' -> j >>= \j' ->
                        sthunk $ join $
                            unify context <$> sforce i' <*> sforce j')
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
    (Scoped e (SThunk m) m, Framed e m, MonadFix m, MonadIO m)

-- | unify raises an error if the result is would be 'NMany []'.
unify :: MonadNixLint e m
      => NExprF () -> Symbolic m -> Symbolic m -> m (Symbolic m)
unify context x y = do
    x' <- liftIO $ readIORef x
    y' <- liftIO $ readIORef y
    case (x', y') of
        (NAny, _) -> return y
        (_, NAny) -> return x
        (NMany xs, NMany ys) -> do
            m <- merge context xs ys
            if null m
                then do
                    x' <- renderSymbolic x
                    y' <- renderSymbolic y
                    throwError $ "Cannot unify "
                        ++ show x' ++ " with " ++ show y'
                         ++ " in context: " ++ show context
                else
                    packSymbolic (NMany m)

lintExpr :: MonadNixLint e m => NExpr -> m (Symbolic m)
lintExpr = cata lint

lint :: forall e m. MonadNixLint e m
     => NExprF (m (Symbolic m)) -> m (Symbolic m)

lint (NSym var) = do
    mres <- lookupVar var
    case mres of
        Nothing -> throwError $ "Undefined variable: " ++ show var
        Just v  -> sforce v

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
    extract (NMany [TSet _s]) (Nothing:_ks) =
        error "NYI: Dynamic selection"
    extract (NMany [TSet Nothing]) (_:_ks) =
        error "NYI: Selection in unknown set"
    extract (NMany [TSet (Just s)]) (Just k:ks) = case M.lookup k s of
        Just v  -> sforce v >>= unpackSymbolic >>= flip extract ks
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
    traverse (withScopes @(SThunk m) scope) l
        >>= foldM (unify (void e)) y
        >>= svalueThunk
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

lint e@(NApp fun arg) = lintApp (void e) fun arg

lint (NAbs params body) = do
    scope <- currentScopes @_ @(SThunk m)
    mkSymbolic [TFunction (sthunk . pushScopes scope <$> params)
                          (sthunk (pushScopes scope body))]

infixl 1 `lintApp`
lintApp :: forall e m. MonadNixLint e m
        => NExprF () -> m (Symbolic m) -> m (Symbolic m) -> m (Symbolic m)
lintApp context fun arg = fun >>= unpackSymbolic >>= \case
    NAny -> throwError "Cannot apply something not known to be a function"
    NMany xs -> do
        ys <- forM xs $ \case
            TFunction params f -> do
                args <- buildArgument params =<< sthunk arg
                clearScopes @(SThunk m) (pushScope args (sforce =<< f))
            TBuiltin _ _f -> error "NYI: lintApp builtin"
            TSet _m -> error "NYI: lintApp Set"
            _x -> throwError "Attempt to call non-function"
        y <- everyPossible
        foldM (unify context) y ys

buildArgument :: forall e m. MonadNixLint e m
              => Params (m (SThunk m)) -> SThunk m
              -> m (HashMap Text (SThunk m))
buildArgument params arg = case params of
    Param name -> return $ M.singleton name arg
    ParamSet s isVariadic m -> sforce arg >>= unpackSymbolic >>= \case
        NMany [TSet Nothing] -> error "NYI"
        NMany [TSet (Just args)] -> do
            res <- loebM (alignWithKey (assemble isVariadic) args s)
            maybe (pure res) (selfInject res) m

        x -> throwError $ "Expected set in function call, received: "
                ++ show (() <$ x)
  where
    selfInject :: HashMap Text (SThunk m) -> Text
               -> m (HashMap Text (SThunk m))
    selfInject res n = do
        ref <- sthunk $ mkSymbolic [TSet (Just res)]
        return $ M.insert n ref res

    assemble :: Bool
             -> Text
             -> These (SThunk m) (Maybe (m (SThunk m)))
             -> HashMap Text (SThunk m)
             -> m (SThunk m)
    assemble isVariadic k = \case
        That Nothing  ->
            const $ throwError $ "Missing value for parameter: " ++ show k
        That (Just f) -> \args -> do
            scope <- currentScopes @_ @(SThunk m)
            sthunk $ clearScopes @(SThunk m) $
                pushScopes scope $ pushScope args $ sforce =<< f
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
attrSetAlter (Nothing:_ps) _m _val =
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
                  NMany [TSet (Just s)] -> recurse (sforce <$> s)
                  --TODO: Keep a stack of attributes we've already traversed, so
                  --that we can report that to the user
                  x -> throwError $ "attribute " ++ show p
                    ++ " is not a set; its value is " ++ show (void x)
  where
    go = return $ M.insert p val m

    recurse s = attrSetAlter ps s val >>= \m' ->
        if | M.null m' -> return m
           | otherwise -> do
             scope <- currentScopes @_ @(SThunk m)
             return $ M.insert p (embed scope m') m
      where
        embed scope m' = traverse (sthunk . withScopes scope) m'
            >>= (\t -> mkSymbolic [TSet (Just t)])

lintBinds :: forall e m. MonadNixLint e m
          => NExprF ()
          -> Bool
          -> Bool
          -> [Binding (m (Symbolic m))]
          -> m (HashMap Text (SThunk m))
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
                    Just v -> sforce v)

    buildResult :: [([Maybe Text], m (Symbolic m))]
                -> m (HashMap Text (SThunk m))
    buildResult bindings = do
        s <- foldM insert M.empty bindings
        scope <- currentScopes @_ @(SThunk m)
        if recursive
            then loebM (encapsulate scope <$> s)
            else traverse (sthunk . withScopes scope) s

    encapsulate scope f attrs =
        sthunk . withScopes scope . pushScope attrs $ f

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
