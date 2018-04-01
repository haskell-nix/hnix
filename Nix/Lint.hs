{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Lint where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Align
import           Data.Align.Key
import           Data.Fix
import           Data.Functor.Compose
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.These
import           Nix.Atoms
import           Nix.Expr
import           Nix.Monad
import           Nix.Scope
import           Nix.StringOperations (runAntiquoted)
import           Nix.Utils

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
    | TSet (HashMap Text r)
    | TFunction (Params r) r
    | TPath
    | TBuiltin [r] r
    deriving (Show, Eq, Ord, Functor)

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
    deriving (Show, Eq, Ord, Functor)

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

unpackSymbolic :: MonadIO m => Symbolic -> m (NSymbolicF (NTypeF Symbolic))
unpackSymbolic (Fix (Compose x)) = getCompose <$> liftIO (readIORef x)

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
        (TSet l, TSet r) -> do
            m <- sequenceA $ M.intersectionWith
                (\i j -> i >>= \i' -> j >>= \j' -> unify context i' j')
                (return <$> l) (return <$> r)
            if M.null m
                then go xs ys
                else (TSet m :) <$> go xs ys
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
    (Scoped e Symbolic m, Framed e m, MonadNix m, MonadIO m)

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
                then
                    throwError $ "Cannot unify "
                         ++ show (void xs) ++ " with " ++ show (void ys)
                         ++ " in context: " ++ show context
                else do
                    r <- liftIO $ newIORef $ Compose (NMany m)
                    return $ Fix $ Compose r

symbolicLintExpr :: MonadNixLint e m => NExpr -> m Symbolic
symbolicLintExpr = cata lint

lint :: MonadNixLint e m => NExprF (m Symbolic) -> m Symbolic

lint (NSym var) = do
    mres <- lookupVar var
    case mres of
        Nothing -> throwError $ "Undefined variable: " ++ show var
        Just v  -> v

lint v@(NConstant c)    = mkSymbolic [TConstant [t]]
  where
      t = case c of
          NInt _  -> TInt
          NBool _ -> TBool
          NNull   -> TNull
          NUri _  -> TUri

lint v@(NStr _)         = mkSymbolic [TStr]
lint v@(NLiteralPath _) = mkSymbolic [TPath]
lint v@(NEnvPath _)     = mkSymbolic [TPath]

lint e@(NUnary _op arg) =
    join $ unify (void e) <$> arg <*> mkSymbolic [TConstant [TInt, TBool]]

{-
lint (NBinary op larg rarg) = do
    lval <- larg
    rval <- rarg
    let unsupportedTypes =
            "unsupported argument types for binary operator "
                ++ show (() <$ lval, op, () <$ rval)
    case (lval, rval) of
        (NVConstant lc, NVConstant rc) -> case (op, lc, rc) of
            (NEq,  _, _) ->
                -- TODO: Refactor so that lint (NBinary ..) dispatches based
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
            NUpdate -> return $ NVSet $ rs `M.union` ls
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

lint (NSelect aset attr alternative) = do
    aset' <- aset
    ks    <- lintSelector True attr
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
    extract (NVSet s) (k:ks) = case M.lookup k s of
        Just v  -> do
            s' <- forceThunk v
            extract s' ks
        Nothing -> return Nothing
    extract _ (_:_) = return Nothing
    extract v [] = return $ Just v
-}

lint (NHasAttr aset attr) = aset >>= unpackSymbolic >>= \case
    NMany [TSet s] -> lintSelector True attr >>= \case
        [keyName] -> mkSymbolic [TConstant [TBool]]
        _ -> throwError $ "attr name argument to hasAttr"
                ++ " is not a single-part name"
    _ -> throwError "argument to hasAttr has wrong type"

lint e@(NList l) = do
    scope <- currentScopes
    l' <- traverse (withScopes @Symbolic scope) l
    y <- everyPossible
    (\t -> mkSymbolic [TList t]) =<< foldM (unify (void e)) y l'

lint (NSet binds) = do
    s <- lintBinds True False binds
    mkSymbolic [TSet s]

lint (NRecSet binds) = do
    s <- lintBinds True True binds
    mkSymbolic [TSet s]

lint (NLet binds e) = do
    s <- lintBinds True True binds
    pushScope s e

lint e@(NIf cond t f) = do
    c <- cond
    unify (void e) c =<< mkSymbolic [TConstant [TBool]]
    join $ unify (void e) <$> t <*> f

lint (NWith scope e) = scope >>= unpackSymbolic >>= \case
    NMany [TSet s'] -> pushWeakScope s' e
    _ -> throwError "scope must be a set in with statement"

lint (NAssert cond e) = do
    c <- cond
    unify (void e) c =<< mkSymbolic [TConstant [TBool]]
    e

{-
lint (NApp fun arg) = fun >>= \case
    TFunction params f -> do
        args <- buildArgument params =<< buildThunk arg
        clearScopes @Symbolic (pushScope args (forceThunk =<< f))
    TBuiltin _ f -> f =<< buildThunk arg
    TSet m
        | Just f <- M.lookup "__functor" m
            -> forceThunk f `lintApp` fun `lintApp` arg
    x -> throwError $ "Attempt to call non-function: " ++ show (() <$ x)
-}

lint (NAbs params body) = do
    -- It is the environment at the definition site, not the call site, that
    -- needs to be used when lintuating the body and default arguments. This
    -- is reflected here by demanding the monadic action immediately; whereas
    -- in the lazy evaluator (Eval.hs), we must capture the current scope in
    -- order to restore it when the function is later applied.
    p <- sequence params
    -- jww (2018-03-31): Need to establish function parameters here, so that
    -- when we symbolically evaluate body it will see them.
    b <- body
    mkSymbolic [TFunction p b]

buildArgument :: forall e m. MonadNixLint e m
              => Params (m Symbolic) -> Symbolic -> m (HashMap Text Symbolic)
buildArgument params arg = case params of
    Param name -> return $ M.singleton name arg
    ParamSet ps m -> go ps m
  where
    go ps m = unpackSymbolic arg >>= \case
        NMany [TSet args] -> do
            let (s, isVariadic) = case ps of
                  FixedParamSet    s' -> (s', False)
                  VariadicParamSet s' -> (s', True)
            res <- loebM (alignWithKey (assemble isVariadic) args s)
            maybe (pure res) (selfInject res) m

        x -> throwError $ "Expected set in function call, received: "
                ++ show (() <$ x)

    selfInject :: HashMap Text Symbolic -> Text -> m (HashMap Text Symbolic)
    selfInject res n = do
        ref <- mkSymbolic [TSet res]
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
             => [Text]
             -> HashMap Text (m Symbolic)
             -> m Symbolic
             -> m (HashMap Text (m Symbolic))
attrSetAlter [] _ _ = throwError "invalid selector with no components"
attrSetAlter (p:ps) m val = case M.lookup p m of
    Nothing | null ps   -> go
            | otherwise -> recurse M.empty
    Just v  | null ps   -> go
            | otherwise -> v >>= unpackSymbolic >>= \case
                  NMany [TSet s] -> recurse s
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
        embed scope m' =
            (\t -> mkSymbolic [TSet t]) =<< traverse (withScopes scope) m'

lintBinds :: forall e m. MonadNixLint e m
          => Bool
          -> Bool
          -> [Binding (m Symbolic)]
          -> m (HashMap Text Symbolic)
lintBinds allowDynamic recursive = buildResult . concat <=< mapM go
  where
    go :: Binding (m Symbolic) -> m [([Text], m Symbolic)]
    go (NamedVar x y) =
        sequence [liftM2 (,) (lintSelector allowDynamic x) (pure y)]
    go (Inherit ms names) = forM names $ \name -> do
        key <- lintKeyName allowDynamic name
        return ([key], do
            mv <- case ms of
                Nothing -> lookupVar key
                Just s -> s >>= unpackSymbolic >>= \case
                    NMany [TSet s] -> pushScope s (lookupVar key)
                    x -> throwError
                        $ "First argument to inherit should be a set, saw: "
                        ++ show (() <$ x)
            case mv of
                Nothing -> throwError $ "Inheriting unknown attribute: "
                    ++ show (() <$ name)
                Just v -> v)

    buildResult :: [([Text], m Symbolic)] -> m (HashMap Text Symbolic)
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

lintSelector :: (Framed e m, MonadNix m)
             => Bool -> NAttrPath (m Symbolic) -> m [Text]
lintSelector = mapM . lintKeyName

lintKeyName :: (Framed e m, MonadNix m)
            => Bool -> NKeyName (m Symbolic) -> m Text
lintKeyName _ (StaticKey k) = return k
lintKeyName dyn (DynamicKey k)
    | dyn = do
          v <- runAntiquoted lintString id k
          valueTextNoContext =<< normalForm v
    | otherwise =
      throwError "dynamic attribute not allowed in this context"
