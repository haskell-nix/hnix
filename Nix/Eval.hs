{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Eval where

import           Control.Applicative
import           Control.Arrow (first)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Align
import           Data.Align.Key
import           Data.Coerce
import           Data.Fix
import           Data.Functor.Compose
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashMap.Strict.InsOrd (toHashMap)
import           Data.List (intercalate)
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.These
import           Data.Traversable (for)
import           Nix.Atoms
import           Nix.Expr
import           Nix.Monad
import           Nix.Pretty
import           Nix.Scope
import           Nix.Stack
import           Nix.StringOperations (runAntiquoted)
import           Nix.Thunk
import           Nix.Utils

type MonadEval e m =
    ( Scoped e (NThunk m) m
    , Framed e m
    , MonadExpr (NThunk m) (NValue m) m
    , MonadVar m
    , MonadFile m
    )

-- | Evaluate an nix expression, with a given ValueSet as environment
evalExpr :: (MonadEval e m, MonadFix m, MonadNix m) => NExpr -> m (NValue m)
evalExpr = cata eval

eval :: forall e m. (MonadEval e m, MonadFix m, MonadNix m)
     => NExprF (m (NValue m)) -> m (NValue m)

eval (NSym var) = do
    traceM $ "NSym: var = " ++ show var
    mres <- lookupVar var
    case mres of
        Nothing ->
            throwError $ "Undefined variable '" ++ Text.unpack var ++ "'"
        Just v -> force v pure

eval (NConstant x)    = return $ NVConstant x
eval (NStr str)       = traceM "NStr" >> evalString str
eval (NLiteralPath p) =
    traceM "NLiteralPath" >> NVLiteralPath <$> makeAbsolutePath p
eval (NEnvPath p)     = return $ NVEnvPath p

eval (NUnary op arg) = do
    traceM "NUnary"
    arg >>= \case
        NVConstant c -> case (op, c) of
            (NNeg, NInt   i) -> return $ NVConstant $ NInt   (-i)
            (NNeg, NFloat f) -> return $ NVConstant $ NFloat (-f)
            (NNot, NBool  b) -> return $ NVConstant $ NBool  (not b)
            _ -> throwError $ "unsupported argument type for unary operator "
                     ++ show op
        x -> throwError $ "argument to unary operator"
                ++ " must evaluate to an atomic type: " ++ showValue x

eval (NBinary op larg rarg) = case op of
    NOr -> larg >>= \case
        NVConstant (NBool l) -> if l
            then valueRefBool True
            else rarg >>= \case
                NVConstant (NBool r) -> valueRefBool r
                v -> throwError $ "operator `||`: left argument: boolean expected, got " ++ show (void v)
        v -> throwError $ "operator `||`: right argument: boolean expected, got " ++ show (void v)
    NAnd -> larg >>= \case
        NVConstant (NBool l) -> if l
            then rarg >>= \case
                NVConstant (NBool r) -> valueRefBool r
                v -> throwError $ "operator `&&`: left argument: boolean expected, got " ++ show (void v)
            else valueRefBool False
        v -> throwError $ "operator `&&`: right argument: boolean expected, got " ++ show (void v)
    -- TODO: Refactor so that eval (NBinary ..) *always* dispatches based on
    -- operator first
    _ -> do
        lval <- traceM "NBinary:left" >> larg
        rval <- traceM "NBinary:right" >> rarg
        let unsupportedTypes =
                "unsupported argument types for binary operator "
                    ++ showValue lval ++ " " ++ show op ++ " " ++ showValue rval
            numBinOp :: (forall a. Num a => a -> a -> a) -> NAtom -> NAtom
                     -> m (NValue m)
            numBinOp f = numBinOp' f f
            numBinOp'
                :: (Integer -> Integer -> Integer)
                -> (Float -> Float -> Float)
                -> NAtom -> NAtom -> m (NValue m)
            numBinOp' intF floatF l r = case (l, r) of
                (NInt   li, NInt   ri) ->
                    valueRefInt   $             li `intF`               ri
                (NInt   li, NFloat rf) ->
                    valueRefFloat $ fromInteger li `floatF`             rf
                (NFloat lf, NInt   ri) ->
                    valueRefFloat $             lf `floatF` fromInteger ri
                (NFloat lf, NFloat rf) ->
                    valueRefFloat $             lf `floatF`             rf
                _ -> throwError unsupportedTypes
        case (lval, rval) of
            (NVConstant lc, NVConstant rc) -> case (op, lc, rc) of
                (NEq,  _, _) -> valueRefBool =<< valueEq lval rval
                (NNEq, _, _) -> valueRefBool . not =<< valueEq lval rval
                (NLt,  l, r) -> valueRefBool $ l <  r
                (NLte, l, r) -> valueRefBool $ l <= r
                (NGt,  l, r) -> valueRefBool $ l >  r
                (NGte, l, r) -> valueRefBool $ l >= r
                (NAnd,  _, _) -> error "should be impossible: && is handled above"
                (NOr,   _, _) -> error "should be impossible: || is handled above"
                (NImpl, NBool l, NBool r) -> valueRefBool $ not l || r
                (NPlus,  l, r) -> numBinOp (+) l r
                (NMinus, l, r) -> numBinOp (-) l r
                (NMult,  l, r) -> numBinOp (*) l r
                (NDiv,   l, r) -> numBinOp' div (/) l r
                _ -> throwError unsupportedTypes

            (NVStr ls lc, NVStr rs rc) -> case op of
                NPlus -> return $ NVStr (ls `mappend` rs) (lc `mappend` rc)
                NEq   -> valueRefBool =<< valueEq lval rval
                NNEq  -> valueRefBool . not =<< valueEq lval rval
                NLt   -> valueRefBool $ ls <  rs
                NLte  -> valueRefBool $ ls <= rs
                NGt   -> valueRefBool $ ls >  rs
                NGte  -> valueRefBool $ ls >= rs
                _     -> throwError unsupportedTypes

            (NVSet ls, NVSet rs) -> case op of
                NUpdate -> return $ NVSet $ rs `M.union` ls
                NEq -> valueRefBool =<< valueEq lval rval
                NNEq -> valueRefBool . not =<< valueEq lval rval
                _ -> throwError unsupportedTypes

            (NVList ls, NVList rs) -> case op of
                NConcat -> return $ NVList $ ls ++ rs
                NEq -> valueRefBool =<< valueEq lval rval
                NNEq -> valueRefBool . not =<< valueEq lval rval
                _ -> throwError unsupportedTypes

            (NVLiteralPath ls, NVLiteralPath rs) -> case op of
                -- TODO: Canonicalise path
                NPlus -> NVLiteralPath <$> makeAbsolutePath (ls ++ rs)
                _ -> throwError unsupportedTypes

            (NVLiteralPath ls, NVStr rs _) -> case op of
                -- TODO: Canonicalise path
                NPlus -> NVLiteralPath
                    <$> makeAbsolutePath (ls `mappend` Text.unpack rs)
                _ -> throwError unsupportedTypes

            _ -> throwError unsupportedTypes

eval (NSelect aset attr alternative) = do
    traceM "NSelect"
    mres <- evalSelect aset attr
    case mres of
        Right v -> do
            traceM $ "Wrapping a selector: " ++ show (() <$ v)
            pure v
        Left (s, ks) -> fromMaybe err alternative
          where
            err = throwError $ "could not look up attribute "
                ++ intercalate "." (map Text.unpack ks)
                ++ " in " ++ showValue s

eval (NHasAttr aset attr) = do
    traceM "NHasAttr"
    NVConstant . NBool . either (const False) (const True)
        <$> evalSelect aset attr

eval (NList l) = do
    traceM "NList"
    scope <- currentScopes
    NVList <$> for l (thunk . withScopes @(NThunk m) scope)

eval (NSet binds) = do
    traceM "NSet..1"
    s <- evalBinds True False binds
    traceM $ "NSet..2: s = " ++ show (() <$ s)
    return $ NVSet s

eval (NRecSet binds) = do
    traceM "NRecSet..1"
    s <- evalBindsWithAlter (attrSetAlterWithOverride force) True True binds
    traceM $ "NRecSet..2: s = " ++ show (() <$ s)
    return $ NVSet s

eval (NLet binds e) = do
    traceM "Let..1"
    s <- evalBinds True True binds
    traceM $ "Let..2: s = " ++ show (() <$ s)
    pushScope s e

eval (NIf cond t f) = do
    traceM "NIf"
    cond >>= \case
        NVConstant (NBool True) -> t
        NVConstant (NBool False) -> f
        x -> throwError $ "condition must be a boolean: "++ showValue x

eval (NWith scope body) = do
    traceM "NWith"
    s <- thunk scope
    pushWeakScope ?? body $ force s $ \case
        NVSet s -> return s
        x -> throwError $ "scope must be a set in with statement, but saw: "
                ++ showValue x

eval (NAssert cond body) = do
    traceM "NAssert"
    cond >>= \case
        NVConstant (NBool True) -> body
        NVConstant (NBool False) -> throwError "assertion failed"
        x -> throwError $ "assertion condition must be boolean, but saw: "
                ++ showValue x

eval (NApp fun arg) = do
    traceM "NApp"
    evalApp fun =<< thunk arg

eval (NAbs params body) = do
    traceM "NAbs"
    -- It is the environment at the definition site, not the call site, that
    -- needs to be used when evaluating the body and default arguments, hence
    -- we defer here so the present scope is restored when the parameters and
    -- body are forced during application.
    scope <- currentScopes @_ @(NThunk m)
    traceM $ "Creating lambda abstraction in scope: " ++ show scope
    return $ NVClosure scope (thunk <$> params) (thunk body)

infixl 1 `evalApp`
evalApp :: forall e m. (MonadEval e m, MonadFix m)
        => m (NValue m) -> NThunk m -> m (NValue m)
evalApp fun arg = fun >>= \case
    NVClosure scope params f -> do
        traceM "evalApp:NVFunction"
        env <- currentScopes @_ @(NThunk m)
        args <- buildArgument params =<< thunk (withScopes env (force arg pure))
        traceM $ "Evaluating function application with args: "
            ++ show (newScope args)
        withScopes @(NThunk m) scope $ pushScope args $
            force ?? pure =<< f
    NVBuiltin name f -> do
        traceM $ "evalApp:NVBuiltin " ++ name
        env <- currentScopes @_ @(NThunk m)
        f =<< thunk (withScopes env (force arg pure))
    s@(NVSet m) | Just f <- M.lookup "__functor" m -> do
        traceM "evalApp:__functor"
        force f $ \f' -> pure f' `evalApp` valueThunk s `evalApp` arg
    x -> throwError $ "Attempt to call non-function: " ++ showValue x

-----

valueRefBool :: MonadNix m => Bool -> m (NValue m)
valueRefBool = return . NVConstant . NBool

valueRefInt :: MonadNix m => Integer -> m (NValue m)
valueRefInt = return . NVConstant . NInt

valueRefFloat :: MonadNix m => Float -> m (NValue m)
valueRefFloat = return . NVConstant . NFloat

thunkEq :: (MonadNix m, Framed e m, MonadFile m, MonadVar m)
        => NThunk m -> NThunk m -> m Bool
thunkEq lt rt = force lt $ \lv -> force rt $ \rv -> valueEq lv rv

-- | Checks whether two containers are equal, using the given item equality
--   predicate. If there are any item slots that don't match between the two
--   containers, the result will be False.
alignEqM
    :: (Align f, Traversable f, Monad m)
    => (a -> b -> m Bool)
    -> f a
    -> f b
    -> m Bool
alignEqM eq fa fb = fmap (either (const False) (const True)) $ runExceptT $ do
    pairs <- forM (align fa fb) $ \case
        These a b -> return (a, b)
        _ -> throwE ()
    forM_ pairs $ \(a, b) -> guard =<< lift (eq a b)

valueEq :: (MonadNix m, Framed e m, MonadFile m, MonadVar m)
        => NValue m -> NValue m -> m Bool
valueEq l r = case (l, r) of
    (NVStr ls _, NVConstant (NUri ru)) -> pure $ ls == ru
    (NVConstant (NUri lu), NVStr rs _) -> pure $ lu == rs
    (NVConstant lc, NVConstant rc) -> pure $ lc == rc
    (NVStr ls _, NVStr rs _) -> pure $ ls == rs
    (NVList ls, NVList rs) -> alignEqM thunkEq ls rs
    (NVSet lm, NVSet rm) -> alignEqM thunkEq lm rm
    (NVLiteralPath lp, NVLiteralPath rp) -> pure $ lp == rp
    (NVEnvPath lp, NVEnvPath rp) -> pure $ lp == rp
    _ -> pure False

-----

normalFormBy :: forall e m. MonadEval e m
             => (forall r. NThunk m -> (NValue m -> m r) -> m r)
             -> NValue m
             -> m (NValueNF m)
normalFormBy k = \case
    NVConstant a     -> return $ Fix $ NVConstant a
    NVStr t s        -> return $ Fix $ NVStr t s
    NVList l         ->
        Fix . NVList <$> traverse (`k` normalFormBy k) l
    NVSet s          ->
        Fix . NVSet <$> traverse (`k` normalFormBy k) s
    NVClosure s p f   -> withScopes @(NThunk m) s $ do
        p' <- traverse (fmap (`k` normalFormBy k)) p
        return $ Fix $
            NVClosure emptyScopes p' ((`k` normalFormBy k) =<< f)
    NVLiteralPath fp -> return $ Fix $ NVLiteralPath fp
    NVEnvPath p      -> return $ Fix $ NVEnvPath p
    NVBuiltin name f -> return $ Fix $ NVBuiltin name f

normalForm :: forall e m. MonadEval e m => NValue m -> m (NValueNF m)
normalForm = normalFormBy force

valueText :: forall e m. (MonadEval e m, MonadNix m)
          => Bool -> NValueNF m -> m (Text, DList Text)
valueText addPathsToStore = cata phi where
    phi :: NValueF m (m (Text, DList Text)) -> m (Text, DList Text)
    phi (NVConstant a)    = pure (atomText a, mempty)
    phi (NVStr t c)       = pure (t, c)
    phi (NVList _)        = throwError "Cannot coerce a list to a string"
    phi (NVSet set)
      | Just asString <-
        -- TODO: Should this be run through valueText recursively?
        M.lookup "__asString" set = asString
      | otherwise = throwError "Cannot coerce a set to a string"
    phi NVClosure {} = throwError "Cannot coerce a function to a string"
    phi (NVLiteralPath originalPath)
        | addPathsToStore = do
            -- TODO: Capture and use the path of the file being processed as the
            -- base path
            storePath <- addPath originalPath
            pure (Text.pack $ unStorePath storePath, mempty)
        | otherwise = pure (Text.pack originalPath, mempty)
    phi (NVEnvPath p)     =
        -- TODO: Ensure this is a store path
        pure (Text.pack p, mempty)
    phi (NVBuiltin _ _)    = throwError "Cannot coerce a function to a string"

valueTextNoContext :: (MonadEval e m, MonadNix m)
                   => Bool -> NValueNF m -> m Text
valueTextNoContext addPathsToStore = fmap fst . valueText addPathsToStore

----

-- | The following functions are generalized so that they can be used by other
--   evaluators which do not differ in the core aspects of the lambda calculus
--   that Nix represents.
--
--   jww (2018-04-02): This "subset" of the language should be called out more
--   directly, as a separate data type, to avoid abstracting it in this ad hoc
--   way.

class (Monoid (MText m), Coercible (Thunk m v) t)
      => MonadExpr t v m | m -> t, m -> v where
    embedSet   :: HashMap Text t -> m v
    projectSet :: v -> m (Maybe (HashMap Text t))

    type MText m :: *

    wrapText   :: Text -> m (MText m)
    unwrapText :: MText m -> m Text

    embedText   :: MText m  -> m v
    projectText :: v -> m (Maybe (Maybe (MText m)))

buildArgument
    :: forall e t v m. (MonadExpr t v m, Scoped e t m, Framed e m,
                  MonadVar m, MonadFix m, MonadFile m)
    => Params (m t) -> t -> m (HashMap Text t)
buildArgument params arg = case params of
    Param name -> return $ M.singleton name arg
    ParamSet s isVariadic m ->
        forceThunk (coerce arg) $ projectSet @t @v @m >=> \case
            Just args -> do
                let inject = case m of
                        Nothing -> id
                        Just n -> M.insert n $ const $ pure arg
                loebM (inject $ alignWithKey (assemble isVariadic) args (toHashMap s))

            x -> throwError $ "Expected set in function call, received: "
                    ++ show (void x)
  where
    assemble :: Bool
             -> Text
             -> These t (Maybe (m t))
             -> HashMap Text t
             -> m t
    assemble isVariadic k = \case
        That Nothing  ->
            const $ throwError $ "Missing value for parameter: " ++ show k
        That (Just f) -> \args -> do
            scope <- currentScopes @_ @t
            traceM $ "Deferring default argument in scope: " ++ show scope
            fmap (coerce @(Thunk m v) @t) $ buildThunk $ clearScopes @t $ do
                traceM $ "Evaluating default argument with args: "
                    ++ show (newScope args)
                pushScopes scope $ pushScope args $
                    (\x -> forceThunk (coerce x) pure) =<< f
        This x | isVariadic -> const (pure x)
               | otherwise  ->
                 const $ throwError $ "Unexpected parameter: " ++ show k
        These x _ -> const (pure x)

attrSetAlter
    :: forall e t v m. (MonadExpr t v m, Scoped e t m, Framed e m,
                  MonadVar m, MonadFile m)
    => Bool
    -> [Text]
    -> HashMap Text (m v)
    -> m v
    -> m (HashMap Text (m v))
attrSetAlter _ [] _ _ = throwError "invalid selector with no components"
attrSetAlter overwrite (p:ps) m val = case M.lookup p m of
    Nothing
        | null ps   -> go
        | otherwise -> recurse M.empty
    Just v
        | null ps   -> go
        | otherwise -> v >>= projectSet @t @v @m >>= \case
              Just s -> recurse ((\x -> forceThunk (coerce x) pure) <$> s)
              -- TODO: Keep a stack of attributes we've already traversed, so
              -- that we can report that to the user
              x -> throwError $ "attribute " ++ show p
                      ++ " is not a set, but a " ++ show (void x)
  where
    go = return $ M.insertWith handleCollision p val m
    handleCollision = if overwrite
      then \new _   -> new
      else \_   old -> old

    recurse s = attrSetAlter overwrite ps s val >>= \m' ->
        if | M.null m' -> return m
           | otherwise   -> do
             scope <- currentScopes @_ @t
             return $ M.insert p (embed scope m') m
      where
        embed scope m' = embedSet @t @v @m
            =<< traverse (fmap coerce . buildThunk . withScopes scope) m'

attrSetAlterWithOverride
    :: forall e t v m. (MonadExpr t v m, Scoped e t m, Framed e m,
                  MonadVar m, MonadFix m, MonadFile m)
    => (t -> (v -> m v) -> m v)
    -> [Text]
    -> HashMap Text (m v)
    -> m v
    -> m (HashMap Text (m v))
attrSetAlterWithOverride forceT = \case
  path@["__overrides"] -> \m -> (=<<) $ \v -> projectSet v >>= \case
    Just overrides -> do
      initial <- attrSetAlter False path m $ return v
      let update :: HashMap Text (m v) -> (Text, t) -> m (HashMap Text (m v))
          update m (name, t) = attrSetAlter True [name] m $ forceT t pure
      foldM update initial $ M.toList overrides
    Nothing -> throwError "__overrides must be a set"
  path -> attrSetAlter False path

evalBinds
    :: forall e t v m. (MonadExpr t v m, Scoped e t m, Framed e m,
                  MonadVar m, MonadFix m, MonadFile m)
    => Bool
    -> Bool
    -> [Binding (m v)]
    -> m (HashMap Text t)
evalBinds = evalBindsWithAlter $ attrSetAlter False

evalBindsWithAlter
    :: forall e t v m. (MonadExpr t v m, Scoped e t m, Framed e m,
                  MonadVar m, MonadFix m, MonadFile m)
    => ([Text] -> HashMap Text (m v) -> m v -> m (HashMap Text (m v)))
    -> Bool
    -> Bool
    -> [Binding (m v)]
    -> m (HashMap Text t)
evalBindsWithAlter alter allowDynamic recursive = buildResult . concat <=< mapM go
  where
    go :: Binding (m v) -> m [([Text], m v)]
    -- jww (2018-04-05): This needs to always happen last, so that it, you
    -- know, overrides. It works if __overrides occurs as the last attribute.
    go (NamedVar [StaticKey "__overrides"] finalValue) =
        finalValue >>= projectSet >>= \case
            Just o' -> do
                traceM $ "evalBinds..3: o = " ++ show (() <$ o')
                return $ map (first (:[])) $
                    fmap (fmap (\x -> forceThunk (coerce x) pure))
                               (M.toList o')
            x -> throwError $ "__overrides must be a set, but saw: "
                        ++ show (void x)

    go (NamedVar pathExpr finalValue) = do
        let go = \case
                [] -> pure ([], finalValue)
                h : t -> evalSetterKeyName allowDynamic h >>= \case
                    Nothing -> do
                        s <- embedSet mempty
                        pure ([], pure s)
                    Just k -> do
                        (restOfPath, v) <- go t
                        pure (k : restOfPath, v)
        go pathExpr >>= \case
            -- When there are no path segments, e.g. `${null} = 5;`, we don't
            -- bind anything
            ([], _) -> pure []
            result -> pure [result]

    go (Inherit ms names) = fmap catMaybes $ forM names $ \name ->
        evalSetterKeyName allowDynamic name >>= \case
            Nothing -> return Nothing
            Just key -> return $ Just ([key], do
                mv <- case ms of
                    Nothing -> lookupVar key
                    Just s -> s >>= projectSet >>= \case
                        Just s -> pushScope s (lookupVar @_ @t key)
                        x -> throwError
                            $ "First argument to inherit should be a set, saw: "
                            ++ show (void x)
                case mv of
                    Nothing -> throwError $ "Inheriting unknown attribute: "
                        ++ show (void name)
                    Just v -> forceThunk (coerce v) pure)

    buildResult :: [([Text], m v)] -> m (HashMap Text t)
    buildResult bindings = do
        s <- foldM insert M.empty bindings
        scope <- currentScopes @_ @t
        if recursive
            then loebM (encapsulate scope <$> s)
            else traverse (fmap coerce . buildThunk . withScopes scope) s

    encapsulate scope f attrs =
        fmap coerce . buildThunk . withScopes scope . pushScope attrs $ f

    insert m (path, value) = alter path m value

evalSelect
    :: (MonadExpr t v m, Framed e m, MonadVar m, MonadFile m)
    => m (NValue m)
    -> NAttrPath (m v)
    -> m (Either (NValueF m (NThunk m), [Text]) (NValueF m (NThunk m)))
evalSelect aset attr =
    join $ extract <$> aset <*> evalSelector True attr
  where
    extract (NVSet s) (k:ks) = case M.lookup k s of
        Just v  -> force v $ extract ?? ks
        Nothing -> return $ Left (NVSet s, k:ks)
    extract x (k:ks) = return $ Left (x, k:ks)
    extract v [] = return $ Right v

evalSelector :: (Framed e m, MonadExpr t v m, MonadFile m)
             => Bool -> NAttrPath (m v) -> m [Text]
evalSelector allowDynamic = mapM $ evalGetterKeyName allowDynamic

evalKeyNameStatic :: (Framed e m, MonadExpr t v m, MonadFile m)
                  => NKeyName (m v) -> m Text
evalKeyNameStatic = \case
    StaticKey k -> pure k
    DynamicKey _ -> throwError "dynamic attribute not allowed in this context"

-- | Returns Nothing iff the key value is null
evalKeyNameDynamicNullable :: (Framed e m, MonadExpr t v m, MonadFile m)
                           => NKeyName (m v) -> m (Maybe Text)
evalKeyNameDynamicNullable = \case
    StaticKey k -> pure $ Just k
    DynamicKey k -> runAntiquoted evalString id k >>= projectText >>= \case
        Just (Just s) -> Just <$> unwrapText s
        Just Nothing -> return Nothing
        bad -> throwError $ "evaluating key name: expected string, got "
                  ++ show (void bad)

evalKeyNameDynamicNotNull :: (Framed e m, MonadExpr t v m, MonadFile m)
                          => NKeyName (m v) -> m Text
evalKeyNameDynamicNotNull = evalKeyNameDynamicNullable >=> \case
    Nothing -> throwError "value is null while a string was expected"
    Just k -> pure k

-- | Evaluate a component of an attribute path in a context where we are
-- *binding* a value
evalSetterKeyName :: (Framed e m, MonadExpr t v m, MonadFile m)
                  => Bool -> NKeyName (m v) -> m (Maybe Text)
evalSetterKeyName canBeDynamic
    | canBeDynamic = evalKeyNameDynamicNullable
    | otherwise    = fmap Just . evalKeyNameStatic

-- | Evaluate a component of an attribute path in a context where we are
-- *retrieving* a value
evalGetterKeyName :: (Framed e m, MonadExpr t v m, MonadFile m)
                  => Bool -> NKeyName (m v) -> m Text
evalGetterKeyName canBeDynamic
    | canBeDynamic = evalKeyNameDynamicNotNull
    | otherwise    = evalKeyNameStatic

evalString :: forall e t v m. (Framed e m, MonadExpr t v m, MonadFile m)
           => NString (m v) -> m v
evalString = \case
    Indented     parts -> fromParts parts
    DoubleQuoted parts -> fromParts parts
  where
    go = runAntiquoted (wrapText @t @v @m) $ \x -> do
        x' <- x
        projectText @t @v @m x' >>= \case
            Just (Just txt) -> return txt
            _ -> throwError "Value cannot be rendered as text"

    fromParts parts = embedText @t @v @m . mconcat =<< mapM go parts

-----

tracingEvalExpr :: (Framed e m, MonadIO n, Alternative n)
                => (NExprF (m v) -> m v) -> NExprLoc -> n (m v)
tracingEvalExpr eval =
    flip runReaderT (0 :: Int)
        . adiM (pure <$> eval . annotated . getCompose) psi
  where
    psi k v@(Fix x) = do
        depth <- ask
        guard (depth < 200)
        liftIO $ putStrLn $ "eval: " ++ replicate (depth * 2) ' '
            ++ show (stripAnnotation v)
        res <- local succ $
            fmap (withExprContext (() <$ x)) (k v)
        liftIO $ putStrLn $ "eval: " ++ replicate (depth * 2) ' ' ++ "."
        return res

framedEvalExpr :: Framed e m
               => (NExprF (m v) -> m v) -> NExprLoc -> m v
framedEvalExpr eval = adi (eval . annotated . getCompose) psi
  where
    psi k v@(Fix x) = withExprContext (() <$ x) (k v)

-----

{-
streamValues :: MonadVar m => NValue m -> Stream (NValueF m) m ()
streamValues = void . yields . fmap go
  where
    go (NThunk (Left v)) = streamValues v
    go (NThunk v) = effect (streamValues <$> forceThunk v)
-}

