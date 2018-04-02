{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Eval
    (MonadNixEval, evalExpr, tracingExprEval, evalBinds,
     exprNormalForm, normalForm, builtin, builtin2, builtin3,
     atomText, valueText, valueTextNoContext, buildArgument,
     contextualExprEval, thunkEq, evalApp) where

import           Control.Monad hiding (mapM, sequence)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Align
import           Data.Align.Key
import           Data.Fix
import           Data.Functor.Compose
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.List (intercalate)
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.These
import           Data.Traversable (for)
import           Nix.Atoms
import           Nix.Expr
import           Nix.Monad
import           Nix.Scope
import           Nix.Stack
import           Nix.StringOperations (runAntiquoted)
import           Nix.Utils

type MonadNixEval e m =
    (Scoped e (NThunk m) m, Framed e m, MonadNix m, MonadIO m )

-- | Evaluate an nix expression, with a given ValueSet as environment
evalExpr :: MonadNixEval e m => NExpr -> m (NValue m)
evalExpr = cata eval

eval :: forall e m. MonadNixEval e m => NExprF (m (NValue m)) -> m (NValue m)

eval (NSym var) = do
    traceM $ "NSym..1: var = " ++ show var
    mres <- lookupVar var
    case mres of
        Nothing -> throwError $ "Undefined variable: " ++ show var
        Just v -> force v

eval (NConstant x)    = return $ NVConstant x
eval (NStr str)       = evalString str
eval (NLiteralPath p) = NVLiteralPath <$> makeAbsolutePath p
eval (NEnvPath p)     = return $ NVEnvPath p

eval (NUnary op arg) = arg >>= \case
    NVConstant c -> case (op, c) of
        (NNeg, NInt   i) -> return $ NVConstant $ NInt   (-i)
        (NNeg, NFloat f) -> return $ NVConstant $ NFloat (-f)
        (NNot, NBool  b) -> return $ NVConstant $ NBool  (not b)
        _ -> throwError $ "unsupported argument type for unary operator "
                 ++ show op
    _ -> throwError "argument to unary operator must evaluate to an atomic type"

eval (NBinary op larg rarg) = do
    lval <- larg
    rval <- rarg
    let unsupportedTypes =
            "unsupported argument types for binary operator "
                ++ show (() <$ lval, op, () <$ rval)
        numBinOp :: (forall a. Num a => a -> a -> a) -> NAtom -> NAtom -> m (NValue m)
        numBinOp f = numBinOp' f f
        numBinOp'
            :: (Integer -> Integer -> Integer)
            -> (Float -> Float -> Float)
            -> NAtom -> NAtom -> m (NValue m)
        numBinOp' intF floatF l r = case (l, r) of
            (NInt   li, NInt   ri) -> valueRefInt   $             li `intF`               ri
            (NInt   li, NFloat rf) -> valueRefFloat $ fromInteger li `floatF`             rf
            (NFloat lf, NInt   ri) -> valueRefFloat $             lf `floatF` fromInteger ri
            (NFloat lf, NFloat rf) -> valueRefFloat $             lf `floatF`             rf
            _ -> throwError unsupportedTypes
    case (lval, rval) of
        (NVConstant lc, NVConstant rc) -> case (op, lc, rc) of
            -- TODO: Refactor so that eval (NBinary ..) dispatches based on
            -- operator first
            (NEq,  _, _) -> valueRefBool =<< valueEq lval rval
            (NNEq, _, _) -> valueRefBool . not =<< valueEq lval rval
            (NLt,  l, r) -> valueRefBool $ l <  r
            (NLte, l, r) -> valueRefBool $ l <= r
            (NGt,  l, r) -> valueRefBool $ l >  r
            (NGte, l, r) -> valueRefBool $ l >= r
            (NAnd,  NBool l, NBool r) -> valueRefBool $ l && r
            (NOr,   NBool l, NBool r) -> valueRefBool $ l || r
            (NImpl, NBool l, NBool r) -> valueRefBool $ not l || r
            (NPlus,  l, r) -> numBinOp (+) l r
            (NMinus, l, r) -> numBinOp (-) l r
            (NMult,  l, r) -> numBinOp (*) l r
            (NDiv,   l, r) -> numBinOp' div (/) l r
            _ -> throwError unsupportedTypes

        (NVStr ls lc, NVStr rs rc) -> case op of
            NPlus -> return $ NVStr (ls `mappend` rs) (lc `mappend` rc)
            NEq  -> valueRefBool =<< valueEq lval rval
            NNEq -> valueRefBool . not =<< valueEq lval rval
            _    -> throwError unsupportedTypes

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
            NPlus -> NVLiteralPath <$> makeAbsolutePath (ls `mappend` Text.unpack rs)
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
    extract (NVSet s) (k:ks) = case M.lookup k s of
        Just v -> do
            s' <- force v
            extract s' ks
        Nothing -> return Nothing
    extract _ (_:_) = return Nothing
    extract v [] = return $ Just v

eval (NHasAttr aset attr) = aset >>= \case
    NVSet s -> evalSelector True attr >>= \case
        [keyName] ->
            return $ NVConstant $ NBool $ keyName `M.member` s
        _ -> throwError "attr name argument to hasAttr is not a single-part name"
    _ -> throwError "argument to hasAttr has wrong type"

eval (NList l) = do
    scope <- currentScopes
    NVList <$> for l (thunk . withScopes @(NThunk m) scope)

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

eval (NApp fun arg) = evalApp fun arg

eval (NAbs params body) = do
    -- It is the environment at the definition site, not the call site, that
    -- needs to be used when evaluating the body and default arguments, hence
    -- we defer here so the present scope is restored when the parameters and
    -- body are forced during application.
    scope <- currentScopes @_ @(NThunk m)
    traceM $ "Creating lambda abstraction in scope: " ++ show scope
    return $ NVFunction
        (thunk . pushScopes scope <$> params)
        (thunk (pushScopes scope body))

infixl 1 `evalApp`
evalApp :: forall e m. MonadNixEval e m
        => m (NValue m) -> m (NValue m) -> m (NValue m)
evalApp fun arg = fun >>= \case
    NVFunction params f -> do
        args <- buildArgument params =<< thunk arg
        traceM $ "Evaluating function application with args: "
            ++ show (newScope args)
        clearScopes @(NThunk m) (pushScope args (force =<< f))
    NVBuiltin _ f -> f =<< thunk arg
    NVSet m
        | Just f <- M.lookup "__functor" m
            -> force f `evalApp` fun `evalApp` arg
    x -> throwError $ "Attempt to call non-function: " ++ show (() <$ x)

valueRefBool :: MonadNix m => Bool -> m (NValue m)
valueRefBool = return . NVConstant . NBool

valueRefInt :: MonadNix m => Integer -> m (NValue m)
valueRefInt = return . NVConstant . NInt

valueRefFloat :: MonadNix m => Float -> m (NValue m)
valueRefFloat = return . NVConstant . NFloat

thunkEq :: (MonadNix m, MonadIO m) => NThunk m -> NThunk m -> m Bool
thunkEq lt rt = do
    lv <- force lt
    rv <- force rt
    valueEq lv rv

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

valueEq :: (MonadNix m, MonadIO m) => NValue m -> NValue m -> m Bool
valueEq l r = case (l, r) of
    (NVConstant lc, NVConstant rc) -> pure $ lc == rc
    (NVStr ls _, NVStr rs _) -> pure $ ls == rs
    (NVList ls, NVList rs) -> alignEqM thunkEq ls rs
    (NVSet lm, NVSet rm) -> alignEqM thunkEq lm rm
    _ -> pure False

buildArgument :: forall e m. MonadNixEval e m
              => Params (m (NThunk m)) -> NThunk m -> m (ValueSet m)
buildArgument params arg = case params of
    Param name -> return $ M.singleton name arg
    ParamSet s isVariadic m -> force arg >>= \case
        NVSet args -> do
            res <- loebM (alignWithKey (assemble isVariadic) args s)
            maybe (pure res) (selfInject res) m

        x -> throwError $ "Expected set in function call, received: "
                ++ show (() <$ x)
  where
    selfInject :: ValueSet m -> Text -> m (ValueSet m)
    selfInject res n = do
        ref <- valueThunk $ NVSet res
        return $ M.insert n ref res

    assemble :: Bool
             -> Text
             -> These (NThunk m) (Maybe (m (NThunk m)))
             -> ValueSet m
             -> m (NThunk m)
    assemble isVariadic k = \case
        That Nothing  ->
            const $ throwError $ "Missing value for parameter: " ++ show k
        That (Just f) -> \args -> do
            scope <- currentScopes @_ @(NThunk m)
            traceM $ "Deferring default argument in scope: " ++ show scope
            thunk $ clearScopes @(NThunk m) $ do
                traceM $ "Evaluating default argument with args: "
                    ++ show (newScope args)
                pushScopes scope $ pushScope args $
                    force =<< f
        This x | isVariadic -> const (pure x)
               | otherwise  ->
                 const $ throwError $ "Unexpected parameter: " ++ show k
        These x _ -> const (pure x)

attrSetAlter :: forall e m. MonadNixEval e m
             => [Text]
             -> HashMap Text (m (NValue m))
             -> m (NValue m)
             -> m (HashMap Text (m (NValue m)))
attrSetAlter [] _ _ = throwError "invalid selector with no components"
attrSetAlter (p:ps) m val = case M.lookup p m of
    Nothing | null ps   -> go
            | otherwise -> recurse M.empty
    Just v  | null ps   -> go
            | otherwise -> v >>= \case
                  NVSet s -> recurse (force <$> s)
                  --TODO: Keep a stack of attributes we've already traversed, so
                  --that we can report that to the user
                  x -> throwError $ "attribute " ++ show p
                    ++ " is not a set; its value is " ++ show (void x)
  where
    go = return $ M.insert p val m

    recurse s = attrSetAlter ps s val >>= \m' ->
        if | M.null m' -> return m
           | otherwise   -> do
             scope <- currentScopes @_ @(NThunk m)
             return $ M.insert p (embed scope m') m
      where
        embed scope m' =
            NVSet <$> traverse (thunk . withScopes scope) m'

evalBinds :: forall e m. MonadNixEval e m
          => Bool
          -> Bool
          -> [Binding (m (NValue m))]
          -> m (ValueSet m)
evalBinds allowDynamic recursive = buildResult . concat <=< mapM go
  where
    go :: Binding (m (NValue m)) -> m [([Text], m (NValue m))]
    go (NamedVar pathExpr finalValue) = do
        let go = \case
                [] -> pure ([], finalValue)
                h : t -> evalSetterKeyName allowDynamic h >>= \case
                    Nothing -> pure ([], pure $ NVSet mempty)
                    Just k -> do
                        (restOfPath, v) <- go t
                        pure (k : restOfPath, v)
        go pathExpr >>= \case
            -- When there are no path segments, e.g. `${null} = 5;`, we don't
            -- bind anything
            ([], _) -> pure []
            result -> pure [result]
    go (Inherit ms names) = fmap catMaybes $ forM names $ \name -> do
        evalSetterKeyName allowDynamic name >>= \case
            Nothing -> return Nothing
            Just key -> return $ Just ([key], do
                mv <- case ms of
                    Nothing -> lookupVar key
                    Just s -> s >>= \case
                        NVSet s -> pushScope s (lookupVar key)
                        x -> throwError
                            $ "First argument to inherit should be a set, saw: "
                            ++ show (() <$ x)
                case mv of
                    Nothing -> throwError $ "Inheriting unknown attribute: "
                        ++ show (() <$ name)
                    Just v -> force v)

    buildResult :: [([Text], m (NValue m))] -> m (ValueSet m)
    buildResult bindings = do
        s <- foldM insert M.empty bindings
        scope <- currentScopes @_ @(NThunk m)
        if recursive
            then loebM (encapsulate scope <$> s)
            else traverse (thunk . withScopes scope) s

    encapsulate scope f attrs =
        thunk . withScopes scope . pushScope attrs $ f

    insert m (path, value) = attrSetAlter path m value

evalString :: MonadNixEval e m
           => NString (m (NValue m)) -> m (NValue m)
evalString nstr = do
    let fromParts parts = do
          (t, c) <- mconcat <$> mapM go parts
          return $ NVStr t c
    case nstr of
      Indented     parts -> fromParts parts
      DoubleQuoted parts -> fromParts parts
  where
    go = runAntiquoted (return . (, mempty))
                       (valueText True <=< (normalForm =<<))

evalSelector :: MonadNixEval e m
             => Bool -> NAttrPath (m (NValue m)) -> m [Text]
evalSelector allowDynamic = mapM $ evalGetterKeyName allowDynamic

evalKeyNameStatic :: MonadNixEval e m => NKeyName (m (NValue m)) -> m Text
evalKeyNameStatic = \case
    StaticKey k -> pure k
    DynamicKey _ -> throwError "dynamic attribute not allowed in this context"

-- | Returns Nothing iff the key value is null
evalKeyNameDynamicNullable :: MonadNixEval e m => NKeyName (m (NValue m)) -> m (Maybe Text)
evalKeyNameDynamicNullable = \case
    StaticKey k -> pure $ Just k
    DynamicKey k -> runAntiquoted evalString id k >>= \case
        NVStr s _ -> pure $ Just s
        NVConstant NNull -> pure Nothing
        bad -> throwError $ "evaluating key name: expected string, got "
                  ++ show (void bad)

evalKeyNameDynamicNotNull :: MonadNixEval e m => NKeyName (m (NValue m)) -> m Text
evalKeyNameDynamicNotNull = evalKeyNameDynamicNullable >=> \case
    Nothing -> throwError "value is null while a string was expected"
    Just k -> pure k

-- | Evaluate a component of an attribute path in a context where we are
-- *binding* a value
evalSetterKeyName :: MonadNixEval e m => Bool -> NKeyName (m (NValue m)) -> m (Maybe Text)
evalSetterKeyName canBeDynamic = case canBeDynamic of
    False -> fmap Just . evalKeyNameStatic
    True -> evalKeyNameDynamicNullable

-- | Evaluate a component of an attribute path in a context where we are
-- *retrieving* a value
evalGetterKeyName :: MonadNixEval e m => Bool -> NKeyName (m (NValue m)) -> m Text
evalGetterKeyName canBeDynamic = case canBeDynamic of
    False -> evalKeyNameStatic
    True -> evalKeyNameDynamicNotNull

contextualExprEval :: MonadNixEval e m => NExprLoc -> m (NValue m)
contextualExprEval = adi (eval . annotated . getCompose) psi
  where
    psi k v@(Fix x) = withExprContext (() <$ x) (k v)

tracingExprEval :: MonadNixEval e m => NExprLoc -> IO (m (NValue m))
tracingExprEval =
    flip runReaderT (0 :: Int)
        . adiM (pure <$> eval . annotated . getCompose) psi
  where
    psi k v@(Fix x) = do
        depth <- ask
        liftIO $ putStrLn $ "eval: " ++ replicate (depth * 2) ' '
            ++ show (stripAnnotation v)
        res <- local succ $
            fmap (withExprContext (() <$ x)) (k v)
        liftIO $ putStrLn $ "eval: " ++ replicate (depth * 2) ' ' ++ "."
        return res

exprNormalForm :: MonadNixEval e m => NExpr -> m (NValueNF m)
exprNormalForm = normalForm <=< evalExpr

normalForm :: (MonadNix m, MonadIO m) => NValue m -> m (NValueNF m)
normalForm = \case
    NVConstant a     -> return $ Fix $ NVConstant a
    NVStr t s        -> return $ Fix $ NVStr t s
    NVList l         ->
        Fix . NVList <$> traverse (normalForm <=< force) l
    NVSet s          ->
        Fix . NVSet <$> traverse (normalForm <=< force) s
    NVFunction p f   -> do
        p' <- traverse (fmap (normalForm <=< force)) p
        return $ Fix $
            NVFunction p' (normalForm =<< force =<< f)
    NVLiteralPath fp -> return $ Fix $ NVLiteralPath fp
    NVEnvPath p      -> return $ Fix $ NVEnvPath p
    NVBuiltin name f -> return $ Fix $ NVBuiltin name f

valueText :: forall e m. MonadNixEval e m
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
    phi (NVFunction _ _)  = throwError "Cannot coerce a function to a string"
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

valueTextNoContext :: MonadNixEval e m => Bool -> NValueNF m -> m Text
valueTextNoContext addPathsToStore = fmap fst . valueText addPathsToStore
