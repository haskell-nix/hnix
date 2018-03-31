{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Eval
    (evalExpr, tracingExprEval, evalBinds, exprNormalForm, normalForm,
     builtin, builtin2, builtin3, atomText, valueText, buildArgument,
     contextualExprEval, thunkEq
    ) where

import           Control.Monad hiding (mapM, sequence)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Align
import           Data.Align.Key
import           Data.Fix
import           Data.Functor.Compose
import           Data.List (intercalate)
import qualified Data.Map.Lazy as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.These
import           Nix.Atoms
import           Nix.Expr
import           Nix.Monad
import           Nix.Scope
import           Nix.StringOperations (runAntiquoted)
import           Nix.Utils

-- | Evaluate an nix expression, with a given ValueSet as environment
evalExpr :: MonadNix m => NExpr -> m (NValue m)
evalExpr = cata eval

eval :: MonadNix m => NExprF (m (NValue m)) -> m (NValue m)

eval (NSym var) = do
    traceM $ "NSym..1: var = " ++ show var
    mres <- lookupVar var
    case mres of
        Nothing -> throwError $ "Undefined variable: " ++ show var
        Just v -> return v

eval (NConstant x)    = return $ NVConstant x
eval (NStr str)       = evalString str
eval (NLiteralPath p) = NVLiteralPath <$> makeAbsolutePath p
eval (NEnvPath p)     = return $ NVEnvPath p

eval (NUnary op arg) = arg >>= \case
    NVConstant c -> case (op, c) of
        (NNeg, NInt  i) -> return $ NVConstant $ NInt  (-i)
        (NNot, NBool b) -> return $ NVConstant $ NBool (not b)
        _ -> throwError $ "unsupported argument type for unary operator "
                 ++ show op
    _ -> throwError "argument to unary operator must evaluate to an atomic type"

eval (NBinary op larg rarg) = do
    lval <- larg
    rval <- rarg
    let unsupportedTypes =
            "unsupported argument types for binary operator "
                ++ show (() <$ lval, op, () <$ rval)
    case (lval, rval) of
        (NVConstant lc, NVConstant rc) -> case (op, lc, rc) of
            (NEq,  _, _) ->
                -- TODO: Refactor so that eval (NBinary ..) dispatches based
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
            NUpdate -> return $ NVSet $ rs `Map.union` ls
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
    extract (NVSet s) (k:ks) = case Map.lookup k s of
        Just v  -> do
            s' <- forceThunk v
            extract s' ks
        Nothing -> return Nothing
    extract _ (_:_) = return Nothing
    extract v [] = return $ Just v

eval (NHasAttr aset attr) = aset >>= \case
    NVSet s -> evalSelector True attr >>= \case
        [keyName] ->
            return $ NVConstant $ NBool $ keyName `Map.member` s
        _ -> throwError "attr name argument to hasAttr is not a single-part name"
    _ -> throwError "argument to hasAttr has wrong type"

eval (NList l) = do
    scope <- currentScope
    NVList <$> traverse (deferInScope scope) l

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
    scope <- currentScope
    traceM $ "Creating lambda abstraction in scope: " ++ show scope
    return $ NVFunction (buildThunk . pushScopes scope <$> params)
                        (buildThunk (pushScopes scope body))

infixl 1 `evalApp`
evalApp :: MonadNix m => m (NValue m) -> m (NValue m) -> m (NValue m)
evalApp fun arg = fun >>= \case
    NVFunction params f -> do
        args <- buildArgument params =<< buildThunk arg
        traceM $ "Evaluating function application with args: "
            ++ show (newScope args)
        clearScopes (pushScope args (forceThunk =<< f))
    NVBuiltin _ f -> f =<< buildThunk arg
    NVSet m
        | Just f <- Map.lookup "__functor" m
            -> forceThunk f `evalApp` fun `evalApp` arg
    x -> throwError $ "Attempt to call non-function: " ++ show (() <$ x)

valueRefBool :: MonadNix m => Bool -> m (NValue m)
valueRefBool = return . NVConstant . NBool

valueRefInt :: MonadNix m => Integer -> m (NValue m)
valueRefInt = return . NVConstant . NInt

thunkEq :: MonadNix m => NThunk m -> NThunk m -> m Bool
thunkEq lt rt = do
    lv <- forceThunk lt
    rv <- forceThunk rt
    valueEq lv rv

-- | Checks whether two containers are equal, using the given item equality predicate.  If there are any item slots that don't match between the two containers, the result will be False.
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

valueEq :: MonadNix m => NValue m -> NValue m -> m Bool
valueEq l r = case (l, r) of
    (NVConstant lc, NVConstant rc) -> pure $ lc == rc
    (NVStr ls _, NVStr rs _) -> pure $ ls == rs
    (NVList ls, NVList rs) -> alignEqM thunkEq ls rs
    (NVSet lm, NVSet rm) -> alignEqM thunkEq lm rm
    _ -> pure False

buildArgument :: forall m. MonadNix m
              => Params (m (NThunk m)) -> NThunk m -> m (ValueSet m)
buildArgument params arg = case params of
    Param name -> return $ Map.singleton name arg
    ParamSet ps m -> go ps m
  where
    go ps m = forceThunk arg >>= \case
        NVSet args -> do
            let (s, isVariadic) = case ps of
                  FixedParamSet    s' -> (s', False)
                  VariadicParamSet s' -> (s', True)
            res <- loebM (alignWithKey (assemble isVariadic) args s)
            maybe (pure res) (selfInject res) m

        x -> throwError $ "Expected set in function call, received: "
                ++ show (() <$ x)

    selfInject :: ValueSet m -> Text -> m (ValueSet m)
    selfInject res n = do
        ref <- valueRef $ NVSet res
        return $ Map.insert n ref res

    assemble :: Bool
             -> Text
             -> These (NThunk m) (Maybe (m (NThunk m)))
             -> ValueSet m
             -> m (NThunk m)
    assemble isVariadic k = \case
        That Nothing  ->
            const $ throwError $ "Missing value for parameter: " ++ show k
        That (Just f) -> \args -> do
            scope <- currentScope
            traceM $ "Deferring default argument in scope: " ++ show scope
            buildThunk $ clearScopes $ do
                traceM $ "Evaluating default argument with args: "
                    ++ show (newScope args)
                pushScopes scope $ pushScope args $ forceThunk =<< f
        This x | isVariadic -> const (pure x)
               | otherwise  ->
                 const $ throwError $ "Unexpected parameter: " ++ show k
        These x _ -> const (pure x)

attrSetAlter :: MonadNix m
             => [Text]
             -> Map.Map Text (m (NValue m))
             -> m (NValue m)
             -> m (Map.Map Text (m (NValue m)))
attrSetAlter [] _ _ = throwError "invalid selector with no components"
attrSetAlter (p:ps) m val = case Map.lookup p m of
    Nothing | null ps   -> go
            | otherwise -> recurse Map.empty
    Just v  | null ps   -> go
            | otherwise -> v >>= \case
                  NVSet s -> recurse (fmap forceThunk s)
                  --TODO: Keep a stack of attributes we've already traversed, so
                  --that we can report that to the user
                  x -> throwError $ "attribute " ++ show p
                    ++ " is not a set; its value is " ++ show (void x)
  where
    go = return $ Map.insert p val m

    recurse s = attrSetAlter ps s val >>= \m' ->
        if | Map.null m' -> return m
           | otherwise   -> do
             scope <- currentScope
             return $ Map.insert p (embed scope m') m
      where
        embed scope m' = NVSet <$> traverse (deferInScope scope) m'

evalBinds :: forall m. MonadNix m
          => Bool
          -> Bool
          -> [Binding (m (NValue m))]
          -> m (ValueSet m)
evalBinds allowDynamic recursive = buildResult . concat <=< mapM go
  where
    go :: Binding (m (NValue m)) -> m [([Text], m (NValue m))]
    go (NamedVar x y) =
        sequence [liftM2 (,) (evalSelector allowDynamic x) (pure y)]
    go (Inherit ms names) = forM names $ \name -> do
        key <- evalKeyName allowDynamic name
        return ([key], do
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
                Just v -> return v)

    buildResult :: [([Text], m (NValue m))] -> m (ValueSet m)
    buildResult bindings = do
        s <- foldM insert Map.empty bindings
        scope <- currentScope
        if recursive
            then loebM (encapsulate scope <$> s)
            else traverse (deferInScope scope) s

    encapsulate scope f attrs =
        buildThunk . clearScopes . pushScopes scope . pushScope attrs $ f

    insert m (path, value) = attrSetAlter path m value

evalString :: MonadNix m => NString (m (NValue m)) -> m (NValue m)
evalString nstr = do
    let fromParts parts = do
          (t, c) <- mconcat <$> mapM go parts
          return $ NVStr t c
    case nstr of
      Indented     parts -> fromParts parts
      DoubleQuoted parts -> fromParts parts
  where
    go = runAntiquoted (return . (, mempty)) (valueText <=< (normalForm =<<))

evalSelector :: MonadNix m => Bool -> NAttrPath (m (NValue m)) -> m [Text]
evalSelector = mapM . evalKeyName

evalKeyName :: MonadNix m => Bool -> NKeyName (m (NValue m)) -> m Text
evalKeyName _ (StaticKey k) = return k
evalKeyName dyn (DynamicKey k)
    | dyn = do
          v <- runAntiquoted evalString id k
          valueTextNoContext =<< normalForm v
    | otherwise =
      throwError "dynamic attribute not allowed in this context"

contextualExprEval :: forall m. MonadNix m => NExprLoc -> m (NValue m)
contextualExprEval =
    adi (eval . annotated . getCompose) psi
  where
    psi k v@(Fix x) = withExprContext (() <$ x) (k v)

tracingExprEval :: MonadNix m => NExprLoc -> IO (m (NValue m))
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

exprNormalForm :: MonadNix m => NExpr -> m (NValueNF m)
exprNormalForm = normalForm <=< evalExpr

normalForm :: MonadNix m => NValue m -> m (NValueNF m)
normalForm = \case
    NVConstant a     -> return $ Fix $ NVConstant a
    NVStr t s        -> return $ Fix $ NVStr t s
    NVList l         -> Fix . NVList <$> traverse (normalForm <=< forceThunk) l
    NVSet s          -> Fix . NVSet <$> traverse (normalForm <=< forceThunk) s
    NVFunction p f   -> do
        p' <- traverse (fmap (normalForm <=< forceThunk)) p
        return $ Fix $ NVFunction p' (normalForm =<< forceThunk =<< f)
    NVLiteralPath fp -> return $ Fix $ NVLiteralPath fp
    NVEnvPath p      -> return $ Fix $ NVEnvPath p
    NVBuiltin name f -> return $ Fix $ NVBuiltin name f
