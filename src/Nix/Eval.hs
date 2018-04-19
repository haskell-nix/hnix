{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Eval where

import           Control.Applicative
import           Control.Arrow (first, second)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Data.Align.Key
import           Data.Fix
import           Data.Functor.Compose
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.IORef
import           Data.List (intercalate, partition, foldl')
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.These
import           Data.Traversable (for)
import           Data.Void
import           Nix.Atoms
import           Nix.Convert
import           Nix.Expr
import           Nix.Pretty
import           Nix.Scope
import           Nix.Stack
import           Nix.StringOperations (runAntiquoted)
import           Nix.Thunk
import           Nix.Utils
-- import           System.IO.Unsafe -- move this into a tracing module
import           Text.Megaparsec.Pos

class (Show v, Monad m) => MonadEval v m | v -> m where
    freeVariable :: Text -> m v

    evalCurPos      :: m v
    evalConstant    :: NAtom -> m v
    evalString      :: Text -> DList Text -> m v
    evalLiteralPath :: FilePath -> m v
    evalEnvPath     :: FilePath -> m v
    evalUnary       :: NUnaryOp -> v -> m v
    evalBinary      :: NBinaryOp -> v -> m v -> m v
    -- ^ The second argument is an action because operators such as boolean &&
    -- and || may not evaluate the second argument.
    evalWith        :: m v -> m v -> m v
    evalIf          :: v -> m v -> m v -> m v
    evalAssert      :: v -> m v -> m v
    evalApp         :: v -> m v -> m v
    evalAbs         :: Params Void -> (m v -> m v) -> m v

    evalError :: String -> m a

type MonadNixEval e v t m =
    (MonadEval v m, Scoped e t m, MonadThunk v t m, MonadFix m,
     Framed e m, MonadFile m, MonadVar m,
     ToValue Bool m v, ToValue [t] m v,
     FromValue (Text, DList Text) m v,
     ToValue (AttrSet t) m v, FromValue (AttrSet t) m v,
     ToValue (AttrSet t, AttrSet SourcePos) m v,
     FromValue (AttrSet t, AttrSet SourcePos) m v)

wrapExpr :: NExprF (m v) -> NExpr
wrapExpr x = Fix (Fix (NSym "<?>") <$ x)

exprFContext :: (Framed e m) => NExprF (m v) -> m r -> m r
exprFContext e = withStringContext $
    "While forcing thunk for: " ++ show (prettyNix (wrapExpr e)) ++ "\n"

-- | Evaluate an nix expression, with a given NThunkSet as environment
evalExpr :: MonadNixEval e v t m => NExpr -> m v
evalExpr = cata eval

eval :: forall e v t m. MonadNixEval e v t m => NExprF (m v) -> m v

eval (NSym "__curPos") = evalCurPos

eval (NSym var) = lookupVar var >>= \case
    Nothing -> freeVariable var
    Just v  -> force v pure

eval (NConstant x)          = evalConstant x
eval (NStr str)             = uncurry evalString =<< assembleString str
eval (NLiteralPath p)       = evalLiteralPath p
eval (NEnvPath p)           = evalEnvPath p
eval (NUnary op arg)        = evalUnary op =<< arg

eval (NBinary NApp fun arg) = do
    traceM "NApp"
    scope <- currentScopes @_ @t
    evalApp ?? withScopes scope arg =<< fun

eval (NBinary op larg rarg) = larg >>= \lval -> evalBinary op lval rarg

eval (NSelect aset attr alt) = do
    traceM "NSelect"
    mres <- evalSelect aset attr
    traceM "NSelect..2"
    case mres of
        Right v -> pure v
        Left (s, ks) -> fromMaybe err alt
          where
            err = evalError @v $ "Could not look up attribute "
                ++ intercalate "." (map Text.unpack (NE.toList ks))
                ++ " in " ++ show @v s

eval (NHasAttr aset attr) = do
    traceM "NHasAttr"
    toValue . either (const False) (const True) =<< evalSelect aset attr

eval e@(NList l) = do
    traceM "NList"
    scope <- currentScopes
    toValue =<< for l (thunk . exprFContext e . withScopes @t scope)

eval e@(NSet binds) = do
    traceM "NSet..1"
    (s, p) <- evalBinds e True False binds
    traceM $ "NSet..2: s = " ++ show (void s)
    traceM $ "NSet..2: p = " ++ show (void p)
    toValue (s, p)

eval e@(NRecSet binds) = do
    traceM "NRecSet..1"
    (s, p) <- evalBinds e True True (desugarBinds (eval . NRecSet) binds)
    traceM $ "NRecSet..2: s = " ++ show (void s)
    traceM $ "NRecSet..2: p = " ++ show (void p)
    toValue (s, p)

eval e@(NLet binds body) = do
    traceM "Let..1"
    (s, _) <- evalBinds e True True (NE.toList binds)
    traceM $ "Let..2: s = " ++ show (void s)
    pushScope s body

eval (NIf cond t f) = cond >>= \v -> evalIf v t f

eval (NWith scope body) = evalWith scope body

eval (NAssert cond body) = cond >>= \v -> evalAssert v body

eval e@(NAbs params body) = do
    -- It is the environment at the definition site, not the call site, that
    -- needs to be used when evaluating the body and default arguments, hence
    -- we defer here so the present scope is restored when the parameters and
    -- body are forced during application.
    traceM "NAbs"
    scope <- currentScopes @_ @t
    evalAbs (clearDefaults params) $ \arg ->
        -- jww (2018-04-17): We need to use the bound library here, so that
        -- the body is only evaluated once.
        withScopes @t scope $ do
            args <- buildArgument e params arg
            pushScope args body
  where
    clearDefaults :: Params r -> Params Void
    clearDefaults (Param name) = Param name
    clearDefaults (ParamSet xs b mv) = ParamSet (map (Nothing <$) xs) b mv

-- | If you know that the 'scope' action will result in an 'AttrSet t', then
--   this implementation may be used as an implementation for 'evalWith'.
evalWithAttrSet :: forall e v t m. MonadNixEval e v t m => m v -> m v -> m v
evalWithAttrSet scope body = do
    -- The scope is deliberately wrapped in a thunk here, since it is
    -- evaluated each time a name is looked up within the weak scope, and
    -- we want to be sure the action it evaluates is to force a thunk, so
    -- its value is only computed once.
    traceM "Evaluating with scope"
    cur <- currentScopes @_ @t
    s <- thunk $ exprFContext (NWith scope body)
              $ withScopes cur scope
    pushWeakScope ?? body $ force s $ fromValue @(AttrSet t)

attrSetAlter :: forall e v t m. MonadNixEval e v t m
             => [Text]
             -> AttrSet (m v)
             -> m v
             -> m (AttrSet (m v))
attrSetAlter [] _ _ = evalError @v "invalid selector with no components"
attrSetAlter (p:ps) m val = case M.lookup p m of
    Nothing
        | null ps   -> go
        | otherwise -> recurse M.empty
    Just x
        | null ps   -> go
        | otherwise ->
          x >>= fromValue >>= \s -> recurse (force ?? pure <$> s)
  where
    go = return $ M.insert p val m

    -- jww (2018-04-13): Need to record positions for attr paths as well
    recurse s = attrSetAlter ps s val <&> \m' ->
        M.insert p (toValue =<< fmap (value @_ @_ @m) <$> sequence m') m

desugarBinds :: forall r. ([Binding r] -> r) -> [Binding r] -> [Binding r]
desugarBinds embed binds = evalState (mapM (go <=< collect) binds) M.empty
  where
    collect :: Binding r
            -> State (HashMap VarName (Maybe SourcePos, [Binding r]))
                     (Either VarName (Binding r))
    collect (NamedVar (StaticKey x p:|y:ys) val) = do
        m <- get
        let v = case M.lookup x m of
                Nothing     -> (p, [NamedVar (y:|ys) val])
                Just (p, v) -> (p, NamedVar (y:|ys) val : v)
        put $ M.insert x v m
        pure $ Left x
    collect x = pure $ Right x

    go :: Either VarName (Binding r)
       -> State (HashMap VarName (Maybe SourcePos, [Binding r]))
                (Binding r)
    go (Right x) = pure x
    go (Left x) = do
        Just (p, v) <- gets $ M.lookup x
        pure $ NamedVar (StaticKey x p :| []) (embed v)

evalBinds :: forall e v t m. MonadNixEval e v t m
          => NExprF (m v)
          -> Bool
          -> Bool
          -> [Binding (m v)]
          -> m (AttrSet t, AttrSet SourcePos)
evalBinds e allowDynamic recursive binds = do
    scope <- currentScopes @_ @t
    buildResult scope . concat =<< mapM (go scope) (moveOverridesLast binds)
  where
    moveOverridesLast = (\(x, y) -> y ++ x) .
        partition (\case NamedVar (StaticKey "__overrides" _ :| []) _ -> True
                         _ -> False)

    go :: Scopes m t -> Binding (m v) -> m [([Text], Maybe SourcePos, m v)]
    go _ (NamedVar (StaticKey "__overrides" _ :| []) finalValue) =
        finalValue >>= fromValue >>= \(o', p') ->
            return $ map (\(k, v) -> ([k], M.lookup k p', force v pure))
                         (M.toList o')

    go _ (NamedVar pathExpr finalValue) = do
        let go :: NAttrPath (m v) -> m ([Text], Maybe SourcePos, m v)
            go = \case
                h :| t -> evalSetterKeyName allowDynamic h >>= \case
                    (Nothing, _) ->
                        pure ([], Nothing,
                              toValue (mempty :: AttrSet t))
                    (Just k, pos) -> case t of
                        [] -> pure ([k], pos, finalValue)
                        x:xs -> do
                            (restOfPath, _, v) <- go (x:|xs)
                            pure (k : restOfPath, pos, v)
        go pathExpr <&> \case
            -- When there are no path segments, e.g. `${null} = 5;`, we don't
            -- bind anything
            ([], _, _) -> []
            result -> [result]

    go scope (Inherit ms names) = fmap catMaybes $ forM names $ \name ->
        evalSetterKeyName allowDynamic name >>= \case
            (Nothing, _) -> return Nothing
            (Just key, pos) -> return $ Just ([key], pos, do
                mv <- case ms of
                    Nothing -> withScopes scope $ lookupVar key
                    Just s -> s >>= fromValue @(AttrSet t) >>= \s ->
                        clearScopes @t $ pushScope s $ lookupVar key
                case mv of
                    Nothing -> evalError @v $ "Inheriting unknown attribute: "
                        ++ show (void name)
                    Just v -> force v pure)

    buildResult :: Scopes m t
                -> [([Text], Maybe SourcePos, m v)]
                -> m (AttrSet t, AttrSet SourcePos)
    buildResult scope bindings = do
        s <- foldM insert M.empty bindings
        res <- if recursive
               then loebM (encapsulate <$> s)
               else traverse (thunk . exprFContext e . withScopes scope) s
        return (res, foldl' go M.empty bindings)
      where
        -- jww (2018-04-13): Need to record positions for attr paths as well
        go m ([k], Just pos, _) = M.insert k pos m
        go m _ = m

        encapsulate f attrs =
            thunk . exprFContext e
                  . withScopes scope
                  . pushScope attrs $ f

        insert m (path, _, value) = attrSetAlter path m value

evalSelect :: forall e v t m. MonadNixEval e v t m
           => m v
           -> NAttrPath (m v)
           -> m (Either (v, NonEmpty Text) v)
evalSelect aset attr = do
    traceM "evalSelect"
    s <- aset
    traceM "evalSelect..2"
    path <- evalSelector True attr
    traceM $ "evalSelect..3: " ++ show path
    res <- extract s path
    traceM "evalSelect..4"
    return res
  where
    extract x path@(k:|ks) = fromValueMay x >>= \case
        Just (s :: AttrSet t, p :: AttrSet SourcePos) ->
            case M.lookup k s of
                Just v -> do
                    traceM $ "Forcing value at selector " ++ Text.unpack k
                    force v $ case ks of
                        []   -> pure . Right
                        y:ys -> extract ?? (y:|ys)
                Nothing ->
                    Left . (, path) <$> toValue (s, p)
        Nothing ->
            return $ Left (x, path)

evalSelector :: (MonadEval v m, FromValue (Text, DList Text) m v)
             => Bool -> NAttrPath (m v) -> m (NonEmpty Text)
evalSelector allowDynamic binds =
    NE.map fst <$> traverse (evalGetterKeyName allowDynamic) binds

-- | Evaluate a component of an attribute path in a context where we are
-- *retrieving* a value
evalGetterKeyName :: (MonadEval v m, FromValue (Text, DList Text) m v)
                  => Bool -> NKeyName (m v) -> m (Text, Maybe SourcePos)
evalGetterKeyName canBeDynamic
    | canBeDynamic = evalKeyNameDynamicNotNull
    | otherwise    = evalKeyNameStatic

evalKeyNameStatic :: forall v m. MonadEval v m
                  => NKeyName (m v) -> m (Text, Maybe SourcePos)
evalKeyNameStatic = \case
    StaticKey k p -> pure (k, p)
    DynamicKey _ ->
        evalError @v "dynamic attribute not allowed in this context"

evalKeyNameDynamicNotNull
    :: forall v m. (MonadEval v m, FromValue (Text, DList Text) m v)
    => NKeyName (m v) -> m (Text, Maybe SourcePos)
evalKeyNameDynamicNotNull = evalKeyNameDynamicNullable >=> \case
    (Nothing, _) ->
        evalError @v "value is null while a string was expected"
    (Just k, p) -> pure (k, p)

-- | Evaluate a component of an attribute path in a context where we are
-- *binding* a value
evalSetterKeyName :: (MonadEval v m, FromValue (Text, DList Text) m v)
                  => Bool -> NKeyName (m v) -> m (Maybe Text, Maybe SourcePos)
evalSetterKeyName canBeDynamic
    | canBeDynamic = evalKeyNameDynamicNullable
    | otherwise    = fmap (first Just) . evalKeyNameStatic

-- | Returns Nothing iff the key value is null
evalKeyNameDynamicNullable
    :: forall v m. (MonadEval v m, FromValue (Text, DList Text) m v)
    => NKeyName (m v)
    -> m (Maybe Text, Maybe SourcePos)
evalKeyNameDynamicNullable = \case
    StaticKey k p -> pure (Just k, p)
    DynamicKey k ->
        runAntiquoted "\n" (fmap Just . assembleString) (>>= fromValueMay) k
            <&> \case Just (t, _) -> (Just t, Nothing)
                      _ -> (Nothing, Nothing)

assembleString :: forall v m. (MonadEval v m, FromValue (Text, DList Text) m v)
               => NString (m v) -> m (Text, DList Text)
assembleString = \case
    Indented _   parts -> fromParts parts
    DoubleQuoted parts -> fromParts parts
  where
    go = runAntiquoted "\n" (pure . (, mempty)) (>>= fromValue)

    fromParts parts = mconcat <$> mapM go parts

buildArgument :: forall e v t m. MonadNixEval e v t m
              => NExprF (m v) -> Params (m v) -> m v -> m (AttrSet t)
buildArgument e params arg = do
    scope <- currentScopes @_ @t
    case params of
        Param name -> M.singleton name
            <$> thunk (exprFContext e (withScopes scope arg))
        ParamSet s isVariadic m ->
            arg >>= fromValue >>= \args -> do
                let inject = case m of
                        Nothing -> id
                        Just n -> M.insert n $ const $
                            thunk (exprFContext e (withScopes scope arg))
                loebM (inject $ alignWithKey (assemble scope isVariadic)
                                             args (M.fromList s))
  where
    assemble :: Scopes m t
             -> Bool
             -> Text
             -> These t (Maybe (m v))
             -> AttrSet t
             -> m t
    assemble scope isVariadic k = \case
        That Nothing  ->
            const $ evalError @v $ "Missing value for parameter: " ++ show k
        That (Just f) -> \args ->
            thunk $ exprFContext e
                  $ withScopes scope
                  $ pushScope args f
        This x | isVariadic -> const (pure x)
               | otherwise  ->
                 const $ evalError @v $ "Unexpected parameter: " ++ show k
        These x _ -> const (pure x)

-----

newtype FlaggedF (f :: * -> *) r = FlaggedF { flagged :: (IORef Bool, f r) }
    deriving (Functor, Foldable, Traversable)

instance Show (f r) => Show (FlaggedF f r) where
    show (FlaggedF (_, x)) = show x

-- instance Show (f r) => Show (FlaggedF f r) where
--     show (FlaggedF (b, x)) =
--         let !used = unsafePerformIO (readIORef b) in
--         if used
--         then show x
--         else "<<" ++ show x ++ ">>"

type Flagged (f :: * -> *) = Fix (FlaggedF f)

flagExprLoc :: MonadIO n => NExprLoc -> n (Flagged NExprLocF)
flagExprLoc = cataM $ \x -> do
    flag <- liftIO $ newIORef False
    pure $ Fix $ FlaggedF (flag, x)

stripFlags :: Flagged NExprLocF -> NExprLoc
stripFlags = cata $ \(FlaggedF (_, x)) -> Fix x

pruneTree :: MonadIO n => Flagged NExprLocF -> n (Maybe NExprLoc)
pruneTree = cataM $ \(FlaggedF (b, Compose x)) -> do
    used <- liftIO $ readIORef b
    pure $ if used
           then Just (Fix (Compose (fmap prune x)))
           else Nothing
  where
    prune :: NExprF (Maybe NExprLoc) -> NExprF NExprLoc
    prune = \case
        NStr str                  -> NStr (pruneString str)
        NHasAttr (Just aset) attr -> NHasAttr aset (NE.map pruneKeyName attr)
        NList l                   -> NList (catMaybes l)
        NSet binds                -> NSet (mapMaybe pruneBinding binds)
        NRecSet binds             -> NRecSet (mapMaybe pruneBinding binds)
        NAbs params (Just body)   -> NAbs (pruneParams params) body

        NLet binds (Just body@(Fix (Compose (Ann _ x)))) ->
            case mapMaybe pruneBinding (NE.toList binds) of
                [] -> x
                b:bs -> NLet (b:|bs) body

        NSelect (Just aset) attr alt ->
            NSelect aset (NE.map pruneKeyName attr) (join alt)

        -- These are the only short-circuiting binary operators
        NBinary NAnd (Just (Fix (Compose (Ann _ larg)))) Nothing -> larg
        NBinary NOr  (Just (Fix (Compose (Ann _ larg)))) Nothing -> larg

        -- If the scope of a with was never referenced, it's not needed
        NWith Nothing (Just (Fix (Compose (Ann _ body)))) -> body

        NAssert Nothing _ ->
            error "How can an assert be used, but its condition not?"

        NAssert _ (Just (Fix (Compose (Ann _ body)))) -> body

        NIf Nothing _ _ ->
            error "How can an if be used, but its condition not?"

        NIf _ Nothing (Just (Fix (Compose (Ann _ f)))) -> f
        NIf _ (Just (Fix (Compose (Ann _ t)))) Nothing -> t

        x -> fromMaybe nNull <$> x

    pruneString :: NString (Maybe NExprLoc) -> NString NExprLoc
    pruneString (DoubleQuoted xs) =
        DoubleQuoted (mapMaybe pruneAntiquotedText xs)
    pruneString (Indented n xs)   =
        Indented n (mapMaybe pruneAntiquotedText xs)

    pruneAntiquotedText
        :: Antiquoted Text (Maybe NExprLoc)
        -> Maybe (Antiquoted Text NExprLoc)
    pruneAntiquotedText (Plain v)             = Just (Plain v)
    pruneAntiquotedText EscapedNewline        = Just EscapedNewline
    pruneAntiquotedText (Antiquoted Nothing)  = Nothing
    pruneAntiquotedText (Antiquoted (Just k)) = Just (Antiquoted k)

    pruneAntiquoted
        :: Antiquoted (NString (Maybe NExprLoc)) (Maybe NExprLoc)
        -> Maybe (Antiquoted (NString NExprLoc) NExprLoc)
    pruneAntiquoted (Plain v)             = Just (Plain (pruneString v))
    pruneAntiquoted EscapedNewline        = Just EscapedNewline
    pruneAntiquoted (Antiquoted Nothing)  = Nothing
    pruneAntiquoted (Antiquoted (Just k)) = Just (Antiquoted k)

    pruneKeyName :: NKeyName (Maybe NExprLoc) -> NKeyName NExprLoc
    pruneKeyName (StaticKey n p) = StaticKey n p
    pruneKeyName (DynamicKey k)
        | Just k' <- pruneAntiquoted k = DynamicKey k'
        | otherwise = StaticKey "unused" Nothing

    pruneParams :: Params (Maybe NExprLoc) -> Params NExprLoc
    pruneParams (Param n) = Param n
    pruneParams (ParamSet xs b n) = ParamSet (map (second join) xs) b n

    pruneBinding :: Binding (Maybe NExprLoc) -> Maybe (Binding NExprLoc)
    pruneBinding (NamedVar _ Nothing)  = Nothing
    pruneBinding (NamedVar xs (Just x)) =
        Just (NamedVar (NE.map pruneKeyName xs) x)
    pruneBinding (Inherit _ [])  = Nothing
    pruneBinding (Inherit m xs)  =
        Just (Inherit (join m) (map pruneKeyName xs))

nNull :: Fix (Compose (Ann SrcSpan) NExprF)
nNull = Fix (Compose (Ann (SrcSpan nullPos nullPos) (NConstant NNull)))
  where
    nullPos = SourcePos "<unknown>" (mkPos 0) (mkPos 0)

tracingEvalExpr :: (Framed e m, MonadIO m,
                   MonadCatch n, MonadIO n, Alternative n)
                => (NExprF (m v) -> m v) -> NExprLoc -> n (m (NExprLoc, v))
tracingEvalExpr eval expr = do
    expr' <- flagExprLoc expr
    res <- flip catch handle $ flip runReaderT (0 :: Int) $
        adiM (pure <$> eval . annotated . getCompose . snd . flagged)
             psi expr'
    return $ do
        v <- res
        expr'' <- pruneTree expr'
        return (fromMaybe nNull expr'', v)
  where
    handle err = error $ "Error during evaluation: "
        ++ show (err :: SomeException)

    psi k v@(Fix (FlaggedF (b, x))) = do
        depth <- ask
        guard (depth < 200)
        local succ $ do
            action <- k v
            -- action <- k =<< case x of
            --     Compose (Ann appAnn
            --              (NBinary NApp
            --               (Fix (FlaggedF
            --                     (impBool,
            --                      Compose (Ann impAnn
            --                               (NSym "import")))))
            --                  appArg)) -> do
            --         pure $ Fix (FlaggedF (b, error "import detected"))
            --     _ -> pure v
            return $ withExprContext (stripFlags v) $ do
                traceM $ "eval: " ++ replicate (depth * 2) ' '
                    ++ show (stripAnnotation (stripFlags v))
                liftIO $ writeIORef b True
                res <- action
                traceM $ "eval: " ++ replicate (depth * 2) ' ' ++ "."
                return res

framedEvalExpr :: Framed e m => (NExprF (m v) -> m v) -> NExprLoc -> m v
framedEvalExpr eval = adi (eval . annotated . getCompose) psi
  where
    psi k v = withExprContext v (k v)

-----

{-
streamValues :: MonadVar m => v -> Stream (EValueF m) m ()
streamValues = void . yields . fmap go
  where
    go (EThunk (Left v)) = streamValues v
    go (EThunk v) = effect (streamValues <$> forceThunk v)
-}
