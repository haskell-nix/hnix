{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Align.Key
import           Data.Fix
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.List (partition, foldl')
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.These
import           Data.Traversable (for)
import           Nix.Atoms
import           Nix.Convert
import           Nix.Expr
import           Nix.Frames
import           Nix.Scope
import           Nix.Strings (runAntiquoted)
import           Nix.Thunk
import           Nix.Utils

class (Show v, Monad m) => MonadEval v m | v -> m where
    freeVariable :: Text -> m v
    attrMissing  :: NonEmpty Text -> Maybe v -> m v
    evaledSym    :: Text -> v -> m v

    evalCurPos      :: m v
    evalConstant    :: NAtom -> m v
    evalString      :: NString (m v) -> m v
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
    evalAbs         :: Params (m v)
                    -> (forall a. m v -> (AttrSet (m v) -> m v -> m (a, v)) -> m (a, v))
                    -> m v

{-
    evalSelect     :: v -> NonEmpty Text -> Maybe (m v) -> m v
    evalHasAttr    :: v -> NonEmpty Text -> m v

    -- | This and the following methods are intended to allow things like
    --   adding provenance information.
    evalListElem   :: [m v] -> Int -> m v -> m v
    evalList       :: [t] -> m v
    evalSetElem    :: AttrSet (m v) -> Text -> m v -> m v
    evalSet        :: AttrSet t -> AttrSet SourcePos -> m v
    evalRecSetElem :: AttrSet (m v) -> Text -> m v -> m v
    evalRecSet     :: AttrSet t -> AttrSet SourcePos -> m v
    evalLetElem    :: Text -> m v -> m v
    evalLet        :: m v -> m v
-}

    evalError :: Exception s => s -> m a

type MonadNixEval e v t m =
    (MonadEval v m,
     Scoped e t m,
     MonadThunk v t m,
     MonadFix m,
     ToValue Bool m v,
     ToValue [t] m v,
     FromValue (Text, DList Text) m v,
     ToValue (AttrSet t, AttrSet SourcePos) m v,
     FromValue (AttrSet t, AttrSet SourcePos) m v)

data EvalFrame m v
    = EvaluatingExpr (Scopes m v) NExprLoc
    | ForcingExpr (Scopes m v) NExprLoc
    | Calling String SrcSpan
    deriving (Show, Typeable)

instance (Typeable m, Typeable v) => Exception (EvalFrame m v)

eval :: forall e v t m. MonadNixEval e v t m => NExprF (m v) -> m v

eval (NSym "__curPos") = evalCurPos

eval (NSym var) =
    lookupVar var >>= maybe (freeVariable var) (force ?? evaledSym var)

eval (NConstant x)          = evalConstant x
eval (NStr str)             = evalString str
eval (NLiteralPath p)       = evalLiteralPath p
eval (NEnvPath p)           = evalEnvPath p
eval (NUnary op arg)        = evalUnary op =<< arg

eval (NBinary NApp fun arg) = do
    scope <- currentScopes @_ @t
    evalApp ?? withScopes scope arg =<< fun

eval (NBinary op larg rarg) = larg >>= \lval -> evalBinary op lval rarg

eval (NSelect aset attr alt) = do
    traceM "NSelect"
    mres <- evalSelect aset attr
    traceM "NSelect..2"
    case mres of
        Right v -> v
        Left (s, ks) -> fromMaybe (attrMissing ks (Just s)) alt

eval (NHasAttr aset attr) =
    toValue . either (const False) (const True) =<< evalSelect aset attr

eval (NList l) = do
    scope <- currentScopes
    toValue =<< for l (thunk . withScopes @t scope)

eval (NSet binds) = do
    traceM "NSet..1"
    (s, p) <- evalBinds True False binds
    traceM $ "NSet..2: s = " ++ show (void s)
    traceM $ "NSet..2: p = " ++ show (void p)
    toValue (s, p)

eval (NRecSet binds) = do
    traceM "NRecSet..1"
    (s, p) <- evalBinds True True (desugarBinds (eval . NRecSet) binds)
    traceM $ "NRecSet..2: s = " ++ show (void s)
    traceM $ "NRecSet..2: p = " ++ show (void p)
    toValue (s, p)

eval (NLet binds body) = do
    traceM "Let..1"
    (s, _) <- evalBinds True True binds
    traceM $ "Let..2: s = " ++ show (void s)
    pushScope s body

eval (NIf cond t f) = cond >>= \v -> evalIf v t f

eval (NWith scope body) = evalWith scope body

eval (NAssert cond body) = cond >>= evalAssert ?? body

eval (NAbs params body) = do
    -- It is the environment at the definition site, not the call site, that
    -- needs to be used when evaluating the body and default arguments, hence
    -- we defer here so the present scope is restored when the parameters and
    -- body are forced during application.
    scope <- currentScopes @_ @t
    evalAbs params $ \arg k ->
        withScopes @t scope $ do
            args <- buildArgument params arg
            pushScope args (k (M.map (`force` pure) args) body)

-- | If you know that the 'scope' action will result in an 'AttrSet t', then
--   this implementation may be used as an implementation for 'evalWith'.
evalWithAttrSet :: forall e v t m. MonadNixEval e v t m => m v -> m v -> m v
evalWithAttrSet scope body = do
    -- The scope is deliberately wrapped in a thunk here, since it is
    -- evaluated each time a name is looked up within the weak scope, and
    -- we want to be sure the action it evaluates is to force a thunk, so
    -- its value is only computed once.
    cur <- currentScopes @_ @t
    s <- thunk $ withScopes cur scope
    pushWeakScope ?? body $ force s $
        fmap fst . fromValue @(AttrSet t, AttrSet SourcePos)

attrSetAlter :: forall e v t m. MonadNixEval e v t m
             => [Text]
             -> AttrSet (m v)
             -> m v
             -> m (AttrSet (m v))
attrSetAlter [] _ _ =
    evalError @v $ ErrorCall "invalid selector with no components"
attrSetAlter (p:ps) m val = case M.lookup p m of
    Nothing
        | null ps   -> go
        | otherwise -> recurse M.empty
    Just x
        | null ps   -> go
        | otherwise ->
          x >>= fromValue @(AttrSet t, AttrSet SourcePos)
              >>= \(s, _) -> recurse (force ?? pure <$> s)
  where
    go = return $ M.insert p val m

    recurse s = attrSetAlter ps s val <&> \m' ->
        M.insert p (toValue @(AttrSet t, AttrSet SourcePos)
                        =<< fmap (, mempty)
                                 (fmap (value @_ @_ @m) <$> sequence m')) m

desugarBinds :: forall r. ([Binding r] -> r) -> [Binding r] -> [Binding r]
desugarBinds embed binds = evalState (mapM (go <=< collect) binds) M.empty
  where
    collect :: Binding r
            -> State (HashMap VarName (SourcePos, [Binding r]))
                     (Either VarName (Binding r))
    collect (NamedVar (StaticKey x :| y:ys) val p) = do
        m <- get
        put $ M.insert x ?? m $ case M.lookup x m of
            Nothing     -> (p, [NamedVar (y:|ys) val p])
            Just (p, v) -> (p, NamedVar (y:|ys) val p : v)
        pure $ Left x
    collect x = pure $ Right x

    go :: Either VarName (Binding r)
       -> State (HashMap VarName (SourcePos, [Binding r]))
                (Binding r)
    go (Right x) = pure x
    go (Left x) = do
        Just (p, v) <- gets $ M.lookup x
        pure $ NamedVar (StaticKey x :| []) (embed v) p

evalBinds :: forall e v t m. MonadNixEval e v t m
          => Bool
          -> Bool
          -> [Binding (m v)]
          -> m (AttrSet t, AttrSet SourcePos)
evalBinds allowDynamic recursive binds = do
    scope <- currentScopes @_ @t
    buildResult scope . concat =<< mapM (go scope) (moveOverridesLast binds)
  where
    moveOverridesLast = (\(x, y) -> y ++ x) .
        partition (\case NamedVar (StaticKey "__overrides" :| []) _ _pos -> True
                         _ -> False)

    go :: Scopes m t -> Binding (m v) -> m [([Text], SourcePos, m v)]
    go _ (NamedVar (StaticKey "__overrides" :| []) finalValue pos) =
        finalValue >>= fromValue >>= \(o', p') ->
            -- jww (2018-05-09): What to do with the key position here?
            return $ map (\(k, v) -> ([k], fromMaybe pos (M.lookup k p'),
                                     force v pure))
                         (M.toList o')

    go _ (NamedVar pathExpr finalValue pos) = do
        let go :: NAttrPath (m v) -> m ([Text], SourcePos, m v)
            go = \case
                h :| t -> evalSetterKeyName allowDynamic h >>= \case
                    Nothing ->
                        pure ([], nullPos,
                              toValue @(AttrSet t, AttrSet SourcePos)
                                  (mempty, mempty))
                    Just k -> case t of
                        [] -> pure ([k], pos, finalValue)
                        x:xs -> do
                            (restOfPath, _, v) <- go (x:|xs)
                            pure (k : restOfPath, pos, v)
        go pathExpr <&> \case
            -- When there are no path segments, e.g. `${null} = 5;`, we don't
            -- bind anything
            ([], _, _) -> []
            result -> [result]

    go scope (Inherit ms names pos) = fmap catMaybes $ forM names $
        evalSetterKeyName allowDynamic >=> \case
            Nothing -> return Nothing
            Just key -> return $ Just ([key], pos, do
                mv <- case ms of
                    Nothing -> withScopes scope $ lookupVar key
                    Just s -> s
                        >>= fromValue @(AttrSet t, AttrSet SourcePos)
                        >>= \(s, _) ->
                            clearScopes @t $ pushScope s $ lookupVar key
                case mv of
                    Nothing -> attrMissing (key :| []) Nothing
                    Just v -> force v pure)

    buildResult :: Scopes m t
                -> [([Text], SourcePos, m v)]
                -> m (AttrSet t, AttrSet SourcePos)
    buildResult scope bindings = do
        s <- foldM insert M.empty bindings
        res <- if recursive
               then loebM (encapsulate <$> s)
               else traverse (thunk . withScopes scope) s
        return (res, foldl' go M.empty bindings)
      where
        go m ([k], pos, _) = M.insert k pos m
        go m _ = m

        encapsulate f attrs =
            thunk . withScopes scope . pushScope attrs $ f

        insert m (path, _, value) = attrSetAlter path m value

evalSelect :: forall e v t m. MonadNixEval e v t m
           => m v
           -> NAttrPath (m v)
           -> m (Either (v, NonEmpty Text) (m v))
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
                Just t -> do
                    traceM $ "Forcing value at selector " ++ Text.unpack k
                    case ks of
                        []   -> pure $ Right $ force t pure
                        y:ys -> force t $ extract ?? (y:|ys)
                Nothing ->
                    Left . (, path) <$> toValue (s, p)
        Nothing ->
            return $ Left (x, path)

evalSelector :: (MonadEval v m, FromValue (Text, DList Text) m v)
             => Bool -> NAttrPath (m v) -> m (NonEmpty Text)
evalSelector = traverse . evalGetterKeyName

-- | Evaluate a component of an attribute path in a context where we are
-- *retrieving* a value
evalGetterKeyName :: (MonadEval v m, FromValue (Text, DList Text) m v)
                  => Bool -> NKeyName (m v) -> m Text
evalGetterKeyName canBeDynamic
    | canBeDynamic = evalKeyNameDynamicNotNull
    | otherwise    = evalKeyNameStatic

evalKeyNameStatic :: forall v m. MonadEval v m => NKeyName (m v) -> m Text
evalKeyNameStatic = \case
    StaticKey k -> pure k
    _ -> evalError @v $ ErrorCall "dynamic attribute not allowed in this context"

evalKeyNameDynamicNotNull
    :: forall v m. (MonadEval v m, FromValue (Text, DList Text) m v)
    => NKeyName (m v) -> m Text
evalKeyNameDynamicNotNull = evalKeyNameDynamicNullable >=> \case
    Just k -> pure k
    Nothing -> evalError @v $ ErrorCall "value is null while a string was expected"

-- | Evaluate a component of an attribute path in a context where we are
-- *binding* a value
evalSetterKeyName :: (MonadEval v m, FromValue (Text, DList Text) m v)
                  => Bool -> NKeyName (m v) -> m (Maybe Text)
evalSetterKeyName canBeDynamic
    | canBeDynamic = evalKeyNameDynamicNullable
    | otherwise    = fmap Just . evalKeyNameStatic

-- | Returns Nothing iff the key value is null
evalKeyNameDynamicNullable
    :: forall v m. (MonadEval v m, FromValue (Text, DList Text) m v)
    => NKeyName (m v)
    -> m (Maybe Text)
evalKeyNameDynamicNullable = \case
    StaticKey k -> pure (Just k)
    DynamicKey k ->
        runAntiquoted "\n" assembleString (>>= fromValueMay) k
            <&> \case Just (t, _) -> Just t
                      _ -> Nothing

assembleString :: forall v m. (MonadEval v m, FromValue (Text, DList Text) m v)
               => NString (m v) -> m (Maybe (Text, DList Text))
assembleString = \case
    Indented _   parts -> fromParts parts
    DoubleQuoted parts -> fromParts parts
  where
    go = runAntiquoted "\n" (pure . Just . (, mempty)) (>>= fromValueMay)

    fromParts parts = fmap mconcat . sequence <$> mapM go parts

buildArgument :: forall e v t m. MonadNixEval e v t m
              => Params (m v) -> m v -> m (AttrSet t)
buildArgument params arg = do
    scope <- currentScopes @_ @t
    case params of
        Param name -> M.singleton name
            <$> thunk (withScopes scope arg)
        ParamSet s isVariadic m ->
            arg >>= fromValue @(AttrSet t, AttrSet SourcePos)
                >>= \(args, _) -> do
                let inject = case m of
                        Nothing -> id
                        Just n -> M.insert n $ const $
                            thunk (withScopes scope arg)
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
            const $ evalError @v $ ErrorCall $
                "Missing value for parameter: " ++ show k
        That (Just f) -> \args ->
            thunk $ withScopes scope $ pushScope args f
        This x | isVariadic -> const (pure x)
               | otherwise  ->
                 const $ evalError @v $ ErrorCall $
                     "Unexpected parameter: " ++ show k
        These x _ -> const (pure x)

addSourcePositions :: (MonadReader e m, Has e SrcSpan)
                   => Transform NExprLocF (m a)
addSourcePositions f v@(Fix (Compose (Ann ann _))) =
    local (set hasLens ann) (f v)

addStackFrames
    :: forall t e m a. (Scoped e t m, Framed e m, Typeable t, Typeable m)
    => Transform NExprLocF (m a)
addStackFrames f v = do
    scopes <- currentScopes @e @t
    withFrame Info (EvaluatingExpr scopes v) (f v)

framedEvalExprLoc
    :: forall t e v m.
      (MonadNixEval e v t m, Framed e m, Has e SrcSpan,
       Typeable t, Typeable m)
    => NExprLoc -> m v
framedEvalExprLoc = adi (eval . annotated . getCompose)
                        (addStackFrames @t . addSourcePositions)
