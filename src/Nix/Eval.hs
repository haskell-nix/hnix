{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Eval where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Align.Key (alignWithKey)
import           Data.Either (isRight)
import           Data.Fix (Fix(Fix))
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.List (partition)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Text (Text)
import           Data.These (These(..))
import           Data.Traversable (for)
import           Nix.Atoms
import           Nix.Convert
import           Nix.Expr
import           Nix.Frames
import           Nix.String
import           Nix.Scope
import           Nix.Strings (runAntiquoted)
import           Nix.Thunk
import           Nix.Utils

class (Show v, Monad m) => MonadEval v m where
    freeVariable    :: Text -> m v
    synHole         :: Text -> m v
    attrMissing     :: NonEmpty Text -> Maybe v -> m v
    evaledSym       :: Text -> v -> m v
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

type MonadNixEval v t m =
    (MonadEval v m,
     Scoped t m,
     MonadThunk v t m,
     MonadFix m,
     ToValue Bool m v,
     ToValue [t] m v,
     FromValue NixString m v,
     ToValue (AttrSet t, AttrSet SourcePos) m v,
     FromValue (AttrSet t, AttrSet SourcePos) m v)

data EvalFrame m v
    = EvaluatingExpr (Scopes m v) NExprLoc
    | ForcingExpr (Scopes m v) NExprLoc
    | Calling String SrcSpan
    | SynHole (SynHoleInfo m v)
    deriving (Show, Typeable)

instance (Typeable m, Typeable v) => Exception (EvalFrame m v)

data SynHoleInfo m t = SynHoleInfo
   { _synHoleInfo_expr :: NExprLoc
   , _synHoleInfo_scope :: Scopes m t
   } deriving (Show, Typeable)

instance (Typeable m, Typeable t) => Exception (SynHoleInfo m t)

eval :: forall v t m. MonadNixEval v t m => NExprF (m v) -> m v

eval (NSym "__curPos") = evalCurPos

eval (NSym var) =
    (lookupVar var :: m (Maybe t)) >>= maybe (freeVariable var) (force ?? evaledSym var)

eval (NConstant x)    = evalConstant x
eval (NStr str)       = evalString str
eval (NLiteralPath p) = evalLiteralPath p
eval (NEnvPath p)     = evalEnvPath p
eval (NUnary op arg)  = evalUnary op =<< arg

eval (NBinary NApp fun arg) = do
    scope <- currentScopes :: m (Scopes m t)
    fun >>= (`evalApp` withScopes scope arg)

eval (NBinary op larg rarg) = larg >>= evalBinary op ?? rarg

eval (NSelect aset attr alt) = evalSelect aset attr >>= either go id
  where
    go (s, ks) = fromMaybe (attrMissing ks (Just s)) alt

eval (NHasAttr aset attr) = evalSelect aset attr >>= toValue . isRight

eval (NList l) = do
    scope <- currentScopes
    for l (thunk . withScopes @t scope) >>= toValue

eval (NSet binds) =
    evalBinds False (desugarBinds (eval . NSet) binds) >>= toValue

eval (NRecSet binds) =
    evalBinds True (desugarBinds (eval . NSet) binds) >>= toValue

eval (NLet binds body) = evalBinds True binds >>= (pushScope ?? body) . fst

eval (NIf cond t f) = cond >>= \v -> evalIf v t f

eval (NWith scope body) = evalWith scope body

eval (NAssert cond body) = cond >>= evalAssert ?? body

eval (NAbs params body) = do
    -- It is the environment at the definition site, not the call site, that
    -- needs to be used when evaluating the body and default arguments, hence
    -- we defer here so the present scope is restored when the parameters and
    -- body are forced during application.
    scope <- currentScopes :: m (Scopes m t)
    evalAbs params $ \arg k -> withScopes scope $ do
        args <- buildArgument params arg
        pushScope args (k (M.map (`force` pure) args) body)

eval (NSynHole name) = synHole name

-- | If you know that the 'scope' action will result in an 'AttrSet t', then
--   this implementation may be used as an implementation for 'evalWith'.
evalWithAttrSet :: forall v t m. MonadNixEval v t m => m v -> m v -> m v
evalWithAttrSet aset body = do
    -- The scope is deliberately wrapped in a thunk here, since it is
    -- evaluated each time a name is looked up within the weak scope, and
    -- we want to be sure the action it evaluates is to force a thunk, so
    -- its value is only computed once.
    scope <- currentScopes :: m (Scopes m t)
    s <- thunk $ withScopes scope aset
    pushWeakScope ?? body $ force s $
        fmap fst . fromValue @(AttrSet t, AttrSet SourcePos)

attrSetAlter :: forall v t m. MonadNixEval v t m
             => [Text]
             -> SourcePos
             -> AttrSet (m v)
             -> AttrSet SourcePos
             -> m v
             -> m (AttrSet (m v), AttrSet SourcePos)
attrSetAlter [] _ _ _ _ =
    evalError @v $ ErrorCall "invalid selector with no components"

attrSetAlter (k:ks) pos m p val = case M.lookup k m of
    Nothing | null ks   -> go
            | otherwise -> recurse M.empty M.empty
    Just x  | null ks   -> go
            | otherwise ->
                x >>= fromValue @(AttrSet t, AttrSet SourcePos)
                  >>= \(st, sp) -> recurse (force ?? pure <$> st) sp
  where
    go = return (M.insert k val m, M.insert k pos p)

    recurse st sp = attrSetAlter ks pos st sp val <&> \(st', _) ->
        ( M.insert k (toValue @(AttrSet t, AttrSet SourcePos)
                         =<< (, mempty) . fmap value <$> sequence st') st
        , M.insert k pos sp )

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
            Just (q, v) -> (q, NamedVar (y:|ys) val q : v)
        pure $ Left x
    collect x = pure $ Right x

    go :: Either VarName (Binding r)
       -> State (HashMap VarName (SourcePos, [Binding r]))
                (Binding r)
    go (Right x) = pure x
    go (Left x) = do
        maybeValue <- gets (M.lookup x)
        case maybeValue of
          Nothing ->
            fail ("No binding " ++ show x)
          Just (p, v) ->
            pure $ NamedVar (StaticKey x :| []) (embed v) p

evalBinds :: forall v t m. MonadNixEval v t m
          => Bool
          -> [Binding (m v)]
          -> m (AttrSet t, AttrSet SourcePos)
evalBinds recursive binds = do
    scope <- currentScopes :: m (Scopes m t)
    buildResult scope . concat =<< mapM (go scope) (moveOverridesLast binds)
  where
    moveOverridesLast = uncurry (++) .
        partition (\case
            NamedVar (StaticKey "__overrides" :| []) _ _pos -> False
            _ -> True)

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
                h :| t -> evalSetterKeyName h >>= \case
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
        evalSetterKeyName >=> \case
            Nothing  -> pure Nothing
            Just key -> pure $ Just ([key], pos, do
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
        (s, p) <- foldM insert (M.empty, M.empty) bindings
        res <- if recursive
               then loebM (encapsulate <$> s)
               else traverse mkThunk s
        return (res, p)
      where
        mkThunk = thunk . withScopes scope

        encapsulate f attrs = mkThunk . pushScope attrs $ f

        insert (m, p) (path, pos, value) = attrSetAlter path pos m p value

evalSelect :: forall v t m. MonadNixEval v t m
           => m v
           -> NAttrPath (m v)
           -> m (Either (v, NonEmpty Text) (m v))
evalSelect aset attr = do
    s <- aset
    path <- traverse evalGetterKeyName attr
    extract s path
  where
    extract x path@(k:|ks) = fromValueMay x >>= \case
        Just (s :: AttrSet t, p :: AttrSet SourcePos)
            | Just t <- M.lookup k s -> case ks of
                []   -> pure $ Right $ force t pure
                y:ys -> force t $ extract ?? (y:|ys)
            | otherwise -> Left . (, path) <$> toValue (s, p)
        Nothing -> return $ Left (x, path)

-- | Evaluate a component of an attribute path in a context where we are
-- *retrieving* a value
evalGetterKeyName :: forall v m. (MonadEval v m, FromValue NixString m v)
                  => NKeyName (m v) -> m Text
evalGetterKeyName = evalSetterKeyName >=> \case
    Just k -> pure k
    Nothing -> evalError @v $ ErrorCall "value is null while a string was expected"

-- | Evaluate a component of an attribute path in a context where we are
-- *binding* a value
evalSetterKeyName :: (MonadEval v m, FromValue NixString m v)
                  => NKeyName (m v) -> m (Maybe Text)
evalSetterKeyName = \case
    StaticKey  k -> pure (Just k)
    DynamicKey k ->
        runAntiquoted "\n" assembleString (>>= fromValueMay) k <&>
            \case Just ns -> Just (hackyStringIgnoreContext ns)
                  _ -> Nothing

assembleString :: forall v m. (MonadEval v m, FromValue NixString m v)
               => NString (m v) -> m (Maybe NixString)
assembleString = \case
    Indented _   parts -> fromParts parts
    DoubleQuoted parts -> fromParts parts
  where
    fromParts = fmap (fmap principledStringMConcat . sequence) . traverse go

    go = runAntiquoted "\n" (pure . Just . principledMakeNixStringWithoutContext) (>>= fromValueMay)

buildArgument :: forall v t m. MonadNixEval v t m
              => Params (m v) -> m v -> m (AttrSet t)
buildArgument params arg = do
    scope <- currentScopes :: m (Scopes m t)
    case params of
        Param name -> M.singleton name <$> thunk (withScopes scope arg)
        ParamSet s isVariadic m ->
            arg >>= fromValue @(AttrSet t, AttrSet SourcePos)
                >>= \(args, _) -> do
                let inject = case m of
                        Nothing -> id
                        Just n -> M.insert n $ const $
                            thunk (withScopes scope arg)
                loebM (inject $ M.mapMaybe id $ alignWithKey (assemble scope isVariadic)
                                             args (M.fromList s))
  where
    assemble :: Scopes m t
             -> Bool
             -> Text
             -> These t (Maybe (m v))
             -> Maybe (AttrSet t -> m t)
    assemble scope isVariadic k = \case
        That Nothing  -> Just $
            const $ evalError @v $ ErrorCall $
                "Missing value for parameter: " ++ show k
        That (Just f) -> Just $ \args ->
            thunk $ withScopes scope $ pushScope args f
        This _ | isVariadic -> Nothing
               | otherwise  -> Just $
                 const $ evalError @v $ ErrorCall $
                     "Unexpected parameter: " ++ show k
        These x _ -> Just (const (pure x))

addSourcePositions :: (MonadReader e m, Has e SrcSpan)
                   => Transform NExprLocF (m a)
addSourcePositions f v@(Fix (Compose (Ann ann _))) =
    local (set hasLens ann) (f v)

addStackFrames
    :: forall t e m a. (Scoped t m, Framed e m, Typeable t, Typeable m)
    => Transform NExprLocF (m a)
addStackFrames f v = do
    scopes <- currentScopes :: m (Scopes m t)
    withFrame Info (EvaluatingExpr scopes v) (f v)

framedEvalExprLoc
    :: forall t e v m.
      (MonadNixEval v t m, Framed e m, Has e SrcSpan,
       Typeable t, Typeable m)
    => NExprLoc -> m v
framedEvalExprLoc = adi (eval . annotated . getCompose)
                        (addStackFrames @t . addSourcePositions)
