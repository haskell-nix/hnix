{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}



module Nix.Eval where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Semialign.Indexed         ( ialignWith )
import           Data.Either                    ( isRight )
import           Data.Fix                       ( Fix(Fix) )
import           Data.HashMap.Lazy              ( HashMap )
import qualified Data.HashMap.Lazy             as M
import           Data.List                      ( partition )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Maybe                     ( fromMaybe
                                                , catMaybes
                                                )
import           Data.Text                      ( Text )
import           Data.These                     ( These(..) )
import           Data.Traversable               ( for )
import           Nix.Atoms
import           Nix.Convert
import           Nix.Expr
import           Nix.Expr.Strings               ( runAntiquoted )
import           Nix.Frames
import           Nix.String
import           Nix.Scope
import           Nix.Utils
import           Nix.Value.Monad

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
  evalList       :: [v] -> m v
  evalSetElem    :: AttrSet (m v) -> Text -> m v -> m v
  evalSet        :: AttrSet v -> AttrSet SourcePos -> m v
  evalRecSetElem :: AttrSet (m v) -> Text -> m v -> m v
  evalRecSet     :: AttrSet v -> AttrSet SourcePos -> m v
  evalLetElem    :: Text -> m v -> m v
  evalLet        :: m v -> m v
-}
  evalError :: Exception s => s -> m a

type MonadNixEval v m
  = ( MonadEval v m
  , Scoped v m
  , MonadValue v m
  , MonadFix m
  , ToValue Bool m v
  , ToValue [v] m v
  , FromValue NixString m v
  , ToValue (AttrSet v, AttrSet SourcePos) m v
  , FromValue (AttrSet v, AttrSet SourcePos) m v
  )

data EvalFrame m v
    = EvaluatingExpr (Scopes m v) NExprLoc
    | ForcingExpr (Scopes m v) NExprLoc
    | Calling String SrcSpan
    | SynHole (SynHoleInfo m v)
    deriving (Show, Typeable)

instance (Typeable m, Typeable v) => Exception (EvalFrame m v)

data SynHoleInfo m v = SynHoleInfo
   { _synHoleInfo_expr :: NExprLoc
   , _synHoleInfo_scope :: Scopes m v
   } deriving (Show, Typeable)

instance (Typeable m, Typeable v) => Exception (SynHoleInfo m v)

-- jww (2019-03-18): By deferring only those things which must wait until
-- context of us, this can be written as:
-- eval :: forall v m . MonadNixEval v m => NExprF v -> m v
eval :: forall v m . MonadNixEval v m => NExprF (m v) -> m v

eval (NSym "__curPos") = evalCurPos

eval (NSym var       ) = do
  mres <- lookupVar var
  maybe
    (freeVariable var)
    (evaledSym var <=< demand)
    mres

eval (NConstant    x      ) = evalConstant x
eval (NStr         str    ) = evalString str
eval (NLiteralPath p      ) = evalLiteralPath p
eval (NEnvPath     p      ) = evalEnvPath p
eval (NUnary op arg       ) = evalUnary op =<< arg

eval (NBinary NApp fun arg) = do
  scope <- currentScopes :: m (Scopes m v)
  fun >>= (`evalApp` withScopes scope arg)

eval (NBinary op   larg rarg) = larg >>= evalBinary op ?? rarg

eval (NSelect aset attr alt ) = evalSelect aset attr >>= either go id
  where go (s, ks) = fromMaybe (attrMissing ks (pure s)) alt

eval (NHasAttr aset attr) = evalSelect aset attr >>= toValue . isRight

eval (NList l           ) = do
  scope <- currentScopes
  for l (defer @v @m . withScopes @v scope) >>= toValue

eval (NSet NNonRecursive binds) =
  evalBinds False (desugarBinds (eval . NSet NNonRecursive) binds) >>= toValue

eval (NSet NRecursive binds) =
  evalBinds True (desugarBinds (eval . NSet NNonRecursive) binds) >>= toValue

eval (NLet binds body    ) = evalBinds True binds >>= (pushScope ?? body) . fst

eval (NIf cond t f       ) = cond >>= \v -> evalIf v t f

eval (NWith   scope  body) = evalWith scope body

eval (NAssert cond   body) = cond >>= evalAssert ?? body

eval (NAbs    params body) = do
  -- It is the environment at the definition site, not the call site, that
  -- needs to be used when evaluating the body and default arguments, hence we
  -- defer here so the present scope is restored when the parameters and body
  -- are forced during application.
  scope <- currentScopes :: m (Scopes m v)
  evalAbs params $ \arg k -> withScopes scope $ do
    args <- buildArgument params arg
    pushScope args (k (fmap (inform (withScopes scope)) args) body)

eval (NSynHole name) = synHole name

-- | If you know that the 'scope' action will result in an 'AttrSet v', then
--   this implementation may be used as an implementation for 'evalWith'.
evalWithAttrSet :: forall v m . MonadNixEval v m => m v -> m v -> m v
evalWithAttrSet aset body = do
  -- The scope is deliberately wrapped in a thunk here, since it is demanded
  -- each time a name is looked up within the weak scope, and we want to be
  -- sure the action it evaluates is to force a thunk, so its value is only
  -- computed once.
  scope <- currentScopes :: m (Scopes m v)
  s     <- defer $ withScopes scope aset
  let s' = (fmap fst . fromValue @(AttrSet v, AttrSet SourcePos)) =<< demand s
  pushWeakScope s' body

attrSetAlter
  :: forall v m
   . MonadNixEval v m
  => [Text]
  -> SourcePos
  -> AttrSet (m v)
  -> AttrSet SourcePos
  -> m v
  -> m (AttrSet (m v), AttrSet SourcePos)
attrSetAlter [] _ _ _ _ = evalError @v $ ErrorCall "invalid selector with no components"
attrSetAlter (k : ks) pos m p val =
  bool
    go
    (maybe
      (recurse M.empty M.empty)
      (\x ->
        do
          (st, sp) <- fromValue @(AttrSet v, AttrSet SourcePos) =<< x
          recurse ((pure <=< demand) <$> st) sp
      )
      (M.lookup k m)
    )
    (not $ null ks)
 where
  go = pure (M.insert k val m, M.insert k pos p)

  recurse st sp =
    (\(st', _) ->
      (M.insert
        k
        (toValue @(AttrSet v, AttrSet SourcePos) =<< (, mempty) <$> sequence st')
        m
      , M.insert k pos p
      )
    ) <$> attrSetAlter ks pos st sp val

desugarBinds :: forall r . ([Binding r] -> r) -> [Binding r] -> [Binding r]
desugarBinds embed binds = evalState (mapM (go <=< collect) binds) M.empty
 where
  collect
    :: Binding r
    -> State
         (HashMap VarName (SourcePos, [Binding r]))
         (Either VarName (Binding r))
  collect (NamedVar (StaticKey x :| y : ys) val p) = do
    m <- get
    put $ M.insert x ?? m $
      maybe
        (p, [NamedVar (y :| ys) val p])
        (\ (q, v) -> (q, NamedVar (y :| ys) val q : v))
        (M.lookup x m)
    pure $ Left x
  collect x = pure $ pure x

  go
    :: Either VarName (Binding r)
    -> State (HashMap VarName (SourcePos, [Binding r])) (Binding r)
  go =
    either
      (\ x -> do
        maybeValue <- gets (M.lookup x)
        maybe
          (error $ "No binding " <> show x)
          (\ (p, v) -> pure $ NamedVar (StaticKey x :| []) (embed v) p)
          maybeValue
      )
      pure

evalBinds
  :: forall v m
   . MonadNixEval v m
  => Bool
  -> [Binding (m v)]
  -> m (AttrSet v, AttrSet SourcePos)
evalBinds recursive binds = do
  scope <- currentScopes :: m (Scopes m v)
  buildResult scope . concat =<< mapM (go scope) (moveOverridesLast binds)
 where
  moveOverridesLast = uncurry (<>) . partition
    (\case
      NamedVar (StaticKey "__overrides" :| []) _ _pos -> False
      _ -> True
    )

  go :: Scopes m v -> Binding (m v) -> m [([Text], SourcePos, m v)]
  go _ (NamedVar (StaticKey "__overrides" :| []) finalValue pos) =
    finalValue >>= fromValue >>= \(o', p') ->
          -- jww (2018-05-09): What to do with the key position here?
                                              pure $ fmap
      (\(k, v) -> ([k], fromMaybe pos (M.lookup k p'), pure =<< demand v))
      (M.toList o')

  go _ (NamedVar pathExpr finalValue pos) = do
    let
      gogo :: NAttrPath (m v) -> m ([Text], SourcePos, m v)
      gogo = \case
        h :| t -> evalSetterKeyName h >>= \case
          Nothing ->
            pure
              ( mempty
              , nullPos
              , toValue @(AttrSet v, AttrSet SourcePos) (mempty, mempty)
              )
          Just k -> case t of
            []     -> pure ([k], pos, finalValue)
            x : xs -> do
              (restOfPath, _, v) <- gogo (x :| xs)
              pure (k : restOfPath, pos, v)

    gogo pathExpr <&> \case
        -- When there are no path segments, e.g. `${null} = 5;`, we don't
        -- bind anything
      ([], _, _) -> mempty
      result     -> [result]

  go scope (Inherit ms names pos) =
    fmap catMaybes $ forM names $ evalSetterKeyName >=>
      (pure . maybe
        Nothing
        (\ key -> pure
          ([key]
          , pos
          , do
            mv <-
              maybe
                (withScopes scope $ lookupVar key)
                (\ s ->
                    --  2021-02-25: NOTE: This is obviously a do block.
                    -- In the middle of the huge move, can not test refactor compilation.
                  s >>= fromValue @(AttrSet v, AttrSet SourcePos) >>= \(attrset, _) ->
                    clearScopes @v $ pushScope attrset $ lookupVar key)
                ms
            maybe
              (attrMissing (key :| []) Nothing)
              (pure <=< demand)
              mv
          )
        )
      )

  buildResult
    :: Scopes m v
    -> [([Text], SourcePos, m v)]
    -> m (AttrSet v, AttrSet SourcePos)
  buildResult scope bindings = do
    (s, p) <- foldM insert (M.empty, M.empty) bindings
    res <- if recursive then loebM (encapsulate <$> s) else traverse mkThunk s
    pure (res, p)
   where
    mkThunk = defer . withScopes scope

    encapsulate f attrs = mkThunk . pushScope attrs $ f

    insert (m, p) (path, pos, value) = attrSetAlter path pos m p value

evalSelect
  :: forall v m
   . MonadNixEval v m
  => m v
  -> NAttrPath (m v)
  -> m (Either (v, NonEmpty Text) (m v))
evalSelect aset attr = do
  s    <- aset
  path <- traverse evalGetterKeyName attr
  extract s path
 where
  extract x path@(k :| ks) = fromValueMay x >>= \case
    Just (s :: AttrSet v, p :: AttrSet SourcePos)
      | Just t <- M.lookup k s -> case ks of
        []     -> pure $ pure $ pure =<< demand t
        y : ys -> (extract ?? (y :| ys)) =<< demand t
      | otherwise -> Left . (, path) <$> toValue (s, p)
    Nothing -> pure $ Left (x, path)

-- | Evaluate a component of an attribute path in a context where we are
-- *retrieving* a value
evalGetterKeyName
  :: forall v m
   . (MonadEval v m, FromValue NixString m v)
  => NKeyName (m v)
  -> m Text
evalGetterKeyName = evalSetterKeyName >=>
  maybe
    (evalError @v $ ErrorCall "value is null while a string was expected")
    pure

-- | Evaluate a component of an attribute path in a context where we are
-- *binding* a value
evalSetterKeyName
  :: (MonadEval v m, FromValue NixString m v)
  => NKeyName (m v)
  -> m (Maybe Text)
evalSetterKeyName = \case
  StaticKey k -> pure (pure k)
  DynamicKey k ->
    ((pure . stringIgnoreContext) `ifJust`) <$>runAntiquoted "\n" assembleString (fromValueMay =<<) k

assembleString
  :: forall v m
   . (MonadEval v m, FromValue NixString m v)
  => NString (m v)
  -> m (Maybe NixString)
assembleString =
  fromParts .
    \case
      Indented _ parts   -> parts
      DoubleQuoted parts -> parts
 where
  fromParts = fmap (fmap mconcat . sequence) . traverse go

  go =
    runAntiquoted
      "\n"
      (pure . pure . makeNixStringWithoutContext)
      (fromValueMay =<<)

buildArgument
  :: forall v m . MonadNixEval v m => Params (m v) -> m v -> m (AttrSet v)
buildArgument params arg = do
  scope <- currentScopes :: m (Scopes m v)
  case params of
    Param name -> M.singleton name <$> defer (withScopes scope arg)
    ParamSet s isVariadic m ->
      do
        (args, _) <- fromValue @(AttrSet v, AttrSet SourcePos) =<< arg
        let
          inject =
            maybe
              id
              (\ n -> M.insert n $ const $ defer (withScopes scope arg))
              m
        loebM
          (inject $
              M.mapMaybe
                id
                $ ialignWith
                  (assemble scope isVariadic)
                  args
                  (M.fromList s)
          )
 where
  assemble
    :: Scopes m v
    -> Bool
    -> Text
    -> These v (Maybe (m v))
    -> Maybe (AttrSet v -> m v)
  assemble scope isVariadic k = \case
    That Nothing -> pure $ const $ evalError @v $ ErrorCall $ "Missing value for parameter: " <>show k
    That (Just f) -> pure $ \args -> defer $ withScopes scope $ pushScope args f
    This _
      | isVariadic -> Nothing
      | otherwise  -> pure $ const $ evalError @v $ ErrorCall $ "Unexpected parameter: " <> show k
    These x _ -> pure (const (pure x))

addSourcePositions
  :: (MonadReader e m, Has e SrcSpan) => Transform NExprLocF (m a)
addSourcePositions f v@(Fix (Compose (Ann ann _))) =
  local (set hasLens ann) (f v)

addStackFrames
  :: forall v e m a
   . (Scoped v m, Framed e m, Typeable v, Typeable m)
  => Transform NExprLocF (m a)
addStackFrames f v = do
  scopes <- currentScopes :: m (Scopes m v)
  withFrame Info (EvaluatingExpr scopes v) (f v)

framedEvalExprLoc
  :: forall e v m
   . (MonadNixEval v m, Framed e m, Has e SrcSpan, Typeable m, Typeable v)
  => NExprLoc
  -> m v
framedEvalExprLoc =
  adi (eval . annotated . getCompose) (addStackFrames @v . addSourcePositions)
