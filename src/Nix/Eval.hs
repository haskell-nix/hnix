{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}



module Nix.Eval where

import           Control.Monad                  ( foldM )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Semialign.Indexed         ( ialignWith )
import           Data.Fix                       ( Fix(Fix) )
import qualified Data.HashMap.Lazy             as M
import           Data.List                      ( partition )
import           Data.These                     ( These(..) )
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
                  -> ( forall a. m v
                    -> ( AttrSet (m v)
                      -> m v
                      -> m (a, v)
                      )
                    -> m (a, v)
                    )
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
  }
  deriving (Show, Typeable)

instance (Typeable m, Typeable v) => Exception (SynHoleInfo m v)

-- jww (2019-03-18): By deferring only those things which must wait until
-- context of us, this can be written as:
-- eval :: forall v m . MonadNixEval v m => NExprF v -> m v
eval :: forall v m . MonadNixEval v m => NExprF (m v) -> m v

eval (NSym "__curPos") = evalCurPos

eval (NSym var       ) =
  do
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

eval (NBinary NApp fun arg) =
  do
    scope <- currentScopes :: m (Scopes m v)
    (`evalApp` withScopes scope arg) =<< fun

eval (NBinary op   larg rarg) = larg >>= evalBinary op ?? rarg

eval (NSelect aset attr alt ) = evalSelect aset attr >>= either go id
  where go (s, ks) = fromMaybe (attrMissing ks (pure s)) alt

eval (NHasAttr aset attr) = evalSelect aset attr >>= toValue . isRight

eval (NList l           ) =
  do
    scope <- currentScopes
    toValue =<< traverse (defer @v @m . withScopes @v scope) l

eval (NSet NNonRecursive binds) =
  toValue =<< evalBinds False (desugarBinds (eval . NSet NNonRecursive) binds)

eval (NSet NRecursive binds) =
  toValue =<< evalBinds True (desugarBinds (eval . NSet NNonRecursive) binds)

eval (NLet binds body    ) =
  do
    (x, _) <- evalBinds True binds
    pushScope x body

eval (NIf cond t f       ) =
  do
    v <- cond
    evalIf v t f

eval (NWith   scope  body) = evalWith scope body

eval (NAssert cond   body) =
  do
    x <- cond
    evalAssert x body

eval (NAbs    params body) = do
  -- It is the environment at the definition site, not the call site, that
  -- needs to be used when evaluating the body and default arguments, hence we
  -- defer here so the present scope is restored when the parameters and body
  -- are forced during application.
  scope <- currentScopes :: m (Scopes m v)
  evalAbs params $ \arg k -> withScopes scope $ do
    args <- buildArgument params arg
    pushScope args (k (fmap (withScopes scope . inform) args) body)

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
      (recurse mempty mempty)
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
desugarBinds embed binds = evalState (traverse (go <=< collect) binds) mempty
 where
  collect
    :: Binding r
    -> State
         (HashMap VarName (SourcePos, [Binding r]))
         (Either VarName (Binding r))
  collect (NamedVar (StaticKey x :| y : ys) val p) =
    do
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
evalBinds recursive binds =
  do
    scope <- currentScopes :: m (Scopes m v)

    buildResult scope . concat =<< traverse (applyBindToAdt scope) (moveOverridesLast binds)

 where
  buildResult
    :: Scopes m v
    -> [([Text], SourcePos, m v)]
    -> m (AttrSet v, AttrSet SourcePos)
  buildResult scope bindings =
    do
      (s, p) <- foldM insert (mempty, mempty) bindings
      res <-
        bool
          (traverse mkThunk s)
          (loebM (encapsulate <$> s))
          recursive

      pure (res, p)

   where
    mkThunk = defer . withScopes scope

    encapsulate f attrs = mkThunk . pushScope attrs $ f

    insert (m, p) (path, pos, value) = attrSetAlter path pos m p value

  applyBindToAdt :: Scopes m v -> Binding (m v) -> m [([Text], SourcePos, m v)]
  applyBindToAdt _ (NamedVar (StaticKey "__overrides" :| []) finalValue pos) =
    do
      (o', p') <- fromValue =<< finalValue
      -- jww (2018-05-09): What to do with the key position here?
      pure $
        (\ (k, v) ->
          ( [k]
          , fromMaybe pos (M.lookup k p')
          , pure =<< demand v
          )
        ) <$>
        M.toList o'

  applyBindToAdt _ (NamedVar pathExpr finalValue pos) =
    do
      fmap
        (\case
          -- When there are no path segments, e.g. `${null} = 5;`, we don't
          -- bind anything
          ([], _, _) -> mempty
          result     -> [result]
        )
        (processAttrSetKeys pathExpr)

   where
    processAttrSetKeys :: NAttrPath (m v) -> m ([Text], SourcePos, m v)
    processAttrSetKeys =
      \case
        h :| t ->
          maybe
            -- Empty attrset - return a stub.
            (pure ( mempty, nullPos, toValue @(AttrSet v, AttrSet SourcePos) (mempty, mempty)) )
            (\ k ->
              list
                -- No more keys in the attrset - return the result
                (pure ( [k], pos, finalValue ) )
                -- There are unprocessed keys in attrset - recurse appending the results
                (\ (x : xs) ->
                  do
                    (restOfPath, _, v) <- processAttrSetKeys (x :| xs)
                    pure ( k : restOfPath, pos, v )
                )
                t
            )
            =<< evalSetterKeyName h

  applyBindToAdt scope (Inherit ms names pos) =
    catMaybes <$>
      traverse
        processScope
        names
   where
    processScope
      :: NKeyName (m v)
      -> m (Maybe ([Text], SourcePos, m v))
    processScope nkeyname =
      (\ mkey ->
        do
          key <- mkey
          pure
            ([key]
            , pos
            , maybe
                (attrMissing (key :| []) Nothing)
                (pure <=< demand)
                =<< maybe
                    (withScopes scope $ lookupVar key)
                    (\ s ->
                      do
                        (attrset, _) <- fromValue @(AttrSet v, AttrSet SourcePos) =<< s

                        clearScopes @v $ pushScope attrset $ lookupVar key
                    )
                    ms
            )
      ) <$>
        evalSetterKeyName nkeyname

  moveOverridesLast = uncurry (<>) . partition
    (\case
      NamedVar (StaticKey "__overrides" :| []) _ _pos -> False
      _ -> True
    )

evalSelect
  :: forall v m
   . MonadNixEval v m
  => m v
  -> NAttrPath (m v)
  -> m (Either (v, NonEmpty Text) (m v))
evalSelect aset attr =
  do
    s    <- aset
    path <- traverse evalGetterKeyName attr

    extract s path

 where
  extract x path@(k :| ks) =
    do
      x' <- fromValueMay x

      case x' of
        Nothing -> pure $ Left (x, path)
        Just (s :: AttrSet v, p :: AttrSet SourcePos)
          | Just t <- M.lookup k s ->
            do
              list
                (pure $ pure $ pure =<< demand t)
                (\ (y : ys) -> (extract ?? (y :| ys)) =<< demand t)
                ks
          | otherwise -> Left . (, path) <$> toValue (s, p)

-- | Evaluate a component of an attribute path in a context where we are
-- *retrieving* a value
evalGetterKeyName
  :: forall v m
   . (MonadEval v m, FromValue NixString m v)
  => NKeyName (m v)
  -> m Text
evalGetterKeyName =
  maybe
    (evalError @v $ ErrorCall "value is null while a string was expected")
    pure
    <=< evalSetterKeyName

-- | Evaluate a component of an attribute path in a context where we are
-- *binding* a value
evalSetterKeyName
  :: (MonadEval v m, FromValue NixString m v)
  => NKeyName (m v)
  -> m (Maybe Text)
evalSetterKeyName =
  \case
    StaticKey k -> pure (pure k)
    DynamicKey k ->
      maybe
        mempty
        (pure . stringIgnoreContext)
        <$> runAntiquoted "\n" assembleString (fromValueMay =<<) k

assembleString
  :: forall v m
   . (MonadEval v m, FromValue NixString m v)
  => NString (m v)
  -> m (Maybe NixString)
assembleString =
  fromParts .
    \case
      Indented   _ parts -> parts
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
buildArgument params arg =
  do
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
                  (ialignWith
                    (assemble scope isVariadic)
                    args
                    (M.fromList s))
            )
 where
  assemble
    :: Scopes m v
    -> Bool
    -> Text
    -> These v (Maybe (m v))
    -> Maybe (AttrSet v -> m v)
  assemble scope isVariadic k =
    \case
      That Nothing -> pure $ const $ evalError @v $ ErrorCall $ "Missing value for parameter: " <> show k
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
addStackFrames f v =
  do
    scopes <- currentScopes :: m (Scopes m v)

    -- sectioning gives GHC optimization
    -- If opimization question would arrive again, check the @(`withFrameInfo` f v) $ EvaluatingExpr scopes v@
    -- for possible @scopes@ implementation @v@ type arguments sharing between runs.
    (`withFrameInfo` f v) $ (`EvaluatingExpr` v) scopes
 where
  withFrameInfo = withFrame Info

framedEvalExprLoc
  :: forall e v m
   . (MonadNixEval v m, Framed e m, Has e SrcSpan, Typeable m, Typeable v)
  => NExprLoc
  -> m v
framedEvalExprLoc =
  adi (eval . annotated . getCompose) (addStackFrames @v . addSourcePositions)
