{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language RankNTypes #-}


module Nix.Eval where

import           Nix.Prelude
import           Relude.Extra                   ( set )
import           Control.Monad                  ( foldM )
import           Control.Monad.Fix              ( MonadFix )
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Data.Semialign.Indexed         ( ialignWith )
import qualified Data.HashMap.Strict           as HM
import           Data.List                      ( partition )
import           Data.These                     ( These(..) )
import           Nix.Atoms
import           Nix.Convert
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Expr.Strings               ( runAntiquoted )
import           Nix.Frames
import           Nix.String
import           Nix.Scope
import           Nix.Value.Monad

class (Show v, Monad m) => MonadEval v m where
  freeVariable    :: VarName -> m v
  synHole         :: VarName -> m v
  attrMissing     :: NonEmpty VarName -> Maybe v -> m v
  evaledSym       :: VarName -> v -> m v
  evalCurPos      :: m v
  evalConstant    :: NAtom -> m v
  evalString      :: NString (m v) -> m v
  evalLiteralPath :: Path -> m v
  evalPath        :: NString (m v) -> m v
  evalEnvPath     :: Path -> m v
  evalUnary       :: NUnaryOp -> v -> m v
  evalBinary      :: NBinaryOp -> v -> m v -> m v
  -- ^ The second argument is an action because operators such as boolean &&
  -- and || may not evaluate the second argument.
  evalWith        :: m v -> m v -> m v
  evalIf          :: v -> m v -> m v -> m v
  evalAssert      :: v -> m v -> m v
  evalApp         :: v -> m v -> m v
  evalAbs         :: Params (m v)
                  -> ( forall a
                    . m v
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
  evalSet        :: AttrSet v -> PositionSet -> m v
  evalRecSetElem :: AttrSet (m v) -> Text -> m v -> m v
  evalRecSet     :: AttrSet v -> PositionSet -> m v
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
  , ToValue (AttrSet v, PositionSet) m v
  , FromValue (AttrSet v, PositionSet) m v
  )

data EvalFrame m v
  = EvaluatingExpr (Scopes m v) NExprLoc
  | ForcingExpr (Scopes m v) NExprLoc
  | Calling VarName SrcSpan
  | SynHole (SynHoleInfo m v)
  deriving (Show, Typeable)

instance (Typeable m, Typeable v) => Exception (EvalFrame m v)

data SynHoleInfo m v = SynHoleInfo
  { _synHoleInfo_expr :: NExprLoc
  , _synHoleInfo_scope :: Scopes m v
  }
  deriving (Show, Typeable)

instance (Typeable m, Typeable v) => Exception (SynHoleInfo m v)

-- TODO: Consider optimizing signature by deferring only context-dependent parts:
-- eval :: forall v m . MonadNixEval v m => NExprF v -> m v
eval :: forall v m . MonadNixEval v m => NExprF (m v) -> m v

eval (NSym "__curPos") = evalCurPos

eval (NSym var       ) =
  do
    mVal <- lookupVar var
    case mVal of
      Nothing -> freeVariable var
      Just v -> evaledSym var =<< demand v

eval (NConstant    x      ) = evalConstant x
eval (NStr         str    ) = evalString str
eval (NLiteralPath p      ) = evalLiteralPath p
eval (NPath        str    ) = evalPath str
eval (NEnvPath     p      ) = evalEnvPath p
eval (NUnary op arg       ) = evalUnary op =<< arg

eval (NApp fun arg        ) =
  do
    f <- fun
    scope <- askScopes
    evalApp f $ withScopes scope arg

eval (NBinary op   larg rarg) =
  do
    lav <- larg
    evalBinary op lav rarg

eval (NSelect alt aset attr) =
  do
    let useAltOrReportMissing (s, ks) =
          case alt of
            Nothing -> attrMissing ks $ pure s
            Just v -> v

    eAttr <- evalSelect aset attr
    case coerce eAttr of
      Left miss -> useAltOrReportMissing miss
      Right v -> v

eval (NHasAttr aset attr) =
  do
    eAttr <- evalSelect aset attr
    toValue $ isRight eAttr

eval (NList l           ) =
  do
    scope <- askScopes
    toValue =<< traverse (defer @v @m . withScopes @v scope) l

eval (NSet r binds) =
  do
    -- Bindings are already desugared at the AST level (see Nix.Expr.Desugar)
    attrSet <- evalBinds (r == Recursive) binds
    toValue attrSet

eval (NLet binds body    ) =
  do
    -- Bindings are already desugared at the AST level (see Nix.Expr.Desugar)
    (attrSet, _) <- evalBinds True binds
    pushScope (coerce attrSet) body

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
  curScope <- askScopes
  let
    withCurScope = withScopes curScope

    fun :: m v -> (AttrSet (m v) -> m v -> m r) -> m r
    fun arg k =
      withCurScope $
        do
          (coerce -> newScopeToAdd) <- buildArgument params arg
          pushScope
            newScopeToAdd $
            k
              (coerce $ withCurScope . inform <$> newScopeToAdd)
              body

  evalAbs
    params
    fun

eval (NSynHole name) = synHole name

-- | If you know that the 'scope' action will result in an 'AttrSet v', then
--   this implementation may be used as an implementation for 'evalWith'.
evalWithAttrSet :: forall v m . MonadNixEval v m => m v -> m v -> m v
evalWithAttrSet aset body = do
  scopes <- askScopes
  -- The scope is deliberately wrapped in a thunk here, since it is demanded
  -- each time a name is looked up within the weak scope, and we want to be
  -- sure the action it evaluates is to force a thunk, so its value is only
  -- computed once.
  deferredAset <- defer $ withScopes scopes aset
  let weakscope = coerce . fst <$> (fromValue @(AttrSet v, PositionSet) =<< demand deferredAset)

  pushWeakScope weakscope body

attrSetAlter
  :: forall v m
   . MonadNixEval v m
  => Bool          -- ^ allowOverwrite: if True, overwrite existing keys (for __overrides)
  -> [VarName]
  -> NSourcePos
  -> AttrSet (m v)
  -> PositionSet
  -> m v
  -> m (AttrSet (m v), PositionSet)
attrSetAlter allowOverwrite ks' pos m' p' val =
  swap <$> go p' m' ks'
 where
  -- This `go` does traverse in disquise. Notice how it traverses `ks`.
  go
    :: PositionSet
    -> AttrSet (m v)
    -> [VarName]
    -> m (PositionSet, AttrSet (m v))
  go _ _ [] = evalError @v $ ErrorCall "invalid selector with no components"
  go p m (k : ks) =
    if isPresent ks
      then
        case HM.lookup k m of
          Nothing -> recurse mempty mempty
          -- Dynamic attribute already defined - raise error (matching Nix behavior)
          -- unless allowOverwrite is True (for __overrides)
          Just _ | not allowOverwrite ->
            let oldPosMsg = case HM.lookup k p of
                  Just oldPos -> " at " <> show oldPos
                  Nothing     -> ""
            in evalError @v $ ErrorCall $
                 "dynamic attribute '" <> toString (varNameText k) <>
                 "' already defined" <> oldPosMsg
          -- With allowOverwrite, just overwrite the existing key
          Just _ -> pure $ insertVal (defer $ toValue @(AttrSet v, PositionSet) mempty)
      else
        -- Single-key binding: check for duplicate (from dynamic keys that evaluate to same value)
        -- unless allowOverwrite is True (for __overrides)
        case HM.lookup k m of
          Nothing -> pure $ insertVal val
          Just _ | allowOverwrite -> pure $ insertVal val
          Just _ ->
            let oldPosMsg = case HM.lookup k p of
                  Just oldPos -> " at " <> show oldPos
                  Nothing     -> ""
            in evalError @v $ ErrorCall $
                 "dynamic attribute '" <> toString (varNameText k) <>
                 "' already defined" <> oldPosMsg
   where
    insertVal :: m v -> (PositionSet, AttrSet (m v))
    insertVal v =
      ( insertPos
      , insertV v
      )
     where
      insertV v' = HM.insert k v' m
      insertPos = HM.insert k pos p

    recurse
      :: PositionSet
      -> AttrSet (m v)
      -> m ( PositionSet
          , AttrSet (m v)
          )
    recurse p'' m'' =
      insertVal . ((toValue @(AttrSet v, PositionSet)) <=< ((,mempty) <$>) . sequenceA . snd) <$> go p'' m'' ks

evalBinds
  :: forall v m
   . MonadNixEval v m
--  2021-07-19: NOTE: Recutsivity data type
  => Bool
  -> [Binding (m v)]
--  2021-07-19: NOTE: AttrSet is a Scope
  -> m (AttrSet v, PositionSet)
evalBinds isRecursive binds =
  do
    scope <- askScopes

    buildResult scope . fold =<< (`traverse` moveOverridesLast binds) (applyBindToAdt scope)

 where
  buildResult
    :: Scopes m v
    -> [(Bool, [VarName], NSourcePos, m v)]  -- (allowOverwrite, path, pos, value)
    -> m (AttrSet v, PositionSet)
  buildResult scopes bindings =
    do
      (coerce -> scope, p) <- foldM insert mempty bindings
      res <-
        if isRecursive
          then loebM . fmap encapsulate $ scope
          else traverse mkThunk scope

      pure (coerce res, p)

   where
    insert :: (AttrSet (m v), PositionSet) -> (Bool, [VarName], NSourcePos, m v) -> m (AttrSet (m v), PositionSet)
    insert (m, p) (allowOverwrite, path, pos, value) = attrSetAlter allowOverwrite path pos m p value

    mkThunk = defer . withScopes scopes

    encapsulate f ~attrs = mkThunk $ pushScope attrs f  -- ~attrs: lazy for loebM knot-tying

  applyBindToAdt :: Scopes m v -> Binding (m v) -> m [(Bool, [VarName], NSourcePos, m v)]
  applyBindToAdt _ (NamedVar (StaticKey "__overrides" :| []) finalValue pos) =
    do
      (o', p') <- fromValue =<< finalValue
      -- TODO: Determine correct position handling for __overrides keys
      -- Currently falls back to the binding position when key position is missing
      -- __overrides is allowed to overwrite existing keys
      pure $
        (\ (k, v) ->
          ( True  -- allowOverwrite for __overrides
          , one k
          , case HM.lookup k p' of
              Nothing -> pos
              Just p2 -> p2
          , demand v
          )
        ) <$> HM.toList o'

  applyBindToAdt _ (NamedVar pathExpr finalValue pos) =
    (\case
      -- When there are no path segments, e.g. `${null} = 5;`, we don't
      -- bind anything
      (_, [], _, _) -> mempty
      result        -> one result
    ) <$> processAttrSetKeys pathExpr
   where
    processAttrSetKeys :: NAttrPath (m v) -> m (Bool, [VarName], NSourcePos, m v)
    processAttrSetKeys (h :| t) =
      do
        mk <- evalSetterKeyName h
        case mk of
          -- Empty attrset - return a stub.
          Nothing -> pure (False, mempty, nullPos, toValue @(AttrSet v, PositionSet) mempty)
          Just k ->
            handlePresence
              -- No more keys in the attrset - return the result
              (pure (False, one k, pos, finalValue))  -- allowOverwrite = False
              -- There are unprocessed keys in attrset - recurse appending the results
              (\ (x : xs) -> do
                (_, restOfPath, _, v) <- processAttrSetKeys (x :| xs)
                pure (False, k : restOfPath, pos, v)  -- allowOverwrite = False
              )
              t

  applyBindToAdt scopes (Inherit ms names pos) =
    pure $ processScope <$> names
   where
    processScope
      :: VarName
      -> (Bool, [VarName], NSourcePos, m v)
    processScope var =
      ( False  -- allowOverwrite = False for Inherit
      , one var
      , pos
      , do
          scopeVal <-
            case ms of
              Nothing -> withScopes scopes $ lookupVar var
              Just s -> do
                (coerce -> scope, _) <- fromValue @(AttrSet v, PositionSet) =<< s
                clearScopes $ pushScope @v scope $ lookupVar var
          case scopeVal of
            Nothing -> attrMissing (one var) Nothing
            Just v -> demand v
      )

  moveOverridesLast = uncurry (<>) . partition
    (\case
      NamedVar (StaticKey "__overrides" :| []) _ _ -> False
      _ -> True
    )

evalSelect
  :: forall v m
   . MonadNixEval v m
  => m v
  -> NAttrPath (m v)
  -> m (Either (v, NonEmpty VarName) (m v))
evalSelect aset attr =
  do
    s    <- aset
    path <- traverse evalGetterKeyName attr

    extract path s

 where
  extract :: NonEmpty VarName -> v -> m (Either (v, NonEmpty VarName) (m v))
  extract path@(k :| ks) x =
    do
      mset <- fromValueMay @(AttrSet v, PositionSet) x
      case mset of
        Nothing -> left
        Just (attrs, _) ->
          case HM.lookup k attrs of
            Nothing -> left
            Just v ->
              handlePresence
                (pure . pure)
                (\ (y : ys) -> (extract (y :| ys) =<<))
                ks
                (demand v)
   where
    left :: m (Either (v, NonEmpty VarName) b)
    left = pure $ Left (x, path)

-- | Evaluate a component of an attribute path in a context where we are
-- *retrieving* a value
evalGetterKeyName
  :: forall v m
   . (MonadEval v m, FromValue NixString m v)
  => NKeyName (m v)
  -> m VarName
evalGetterKeyName key = do
  mk <- evalSetterKeyName key
  case mk of
    Nothing -> evalError @v $ ErrorCall "value is null while a string was expected"
    Just v -> pure v

-- | Evaluate a component of an attribute path in a context where we are
-- *binding* a value
evalSetterKeyName
  :: (MonadEval v m, FromValue NixString m v)
  => NKeyName (m v)
  -> m (Maybe VarName)
evalSetterKeyName =
  \case
    StaticKey k -> pure $ pure k
    DynamicKey k ->
      mkVarName . ignoreContext <<$>> runAntiquoted "\n" assembleString (fromValueMay =<<) k

assembleString
  :: forall v m
   . (MonadEval v m, FromValue NixString m v)
  => NString (m v)
  -> m (Maybe NixString)
assembleString = fromParts . stringParts
 where
  fromParts :: [Antiquoted Text (m v)] -> m (Maybe NixString)
  fromParts xs = fold <<$>> traverse2 fun xs

  fun :: Antiquoted Text (m v) -> m (Maybe NixString)
  fun =
    runAntiquoted
      "\n"
      (pure . pure . mkNixStringWithoutContext)
      (fromValueMay =<<)

buildArgument
  :: forall v m . MonadNixEval v m => Params (m v) -> m v -> m (AttrSet v)
buildArgument params arg =
  case params of
    -- For simple parameter binding, arg is already thunked from evalApp.
    -- No need for additional defer - just bind the name to the existing thunk.
    Param name -> one . (name,) <$> arg
    ParamSet mname variadic pset -> do
      -- ParamSet needs scope for default value evaluation
      scope <- askScopes
      let argThunk = defer $ withScopes scope arg
      (args, _) <- fromValue @(AttrSet v, PositionSet) =<< arg
      let
        inject =
          case mname of
            Nothing -> id
            Just name -> HM.insert name (const argThunk) -- why insert into const? Thunk value getting magic point?
      loebM $
        inject $
          HM.mapMaybe
            id
            $ ialignWith
                (assemble scope variadic)
                args
                pset
 where
  assemble
    :: Scopes m v
    -> Variadic
    -> VarName
    -> These v (Maybe (m v))
    -> Maybe (AttrSet v -> m v)
  assemble _ Variadic _ (This _) = Nothing
  assemble scope _ k t =
    pure $
      case t of
        That Nothing -> const $ evalError @v $ ErrorCall $ "Missing value for parameter: ''" <> show k
        That (Just f) -> coerce $ defer . withScopes scope . (`pushScope` f)
        This _ -> const $ evalError @v $ ErrorCall $ "Unexpected parameter: " <> show k
        These x _ -> const $ pure x

-- | Add source positions to @NExprLoc@.
--
-- Takes @NExprLoc@, by itself takes source position informatoin, does transformation,
-- returns @NExprLoc@ with source positions.
--
-- Actually:
--
-- > => (NExprLoc -> m a)
-- > -> NExprLoc -> m a
addSourcePositions
  :: (MonadReader e m, Has e SrcSpan) => Transform NExprLocF (m a)
addSourcePositions f (v@(Ann ann _) :: NExprLoc) =
  local (set hasLens ann) $ f v

addStackFrames
  :: forall v e m a
   . (Scoped v m, Framed e m, Typeable v, Typeable m)
  => TransformF NExprLoc (m a)
addStackFrames f v =
  do
    scopes <- askScopes

    -- sectioning gives GHC optimization
    -- If opimization question would arrive again, check the @(`withFrameInfo` f v) $ EvaluatingExpr scopes v@
    -- for possible @scopes@ implementation @v@ type arguments sharing between runs.
    (`withFrameInfo` f v) $ (`EvaluatingExpr` v) scopes
 where
  withFrameInfo = withFrame Info

evalWithMetaInfo
  :: forall e v m
  . (MonadNixEval v m, Framed e m, Has e SrcSpan, Typeable m, Typeable v)
  => NExprLoc
  -> m v
evalWithMetaInfo =
  adi addMetaInfo evalContent

-- | Add source positions & frame context system.
addMetaInfo
  :: forall v m e a
  . (Framed e m, Scoped v m, Has e SrcSpan, Typeable m, Typeable v)
  => TransformF NExprLoc (m a)
addMetaInfo = addStackFrames @v . addSourcePositions

-- | Takes annotated expression. Strip from annotation. Evaluate.
evalContent
  :: MonadNixEval v m
  => AnnF ann NExprF (m v)
  -> m v
evalContent = eval . stripAnnF
