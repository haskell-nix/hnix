{-# language CPP #-}
{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PartialTypeSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

{-# options_ghc -fno-warn-name-shadowing #-}


-- | This module provides a "reducing" expression evaluator, which reduces
--   away pure, non self-referential aspects of an expression tree, yielding a
--   new expression tree. It does not yet attempt to reduce everything
--   possible, and will always yield a tree with the same meaning as the
--   original. It should be seen as an opportunistic simplifier, but which
--   gives up easily if faced with any potential for ambiguity in the result.

module Nix.Reduce
  ( reduceExpr
  , reducingEvalExpr
  ) where

import           Control.Monad.Catch            ( MonadCatch(catch) )
#if !MIN_VERSION_base(4,13,0)
import           Prelude                 hiding ( fail )
import           Control.Monad.Fail
#endif
import           Control.Monad.Fix              ( MonadFix )
import           Data.Fix                       ( Fix(..)
                                                , foldFix
                                                , foldFixM
                                                )
import qualified Data.HashMap.Internal         as HM
                                                ( lookup
                                                , insert
                                                , singleton
                                                , fromList
                                                )
import qualified Data.List.NonEmpty            as NE
import qualified Text.Show
import           Nix.Atoms
import           Nix.Effects.Basic              ( pathToDefaultNixFile )
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.Options                    ( Options
                                                , reduceSets
                                                , reduceLists
                                                )
import           Nix.Parser
import           Nix.Scope
import           Nix.Utils
import           System.Directory
import           System.FilePath

newtype Reducer m a = Reducer
    { runReducer ::
        ReaderT
          ( Maybe Path
          , Scopes (Reducer m) NExprLoc
          )
          ( StateT
              ( HashMap Path NExprLoc
              , HashMap Text Text
              )
            m
          )
          a
    }
  deriving
    ( Functor, Applicative, Alternative
    , Monad, MonadPlus, MonadFix, MonadIO, MonadFail
    , MonadReader (Maybe Path, Scopes (Reducer m) NExprLoc)
    , MonadState (HashMap Path NExprLoc, HashMap Text Text)
    )

staticImport
  :: forall m
   . ( MonadIO m
     , Scoped NExprLoc m
     , MonadFail m
     , MonadReader (Maybe Path, Scopes m NExprLoc) m
     , MonadState (HashMap Path NExprLoc, HashMap Text Text) m
     )
  => SrcSpan
  -> Path
  -> m NExprLoc
staticImport pann path = do
  mfile <- asks (coerce . fst)
  path  <- liftIO $ pathToDefaultNixFile path
  path' <- liftIO $ pathToDefaultNixFile =<< (coerce <$> canonicalizePath . coerce)
    (maybe id ((</>) . takeDirectory) mfile (coerce path))

  imports <- gets fst
  maybe
    (go path')
    pure
    (HM.lookup path' imports)
 where
  go :: Path -> m NExprLoc
  go path = do
    liftIO $ putStrLn $ "Importing file " <> (coerce path)

    eres <- liftIO $ parseNixFileLoc path
    either
      (\ err -> fail $ "Parse failed: " <> show err)
      (\ x -> do
        let
          pos  = SourcePos "Reduce.hs" (mkPos 1) (mkPos 1)
          span = SrcSpan pos pos
          cur  =
            NamedVar
              (StaticKey "__cur_file" :| mempty)
              (Fix (NLiteralPathAnnF pann path))
              pos
          x' = Fix $ NLet_ span [cur] x
        modify $ first $ HM.insert path x'
        local
          (const (pure path, mempty)) $
          do
            x'' <- foldFix reduce x'
            modify $ first $ HM.insert path x''
            pure x''
      )
      eres

-- gatherNames :: NExprLoc -> HashSet VarName
-- gatherNames = foldFix $ \case
--     NSymAnnF _ var -> S.singleton var
--     AnnF _ x -> fold x

reduceExpr
  :: (MonadIO m, MonadFail m) => Maybe Path -> NExprLoc -> m NExprLoc
reduceExpr mpath expr =
  (`evalStateT` (mempty, mempty))
    . (`runReaderT` (mpath, mempty))
    . runReducer
    $ foldFix reduce expr

reduce
  :: forall m
   . ( MonadIO m
     , Scoped NExprLoc m
     , MonadFail m
     , MonadReader (Maybe Path, Scopes m NExprLoc) m
     , MonadState (HashMap Path NExprLoc, HashMap Text Text) m
     )
  => NExprLocF (m NExprLoc)
  -> m NExprLoc

-- | Reduce the variable to its value if defined.
--   Leave it as it is otherwise.
reduce (NSymAnnF ann var) =
  fromMaybe (Fix (NSymAnnF ann var)) <$> lookupVar var

-- | Reduce binary and integer negation.
reduce (NUnary_ uann op arg) =
  do
    x <- arg
    pure $ Fix $
      case (op, x) of
        (NNeg, Fix (NConstantAnnF cann (NInt  n))) -> NConstantAnnF cann $ NInt $ negate n
        (NNot, Fix (NConstantAnnF cann (NBool b))) -> NConstantAnnF cann $ NBool $ not b
        _                                       -> NUnary_    uann op x

-- | Reduce function applications.
--
--     * Reduce an import to the actual imported expression.
--
--     * Reduce a lambda function by adding its name to the local
--       scope and recursively reducing its body.
reduce (NBinary_ bann NApp fun arg) = fun >>= \case
  f@(Fix (NSymAnnF _ "import")) ->
    (\case
        -- Fix (NEnvPath_     pann origPath) -> staticImport pann origPath
      Fix (NLiteralPathAnnF pann origPath) -> staticImport pann origPath
      v -> pure $ Fix $ NBinary_ bann NApp f v
    ) =<< arg

  Fix (NAbs_ _ (Param name) body) ->
    do
      x <- arg
      pushScope
        (coerce $ HM.singleton name x)
        (foldFix reduce body)

  f -> Fix . NBinary_ bann NApp f <$> arg

-- | Reduce an integer addition to its result.
reduce (NBinary_ bann op larg rarg) =
  do
    lval <- larg
    rval <- rarg
    pure $ Fix $
      case (op, lval, rval) of
        (NPlus, Fix (NConstantAnnF ann (NInt x)), Fix (NConstantAnnF _ (NInt y))) -> NConstantAnnF ann  $ NInt $ x + y
        _                                                                   -> NBinary_   bann op lval rval

-- | Reduce a select on a Set by substituting the set to the selected value.
--
-- Before applying this reduction, we need to ensure that:
--
--   1. The selected expr is indeed a set.
--   2. The selection AttrPath is a list of StaticKeys.
--   3. The selected AttrPath exists in the set.
reduce base@(NSelect_ _ _ _ attrs)
  | sAttrPath $ NE.toList attrs = do
    (NSelect_ _ _ aset attrs) <- sequence base
    inspectSet (unFix aset) attrs
  | otherwise = sId
 where
  sId = Fix <$> sequence base
  -- The selection AttrPath is composed of StaticKeys.
  sAttrPath (StaticKey _ : xs) = sAttrPath xs
  sAttrPath []                 = True
  sAttrPath _                  = False
  -- Find appropriate bind in set's binds.
  findBind []   _              = Nothing
  findBind (x : xs) attrs@(a :| _) = case x of
    n@(NamedVar (a' :| _) _ _) | a' == a -> pure n
    _ -> findBind xs attrs
  -- Follow the attrpath recursively in sets.
  inspectSet (NSetAnnF _ NonRecursive binds) attrs = case findBind binds attrs of
    Just (NamedVar _ e _) -> case NE.uncons attrs of
      (_, Just attrs) -> inspectSet (unFix e) attrs
      _               -> pure e
    _ -> sId
  inspectSet _ _ = sId

-- reduce (NHasAttr aset attr) =

-- | Reduce a set by inlining its binds outside of the set
--   if none of the binds inherit the super set.
reduce e@(NSetAnnF ann NonRecursive binds) =
  do
    let
      usesInherit =
        any
          (\case
            Inherit{} -> True
            _         -> False
          )
          binds

    bool
      (Fix <$> sequence e)
      (clearScopes @NExprLoc $ Fix . NSetAnnF ann NonRecursive <$> traverse sequence binds)
      usesInherit

-- Encountering a 'rec set' construction eliminates any hope of inlining
-- definitions.
reduce (NSetAnnF ann Recursive binds) =
  clearScopes @NExprLoc $ Fix . NSetAnnF ann Recursive <$> traverse sequence binds

-- Encountering a 'with' construction eliminates any hope of inlining
-- definitions.
reduce (NWith_ ann scope body) =
  clearScopes @NExprLoc $ Fix <$> liftA2 (NWith_ ann) scope body

-- | Reduce a let binds section by pushing lambdas,
--   constants and strings to the body scope.
reduce (NLet_ ann binds body) =
  do
    binds' <- traverse sequence binds
    body'  <-
      (`pushScope` body) . coerce . HM.fromList . catMaybes =<<
        traverse
          (\case
            NamedVar (StaticKey name :| []) def _pos ->
              let
                defcase =
                  \case
                    d@(Fix NAbs_     {}) -> pure (name, d)
                    d@(Fix NConstantAnnF{}) -> pure (name, d)
                    d@(Fix NStrAnnF     {}) -> pure (name, d)
                    _                    -> Nothing
              in
              defcase <$> def

            _ -> pure Nothing
          )
          binds

    -- let names = gatherNames body'
    -- binds' <- traverse sequence binds <&> \b -> flip filter b $ \case
    --     NamedVar (StaticKey name _ :| []) _ ->
    --         name `S.member` names
    --     _ -> True
    pure $ Fix $ NLet_ ann binds' body'
    -- where
    --   go m [] = pure m
    --   go m (x:xs) = case x of
    --       NamedVar (StaticKey name _ :| []) def -> do
    --           v <- pushScope m def
    --           go (M.insert name v m) xs
    --       _ -> go m xs

-- | Reduce an if to the relevant path if
--   the condition is a boolean constant.
reduce e@(NIf_ _ b t f) =
  (\case
    Fix (NConstantAnnF _ (NBool b')) -> if b' then t else f
    _                             -> Fix <$> sequence e
  ) =<< b

-- | Reduce an assert atom to its encapsulated
--   symbol if the assertion is a boolean constant.
reduce e@(NAssert_ _ b body) =
  (\case
    Fix (NConstantAnnF _ (NBool b')) | b' -> body
    _ -> Fix <$> sequence e
  ) =<< b

reduce (NAbs_ ann params body) = do
  params' <- sequence params
  -- Make sure that variable definitions in scope do not override function
  -- arguments.
  let
    scope = coerce $
      case params' of
        Param    name     -> one (name, Fix $ NSymAnnF ann name)
        ParamSet _ _ pset ->
          HM.fromList $ (\(k, _) -> (k, Fix $ NSymAnnF ann k)) <$> pset
  Fix . NAbs_ ann params' <$> pushScope scope body

reduce v = Fix <$> sequence v

-- newtype FlaggedF f r = FlaggedF { flagged :: (IORef Bool, f r) }
newtype FlaggedF f r = FlaggedF (IORef Bool, f r)
  deriving (Functor, Foldable, Traversable)

instance Show (f r) => Show (FlaggedF f r) where
  show (FlaggedF (_, x)) = show x

type Flagged f = Fix (FlaggedF f)

flagExprLoc :: (MonadIO n, Traversable f) => Fix f -> n (Flagged f)
flagExprLoc = foldFixM $ \x -> do
  flag <- liftIO $ newIORef False
  pure $ coerce (flag, x)

-- stripFlags :: Functor f => Flagged f -> Fix f
-- stripFlags = foldFix $ Fix . snd . flagged

pruneTree :: MonadIO n => Options -> Flagged NExprLocF -> n (Maybe NExprLoc)
pruneTree opts =
  foldFixM $
    \(FlaggedF (b, Compose x)) ->
      bool
        Nothing
        (Fix . Compose <$> traverse prune x)
        <$> liftIO (readIORef b)
 where
  prune :: NExprF (Maybe NExprLoc) -> Maybe (NExprF NExprLoc)
  prune = \case
    NStr str -> pure $ NStr $ pruneString str
    NHasAttr (Just aset) attr ->
      pure $ NHasAttr aset $ pruneKeyName <$> attr
    NAbs params (Just body) -> pure $ NAbs (pruneParams params) body

    NList l -> pure $ NList $
      bool
        (fromMaybe annNNull <$>)
        catMaybes
        (reduceLists opts)  -- Reduce list members that aren't used; breaks if elemAt is used
        l
    NSet recur binds -> pure $ NSet recur $
      bool
        (fromMaybe annNNull <<$>>)
        (mapMaybe sequence)
        (reduceSets opts)  -- Reduce set members that aren't used; breaks if hasAttr is used
        binds

    NLet binds (Just body@(Ann _ x)) ->
      pure $
        list
          x
          (`NLet` body)
          (mapMaybe pruneBinding binds)

    NSelect alt (Just aset) attr ->
      pure $ NSelect (join alt) aset $ pruneKeyName <$> attr

    -- These are the only short-circuiting binary operators
    NBinary NAnd (Just (Ann _ larg)) _ -> pure larg
    NBinary NOr  (Just (Ann _ larg)) _ -> pure larg

    -- If the function was never called, it means its argument was in a
    -- thunk that was forced elsewhere.
    NBinary NApp Nothing (Just _) -> Nothing

    -- The idea behind emitted a binary operator where one side may be
    -- invalid is that we're trying to emit what will reproduce whatever
    -- fail the user encountered, which means providing all aspects of
    -- the evaluation path they ultimately followed.
    NBinary op Nothing (Just rarg) -> pure $ NBinary op annNNull rarg
    NBinary op (Just larg) Nothing -> pure $ NBinary op larg annNNull

    -- If the scope of a with was never referenced, it's not needed
    NWith Nothing (Just (Ann _ body)) -> pure body

    NAssert Nothing _              -> fail "How can an assert be used, but its condition not?"
    NAssert _ (Just (Ann _ body)) -> pure body
    NAssert (Just cond) _          -> pure $ NAssert cond annNNull

    NIf Nothing _ _ -> fail "How can an if be used, but its condition not?"

    NIf _ Nothing (Just (Ann _ f)) -> pure f
    NIf _ (Just (Ann _ t)) Nothing -> pure t

    x                     -> sequence x

  pruneString :: NString (Maybe NExprLoc) -> NString NExprLoc
  pruneString (DoubleQuoted xs) = DoubleQuoted $ mapMaybe pruneAntiquotedText xs
  pruneString (Indented n   xs) = Indented n   $ mapMaybe pruneAntiquotedText xs

  pruneAntiquotedText
    :: Antiquoted Text (Maybe NExprLoc) -> Maybe (Antiquoted Text NExprLoc)
  pruneAntiquotedText (Plain v)             = pure $ Plain v
  pruneAntiquotedText EscapedNewline        = pure EscapedNewline
  pruneAntiquotedText (Antiquoted (Just k)) = pure $ Antiquoted k
  pruneAntiquotedText (Antiquoted Nothing ) = Nothing

  pruneAntiquoted
    :: Antiquoted (NString (Maybe NExprLoc)) (Maybe NExprLoc)
    -> Maybe (Antiquoted (NString NExprLoc) NExprLoc)
  pruneAntiquoted (Plain v)             = pure $ Plain $ pruneString v
  pruneAntiquoted EscapedNewline        = pure EscapedNewline
  pruneAntiquoted (Antiquoted (Just k)) = pure $ Antiquoted k
  pruneAntiquoted (Antiquoted Nothing ) = Nothing

  pruneKeyName :: NKeyName (Maybe NExprLoc) -> NKeyName NExprLoc
  pruneKeyName (StaticKey n) = StaticKey n
  pruneKeyName (DynamicKey k) | Just k' <- pruneAntiquoted k = DynamicKey k'
                              | otherwise = StaticKey "<unused?>"

  pruneParams :: Params (Maybe NExprLoc) -> Params NExprLoc
  pruneParams (Param n) = Param n
  pruneParams (ParamSet mname variadic pset) =
    ParamSet mname variadic (reduceOrPassMode <$> pset)
   where
    reduceOrPassMode =
      second $
        bool
          fmap
          ((pure .) . maybe annNNull)
          (reduceSets opts)  -- Reduce set members that aren't used; breaks if hasAttr is used
          (fromMaybe annNNull)

  pruneBinding :: Binding (Maybe NExprLoc) -> Maybe (Binding NExprLoc)
  pruneBinding (NamedVar _                 Nothing  _  ) = Nothing
  pruneBinding (NamedVar xs                (Just x) pos) = pure $ NamedVar (pruneKeyName <$> xs) x pos
  pruneBinding (Inherit  _                 []       _  ) = Nothing
  pruneBinding (Inherit  (join -> Nothing) _        _  ) = Nothing
  pruneBinding (Inherit  (join -> m)       xs       pos) = pure $ Inherit m xs pos

reducingEvalExpr
  :: (Framed e m, Has e Options, Exception r, MonadCatch m, MonadIO m)
  => (NExprLocF (m a) -> m a)
  -> Maybe Path
  -> NExprLoc
  -> m (NExprLoc, Either r a)
reducingEvalExpr eval mpath expr =
  do
    expr'           <- flagExprLoc =<< liftIO (reduceExpr mpath expr)
    eres <- (`catch` pure . Left) $
      pure <$> foldFix (addEvalFlags eval) expr'
    opts :: Options <- asks $ view hasLens
    expr''          <- pruneTree opts expr'
    pure (fromMaybe annNNull expr'', eres)
 where
  addEvalFlags k (FlaggedF (b, x)) = liftIO (writeIORef b True) *> k x

instance Monad m => Scoped NExprLoc (Reducer m) where
  currentScopes = currentScopesReader
  clearScopes   = clearScopesReader @(Reducer m) @NExprLoc
  pushScopes    = pushScopesReader
  lookupVar     = lookupVarReader
