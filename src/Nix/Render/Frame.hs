{-# language CPP #-}
{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language MultiWayIf #-}
{-# language TypeFamilies #-}


-- | Code for rendering/representation of the messages packaged with their context (Frames).
module Nix.Render.Frame where

import           Nix.Prelude         hiding ( Comparison )
import           GHC.Exception              ( ErrorCall )
import           Data.Fix                   ( Fix(..) )
import           Nix.Eval            hiding ( addMetaInfo )
import           Nix.Exec
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.Normal
import           Nix.Options
import           Nix.Pretty
import           Nix.Render
import           Nix.Thunk
import           Nix.Value
import           Prettyprinter       hiding ( list )
import qualified Text.Show                 as Text
import           Text.Megaparsec.Pos        ( sourcePosPretty)
import qualified Text.Show.Pretty          as PS

renderFrames
  :: forall v t f e m ann
   . ( MonadReader e m
     , Has e Options
     , MonadFile m
     , MonadCitedThunks t f m
     , Typeable v
     )
  => Frames
  -> m (Doc ann)
renderFrames []       = stub
renderFrames xss@(x : xs) =
  do
    opts <- askOptions
    let
      verbosity :: Verbosity
      verbosity = getVerbosity opts
    renderedFrames <- if
        | verbosity <= ErrorsOnly -> render1 x
      --  2021-10-22: NOTE: List reverse is completely conterproductive. `reverse` of list famously neest to traverse the whole list to take the last element
        | verbosity <= Informational -> (foldMap renderPosition (reverse xs) <>) <$> render1 x
        | otherwise -> foldMapM render1 (reverse xss)
    pure $
      handlePresence
        mempty
        vsep
        renderedFrames
 where
  render1 :: NixFrame -> m [Doc ann1]
  render1 = renderFrame @v @t @f

  renderPosition :: NixFrame -> [Doc ann]
  renderPosition =
    whenJust
      (\ pos -> one ("While evaluating at " <> pretty (sourcePosPretty $ toSourcePos pos) <> colon))
      . framePos @v @m

framePos
  :: forall v (m :: Type -> Type)
   . (Typeable m, Typeable v)
  => NixFrame
  -> Maybe NSourcePos
framePos (NixFrame _ f) =
  (\case
    EvaluatingExpr _ (Ann (SrcSpan beg _) _) -> pure beg
    _ -> Nothing
  )
  =<< fromException @(EvalFrame m v) f

renderFrame
  :: forall v t f e m ann
   . ( MonadReader e m
     , Has e Options
     , MonadFile m
     , MonadCitedThunks t f m
     , Typeable v
     )
  => NixFrame
  -> m [Doc ann]
renderFrame (NixFrame level f)
  | Just (e :: EvalFrame      m v) <- fromException f = renderEvalFrame  level  e
  | Just (e :: ThunkLoop         ) <- fromException f = renderThunkLoop  level  e
  | Just (e :: ValueFrame t f m  ) <- fromException f = renderValueFrame level  e
  | Just (e :: NormalLoop t f m  ) <- fromException f = renderNormalLoop level  e
  | Just (e :: ExecFrame  t f m  ) <- fromException f = renderExecFrame  level  e
  | Just (e :: ErrorCall         ) <- fromException f = pure $ one $ pretty (Text.show e)
  | Just (e :: SynHoleInfo    m v) <- fromException f = pure $ one $ pretty (Text.show e)
  | otherwise = fail $ "Unrecognized frame: " <> show f

wrapExpr :: NExprF r -> NExpr
wrapExpr x = Fix (Fix (NSym Unknown "<?>") <$ x)

renderEvalFrame
  :: forall e m v ann
  . (MonadReader e m, Has e Options, MonadFile m)
  => NixLevel
  -> EvalFrame m v
  -> m [Doc ann]
renderEvalFrame level f =
  do
    opts <- askOptions
    let
      addMetaInfo :: ([Doc ann] -> [Doc ann]) -> SrcSpan -> Doc ann -> m [Doc ann]
      addMetaInfo trans loc = fmap (trans . one) . renderLocation loc

    case f of
      EvaluatingExpr scope e@(Ann loc _) ->
        addMetaInfo
          (scopeInfo <>)
          loc
          =<< renderExpr level "While evaluating" "Expression" e
         where
          scopeInfo :: [Doc ann]
          scopeInfo =
            one (pretty $ Text.show scope) `whenTrue` isShowScopes opts

      ForcingExpr _scope e@(Ann loc _) | isThunks opts ->
        addMetaInfo
          id
          loc
          =<< renderExpr level "While forcing thunk from" "Forcing thunk" e

      Calling name loc ->
        addMetaInfo
          id
          loc
          $ "While calling `builtins." <> prettyVarName name <> "`"

      SynHole synfo ->
        sequenceA
          [ renderLocation loc =<<
              renderExpr level "While evaluating" "Syntactic Hole" e
          , pure $ pretty $ Text.show $ _synHoleInfo_scope synfo
          ]
         where
          e@(Ann loc _) = _synHoleInfo_expr synfo

      ForcingExpr _ _ -> stub


renderExpr
  :: (MonadReader e m, Has e Options, MonadFile m)
  => NixLevel
  -> Text
  -> Text
  -> NExprLoc
  -> m (Doc ann)
renderExpr _level longLabel shortLabel e@(Ann _ x) =
  do
    opts <- askOptions
    let
      verbosity :: Verbosity
      verbosity = getVerbosity opts

      expr :: NExpr
      expr = stripAnnotation e

      concise = prettyNix $ Fix $ Fix (NSym Unknown "<?>") <$ x

      chatty =
        bool
          (pretty $ PS.ppShow expr)
          (prettyNix expr)
          (verbosity == Chatty)

    pure $
      bool
        (pretty shortLabel <> fillSep [": ", concise])
        (vsep [pretty (longLabel <> ":\n>>>>>>>>"), indent 2 chatty, "<<<<<<<<"])
        (verbosity >= Chatty)

renderValueFrame
  :: forall e t f m ann
   . (MonadReader e m, Has e Options, MonadFile m, MonadCitedThunks t f m)
  => NixLevel
  -> ValueFrame t f m
  -> m [Doc ann]
renderValueFrame level = fmap one . \case
  ForcingThunk    _t -> pure "ForcingThunk" -- jww (2019-03-18): NYI
  ConcerningValue _v -> pure "ConcerningValue"
  Comparison     _ _ -> pure "Comparing"
  Addition       _ _ -> pure "Adding"
  Division       _ _ -> pure "Dividing"
  Multiplication _ _ -> pure "Multiplying"

  Coercion       x y -> pure
    $ fold [desc, pretty (describeValue x), " to ", pretty (describeValue y)]
   where
    desc =
      bool
        "While coercing "
        "Cannot coerce "
        (level <= Error)

  CoercionToJson v ->
    ("CoercionToJson " <>) <$> dumbRenderValue v
  CoercionFromJson _j -> pure "CoercionFromJson"
  Expectation t v     ->
    (msg <>) <$> dumbRenderValue v
   where
    msg = "Expected " <> pretty (describeValue t) <> ", but saw "

--  2021-10-28: NOTE: notice it ignores `level`, `longlabel` & `shortlabel`, to underline that `dumbRenderValue` synonym was created
renderValue
  :: forall e t f m ann
   . (MonadReader e m, Has e Options, MonadFile m, MonadCitedThunks t f m)
  => NixLevel
  -> Text
  -> Text
  -> NValue t f m
  -> m (Doc ann)
renderValue _level _longLabel _shortLabel v =
  do
    opts <- askOptions
    bool
      prettyNValue
      prettyNValueProv
      (isValues opts)
      <$> removeEffects v

dumbRenderValue
  :: forall e t f m ann
   . (MonadReader e m, Has e Options, MonadFile m, MonadCitedThunks t f m)
   => (NValue t f m -> m (Doc ann))
dumbRenderValue = renderValue Info mempty mempty

renderExecFrame
  :: (MonadReader e m, Has e Options, MonadFile m, MonadCitedThunks t f m)
  => NixLevel
  -> ExecFrame t f m
  -> m [Doc ann]
renderExecFrame _level (Assertion ann v) =
  fmap
    one
    $ renderLocation ann . fillSep . on (<>) one "Assertion failed:" =<< dumbRenderValue v

renderThunkLoop
  :: (MonadReader e m, Has e Options, MonadFile m, Show (ThunkId m))
  => NixLevel
  -> ThunkLoop
  -> m [Doc ann]
renderThunkLoop _level (ThunkLoop n) =
  pure . one . pretty $ "Infinite recursion in thunk " <> n

renderNormalLoop
  :: (MonadReader e m, Has e Options, MonadFile m, MonadCitedThunks t f m)
  => NixLevel
  -> NormalLoop t f m
  -> m [Doc ann]
renderNormalLoop _level (NormalLoop v) =
  one . ("Infinite recursion during normalization forcing " <>) <$> dumbRenderValue v
