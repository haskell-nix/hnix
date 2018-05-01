{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nix.Render.Frame where

import           Control.Monad.Reader
import           Data.Fix
import           Data.Typeable
import           Nix.Eval
import           Nix.Exec
import           Nix.Expr
import           Nix.Frames
import           Nix.Normal
import           Nix.Options
import           Nix.Parser.Library hiding (colon)
import           Nix.Pretty
import           Nix.Render
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value
import qualified Text.PrettyPrint.ANSI.Leijen as P
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.Show.Pretty as PS

renderFrames :: forall v e m.
               (MonadReader e m, Has e Options,
                MonadVar m, MonadFile m, Typeable m, Typeable v)
             => Frames -> m Doc
renderFrames [] = pure mempty
renderFrames (x:xs) = do
    opts :: Options <- asks (view hasLens)
    frames <-
        if | verbose opts <= ErrorsOnly ->
             renderFrame @v x
           | verbose opts <= Informational -> do
             f <- renderFrame @v x
             pure $ concatMap go (reverse xs) ++ f
           | otherwise ->
             concat <$> mapM (renderFrame @v) (reverse (x:xs))
    pure $ case frames of
        [] -> mempty
        _  -> foldr1 (P.<$>) frames
  where
    go :: NixFrame -> [Doc]
    go f = case framePos @v @m f of
        Just pos ->
            [text "While evaluating at "
                <> text (sourcePosPretty pos)
                <> colon]
        Nothing -> []

framePos :: forall v (m :: * -> *). (Typeable m, Typeable v) => NixFrame
         -> Maybe SourcePos
framePos (NixFrame _ f)
    | Just (e :: EvalFrame m v) <- fromFrame f = case e of
          EvaluatingExpr _ (Fix (Compose (Ann (SrcSpan beg _) _))) ->
              Just beg
          _ -> Nothing
    | otherwise = Nothing

renderFrame :: forall v e m.
              (MonadReader e m, Has e Options, MonadVar m,
               MonadFile m, Typeable m, Typeable v)
            => NixFrame -> m [Doc]
renderFrame (NixFrame level f)
    | Just (e :: EvalFrame m v) <- fromFrame f = renderEvalFrame level e
    | Just (e :: ThunkLoop)     <- fromFrame f = renderThunkLoop level e
    | Just (e :: ValueFrame m)  <- fromFrame f = renderValueFrame level e
    | Just (_ :: NormalLoop m)  <- fromFrame f =
      pure [text "<<loop during normalization>>"]
    | Just (e :: ExecFrame m)   <- fromFrame f = renderExecFrame level e
    | Just (e :: String)        <- fromFrame f = pure [text e]
    | Just (e :: Doc)           <- fromFrame f = pure [e]
    | otherwise = error $ "Unrecognized frame: " ++ show f

wrapExpr :: NExprF r -> NExpr
wrapExpr x = Fix (Fix (NSym "<?>") <$ x)

renderEvalFrame :: (MonadReader e m, Has e Options, MonadFile m)
                => NixLevel -> EvalFrame m v -> m [Doc]
renderEvalFrame level f = do
    opts :: Options <- asks (view hasLens)
    case f of
        EvaluatingExpr _scope e@(Fix (Compose (Ann ann _))) ->
            fmap (:[]) $ renderLocation ann
                =<< renderExpr level "While evaluating" "Expression" e

        ForcingExpr _scope e@(Fix (Compose (Ann ann _)))
            | thunks opts ->
                  fmap (:[]) $ renderLocation ann
                      =<< renderExpr level "While forcing thunk from"
                                     "Forcing thunk" e
        _ -> pure []

renderExpr :: (MonadReader e m, Has e Options, MonadFile m)
           => NixLevel -> String -> String -> NExprLoc -> m Doc
renderExpr _level longLabel shortLabel e@(Fix (Compose (Ann _ x))) = do
    opts :: Options <- asks (view hasLens)
    let rendered
            | verbose opts >= DebugInfo =
              text (PS.ppShow (stripAnnotation e))
            | verbose opts >= Chatty =
              prettyNix (stripAnnotation e)
            | otherwise =
              prettyNix (Fix (Fix (NSym "<?>") <$ x))
    pure $ if verbose opts >= Chatty
           then text (longLabel ++ ":\n>>>>>>>>")
                    P.<$> indent 2 rendered
                    P.<$> text "<<<<<<<<"
           else text shortLabel <> text ": " </> rendered

renderValueFrame :: (MonadReader e m, Has e Options, MonadFile m)
                 => NixLevel -> ValueFrame m -> m [Doc]
renderValueFrame level = pure . (:[]) . \case
    ForcingThunk       -> text "ForcingThunk"
    ConcerningValue _v -> text "ConcerningValue"
    Comparison _ _     -> text "Comparing"
    Addition _ _       -> text "Adding"
    Division _ _       -> text "Dividing"
    Multiplication _ _ -> text "Multiplying"

    Coercion x y ->
        text desc <> text (describeValue x)
            <> text " to " <> text (describeValue y)
      where
        desc | level <= Error = "Cannot coerce "
             | otherwise     = "While coercing "

    CoercionToJsonNF _v -> text "CoercionToJsonNF"
    CoercionFromJson _j -> text "CoercionFromJson"
    ExpectationNF _t _v -> text "ExpectationNF"
    Expectation _t _v   -> text "Expectation"

renderValue :: (MonadReader e m, Has e Options, MonadFile m, MonadVar m)
            => NixLevel -> String -> String -> NValue m -> m Doc
renderValue _level _longLabel _shortLabel v = do
    opts :: Options <- asks (view hasLens)
    if values opts
        then prettyNValueProv v
        else prettyNValue v

renderExecFrame :: (MonadReader e m, Has e Options, MonadVar m, MonadFile m)
                => NixLevel -> ExecFrame m -> m [Doc]
renderExecFrame level = \case
    Assertion ann v ->
        fmap (:[]) $ renderLocation ann
            =<< ((text "Assertion failed:" </>)
                     <$> renderValue level "" "" v)

renderThunkLoop :: (MonadReader e m, Has e Options, MonadFile m)
                => NixLevel -> ThunkLoop -> m [Doc]
renderThunkLoop _level = pure . (:[]) . \case
    ThunkLoop Nothing -> text "<<loop>>"
    ThunkLoop (Just n) ->
        text $ "<<loop forcing thunk #" ++ show n ++ ">>"
