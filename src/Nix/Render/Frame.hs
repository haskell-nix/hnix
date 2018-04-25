{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nix.Render.Frame where

import           Control.Monad.Reader
import           Data.Fix
import           Data.Functor.Compose
import           Data.Typeable
import           Nix.Eval
import           Nix.Exec
import           Nix.Expr
import           Nix.Frames
import           Nix.Normal
import           Nix.Options
import           Nix.Parser.Library
import           Nix.Pretty
import           Nix.Render
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value
import qualified Text.PrettyPrint.ANSI.Leijen as P
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

renderFrames :: (MonadReader e m, Has e Options,
                MonadVar m, MonadFile m, Typeable m)
             => Frames -> m Doc
renderFrames [] = pure mempty
renderFrames xs = fmap (foldr1 (P.<$>)) $ mapM renderFrame $ reverse xs

renderFrame :: forall e m. (MonadReader e m, Has e Options, MonadVar m,
                      MonadFile m, Typeable m)
            => NixFrame -> m Doc
renderFrame (NixFrame level f)
    | Just (e :: EvalFrame)    <- fromFrame f = renderEvalFrame level e
    | Just (e :: ThunkLoop)    <- fromFrame f = renderThunkLoop level e
    | Just (e :: ValueFrame m) <- fromFrame f = renderValueFrame level e
    | Just (_ :: NormalLoop m) <- fromFrame f =
      pure $ text "<<loop during normalization>>"
    | Just (e :: ExecFrame m)  <- fromFrame f = renderExecFrame level e
    | Just (e :: String)       <- fromFrame f = pure $ text e
    | Just (e :: Doc)          <- fromFrame f = pure e
    | otherwise = error $ "Unrecognized frame: " ++ show f

wrapExpr :: NExprF r -> NExpr
wrapExpr x = Fix (Fix (NSym "<?>") <$ x)

renderEvalFrame :: (MonadReader e m, Has e Options, MonadFile m)
                => NixLevel -> EvalFrame -> m Doc
renderEvalFrame _level = \case
    ExprContext e ->
        pure $ text "While forcing thunk for: " </> prettyNix (wrapExpr e)

    EvaluatingExpr e@(Fix (Compose (Ann ann x))) -> do
        opts :: Options <- asks (view hasLens)
        let rendered = prettyNix $
                if verbose opts >= Chatty
                then stripAnnotation e
                else Fix (Fix (NSym "<?>") <$ x)
            msg = if verbose opts >= Chatty
                  then text "While evaluating:\n>>>>>>>>"
                           P.<$> indent 2 rendered
                           P.<$> text "<<<<<<<<"
                  else "Expression: " </> rendered
        renderLocation ann msg

renderValueFrame :: (MonadReader e m, Has e Options, MonadFile m)
                 => NixLevel -> ValueFrame m -> m Doc
renderValueFrame level = \case
    ForcingThunk       -> pure $ text "ForcingThunk"
    ConcerningValue _v -> pure $ text "ConcerningValue"

    Coercion x y ->
        pure $ text desc <> text (describeValue x)
            <> text " to " <> text (describeValue y)
      where
        desc | level <= Error = "Cannot coerce "
             | otherwise     = "While coercing "

    CoercionToJsonNF _v -> pure $ text "CoercionToJsonNF"
    CoercionFromJson _j -> pure $ text "CoercionFromJson"
    ExpectationNF _t _v -> pure $ text "ExpectationNF"
    Expectation _t _v   -> pure $ text "Expectation"

renderExecFrame :: (MonadReader e m, Has e Options, MonadVar m, MonadFile m)
                => NixLevel -> ExecFrame m -> m Doc
renderExecFrame _level = \case
    Assertion v ->
        -- jww (2018-04-24): Render values nicely based on the verbosity.
        (text "Assertion failed:" </>) <$> renderNValue v

renderThunkLoop :: (MonadReader e m, Has e Options, MonadFile m)
                => NixLevel -> ThunkLoop -> m Doc
renderThunkLoop _level = \case
    ThunkLoop Nothing -> pure $ text "<<loop>>"
    ThunkLoop (Just n) ->
        pure $ text $ "<<loop forcing thunk #" ++ show n ++ ">>"
