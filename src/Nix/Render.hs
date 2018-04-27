{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nix.Render where

import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Set as Set
import           Data.Void
import           Nix.Expr.Types.Annotated
import           Nix.Parser.Library

class Monad m => MonadFile m where
    readFile :: FilePath -> m ByteString

posAndMsg :: SourcePos -> Doc -> ParseError t Void
posAndMsg beg msg =
    FancyError (beg :| [])
        (Set.fromList [ErrorFail (show msg) :: ErrorFancy Void])

renderLocation :: MonadFile m => SrcSpan -> Doc -> m Doc
renderLocation (SrcSpan beg@(SourcePos "<string>" _ _) _) msg =
    return $ text $ init $ parseErrorPretty @Char (posAndMsg beg msg)

renderLocation (SrcSpan beg@(SourcePos path _ _) _) msg = do
    contents <- Nix.Render.readFile path
    return $ text $ init $ parseErrorPretty' contents (posAndMsg beg msg)

{-
-}

{-
throwError :: (Framed e m r, MonadFile m, MonadThrow m) => String -> m a
throwError str = do
    opts :: Options <- asks (view hasLens)
    context <- asks (reverse . view hasLens)
    infos   <- case context of
        [] -> return []
        _ | verbose opts >= Talkative ->
            mapM renderFrame $
                filter noAsserts (init context) ++ [last context]
          | otherwise ->
            return []
    traceM "throwing error"
    throwM $ NixEvalException $ intercalate "\n" $ infos ++ [str]
  where
    noAsserts (Right (Fix (Compose (Ann _ (NAssert _ _))))) = False
    noAsserts _ = True

    justPos (Left _) = []
    justPos (Right (Fix (Compose (Ann (SrcSpan beg _) _)))) = [beg]
-}

{-
renderNormalError :: Eff (NormalError ': r) a
                  -> Eff r a
renderNormalError = interpret $ \case
    NormalLoop _ -> pure $ text "<<loop during normalization>>"

renderThunkError :: Eff (ThunkError ': r) a
                 -> Eff r a
renderThunkError = interpret $ \case
    ThunkLoop Nothing  -> pure $ text "<<loop>>"
    ThunkLoop (Just n) ->
        pure $ text $ "<<loop forcing thunk #" ++ show n ++ ">>"
-}

{-
renderEvalFrame :: NixLevel
                 -> Eff (EvalFrame ': r) a
                 -> Eff r a
renderEvalFrame lvl = interpret $ \case
    EvalutingExpr x y ->
        pure $ text "While evaluating <> describeValue x
            <> text " to " <> describeValue y
      where
        desc | lvl <= Error = "Cannot coerce "
             | otherwise   = "While coercing "
-}

{-
renderValueFrame :: NixLevel
                 -> Eff (ValueFrame ': r) a
                 -> Eff r a
renderValueFrame lvl = interpret $ \case
    Coercion x y ->
        pure $ text desc <> describeValue x
            <> text " to " <> describeValue y
      where
        desc | lvl <= Error = "Cannot coerce "
             | otherwise   = "While coercing "
-}
