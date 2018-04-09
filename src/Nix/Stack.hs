{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Stack where

import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Fix
import           Data.Functor.Compose
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Parser.Library
import           Nix.Pretty
import           Nix.Utils
#ifndef USE_PARSEC
import           Text.Trifecta.Rendering
import           Text.Trifecta.Result
#endif

data NixException = NixEvalException String
    deriving Show

instance Exception NixException

type Frames = [Either String (NExprLocF ())]

type Framed e m = (MonadReader e m, Has e Frames, MonadThrow m)

withExprContext :: Framed e m => NExprLocF () -> m r -> m r
withExprContext expr = local (over hasLens (Right @String expr :))

withStringContext :: Framed e m => String -> m r -> m r
withStringContext str = local (over hasLens (Left @_ @(NExprLocF ()) str :))

class Monad m => MonadFile m where
    readFile :: FilePath -> m ByteString

renderLocation :: MonadFile m => SrcSpan -> Doc -> m Doc
#if USE_PARSEC
renderLocation _ _ = error "renderLocation: No implementation yet for parsec"
#else
renderLocation (SrcSpan beg@(Directed "<string>" _ _ _ _) end) msg =
    return $ explain (addSpan (deltaToTrifecta beg) (deltaToTrifecta end)
                              emptyRendering)
                     (Err (Just msg) [] mempty [])
renderLocation (SrcSpan beg@(Directed path _ _ _ _) end) msg = do
    contents <- Nix.Stack.readFile path
    return $ explain (addSpan (deltaToTrifecta beg) (deltaToTrifecta end)
                              (rendered (deltaToTrifecta beg) contents))
                     (Err (Just msg) [] mempty [])
renderLocation (SrcSpan beg end) msg =
    return $ explain (addSpan (deltaToTrifecta beg) (deltaToTrifecta end)
                              emptyRendering)
                     (Err (Just msg) [] mempty [])
#endif

renderFrame :: MonadFile m => Either String (NExprLocF ()) -> m String
renderFrame (Left str) = return str
renderFrame (Right (Compose (Ann ann expr))) =
    show <$> renderLocation ann
        (prettyNix (Fix (Fix (NSym "<?>") <$ expr)))

throwError :: (Framed e m, MonadFile m, MonadThrow m) => String -> m a
throwError str = do
    context <- asks (reverse . view hasLens)
    infos   <- mapM renderFrame context
    throwM $ NixEvalException $ unlines $ infos ++ [str]
