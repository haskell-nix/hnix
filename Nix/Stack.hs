{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Stack where

import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Fix
import           Data.Functor.Compose
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Pretty
import           Nix.Utils
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import           Text.Trifecta.Rendering
import           Text.Trifecta.Result

type Frames = [Either String (NExprLocF ())]

type Framed e m = (MonadReader e m, Has e Frames)

withExprContext :: Framed e m => NExprLocF () -> m r -> m r
withExprContext expr = local (over hasLens (Right @String expr :))

withStringContext :: Framed e m => String -> m r -> m r
withStringContext str = local (over hasLens (Left @_ @(NExprLocF ()) str :))

class Monad m => MonadFile m where
    readFile :: FilePath -> m ByteString

renderLocation :: MonadFile m => SrcSpan -> Doc -> m Doc
renderLocation (SrcSpan beg@(Directed "<string>" _ _ _ _) end) msg =
    return $ explain (addSpan beg end emptyRendering)
                     (Err (Just msg) [] mempty [])
renderLocation (SrcSpan beg@(Directed path _ _ _ _) end) msg = do
    contents <- Nix.Stack.readFile (Text.unpack (Text.decodeUtf8 path))
    return $ explain (addSpan beg end (rendered beg contents))
                     (Err (Just msg) [] mempty [])
renderLocation (SrcSpan beg end) msg =
    return $ explain (addSpan beg end emptyRendering)
                     (Err (Just msg) [] mempty [])

renderFrame :: MonadFile m => Either String (NExprLocF ()) -> m String
renderFrame (Left str) = return str
renderFrame (Right (Compose (Ann ann expr))) =
    show <$> renderLocation ann
        (prettyNix (Fix (const (Fix (NSym "<?>")) <$> expr)))

throwError :: (Framed e m, MonadFile m) => String -> m a
throwError str = do
    context <- asks (reverse . view hasLens)
    infos   <- mapM renderFrame context
    errorWithoutStackTrace $ unlines (infos ++ ["hnix: "++ str])
