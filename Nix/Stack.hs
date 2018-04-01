{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Stack where

import           Control.Monad.Reader
import qualified Data.ByteString as BS
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

renderLocation :: MonadIO m => SrcSpan -> Doc -> m Doc
renderLocation (SrcSpan beg@(Directed path _ _ _ _) end) msg = do
    contents <- liftIO $ BS.readFile (Text.unpack (Text.decodeUtf8 path))
    return $ explain (addSpan beg end (rendered beg contents))
                     (Err (Just msg) [] mempty [])
renderLocation (SrcSpan beg end) msg =
    return $ explain (addSpan beg end emptyRendering)
                     (Err (Just msg) [] mempty [])

renderFrame :: MonadIO m => Either String (NExprLocF ()) -> m String
renderFrame (Left str) = return str
renderFrame (Right (Compose (Ann ann expr))) =
    show <$> renderLocation ann
        (prettyNix (Fix (const (Fix (NSym "<?>")) <$> expr)))

throwError :: (Framed e m, MonadIO m) => String -> m a
throwError str = do
    context <- asks (reverse . view hasLens)
    infos   <- liftIO $ mapM renderFrame context
    error $ unlines (infos ++ ["hnix: "++ str])
