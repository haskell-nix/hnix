{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Nix.Stack where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import Nix.Expr.Types.Annotated
import Nix.Utils
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

type Frames = [Either String (NExprLocF ())]

type Framed e m = (MonadReader e m, Has e Frames)

withExprContext :: Framed e m => NExprLocF () -> m r -> m r

withStringContext :: Framed e m => String -> m r -> m r

class Monad m => MonadFile m where
    readFile :: FilePath -> m ByteString

renderLocation :: MonadFile m => SrcSpan -> Doc -> m Doc

renderFrame :: MonadFile m => Either String (NExprLocF ()) -> m String

throwError :: (Framed e m, MonadFile m) => String -> m a
