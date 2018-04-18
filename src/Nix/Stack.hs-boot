{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Nix.Stack where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Nix.Expr.Types.Annotated
import Nix.Options
import Nix.Utils
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

type Frames = [Either String NExprLoc]

type Framed e m = (MonadReader e m, Has e Frames, Has e Options, MonadThrow m)

withExprContext :: Framed e m => NExprLoc -> m r -> m r

withStringContext :: Framed e m => String -> m r -> m r

class Monad m => MonadFile m where
    readFile :: FilePath -> m ByteString

renderLocation :: (Framed e m, MonadFile m) => SrcSpan -> Doc -> m Doc

renderFrame :: (Framed e m, MonadFile m) => Either String NExprLoc -> m String

throwError :: (Framed e m, MonadFile m, MonadThrow m) => String -> m a
