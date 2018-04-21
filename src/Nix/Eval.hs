{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Eval where

import           Data.Functor.Compose
import           Nix.Core (MonadNixEval)
import qualified Nix.Core as Core
import           Nix.Expr.Types.Annotated
import           Nix.Stack
import           Nix.Utils

addStackFrames :: Framed e m => Transform NExprLocF (m a)
addStackFrames f v = withExprContext v (f v)

framedEvalExpr :: MonadNixEval e v t m => NExprLoc -> m v
framedEvalExpr = adi (Core.eval . annotated . getCompose) addStackFrames
