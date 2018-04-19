{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Entry where

import Control.Applicative (Alternative)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Nix.Effects (MonadEffects)
import Nix.Expr.Types (NExpr)
import Nix.Expr.Types.Annotated (NExprLoc)
import Nix.Scope (Scoped)
import Nix.Stack (Framed, MonadFile)
import Nix.Thunk
import Nix.Value

type MonadNix e m =
    (Scoped e (NThunk m) m, Framed e m, MonadVar m, MonadFile m,
     MonadEffects m, MonadFix m, MonadCatch m)

evalTopLevelExprGen
    :: forall e m a r. MonadNix e m
    => (a -> m r) -> Maybe FilePath -> a -> m r

eval :: forall e m. MonadNix e m
     => Maybe FilePath -> NExpr -> m (NValue m)

evalLoc :: forall e m. MonadNix e m
        => Maybe FilePath -> NExprLoc -> m (NValue m)

tracingEvalLoc
    :: forall e m. (MonadNix e m, Alternative m, MonadIO m)
    => Maybe FilePath -> NExprLoc -> m (NValue m)
