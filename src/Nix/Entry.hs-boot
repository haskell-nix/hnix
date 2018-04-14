{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Entry where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Fix (MonadFix)
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
    :: forall e m a. MonadNix e m
    => (a -> m (NValue m)) -> Maybe FilePath -> [String] -> a
    -> m (NValueNF m)

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: forall e m. MonadNix e m
                 => Maybe FilePath -> [String] -> NExpr -> m (NValueNF m)

evalTopLevelExprLoc :: forall e m. MonadNix e m
                    => Maybe FilePath -> [String] -> NExprLoc -> m (NValueNF m)
