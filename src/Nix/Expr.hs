-- | Wraps the expression submodules.
module Nix.Expr (
  module Nix.Expr.Types,
  module Nix.Expr.Types.Annotated,
  module Nix.Expr.Shorthands
  ) where

import Nix.Expr.Types
import Nix.Expr.Shorthands
import Nix.Expr.Types.Annotated
