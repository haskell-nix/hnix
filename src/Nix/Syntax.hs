{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Syntax where

#ifdef MIN_VERSION_serialise
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Ser
#endif
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Binary (Binary)
import qualified Data.Binary as Bin
import           Data.Data
import           Data.Eq.Deriving
import           Data.Fix
import           Data.Functor.Classes
import           Data.Hashable
#if MIN_VERSION_hashable(1, 2, 5)
import           Data.Hashable.Lifted
#endif
import           Data.List (inits, tails)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Data.Ord.Deriving
import           Data.Text (Text, pack, unpack)
import           Data.Traversable
import           GHC.Exts
import           GHC.Generics
import           Language.Haskell.TH.Syntax
import           Lens.Family2
import           Lens.Family2.TH
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Atoms
import           Nix.Utils
import           Text.Megaparsec.Pos
import           Text.Read.Deriving
import           Text.Show.Deriving
#if MIN_VERSION_base(4, 10, 0)
import           Type.Reflection (eqTypeRep)
import qualified Type.Reflection as Reflection
#endif

-- | A data structure that preserves the exact syntax of a Nix file, as it
--   relates to the expressions parsed from within that file. This structure,
--   when rendered, must always reconstitute the exact input it was parsed
--   from.
--
--   This type is also a monoid, which can be used to "build up" synthesized
--   Nix files.
data NSyntaxF a r
  = NSynEmpty                   -- an empty source file
  | NSynGlue r r                -- glue two syntactic constructs together
  | NSynOrig Text r
    -- ^ Preserves exact original input. For example, 'NSynComment' only
    --   contains the text of the comment, not its placement or quantity of
    --   hash marks. This syntax is preserved by wrapping with 'NSynOrig'.
  | NSynLineComment Text        -- textual line comment
  | NSynBlockComment Text       -- textual block comment
  | NSynSpace Text              -- just whitespace
  | NSynSyntax Text             -- syntax without a corresponding expression
  | NSynExpr a                  -- the expression that was parsed here
  | NSynBracket r r r
    -- ^ When a constructed is "bracketed", it has a prolog, content, and
    --   epilog --where either prolog or epilog might be empty. For example:
    --
    --   @@
    --     { inherit foo; }
    --   @@
    --
    --   This could be rendered as:
    --
    --   @@
    --     NSynBracket
    --       (NSynGlue (NSynSyntax "{") (NSynSpace " "))
    --       (NSynBracket
    --         (NSynGlue (NSynSyntax "inherit") (NSynSpace " "))
    --         (NSynSyntax "foo")
    --         (NSynGlue (NSynSyntax ";")))
    --       (NSynGlue (NSynSpace " ") (NSynSyntax "}"))
    --   @@
    --
    --   Many equivalent representations are possible, so long as they all
    --   render to the same source text.
  deriving (Ord, Eq, Generic, Generic1, Typeable, Data, Functor,
            Foldable, Traversable, Show, NFData, Hashable)

newtype NExprSynF f = NExprSynF { runNExprSyn :: Fix (NSyntaxF (f (NExprSynF f))) }
  deriving (Generic, Typeable)

deriving instance Ord (f (NExprSynF f)) => Ord (NExprSynF f)
deriving instance Eq (f (NExprSynF f)) => Eq (NExprSynF f)
deriving instance (Data (f (NExprSynF f)), Typeable f) => Data (NExprSynF f)
deriving instance Show (f (NExprSynF f)) => Show (NExprSynF f)

type NExprSyn = NExprSynF (AnnF SrcSpan NExprF)

instance Semigroup NExprSyn where
    NExprSynF x <> NExprSynF y = NExprSynF (Fix (NSynGlue x y))

instance Monoid NExprSyn where
    mempty = NExprSynF (Fix NSynEmpty)
    mappend = (<>)

class Syntactic expr where
    type WhiteSpace expr :: *
    type Syntax expr :: *

    synWhiteSpace :: Text -> WhiteSpace expr
    synLineComment :: Text -> WhiteSpace expr
    synBlockComment :: Text -> WhiteSpace expr
    synSyntax :: Text -> Syntax expr
    synExpr :: SrcSpan -> NExprF expr -> expr
    synWithSuffix :: expr -> WhiteSpace expr -> expr

instance Syntactic NExprSyn where
    type WhiteSpace NExprSyn = NExprSyn
    type Syntax NExprSyn = NExprSyn

    synWhiteSpace s = NExprSynF (Fix (NSynSpace s))
    -- jww (2019-03-09): Strip out the comment leaders from 's'
    synLineComment s =
        NExprSynF (Fix (NSynOrig s (Fix (NSynLineComment s))))
    synBlockComment s =
        NExprSynF (Fix (NSynOrig s (Fix (NSynBlockComment s))))
    synSyntax s = NExprSynF (Fix (NSynSyntax s))
    synExpr loc e = NExprSynF (Fix (NSynExpr (Compose (Ann loc e))))
    synWithSuffix = (<>)

instance Syntactic NExprLoc where
    type WhiteSpace NExprLoc = ()
    type Syntax NExprLoc = Text

    synWhiteSpace _ = ()
    synLineComment _ = ()
    synBlockComment _ = ()
    synExpr loc e = Fix (Compose (Ann loc e))
    synSyntax = id
    synWithSuffix e _ = e
