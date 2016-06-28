{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | The nix expression type and supporting types.
module Nix.Expr.Types where

import           Control.Monad hiding (forM_, mapM, sequence)
import           Data.Data
import           Data.Fix
import           Data.Foldable
import           Data.Functor.Classes (Show1(..))
import           Data.Map (Map)
import           Data.Text (Text, pack)
import           Data.Traversable
import           GHC.Exts
import           GHC.Generics
import           Nix.Atoms
import           Prelude hiding (readFile, concat, concatMap, elem, mapM,
                                 sequence, minimum, foldr)

-- | The main nix expression type. This is polymorphic so that it can be made
-- a functor, which allows us to traverse expressions and map functions over
-- them. The actual 'NExpr' type is a fixed point of this functor, defined
-- below.
data NExprF r
  = NConstant !NAtom
  -- ^ Constants: ints, bools, URIs, and null.
  | NStr !(NString r)
  -- ^ A string, with interpolated expressions.
  | NSym !Text
  -- ^ A variable. For example, in the expression @f a@, @f@ is represented
  -- as @NSym "f"@ and @a@ as @NSym "a"@.
  | NList ![r]
  -- ^ A list literal.
  | NSet ![Binding r]
  -- ^ An attribute set literal, not recursive.
  | NRecSet ![Binding r]
  -- ^ An attribute set literal, recursive.
  | NLiteralPath !FilePath
  -- ^ A path expression, which is evaluated to a store path. The path here
  -- can be relative, in which case it's evaluated relative to the file in
  -- which it appears.
  | NEnvPath !FilePath
  -- ^ A path which refers to something in the Nix search path (the NIX_PATH
  -- environment variable. For example, @<nixpkgs/pkgs>@.
  | NUnary !NUnaryOp !r
  -- ^ Application of a unary operator to an expression.
  | NBinary !NBinaryOp !r !r
  -- ^ Application of a binary operator to two expressions.
  | NSelect !r !(NAttrPath r) !(Maybe r)
  -- ^ Dot-reference into an attribute set, optionally providing an
  -- alternative if the key doesn't exist.
  | NHasAttr !r !(NAttrPath r)
  -- ^ Ask if a set contains a given attribute path.
  | NAbs !(Params r) !r
  -- ^ A function literal (lambda abstraction).
  | NApp !r !r
  -- ^ Apply a function to an argument.
  | NLet ![Binding r] !r
  -- ^ Evaluate the second argument after introducing the bindings.
  | NIf !r !r !r
  -- ^ If-then-else statement.
  | NWith !r !r
  -- ^ Evaluate an attribute set, bring its bindings into scope, and
  -- evaluate the second argument.
  | NAssert !r !r
  -- ^ Assert that the first returns true before evaluating the second.
  deriving (Ord, Eq, Generic, Typeable, Data, Functor, Show)

instance Show1 NExprF where
  showsPrec1 = showsPrec

-- | We make an `IsString` for expressions, where the string is interpreted
-- as an identifier. This is the most common use-case...
instance IsString NExpr where
  fromString = Fix . NSym . fromString

-- | The monomorphic expression type is a fixed point of the polymorphic one.
type NExpr = Fix NExprF

-- | A single line of the bindings section of a let expression or of a set.
data Binding r
  = NamedVar !(NAttrPath r) !r
  -- ^ An explicit naming, such as @x = y@ or @x.y = z@.
  | Inherit !(Maybe r) ![NKeyName r]
  -- ^ Using a name already in scope, such as @inherit x;@ which is shorthand
  -- for @x = x;@ or @inherit (x) y;@ which means @y = x.y;@.
  deriving (Typeable, Data, Ord, Eq, Functor, Show)

-- | @Params@ represents all the ways the formal parameters to a
-- function can be represented.
data Params r
  = Param !Text
  -- ^ For functions with a single named argument, such as @x: x + 1@.
  | ParamSet !(ParamSet r) !(Maybe Text)
  -- ^ Explicit parameters (argument must be a set). Might specify a name
  -- to bind to the set in the function body.
  deriving (Ord, Eq, Generic, Typeable, Data, Functor, Show,
            Foldable, Traversable)

instance IsString (Params r) where
  fromString = Param . fromString

-- | An explicit parameter set; provides a shorthand for unpacking arguments.
data ParamSet r
  = FixedParamSet !(Map Text (Maybe r))
  -- ^ A fixed set, where no arguments beyond what is specified in the map
  -- may be given. The map might contain defaults for arguments not passed.
  | VariadicParamSet !(Map Text (Maybe r))
  -- ^ Same as the 'FixedParamSet', but extra arguments are allowed.
  deriving (Ord, Eq, Generic, Typeable, Data, Functor, Show,
            Foldable, Traversable)

-- | 'Antiquoted' represents an expression that is either
-- antiquoted (surrounded by ${...}) or plain (not antiquoted).
data Antiquoted v r = Plain !v | Antiquoted !r
  deriving (Ord, Eq, Generic, Typeable, Data, Functor, Show)

-- | An 'NString' is a list of things that are either a plain string
-- or an antiquoted expression. After the antiquotes have been evaluated,
-- the final string is constructed by concating all the parts.
data NString r
  = DoubleQuoted ![Antiquoted Text r]
  -- ^ Strings wrapped with double-quotes (") are not allowed to contain
  -- literal newline characters.
  | Indented ![Antiquoted Text r]
  -- ^ Strings wrapped with two single quotes ('') can contain newlines,
  -- and their indentation will be stripped.
  deriving (Eq, Ord, Generic, Typeable, Data, Functor, Show)

-- | For the the 'IsString' instance, we use a plain doublequoted string.
instance IsString (NString r) where
  fromString "" = DoubleQuoted []
  fromString string = DoubleQuoted [Plain $ pack string]

-- | A 'KeyName' is something that can appear at the right side of an
-- equals sign. For example, @a@ is a 'KeyName' in @{ a = 3; }@, @let a = 3;
-- in ...@, @{}.a@ or @{} ? a@.
--
-- Nix supports both static keynames (just an identifier) and dynamic
-- identifiers. Dynamic identifiers can be either a string (e.g.:
-- @{ "a" = 3; }@) or an antiquotation (e.g.: @let a = "example";
-- in { ${a} = 3; }.example@).
--
-- Note: There are some places where a dynamic keyname is not allowed.
-- In particular, those include:
--
--   * The RHS of a @binding@ inside @let@: @let ${"a"} = 3; in ...@
--     produces a syntax error.
--   * The attribute names of an 'inherit': @inherit ${"a"};@ is forbidden.
--
-- Note: In Nix, a simple string without antiquotes such as @"foo"@ is
-- allowed even if the context requires a static keyname, but the
-- parser still considers it a 'DynamicKey' for simplicity.
data NKeyName r
  = DynamicKey !(Antiquoted (NString r) r)
  | StaticKey !Text
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

-- | Most key names are just static text, so this instance is convenient.
instance IsString (NKeyName r) where
  fromString = StaticKey . fromString

-- | Deriving this instance automatically is not possible because @r@
-- occurs not only as last argument in @Antiquoted (NString r) r@
instance Functor NKeyName where
  fmap f (DynamicKey (Plain str)) = DynamicKey . Plain $ fmap f str
  fmap f (DynamicKey (Antiquoted e)) = DynamicKey . Antiquoted $ f e
  fmap _ (StaticKey key) = StaticKey key

-- | A selector (for example in a @let@ or an attribute set) is made up
-- of strung-together key names.
type NAttrPath r = [NKeyName r]

-- | There are two unary operations: logical not and integer negation.
data NUnaryOp = NNeg | NNot
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

-- | Binary operators expressible in the nix language.
data NBinaryOp
  = NEq -- ^ Equality (==)
  | NNEq -- ^ Inequality (!=)
  | NLt -- ^ Less than (<)
  | NLte -- ^ Less than or equal (<=)
  | NGt -- ^ Greater than (>)
  | NGte -- ^ Greater than or equal (>=)
  | NAnd -- ^ Logical and (&&)
  | NOr -- ^ Logical or (||)
  | NImpl -- ^ Logical implication (->)
  | NUpdate -- ^ Joining two attribut sets (//)
  | NPlus -- ^ Addition (+)
  | NMinus -- ^ Subtraction (-)
  | NMult -- ^ Multiplication (*)
  | NDiv -- ^ Division (/)
  | NConcat -- ^ List concatenation (++)
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

-- | Get the name out of the parameter (there might be none).
paramName :: Params r -> Maybe Text
paramName (Param n) = Just n
paramName (ParamSet _ n) = n
