{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | The nix expression type and supporting types.
module Nix.Expr where

import           Control.Monad hiding (forM_, mapM, sequence)
import           Data.Data
import           Data.Fix
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Text (Text, pack)
import           Data.Traversable
import           GHC.Exts
import           GHC.Generics
import           Prelude hiding (readFile, concat, concatMap, elem, mapM,
                                 sequence, minimum, foldr)

-- | Atoms are values that evaluate to themselves. This means that
-- they appear in both the parsed AST (in the form of literals) and
-- the evaluated form.
data NAtom
  -- | An integer. The c nix implementation currently only supports
  -- integers that fit in the range of 'Int64'.
  = NInt Integer
  -- | Booleans.
  | NBool Bool
  -- | Null values. There's only one of this variant.
  | NNull
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

data NSetBind = Rec | NonRec
  deriving (Ord, Eq, Generic, Typeable, Data, Show)

-- | A single line of the bindings section of a let expression or of
-- a set.
data Binding r
  = NamedVar (NAttrPath r) r
  | Inherit (Maybe r) [NAttrPath r]
  deriving (Typeable, Data, Ord, Eq, Functor, Show)

-- | For functions which are called with a set as an argument.
data FormalParamSet r
  = FixedParamSet (Map Text (Maybe r))
    -- ^ Parameters for a function that expects an attribute set. The values
  -- are @Just@ if they specify a default argument. For a fixed set, no
  -- arguments beyond what is specified in the map may be given.
  | VariadicParamSet (Map Text (Maybe r))
  -- ^ Same as the 'FixedParamSet', but extra arguments are allowed.
  deriving (Eq, Ord, Generic, Typeable, Data, Functor, Show,
            Foldable, Traversable)

-- | @Formals@ represents all the ways the formal parameters to a
-- function can be represented.
data Formals r
  = FormalName Text
  -- ^ For functions with a single named argument, such as @x: x + 1@.
  | FormalSet (FormalParamSet r) (Maybe Text)
  -- ^ For functions that expect an attribute set argument, and unpack values
  -- from it. For example, @{x, y}: x + y@.
  deriving (Ord, Eq, Generic, Typeable, Data, Functor, Show,
            Foldable, Traversable)

-- | For the two different kinds of strings.
data StringKind = DoubleQuoted | Indented
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

-- | An 'NString' is a list of things that are either a plain string
-- or an antiquoted expression. After the antiquotes have been evaluated,
-- the final string is constructed by concating all the parts.
data NString r = NString StringKind [Antiquoted Text r] | NUri Text
  deriving (Eq, Ord, Generic, Typeable, Data, Functor, Show)

-- | A 'KeyName' is something that can appear at the right side of an
-- equals sign.
-- For example, @a@ is a 'KeyName' in @{ a = 3; }@, @let a = 3;
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
  = DynamicKey (Antiquoted (NString r) r)
  | StaticKey Text
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

-- | Deriving this instance automatically is not possible because @r@
-- occurs not only as last argument in @Antiquoted (NString r) r@
instance Functor NKeyName where
  fmap f (DynamicKey (Plain str)) = DynamicKey . Plain $ fmap f str
  fmap f (DynamicKey (Antiquoted e)) = DynamicKey . Antiquoted $ f e
  fmap _ (StaticKey key) = StaticKey key

-- | A selector (for example in a @let@ or an attribute set) is made up
-- of strung-together key names.
type NAttrPath r = [NKeyName r]

-- | A functor-ized nix expression type, which lets us do things like traverse
-- expressions and map functions over them. The actual 'NExpr' type is defined
-- below.
data NExprF r
  = NConstant NAtom
  -- ^ Constants: ints, bools, and null.
  | NStr (NString r)
  -- ^ A string, with interpolated expressions.
  | NList [r]
  -- ^ A list literal.
  | NSet NSetBind [Binding r]
  -- ^ An attribute set literal, possibly recursive.
  | NAbs (Formals r) r
  -- ^ A lambda abstraction.
  | NPath Bool FilePath
  -- ^ A path expression, which is evaluated to a store path. The boolean
  -- argument of 'NPath' is 'True' if the path refers to something in the
  -- Nix search path. For example, @<nixpkgs/pkgs>@ is represented by
  -- @NPath True "nixpkgs/pkgs"@, while @foo/bar@ is represented by @NPath
  -- False "foo/bar@.
  | NOper (NOperF r)
  -- ^ Binary or unary operators.
  | NSelect r (NAttrPath r) (Maybe r)
  -- ^ Dot-reference into an attribute set, optionally providing an
  -- alternative if the key doesn't exist.
  | NHasAttr r (NAttrPath r)
  -- ^ Ask if a set contains a given attribute path.
  | NApp r r
  -- ^ Apply a function to an argument.
  | NSym Text
  -- ^ A variable. For example, in the expression @f a@, @f@ is represented
  -- as @NSym "f"@ and @a@ as @NSym "a"@.
  | NLet [Binding r] r
  -- ^ Evaluate the second argument after introducing the bindings.
  | NIf r r r
  -- ^ If-then-else statement.
  | NWith r r
  -- ^ Evaluate an attribute set, bring its bindings into scope, and
  -- evaluate the second argument.
  | NAssert r r
  -- ^ Assert that the first returns true before evaluating the second.
  deriving (Ord, Eq, Generic, Typeable, Data, Functor, Show)

type NExpr = Fix NExprF

-- | 'Antiquoted' represents an expression that is either
-- antiquoted (surrounded by ${...}) or plain (not antiquoted).
data Antiquoted v r = Plain v | Antiquoted r
  deriving (Ord, Eq, Generic, Typeable, Data, Functor, Show)

-- | For the the 'IsString' instance, we use a plain doublequoted string.
instance IsString (NString r) where
  fromString "" = NString DoubleQuoted []
  fromString x = NString DoubleQuoted . (:[]) . Plain . pack $ x

-- | Operator expressions are unary or binary.
data NOperF r
  = NUnary NUnaryOp r
  | NBinary NBinaryOp r r
  deriving (Eq, Ord, Generic, Typeable, Data, Functor, Show)

-- | There are two unary operations: logical not and integer negation.
data NUnaryOp = NNeg | NNot
  deriving (Eq, Ord, Generic, Typeable, Data, Show)

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

mkInt :: Integer -> NExpr
mkInt = Fix . NConstant . NInt

mkStr :: StringKind -> Text -> NExpr
mkStr kind x = Fix . NStr . NString kind $ if x == ""
  then []
  else [Plain x]

mkUri :: Text -> NExpr
mkUri = Fix . NStr . NUri

mkPath :: Bool -> FilePath -> NExpr
mkPath b = Fix . NPath b

-- | Make a path expression which pulls from the NIX_PATH env variable.
mkEnvPath :: FilePath -> NExpr
mkEnvPath = mkPath True

-- | Make a path expression which references a relative path.
mkRelPath :: FilePath -> NExpr
mkRelPath = mkPath False

mkSym :: Text -> NExpr
mkSym = Fix . NSym

mkSelector :: Text -> NAttrPath NExpr
mkSelector = (:[]) . StaticKey

mkBool :: Bool -> NExpr
mkBool = Fix . NConstant . NBool

mkNull :: NExpr
mkNull = Fix (NConstant NNull)

mkOper :: NUnaryOp -> NExpr -> NExpr
mkOper op = Fix . NOper . NUnary op

mkOper2 :: NBinaryOp -> NExpr -> NExpr -> NExpr
mkOper2 op a = Fix . NOper . NBinary op a

mkFormalSet :: [(Text, Maybe NExpr)] -> Formals NExpr
mkFormalSet = mkFixedParamSet

mkFixedParamSet :: [(Text, Maybe NExpr)] -> Formals NExpr
mkFixedParamSet ps = FormalSet (FixedParamSet $ Map.fromList ps) Nothing

mkVariadicParamSet :: [(Text, Maybe NExpr)] -> Formals NExpr
mkVariadicParamSet ps = FormalSet (VariadicParamSet $ Map.fromList ps) Nothing

mkApp :: NExpr -> NExpr -> NExpr
mkApp e = Fix . NApp e

mkRecSet :: [Binding NExpr] -> NExpr
mkRecSet = Fix . NSet Rec

mkNonRecSet :: [Binding NExpr] -> NExpr
mkNonRecSet = Fix . NSet NonRec

mkLet :: [Binding NExpr] -> NExpr -> NExpr
mkLet bs = Fix . NLet bs

mkList :: [NExpr] -> NExpr
mkList = Fix . NList

mkWith :: NExpr -> NExpr -> NExpr
mkWith e = Fix . NWith e

mkAssert :: NExpr -> NExpr -> NExpr
mkAssert e = Fix . NWith e

mkIf :: NExpr -> NExpr -> NExpr -> NExpr
mkIf e1 e2 = Fix . NIf e1 e2

mkFunction :: Formals NExpr -> NExpr -> NExpr
mkFunction params = Fix . NAbs params

-- | Shorthand for producing a binding of a name to an expression.
bindTo :: Text -> NExpr -> Binding NExpr
bindTo name val = NamedVar (mkSelector name) val

-- | Append a list of bindings to a set or let expression.
-- For example, adding `[a = 1, b = 2]` to `let c = 3; in 4` produces
-- `let a = 1; b = 2; c = 3; in 4`.
appendBindings :: [Binding NExpr] -> NExpr -> NExpr
appendBindings newBindings (Fix e) = case e of
  NLet bindings e' -> Fix $ NLet (bindings <> newBindings) e'
  NSet bindType bindings -> Fix $ NSet bindType (bindings <> newBindings)
  _ -> error "Can only append bindings to a set or a let"

-- | Applies a transformation to the body of a nix function.
modifyFunctionBody :: (NExpr -> NExpr) -> NExpr -> NExpr
modifyFunctionBody f (Fix e) = case e of
  NAbs params body -> Fix $ NAbs params (f body)
  _ -> error "Not a function"

