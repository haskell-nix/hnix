{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | The nix expression type and supporting types.
module Nix.Expr.Types where

import           Control.DeepSeq
import           Data.Binary
import           Data.Data
import           Data.Eq.Deriving
import           Data.Fix
import           Data.Functor.Classes
import           Data.Text (Text, pack, unpack)
import           Data.Traversable
import           GHC.Exts
import           GHC.Generics
import           Language.Haskell.TH.Syntax
import           Nix.Atoms
import           Nix.Parser.Library (SourcePos(..))
import           Nix.Utils
import           Text.Show.Deriving
import           Text.Megaparsec.Pos
import           Type.Reflection (eqTypeRep)
import qualified Type.Reflection as Reflection

type VarName = Text

-- | The main nix expression type. This is polymorphic so that it can be made
-- a functor, which allows us to traverse expressions and map functions over
-- them. The actual 'NExpr' type is a fixed point of this functor, defined
-- below.
data NExprF r
  = NConstant NAtom
  -- ^ Constants: ints, bools, URIs, and null.
  | NStr (NString r)
  -- ^ A string, with interpolated expressions.
  | NSym VarName
  -- ^ A variable. For example, in the expression @f a@, @f@ is represented
  -- as @NSym "f"@ and @a@ as @NSym "a"@.
  | NList [r]
  -- ^ A list literal.
  | NSet [Binding r]
  -- ^ An attribute set literal, not recursive.
  | NRecSet [Binding r]
  -- ^ An attribute set literal, recursive.
  | NLiteralPath FilePath
  -- ^ A path expression, which is evaluated to a store path. The path here
  -- can be relative, in which case it's evaluated relative to the file in
  -- which it appears.
  | NEnvPath FilePath
  -- ^ A path which refers to something in the Nix search path (the NIX_PATH
  -- environment variable. For example, @<nixpkgs/pkgs>@.
  | NUnary NUnaryOp r
  -- ^ Application of a unary operator to an expression.
  | NBinary NBinaryOp r r
  -- ^ Application of a binary operator to two expressions.
  | NSelect r (NAttrPath r) (Maybe r)
  -- ^ Dot-reference into an attribute set, optionally providing an
  -- alternative if the key doesn't exist.
  | NHasAttr r (NAttrPath r)
  -- ^ Ask if a set contains a given attribute path.
  | NAbs (Params r) r
  -- ^ A function literal (lambda abstraction).
  | NLet [Binding r] r
  -- ^ Evaluate the second argument after introducing the bindings.
  | NIf r r r
  -- ^ If-then-else statement.
  | NWith r r
  -- ^ Evaluate an attribute set, bring its bindings into scope, and
  -- evaluate the second argument.
  | NAssert r r
  -- ^ Assert that the first returns true before evaluating the second.
  deriving (Ord, Eq, Generic, Generic1, Typeable, Data, Functor,
            Foldable, Traversable, Show, NFData, NFData1)

-- | We make an `IsString` for expressions, where the string is interpreted
-- as an identifier. This is the most common use-case...
instance IsString NExpr where
  fromString = Fix . NSym . fromString

instance Lift (Fix NExprF) where
    lift = dataToExpQ $ \b ->
        case Reflection.typeOf b `eqTypeRep` Reflection.typeRep @Text of
            Just HRefl -> Just [| pack $(liftString $ unpack b) |]
            Nothing -> Nothing


-- | The monomorphic expression type is a fixed point of the polymorphic one.
type NExpr = Fix NExprF

-- | A single line of the bindings section of a let expression or of a set.
data Binding r
  = NamedVar (NAttrPath r) r
  -- ^ An explicit naming, such as @x = y@ or @x.y = z@.
  | Inherit (Maybe r) (NAttrPath r)
  -- ^ Using a name already in scope, such as @inherit x;@ which is shorthand
  -- for @x = x;@ or @inherit (x) y;@ which means @y = x.y;@.
  deriving (Generic, Generic1, Typeable, Data, Ord, Eq, Functor,
            Foldable, Traversable, Show, NFData, NFData1)

-- | @Params@ represents all the ways the formal parameters to a
-- function can be represented.
data Params r
  = Param VarName
  -- ^ For functions with a single named argument, such as @x: x + 1@.
  | ParamSet (ParamSet r) Bool (Maybe VarName)
  -- ^ Explicit parameters (argument must be a set). Might specify a name to
  -- bind to the set in the function body. The bool indicates whether it is
  -- variadic or not.
  deriving (Ord, Eq, Generic, Generic1, Typeable, Data, Functor, Show,
            Foldable, Traversable, NFData, NFData1)

-- This uses an association list because nix XML serialization preserves the
-- order of the param set.
type ParamSet r = [(VarName, Maybe r)]

instance IsString (Params r) where
  fromString = Param . fromString

-- | 'Antiquoted' represents an expression that is either
-- antiquoted (surrounded by ${...}) or plain (not antiquoted).
data Antiquoted (v :: *) (r :: *) = Plain v | EscapedNewline | Antiquoted r
  deriving (Ord, Eq, Generic, Generic1, Typeable, Data, Functor,
            Foldable, Traversable, Show, NFData, NFData1)

-- | An 'NString' is a list of things that are either a plain string
-- or an antiquoted expression. After the antiquotes have been evaluated,
-- the final string is constructed by concating all the parts.
data NString r
  = DoubleQuoted [Antiquoted Text r]
  -- ^ Strings wrapped with double-quotes (") can contain literal newline
  -- characters, but the newlines are preserved and no indentation is stripped.
  | Indented [Antiquoted Text r]
  -- ^ Strings wrapped with two single quotes ('') can contain newlines,
  -- and their indentation will be stripped.
  deriving (Eq, Ord, Generic, Generic1, Typeable, Data, Functor,
            Foldable, Traversable, Show, NFData, NFData1)

-- | For the the 'IsString' instance, we use a plain doublequoted string.
instance IsString (NString r) where
  fromString "" = DoubleQuoted []
  fromString string = DoubleQuoted [Plain $ pack string]

-- | A 'KeyName' is something that can appear on the left side of an
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
  = DynamicKey (Antiquoted (NString r) r)
  | StaticKey VarName (Maybe SourcePos)
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

instance Generic1 NKeyName where
  type Rep1 NKeyName = NKeyName -- jww (2018-04-09): wrong
  from1 = id
  to1   = id
instance NFData1 NKeyName where
    liftRnf _ (StaticKey !_ !_) = ()
    liftRnf _ (DynamicKey (Plain !_)) = ()
    liftRnf _ (DynamicKey EscapedNewline) = ()
    liftRnf k (DynamicKey (Antiquoted r)) = k r

-- | Most key names are just static text, so this instance is convenient.
instance IsString (NKeyName r) where
  fromString = flip StaticKey Nothing . fromString

instance Eq1 NKeyName where
  liftEq eq (DynamicKey a) (DynamicKey b) = liftEq2 (liftEq eq) eq a b
  liftEq _ (StaticKey a _) (StaticKey b _) = a == b
  liftEq _ _ _ = False

-- Deriving this instance automatically is not possible because @r@
-- occurs not only as last argument in @Antiquoted (NString r) r@
instance Show1 NKeyName where
  liftShowsPrec sp sl p = \case
    DynamicKey a -> showsUnaryWith (liftShowsPrec2 (liftShowsPrec sp sl) (liftShowList sp sl) sp sl) "DynamicKey" p a
    StaticKey t _  -> showsUnaryWith showsPrec "StaticKey" p t

-- Deriving this instance automatically is not possible because @r@
-- occurs not only as last argument in @Antiquoted (NString r) r@
instance Functor NKeyName where
  fmap = fmapDefault

-- Deriving this instance automatically is not possible because @r@
-- occurs not only as last argument in @Antiquoted (NString r) r@
instance Foldable NKeyName where
  foldMap = foldMapDefault

-- Deriving this instance automatically is not possible because @r@
-- occurs not only as last argument in @Antiquoted (NString r) r@
instance Traversable NKeyName where
  traverse f = \case
    DynamicKey (Plain str)    -> DynamicKey . Plain <$> traverse f str
    DynamicKey EscapedNewline -> pure $ DynamicKey EscapedNewline
    DynamicKey (Antiquoted e) -> DynamicKey . Antiquoted <$> f e
    StaticKey key pos -> pure (StaticKey key pos)

-- | A selector (for example in a @let@ or an attribute set) is made up
-- of strung-together key names.
type NAttrPath r = [NKeyName r]

-- | There are two unary operations: logical not and integer negation.
data NUnaryOp = NNeg | NNot
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

-- | Binary operators expressible in the nix language.
data NBinaryOp
  = NEq      -- ^ Equality (==)
  | NNEq     -- ^ Inequality (!=)
  | NLt      -- ^ Less than (<)
  | NLte     -- ^ Less than or equal (<=)
  | NGt      -- ^ Greater than (>)
  | NGte     -- ^ Greater than or equal (>=)
  | NAnd     -- ^ Logical and (&&)
  | NOr      -- ^ Logical or (||)
  | NImpl    -- ^ Logical implication (->)
  | NUpdate  -- ^ Joining two attribut sets (//)
  | NPlus    -- ^ Addition (+)
  | NMinus   -- ^ Subtraction (-)
  | NMult    -- ^ Multiplication (*)
  | NDiv     -- ^ Division (/)
  | NConcat  -- ^ List concatenation (++)
  | NApp     -- ^ Apply a function to an argument.
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

-- | Get the name out of the parameter (there might be none).
paramName :: Params r -> Maybe VarName
paramName (Param n) = Just n
paramName (ParamSet _ _ n) = n

$(deriveEq1 ''NExprF)
$(deriveEq1 ''NString)
$(deriveEq1 ''Binding)
$(deriveEq1 ''Params)
$(deriveEq1 ''Antiquoted)
$(deriveEq2 ''Antiquoted)

$(deriveShow1 ''NExprF)
$(deriveShow1 ''NString)
$(deriveShow1 ''Params)
$(deriveShow1 ''Binding)
$(deriveShow1 ''Antiquoted)
$(deriveShow2 ''Antiquoted)

instance (Binary v, Binary a) => Binary (Antiquoted v a)
instance Binary a => Binary (NString a)
instance Binary a => Binary (Binding a)
instance Binary Pos where
    put x = put (unPos x)
    get = mkPos <$> get
instance Binary SourcePos
instance Binary a => Binary (NKeyName a)
instance Binary a => Binary (Params a)
instance Binary NAtom
instance Binary NUnaryOp
instance Binary NBinaryOp
instance Binary a => Binary (NExprF a)

stripPositionInfo :: NExpr -> NExpr
stripPositionInfo = transport phi
  where
    phi (NSet binds)         = NSet (map go binds)
    phi (NRecSet binds)      = NRecSet (map go binds)
    phi (NLet binds body)    = NLet (map go binds) body
    phi (NSelect s attr alt) = NSelect s (map clear attr) alt
    phi x = x

    go (NamedVar path r)  = NamedVar (map clear path) r
    go (Inherit ms names) = Inherit ms (map clear names)

    clear (StaticKey name _) = StaticKey name Nothing
    clear k = k

class ConvertValue v a where
    ofVal   :: a -> v
    wantVal :: v -> Maybe a

type Convertible v t =
    (ConvertValue v Bool,
     ConvertValue v Int,
     ConvertValue v Integer,
     ConvertValue v Float,
     ConvertValue v Text,
     ConvertValue v (Maybe Text),  -- text or null
     ConvertValue v [t],
     ConvertValue v (AttrSet t, AttrSet SourcePos),
     ConvertValue v (AttrSet t))
