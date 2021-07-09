
-- | A bunch of shorthands for making nix expressions.
--
-- Functions with an @F@ suffix return a more general type (base functor @F a@) without the outer
-- 'Fix' wrapper that creates @a@.
module Nix.Expr.Shorthands where

import           Data.Fix
import           Nix.Atoms
import           Nix.Expr.Types

-- * Basic expression builders

-- | Put null.
mkNull :: NExpr
mkNull = Fix mkNullF

-- | Put boolean.
mkBool :: Bool -> NExpr
mkBool = Fix . mkBoolF

-- | Put integer.
mkInt :: Integer -> NExpr
mkInt = Fix . mkIntF

-- | Put floating point number.
mkFloat :: Float -> NExpr
mkFloat = Fix . mkFloatF

-- | Put a regular (double-quoted) string.
mkStr :: Text -> NExpr
mkStr = Fix . NStr . DoubleQuoted . \case
  "" -> mempty
  x  -> [Plain x]

-- | Put an indented string.
mkIndentedStr :: Int -> Text -> NExpr
mkIndentedStr w = Fix . NStr . Indented w . \case
  "" -> mempty
  x  -> [Plain x]

-- | Put a path. Use @True@ if the path should be read from the environment, else use @False@.
mkPath :: Bool -> FilePath -> NExpr
mkPath b = Fix . mkPathF b

-- | Put a path expression which pulls from the @NIX_PATH@ @env@ variable.
mkEnvPath :: FilePath -> NExpr
mkEnvPath = Fix . mkEnvPathF

-- | Put a path which references a relative path.
mkRelPath :: FilePath -> NExpr
mkRelPath = Fix . mkRelPathF

-- | Put a variable (symbol).
mkSym :: Text -> NExpr
mkSym = Fix . mkSymF

-- | Put syntactic hole.
mkSynHole :: Text -> NExpr
mkSynHole = Fix . mkSynHoleF

mkSelector :: Text -> NAttrPath NExpr
mkSelector = (:| mempty) . StaticKey

-- | Put an unary operator.
mkOper :: NUnaryOp -> NExpr -> NExpr
mkOper op = Fix . NUnary op

-- | Put a binary operator.
mkOper2 :: NBinaryOp -> NExpr -> NExpr -> NExpr
mkOper2 op a = Fix . NBinary op a

mkParamset :: [(Text, Maybe NExpr)] -> Bool -> Params NExpr
mkParamset params variadic = ParamSet params variadic mempty

-- | Put a recursive set.
--
-- @rec { .. };@
mkRecSet :: [Binding NExpr] -> NExpr
mkRecSet = Fix . NSet Recursive

-- | Put a non-recursive set.
--
-- > { .. }
mkNonRecSet :: [Binding NExpr] -> NExpr
mkNonRecSet = Fix . NSet NonRecursive

-- | Put a list.
mkList :: [NExpr] -> NExpr
mkList = Fix . NList

-- | Wrap in a @let@.
--
-- (Evaluate the second argument after introducing the bindings.)
--
-- +------------------------+-----------------+
-- | Haskell                | Nix             |
-- +========================+=================+
-- | @mkLets bindings expr@ | @let bindings;@ |
-- |                        | @in expr@       |
-- +------------------------+-----------------+
mkLets :: [Binding NExpr] -> NExpr -> NExpr
mkLets bindings = Fix . NLet bindings

-- | Create @where@:
-- 1st expr - @where@ body
-- 2nd - main expression @where@ serves to.
--
-- +--------------------+-------------------+
-- | Haskell            | Nix               |
-- +====================+===================+
-- | @mkWith body main@ | @with body; expr@ |
-- +--------------------+----00-------------+
mkWith :: NExpr -> NExpr -> NExpr
mkWith e = Fix . NWith e

mkAssert :: NExpr -> NExpr -> NExpr
mkAssert e = Fix . NAssert e

mkIf :: NExpr -> NExpr -> NExpr -> NExpr
mkIf e1 e2 = Fix . NIf e1 e2

mkFunction :: Params NExpr -> NExpr -> NExpr
mkFunction params = Fix . NAbs params

-- | Lambda function.
-- > x ==> x
--Haskell:
-- > \\ x -> x
--Nix:
-- > x: x
(==>) :: Params NExpr -> NExpr -> NExpr
(==>) = mkFunction
infixr 1 ==>

{-
mkDot :: NExpr -> Text -> NExpr
mkDot e key = mkDots e [key]

-- | Create a dotted expression using only text.
mkDots :: NExpr -> [Text] -> NExpr
mkDots e [] = e
mkDots (Fix (NSelect e keys' x)) keys =
  -- Special case: if the expression in the first argument is already
  -- a dotted expression, just extend it.
  Fix (NSelect e (keys' <> fmap (`StaticKey` Nothing) keys) x)
mkDots e keys = Fix $ NSelect e (fmap (`StaticKey` Nothing) keys) Nothing
-}

-- ** Basic base functor builders

mkNullF :: NExprF a
mkNullF = NConstant NNull

mkBoolF :: Bool -> NExprF a
mkBoolF = NConstant . NBool

mkIntF :: Integer -> NExprF a
mkIntF = NConstant . NInt

mkFloatF :: Float -> NExprF a
mkFloatF = NConstant . NFloat

mkPathF :: Bool -> FilePath -> NExprF a
mkPathF False = NLiteralPath
mkPathF True  = NEnvPath

mkEnvPathF :: FilePath -> NExprF a
mkEnvPathF = mkPathF True

mkRelPathF :: FilePath -> NExprF a
mkRelPathF = mkPathF False

mkSymF :: Text -> NExprF a
mkSymF = NSym

mkSynHoleF :: Text -> NExprF a
mkSynHoleF = NSynHole

-- * Other
-- (org this better/make a better name for section(s))

-- | An `inherit` clause without an expression to pull from.
inherit :: [NKeyName e] -> Binding e
inherit ks = Inherit Nothing ks nullPos

-- | An `inherit` clause with an expression to pull from.
inheritFrom :: e -> [NKeyName e] -> Binding e
inheritFrom expr ks = Inherit (pure expr) ks nullPos

-- | Shorthand for producing a binding of a name to an expression: @=@
bindTo :: Text -> NExpr -> Binding NExpr
bindTo name x = NamedVar (mkSelector name) x nullPos

-- | @=@. @bindTo@ infix version. Bind name to an expression.
($=) :: Text -> NExpr -> Binding NExpr
($=) = bindTo
infixr 2 $=

-- | Append a list of bindings to a set or let expression.
-- For example, adding `[a = 1, b = 2]` to `let c = 3; in 4` produces
-- `let a = 1; b = 2; c = 3; in 4`.
appendBindings :: [Binding NExpr] -> NExpr -> NExpr
appendBindings newBindings (Fix e) =
  case e of
    NLet bindings e'    -> mkLets (bindings <> newBindings) e'
    NSet recur bindings -> Fix $ NSet recur (bindings <> newBindings)
    _                   -> error "Can only append bindings to a set or a let"

-- | Applies a transformation to the body of a nix function.
modifyFunctionBody :: (NExpr -> NExpr) -> NExpr -> NExpr
modifyFunctionBody f (Fix (NAbs params body)) = mkFunction params $ f body
modifyFunctionBody _ _ = error "Not a function"

-- | A let statement with multiple assignments.
letsE :: [(Text, NExpr)] -> NExpr -> NExpr
letsE pairs = mkLets $ uncurry ($=) <$> pairs

-- | Wrapper for a single-variable @let@.
letE :: Text -> NExpr -> NExpr -> NExpr
letE varName varExpr = letsE [(varName, varExpr)]

-- | Make an attribute set (non-recursive).
attrsE :: [(Text, NExpr)] -> NExpr
attrsE pairs = mkNonRecSet $ uncurry ($=) <$> pairs

-- | Make an attribute set (recursive).
recAttrsE :: [(Text, NExpr)] -> NExpr
recAttrsE pairs = mkRecSet $ uncurry ($=) <$> pairs

-- | Logical negation.
mkNot :: NExpr -> NExpr
mkNot = mkOper NNot

-- | Dot-reference into an attribute set: @attrSet.k@
(@.) :: NExpr -> Text -> NExpr
(@.) obj name = Fix $ NSelect obj (StaticKey name :| mempty) Nothing
infixl 2 @.

-- * Nix binary operators

-- | Nix binary operator builder.
mkBinop :: NBinaryOp -> NExpr -> NExpr -> NExpr
mkBinop = mkOper2

(@@), ($==), ($!=), ($<), ($<=), ($>), ($>=), ($&&), ($||), ($->), ($//), ($+), ($-), ($*), ($/), ($++)
  :: NExpr -> NExpr -> NExpr
-- | Function application (@' '@ in @f x@)
(@@) = mkOper2 NApp
infixl 1 @@
-- | Equality: @==@
($==) = mkOper2 NEq
-- | Inequality: @!=@
($!=) = mkOper2 NNEq
-- | Less than: @<@
($<)  = mkOper2 NLt
-- | Less than OR equal: @<=@
($<=) = mkOper2 NLte
-- | Greater than: @>@
($>)  = mkOper2 NGt
-- | Greater than OR equal: @>=@
($>=) = mkOper2 NGte
-- | AND: @&&@
($&&) = mkOper2 NAnd
-- | OR: @||@
($||) = mkOper2 NOr
-- | Logical implication: @->@
($->) = mkOper2 NImpl
-- | Extend/override the left attr set, with the right one: @//@
($//) = mkOper2 NUpdate
-- | Addition: @+@
($+)  = mkOper2 NPlus
-- | Subtraction: @-@
($-)  = mkOper2 NMinus
-- | Multiplication: @*@
($*)  = mkOper2 NMult
-- | Division: @/@
($/)  = mkOper2 NDiv
-- | List concatenation: @++@
($++) = mkOper2 NConcat

