-- | A bunch of shorthands for making nix expressions.
module Nix.Expr.Shorthands where

import Prelude
import Data.Monoid
import Data.Text (Text)
import Data.Fix
import qualified Data.Map as Map
import Nix.Atoms
import Nix.Expr.Types

-- | Make an integer literal expression.
mkInt :: Integer -> NExpr
mkInt = Fix . NConstant . NInt

-- | Make a regular (double-quoted) string.
mkStr :: Text -> NExpr
mkStr = Fix . NStr . DoubleQuoted . \case
  "" -> []
  x -> [Plain x]

mkIndentedStr :: Text -> NExpr
mkIndentedStr = Fix . NStr . Indented . \case
  "" -> []
  x -> [Plain x]

-- | Make a literal URI expression.
mkUri :: Text -> NExpr
mkUri = Fix . NConstant . NUri

-- | Make a path. Use 'True' if the path should be read from the
-- environment, else 'False'.
mkPath :: Bool -> FilePath -> NExpr
mkPath False = Fix . NLiteralPath
mkPath True = Fix . NEnvPath

-- | Make a path expression which pulls from the NIX_PATH env variable.
mkEnvPath :: FilePath -> NExpr
mkEnvPath = mkPath True

-- | Make a path expression which references a relative path.
mkRelPath :: FilePath -> NExpr
mkRelPath = mkPath False

-- | Make a variable (symbol)
mkSym :: Text -> NExpr
mkSym = Fix . NSym

mkSelector :: Text -> NAttrPath NExpr
mkSelector = (:[]) . StaticKey

mkBool :: Bool -> NExpr
mkBool = Fix . NConstant . NBool

mkNull :: NExpr
mkNull = Fix (NConstant NNull)

mkOper :: NUnaryOp -> NExpr -> NExpr
mkOper op = Fix . NUnary op

mkOper2 :: NBinaryOp -> NExpr -> NExpr -> NExpr
mkOper2 op a = Fix . NBinary op a

mkParamset :: [(Text, Maybe NExpr)] -> Params NExpr
mkParamset params = ParamSet (mkFixedParamSet params) Nothing

mkFixedParamSet :: [(Text, Maybe NExpr)] -> ParamSet NExpr
mkFixedParamSet ps = FixedParamSet (Map.fromList ps)

mkVariadicParamSet :: [(Text, Maybe NExpr)] -> ParamSet NExpr
mkVariadicParamSet ps = VariadicParamSet (Map.fromList ps)

mkApp :: NExpr -> NExpr -> NExpr
mkApp e = Fix . NApp e

mkRecSet :: [Binding NExpr] -> NExpr
mkRecSet = Fix . NRecSet

mkNonRecSet :: [Binding NExpr] -> NExpr
mkNonRecSet = Fix . NSet

mkLets :: [Binding NExpr] -> NExpr -> NExpr
mkLets bindings = Fix . NLet bindings

mkList :: [NExpr] -> NExpr
mkList = Fix . NList

mkWith :: NExpr -> NExpr -> NExpr
mkWith e = Fix . NWith e

mkAssert :: NExpr -> NExpr -> NExpr
mkAssert e = Fix . NWith e

mkIf :: NExpr -> NExpr -> NExpr -> NExpr
mkIf e1 e2 = Fix . NIf e1 e2

mkFunction :: Params NExpr -> NExpr -> NExpr
mkFunction params = Fix . NAbs params

mkDot :: NExpr -> Text -> NExpr
mkDot e key = Fix $ NSelect e [StaticKey key] Nothing

-- | An `inherit` clause without an expression to pull from.
inherit :: [NKeyName e] -> Binding e
inherit = Inherit Nothing

-- | An `inherit` clause with an expression to pull from.
inheritFrom :: e -> [NKeyName e] -> Binding e
inheritFrom expr = Inherit (Just expr)

-- | Shorthand for producing a binding of a name to an expression.
bindTo :: Text -> NExpr -> Binding NExpr
bindTo name val = NamedVar (mkSelector name) val

-- | Infix version of bindTo.
($=) :: Text -> NExpr -> Binding NExpr
name $= value = bindTo name value

infixr 2 $=

-- | Append a list of bindings to a set or let expression.
-- For example, adding `[a = 1, b = 2]` to `let c = 3; in 4` produces
-- `let a = 1; b = 2; c = 3; in 4`.
appendBindings :: [Binding NExpr] -> NExpr -> NExpr
appendBindings newBindings (Fix e) = case e of
  NLet bindings e' -> Fix $ NLet (bindings <> newBindings) e'
  NSet bindings -> Fix $ NSet (bindings <> newBindings)
  NRecSet bindings -> Fix $ NRecSet (bindings <> newBindings)
  _ -> error "Can only append bindings to a set or a let"

-- | Applies a transformation to the body of a nix function.
modifyFunctionBody :: (NExpr -> NExpr) -> NExpr -> NExpr
modifyFunctionBody f (Fix e) = case e of
  NAbs params body -> Fix $ NAbs params (f body)
  _ -> error "Not a function"

-- | A let statement with multiple assignments.
letsE :: [(Text, NExpr)] -> NExpr -> NExpr
letsE pairs = Fix . NLet (map (uncurry bindTo) pairs)

-- | Wrapper for a single-variable @let@.
letE :: Text -> NExpr -> NExpr -> NExpr
letE varName varExpr = letsE [(varName, varExpr)]

-- | Make an attribute set (non-recursive).
attrsE :: [(Text, NExpr)] -> NExpr
attrsE pairs = Fix $ NSet (map (uncurry bindTo) pairs)

-- | Make an attribute set (recursive).
recAttrsE :: [(Text, NExpr)] -> NExpr
recAttrsE pairs = Fix $ NRecSet (map (uncurry bindTo) pairs)

-- | Logical negation.
mkNot :: NExpr -> NExpr
mkNot = Fix . NUnary NNot

-- | Dot-reference into an attribute set.
(!.) :: NExpr -> Text -> NExpr
(!.) = mkDot
infixl 8 !.

mkBinop :: NBinaryOp -> NExpr -> NExpr -> NExpr
mkBinop op e1 e2 = Fix (NBinary op e1 e2)

-- | Various nix binary operators
($==), ($!=), ($<), ($<=), ($>), ($>=), ($&&), ($||), ($->),
  ($//), ($+), ($-), ($*), ($/), ($++)
  :: NExpr -> NExpr -> NExpr
e1 $== e2 = mkBinop NEq e1 e2
e1 $!= e2 = mkBinop NNEq e1 e2
e1 $< e2 = mkBinop NLt e1 e2
e1 $<= e2 = mkBinop NLte e1 e2
e1 $> e2 = mkBinop NGt e1 e2
e1 $>= e2 = mkBinop NGte e1 e2
e1 $&& e2 = mkBinop NAnd e1 e2
e1 $|| e2 = mkBinop NOr e1 e2
e1 $-> e2 = mkBinop NImpl e1 e2
e1 $// e2 = mkBinop NUpdate e1 e2
e1 $+ e2 = mkBinop NPlus e1 e2
e1 $- e2 = mkBinop NMinus e1 e2
e1 $* e2 = mkBinop NMult e1 e2
e1 $/ e2 = mkBinop NDiv e1 e2
e1 $++ e2 = mkBinop NConcat e1 e2

-- | Function application expression.
(@@) :: NExpr -> NExpr -> NExpr
(@@) = mkApp
infixl 1 @@

-- | Lambda shorthand.
(==>) :: Params NExpr -> NExpr -> NExpr
(==>) = mkFunction

infixr 1 ==>
