{-# LANGUAGE OverloadedStrings #-}
module Nix.Pretty where

import Prelude hiding ((<$>))
import Data.Fix
import Data.Map (toList)
import Data.Maybe (isJust)
import Data.Text (Text, unpack, replace, strip)
import Nix.Types
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.Text as Text

-- | This type represents a pretty printed nix expression
-- together with some information about the expression.
data NixDoc = NixDoc
  { -- | The rendered expression, without any parentheses.
    withoutParens    :: Doc

    -- | The root operator is the operator at the root of
    -- the expression tree. For example, in '(a * b) + c', '+' would be the root
    -- operator. It is needed to determine if we need to wrap the expression in
    -- parentheses.
  , rootOp :: OperatorInfo
  }

-- | A simple expression is never wrapped in parentheses. The expression
-- behaves as if its root operator had a precedence higher than all
-- other operators (including function application).
simpleExpr :: Doc -> NixDoc
simpleExpr = flip NixDoc $ OperatorInfo maxBound NAssocNone "simple expr"

-- | An expression that behaves as if its root operator
-- had a precedence lower than all other operators.
-- That ensures that the expression is wrapped in parantheses in
-- almost always, but it's still rendered without parentheses
-- in cases where parentheses are never required (such as in the LHS
-- of a binding).
leastPrecedence :: Doc -> NixDoc
leastPrecedence = flip NixDoc $ OperatorInfo minBound NAssocNone "least precedence"

appOpNonAssoc :: OperatorInfo
appOpNonAssoc = appOp { associativity = NAssocNone }

wrapParens :: OperatorInfo -> NixDoc -> Doc
wrapParens op sub
  | precedence (rootOp sub) > precedence op = withoutParens sub
  | precedence (rootOp sub) == precedence op
    && associativity (rootOp sub) == associativity op
    && associativity op /= NAssocNone = withoutParens sub
  | otherwise = parens $ withoutParens sub

prettyString :: NString NixDoc -> Doc
prettyString (NString DoubleQuoted parts) = dquotes . hcat . map prettyPart $ parts
  where prettyPart (Plain t)      = text . concatMap escape . unpack $ t
        prettyPart (Antiquoted r) = text "$" <> braces (withoutParens r)
        escape '"' = "\""
        escape x = maybe [x] (('\\':) . (:[])) $ toEscapeCode x
prettyString (NString Indented parts)
  = group $ nest 2 (squote <> squote <$$> content) <$$> squote <> squote
 where
  content = vsep . map prettyLine . stripLastIfEmpty . splitLines $ parts
  stripLastIfEmpty = reverse . f . reverse where
    f ([Plain t] : xs) | Text.null (strip t) = xs
    f xs = xs
  prettyLine = hcat . map prettyPart
  prettyPart (Plain t) = text . unpack . replace "$" "''$" . replace "''" "'''" $ t
  prettyPart (Antiquoted r) = text "$" <> braces (withoutParens r)

prettyString (NUri uri) = text (unpack uri)

prettyFormals :: Formals NixDoc -> Doc
prettyFormals (FormalName n) = text $ unpack n
prettyFormals (FormalSet s) = prettyParamSet s
prettyFormals (FormalLeftAt n s) = text (unpack n) <> text "@" <> prettyParamSet s
prettyFormals (FormalRightAt s n) = prettyParamSet s <> text "@" <> text (unpack n)

prettyParamSet :: FormalParamSet NixDoc -> Doc
prettyParamSet (FormalParamSet args) =
  lbrace <+> (hcat . punctuate (comma <> space) . map prettySetArg) (toList args) <+> rbrace

prettyBind :: Binding NixDoc -> Doc
prettyBind (NamedVar n v) = prettySelector n <+> equals <+> withoutParens v <> semi
prettyBind (Inherit s ns)
  = text "inherit" <+> scope <> fillSep (map prettySelector ns) <> semi
 where scope = maybe empty ((<> space) . parens . withoutParens) s

prettyKeyName :: NKeyName NixDoc -> Doc
prettyKeyName (StaticKey key) = text . unpack $ key
prettyKeyName (DynamicKey key) = runAntiquoted prettyString withoutParens key

prettySelector :: NSelector NixDoc -> Doc
prettySelector = hcat . punctuate dot . map prettyKeyName

prettySetArg :: (Text, Maybe NixDoc) -> Doc
prettySetArg (n, Nothing) = text (unpack n)
prettySetArg (n, Just v) = text (unpack n) <+> text "?" <+> withoutParens v

prettyOper :: NOperF NixDoc -> NixDoc
prettyOper (NBinary op r1 r2) = flip NixDoc opInfo $ hsep
  [ wrapParens (f NAssocLeft) r1
  , text $ operatorName opInfo
  , wrapParens (f NAssocRight) r2
  ]
 where
  opInfo = getBinaryOperator op
  f x
    | associativity opInfo /= x = opInfo { associativity = NAssocNone }
    | otherwise = opInfo
prettyOper (NUnary op r1) =
  NixDoc (text (operatorName opInfo) <> wrapParens opInfo r1) opInfo
 where opInfo = getUnaryOperator op

prettyAtom :: NAtom -> NixDoc
prettyAtom atom = simpleExpr $ text $ unpack $ atomText atom

prettyNix :: NExpr -> Doc
prettyNix = withoutParens . cata phi where
  phi :: NExprF NixDoc -> NixDoc
  phi (NConstant atom) = prettyAtom atom
  phi (NStr str) = simpleExpr $ prettyString str
  phi (NList xs) = simpleExpr $ group $
    nest 2 (vsep $ lbracket : map (wrapParens appOpNonAssoc) xs) <$> rbracket
  phi (NSet rec xs) = simpleExpr $ group $
    nest 2 (vsep $ prefix rec <> lbrace : map prettyBind xs) <$> rbrace
   where
    prefix Rec = text "rec" <> space
    prefix NonRec = empty
  phi (NAbs args body) = leastPrecedence $
    (prettyFormals args <> colon) </> withoutParens body

  phi (NOper oper) = prettyOper oper
  phi (NSelect r attr o) = (if isJust o then leastPrecedence else flip NixDoc selectOp) $
     wrapParens selectOp r <> dot <> prettySelector attr <> ordoc
    where ordoc = maybe empty (((space <> text "or") <+>) . withoutParens) o
  phi (NHasAttr r attr)
    = NixDoc (wrapParens hasAttrOp r <+> text "?" <+> prettySelector attr) hasAttrOp
  phi (NApp fun arg)
    = NixDoc (wrapParens appOp fun <+> wrapParens appOpNonAssoc arg) appOp

  phi (NSym name) = simpleExpr $ text (unpack name)
  phi (NLet binds body) = leastPrecedence $ group $ nest 2 $
        vsep (text "let" : map prettyBind binds) <$> text "in" <+> withoutParens body
  phi (NIf cond trueBody falseBody) = leastPrecedence $
    group $ nest 2 $ (text "if" <+> withoutParens cond) <$>
      (  align (text "then" <+> withoutParens trueBody)
     <$> align (text "else" <+> withoutParens falseBody)
      )
  phi (NWith scope body) = leastPrecedence $
    text "with"  <+> withoutParens scope <> semi <+> withoutParens body
  phi (NAssert cond body) = leastPrecedence $
    text "assert" <+> withoutParens cond <> semi <+> withoutParens body
