module Nix.Pretty where

import Text.PrettyPrint.HughesPJ
import Nix.Types
import Data.Map (toList)
import Data.Text (Text)

prettyBind :: (NExpr, NExpr) -> Doc
prettyBind (n, v) = prettyNix n <+> equals <+> prettyNix v <> semi

prettySetArg :: (Text, Maybe NExpr) -> Doc
prettySetArg (n, Nothing) = text (show n)
prettySetArg (n, Just v) = text (show n) <+> text "?" <+> prettyNix v

prettyFold = foldr (<+>) empty

prettyNix :: NExpr -> Doc
prettyNix (Fix expr) = go expr where
  go (NConstant atom) = text $ show atom
  go (NOper oper) = text $ show oper
  go (NList list) = lbrack <+> (prettyFold $ map prettyNix list) <+> rbrack

  go (NArgSet args) = lbrace <+> (prettyFold $ map prettySetArg $ toList args) <+> rbrace

  go (NSet rec list) = 
    (if rec then text "rec" else empty)
    <+> lbrace <+> (foldr (<+>) empty $ map prettyBind list) <+> rbrace

  go (NLet binds body) = text "let"
  go (NIf cond trueBody falseBody) =
    text "if" <+> prettyNix cond
    <+> text "then" <+> prettyNix trueBody
    <+> text "else" <+> prettyNix falseBody

  go (NWith scope body) = text "with" <+> prettyNix scope <> semi <+> prettyNix body
  go (NAssert cond body) = text "assert" <+> prettyNix cond <> semi <+> prettyNix body
  go (NInherit attrs) = text "inherit"

  go (NVar e) = prettyNix e
  go (NApp fun arg) = prettyNix fun <+> prettyNix arg
  go (NAbs args body) = prettyNix args <> colon <+> prettyNix body

