{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Pretty where

import           Data.Fix
import           Data.HashMap.Lazy (toList)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashMap.Strict.InsOrd as OM
import qualified Data.HashSet as HashSet
import           Data.List (isPrefixOf, sort)
import           Data.Maybe (isJust)
import           Data.Text (pack, unpack, replace, strip)
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Expr
import           Nix.Value
import           Nix.Parser.Library (reservedNames)
import           Nix.Parser.Operators
import           Nix.StringOperations
import           Nix.Thunk
import           Prelude hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

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
prettyString (DoubleQuoted parts) = dquotes . hcat . map prettyPart $ parts
  where prettyPart (Plain t)      = text . concatMap escape . unpack $ t
        prettyPart (Antiquoted r) = text "$" <> braces (withoutParens r)
        escape '"' = "\\\""
        escape x = maybe [x] (('\\':) . (:[])) $ toEscapeCode x
prettyString (Indented parts)
  = group $ nest 2 (squote <> squote <$$> content) <$$> squote <> squote
 where
  content = vsep . map prettyLine . stripLastIfEmpty . splitLines $ parts
  stripLastIfEmpty = reverse . f . reverse where
    f ([Plain t] : xs) | Text.null (strip t) = xs
    f xs = xs
  prettyLine = hcat . map prettyPart
  prettyPart (Plain t) = text . unpack . replace "${" "''${" . replace "''" "'''" $ t
  prettyPart (Antiquoted r) = text "$" <> braces (withoutParens r)

prettyParams :: Params NixDoc -> Doc
prettyParams (Param n) = text $ unpack n
prettyParams (ParamSet s v mname) = prettyParamSet s v <> case mname of
  Nothing -> empty
  Just name -> text "@" <> text (unpack name)

prettyParamSet :: ParamSet NixDoc -> Bool -> Doc
prettyParamSet args var =
    encloseSep (lbrace <> space) (align rbrace) sep prettyArgs
  where
    prettySetArg (n, maybeDef) = case maybeDef of
      Nothing -> text (unpack n)
      Just v -> text (unpack n) <+> text "?" <+> withoutParens v
    prettyArgs
        | var = map prettySetArg (OM.toList args)
        | otherwise = map prettySetArg (OM.toList args) ++ [text "..."]
    sep = align (comma <> space)

prettyBind :: Binding NixDoc -> Doc
prettyBind (NamedVar n v) =
    prettySelector n <+> equals <+> withoutParens v <> semi
prettyBind (Inherit s ns)
  = text "inherit" <+> scope <> align (fillSep (map prettyKeyName ns)) <> semi
 where scope = maybe empty ((<> space) . parens . withoutParens) s

prettyKeyName :: NKeyName NixDoc -> Doc
prettyKeyName (StaticKey "" _) = dquotes $ text ""
prettyKeyName (StaticKey key _)
  | HashSet.member (unpack key) reservedNames = dquotes $ text $ unpack key
prettyKeyName (StaticKey key _) = text . unpack $ key
prettyKeyName (DynamicKey key) = runAntiquoted prettyString withoutParens key

prettySelector :: NAttrPath NixDoc -> Doc
prettySelector = hcat . punctuate dot . map prettyKeyName

prettyAtom :: NAtom -> NixDoc
prettyAtom atom = simpleExpr $ text $ unpack $ atomText atom

prettyNix :: NExpr -> Doc
prettyNix = withoutParens . cata phi where
  phi :: NExprF NixDoc -> NixDoc
  phi (NConstant atom) = prettyAtom atom
  phi (NStr str) = simpleExpr $ prettyString str
  phi (NList []) = simpleExpr $ lbracket <> rbracket
  phi (NList xs) = simpleExpr $ group $
    nest 2 (vsep $ lbracket : map (wrapParens appOpNonAssoc) xs) <$> rbracket
  phi (NSet []) = simpleExpr $ lbrace <> rbrace
  phi (NSet xs) = simpleExpr $ group $
    nest 2 (vsep $ lbrace : map prettyBind xs) <$> rbrace
  phi (NRecSet []) = simpleExpr $ recPrefix <> lbrace <> rbrace
  phi (NRecSet xs) = simpleExpr $ group $
    nest 2 (vsep $ recPrefix <> lbrace : map prettyBind xs) <$> rbrace
  phi (NAbs args body) = leastPrecedence $
   (prettyParams args <> colon) </> indent 2 (withoutParens body)
  phi (NBinary op r1 r2) = flip NixDoc opInfo $ hsep
    [ wrapParens (f NAssocLeft) r1
    , text $ operatorName opInfo
    , wrapParens (f NAssocRight) r2
    ]
    where
      opInfo = getBinaryOperator op
      f x
        | associativity opInfo /= x = opInfo { associativity = NAssocNone }
        | otherwise = opInfo
  phi (NUnary op r1) =
    NixDoc (text (operatorName opInfo) <> wrapParens opInfo r1) opInfo
    where opInfo = getUnaryOperator op
  phi (NSelect r [] _) = r
  phi (NSelect r attr o) = (if isJust o then leastPrecedence else flip NixDoc selectOp) $
     wrapParens selectOp r <> dot <> prettySelector attr <> ordoc
    where ordoc = maybe empty (((space <> text "or") <+>) . withoutParens) o
  phi (NHasAttr r attr)
    = NixDoc (wrapParens hasAttrOp r <+> text "?" <+> prettySelector attr) hasAttrOp
  phi (NApp fun arg)
    = NixDoc (wrapParens appOp fun <+> wrapParens appOpNonAssoc arg) appOp
  phi (NEnvPath p) = simpleExpr $ text ("<" ++ p ++ ">")
  phi (NLiteralPath p) = simpleExpr $ text $ case p of
    "./" -> "./."
    "../" -> "../."
    ".." -> "../."
    txt | "/" `isPrefixOf` txt -> txt
        | "./" `isPrefixOf` txt -> txt
        | "../" `isPrefixOf` txt -> txt
        | otherwise -> "./" ++ txt
  phi (NSym name) = simpleExpr $ text (unpack name)
  phi (NLet binds body) = leastPrecedence $ group $ text "let" <$> indent 2 (
        vsep (map prettyBind binds)) <$> text "in" <+> withoutParens body
  phi (NIf cond trueBody falseBody) = leastPrecedence $
    group $ nest 2 $ (text "if" <+> withoutParens cond) <$>
      (  align (text "then" <+> withoutParens trueBody)
     <$> align (text "else" <+> withoutParens falseBody)
      )
  phi (NWith scope body) = leastPrecedence $
   text "with"  <+> withoutParens scope <> semi <$> align (withoutParens body)
  phi (NAssert cond body) = leastPrecedence $
   text "assert" <+> withoutParens cond <> semi <$> align (withoutParens body)

  recPrefix = text "rec" <> space

prettyNixValue :: Functor m => NValueNF m -> Doc
prettyNixValue = prettyNix . valueToExpr
  where valueToExpr :: Functor m => NValueNF m -> NExpr
        valueToExpr = hmap go
        -- hmap does the recursive conversion from NValue to NExpr.
        -- fun fact: it is not defined in data-fixed, but I was certain it
        -- should exists so I found it in unification-fd by hoogling its type
        hmap :: (Functor f, Functor g) => (forall a. f a -> g a)
             -> Fix f -> Fix g
        hmap eps = ana (eps . unFix)
        go (NVConstant a) = NConstant a
        go (NVStr t _) = NStr (DoubleQuoted [Plain t])
        go (NVList l) = NList l
        go (NVSet s p) = NSet [ NamedVar [StaticKey k (M.lookup k p)] v
                              | (k, v) <- toList s ]
        go (NVClosure s p _) =
            NSym . pack $ "<closure in " ++ show s
                ++ " with " ++ show (() <$ p)  ++ ">"
        go (NVLiteralPath fp) = NLiteralPath fp
        go (NVEnvPath p) = NEnvPath p
        go (NVBuiltin name _) = NSym $ Text.pack $ "builtins." ++ name

printNix :: Functor m => NValueNF m -> String
printNix = cata phi
  where phi :: NValueF m String -> String
        phi (NVConstant a) = unpack $ atomText a
        phi (NVStr t _) = show t
        phi (NVList l) = "[ " ++ unwords l ++ " ]"
        phi (NVSet s _) =
            "{ " ++ concat [ unpack k ++ " = " ++ v ++ "; "
                           | (k, v) <- sort $ toList s ] ++ "}"
        phi NVClosure {} = "<<lambda>>"
        phi (NVLiteralPath fp) = fp
        phi (NVEnvPath p) = p
        phi (NVBuiltin name _) = "<<builtin " ++ name ++ ">>"

removeEffects :: Functor m => NValue m -> NValueNF m
removeEffects = Fix . fmap dethunk
  where
    dethunk (NThunk (Value v)) = removeEffects v
    dethunk (NThunk _) = Fix $ NVStr "<thunk>" mempty

showValue :: Functor m => NValue m -> String
showValue = show . prettyNixValue . removeEffects
