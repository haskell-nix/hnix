{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Pretty where

import           Control.Monad
import           Data.Fix
import           Data.HashMap.Lazy (toList)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as HashSet
import           Data.List (isPrefixOf, sort)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isJust)
import           Data.Text (pack, unpack, replace, strip)
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Expr
import           Nix.Parser.Library (reservedNames)
import           Nix.Parser.Operators
import           Nix.Strings
import           Nix.Thunk
#if ENABLE_TRACING
import           Nix.Utils
#else
import           Nix.Utils hiding ((<$>))
#endif
import           Nix.Value
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
  , wasPath :: Bool -- This is needed so that when a path is used in a selector path
                    -- we can add brackets appropiately
  }

mkNixDoc :: Doc -> OperatorInfo -> NixDoc
mkNixDoc d o = NixDoc { withoutParens = d, rootOp = o, wasPath = False }

-- | A simple expression is never wrapped in parentheses. The expression
--   behaves as if its root operator had a precedence higher than all
--   other operators (including function application).
simpleExpr :: Doc -> NixDoc
simpleExpr d = mkNixDoc d (OperatorInfo minBound NAssocNone "simple expr")

pathExpr :: Doc -> NixDoc
pathExpr d = (simpleExpr d) { wasPath = True }

-- | An expression that behaves as if its root operator had a precedence lower
--   than all other operators. That ensures that the expression is wrapped in
--   parantheses in almost always, but it's still rendered without parentheses
--   in cases where parentheses are never required (such as in the LHS of a
--   binding).
leastPrecedence :: Doc -> NixDoc
leastPrecedence =
    flip mkNixDoc $ OperatorInfo maxBound NAssocNone "least precedence"

appOp :: OperatorInfo
appOp = getBinaryOperator NApp

appOpNonAssoc :: OperatorInfo
appOpNonAssoc = (getBinaryOperator NApp) { associativity = NAssocNone }

selectOp :: OperatorInfo
selectOp = getSpecialOperator NSelectOp

hasAttrOp :: OperatorInfo
hasAttrOp = getSpecialOperator NHasAttrOp

wrapParens :: OperatorInfo -> NixDoc -> Doc
wrapParens op sub
  | precedence (rootOp sub) < precedence op = withoutParens sub
  | precedence (rootOp sub) == precedence op
    && associativity (rootOp sub) == associativity op
    && associativity op /= NAssocNone = withoutParens sub
  | otherwise = parens $ withoutParens sub

-- Used in the selector case to print a path in a selector as
-- "${./abc}"
wrapPath :: OperatorInfo -> NixDoc -> Doc
wrapPath op sub =
  if wasPath sub then dquotes (text "$" <> braces (withoutParens sub))
                else wrapParens op sub

prettyString :: NString NixDoc -> Doc
prettyString (DoubleQuoted parts) = dquotes . hcat . map prettyPart $ parts
  where prettyPart (Plain t)      = text . concatMap escape . unpack $ t
        prettyPart EscapedNewline = text "''\\n"
        prettyPart (Antiquoted r) = text "$" <> braces (withoutParens r)
        escape '"' = "\\\""
        escape x = maybe [x] (('\\':) . (:[])) $ toEscapeCode x
prettyString (Indented _ parts)
  = group $ nest 2 (squote <> squote <$$> content) <$$> squote <> squote
 where
  content = vsep . map prettyLine . stripLastIfEmpty . splitLines $ parts
  stripLastIfEmpty = reverse . f . reverse where
    f ([Plain t] : xs) | Text.null (strip t) = xs
    f xs = xs
  prettyLine = hcat . map prettyPart
  prettyPart (Plain t) = text . unpack . replace "${" "''${" . replace "''" "'''" $ t
  prettyPart EscapedNewline = text "\\n"
  prettyPart (Antiquoted r) = text "$" <> braces (withoutParens r)

prettyParams :: Params NixDoc -> Doc
prettyParams (Param n) = text $ unpack n
prettyParams (ParamSet s v mname) = prettyParamSet s v <> case mname of
  Nothing -> empty
  Just name -> text "@" <> text (unpack name)

prettyParamSet :: ParamSet NixDoc -> Bool -> Doc
prettyParamSet args var =
    encloseSep (lbrace <> space) (align (space <> rbrace)) sep (map prettySetArg args ++ prettyVariadic)
  where
    prettySetArg (n, maybeDef) = case maybeDef of
      Nothing -> text (unpack n)
      Just v -> text (unpack n) <+> text "?" <+> withoutParens v
    prettyVariadic = [text "..." | var]
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
  | HashSet.member key reservedNames = dquotes $ text $ unpack key
prettyKeyName (StaticKey key _) = text . unpack $ key
prettyKeyName (DynamicKey key) =
    runAntiquoted (DoubleQuoted [Plain "\n"])
        prettyString ((text "$" <>) . braces . withoutParens) key

prettySelector :: NAttrPath NixDoc -> Doc
prettySelector = hcat . punctuate dot . map prettyKeyName . NE.toList

prettyAtom :: NAtom -> NixDoc
prettyAtom atom = simpleExpr $ text $ unpack $ atomText atom

prettyNix :: NExpr -> Doc
prettyNix = withoutParens . cata exprFNixDoc

prettyOriginExpr :: NExprLocF (Maybe (NValue m)) -> Doc
prettyOriginExpr = withoutParens . go
  where
    go = exprFNixDoc . annotated . getCompose . fmap render

    render Nothing = simpleExpr $ text "_"
    render (Just (NValue (reverse -> p:_) _)) = go (_originExpr p)
    render (Just (NValue _ _)) = simpleExpr $ text "?"
        -- simpleExpr $ foldr ((<$>) . parens . indent 2 . withoutParens
        --                           . go . originExpr)
        --     mempty (reverse ps)

exprFNixDoc :: NExprF NixDoc -> NixDoc
exprFNixDoc = \case
    NConstant atom -> prettyAtom atom
    NStr str -> simpleExpr $ prettyString str
    NList [] -> simpleExpr $ lbracket <> rbracket
    NList xs -> simpleExpr $ group $
        nest 2 (vsep $ lbracket : map (wrapParens appOpNonAssoc) xs) <$> rbracket
    NSet [] -> simpleExpr $ lbrace <> rbrace
    NSet xs -> simpleExpr $ group $
        nest 2 (vsep $ lbrace : map prettyBind xs) <$> rbrace
    NRecSet [] -> simpleExpr $ recPrefix <> lbrace <> rbrace
    NRecSet xs -> simpleExpr $ group $
        nest 2 (vsep $ recPrefix <> lbrace : map prettyBind xs) <$> rbrace
    NAbs args body -> leastPrecedence $
        nest 2 ((prettyParams args <> colon) <$> withoutParens body)
    NBinary NApp fun arg ->
        mkNixDoc (wrapParens appOp fun <+> wrapParens appOpNonAssoc arg) appOp
    NBinary op r1 r2 -> flip mkNixDoc opInfo $ hsep
        [ wrapParens (f NAssocLeft) r1
        , text $ unpack $ operatorName opInfo
        , wrapParens (f NAssocRight) r2
        ]
      where
        opInfo = getBinaryOperator op
        f x | associativity opInfo /= x = opInfo { associativity = NAssocNone }
            | otherwise = opInfo
    NUnary op r1 ->
        mkNixDoc (text (unpack (operatorName opInfo)) <> wrapParens opInfo r1) opInfo
      where opInfo = getUnaryOperator op
    NSelect r attr o ->
      (if isJust o then leastPrecedence else flip mkNixDoc selectOp) $
          wrapPath selectOp r <> dot <> prettySelector attr <> ordoc
      where ordoc = maybe empty (((space <> text "or") <+>) . wrapParens selectOp) o
    NHasAttr r attr ->
        mkNixDoc (wrapParens hasAttrOp r <+> text "?" <+> prettySelector attr) hasAttrOp
    NEnvPath p -> simpleExpr $ text ("<" ++ p ++ ">")
    NLiteralPath p -> pathExpr $ text $ case p of
        "./" -> "./."
        "../" -> "../."
        ".." -> "../."
        txt | "/" `isPrefixOf` txt -> txt
            | "~/" `isPrefixOf` txt -> txt
            | "./" `isPrefixOf` txt -> txt
            | "../" `isPrefixOf` txt -> txt
            | otherwise -> "./" ++ txt
    NSym name -> simpleExpr $ text (unpack name)
    NLet binds body -> leastPrecedence $ group $ text "let" <$> indent 2 (
        vsep (map prettyBind binds)) <$> text "in" <+> withoutParens body
    NIf cond trueBody falseBody -> leastPrecedence $
        group $ nest 2 $ (text "if" <+> withoutParens cond) <$>
          (  align (text "then" <+> withoutParens trueBody)
         <$> align (text "else" <+> withoutParens falseBody)
          )
    NWith scope body -> leastPrecedence $
        text "with"  <+> withoutParens scope <> semi <$> align (withoutParens body)
    NAssert cond body -> leastPrecedence $
        text "assert" <+> withoutParens cond <> semi <$> align (withoutParens body)
  where
    recPrefix = text "rec" <> space

prettyNValueNF :: Functor m => NValueNF m -> Doc
prettyNValueNF = prettyNix . valueToExpr
  where valueToExpr :: Functor m => NValueNF m -> NExpr
        valueToExpr = transport go

        go (NVConstantF a) = NConstant a
        go (NVStrF t _) = NStr (DoubleQuoted [Plain t])
        go (NVListF l) = NList l
        go (NVSetF s p) = NSet
            [ NamedVar (StaticKey k (M.lookup k p) :| []) v
            | (k, v) <- toList s ]
        go (NVClosureF _ _) = NSym . pack $ "<closure>"
        go (NVPathF p) = NLiteralPath p
        go (NVBuiltinF name _) = NSym $ Text.pack $ "builtins." ++ name

printNix :: Functor m => NValueNF m -> String
printNix = cata phi
  where phi :: NValueF m String -> String
        phi (NVConstantF a) = unpack $ atomText a
        phi (NVStrF t _) = show t
        phi (NVListF l) = "[ " ++ unwords l ++ " ]"
        phi (NVSetF s _) =
            "{ " ++ concat [ unpack k ++ " = " ++ v ++ "; "
                           | (k, v) <- sort $ toList s ] ++ "}"
        phi NVClosureF {} = "<<lambda>>"
        phi (NVPathF fp) = fp
        phi (NVBuiltinF name _) = "<<builtin " ++ name ++ ">>"

removeEffects :: Functor m => NValueF m (NThunk m) -> NValueNF m
removeEffects = Fix . fmap dethunk
  where
    dethunk (NThunk _ (Value v)) = removeEffects (_baseValue v)
    dethunk (NThunk _ _) = Fix $ NVStrF "<thunk>" mempty

removeEffectsM :: MonadVar m => NValueF m (NThunk m) -> m (NValueNF m)
removeEffectsM = fmap Fix . traverse dethunk

prettyNValueF :: MonadVar m => NValueF m (NThunk m) -> m Doc
prettyNValueF = fmap prettyNValueNF . removeEffectsM

prettyNValue :: MonadVar m => NValue m -> m Doc
prettyNValue (NValue _ v) = prettyNValueF v

prettyNValueProv :: MonadVar m => NValue m -> m Doc
prettyNValueProv = \case
    NValue [] v -> prettyNValueF v
    NValue ps v -> do
        v' <- prettyNValueF v
        pure $ v' </> indent 2 (parens (mconcat
            (text "from: " : map (prettyOriginExpr . _originExpr) ps)))

prettyNThunk :: MonadVar m => NThunk m -> m Doc
prettyNThunk = \case
    t@(NThunk ps _) -> do
        v' <- fmap prettyNValueNF (dethunk t)
        pure $ v' </> indent 2 (parens (mconcat
            (text "thunk from: " : map (prettyOriginExpr . _originExpr) ps)))

dethunk :: MonadVar m => NThunk m -> m (NValueNF m)
dethunk = \case
    NThunk _ (Value v) -> removeEffectsM (_baseValue v)
    NThunk _ (Thunk _ active ref) -> do
        nowActive <- atomicModifyVar active (True,)
        if nowActive
            then pure $ Fix $ NVStrF "<thunk>" mempty
            else do
                eres <- readVar ref
                case eres of
                    Computed v -> removeEffectsM (_baseValue v)
                    _ -> pure $ Fix $ NVStrF "<thunk>" mempty
