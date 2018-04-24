{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Pretty where

import           Control.Monad
import           Data.Fix
import           Data.Functor.Compose
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
  }

-- | A simple expression is never wrapped in parentheses. The expression
--   behaves as if its root operator had a precedence higher than all
--   other operators (including function application).
simpleExpr :: Doc -> NixDoc
simpleExpr = flip NixDoc $ OperatorInfo minBound NAssocNone "simple expr"

-- | An expression that behaves as if its root operator had a precedence lower
--   than all other operators. That ensures that the expression is wrapped in
--   parantheses in almost always, but it's still rendered without parentheses
--   in cases where parentheses are never required (such as in the LHS of a
--   binding).
leastPrecedence :: Doc -> NixDoc
leastPrecedence =
    flip NixDoc $ OperatorInfo maxBound NAssocNone "least precedence"

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

prettyString :: NString NixDoc -> Doc
prettyString (DoubleQuoted parts) = dquotes . hcat . map prettyPart $ parts
  where prettyPart (Plain t)      = text . concatMap escape . unpack $ t
        prettyPart EscapedNewline = text "\n"
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
  prettyPart EscapedNewline = text "\n"
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
    prettyVariadic = if var then [text "..."] else []
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

    render Nothing                       = simpleExpr $ text "_"
    render (Just (NValue Nothing _))     = simpleExpr $ text "?"
    render (Just (NValue (Just expr) _)) = go (originExpr expr)

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
        NixDoc (wrapParens appOp fun <+> wrapParens appOpNonAssoc arg) appOp
    NBinary op r1 r2 -> flip NixDoc opInfo $ hsep
        [ wrapParens (f NAssocLeft) r1
        , text $ unpack $ operatorName opInfo
        , wrapParens (f NAssocRight) r2
        ]
      where
        opInfo = getBinaryOperator op
        f x | associativity opInfo /= x = opInfo { associativity = NAssocNone }
            | otherwise = opInfo
    NUnary op r1 ->
        NixDoc (text (unpack (operatorName opInfo)) <> wrapParens opInfo r1) opInfo
      where opInfo = getUnaryOperator op
    NSelect r attr o ->
      (if isJust o then leastPrecedence else flip NixDoc selectOp) $
          wrapParens selectOp r <> dot <> prettySelector attr <> ordoc
      where ordoc = maybe empty (((space <> text "or") <+>) . wrapParens selectOp) o
    NHasAttr r attr ->
        NixDoc (wrapParens hasAttrOp r <+> text "?" <+> prettySelector attr) hasAttrOp
    NEnvPath p -> simpleExpr $ text ("<" ++ p ++ ">")
    NLiteralPath p -> simpleExpr $ text $ case p of
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

prettyNixValue :: Functor m => NValueNF m -> Doc
prettyNixValue = prettyNix . valueToExpr
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
    dethunk (NThunk (Value v)) = removeEffects (baseValue v)
    dethunk (NThunk _) = Fix $ NVStrF "<thunk>" mempty

removeEffectsM :: MonadVar m => NValueF m (NThunk m) -> m (NValueNF m)
removeEffectsM = fmap Fix . traverse dethunk

renderNValueF :: MonadVar m => NValueF m (NThunk m) -> m Doc
renderNValueF = fmap prettyNixValue . removeEffectsM

renderNValue :: MonadVar m => NValue m -> m Doc
renderNValue = \case
    NValue Nothing v -> renderNValueF v
    NValue (Just p) v -> do
        v' <- renderNValueF v
        -- jww (2018-04-23): Need to display the contextExpr as well.
        pure $ v' </> (text " (from: " <> prettyOriginExpr (originExpr p) <> text ")")

dethunk :: MonadVar m => NThunk m -> m (NValueNF m)
dethunk = \case
    NThunk (Value v) -> removeEffectsM (baseValue v)
    NThunk (Thunk
#if ENABLE_TRACING
                     _
#endif
                     _ t) -> readVar t >>= \case
        Computed v -> removeEffectsM (baseValue v)
        _ -> pure $ Fix $ NVStrF "<thunk>" mempty
