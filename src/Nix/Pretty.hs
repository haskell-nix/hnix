{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
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

import           Control.Applicative ((<|>))
import           Control.Monad
import           Control.Monad.Free
import           Data.Fix
import           Data.HashMap.Lazy (toList)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as HashSet
import           Data.List (isPrefixOf, sort)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isJust, fromMaybe)
import           Data.Text (pack, unpack, replace, strip)
import qualified Data.Text as Text
import           Data.Text.Prettyprint.Doc
import           Nix.Atoms
import           Nix.Expr
import           Nix.Parser
import           Nix.String
import           Nix.Strings
import           Nix.Thunk
#if ENABLE_TRACING
import           Nix.Utils
#else
import           Nix.Utils hiding ((<$>))
#endif
import           Nix.Value
import           Prelude hiding ((<$>))
import           Text.Read (readMaybe)

-- | This type represents a pretty printed nix expression
-- together with some information about the expression.
data NixDoc ann = NixDoc
  { -- | The rendered expression, without any parentheses.
    withoutParens    :: Doc ann

    -- | The root operator is the operator at the root of
    -- the expression tree. For example, in '(a * b) + c', '+' would be the root
    -- operator. It is needed to determine if we need to wrap the expression in
    -- parentheses.
  , rootOp :: OperatorInfo
  , wasPath :: Bool -- This is needed so that when a path is used in a selector path
                    -- we can add brackets appropiately
  }

mkNixDoc :: Doc ann -> OperatorInfo -> NixDoc ann
mkNixDoc d o = NixDoc { withoutParens = d, rootOp = o, wasPath = False }

-- | A simple expression is never wrapped in parentheses. The expression
--   behaves as if its root operator had a precedence higher than all
--   other operators (including function application).
simpleExpr :: Doc ann -> NixDoc ann
simpleExpr d = mkNixDoc d (OperatorInfo minBound NAssocNone "simple expr")

pathExpr :: Doc ann -> NixDoc ann
pathExpr d = (simpleExpr d) { wasPath = True }

-- | An expression that behaves as if its root operator had a precedence lower
--   than all other operators. That ensures that the expression is wrapped in
--   parantheses in almost always, but it's still rendered without parentheses
--   in cases where parentheses are never required (such as in the LHS of a
--   binding).
leastPrecedence :: Doc ann -> NixDoc ann
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

wrapParens :: OperatorInfo -> NixDoc ann -> Doc ann
wrapParens op sub
  | precedence (rootOp sub) < precedence op = withoutParens sub
  | precedence (rootOp sub) == precedence op
    && associativity (rootOp sub) == associativity op
    && associativity op /= NAssocNone = withoutParens sub
  | otherwise = parens $ withoutParens sub

-- Used in the selector case to print a path in a selector as
-- "${./abc}"
wrapPath :: OperatorInfo -> NixDoc ann -> Doc ann
wrapPath op sub =
  if wasPath sub
  then dquotes $ "$" <> braces (withoutParens sub)
  else wrapParens op sub

prettyString :: NString (NixDoc ann)-> Doc ann
prettyString (DoubleQuoted parts) = dquotes . hcat . map prettyPart $ parts
  where prettyPart (Plain t)      = pretty . concatMap escape . unpack $ t
        prettyPart EscapedNewline = "''\\n"
        prettyPart (Antiquoted r) = "$" <> braces (withoutParens r)
        escape '"' = "\\\""
        escape x = maybe [x] (('\\':) . (:[])) $ toEscapeCode x
prettyString (Indented _ parts)
  = group $ nest 2 $ vcat [dsquote, content, dsquote]
 where
  dsquote = squote <> squote
  content = vsep . map prettyLine . stripLastIfEmpty . splitLines $ parts
  stripLastIfEmpty = reverse . f . reverse where
    f ([Plain t] : xs) | Text.null (strip t) = xs
    f xs = xs
  prettyLine = hcat . map prettyPart
  prettyPart (Plain t) = pretty . unpack . replace "${" "''${" . replace "''" "'''" $ t
  prettyPart EscapedNewline = "\\n"
  prettyPart (Antiquoted r) = "$" <> braces (withoutParens r)

prettyParams :: Params (NixDoc ann) -> Doc ann
prettyParams (Param n) = pretty $ unpack n
prettyParams (ParamSet s v mname) = prettyParamSet s v <> case mname of
  Nothing -> mempty
  Just name | Text.null name -> mempty
            | otherwise -> "@" <> pretty (unpack name)

prettyParamSet :: ParamSet (NixDoc ann) -> Bool -> Doc ann
prettyParamSet args var =
    encloseSep (lbrace <> space) (align (space <> rbrace)) sep (map prettySetArg args ++ prettyVariadic)
  where
    prettySetArg (n, maybeDef) = case maybeDef of
      Nothing -> pretty (unpack n)
      Just v -> pretty (unpack n) <+> "?" <+> withoutParens v
    prettyVariadic = ["..." | var]
    sep = align (comma <> space)

prettyBind :: Binding (NixDoc ann) -> Doc ann
prettyBind (NamedVar n v _p) =
    prettySelector n <+> equals <+> withoutParens v <> semi
prettyBind (Inherit s ns _p)
  = "inherit" <+> scope <> align (fillSep (map prettyKeyName ns)) <> semi
 where scope = maybe mempty ((<> space) . parens . withoutParens) s

prettyKeyName :: NKeyName (NixDoc ann) -> Doc ann
prettyKeyName (StaticKey "") = dquotes ""
prettyKeyName (StaticKey key)
  | HashSet.member key reservedNames = dquotes $ pretty $ unpack key
prettyKeyName (StaticKey key) = pretty . unpack $ key
prettyKeyName (DynamicKey key) =
    runAntiquoted (DoubleQuoted [Plain "\n"])
        prettyString (("$" <>) . braces . withoutParens) key

prettySelector :: NAttrPath (NixDoc ann) -> Doc ann
prettySelector = hcat . punctuate dot . map prettyKeyName . NE.toList

prettyAtom :: NAtom -> NixDoc ann
prettyAtom atom = simpleExpr $ pretty $ unpack $ atomText atom

prettyNix :: NExpr -> Doc ann
prettyNix = withoutParens . cata exprFNixDoc

prettyOriginExpr :: NExprLocF (Maybe (NValue m)) -> Doc ann
prettyOriginExpr = withoutParens . go
  where
    go = exprFNixDoc . annotated . getCompose . fmap render

    render Nothing = simpleExpr $ "_"
    render (Just (NValue (reverse -> p:_) _)) = go (_originExpr p)
    render (Just (NValue _ _)) = simpleExpr "?"
        -- simpleExpr $ foldr ((\x y -> vsep [x, y]) . parens . indent 2 . withoutParens
        --                           . go . originExpr)
        --     mempty (reverse ps)

exprFNixDoc :: NExprF (NixDoc ann) -> NixDoc ann
exprFNixDoc = \case
    NConstant atom -> prettyAtom atom
    NStr str -> simpleExpr $ prettyString str
    NList [] -> simpleExpr $ lbracket <> rbracket
    NList xs -> simpleExpr $ group $ nest 2 $ vsep $ concat $
        [ [lbracket]
        , map (wrapParens appOpNonAssoc) xs
        , [rbracket]
        ]
    NSet [] -> simpleExpr $ lbrace <> rbrace
    NSet xs -> simpleExpr $ group $ nest 2 $ vsep $ concat $
        [ [lbrace]
        , map prettyBind xs
        , [rbrace]
        ]
    NRecSet [] -> simpleExpr $ recPrefix <> lbrace <> rbrace
    NRecSet xs -> simpleExpr $ group $ nest 2 $ vsep $ concat $
        [ [recPrefix <> lbrace]
        , map prettyBind xs
        , [rbrace]
        ]
    NAbs args body -> leastPrecedence $ nest 2 $ vsep $
        [ prettyParams args <> colon
        , withoutParens body
        ]
    NBinary NApp fun arg ->
        mkNixDoc (wrapParens appOp fun <+> wrapParens appOpNonAssoc arg) appOp
    NBinary op r1 r2 -> flip mkNixDoc opInfo $ hsep
        [ wrapParens (f NAssocLeft) r1
        , pretty $ unpack $ operatorName opInfo
        , wrapParens (f NAssocRight) r2
        ]
      where
        opInfo = getBinaryOperator op
        f x | associativity opInfo /= x = opInfo { associativity = NAssocNone }
            | otherwise = opInfo
    NUnary op r1 ->
        mkNixDoc (pretty (unpack (operatorName opInfo)) <> wrapParens opInfo r1) opInfo
      where opInfo = getUnaryOperator op
    NSelect r' attr o ->
      (if isJust o then leastPrecedence else flip mkNixDoc selectOp) $
          wrapPath selectOp r <> dot <> prettySelector attr <> ordoc
      where
        r = flip mkNixDoc selectOp $ wrapParens appOpNonAssoc r'
        ordoc = maybe mempty (((space <> "or") <+>) . wrapParens appOpNonAssoc) o
    NHasAttr r attr ->
        mkNixDoc (wrapParens hasAttrOp r <+> "?" <+> prettySelector attr) hasAttrOp
    NEnvPath p -> simpleExpr $ pretty ("<" ++ p ++ ">")
    NLiteralPath p -> pathExpr $ pretty $ case p of
        "./" -> "./."
        "../" -> "../."
        ".." -> "../."
        txt | "/" `isPrefixOf` txt -> txt
            | "~/" `isPrefixOf` txt -> txt
            | "./" `isPrefixOf` txt -> txt
            | "../" `isPrefixOf` txt -> txt
            | otherwise -> "./" ++ txt
    NSym name -> simpleExpr $ pretty (unpack name)
    NLet binds body -> leastPrecedence $ group $ vsep $
      [ "let"
      , indent 2 (vsep (map prettyBind binds))
      , "in" <+> withoutParens body
      ]
    NIf cond trueBody falseBody -> leastPrecedence $
        group $ nest 2 $ vsep $
          [ "if" <+> withoutParens cond
          , align ("then" <+> withoutParens trueBody)
          , align ("else" <+> withoutParens falseBody)
          ]
    NWith scope body -> leastPrecedence $ vsep $
        [ "with"  <+> withoutParens scope <> semi
        , align $ withoutParens body
        ]
    NAssert cond body -> leastPrecedence $ vsep $
        [ "assert" <+> withoutParens cond <> semi
        , align $ withoutParens body
        ]
    NSynHole name -> simpleExpr $ pretty ("^" <> unpack name)
  where
    recPrefix = "rec" <> space

fixate :: Functor f => (a -> f (Fix f)) -> Free f a -> Fix f
fixate g = Fix . go
  where
    go (Pure a) = g a
    go (Free f) = fmap (Fix . go) f

valueToExpr :: Functor m => NValueNF m -> NExpr
valueToExpr = transport go . check
  where
    check :: NValueNF m -> Fix (NValueF m)
    check = fixate $ const $ NVStrF $ principledMakeNixStringWithoutContext "<CYCLE>"

    go (NVConstantF a)     = NConstant a
    go (NVStrF ns)         = NStr (DoubleQuoted [Plain (hackyStringIgnoreContext ns)])
    go (NVListF l)         = NList l
    go (NVSetF s p)        = NSet
        [ NamedVar (StaticKey k :| []) v (fromMaybe nullPos (M.lookup k p))
        | (k, v) <- toList s ]
    go (NVClosureF _ _)    = NSym . pack $ "<closure>"
    go (NVPathF p)         = NLiteralPath p
    go (NVBuiltinF name _) = NSym $ Text.pack $ "builtins." ++ name

prettyNValueNF :: Functor m => NValueNF m -> Doc ann
prettyNValueNF = prettyNix . valueToExpr

printNix :: Functor m => NValueNF m -> String
printNix = iter phi . check
  where
    check :: NValueNF m -> Free (NValueF m) String
    check = fmap (const "<CYCLE>")

    phi :: NValueF m String -> String
    phi (NVConstantF a) = unpack $ atomText a
    phi (NVStrF ns) = show $ hackyStringIgnoreContext ns
    phi (NVListF l) = "[ " ++ unwords l ++ " ]"
    phi (NVSetF s _) =
        "{ " ++ concat [ check (unpack k) ++ " = " ++ v ++ "; "
                       | (k, v) <- sort $ toList s ] ++ "}"
      where
        check v =
            fromMaybe v
            ((fmap (surround . show) (readMaybe v :: Maybe Int))
             <|> (fmap (surround . show) (readMaybe v :: Maybe Float)))
          where
            surround s = "\"" ++ s ++ "\""
    phi NVClosureF {} = "<<lambda>>"
    phi (NVPathF fp) = fp
    phi (NVBuiltinF name _) = "<<builtin " ++ name ++ ">>"

removeEffects :: Functor m => NValueF m (NThunk m) -> NValueNF m
removeEffects = Free . fmap dethunk
  where
    dethunk (NThunk _ (Value v)) = removeEffects (_baseValue v)
    dethunk (NThunk _ _) = Free $ NVStrF $ principledMakeNixStringWithoutContext "<thunk>"

removeEffectsM :: MonadVar m => NValueF m (NThunk m) -> m (NValueNF m)
removeEffectsM = fmap Free . traverse dethunk

prettyNValueF :: MonadVar m => NValueF m (NThunk m) -> m (Doc ann)
prettyNValueF = fmap prettyNValueNF . removeEffectsM

prettyNValue :: MonadVar m => NValue m -> m (Doc ann)
prettyNValue (NValue _ v) = prettyNValueF v

prettyNValueProv :: MonadVar m => NValue m -> m (Doc ann)
prettyNValueProv = \case
    NValue [] v -> prettyNValueF v
    NValue ps v -> do
        v' <- prettyNValueF v
        pure $ fillSep $
          [ v'
          , indent 2 $ parens $ mconcat
            $ "from: "
            : map (prettyOriginExpr . _originExpr) ps
          ]
prettyNThunk :: MonadVar m => NThunk m -> m (Doc ann)
prettyNThunk = \case
    t@(NThunk ps _) -> do
        v' <- fmap prettyNValueNF (dethunk t)
        pure $ fillSep $
          [ v'
          , indent 2 $ parens $ mconcat
            $ "thunk from: "
            : map (prettyOriginExpr . _originExpr) ps
          ]
dethunk :: MonadVar m => NThunk m -> m (NValueNF m)
dethunk = \case
    NThunk _ (Value v) -> removeEffectsM (_baseValue v)
    NThunk _ (Thunk _ active ref) -> do
        nowActive <- atomicModifyVar active (True,)
        if nowActive
            then pure $ Free $ NVStrF $ principledMakeNixStringWithoutContext "<thunk>"
            else do
                eres <- readVar ref
                res <- case eres of
                    Computed v -> removeEffectsM (_baseValue v)
                    _ -> pure $ Free $ NVStrF $ principledMakeNixStringWithoutContext "<thunk>"
                _ <- atomicModifyVar active (False,)
                return res
