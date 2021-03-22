{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}


module Nix.Pretty where

import           Prelude                  hiding ( toList, group )
import           Nix.Utils
import           Control.Monad.Free             ( Free(Free) )
import           Data.Fix                       ( Fix(..)
                                                , foldFix )
import           Data.HashMap.Lazy              ( toList )
import qualified Data.HashMap.Lazy             as M
import qualified Data.HashSet                  as HashSet
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( pack
                                                , replace
                                                , strip
                                                )
import qualified Data.Text                     as Text
import           Prettyprinter           hiding ( list )
import           Nix.Atoms
import           Nix.Cited
import           Nix.Expr
import           Nix.Expr.Strings
import           Nix.Normal
import           Nix.Parser
import           Nix.String
import           Nix.Thunk
import           Nix.Value

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
                    -- we can add brackets appropriately
  }

mkNixDoc :: OperatorInfo -> Doc ann -> NixDoc ann
mkNixDoc o d = NixDoc { withoutParens = d, rootOp = o, wasPath = False }

-- | A simple expression is never wrapped in parentheses. The expression
--   behaves as if its root operator had a precedence higher than all
--   other operators (including function application).
simpleExpr :: Doc ann -> NixDoc ann
simpleExpr = mkNixDoc (OperatorInfo minBound NAssocNone "simple expr")

pathExpr :: Doc ann -> NixDoc ann
pathExpr d = (simpleExpr d) { wasPath = True }

-- | An expression that behaves as if its root operator had a precedence lower
--   than all other operators. That ensures that the expression is wrapped in
--   parentheses in almost always, but it's still rendered without parentheses
--   in cases where parentheses are never required (such as in the LHS of a
--   binding).
leastPrecedence :: Doc ann -> NixDoc ann
leastPrecedence =
  mkNixDoc (OperatorInfo maxBound NAssocNone "least precedence")

appOp :: OperatorInfo
appOp = getBinaryOperator NApp

appOpNonAssoc :: OperatorInfo
appOpNonAssoc = (getBinaryOperator NApp) { associativity = NAssocNone }

selectOp :: OperatorInfo
selectOp = getSpecialOperator NSelectOp

hasAttrOp :: OperatorInfo
hasAttrOp = getSpecialOperator NHasAttrOp

wrapParens :: OperatorInfo -> NixDoc ann -> Doc ann
wrapParens op sub =
  bool
    (\ a -> "(" <> a <> ")")
    id
    (precedence (rootOp sub)       < precedence op
    || (precedence (rootOp sub)   == precedence op
        && associativity (rootOp sub) == associativity op
        && associativity op /= NAssocNone)
    )
    (withoutParens sub)

-- Used in the selector case to print a path in a selector as
-- "${./abc}"
wrapPath :: OperatorInfo -> NixDoc ann -> Doc ann
wrapPath op sub =
  bool
    (wrapParens op sub)
    ("\"${" <> withoutParens sub <> "}\"")
    (wasPath sub)

prettyString :: NString (NixDoc ann) -> Doc ann
prettyString (DoubleQuoted parts) = "\"" <> (mconcat . fmap prettyPart $ parts) <> "\""
 where
  -- It serializes Text -> String, because the helper code is done for String,
  -- please, can someone break that code.
  prettyPart (Plain t)      = pretty . concatMap escape . toString $ t
  prettyPart EscapedNewline = "''\\n"
  prettyPart (Antiquoted r) = "${" <> withoutParens r <> "}"
  escape '"' = "\\\""
  escape x   =
    maybe
      [x]
      (('\\' :) . (: mempty))
      (toEscapeCode x)
prettyString (Indented _ parts) = group $ nest 2 $ vcat
  ["''", content, "''"]
 where
  content = vsep . fmap prettyLine . stripLastIfEmpty . splitLines $ parts
  stripLastIfEmpty = reverse . f . reverse   where
    f ([Plain t] : xs) | Text.null (strip t) = xs
    f xs = xs
  prettyLine = hcat . fmap prettyPart
  prettyPart (Plain t) =
    pretty . replace "${" "''${" . replace "''" "'''" $ t
  prettyPart EscapedNewline = "\\n"
  prettyPart (Antiquoted r) = "${" <> withoutParens r <> "}"

prettyParams :: Params (NixDoc ann) -> Doc ann
prettyParams (Param n           ) = pretty n
prettyParams (ParamSet s v mname) = prettyParamSet s v <>
  maybe
    mempty
    (\ name ->
       bool
         mempty
         ("@" <> pretty name)
         (not (Text.null name))
    )
    mname

prettyParamSet :: ParamSet (NixDoc ann) -> Bool -> Doc ann
prettyParamSet args var =
  encloseSep
    "{ "
    (align " }")
    sep
    (fmap prettySetArg args <> prettyVariadic)
 where
  prettySetArg (n, maybeDef) =
    maybe
      (pretty n)
      (\x -> pretty n <> " ? " <> withoutParens x)
      maybeDef
  prettyVariadic = [ "..." | var ]
  sep            = align ", "

prettyBind :: Binding (NixDoc ann) -> Doc ann
prettyBind (NamedVar n v _p) =
  prettySelector n <> " = " <> withoutParens v <> ";"
prettyBind (Inherit s ns _p) =
  "inherit " <> scope <> align (fillSep (fmap prettyKeyName ns)) <> ";"
  where
    scope =
      maybe
        mempty
        ((<> " ") . parens . withoutParens)
        s

prettyKeyName :: NKeyName (NixDoc ann) -> Doc ann
prettyKeyName (StaticKey "") = "\"\""
prettyKeyName (StaticKey key) | HashSet.member key reservedNames = "\"" <> pretty key <> "\""
prettyKeyName (StaticKey  key) = pretty key
prettyKeyName (DynamicKey key) =
  runAntiquoted
    (DoubleQuoted [Plain "\n"])
    prettyString
    (\ x -> "${" <> withoutParens x <> "}")
    key

prettySelector :: NAttrPath (NixDoc ann) -> Doc ann
prettySelector = hcat . punctuate "." . fmap prettyKeyName . NE.toList

prettyAtom :: NAtom -> NixDoc ann
prettyAtom atom = simpleExpr $ pretty $ atomText atom

prettyNix :: NExpr -> Doc ann
prettyNix = withoutParens . foldFix exprFNixDoc

prettyOriginExpr
  :: forall t f m ann
   . HasCitations1 m (NValue t f m) f
  => NExprLocF (Maybe (NValue t f m))
  -> Doc ann
prettyOriginExpr = withoutParens . go
 where
  go = exprFNixDoc . annotated . getCompose . fmap render

  render :: Maybe (NValue t f m) -> NixDoc ann
  render Nothing = simpleExpr "_"
  render (Just (Free (reverse . citations @m -> p:_))) = go (_originExpr p)
  render _       = simpleExpr "?"
    -- render (Just (NValue (citations -> ps))) =
        -- simpleExpr $ foldr ((\x y -> vsep [x, y]) . parens . indent 2 . withoutParens
        --                           . go . originExpr)
        --     mempty (reverse ps)

exprFNixDoc :: NExprF (NixDoc ann) -> NixDoc ann
exprFNixDoc = \case
  NConstant atom -> prettyAtom atom
  NStr      str  -> simpleExpr $ prettyString str
  NList xs ->
    prettyContainer "[" (wrapParens appOpNonAssoc) "]" xs
  NSet NNonRecursive xs ->
    prettyContainer "{" prettyBind "}" xs
  NSet NRecursive xs ->
    prettyContainer "rec {" prettyBind "}" xs
  NAbs args body ->
    leastPrecedence $
      nest 2 $
        vsep
          [ prettyParams args <> ":"
          , withoutParens body
          ]
  NBinary NApp fun arg ->
    mkNixDoc appOp (wrapParens appOp fun <> " " <> wrapParens appOpNonAssoc arg)
  NBinary op r1 r2 ->
    mkNixDoc
      opInfo $
      hsep
        [ wrapParens (f NAssocLeft) r1
        , pretty $ operatorName opInfo
        , wrapParens (f NAssocRight) r2
        ]
   where
    opInfo = getBinaryOperator op
    f x | associativity opInfo /= x = opInfo { associativity = NAssocNone }
        | otherwise                 = opInfo
  NUnary op r1 ->
    mkNixDoc
      opInfo
      (pretty (operatorName opInfo) <> wrapParens opInfo r1)
    where opInfo = getUnaryOperator op
  NSelect r' attr o ->
    maybe
      (mkNixDoc selectOp)
      (const leastPrecedence)
      o
      $ wrapPath selectOp r <> "." <> prettySelector attr <> ordoc
   where
    r     = mkNixDoc selectOp (wrapParens appOpNonAssoc r')
    ordoc =
      maybe
        mempty
        ((" or " <>) . wrapParens appOpNonAssoc)
        o
  NHasAttr r attr ->
    mkNixDoc hasAttrOp (wrapParens hasAttrOp r <> " ? " <> prettySelector attr)
  NEnvPath     p -> simpleExpr $ pretty ("<" <> p <> ">")
  NLiteralPath p ->
    pathExpr $
      pretty $
        case p of
          "./"  -> "./."
          "../" -> "../."
          ".."  -> "../."
          _txt  ->
            bool
              ("./" <> _txt)
              _txt
              (any (`isPrefixOf` _txt) ["/", "~/", "./", "../"])
  NSym name -> simpleExpr $ pretty name
  NLet binds body ->
    leastPrecedence $
      group $
        vsep
          [ "let"
          , indent 2 (vsep (fmap prettyBind binds))
          , "in " <> withoutParens body
          ]
  NIf cond trueBody falseBody ->
    leastPrecedence $
      group $
        nest 2 $
          sep
            [ "if " <> withoutParens cond
            , align ("then " <> withoutParens trueBody)
            , align ("else " <> withoutParens falseBody)
            ]
  NWith scope body ->
    prettyAddScope "with " scope body
  NAssert cond body ->
    prettyAddScope "assert " cond body
  NSynHole name -> simpleExpr $ pretty ("^" <> name)
 where
  prettyContainer h f t c =
    list
      (simpleExpr (h <> t))
      (const $ simpleExpr $ group $ nest 2 $ vsep $ [h] <> (f <$> c) <> [t])
      c

  prettyAddScope h c b =
    leastPrecedence $
      vsep
        [h <> withoutParens c <> ";", align $ withoutParens b]


valueToExpr :: forall t f m . MonadDataContext f m => NValue t f m -> NExpr
valueToExpr = iterNValue (\_ _ -> thk) phi
 where
  thk = Fix . NSym $ "<expr>"

  phi :: NValue' t f m NExpr -> NExpr
  phi (NVConstant' a ) = Fix $ NConstant a
  phi (NVStr'      ns) = mkStr ns
  phi (NVList'     l ) = Fix $ NList l
  phi (NVSet' s p    ) = Fix $ NSet NNonRecursive
    [ NamedVar (StaticKey k :| mempty) v (fromMaybe nullPos (M.lookup k p))
    | (k, v) <- toList s
    ]
  phi (NVClosure' _ _   ) = Fix . NSym $ "<closure>"
  phi (NVPath' p        ) = Fix $ NLiteralPath p
  phi (NVBuiltin' name _) = Fix . NSym $ "builtins." <> pack name

  mkStr ns = Fix $ NStr $ DoubleQuoted [Plain (stringIgnoreContext ns)]

prettyNValue
  :: forall t f m ann . MonadDataContext f m => NValue t f m -> Doc ann
prettyNValue = prettyNix . valueToExpr

prettyNValueProv
  :: forall t f m ann
   . ( HasCitations m (NValue t f m) t
     , HasCitations1 m (NValue t f m) f
     , MonadThunk t m (NValue t f m)
     , MonadDataContext f m
     )
  => NValue t f m
  -> Doc ann
prettyNValueProv v =
  case citations @m @(NValue t f m) v of
    [] -> prettyNVal
    ps ->
      fillSep
        [ prettyNVal
        , indent 2 $
          "(" <> mconcat ("from: ":(prettyOriginExpr . _originExpr <$> ps)) <> ")"
        ]
 where
  prettyNVal = prettyNValue v

prettyNThunk
  :: forall t f m ann
   . ( HasCitations m (NValue t f m) t
     , HasCitations1 m (NValue t f m) f
     , MonadThunk t m (NValue t f m)
     , MonadDataContext f m
     )
  => t
  -> m (Doc ann)
prettyNThunk t =
  do
    let ps = citations @m @(NValue t f m) @t t
    v' <- prettyNValue <$> dethunk t
    pure
      $ fillSep
          [ v'
          , indent 2 $
              "(" <> mconcat ( "thunk from: " : fmap (prettyOriginExpr . _originExpr) ps) <> ")"
          ]

-- | This function is used only by the testing code.
printNix :: forall t f m . MonadDataContext f m => NValue t f m -> String
printNix = iterNValue (\_ _ -> thk) phi
 where
  thk = "<thunk>"

  -- Please, reduce this horrifying String -> Text -> String marshaling in favour of Text
  phi :: NValue' t f m String -> String
  phi (NVConstant' a ) = toString $ atomText a
  phi (NVStr'      ns) = show $ stringIgnoreContext ns
  phi (NVList'     l ) = toString $ "[ " <> unwords (fmap pack l) <> " ]"
  phi (NVSet' s _) =
    "{ " <>
      concat
        [ check (toString k) <> " = " <> v <> "; "
        | (k, v) <- sort $ toList s
        ] <> "}"
   where
    check :: [Char] -> [Char]
    check v =
      fromMaybe
        v
        (fmap (surround . show) (readMaybe v :: Maybe Int)
        <|> fmap (surround . show) (readMaybe v :: Maybe Float)
        )
      where surround s = "\"" <> s <> "\""
  phi NVClosure'{}        = "<<lambda>>"
  phi (NVPath' fp       ) = fp
  phi (NVBuiltin' name _) = "<<builtin " <> name <> ">>"
