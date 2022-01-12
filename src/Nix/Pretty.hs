{-# language CPP #-}
{-# language AllowAmbiguousTypes #-}

{-# options_ghc -fno-warn-name-shadowing #-}

module Nix.Pretty where

import           Nix.Prelude             hiding ( toList, group )
import           Control.Monad.Free             ( Free(Free) )
import           Data.Fix                       ( Fix(..)
                                                , foldFix )
import           Data.HashMap.Lazy              ( toList )
import qualified Data.HashMap.Lazy             as M
import qualified Data.HashSet                  as HashSet
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( replace
                                                , strip
                                                )
import qualified Data.Text                     as Text
import           Prettyprinter           hiding ( list )
import           Nix.Atoms
import           Nix.Cited
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Expr.Strings
import           Nix.Normal
import           Nix.Parser
import           Nix.String
import           Nix.Thunk
import           Nix.Value

-- | This type represents a pretty printed nix expression
-- together with some information about the expression.
data NixDoc ann = NixDoc
  { -- | Rendered expression. Without surrounding parenthesis.
    getDoc :: Doc ann

    -- | The root operator is the operator at the root of
    -- the expression tree. For example, in '(a * b) + c', '+' would be the root
    -- operator. It is needed to determine if we need to wrap the expression in
    -- parentheses.
  , rootOp :: OperatorInfo
  , wasPath :: Bool -- This is needed so that when a path is used in a selector path
                    -- we can add brackets appropriately
  }

-- | Represent Nix antiquotes.
--
-- >
-- > ${ expr }
-- >
antiquote :: NixDoc ann -> Doc ann
antiquote x = "${" <> getDoc x <> "}"

mkNixDoc :: OperatorInfo -> Doc ann -> NixDoc ann
mkNixDoc o d = NixDoc { getDoc = d, rootOp = o, wasPath = False }

-- | A simple expression is never wrapped in parentheses. The expression
--   behaves as if its root operator had a precedence higher than all
--   other operators (including function application).
simpleExpr :: Doc ann -> NixDoc ann
simpleExpr = mkNixDoc $ OperatorInfo minBound NAssocNone "simple expr"

pathExpr :: Doc ann -> NixDoc ann
pathExpr d = (simpleExpr d) { wasPath = True }

-- | An expression that behaves as if its root operator had a precedence lower
--   than all other operators. That ensures that the expression is wrapped in
--   parentheses in almost always, but it's still rendered without parentheses
--   in cases where parentheses are never required (such as in the LHS of a
--   binding).
leastPrecedence :: Doc ann -> NixDoc ann
leastPrecedence =
  mkNixDoc $ OperatorInfo maxBound NAssocNone "least precedence"

appOp :: OperatorInfo
appOp = getBinaryOperator NApp

appOpNonAssoc :: OperatorInfo
appOpNonAssoc = appOp { associativity = NAssocNone }

selectOp :: OperatorInfo
selectOp = getSpecialOperator NSelectOp

hasAttrOp :: OperatorInfo
hasAttrOp = getSpecialOperator NHasAttrOp

-- | Determine if to return doc wraped into parens,
-- according the given operator.
precedenceWrap :: OperatorInfo -> NixDoc ann -> Doc ann
precedenceWrap op subExpr =
  maybeWrap $ getDoc subExpr
 where
  maybeWrap :: Doc ann -> Doc ann
  maybeWrap =
    bool
      parens
      id
      needsParens
   where
    needsParens :: Bool
    needsParens =
      precedence root < precedence op
      || (  precedence    root == precedence    op
         && associativity root == associativity op
         && associativity op   /= NAssocNone
         )

    root = rootOp subExpr


-- Used in the selector case to print a path in a selector as
-- "${./abc}"
wrapPath :: OperatorInfo -> NixDoc ann -> Doc ann
wrapPath op sub =
  bool
    (precedenceWrap op sub)
    (dquotes $ antiquote sub)
    (wasPath sub)

-- | Handle Output representation of the string escape codes.
prettyString :: NString (NixDoc ann) -> Doc ann
prettyString (DoubleQuoted parts) =
  dquotes $ foldMap prettyPart parts
 where
  prettyPart (Plain t)      = pretty $ escapeString t
  prettyPart EscapedNewline = "''\\n"
  prettyPart (Antiquoted r) = antiquote r
prettyString (Indented _ parts) =
  group $ nest 2 $ vcat ["''", content, "''"]
 where
  content = vsep . fmap prettyLine . stripLastIfEmpty . splitLines $ parts
  stripLastIfEmpty :: [[Antiquoted Text r]] -> [[Antiquoted Text r]]
  stripLastIfEmpty =
    filter flt
   where
    flt :: [Antiquoted Text r] -> Bool
    flt [Plain t] | Text.null (strip t) = False
    flt _ = True

  prettyLine :: [Antiquoted Text (NixDoc ann)] -> Doc ann
  prettyLine =
    hcat . fmap prettyPart
   where
    prettyPart :: Antiquoted Text (NixDoc ann) -> Doc ann
    prettyPart (Plain t) =
      pretty . replace "${" "''${" . replace "''" "'''" $ t
    prettyPart EscapedNewline = "\\n"
    prettyPart (Antiquoted r) = antiquote r

prettyVarName :: VarName -> Doc ann
prettyVarName = pretty @Text . coerce

prettyParams :: Params (NixDoc ann) -> Doc ann
prettyParams (Param n           ) = prettyVarName n
prettyParams (ParamSet mname variadic pset) =
  prettyParamSet variadic pset <>
    toDoc `whenJust` mname
 where
  toDoc :: VarName -> Doc ann
  toDoc (coerce -> name) =
    ("@" <> pretty name) `whenFalse` Text.null name

prettyParamSet :: forall ann . Variadic -> ParamSet (NixDoc ann) -> Doc ann
prettyParamSet variadic args =
  encloseSep
    "{ "
    (align " }")
    (align ", ")
    (fmap prettySetArg args <> one "..." `whenTrue` (variadic == Variadic))
 where
  prettySetArg :: (VarName, Maybe (NixDoc ann)) -> Doc ann
  prettySetArg (n, maybeDef) =
    (prettyVarName n <>) $ ((" ? " <>) . getDoc) `whenJust` maybeDef

prettyBind :: Binding (NixDoc ann) -> Doc ann
prettyBind (NamedVar n v _p) =
  prettySelector n <> " = " <> getDoc v <> ";"
prettyBind (Inherit s ns _p) =
  "inherit " <> scope <> align (fillSep $ prettyVarName <$> ns) <> ";"
 where
  scope =
    ((<> " ") . parens . getDoc) `whenJust` s

prettyKeyName :: NKeyName (NixDoc ann) -> Doc ann
prettyKeyName (StaticKey key) =
  bool
    "\"\""
    (bool
      id
      dquotes
      (HashSet.member key reservedNames)
      (prettyVarName key)
    )
    (not $ Text.null $ coerce key)
prettyKeyName (DynamicKey key) =
  runAntiquoted
    (DoubleQuoted $ one $ Plain "\n")
    prettyString
    antiquote
    key

prettySelector :: NAttrPath (NixDoc ann) -> Doc ann
prettySelector = hcat . punctuate "." . fmap prettyKeyName . NE.toList

prettyAtom :: NAtom -> NixDoc ann
prettyAtom = simpleExpr . pretty . atomText

prettyNix :: NExpr -> Doc ann
prettyNix = getDoc . foldFix exprFNixDoc

prettyOriginExpr
  :: forall t f m ann
   . HasCitations1 m (NValue t f m) f
  => NExprLocF (Maybe (NValue t f m))
  -> Doc ann
prettyOriginExpr = getDoc . go
 where
  go :: NExprLocF (Maybe (NValue t f m)) -> NixDoc ann
  go = exprFNixDoc . stripAnnF . fmap render
   where
    render :: Maybe (NValue t f m) -> NixDoc ann
    render Nothing = simpleExpr "_"
    render (Just (Free (reverse . citations @m -> p:_))) = go (getOriginExpr p)
    render _       = simpleExpr "?"
      -- render (Just (NValue (citations -> ps))) =
          -- simpleExpr $ foldr ((\x y -> vsep [x, y]) . parens . indent 2 . getDoc
          --                           . go . originExpr)
          --     mempty (reverse ps)

-- | Takes original expression from inside provenance information.
-- Prettifies that expression.
prettyExtractFromProvenance
  :: forall t f m ann
   . HasCitations1 m (NValue t f m) f
  => [Provenance m (NValue t f m)] -> Doc ann
prettyExtractFromProvenance =
  sep .
    fmap (prettyOriginExpr . getOriginExpr)

exprFNixDoc :: forall ann . NExprF (NixDoc ann) -> NixDoc ann
exprFNixDoc = \case
  NConstant atom -> prettyAtom atom
  NStr      str  -> simpleExpr $ prettyString str
  NList xs ->
    prettyContainer "[" (precedenceWrap appOpNonAssoc) "]" xs
  NSet NonRecursive xs ->
    prettyContainer "{" prettyBind "}" xs
  NSet Recursive xs ->
    prettyContainer "rec {" prettyBind "}" xs
  NAbs args body ->
    leastPrecedence $
      nest 2 $
        vsep
          [ prettyParams args <> ":"
          , getDoc body
          ]
  NBinary NApp fun arg ->
    mkNixDoc appOp (precedenceWrap appOp fun <> " " <> precedenceWrap appOpNonAssoc arg)
  NBinary op r1 r2 ->
    mkNixDoc
      opInfo $
      hsep
        [ f NAssocLeft r1
        , pretty $ operatorName opInfo
        , f NAssocRight r2
        ]
   where
    opInfo = getBinaryOperator op
    f :: NAssoc -> NixDoc ann -> Doc ann
    f x =
      precedenceWrap
        $ bool
            opInfo
            (opInfo { associativity = NAssocNone })
            (associativity opInfo /= x)
  NUnary op r1 ->
    mkNixDoc
      opInfo $
      pretty (operatorName opInfo) <> precedenceWrap opInfo r1
   where
    opInfo = getUnaryOperator op
  NSelect o r' attr ->
    maybe
      (mkNixDoc selectOp)
      (const leastPrecedence)
      o
      $ wrapPath selectOp (mkNixDoc selectOp (precedenceWrap appOpNonAssoc r')) <> "." <> prettySelector attr <>
        ((" or " <>) . precedenceWrap appOpNonAssoc) `whenJust` o
  NHasAttr r attr ->
    mkNixDoc hasAttrOp (precedenceWrap hasAttrOp r <> " ? " <> prettySelector attr)
  NEnvPath     p -> simpleExpr $ pretty @String $ "<" <> coerce p <> ">"
  NLiteralPath p ->
    pathExpr $
      pretty @FilePath $ coerce $
        case p of
          "./"  -> "./."
          "../" -> "../."
          ".."  -> "../."
          path  ->
            bool
              ("./" <> path)
              path
              (any (`isPrefixOf` coerce path) ["/", "~/", "./", "../"])
  NSym name -> simpleExpr $ prettyVarName name
  NLet binds body ->
    leastPrecedence $
      group $
        vsep
          [ "let"
          , indent 2 (vsep $ fmap prettyBind binds)
          , "in " <> getDoc body
          ]
  NIf cond trueBody falseBody ->
    leastPrecedence $
      group $
        nest 2 $
          ifThenElse getDoc
   where
    ifThenElse :: (NixDoc ann -> Doc ann) -> Doc ann
    ifThenElse wp =
      sep
        [ "if " <> wp cond
        , align ("then " <> wp trueBody)
        , align ("else " <> wp falseBody)
        ]
  NWith scope body ->
    prettyAddScope "with " scope body
  NAssert cond body ->
    prettyAddScope "assert " cond body
  NSynHole name -> simpleExpr $ pretty @Text ("^" <> coerce name)
 where
  prettyContainer h f t c =
    list
      (simpleExpr (h <> t))
      (const $ simpleExpr $ group $ nest 2 $ vsep $ one h <> (f <$> c) <> one t)
      c

  prettyAddScope h c b =
    leastPrecedence $
      vsep
        [h <> getDoc c <> ";", align $ getDoc b]


valueToExpr :: forall t f m . MonadDataContext f m => NValue t f m -> NExpr
valueToExpr = iterNValueByDiscardWith thk (Fix . phi)
 where
  thk = Fix . NSym $ "<expr>"

  phi :: NValue' t f m NExpr -> NExprF NExpr
  phi (NVConstant' a     ) = NConstant a
  phi (NVStr'      ns    ) = NStr $ DoubleQuoted $ one $ Plain $ ignoreContext ns
  phi (NVList'     l     ) = NList l
  phi (NVSet'      p    s) = NSet mempty
    [ NamedVar (one $ StaticKey k) v (fromMaybe nullPos $ (`M.lookup` p) k)
    | (k, v) <- toList s
    ]
  phi (NVClosure'  _    _) = NSym "<closure>"
  phi (NVPath'     p     ) = NLiteralPath p
  phi (NVBuiltin'  name _) = NSym $ coerce ((mappend @Text) "builtins.") name

prettyNValue
  :: forall t f m ann . MonadDataContext f m => NValue t f m -> Doc ann
prettyNValue = prettyNix . valueToExpr

-- | During the output, which can print only representation of value,
-- lazy thunks need to looked into & so - be evaluated (*sic)
-- This type is a simple manual witness "is the thunk gets shown".
data ValueOrigin = WasThunk | Value
 deriving Eq

prettyProv
  :: forall t f m ann
   . ( HasCitations m (NValue t f m) t
     , HasCitations1 m (NValue t f m) f
     , MonadThunk t m (NValue t f m)
     , MonadDataContext f m
     )
  => ValueOrigin  -- ^ Was thunk?
  -> NValue t f m
  -> Doc ann
prettyProv wasThunk v =
  list
    id
    (\ ps pv ->
      fillSep
        [ pv
        , indent 2 $
          "(" <> ("thunk " `whenTrue` (wasThunk == WasThunk) <> "from: " <> prettyExtractFromProvenance ps) <> ")"
        ]
    )
    (citations @m @(NValue t f m) v)
    (prettyNValue v)

prettyNValueProv
  :: forall t f m ann
   . ( HasCitations m (NValue t f m) t
     , HasCitations1 m (NValue t f m) f
     , MonadThunk t m (NValue t f m)
     , MonadDataContext f m
     )
  => NValue t f m
  -> Doc ann
prettyNValueProv =
  prettyProv Value

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
  prettyProv WasThunk <$> dethunk t

-- | This function is used only by the testing code.
printNix :: forall t f m . MonadDataContext f m => NValue t f m -> Text
printNix =
  iterNValueByDiscardWith thunkStubText phi
 where
  phi :: NValue' t f m Text -> Text
  phi (NVConstant' a ) = atomText a
  phi (NVStr'      ns) = "\"" <> escapeString (ignoreContext ns) <> "\""
  phi (NVList'     l ) = "[ " <> unwords l <> " ]"
  phi (NVSet' _ s) =
    "{ " <>
      fold
        [ check k <> " = " <> v <> "; "
        | (coerce -> k, v) <- sort $ toList s
        ] <> "}"
   where
    check :: Text -> Text
    check v =
      fromMaybe
        v
        (tryRead @Int <|> tryRead @Float)
     where
      tryRead :: forall a . (Read a, Show a) => Maybe Text
      tryRead = fmap ((\ s -> "\"" <> s <> "\"") . show) $ readMaybe @a $ toString v
  phi NVClosure'{}        = "<<lambda>>"
  phi (NVPath' fp       ) = fromString $ coerce fp
  phi (NVBuiltin' name _) = "<<builtin " <> coerce name <> ">>"
