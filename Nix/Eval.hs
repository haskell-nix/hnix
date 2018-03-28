{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Eval (NValue, NValueF(..), ValueSet, evalExpr, tracingExprEval,
                 builtin, builtin2, atomText, valueText, buildArgument) where

import           Control.Arrow
import           Data.Align.Key
import           Data.Fix
import           Data.Foldable (foldl')
import           Data.Functor.Identity
import           Data.List (intercalate)
import qualified Data.Map.Lazy as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (appEndo, Endo)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.These
import           Data.Typeable (Typeable)
import           GHC.Generics
import           Nix.Atoms
import           Nix.Expr
import           Nix.StringOperations (runAntiquoted)
import           Nix.Utils

type DList a = Endo [a]

-- | An 'NValue' is the most reduced form of an 'NExpr' after evaluation
-- is completed.
data NValueF r
    = NVConstant NAtom
     -- | A string has a value and a context, which can be used to record what a
     -- string has been build from
    | NVStr Text (DList Text)
    | NVList [r]
    | NVSet (Map.Map Text r)
    | NVFunction (Params (ValueSet -> r)) (ValueSet -> r)
      -- ^ A function's parameters is a closed set of terms representing the
      --   "call signature", used at application time to check the type of
      --   arguments passed to the function. Since it may contain default
      --   values that can depend on other values within the final argument
      --   set, this dependency is represented as a set of pending
      --   evaluations. The arguments are normalized into a set when the
      --   function is finally called.
    | NVLiteralPath FilePath
    | NVEnvPath FilePath
    | NVBuiltin String (NValue -> r)
    deriving (Generic, Typeable, Functor)

instance Show f => Show (NValueF f) where
    showsPrec = flip go where
      go (NVConstant atom)    = showsCon1 "NVConstant" atom
      go (NVStr text context) = showsCon2 "NVStr"      text (appEndo context [])
      go (NVList     list)    = showsCon1 "NVList"     list
      go (NVSet     attrs)    = showsCon1 "NVSet"      attrs
      go (NVFunction r _)     = showsCon1 "NVFunction" (() <$ r)
      go (NVLiteralPath p)    = showsCon1 "NVLiteralPath" p
      go (NVEnvPath p)        = showsCon1 "NVEnvPath" p
      go (NVBuiltin name _)   = showsCon1 "NVBuiltin" name

      showsCon1 :: Show a => String -> a -> Int -> String -> String
      showsCon1 con a d =
          showParen (d > 10) $ showString (con ++ " ") . showsPrec 11 a

      showsCon2 :: (Show a, Show b)
                => String -> a -> b -> Int -> String -> String
      showsCon2 con a b d =
          showParen (d > 10)
              $ showString (con ++ " ")
              . showsPrec 11 a
              . showString " "
              . showsPrec 11 b

type NValue = Fix NValueF

type ValueSet = Map.Map Text NValue

-- | A pending evaluation awaits an attribute environment, and a monadic
--   context, in order to finally evaluate to the resulting value.
type PendingEval = ValueSet -> NValue

builtin :: String -> (NValue -> NValue) -> NValue
builtin name f = Fix (NVBuiltin name f)

builtin2 :: String -> (NValue -> NValue -> NValue) -> NValue
builtin2 name f = builtin name (builtin name . f)

valueText :: NValue -> (Text, DList Text)
valueText = cata phi where
    phi (NVConstant a)    = (atomText a, mempty)
    phi (NVStr t c)       = (t, c)
    phi (NVList _)        = error "Cannot coerce a list to a string"
    phi (NVSet set)
      | Just asString <- Map.lookup "__asString" set = asString
      | otherwise = error "Cannot coerce a set to a string"
    phi (NVFunction _ _)  = error "Cannot coerce a function to a string"
    phi (NVLiteralPath p) = (Text.pack p, mempty)
    phi (NVEnvPath p)     = (Text.pack p, mempty)
    phi (NVBuiltin _ _)    = error "Cannot coerce a function to a string"

valueTextNoContext :: NValue -> Text
valueTextNoContext = fst . valueText

-- | Translate an atom into its nix representation.
atomText :: NAtom -> Text
atomText (NInt i)   = Text.pack (show i)
atomText (NBool b)  = if b then "true" else "false"
atomText NNull      = "null"
atomText (NUri uri) = uri

buildArgument :: Params PendingEval -> NValue -> ValueSet
buildArgument params arg = case params of
    Param name -> Map.singleton name arg
    ParamSet (FixedParamSet s) m -> go s m
    ParamSet (VariadicParamSet s) m -> go s m
  where
    go :: Map.Map Text (Maybe PendingEval) -> Maybe Text -> ValueSet
    go s m = case arg of
        Fix (NVSet args) ->
            let res = loeb (alignWithKey assemble args s) in
            maybe res (\n -> Map.insert n arg res) m
        _ -> error $ "Function call expected set, got: " ++ show arg

    assemble k = \case
        That Nothing ->
            error $ "Missing value for parameter: " ++ Text.unpack k
        That (Just f) -> f
        This x -> const x
        These x _ -> const x

-- | Evaluate an nix expression, with a given ValueSet as environment
evalExpr :: NExpr -> PendingEval
evalExpr = cata phi

phi :: NExprF PendingEval -> PendingEval
phi (NSym var) = fromMaybe err . Map.lookup var
  where err = error ("Undefined variable: " ++ show var)
phi (NConstant x) = const $ Fix $ NVConstant x
phi (NStr str) = evalString str
phi (NLiteralPath p) = const $ Fix $ NVLiteralPath p
phi (NEnvPath p) = const $ Fix $ NVEnvPath p

phi (NUnary op arg) = \env -> arg env & \case
  Fix (NVConstant c) -> Fix $ NVConstant $ case (op, c) of
    (NNeg, NInt  i) -> NInt  (-i)
    (NNot, NBool b) -> NBool (not b)
    _ -> error $ "unsupported argument type for unary operator " ++ show op
  _ -> error "argument to unary operator must evaluate to an atomic type"
phi (NBinary op larg rarg) = \env ->
  let Fix lval = larg env
      Fix rval = rarg env
      unsupportedTypes =
          "unsupported argument types for binary operator "
              ++ show (lval, op, rval)
  in case (lval, rval) of
   (NVConstant lc, NVConstant rc) -> Fix $ NVConstant $ case (op, lc, rc) of
     (NEq,  l, r) -> NBool $ l == r
     (NNEq, l, r) -> NBool $ l /= r
     (NLt,  l, r) -> NBool $ l <  r
     (NLte, l, r) -> NBool $ l <= r
     (NGt,  l, r) -> NBool $ l >  r
     (NGte, l, r) -> NBool $ l >= r
     (NAnd,  NBool l, NBool r) -> NBool $ l && r
     (NOr,   NBool l, NBool r) -> NBool $ l || r
     (NImpl, NBool l, NBool r) -> NBool $ not l || r
     (NPlus,  NInt l, NInt r) -> NInt $ l + r
     (NMinus, NInt l, NInt r) -> NInt $ l - r
     (NMult,  NInt l, NInt r) -> NInt $ l * r
     (NDiv,   NInt l, NInt r) -> NInt $ l `div` r
     _ -> error unsupportedTypes
   (NVStr ls lc, NVStr rs rc) -> case op of
     NPlus -> Fix $ NVStr (ls `mappend` rs) (lc `mappend` rc)
     _ -> error unsupportedTypes
   (NVSet ls, NVSet rs) -> case op of
     NUpdate -> Fix $ NVSet $ rs `Map.union` ls
     _ -> error unsupportedTypes
   (NVList ls, NVList rs) -> case op of
     NConcat -> Fix $ NVList $ ls ++ rs
     _ -> error unsupportedTypes
   (NVLiteralPath ls, NVLiteralPath rs) -> case op of
     NPlus -> Fix $ NVLiteralPath $ ls ++ rs -- TODO: Canonicalise path
     _ -> error unsupportedTypes
   (NVLiteralPath ls, NVStr rs rc) -> case op of
     NPlus -> Fix $ NVStr (Text.pack ls `mappend` rs) rc -- TODO: Canonicalise path
     _ -> error unsupportedTypes
   _ -> error unsupportedTypes

phi (NSelect aset attr alternative) = go where
  go env =
    let aset' = aset env
        ks    = evalSelector True attr env
    in case extract aset' ks of
     Just v  -> v
     Nothing -> case alternative of
       Just v  -> v env
       Nothing -> error $ "could not look up attribute '"
           ++ intercalate "." (map show ks) ++ "' in value " ++ show aset'
  extract (Fix (NVSet s)) (k:ks) = case Map.lookup k s of
                                    Just v  -> extract v ks
                                    Nothing -> Nothing
  extract               _  (_:_) = Nothing
  extract               v     [] = Just v

phi (NHasAttr aset attr) = \env -> aset env & \case
  Fix (NVSet s) -> evalSelector True attr env & \case
    [keyName] -> Fix $ NVConstant $ NBool $ keyName `Map.member` s
    _ -> error "attribute name argument to hasAttr is not a single-part name"
  _ -> error "argument to hasAttr has wrong type"

phi (NList l) = \env ->
    Fix . NVList $ map ($ env) l

phi (NSet binds) =
  Fix . NVSet . evalBinds True binds

phi (NRecSet binds) = \env ->
    let mergedEnv   = evaledBinds `Map.union` env
        evaledBinds = evalBinds True binds mergedEnv
    in Fix . NVSet $ evaledBinds

phi (NLet binds e) = \env ->
    let mergedEnv   = evaledBinds `Map.union` env
        evaledBinds = evalBinds True binds mergedEnv
    in e mergedEnv

phi (NIf cond t f) = \env ->
    let Fix cval = cond env
    in case cval of
        NVConstant (NBool True) -> t env
        NVConstant (NBool False) -> f env
        _ -> error "condition must be a boolean"

phi (NWith scope e) = \env ->
  let s = scope env
  in case s of
      (Fix (NVSet scope')) -> e $ Map.union scope' env
      _ -> error "scope must be a set in with statement"

phi (NAssert cond e) = \env ->
  let Fix cond' = cond env
  in case cond' of
      (NVConstant (NBool True)) -> e env
      (NVConstant (NBool False)) -> error "assertion failed"
      _ -> error "assertion condition must be boolean"

phi (NApp fun x) = \env ->
    let fun' = fun env
    in case fun' of
        Fix (NVFunction params f) ->
            f (buildArgument params (x env))
        Fix (NVBuiltin _ f) -> f (x env)
        _ -> error "Attempt to call non-function"

phi (NAbs a b) = \env ->
    -- It is the environment at the definition site, not the call site,
    -- that needs to be used when evaluation the body and the default
    -- arguments
    let extend f env' = f (env' `Map.union` env)
    in Fix $ NVFunction (fmap extend a) (extend b)

tracingExprEval :: NExpr -> IO PendingEval
tracingExprEval =
    fmap (runIdentity . snd) . adiM @() (pure <$> phi) psi
  where
    psi k v@(Fix x) = do
        putStrLn $ "Evaluating: " ++ show x
        k v

evalString :: NString PendingEval -> PendingEval
evalString nstr env =
  let fromParts parts =
        let (t, c) = mconcat $ map
                (runAntiquoted (, mempty) (valueText . ($ env)))
                parts
        in Fix (NVStr t c)
  in case nstr of
    Indented parts -> fromParts parts
    DoubleQuoted parts -> fromParts parts

evalBinds :: Bool -> [Binding PendingEval] -> ValueSet -> ValueSet
evalBinds allowDynamic xs env = buildResult (concatMap go xs) where
  buildResult :: [([Text], NValue)] -> Map.Map Text NValue
  buildResult = foldl' insert Map.empty . map (first reverse) where
    insert _ ([], _) = error "invalid selector with no components"
    insert m (p:ps, v) = modifyPath ps (insertIfNotMember p v) where
      alreadyDefinedErr = error $ "attribute " ++ attr ++ " already defined"
      attr = show $ Text.intercalate "." $ reverse (p:ps)

      modifyPath [] f = f m
      modifyPath (x:parts) f = modifyPath parts $ \m' -> case Map.lookup x m' of
        Nothing                -> Map.singleton x $ g Map.empty
        Just (Fix (NVSet m'')) -> Map.insert x (g m'') m'
        Just _                 -> alreadyDefinedErr
       where g = Fix . NVSet . f

      insertIfNotMember k x m'
        | Map.notMember k m' = Map.insert k x m'
        | otherwise = alreadyDefinedErr

  -- TODO: Inherit
  go (NamedVar x y) = [(evalSelector allowDynamic x env, y env)]
  go _ = [] -- HACK! But who cares right now

evalSelector :: Bool -> NAttrPath PendingEval -> ValueSet -> [Text]
evalSelector dyn x env = map evalKeyName x where
  evalKeyName (StaticKey k) = k
  evalKeyName (DynamicKey k)
    | dyn       = valueTextNoContext . runAntiquoted (`evalString` env) ($ env) $ k
    | otherwise = error "dynamic attribute not allowed in this context"
