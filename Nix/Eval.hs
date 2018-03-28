{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Eval (NValue, NValueF(..), ValueSet,
                 MonadNix(..),
                 evalExpr, tracingExprEval,
                 builtin, builtin2,
                 atomText, valueText,
                 buildArgument) where

import           Control.Arrow
import           Control.Monad hiding (mapM, sequence)
import           Control.Monad.Fix
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
data NValueF m r
    = NVConstant NAtom
     -- | A string has a value and a context, which can be used to record what a
     -- string has been build from
    | NVStr Text (DList Text)
    | NVList [r]
    | NVSet (Map.Map Text r)
    | NVFunction (Params (m r)) (m r)
      -- ^ A function is a closed set of terms representing the "call
      --   signature", used at application time to check the type of arguments
      --   passed to the function. Since it supports default values which may
      --   depend on other values within the final argument set, this
      --   dependency is represented as a set of pending evaluations. The
      --   arguments are finally normalized into a set which is passed to the
      --   function.
    | NVLiteralPath FilePath
    | NVEnvPath FilePath
    | NVBuiltin String (NValue m -> m r)
    deriving (Generic, Typeable, Functor)

type NValue m = Fix (NValueF m)

instance Show f => Show (NValueF m f) where
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

type ValueSet m = Map.Map Text (m (NValue m))

builtin :: String -> (NValue m -> m (NValue m)) -> NValue m
builtin name f = Fix (NVBuiltin name f)

builtin2 :: Monad m
         => String -> (NValue m -> NValue m -> m (NValue m)) -> NValue m
builtin2 name f = builtin name (return . builtin name . f)

valueText :: Functor m => NValue m -> (Text, DList Text)
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

valueTextNoContext :: Functor m => NValue m -> Text
valueTextNoContext = fst . valueText

-- | Translate an atom into its nix representation.
atomText :: NAtom -> Text
atomText (NInt i)   = Text.pack (show i)
atomText (NBool b)  = if b then "true" else "false"
atomText NNull      = "null"
atomText (NUri uri) = uri

class MonadFix m => MonadNix m where
    currentScope :: m (ValueSet m)
    newScope :: ValueSet m -> m r -> m r
    importFile :: NValue m -> m (NValue m)

buildArgument :: forall m. MonadNix m
              => Params (m (NValue m)) -> NValue m -> m (ValueSet m)
buildArgument params arg = case params of
    Param name -> return $ Map.singleton name (pure arg)
    ParamSet (FixedParamSet s) m -> go s m
    ParamSet (VariadicParamSet s) m -> go s m
  where
    go s m = case arg of
        Fix (NVSet args) -> do
            let res = loeb (alignWithKey assemble args s)
            return $ maybe res (selfInject res) m
        _ -> error $ "Expected set in function call, received: " ++ show arg

    selfInject res n = Map.insert n (Fix . NVSet <$> sequence res) res

    assemble :: Text -> These (NValue m) (Maybe (m (NValue m)))
             -> Map.Map Text (m (NValue m))
             -> m (NValue m)
    assemble k = \case
        That Nothing  -> error $ "Missing value for parameter: " ++ show k
        That (Just f) -> (`newScope` f)
        This x        -> const (pure x)
        These x _     -> const (pure x)

-- | Evaluate an nix expression, with a given ValueSet as environment
evalExpr :: MonadNix m => NExpr -> m (NValue m)
evalExpr = cata eval

eval :: MonadNix m => NExprF (m (NValue m)) -> m (NValue m)

eval (NSym var) = do
    env <- currentScope
    fromMaybe (error $ "Undefined variable: " ++ show var)
              (Map.lookup var env)

eval (NConstant x)    = return $ Fix $ NVConstant x
eval (NStr str)       = evalString str
eval (NLiteralPath p) = return $ Fix $ NVLiteralPath p
eval (NEnvPath p)     = return $ Fix $ NVEnvPath p

eval (NUnary op arg) = arg >>= \case
    Fix (NVConstant c) -> return $ Fix $ NVConstant $ case (op, c) of
        (NNeg, NInt  i) -> NInt  (-i)
        (NNot, NBool b) -> NBool (not b)
        _ -> error $ "unsupported argument type for unary operator "
                 ++ show op
    _ -> error "argument to unary operator must evaluate to an atomic type"

eval (NBinary op larg rarg) = do
  lval <- larg
  rval <- rarg
  let unsupportedTypes =
          "unsupported argument types for binary operator "
              ++ show (lval, op, rval)
  case (lval, rval) of
   (Fix (NVConstant lc), Fix (NVConstant rc)) ->
       return $ Fix $ NVConstant $ case (op, lc, rc) of
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
   (Fix (NVStr ls lc), Fix (NVStr rs rc)) -> case op of
     NPlus -> return $ Fix $ NVStr (ls `mappend` rs) (lc `mappend` rc)
     _ -> error unsupportedTypes
   (Fix (NVSet ls), Fix (NVSet rs)) -> case op of
     NUpdate -> return $ Fix $ NVSet $ rs `Map.union` ls
     _ -> error unsupportedTypes
   (Fix (NVList ls), Fix (NVList rs)) -> case op of
     NConcat -> return $ Fix $ NVList $ ls ++ rs
     _ -> error unsupportedTypes
   (Fix (NVLiteralPath ls), Fix (NVLiteralPath rs)) -> case op of
     NPlus -> return $ Fix $ NVLiteralPath $ ls ++ rs -- TODO: Canonicalise path
     _ -> error unsupportedTypes
   (Fix (NVLiteralPath ls), Fix (NVStr rs rc)) -> case op of
     -- TODO: Canonicalise path
     NPlus -> return $ Fix $ NVStr (Text.pack ls `mappend` rs) rc
     _ -> error unsupportedTypes
   _ -> error unsupportedTypes

eval (NSelect aset attr alternative) = do
    aset' <- aset
    ks <- evalSelector True attr
    case extract aset' ks of
        Just v  -> return v
        Nothing -> case alternative of
            Just v  -> v
            Nothing -> error $ "could not look up attribute "
                ++ intercalate "." (map show ks) ++ " in " ++ show aset'
  where
    extract (Fix (NVSet s)) (k:ks) = case Map.lookup k s of
        Just v  -> extract v ks
        Nothing -> Nothing
    extract _  (_:_) = Nothing
    extract v     [] = Just v

eval (NHasAttr aset attr) = aset >>= \case
    Fix (NVSet s) -> evalSelector True attr >>= \case
        [keyName] -> return $ Fix $ NVConstant $ NBool $ keyName `Map.member` s
        _ -> error "attr name argument to hasAttr is not a single-part name"
    _ -> error "argument to hasAttr has wrong type"

eval (NList l) = Fix . NVList <$> sequence l

eval (NSet binds) = Fix . NVSet <$> evalBinds True binds

eval (NRecSet binds) = do
    env <- currentScope
    rec evaledBinds <-
            newScope (fmap pure evaledBinds `Map.union` env)
                     (evalBinds True binds)
    return $ Fix . NVSet $ evaledBinds

eval (NLet binds e) = do
    env <- currentScope
    rec evaledBinds <-
            newScope (fmap pure evaledBinds `Map.union` env)
                     (evalBinds True binds)
    newScope (fmap pure evaledBinds `Map.union` env) e

eval (NIf cond t f) = do
    Fix cval <- cond
    case cval of
        NVConstant (NBool True) -> t
        NVConstant (NBool False) -> f
        _ -> error "condition must be a boolean"

eval (NWith scope e) = do
  env <- currentScope
  s <- scope
  case s of
      Fix (NVSet scope') -> newScope (fmap pure scope' `Map.union` env) e
      _ -> error "scope must be a set in with statement"

eval (NAssert cond e) = do
  Fix cond' <- cond
  case cond' of
      (NVConstant (NBool True)) -> e
      (NVConstant (NBool False)) -> error "assertion failed"
      _ -> error "assertion condition must be boolean"

eval (NApp fun x) = do
    fun' <- fun
    case fun' of
        Fix (NVFunction params f) -> do
            arg <- x
            args <- buildArgument params arg
            newScope args f
        Fix (NVBuiltin _ f) -> do
            arg <- x
            f arg
        _ -> error "Attempt to call non-function"

eval (NAbs a b) = do
    env <- currentScope

    -- It is the environment at the definition site, not the call site,
    -- that needs to be used when evaluation the body and the default
    -- arguments
    let extend f = do
            env' <- currentScope
            newScope (env' `Map.union` env) f

    return $ Fix $ NVFunction (fmap extend a) (extend b)

tracingExprEval :: MonadNix m => NExpr -> IO (m (NValue m))
tracingExprEval =
    fmap (runIdentity . snd) . adiM @() (pure <$> eval) psi
  where
    psi k v@(Fix x) = do
        putStrLn $ "Evaluating: " ++ show x
        k v

{-
exprEvalIO :: NExpr -> IO (Context NValue)
exprEvalIO =
    fmap (runIdentity . snd) . adiM @() (pure <$> eval) psi
  where
    psi :: (Fix NExprF -> IO ((), Identity (Context NValue)))
        -> Fix NExprF -> IO ((), Identity (Context NValue))
    psi k v@(Fix (NApp fun x)) = do
        ((), Identity fun) <- k fun
        ((), Identity arg) <- k x
        case (fun, arg) of
            (Fix (NVBuiltin "import" g), Fix (NVLiteralPath path)) -> do
                eres <- parseNixFile path
                case eres of
                    Failure err ->
                        error $ "Import of " ++ path ++ " failed: " ++ show err
                    Success expr -> k expr
            (Fix (NVFunction params f), _) ->
                return $ f (buildArgument params arg)
            (Fix (NVBuiltin _ f), _) -> return $ f arg
            (_, _) -> error "Attempt to call non-function"
    psi k v = k v
-}

evalString :: Monad m => NString (m (NValue m)) -> m (NValue m)
evalString nstr = do
  let fromParts parts = do
        (t, c) <-
          mconcat <$>
          mapM
            (runAntiquoted (return . (, mempty)) (fmap valueText))
            parts
        return (Fix (NVStr t c))
  case nstr of
    Indented parts -> fromParts parts
    DoubleQuoted parts -> fromParts parts

evalBinds :: Monad m
          => Bool -> [Binding (m (NValue m))] -> m (Map.Map Text (NValue m))
evalBinds allowDynamic xs = buildResult <$> sequence (concatMap go xs) where
  buildResult :: [([Text], NValue m)] -> Map.Map Text (NValue m)
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
  go (NamedVar x y) = [liftM2 (,) (evalSelector allowDynamic x) y]
  go _ = [] -- HACK! But who cares right now

evalSelector :: Monad m => Bool -> NAttrPath (m (NValue m)) -> m [Text]
evalSelector dyn = mapM evalKeyName where
  evalKeyName (StaticKey k) = return k
  evalKeyName (DynamicKey k)
    | dyn       = fmap valueTextNoContext . runAntiquoted evalString id $ k
    | otherwise = error "dynamic attribute not allowed in this context"
