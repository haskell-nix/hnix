{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Nix.Eval where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad hiding (mapM, sequence)
import           Data.Fix
import           Data.Foldable (foldl')
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable as T
import           Nix.Types
import           Prelude hiding (mapM, sequence)

buildArgument :: Formals NValue -> NValue -> NValue
buildArgument paramSpec arg = either error (Fix . NVSet) $ case paramSpec of
    FormalName name -> return $ Map.singleton name arg
    FormalSet s Nothing -> lookupParamSet s
    FormalSet s (Just name) -> Map.insert name arg <$> lookupParamSet s
  where
    go env k def = maybe (Left err) return $ Map.lookup k env <|> def
      where err = "Could not find " ++ show k

    lookupParamSet fps = case fps of
      FixedParamSet s -> case arg of
        Fix (NVSet env) -> Map.traverseWithKey (go env) s
        _               -> Left "Unexpected function environment"
      _ -> error "Can't yet handle variadic param sets"

evalExpr :: NExpr -> NValue -> IO NValue
evalExpr = cata phi
  where
    phi :: NExprF (NValue -> IO NValue) -> NValue -> IO NValue
    phi (NSym var) = \env -> case env of
      Fix (NVSet s) -> maybe err return $ Map.lookup var s
      _ -> error "invalid evaluation environment"
     where err = error ("Undefined variable: " ++ show var)
    phi (NConstant x) = const $ return $ Fix $ NVConstant x
    phi (NStr str) = fmap (Fix . NVStr) . flip evalString str

    phi (NOper x) = \env -> case x of
      NUnary op arg -> arg env >>= \case
        Fix (NVConstant c) -> pure $ Fix $ NVConstant $ case (op, c) of
          (NNeg, NInt  i) -> NInt  (-i)
          (NNot, NBool b) -> NBool (not b)
          _               -> error $ "unsupported argument type for unary operator " ++ show op
        _ -> error $ "argument to unary operator must evaluate to an atomic type"
      NBinary op larg rarg -> do
        lval <- larg env
        rval <- rarg env
        case (lval, rval) of
         (Fix (NVConstant lc), Fix (NVConstant rc)) -> pure $ Fix $ NVConstant $ case (op, lc, rc) of
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
           _ -> error $ "unsupported argument types for binary operator " ++ show op
         (Fix (NVStr ls), Fix (NVStr rs)) -> case op of
           NConcat -> pure $ Fix $ NVStr $ ls `mappend` rs
           _ -> error $ "unsupported argument types for binary operator " ++ show op
         (Fix (NVSet ls), Fix (NVSet rs)) -> case op of
           NUpdate -> pure $ Fix $ NVSet $ rs `Map.union` ls
           _ -> error $ "unsupported argument types for binary operator " ++ show op
         _ -> error $ "unsupported argument types for binary operator " ++ show op

    phi (NSelect aset attr alternative) = go where
      go = \env -> do
        aset' <- aset env
        ks    <- evalSelector True env attr
        case extract aset' ks of
         Just v  -> pure v
         Nothing -> case alternative of
           Just v  -> v env
           Nothing -> error "could not look up attribute in value"
      extract (Fix (NVSet s)) (k:ks) = case (Map.lookup k s) of
                                        Just v  -> extract v ks
                                        Nothing -> Nothing
      extract               _  (_:_) = Nothing
      extract               v     [] = Just v

    phi (NHasAttr aset attr) = \env -> aset env >>= \case
      Fix (NVSet s) -> evalSelector True env attr >>= \case
        [keyName] -> pure $ Fix $ NVConstant $ NBool $ keyName `Map.member` s
        _ -> error $ "attribute name argument to hasAttr is not a single-part name"
      _ -> error $ "argument to hasAttr has wrong type"

    phi (NList l) = \env ->
        Fix . NVList <$> mapM ($ env) l

    phi (NSet recBind binds) = \env -> case env of
      (Fix (NVSet env')) -> do
        rec
          mergedEnv <- pure $ case recBind of
            Rec    -> Fix $ NVSet $ evaledBinds `Map.union` env'
            NonRec -> env
          evaledBinds <- evalBinds True mergedEnv binds
        pure mergedEnv
      _ -> error "invalid evaluation environment"

    phi (NLet binds e) = \env -> case env of
      (Fix (NVSet env')) -> do
        rec
          mergedEnv   <- pure $ Fix $ NVSet $ evaledBinds `Map.union` env'
          evaledBinds <- evalBinds True mergedEnv binds
        e mergedEnv
      _ -> error "invalid evaluation environment"

    phi (NIf cond t f) = \env -> do
      (Fix cval) <- cond env
      case cval of
        NVConstant (NBool True) -> t env
        NVConstant (NBool False) -> f env
        _ -> error "condition must be a boolean"

    phi (NWith scope e) = \env -> case env of
      (Fix (NVSet env')) -> do
        s <- scope env
        case s of
          (Fix (NVSet scope')) -> e . Fix . NVSet $ Map.union scope' env'
          _ -> error "scope must be a set in with statement"
      _ -> error "invalid evaluation environment"

    phi (NAssert cond e) = \env -> do
      (Fix cond') <- cond env
      case cond' of
        (NVConstant (NBool True)) -> e env
        (NVConstant (NBool False)) -> error "assertion failed"
        _ -> error "assertion condition must be boolean"

    phi (NApp fun x) = \env -> do
        fun' <- fun env
        case fun' of
            Fix (NVFunction argset f) -> do
                arg <- x env
                let arg' = buildArgument argset arg
                f arg'
            _ -> error "Attempt to call non-function"

    phi (NAbs a b) = \env -> do
        -- jww (2014-06-28): arglists should not receive the current
        -- environment, but rather should recursively view their own arg
        -- set
        args <- traverse ($ env) a
        return $ Fix $ NVFunction args b

evalString :: NValue -> NString (NValue -> IO NValue) -> IO Text
evalString env (NString _ parts)
  = Text.concat <$> mapM (runAntiquoted return (fmap valueText . ($ env))) parts
evalString _ (NUri t) = return t

evalBinds :: Bool -> NValue -> [Binding (NValue -> IO NValue)] ->
  IO (Map.Map Text NValue)
evalBinds allowDynamic env xs = buildResult <$> sequence (concatMap go xs) where
  buildResult :: [([Text], NValue)] -> Map.Map Text NValue
  buildResult = foldl' insert Map.empty . map (first reverse) where
    insert _ ([], _) = error "invalid selector with no components"
    insert m (p:ps, v) = modifyPath ps (insertIfNotMember p v) where
      alreadyDefinedErr = error $ "attribute " ++ attr ++ " already defined"
      attr = show $ Text.intercalate "." $ reverse (p:ps)

      modifyPath :: [Text] -> (Map.Map Text NValue -> Map.Map Text NValue) -> Map.Map Text NValue
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
  go :: Binding (NValue -> IO NValue) -> [IO ([Text], NValue)]
  go (NamedVar x y) = [liftM2 (,) (evalSelector allowDynamic env x) (y env)]
  go _ = [] -- HACK! But who cares right now

evalSelector :: Bool -> NValue -> NSelector (NValue -> IO NValue) -> IO [Text]
evalSelector dyn env = mapM evalKeyName where
  evalKeyName (StaticKey k) = return k
  evalKeyName (DynamicKey k)
    | dyn       = runAntiquoted (evalString env) (fmap valueText . ($ env)) k
    | otherwise = error "dynamic attribute not allowed in this context"
