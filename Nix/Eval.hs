{-# LANGUAGE OverloadedStrings #-}
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

buildArgument :: Formals (NValue m) -> NValue m -> NValue m
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

evalExpr :: Monad m => NExpr -> NValue m -> m (NValue m)
evalExpr = cata phi
  where
    phi (NSym var) = \env -> case env of
      Fix (NVSet s) -> maybe err return $ Map.lookup var s
      _ -> error "invalid evaluation environment"
     where err = error ("Undefined variable: " ++ show var)
    phi (NConstant x) = const $ return $ Fix $ NVConstant x
    phi (NStr str) = fmap (Fix . NVStr) . flip evalString str
    phi (NOper _x) = error "Operators are not yet defined"
    phi (NSelect _x _attr _or) = error "Select expressions are not yet supported"
    phi (NHasAttr _x _attr) = error "Has attr expressions are not yet supported"

    phi (NList l)     = \env ->
        Fix . NVList <$> mapM ($ env) l

    -- TODO: recursive sets
    phi (NSet _b binds)   = \env ->
        Fix . NVSet <$> evalBinds True env binds

    -- TODO: recursive binding
    phi (NLet binds e) = \env -> case env of
      (Fix (NVSet env')) -> do
        letenv <- evalBinds False env binds
        let newenv = Map.union letenv env'
        e . Fix . NVSet $ newenv
      _ -> error "invalid evaluation environment"

    phi (NIf cond t f)  = \env -> do
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

    phi (NAbs a b)    = \env -> do
        -- jww (2014-06-28): arglists should not receive the current
        -- environment, but rather should recursively view their own arg
        -- set
        args <- traverse ($ env) a
        return $ Fix $ NVFunction args b

evalString :: Monad m
           => NValue m -> NString (NValue m -> m (NValue m)) -> m Text
evalString env (NString _ parts)
  = Text.concat <$> mapM (runAntiquoted return (fmap valueText . ($ env))) parts
evalString _env (NUri t) = return t

evalBinds :: Monad m => Bool -> NValue m -> [Binding (NValue m -> m (NValue m))] ->
  m (Map.Map Text (NValue m))
evalBinds allowDynamic env xs = buildResult <$> sequence (concatMap go xs) where
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
  go (NamedVar x y) = [liftM2 (,) (evalSelector allowDynamic env x) (y env)]
  go _ = [] -- HACK! But who cares right now

  evalSelector :: Monad m => Bool -> NValue m -> NSelector (NValue m -> m (NValue m)) -> m [Text]
  evalSelector dyn e = mapM evalKeyName where
    evalKeyName (StaticKey k) = return k
    evalKeyName (DynamicKey k)
      | dyn       = runAntiquoted (evalString e) (fmap valueText . ($ e)) k
      | otherwise = error "dynamic attribute not allowed in this context"
