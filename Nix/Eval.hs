module Nix.Eval where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad hiding (mapM)
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Traversable as T
import           Nix.Types
import           Prelude hiding (mapM)

buildArgument :: NValue -> NValue -> NValue
buildArgument paramSpec arg =
    -- Having the typed lambda calculus would make this code much safer.
    Fix $ NVSet $ case paramSpec of
        Fix (NVArgSet s) ->
            case arg of
                Fix (NVSet s') ->
                    Map.foldlWithKey' (go s') Map.empty s
                _ -> error "Unexpected function environment"
        Fix (NVConstant (NSym name)) -> Map.singleton name arg
        _ -> error $ "Unexpected param spec: " ++ show paramSpec
  where
    go env m k v = case Map.lookup k env of
        Nothing
            | Just v' <- v -> Map.insert k v' m
            | otherwise   -> error $ "Could not find " ++ show k
        Just v' -> Map.insert k v' m

evalExpr :: NExpr -> NValue -> IO NValue
evalExpr = cata phi
  where
    phi :: NExprF (NValue -> IO NValue) -> NValue -> IO NValue
    phi (NConstant x) = const $ return $ Fix $ NVConstant x
    phi (NOper _x) = error "Operators are not yet defined"

    phi (NList l)     = \env ->
        Fix . NVList <$> mapM ($ env) l

    -- phi (NConcat l)   = \env ->
    --     Fix . NVConstant . NStr . T.concat
    --         <$> mapM (fmap valueText . ($ env)) l

    phi (NArgSet s) = \env -> Fix . NVArgSet <$> mapM (T.sequence . fmap ($ env)) s

    -- TODO: recursive sets
    phi (NSet _b binds)   = \env ->
        Fix . NVSet <$> evalBinds env binds

    -- TODO: recursive binding
    phi (NLet binds e) = \env -> case env of
      (Fix (NVSet env')) -> do
        letenv <- evalBinds env binds
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

    phi (NVar _v)       = error "var: not implemented"

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
        args <- a env
        return $ Fix $ NVFunction args b

evalBinds :: NValue -> [Binding (NValue -> IO NValue)] ->
  IO (Map.Map Text NValue)
evalBinds env xs =
  Map.fromList <$> mapM (fmap (first valueText)) (concatMap go xs) where
    go :: Binding (NValue -> IO NValue) -> [IO (NValue, NValue)]
    go (NamedVar x y) = [liftM2 (,) (x env) (y env)]
    go (Inherit ys) = map (\y -> (,) <$> y env <*> y env) ys
    go (ScopedInherit x ys) = map (\y -> (,) <$> x env <*> y env) ys
