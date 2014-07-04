module Nix.Eval where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad hiding (mapM)
import qualified Data.Map as Map
import           Data.Traversable
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

    phi (NArgSet _xs) = error "Cannot evaluate an argument set"

    phi (NSet _b xs)   = \env ->
        Fix . NVSet . Map.fromList
            <$> mapM (fmap (first valueText) . go env) xs
      where
        go env (x, y) = liftM2 (,) (x env) (y env)

    phi (NLet _v _e)    = error "let: not implemented"
    phi (NIf _i _t _e)  = error "if: not implemented"
    phi (NWith _c _v)   = error "with: not implemented"
    phi (NAssert _e _v) = error "assert: not implemented"
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
