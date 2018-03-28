{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Nix.Eval where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad hiding (mapM, sequence)
import           Control.Monad.Fix
import           Data.Align.Key
import           Data.Fix
import           Data.Foldable (foldl')
import           Data.Functor.Identity
import           Data.Functor.Compose
import           Data.List (intercalate)
import qualified Data.Map.Lazy as Map
import           Data.Monoid (appEndo, Endo)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.These
import           Data.Traversable as T
import           Data.Typeable (Typeable)
import           Debug.Trace
import           GHC.Generics
import           Nix.Atoms
import           Nix.Expr
import           Nix.Parser
import           Nix.StringOperations (runAntiquoted)
import           Prelude hiding (mapM, sequence)

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
    | NVFunction (Params (ValueSet m -> m r)) (ValueSet m -> m r)
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

instance Show f => Show (NValueF m f) where
    showsPrec = flip go where
      go (NVConstant atom) = showsCon1 "NVConstant" atom
      go (NVStr      text context) = showsCon2 "NVStr"    text (appEndo context [])
      go (NVList     list) = showsCon1 "NVList"     list
      go (NVSet     attrs) = showsCon1 "NVSet"      attrs
      go (NVFunction r _)  = showsCon1 "NVFunction" (() <$ r)
      go (NVLiteralPath p) = showsCon1 "NVLiteralPath" p
      go (NVEnvPath p)     = showsCon1 "NVEnvPath" p
      go (NVBuiltin name _) = showsCon1 "NVBuiltin" name

      showsCon1 :: Show a => String -> a -> Int -> String -> String
      showsCon1 con a d = showParen (d > 10) $ showString (con ++ " ") . showsPrec 11 a

      showsCon2 :: (Show a, Show b) => String -> a -> b -> Int -> String -> String
      showsCon2 con a b d = showParen (d > 10) $ showString (con ++ " ") . showsPrec 11 a . showString " " . showsPrec 11 b

type NValue m = Fix (NValueF m)

type ValueSet m = Map.Map Text (NValue m)

-- | A pending evaluation awaits an attribute environment, and a monadic
--   context, in order to finally evaluate to the resulting value.
type PendingEval m = ValueSet m -> m (NValue m)

builtin :: String -> (NValue m -> m (NValue m)) -> NValue m
builtin name f = Fix (NVBuiltin name f)

builtin2 :: Monad m => String -> (NValue m -> NValue m -> m (NValue m)) -> NValue m
builtin2 name f = builtin name (\arg -> return (builtin name (f arg)))


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

expr = let Success x = parseNixString "({ x ? 1, y ? x * 3 }: y - x) { x = 10; }"
       in x
goeval :: MonadFix m => m (NValue m)
goeval = evalExpr expr mempty

goevalIO = goeval @IO
goevalId = goeval @Identity

loebM :: (MonadFix m, Traversable t) => t (t a -> m a) -> m (t a)
loebM f = mfix $ \a -> mapM ($ a) f

buildArgument :: forall m. MonadFix m
              => Params (PendingEval m) -> NValue m -> m (ValueSet m)
buildArgument params arg = case params of
    Param name -> return $ Map.singleton name arg
    ParamSet (FixedParamSet s) m -> go s m
    ParamSet (VariadicParamSet s) m -> go s m
  where
    go :: Map.Map Text (Maybe (PendingEval m)) -> Maybe Text -> m (ValueSet m)
    go s m = case arg of
        Fix (NVSet args) -> do
            res <- loebM (alignWithKey assemble args s)
            return $ maybe res (\n -> Map.insert n arg res) m
        _ -> error $ "Expected set in function call, received: " ++ show arg

    assemble k = \case
        That Nothing ->
            error $ "Missing value for parameter: " ++ Text.unpack k
        That (Just f) -> \env -> trace ("ref.1 " ++ show k) $ f env
        This x -> \_ -> trace ("ref.2 " ++ show k) $ pure x
        These x _ -> \_ -> trace ("ref.3 " ++ show k) $ pure x

-- | Evaluate an nix expression, with a given ValueSet as environment
evalExpr :: forall m. MonadFix m => NExpr -> PendingEval m
evalExpr = cata (\x -> do traceM ("evalExpr mk " ++ show (() <$ x))
                          let f = phi x
                          \env -> do
                              traceM ("evalExpr do " ++ show (() <$ x))
                              f env)
  where
    phi :: NExprF (PendingEval m) -> PendingEval m
    phi (NSym var) =
        trace ("NSym mk " ++ show var) $ \env -> do
            traceM ("NSym ref " ++ show var ++ "...")
            traceM $ "NSym env = " ++ show env
            res <- maybe err return $ Map.lookup var env
            traceM ("NSym ref " ++ show var ++ "...done: " ++ show res)
            return res
      where err = error ("Undefined variable: " ++ show var)
    phi (NConstant x) = const $ return $ Fix $ NVConstant x
    phi (NStr str) = evalString str
    phi (NLiteralPath p) = const $ return $ Fix $ NVLiteralPath p
    phi (NEnvPath p) = const $ return $ Fix $ NVEnvPath p

    phi (NUnary op arg) = \env -> arg env >>= \case
      Fix (NVConstant c) -> pure $ Fix $ NVConstant $ case (op, c) of
        (NNeg, NInt  i) -> NInt  (-i)
        (NNot, NBool b) -> NBool (not b)
        _               -> error $ "unsupported argument type for unary operator " ++ show op
      _ -> error "argument to unary operator must evaluate to an atomic type"
    phi (NBinary op larg rarg) = \env -> do
      Fix lval <- larg env
      Fix rval <- rarg env
      let unsupportedTypes = "unsupported argument types for binary operator " ++ show (lval, op, rval)
      case (lval, rval) of
       (NVConstant lc, NVConstant rc) -> pure $ Fix $ NVConstant $ case (op, lc, rc) of
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
         NPlus -> pure $ Fix $ NVStr (ls `mappend` rs) (lc `mappend` rc)
         _ -> error unsupportedTypes
       (NVSet ls, NVSet rs) -> case op of
         NUpdate -> pure $ Fix $ NVSet $ rs `Map.union` ls
         _ -> error unsupportedTypes
       (NVList ls, NVList rs) -> case op of
         NConcat -> pure $ Fix $ NVList $ ls ++ rs
         _ -> error unsupportedTypes
       (NVLiteralPath ls, NVLiteralPath rs) -> case op of
         NPlus -> pure $ Fix $ NVLiteralPath $ ls ++ rs -- TODO: Canonicalise path
         _ -> error unsupportedTypes
       (NVLiteralPath ls, NVStr rs rc) -> case op of
         NPlus -> pure $ Fix $ NVStr (Text.pack ls `mappend` rs) rc -- TODO: Canonicalise path
         _ -> error unsupportedTypes
       _ -> error unsupportedTypes

    phi (NSelect aset attr alternative) = go where
      go env = do
        aset' <- aset env
        ks    <- evalSelector True attr env
        case extract aset' ks of
         Just v  -> pure v
         Nothing -> case alternative of
           Just v  -> v env
           Nothing -> error $ "could not look up attribute '"
               ++ intercalate "." (map show ks) ++ "' in value " ++ show aset'
      extract (Fix (NVSet s)) (k:ks) = case Map.lookup k s of
                                        Just v  -> extract v ks
                                        Nothing -> Nothing
      extract               _  (_:_) = Nothing
      extract               v     [] = Just v

    phi (NHasAttr aset attr) = \env -> aset env >>= \case
      Fix (NVSet s) -> evalSelector True attr env >>= \case
        [keyName] -> pure $ Fix $ NVConstant $ NBool $ keyName `Map.member` s
        _ -> error "attribute name argument to hasAttr is not a single-part name"
      _ -> error "argument to hasAttr has wrong type"

    phi (NList l) = \env ->
        Fix . NVList <$> mapM ($ env) l

    phi (NSet binds) = \env -> do
      evaledBinds <- evalBinds True binds env
      pure . Fix . NVSet $ evaledBinds

    phi (NRecSet binds) = \env -> do
      rec
        let mergedEnv = evaledBinds `Map.union` env
        evaledBinds <- evalBinds True binds mergedEnv
      pure . Fix . NVSet $ evaledBinds

    phi (NLet binds e) = \env -> do
        rec
          let mergedEnv = evaledBinds `Map.union` env
          evaledBinds <- evalBinds True binds mergedEnv
        e mergedEnv

    phi (NIf cond t f) = \env -> do
      (Fix cval) <- cond env
      case cval of
        NVConstant (NBool True) -> t env
        NVConstant (NBool False) -> f env
        _ -> error "condition must be a boolean"

    phi (NWith scope e) = \env -> do
      s <- scope env
      case s of
        (Fix (NVSet scope')) -> e $ Map.union scope' env
        _ -> error "scope must be a set in with statement"

    phi (NAssert cond e) = \env -> do
      (Fix cond') <- cond env
      case cond' of
        (NVConstant (NBool True)) -> e env
        (NVConstant (NBool False)) -> error "assertion failed"
        _ -> error "assertion condition must be boolean"

    phi (NApp fun x) = \env -> do
        fun' <- fun env
        case fun' of
            Fix (NVFunction params f) -> do
                traceM "phi NApp..1"
                arg <- x env
                traceM "phi NApp..2"
                args <- buildArgument params arg
                traceM "phi NApp..3"
                res <- f args
                traceM "phi NApp..4"
                return res
            Fix (NVBuiltin _ f) -> do
                arg <- x env
                f arg
            _ -> error "Attempt to call non-function"

    phi (NAbs a b) = \env -> do
        -- It is the environment at the definition site, not the call site,
        -- that needs to be used when evaluation the body and the default
        -- arguments
        let extend f env' = f (env' `Map.union` env)
        traceM "phi NAbs.."
        return $ Fix $ NVFunction (fmap extend a) (extend b)

evalString :: Monad m => NString (PendingEval m) -> PendingEval m
evalString nstr env = do
  let fromParts parts = do
        (t, c) <-
          mconcat <$>
          mapM
            (runAntiquoted (return . (, mempty)) (fmap valueText . ($ env)))
            parts
        return (Fix (NVStr t c))
  case nstr of
    Indented parts -> fromParts parts
    DoubleQuoted parts -> fromParts parts

evalBinds :: Monad m
          => Bool -> [Binding (PendingEval m)] -> ValueSet m -> m (ValueSet m)
evalBinds allowDynamic xs env = buildResult <$> sequence (concatMap go xs) where
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
  go (NamedVar x y) = [liftM2 (,) (evalSelector allowDynamic x env) (y env)]
  go _ = [] -- HACK! But who cares right now

evalSelector :: Monad m => Bool -> NAttrPath (PendingEval m) -> ValueSet m -> m [Text]
evalSelector dyn x env = mapM evalKeyName x where
  evalKeyName (StaticKey k) = return k
  evalKeyName (DynamicKey k)
    | dyn       = fmap valueTextNoContext . runAntiquoted (flip evalString env) ($ env) $ k
    | otherwise = error "dynamic attribute not allowed in this context"
