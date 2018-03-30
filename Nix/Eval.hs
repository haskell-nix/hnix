{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Eval
    (NValue, NValueNF, NValueF(..), ValueSet, MonadNix(..), StorePath (..),
     evalExpr, tracingExprEval, evalBinds, exprNormalForm, normalForm,
     builtin, builtin2, builtin3, atomText, valueText, buildArgument) where

import           Control.Monad hiding (mapM, sequence)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Align.Key
import           Data.Fix
import           Data.Functor.Identity
import           Data.List (intercalate)
import qualified Data.Map.Lazy as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (appEndo)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.These
import           Data.Typeable (Typeable)
import           GHC.Generics
import           Nix.Atoms
import           Nix.Expr
import           Nix.Scope
import           Nix.StringOperations (runAntiquoted)
import           Nix.Utils

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
      -- ^ A function is a closed set of parameters representing the "call
      --   signature", used at application time to check the type of arguments
      --   passed to the function. Since it supports default values which may
      --   depend on other values within the final argument set, this
      --   dependency is represented as a set of pending evaluations. The
      --   arguments are finally normalized into a set which is passed to the
      --   function.
      --
      --   Note that 'm r' is being used here because effectively a function
      --   and its set of default arguments is "never fully evaluated". This
      --   enforces in the type that it must be re-evaluated for each call.
    | NVLiteralPath FilePath
    | NVEnvPath FilePath
    | NVBuiltin String (NThunk m -> m (NValue m))
      -- ^ A builtin function is itself already in normal form. Also, it may
      --   or may not choose to evaluate its argument in the production of a
      --   result.
    deriving (Generic, Typeable, Functor)

-- | An 'NValueNF' is a fully evaluated value in normal form. An 'NValue m' is
--   a value in head normal form, where only the "top layer" has been
--   evaluated. An action of type 'm (NValue m)' is a pending evualation that
--   has yet to be performed. An 'NThunk m' is either a pending evaluation, or
--   a value in head normal form. A 'ValueSet' is a set of mappings from keys
--   to thunks.

type NValueNF m = Fix (NValueF m)      -- normal form
type NValue m   = NValueF m (NThunk m) -- head normal form
type ValueSet m = Map.Map Text (NThunk m)

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

valueText :: forall m. MonadNix m => NValueNF m -> m (Text, DList Text)
valueText = cata phi where
    phi :: NValueF m (m (Text, DList Text)) -> m (Text, DList Text)
    phi (NVConstant a)    = pure (atomText a, mempty)
    phi (NVStr t c)       = pure (t, c)
    phi (NVList _)        = error "Cannot coerce a list to a string"
    phi (NVSet set)
      | Just asString <-
        -- TODO: Should this be run through valueText recursively?
        Map.lookup "__asString" set = asString
      | otherwise = error "Cannot coerce a set to a string"
    phi (NVFunction _ _)  = error "Cannot coerce a function to a string"
    phi (NVLiteralPath originalPath) = do
        -- TODO: Capture and use the path of the file being processed as the
        -- base path
        storePath <- addPath originalPath
        pure (Text.pack $ unStorePath storePath, mempty)
    phi (NVEnvPath p)     =
        -- TODO: Ensure this is a store path
        pure (Text.pack p, mempty)
    phi (NVBuiltin _ _)    = error "Cannot coerce a function to a string"

valueTextNoContext :: MonadNix m => NValueNF m -> m Text
valueTextNoContext = fmap fst . valueText

builtin :: MonadNix m => String -> (NThunk m -> m (NValue m)) -> m (NValue m)
builtin name f = return $ NVBuiltin name f

builtin2 :: MonadNix m
         => String -> (NThunk m -> NThunk m -> m (NValue m)) -> m (NValue m)
builtin2 name f = builtin name (builtin name . f)

builtin3 :: MonadNix m
         => String -> (NThunk m -> NThunk m -> NThunk m -> m (NValue m))
         -> m (NValue m)
builtin3 name f =
    builtin name $ \a -> builtin name $ \b -> builtin name $ \c -> f a b c

-- | A path into the nix store
newtype StorePath = StorePath { unStorePath :: FilePath }

class MonadFix m => MonadNix m where
    currentScope :: m (NestedScopes (NThunk m))
    clearScopes  :: m r -> m r
    pushScopes   :: NestedScopes (NThunk m) -> m r -> m r
    lookupVar    :: Text -> m (Maybe (NValue m))

    pushScope :: Scope (NThunk m) -> m r -> m r
    pushScope = pushScopes . NestedScopes . (:[])

    data NThunk m :: *

    valueRef   :: NValue m -> m (NThunk m)
    buildThunk :: m (NValue m) -> m (NThunk m)
    forceThunk :: NThunk m -> m (NValue m)

    -- | Import a path into the nix store, and return the resulting path
    addPath :: FilePath -> m StorePath

    importFile :: NThunk m -> m (NValue m)
    getEnvVar :: NThunk m -> m (NValue m)

deferInScope :: MonadNix m
             => NestedScopes (NThunk m) -> m (NValue m) -> m (NThunk m)
deferInScope scope = buildThunk . clearScopes . pushScopes scope

buildArgument :: forall m. MonadNix m
              => Params (m (NThunk m)) -> NThunk m -> m (ValueSet m)
buildArgument params arg = case params of
    Param name -> return $ Map.singleton name arg
    ParamSet ps m -> go ps m
  where
    go ps m = forceThunk arg >>= \case
        NVSet args -> do
            let (s, isVariadic) = case ps of
                  FixedParamSet    s' -> (s', False)
                  VariadicParamSet s' -> (s', True)
            res <- loebM (alignWithKey (assemble isVariadic) args s)
            maybe (pure res) (selfInject res) m

        x -> error $ "Expected set in function call, received: "
                ++ show (() <$ x)

    selfInject :: ValueSet m -> Text -> m (ValueSet m)
    selfInject res n = do
        ref <- valueRef $ NVSet res
        return $ Map.insert n ref res

    assemble :: Bool
             -> Text
             -> These (NThunk m) (Maybe (m (NThunk m)))
             -> ValueSet m
             -> m (NThunk m)
    assemble isVariadic k = \case
        That Nothing  -> error $ "Missing value for parameter: " ++ show k
        That (Just f) -> \args -> do
            scope <- currentScope
            traceM $ "Deferring default argument in scope: " ++ show scope
            buildThunk $ clearScopes $ do
                traceM $ "Evaluating default argument with args: "
                    ++ show (newScope args)
                pushScopes (extendScope args scope) (forceThunk =<< f)
        This x | isVariadic -> const (pure x)
               | otherwise  -> error $ "Unexpected parameter: " ++ show k
        These x _ -> const (pure x)

-- | Evaluate an nix expression, with a given ValueSet as environment
evalExpr :: MonadNix m => NExpr -> m (NValue m)
evalExpr = cata eval

eval :: MonadNix m => NExprF (m (NValue m)) -> m (NValue m)

eval (NSym var) = do
    traceM $ "NSym..1: var = " ++ show var
    fromMaybe (error $ "Undefined variable: " ++ show var) <$> lookupVar var

eval (NConstant x)    = return $ NVConstant x
eval (NStr str)       = evalString str
eval (NLiteralPath p) = return $ NVLiteralPath p
eval (NEnvPath p)     = return $ NVEnvPath p

eval (NUnary op arg) = arg >>= \case
    NVConstant c -> return $ NVConstant $ case (op, c) of
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
                ++ show (() <$ lval, op, () <$ rval)
    case (lval, rval) of
        (NVConstant lc, NVConstant rc) -> case (op, lc, rc) of
            (NEq,  _, _) ->
                -- TODO: Refactor so that eval (NBinary ..) dispatches based
                -- on operator first
                valueRefBool =<< valueEq lval rval
            (NNEq, _, _) -> valueRefBool . not =<< valueEq lval rval
            (NLt,  l, r) -> valueRefBool $ l <  r
            (NLte, l, r) -> valueRefBool $ l <= r
            (NGt,  l, r) -> valueRefBool $ l >  r
            (NGte, l, r) -> valueRefBool $ l >= r
            (NAnd,  NBool l, NBool r) -> valueRefBool $ l && r
            (NOr,   NBool l, NBool r) -> valueRefBool $ l || r
            (NImpl, NBool l, NBool r) -> valueRefBool $ not l || r
            (NPlus,  NInt l, NInt r) -> valueRefInt $ l + r
            (NMinus, NInt l, NInt r) -> valueRefInt $ l - r
            (NMult,  NInt l, NInt r) -> valueRefInt $ l * r
            (NDiv,   NInt l, NInt r) -> valueRefInt $ l `div` r
            _ -> error unsupportedTypes

        (NVStr ls lc, NVStr rs rc) -> case op of
            NPlus -> return $ NVStr (ls `mappend` rs) (lc `mappend` rc)
            NEq  -> valueRefBool =<< valueEq lval rval
            NNEq -> valueRefBool . not =<< valueEq lval rval
            _    -> error unsupportedTypes

        (NVSet ls, NVSet rs) -> case op of
            NUpdate -> return $ NVSet $ rs `Map.union` ls
            _ -> error unsupportedTypes

        (NVList ls, NVList rs) -> case op of
            NConcat -> return $ NVList $ ls ++ rs
            NEq -> valueRefBool =<< valueEq lval rval
            _ -> error unsupportedTypes

        (NVLiteralPath ls, NVLiteralPath rs) -> case op of
            -- TODO: Canonicalise path
            NPlus -> return $ NVLiteralPath $ ls ++ rs
            _ -> error unsupportedTypes

        (NVLiteralPath ls, NVStr rs rc) -> case op of
            -- TODO: Canonicalise path
            NPlus -> return $ NVStr (Text.pack ls `mappend` rs) rc
            _ -> error unsupportedTypes

        _ -> error unsupportedTypes

eval (NSelect aset attr alternative) = do
    aset' <- aset
    ks    <- evalSelector True attr
    mres  <- extract aset' ks
    case mres of
        Just v -> do
            traceM $ "Wrapping a selector: " ++ show (() <$ v)
            pure v
        Nothing -> fromMaybe err alternative
          where
            err = error $ "could not look up attribute "
                ++ intercalate "." (map Text.unpack ks)
                ++ " in " ++ show (() <$ aset')
  where
    extract (NVSet s) (k:ks) = case Map.lookup k s of
        Just v  -> do
            s' <- forceThunk v
            extract s' ks
        Nothing -> return Nothing
    extract _ (_:_) = return Nothing
    extract v [] = return $ Just v

eval (NHasAttr aset attr) = aset >>= \case
    NVSet s -> evalSelector True attr >>= \case
        [keyName] ->
            return $ NVConstant $ NBool $ keyName `Map.member` s
        _ -> error "attr name argument to hasAttr is not a single-part name"
    _ -> error "argument to hasAttr has wrong type"

eval (NList l) = do
    scope <- currentScope
    NVList <$> traverse (deferInScope scope) l

eval (NSet binds) = do
    traceM "NSet..1"
    s <- evalBinds True False binds
    traceM $ "NSet..2: s = " ++ show (() <$ s)
    return $ NVSet s

eval (NRecSet binds) = do
    traceM "NRecSet..1"
    s <- evalBinds True True binds
    traceM $ "NRecSet..2: s = " ++ show (() <$ s)
    return $ NVSet s

eval (NLet binds e) = do
    traceM "Let..1"
    s <- evalBinds True True binds
    traceM $ "Let..2: s = " ++ show (() <$ s)
    pushScope (newScope s) e

eval (NIf cond t f) = cond >>= \case
    NVConstant (NBool True) -> t
    NVConstant (NBool False) -> f
    _ -> error "condition must be a boolean"

eval (NWith scope e) = scope >>= \case
    NVSet s -> pushScope (newWeakScope s) e
    _ -> error "scope must be a set in with statement"

eval (NAssert cond e) = cond >>= \case
    NVConstant (NBool True) -> e
    NVConstant (NBool False) -> error "assertion failed"
    _ -> error "assertion condition must be boolean"

eval (NApp fun arg) = fun >>= \case
    NVFunction params f -> do
        args <- buildArgument params =<< buildThunk arg
        traceM $ "Evaluating function application with args: "
            ++ show (newScope args)
        clearScopes (pushScope (newScope args) (forceThunk =<< f))
    NVBuiltin _ f -> f =<< buildThunk arg
    _ -> error "Attempt to call non-function"

eval (NAbs params body) = do
    -- It is the environment at the definition site, not the call site, that
    -- needs to be used when evaluating the body and default arguments, hence
    -- we defer here so the present scope is restored when the parameters and
    -- body are forced during application.
    scope <- currentScope
    traceM $ "Creating lambda abstraction in scope: " ++ show scope
    return $ NVFunction (buildThunk . pushScopes scope <$> params)
                        (buildThunk (pushScopes scope body))

valueRefBool :: MonadNix m => Bool -> m (NValue m)
valueRefBool = return . NVConstant . NBool

valueRefInt :: MonadNix m => Integer -> m (NValue m)
valueRefInt = return . NVConstant . NInt

valueEq :: MonadNix m => NValue m -> NValue m -> m Bool
valueEq l r = case (l, r) of
    (NVConstant lc, NVConstant rc) -> pure $ lc == rc
    (NVStr ls _, NVStr rs _) -> pure $ ls == rs
    (NVList ls, NVList rs) -> go ls rs
        where
            go (hl:tl) (hr:tr) = do
                hlv <- forceThunk hl
                hrv <- forceThunk hr
                valueEq hlv hrv >>= \case
                    False -> pure False
                    True -> go tl tr
            go [] [] = pure True
            go _ _ = pure False
    _ -> pure False

tracingExprEval :: MonadNix m => NExpr -> IO (m (NValue m))
tracingExprEval =
    flip runReaderT (0 :: Int)
        . fmap (runIdentity . snd)
        . adiM @() (pure <$> eval) psi
  where
    psi k v@(Fix x) = do
        depth <- ask
        liftIO $ putStrLn $ "eval: " ++ replicate (depth * 2) ' ' ++ show x
        res <- local succ $ k v
        liftIO $ putStrLn $ "eval: " ++ replicate (depth * 2) ' ' ++ "."
        return res

exprNormalForm :: MonadNix m => NExpr -> m (NValueNF m)
exprNormalForm = normalForm <=< evalExpr

normalForm :: MonadNix m => NValue m -> m (NValueNF m)
normalForm = \case
    NVConstant a     -> return $ Fix $ NVConstant a
    NVStr t s        -> return $ Fix $ NVStr t s
    NVList l         -> Fix . NVList <$> traverse (normalForm <=< forceThunk) l
    NVSet s          -> Fix . NVSet <$> traverse (normalForm <=< forceThunk) s
    NVFunction p f   -> do
        p' <- traverse (fmap (normalForm <=< forceThunk)) p
        return $ Fix $ NVFunction p' (normalForm =<< forceThunk =<< f)
    NVLiteralPath fp -> return $ Fix $ NVLiteralPath fp
    NVEnvPath p      -> return $ Fix $ NVEnvPath p
    NVBuiltin name f -> return $ Fix $ NVBuiltin name f

attrSetAlter :: MonadNix m
             => [Text]
             -> Map.Map Text (m (NValue m))
             -> m (NValue m)
             -> m (Map.Map Text (m (NValue m)))
attrSetAlter [] _ _ = error "invalid selector with no components"
attrSetAlter (p:ps) m val = case Map.lookup p m of
    Nothing | null ps   -> go
            | otherwise -> recurse Map.empty
    Just v  | null ps   -> go
            | otherwise -> v >>= \case
                  NVSet s -> recurse (fmap forceThunk s)
                  _ -> error $ "attribute " ++ attr ++ " is not a set"
  where
    attr = show (Text.intercalate "." (p:ps))

    go = return $ Map.insert p val m

    recurse s = attrSetAlter ps s val >>= \m' ->
        if | Map.null m' -> return m
           | otherwise   -> do
             scope <- currentScope
             return $ Map.insert p (embed scope m') m
      where
        embed scope m' = NVSet <$> traverse (deferInScope scope) m'

evalBinds :: forall m. MonadNix m
          => Bool
          -> Bool
          -> [Binding (m (NValue m))]
          -> m (ValueSet m)
evalBinds allowDynamic recursive = buildResult . concat <=< mapM go
  where
    go :: Binding (m (NValue m)) -> m [([Text], m (NValue m))]
    go (NamedVar x y) =
        sequence [liftM2 (,) (evalSelector allowDynamic x) (pure y)]
    go (Inherit ms names) = forM names $ \name -> do
        key <- evalKeyName allowDynamic name
        return ([key], do
            mv <- case ms of
                Nothing -> lookupVar key
                Just s -> s >>= \case
                    NVSet s -> pushScope (newScope s) (lookupVar key)
                    x -> error
                        $ "First argument to inherit should be a set, saw: "
                        ++ show (() <$ x)
            case mv of
                Nothing -> error $ "Inheriting unknown attribute: "
                    ++ show (() <$ name)
                Just v -> return v)

    buildResult :: [([Text], m (NValue m))] -> m (ValueSet m)
    buildResult bindings = do
        s <- foldM insert Map.empty bindings
        scope <- currentScope
        if recursive
            then loebM (encapsulate scope <$> s)
            else traverse (deferInScope scope) s

    encapsulate scope f attrs = deferInScope (extendScope attrs scope) f

    insert m (path, value) = attrSetAlter path m value

evalString :: MonadNix m => NString (m (NValue m)) -> m (NValue m)
evalString nstr = do
    let fromParts parts = do
          (t, c) <- mconcat <$> mapM go parts
          return $ NVStr t c
    case nstr of
      Indented     parts -> fromParts parts
      DoubleQuoted parts -> fromParts parts
  where
    go = runAntiquoted (return . (, mempty)) (valueText <=< (normalForm =<<))

evalSelector :: MonadNix m => Bool -> NAttrPath (m (NValue m)) -> m [Text]
evalSelector = mapM . evalKeyName

evalKeyName :: MonadNix m => Bool -> NKeyName (m (NValue m)) -> m Text
evalKeyName _ (StaticKey k) = return k
evalKeyName dyn (DynamicKey k)
    | dyn = do
          v <- runAntiquoted evalString id k
          valueTextNoContext =<< normalForm v
    | otherwise = error "dynamic attribute not allowed in this context"
