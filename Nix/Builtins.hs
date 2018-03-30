{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Builtins
    (baseEnv, builtins, Cyclic(..), NestedScopes(..),
     evalTopLevelExpr, evalTopLevelExprIO,
     tracingEvalTopLevelExprIO)
    where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Align (alignWith)
import           Data.Char (isDigit)
import           Data.Fix
import           Data.IORef
import qualified Data.Map.Lazy as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.These (fromThese)
import           Data.Foldable (foldlM)
import           Data.Traversable (mapM)
import           Nix.Atoms
import           Nix.Eval
import           Nix.Scope
import           Nix.Expr (NExpr)
import           Nix.Parser
import           Nix.Utils
import           System.Environment
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath
import           System.Process (readProcessWithExitCode)

-- | Evaluate a nix expression in the default context
evalTopLevelExpr :: MonadNix m => Maybe FilePath -> NExpr -> m (NValueNF m)
evalTopLevelExpr mdir expr = do
    base <- do
        base <- baseEnv
        case mdir of
            Nothing -> return base
            Just dir -> do
                ref <- buildThunk $ return $ NVLiteralPath dir
                let m = Map.singleton "__cwd" ref
                traceM $ "Setting __cwd = " ++ show dir
                return $ extendScope m base
    normalForm =<< pushScopes base (evalExpr expr)

evalTopLevelExprIO :: Maybe FilePath -> NExpr -> IO (NValueNF (Cyclic IO))
evalTopLevelExprIO mdir expr =
    runReaderT (runCyclic (evalTopLevelExpr mdir expr)) emptyScopes

tracingEvalTopLevelExprIO :: Maybe FilePath -> NExpr
                          -> IO (NValueNF (Cyclic IO))
tracingEvalTopLevelExprIO mdir expr = do
    base <- case mdir of
        Nothing -> run baseEnv emptyScopes
        Just dir -> do
            ref   <- run (buildThunk $ return $ NVLiteralPath dir) emptyScopes
            let m = Map.singleton "__cwd" ref
            traceM $ "Setting __cwd = " ++ show dir
            base <- run baseEnv emptyScopes
            return $ extendScope m base
    expr' <- tracingExprEval expr
    thnk  <- run expr' base
    run (normalForm thnk) base
  where
    run = runReaderT . runCyclic

baseEnv :: MonadNix m => m (NestedScopes (NThunk m))
baseEnv = do
    ref <- buildThunk $ NVSet <$> builtins
    lst <- (("builtins", ref) :) <$> topLevelBuiltins
    return . NestedScopes . (:[]) . newScope $ Map.fromList lst
  where
    topLevelBuiltins = map mapping . filter isTopLevel <$> builtinsList

newtype Cyclic m a = Cyclic
    { runCyclic :: ReaderT (NestedScopes (NThunk (Cyclic m))) m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

data Deferred m
    = DeferredAction (m (NValue m))
    -- ^ This is closure over the environment where it was created.
    | ComputedValue (NValue m)

instance MonadNix (Cyclic IO) where
    -- jww (2018-03-29): We should use actually stacked scopes here, rather
    -- than constantly merging maps. The number of scope levels will usually
    -- be manageable, but the number of attributes within scopes can be
    -- enormous, making this one of the worst implementations.
    pushScopes s k = Cyclic $ local (combineScopes s) $ do
        scope <- runCyclic currentScope
        traceM $ "scope: " ++ show (() <$ scope)
        runCyclic k

    clearScopes  = Cyclic . local (const (NestedScopes [])) . runCyclic
    currentScope = Cyclic ask

    -- If a variable is being asked for, it's needed in head normal form.
    lookupVar k  = Cyclic $ do
        scope <- ask
        case scopeLookup k scope of
            Nothing -> return Nothing
            Just v  -> runCyclic $ Just <$> forceThunk v

    -- jww (2018-03-29): Cache which files have been read in.
    importFile = forceThunk >=> \case
        NVLiteralPath path -> do
            mres <- lookupVar "__cwd"
            path' <- case mres of
                Nothing  -> do
                    traceM "No known current directory"
                    return path
                Just dir -> normalForm dir >>= \case
                    Fix (NVLiteralPath dir') -> do
                        traceM $ "Current directory for import is: "
                            ++ show dir'
                        return $ dir' </> path
                    x -> error $ "How can the current directory be: " ++ show x
            traceM $ "Importing file " ++ path'
            eres <- Cyclic $ parseNixFile path'
            case eres of
                Failure err  -> error $ "Parse failed: " ++ show err
                Success expr -> do
                    ref <- buildThunk $ return $
                        NVLiteralPath $ takeDirectory path'
                    -- Use this cookie so that when we evaluate the next
                    -- import, we'll remember which directory its containing
                    -- file was in.
                    pushScope (newScope (Map.singleton "__cwd" ref))
                              (evalExpr expr)
        p -> error $ "Unexpected argument to import: " ++ show (() <$ p)

    addPath path = liftIO $ do
        (exitCode, out, _) <-
            readProcessWithExitCode "nix-store" ["--add", path] ""
        case exitCode of
          ExitSuccess -> return $ StorePath out
          _ -> error $ "No such file or directory: " ++ show path

    getEnvVar = forceThunk >=> \case
        NVStr s _ -> do
            mres <- liftIO $ lookupEnv (Text.unpack s)
            return $ case mres of
                Nothing -> NVStr "" mempty
                Just v  -> NVStr (Text.pack v) mempty
        p -> error $ "Unexpected argument to getEnv: " ++ show (() <$ p)

    data NThunk (Cyclic IO) = NThunkIO (IORef (Deferred (Cyclic IO)))

    valueRef value =
        liftIO $ NThunkIO <$> newIORef (ComputedValue value)

    buildThunk action =
        liftIO $ NThunkIO <$> newIORef (DeferredAction action)

    forceThunk (NThunkIO ref) = do
        eres <- liftIO $ readIORef ref
        case eres of
            ComputedValue value -> return value
            DeferredAction action -> do
                scope <- currentScope
                traceM $ "Forcing thunk in scope: " ++ show scope
                value <- action
                traceM $ "Forcing thunk computed: " ++ show (() <$ value)
                liftIO $ writeIORef ref (ComputedValue value)
                return value

builtins :: MonadNix m => m (ValueSet m)
builtins = Map.fromList . map mapping <$> builtinsList

data BuiltinType = Normal | TopLevel
data Builtin m = Builtin
    { kind    :: BuiltinType
    , mapping :: (Text, NThunk m)
    }

isTopLevel :: Builtin m -> Bool
isTopLevel b = case kind b of Normal -> False; TopLevel -> True

builtinsList :: MonadNix m => m [ Builtin m ]
builtinsList = sequence [
      add  TopLevel "toString" toString
    , add  TopLevel "import"   import_

    , add  Normal   "getEnv"          getEnv_
    , add2 Normal   "hasAttr"         hasAttr
    , add2 Normal   "getAttr"         getAttr
    , add2 Normal   "any"             any_
    , add2 Normal   "all"             all_
    , add3 Normal   "foldl'"          foldl'_
    , add  Normal   "head"            head_
    , add  Normal   "tail"            tail_
    , add  Normal   "splitVersion"    splitVersion_
    , add2 Normal   "compareVersions" compareVersions_
    , add2 Normal   "compareVersions" compareVersions_
    , add2 Normal   "sub"             sub_
    , add  Normal   "parseDrvName"    parseDrvName_
  ]
  where
    wrap t n f = Builtin t (n, f)
    add  t n v = wrap t n <$> buildThunk (builtin  (Text.unpack n) v)
    add2 t n v = wrap t n <$> buildThunk (builtin2 (Text.unpack n) v)
    add3 t n v = wrap t n <$> buildThunk (builtin3 (Text.unpack n) v)

-- Helpers

mkBool :: MonadNix m => Bool -> m (NValue m)
mkBool = return . NVConstant . NBool

extractBool :: MonadNix m => NValue m -> m Bool
extractBool = \case
    NVConstant (NBool b) -> return b
    _ -> error "Not a boolean constant"

apply :: MonadNix m => NThunk m -> NThunk m -> m (NValue m)
apply f arg = forceThunk f >>= \case
    NVFunction params pred ->
        (`pushScope` (forceThunk =<< pred)) . newScope
            =<< buildArgument params arg
    x -> error $ "Trying to call a " ++ show (() <$ x)

-- Primops

toString :: MonadNix m => NThunk m -> m (NValue m)
toString str = do
    (s, d) <- valueText =<< normalForm =<< forceThunk str
    return $ NVStr s d

import_ :: MonadNix m => NThunk m -> m (NValue m)
import_ = importFile

getEnv_ :: MonadNix m => NThunk m -> m (NValue m)
getEnv_ = getEnvVar

hasAttr :: MonadNix m => NThunk m -> NThunk m -> m (NValue m)
hasAttr x y = (,) <$> forceThunk x <*> forceThunk y >>= \case
    (NVStr key _, NVSet aset) ->
        return . NVConstant . NBool $ Map.member key aset
    (x, y) -> error $ "Invalid types for builtin.hasAttr: "
                 ++ show (() <$ x, () <$ y)

getAttr :: MonadNix m => NThunk m -> NThunk m -> m (NValue m)
getAttr x y = (,) <$> forceThunk x <*> forceThunk y >>= \case
    (NVStr key _, NVSet aset) ->
        forceThunk (Map.findWithDefault _err key aset)
          where _err = error $ "hasAttr: field does not exist: "
                           ++ Text.unpack key
    (x, y) -> error $ "Invalid types for builtin.hasAttr: "
                 ++ show (() <$ x, () <$ y)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = return False
anyM p (x:xs)   = do
        q <- p x
        if q then return True
             else anyM p xs

any_ :: MonadNix m => NThunk m -> NThunk m -> m (NValue m)
any_ pred = forceThunk >=> \case
    NVList l ->
        mkBool =<< anyM extractBool =<< mapM (apply pred) l
    arg -> error $ "builtins.any takes a list as second argument, not a "
              ++ show (() <$ arg)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x:xs)   = do
        q <- p x
        if q then allM p xs
             else return False

all_ :: MonadNix m => NThunk m -> NThunk m -> m (NValue m)
all_ pred = forceThunk >=> \case
    NVList l ->
        mkBool =<< allM extractBool =<< mapM (apply pred) l
    arg -> error $ "builtins.all takes a list as second argument, not a "
              ++ show (() <$ arg)

--TODO: Strictness
foldl'_ :: MonadNix m => NThunk m -> NThunk m -> NThunk m -> m (NValue m)
foldl'_ f z = forceThunk >=> \case
    NVList vals -> forceThunk =<< foldlM go z vals
    arg -> error $ "builtins.foldl' takes a list as third argument, not a "
              ++ show (() <$ arg)
  where
    go b a = do
        f' <- buildThunk $ apply f b
        buildThunk $ apply f' a

head_ :: MonadNix m => NThunk m -> m (NValue m)
head_ = forceThunk >=> \case
    NVList vals -> case vals of
        [] -> error "builtins.head: empty list"
        h:_ -> forceThunk h
    _ -> error "builtins.head: not a list"

tail_ :: MonadNix m => NThunk m -> m (NValue m)
tail_ = forceThunk >=> \case
    NVList vals -> case vals of
        [] -> error "builtins.tail: empty list"
        _:t -> return $ NVList t
    _ -> error "builtins.tail: not a list"

data VersionComponent
   = VersionComponent_Pre -- ^ The string "pre"
   | VersionComponent_String Text -- ^ A string other than "pre"
   | VersionComponent_Number Integer -- ^ A number
   deriving (Show, Read, Eq, Ord)

versionComponentToString :: VersionComponent -> Text
versionComponentToString = \case
  VersionComponent_Pre -> "pre"
  VersionComponent_String s -> s
  VersionComponent_Number n -> Text.pack $ show n

-- | Based on https://github.com/NixOS/nix/blob/4ee4fda521137fed6af0446948b3877e0c5db803/src/libexpr/names.cc#L44
versionComponentSeparators :: String
versionComponentSeparators = ".-"

splitVersion :: Text -> [VersionComponent]
splitVersion s = case Text.uncons s of
    Nothing -> []
    Just (h, t)
      | h `elem` versionComponentSeparators -> splitVersion t
      | isDigit h ->
          let (digits, rest) = Text.span isDigit s
          in VersionComponent_Number (read $ Text.unpack digits) : splitVersion rest
      | otherwise ->
          let (chars, rest) = Text.span (\c -> not $ isDigit c || c `elem` versionComponentSeparators) s
              thisComponent = case chars of
                  "pre" -> VersionComponent_Pre
                  x -> VersionComponent_String x
          in thisComponent : splitVersion rest

splitVersion_ :: MonadNix m => NThunk m -> m (NValue m)
splitVersion_ = forceThunk >=> \case
    NVStr s _ -> do
        vals <- forM (splitVersion s) $ \c ->
            buildThunk $ return $ NVStr (versionComponentToString c) mempty
        return $ NVList vals
    _ -> error "builtins.splitVersion: not a string"

compareVersions :: Text -> Text -> Ordering
compareVersions s1 s2 =
    mconcat $ alignWith f (splitVersion s1) (splitVersion s2)
  where
    z = VersionComponent_String ""
    f = uncurry compare . fromThese z z

compareVersions_ :: MonadNix m => NThunk m -> NThunk m -> m (NValue m)
compareVersions_ t1 t2 = do
    v1 <- forceThunk t1
    v2 <- forceThunk t2
    case (v1, v2) of
        (NVStr s1 _, NVStr s2 _) ->
            return $ NVConstant $ NInt $ case compareVersions s1 s2 of
                LT -> -1
                EQ -> 0
                GT -> 1
        _ -> error "builtins.splitVersion: not a string"

sub_ :: MonadNix m => NThunk m -> NThunk m -> m (NValue m)
sub_ t1 t2 = do
    v1 <- forceThunk t1
    v2 <- forceThunk t2
    case (v1, v2) of
        (NVConstant (NInt n1), NVConstant (NInt n2)) ->
            return $ NVConstant $ NInt $ n1 - n2
        _ -> error "builtins.splitVersion: not a number"

parseDrvName :: Text -> (Text, Text)
parseDrvName s =
    let sep = "-"
        pieces = Text.splitOn sep s
        isFirstVersionPiece p = case Text.uncons p of
            Just (h, _) | isDigit h -> True
            _ -> False
        -- Like 'break', but always puts the first item into the first result
        -- list
        breakAfterFirstItem :: (a -> Bool) -> [a] -> ([a], [a])
        breakAfterFirstItem f = \case
            h : t ->
                let (a, b) = break f t
                in (h : a, b)
            [] -> ([], [])
        (namePieces, versionPieces) =
          breakAfterFirstItem isFirstVersionPiece pieces
    in (Text.intercalate sep namePieces, Text.intercalate sep versionPieces)

parseDrvName_ :: MonadNix m => NThunk m -> m (NValue m)
parseDrvName_ = forceThunk >=> \case
    NVStr s _ -> do
        let (name, version) = parseDrvName s
        vals <- sequence $ Map.fromList
          [ ("name", buildThunk $ return $ NVStr name mempty)
          , ("version", buildThunk $ return $ NVStr version mempty)
          ]
        return $ NVSet vals
    _ -> error "builtins.splitVersion: not a string"
