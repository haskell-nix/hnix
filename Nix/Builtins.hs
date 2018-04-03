{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Builtins (MonadBuiltins, baseEnv) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.ListM (sortByM)
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512
import           Data.Align (alignWith)
import           Data.ByteString.Base16 as Base16
import           Data.Char (isDigit)
import           Data.Foldable (foldlM)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Text.Encoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import           Data.These (fromThese)
import           Data.Traversable (mapM)
import           GHC.Stack.Types (HasCallStack)
import           Nix.Atoms
import           Nix.Eval
import           Nix.Monad
import           Nix.Scope
import           Nix.Stack
import           System.Directory (listDirectory)
import           System.FilePath.Posix
import           System.Posix.Files

type MonadBuiltins e m =
    (MonadNixEval e m, MonadEval (NThunk m) (NValue m) m, MonadNixEnv m)

baseEnv :: MonadBuiltins e m => m (Scopes m (NThunk m))
baseEnv = do
    ref <- thunk $ NVSet <$> builtins
    lst <- (("builtins", ref) :) <$> topLevelBuiltins
    pushScope (M.fromList lst) currentScopes
  where
    topLevelBuiltins = map mapping . filter isTopLevel <$> builtinsList

builtins :: MonadBuiltins e m => m (ValueSet m)
builtins = M.fromList . map mapping <$> builtinsList

data BuiltinType = Normal | TopLevel
data Builtin m = Builtin
    { kind    :: BuiltinType
    , mapping :: (Text, NThunk m)
    }

isTopLevel :: Builtin m -> Bool
isTopLevel b = case kind b of Normal -> False; TopLevel -> True

builtinsList :: forall e m. MonadBuiltins e m => m [ Builtin m ]
builtinsList = sequence [
      add  TopLevel "toString"                   toString
    , add  TopLevel "import"                     importFile
    , add2 TopLevel "map"                        map_
    , add' TopLevel "baseNameOf"                 (arity1 baseNameOf)
    , add2 TopLevel "removeAttrs"                removeAttrs
    , add  TopLevel "isNull"                     isNull
    , add  Normal   "getEnv"                     getEnvVar
    , add2 Normal   "hasAttr"                    hasAttr
    , add2 Normal   "getAttr"                    getAttr
    , add2 Normal   "any"                        any_
    , add2 Normal   "all"                        all_
    , add3 Normal   "foldl'"                     foldl'_
    , add  Normal   "head"                       head_
    , add  Normal   "tail"                       tail_
    , add  Normal   "splitVersion"               splitVersion_
    , add2 Normal   "compareVersions"            compareVersions_
    , add2 Normal   "compareVersions"            compareVersions_
    , add' Normal   "sub"                        (arity2 ((-) @Integer))
    , add' Normal   "parseDrvName"               parseDrvName
    , add' Normal   "substring"                  substring
    , add' Normal   "stringLength"               (arity1 Text.length)
    , add  Normal   "attrNames"                  attrNames
    , add  Normal   "attrValues"                 attrValues
    , add2 Normal   "catAttrs"                   catAttrs
    , add' Normal   "concatStringsSep"           (arity2 Text.intercalate)
    , add  Normal   "unsafeDiscardStringContext" unsafeDiscardStringContext
    , add2 Normal   "seq"                        seq_
    , add2 Normal   "deepSeq"                    deepSeq
    , add2 Normal   "elem"                       elem_
    , add2 Normal   "genList"                    genList
    , add' Normal   "replaceStrings"             replaceStrings
    , add  Normal   "isAttrs"                    isAttrs
    , add  Normal   "isList"                     isList
    , add  Normal   "isFunction"                 isFunction
    , add  Normal   "isString"                   isString
    , add  Normal   "isInt"                      isInt
    , add  Normal   "isFloat"                    isFloat
    , add  Normal   "isBool"                     isBool
    , add2 Normal   "sort"                       sort_
    , add2 Normal   "lessThan"                   lessThan
    , add  Normal   "concatLists"                concatLists
    , add' Normal   "hashString"                 hashString
    , add  Normal   "readFile"                   readFile_
    , add  Normal   "readDir"                    readDir_
  ]
  where
    wrap t n f = Builtin t (n, f)

    arity1 f = Prim . pure . f
    arity2 f = ((Prim . pure) .) . f

    add  t n v = wrap t n <$> thunk (builtin  (Text.unpack n) v)
    add2 t n v = wrap t n <$> thunk (builtin2 (Text.unpack n) v)
    add3 t n v = wrap t n <$> thunk (builtin3 (Text.unpack n) v)

    add' :: ToBuiltin m a => BuiltinType -> Text -> a -> m (Builtin m)
    add' t n v = wrap t n <$> thunk (toBuiltin (Text.unpack n) v)

-- Helpers

mkBool :: MonadNix m => Bool -> m (NValue m)
mkBool = return . NVConstant . NBool

extractBool :: MonadBuiltins e m => NValue m -> m Bool
extractBool = \case
    NVConstant (NBool b) -> return b
    _ -> throwError "Not a boolean constant"

apply :: MonadBuiltins e m
      => NThunk m -> NThunk m -> m (NValue m)
apply f arg = evalApp (force f) (force arg)

-- Primops

toString :: MonadBuiltins e m => NThunk m -> m (NValue m)
toString str = do
    (s, d) <- valueText False =<< normalForm =<< force str
    return $ NVStr s d

hasAttr :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
hasAttr x y = (,) <$> force x <*> force y >>= \case
    (NVStr key _, NVSet aset) ->
        return . NVConstant . NBool $ M.member key aset
    (x, y) -> throwError $ "Invalid types for builtin.hasAttr: "
                 ++ show (() <$ x, () <$ y)

getAttr :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
getAttr x y = (,) <$> force x <*> force y >>= \case
    (NVStr key _, NVSet aset) -> case M.lookup key aset of
        Nothing -> throwError $ "hasAttr: field does not exist: "
                      ++ Text.unpack key
        Just action -> force action
    (x, y) -> throwError $ "Invalid types for builtin.hasAttr: "
                 ++ show (() <$ x, () <$ y)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = return False
anyM p (x:xs)   = do
        q <- p x
        if q then return True
             else anyM p xs

any_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
any_ pred = force >=> \case
    NVList l ->
        mkBool =<< anyM extractBool =<< mapM (apply pred) l
    arg -> throwError $ "builtins.any takes a list as second argument, not a "
              ++ show (() <$ arg)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x:xs)   = do
        q <- p x
        if q then allM p xs
             else return False

all_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
all_ pred = force >=> \case
    NVList l ->
        mkBool =<< allM extractBool =<< mapM (apply pred) l
    arg -> throwError $ "builtins.all takes a list as second argument, not a "
              ++ show (() <$ arg)

--TODO: Strictness
foldl'_ :: MonadBuiltins e m => NThunk m -> NThunk m -> NThunk m -> m (NValue m)
foldl'_ f z = force >=> \case
    NVList vals -> force =<< foldlM go z vals
    arg -> throwError $ "builtins.foldl' takes a list as third argument, not a "
              ++ show (() <$ arg)
  where
    go b a = do
        f' <- thunk $ apply f b
        thunk $ apply f' a

head_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
head_ = force >=> \case
    NVList vals -> case vals of
        [] -> throwError "builtins.head: empty list"
        h:_ -> force h
    _ -> throwError "builtins.head: not a list"

tail_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
tail_ = force >=> \case
    NVList vals -> case vals of
        [] -> throwError "builtins.tail: empty list"
        _:t -> return $ NVList t
    _ -> throwError "builtins.tail: not a list"

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

splitVersion_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
splitVersion_ = force >=> \case
    NVStr s _ -> do
        vals <- forM (splitVersion s) $ \c ->
            valueThunk $ NVStr (versionComponentToString c) mempty
        return $ NVList vals
    _ -> throwError "builtins.splitVersion: not a string"

compareVersions :: Text -> Text -> Ordering
compareVersions s1 s2 =
    mconcat $ alignWith f (splitVersion s1) (splitVersion s2)
  where
    z = VersionComponent_String ""
    f = uncurry compare . fromThese z z

compareVersions_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
compareVersions_ t1 t2 = do
    v1 <- force t1
    v2 <- force t2
    case (v1, v2) of
        (NVStr s1 _, NVStr s2 _) ->
            return $ NVConstant $ NInt $ case compareVersions s1 s2 of
                LT -> -1
                EQ -> 0
                GT -> 1
        _ -> throwError "builtins.splitVersion: not a string"

splitDrvName :: Text -> (Text, Text)
splitDrvName s =
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

parseDrvName :: Applicative m => Text -> Prim m (HashMap Text Text)
parseDrvName s = Prim $ pure $ M.fromList [("name", name), ("version", version)]
    where (name, version) = splitDrvName s

substring :: Applicative m => Int -> Int -> Text -> Prim m Text
substring start len =
    if start < 0 --NOTE: negative values of 'len' are OK
    then error $ "builtins.substring: negative start position: " ++ show start
    else Prim . pure . Text.take len . Text.drop start

attrNames :: MonadBuiltins e m => NThunk m -> m (NValue m)
attrNames = force >=> \case
    NVSet m -> toValue $ M.keys m
    v -> error $ "builtins.attrNames: Expected attribute set, got "
            ++ show (void v)

attrValues :: MonadBuiltins e m => NThunk m -> m (NValue m)
attrValues = force >=> \case
    NVSet m -> return $ NVList $ M.elems m
    v -> error $ "builtins.attrValues: Expected attribute set, got "
            ++ show (void v)

map_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
map_ f = force >=> \case
    NVList l -> NVList <$> traverse (valueThunk <=< apply f) l
    v -> error $ "map: Expected list, got " ++ show (void v)

catAttrs :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
catAttrs attrName lt = force lt >>= \case
    NVList l -> fmap (NVList . catMaybes) $ forM l $ force >=> \case
        NVSet m -> force attrName >>= \case
            NVStr n _ -> return $ M.lookup n m
            v -> throwError $ "builtins.catAttrs: Expected a string, got "
                    ++ show (void v)
        v -> throwError $ "builtins.catAttrs: Expected a set, got "
                ++ show (void v)
    v -> throwError $ "builtins.catAttrs: Expected a list, got "
            ++ show (void v)

baseNameOf :: Text -> Text
baseNameOf = Text.pack . takeFileName . Text.unpack

unsafeDiscardStringContext :: MonadBuiltins e m => NThunk m -> m (NValue m)
unsafeDiscardStringContext = force >=> \case
    NVStr s _ -> pure $ NVStr s mempty
    v -> throwError $ "builtins.unsafeDiscardStringContext: "
            ++ "Expected a string, got " ++ show (void v)

seq_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
seq_ a b = do
    _ <- force a
    force b

deepSeq :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
deepSeq a b = do
    _ <- normalForm =<< force a
    force b

elem_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
elem_ x xs = force xs >>= \case
    NVList l -> toValue =<< anyM (thunkEq x) l
    v -> throwError $ "builtins.elem: Expected a list, got " ++ show (void v)

genList :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
genList generator length = force length >>= \case
    NVConstant (NInt n) | n >= 0 -> fmap NVList $ forM [0 .. n - 1] $ \i -> do
        thunk $ apply generator =<< valueThunk =<< toValue i
    v -> throwError $ "builtins.genList: Expected a non-negative number, got " ++ show (void v)

--TODO: Preserve string context
replaceStrings :: MonadBuiltins e m => [Text] -> [Text] -> Text -> Prim m Text
replaceStrings from to s = Prim $ do
    when (length from /= length to) $ throwError "'from' and 'to' arguments to 'replaceStrings' have different lengths"
    let lookupPrefix s = do
            (prefix, replacement) <- find ((`Text.isPrefixOf` s) . fst) $ zip from to
            let rest = Text.drop (Text.length prefix) s
            return $ (prefix, replacement, rest)
        finish = LazyText.toStrict . Builder.toLazyText
        go orig result = case lookupPrefix orig of
            Nothing -> case Text.uncons orig of
                Nothing -> finish result
                Just (h, t) -> go t $ result <> Builder.singleton h
            Just (prefix, replacement, rest) -> case prefix of
                "" -> case Text.uncons rest of
                    Nothing -> finish $ result <> Builder.fromText replacement
                    Just (h, t) -> go t $ mconcat
                        [ result
                        , Builder.fromText replacement
                        , Builder.singleton h
                        ]
                _ -> go rest $ result <> Builder.fromText replacement
    return $ go s mempty

removeAttrs :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
removeAttrs set list = do
    toRemove <- fromThunk @[Text] list
    force set >>= \case
        NVSet m -> return $ NVSet $ foldl' (flip M.delete) m toRemove
        v -> throwError $ "removeAttrs: expected set, got " ++ show (void v)

isAttrs :: MonadBuiltins e m => NThunk m -> m (NValue m)
isAttrs = force >=> \case
    NVSet _ -> toValue True
    _ -> toValue False

isList :: MonadBuiltins e m => NThunk m -> m (NValue m)
isList = force >=> \case
    NVList _ -> toValue True
    _ -> toValue False

isFunction :: MonadBuiltins e m => NThunk m -> m (NValue m)
isFunction = force >=> \case
    NVFunction _ _ -> toValue True
    _ -> toValue False

isString :: MonadBuiltins e m => NThunk m -> m (NValue m)
isString = force >=> \case
    NVStr _ _ -> toValue True
    _ -> toValue False

isInt :: MonadBuiltins e m => NThunk m -> m (NValue m)
isInt = force >=> \case
    NVConstant (NInt _) -> toValue True
    _ -> toValue False

isFloat :: MonadBuiltins e m => NThunk m -> m (NValue m)
isFloat = force >=> \case
    NVConstant (NFloat _) -> toValue True
    _ -> toValue False

isBool :: MonadBuiltins e m => NThunk m -> m (NValue m)
isBool = force >=> \case
    NVConstant (NBool _) -> toValue True
    _ -> toValue False

isNull :: MonadBuiltins e m => NThunk m -> m (NValue m)
isNull = force >=> \case
    NVConstant NNull -> toValue True
    _ -> toValue False

sort_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
sort_ comparator list = force list >>= \case
    NVList l -> NVList <$> sortByM cmp l
        where cmp a b = do
                  isLessThan <- force comparator
                      `evalApp` force a
                      `evalApp` force b
                  fromValue isLessThan >>= \case
                      True -> pure LT
                      False -> do
                          isGreaterThan <- force comparator
                              `evalApp` force b
                              `evalApp` force a
                          fromValue isGreaterThan >>= \case
                              True -> pure GT
                              False -> pure EQ
    v -> throwError $ "builtins.sort: expected list, got " ++ show (void v)

lessThan :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
lessThan ta tb = do
    va <- force ta
    vb <- force tb
    let badType = throwError $ "builtins.lessThan: expected two numbers or two strings, "
            ++ "got " ++ show (void va) ++ " and " ++ show (void vb)
    NVConstant . NBool <$> case (va, vb) of
        (NVConstant ca, NVConstant cb) -> case (ca, cb) of
            (NInt   a, NInt   b) -> pure $ a < b
            (NFloat a, NInt   b) -> pure $ a < fromInteger b
            (NInt   a, NFloat b) -> pure $ fromInteger a < b
            (NFloat a, NFloat b) -> pure $ a < b
            _ -> badType
        (NVStr a _, NVStr b _) -> pure $ a < b
        _ -> badType

concatLists :: MonadBuiltins e m => NThunk m -> m (NValue m)
concatLists = force >=> \case
    NVList l -> fmap (NVList . concat) $ forM l $ force >=> \case
        NVList i -> pure i
        v -> throwError $ "builtins.concatLists: expected list, got " ++ show (void v)
    v -> throwError $ "builtins.concatLists: expected list, got " ++ show (void v)

hashString :: MonadBuiltins e m => Text -> Text -> Prim m Text
hashString algo s = Prim $ do
    hash <- case algo of
        "md5" -> pure MD5.hash
        "sha1" -> pure SHA1.hash
        "sha256" -> pure SHA256.hash
        "sha512" -> pure SHA512.hash
        _ -> throwError $ "builtins.hashString: "
            ++ "expected \"md5\", \"sha1\", \"sha256\", or \"sha512\", got " ++ show algo
    pure $ decodeUtf8 $ Base16.encode $ hash $ encodeUtf8 s

absolutePathFromValue :: MonadBuiltins e m => NValue m -> m FilePath
absolutePathFromValue = \case
    NVStr pathText _ -> do
        let path = Text.unpack pathText
        when (not $ isAbsolute path) $
            throwError $ "string " ++ show path ++ " doesn't represent an absolute path"
        pure path
    NVLiteralPath path -> pure path
    NVEnvPath path -> pure path
    v -> throwError $ "expected a path, got " ++ show (void v)

--TODO: Move all liftIO things into MonadNixEnv or similar
readFile_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
readFile_ pathThunk = do
    path <- absolutePathFromValue =<< force pathThunk
    toValue =<< liftIO (Text.readFile path)

data FileType
   = FileType_Regular
   | FileType_Directory
   | FileType_Symlink
   | FileType_Unknown
   deriving (Show, Read, Eq, Ord)

instance ToNix FileType where
    toValue = toValue . \case
        FileType_Regular -> "regular" :: Text
        FileType_Directory -> "directory"
        FileType_Symlink -> "symlink"
        FileType_Unknown -> "unknown"

readDir_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
readDir_ pathThunk = do
    path <- absolutePathFromValue =<< force pathThunk
    items <- liftIO $ listDirectory path
    itemsWithTypes <- liftIO $ forM items $ \item -> do
        s <- getSymbolicLinkStatus $ path </> item
        let t = if
                | isRegularFile s -> FileType_Regular
                | isDirectory s -> FileType_Directory
                | isSymbolicLink s -> FileType_Symlink
                | otherwise -> FileType_Unknown
        pure (Text.pack item, t)
    toValue $ M.fromList itemsWithTypes

newtype Prim m a = Prim { runPrim :: m a }

class ToNix a where
    toValue :: MonadBuiltins e m => a -> m (NValue m)

instance ToNix Bool where
    toValue = return . NVConstant . NBool

instance ToNix Text where
    toValue s = return $ NVStr s mempty

instance ToNix Int where
    toValue = toValue . toInteger

instance ToNix Integer where
    toValue = return . NVConstant . NInt

instance ToNix a => ToNix (HashMap Text a) where
    toValue m = NVSet <$> traverse (thunk . toValue) m

instance ToNix a => ToNix [a] where
    toValue m = NVList <$> traverse (thunk . toValue) m

-- | Types that support conversion to nix in a particular monad
class ToBuiltin m a | a -> m where
    toBuiltin :: String -> a -> m (NValue m)

instance (MonadBuiltins e m, ToNix a) => ToBuiltin m (Prim m a) where
    toBuiltin _ p = toValue =<< runPrim p

instance (MonadBuiltins e m, FromNix a, ToBuiltin m b)
      => ToBuiltin m (a -> b) where
    toBuiltin name f =
        return $ NVBuiltin name $ \a -> toBuiltin name . f =<< fromThunk a

class FromNix a where
    --TODO: Get rid of the HasCallStack - it should be captured by whatever
    --error reporting mechanism we add
    fromValue :: (HasCallStack, MonadBuiltins e m) => NValue m -> m a

fromThunk :: forall a e m. (FromNix a, HasCallStack, MonadBuiltins e m)
          => NThunk m -> m a
fromThunk = fromValue <=< force

instance FromNix Bool where
    fromValue = \case
        NVConstant (NBool b) -> pure b
        v -> throwError $ "fromValue: Expected bool, got " ++ show (void v)

instance FromNix Text where
    fromValue = \case
        NVStr s _ -> pure s
        v -> throwError $ "fromValue: Expected string, got " ++ show (void v)

instance FromNix Int where
    fromValue = fmap fromInteger . fromValue

instance FromNix Integer where
    fromValue = \case
        NVConstant (NInt n) -> pure n
        v -> throwError $ "fromValue: Expected number, got " ++ show (void v)

instance FromNix a => FromNix [a] where
    fromValue = \case
        NVList l -> traverse fromThunk l
        v -> throwError $ "fromValue: Expected list, got " ++ show (void v)
