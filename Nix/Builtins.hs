{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Builtins (baseEnv) where

import           Control.Monad
import           Data.Align (alignWith)
import           Data.Char (isDigit)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.These (fromThese)
import           Data.Foldable (foldlM)
import           Data.Traversable (mapM)
import           GHC.Stack.Types (HasCallStack)
import           Nix.Atoms
import           Nix.Monad
import           Nix.Eval

baseEnv :: MonadNixEnv m => m (NScopes m)
baseEnv = do
    ref <- buildThunk $ NVSet <$> builtins
    lst <- (("builtins", ref) :) <$> topLevelBuiltins
    pushScope (Map.fromList lst) currentScope
  where
    topLevelBuiltins = map mapping . filter isTopLevel <$> builtinsList

builtins :: MonadNixEnv m => m (ValueSet m)
builtins = Map.fromList . map mapping <$> builtinsList

data BuiltinType = Normal | TopLevel
data Builtin m = Builtin
    { kind    :: BuiltinType
    , mapping :: (Text, NThunk m)
    }

isTopLevel :: Builtin m -> Bool
isTopLevel b = case kind b of Normal -> False; TopLevel -> True

builtinsList :: forall m. MonadNixEnv m => m [ Builtin m ]
builtinsList = sequence [
      add  TopLevel "toString"        toString
    , add  TopLevel "import"          importFile
    , add  Normal   "getEnv"          getEnvVar
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
    , add' Normal   "sub"             (arity2 ((-) @Integer))
    , add' Normal   "parseDrvName"    parseDrvName
    , add' Normal   "substring"       substring
    , add' Normal   "stringLength"    (arity1 Text.length)
    , add  Normal   "attrNames"       attrNames
  ]
  where
    wrap t n f = Builtin t (n, f)

    arity1 f = Prim . pure . f
    arity2 f = ((Prim . pure) .) . f

    add  t n v = wrap t n <$> buildThunk (builtin  (Text.unpack n) v)
    add2 t n v = wrap t n <$> buildThunk (builtin2 (Text.unpack n) v)
    add3 t n v = wrap t n <$> buildThunk (builtin3 (Text.unpack n) v)

    add' :: ToBuiltin m a => BuiltinType -> Text -> a -> m (Builtin m)
    add' t n v = wrap t n <$> buildThunk (toBuiltin (Text.unpack n) v)

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
        (`pushScope` (forceThunk =<< pred))
            =<< buildArgument params arg
    x -> error $ "Trying to call a " ++ show (() <$ x)

-- Primops

toString :: MonadNix m => NThunk m -> m (NValue m)
toString str = do
    (s, d) <- valueText =<< normalForm =<< forceThunk str
    return $ NVStr s d

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
            valueRef $ NVStr (versionComponentToString c) mempty
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

parseDrvName :: Applicative m => Text -> Prim m (Map Text Text)
parseDrvName s = Prim $ pure $ Map.fromList [("name", name), ("version", version)]
    where (name, version) = splitDrvName s

substring :: Applicative m => Int -> Int -> Text -> Prim m Text
substring start len =
    if start < 0 --NOTE: negative values of 'len' are OK
    then error $ "builtins.substring: negative start position: " ++ show start
    else Prim . pure . Text.take len . Text.drop start

attrNames :: MonadNix m => NThunk m -> m (NValue m)
attrNames = forceThunk >=> \case
    NVSet m -> toValue $ Map.keys m
    v -> error $ "fromThunk: Expected number, got " ++ show (void v)

newtype Prim m a = Prim { runPrim :: m a }

class ToNix a where
    toValue :: MonadNix m => a -> m (NValue m)

instance ToNix Text where
    toValue s = return $ NVStr s mempty

instance ToNix Int where
    toValue = toValue . toInteger

instance ToNix Integer where
    toValue = return . NVConstant . NInt

instance ToNix a => ToNix (Map Text a) where
    toValue m = NVSet <$> traverse (buildThunk . toValue) m

instance ToNix a => ToNix [a] where
    toValue m = NVList <$> traverse (buildThunk . toValue) m

-- | Types that support conversion to nix in a particular monad
class ToBuiltin m a | a -> m where
    toBuiltin :: String -> a -> m (NValue m)

instance (MonadNix m, ToNix a) => ToBuiltin m (Prim m a) where
    toBuiltin _ p = toValue =<< runPrim p

instance (MonadNix m, FromNix a, ToBuiltin m b) => ToBuiltin m (a -> b) where
    toBuiltin name f = return $ NVBuiltin name $ \a -> toBuiltin name . f =<< fromThunk a

class FromNix a where
    --TODO: Get rid of the HasCallStack - it should be captured by whatever error reporting mechanism we add
    fromThunk :: (HasCallStack, MonadNix m) => NThunk m -> m a

instance FromNix Text where
    fromThunk = forceThunk >=> \case
        NVStr s _ -> pure s
        v -> error $ "fromThunk: Expected string, got " ++ show (void v)

instance FromNix Int where
    fromThunk = fmap fromInteger . fromThunk

instance FromNix Integer where
    fromThunk = forceThunk >=> \case
        NVConstant (NInt n) -> pure n
        v -> error $ "fromThunk: Expected number, got " ++ show (void v)
