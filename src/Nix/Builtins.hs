{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Builtins (MonadBuiltins, baseEnv) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.ListM (sortByM)
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import           Data.Align (alignWith)
import           Data.Array
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import           Data.Char (isDigit)
import           Data.Coerce
import           Data.Foldable (foldlM, foldrM)
import qualified Data.HashMap.Lazy as M
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import           Data.These (fromThese)
import           Data.Traversable (mapM)
import qualified Data.Vector as V
import           Language.Haskell.TH.Syntax (addDependentFile, runIO)
import           Nix.Atoms
import           Nix.Convert
import           Nix.Effects
import           Nix.Eval
import           Nix.Exec
import           Nix.Expr.Types
import           Nix.Normal
import           Nix.Parser
import           Nix.Scope
import           Nix.Stack
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value
import           Nix.XML
import           System.FilePath
import           System.Posix.Files
import           Text.Regex.TDFA

type MonadBuiltins e m =
    (Scoped e (NThunk m) m,
     Framed e m, MonadVar m, MonadFile m, MonadCatch m,
     MonadEffects m, MonadFix m)

baseEnv :: (MonadBuiltins e m, Scoped e (NThunk m) m)
        => m (Scopes m (NThunk m))
baseEnv = do
    ref <- thunk $ flip NVSet M.empty <$> builtins
    lst <- ([("builtins", ref)] ++) <$> topLevelBuiltins
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

valueThunk :: forall e m. MonadBuiltins e m => NValue m -> NThunk m
valueThunk = value @_ @_ @m

builtinsList :: forall e m. MonadBuiltins e m => m [ Builtin m ]
builtinsList = sequence [
      do version <- toValue ("2.0" :: Text)
         pure $ Builtin Normal ("nixVersion", version)

    , add0 TopLevel "__nixPath"                  nixPath
    , add  TopLevel "toString"                   toString
    , add  TopLevel "import"                     import_
    , add2 TopLevel "map"                        map_
    , add  TopLevel "baseNameOf"                 baseNameOf
    , add  TopLevel "dirOf"                      dirOf
    , add2 TopLevel "removeAttrs"                removeAttrs
    , add  TopLevel "isNull"                     isNull
    , add  TopLevel "abort"                      throw_ -- for now
    , add  TopLevel "throw"                      throw_
    , add2 TopLevel "scopedImport"               scopedImport
    , add  TopLevel "derivationStrict"           derivationStrict_
    , add0 TopLevel "derivation"                 $(do
          let f = "data/nix/corepkgs/derivation.nix"
          addDependentFile f
          Success expr <- runIO $ parseNixFile f
          [| evalExpr expr |]
      )

    , add  Normal   "getEnv"                     getEnv_
    , add2 Normal   "hasAttr"                    hasAttr
    , add2 Normal   "getAttr"                    getAttr
    , add2 Normal   "unsafeGetAttrPos"           unsafeGetAttrPos
    , add2 Normal   "any"                        any_
    , add2 Normal   "all"                        all_
    , add3 Normal   "foldl'"                     foldl'_
    , add  Normal   "head"                       head_
    , add  Normal   "tail"                       tail_
    , add  Normal   "splitVersion"               splitVersion_
    , add2 Normal   "compareVersions"            compareVersions_
    , add2 Normal   "match"                      match_
    -- jww (2018-04-09): Support floats for `add` and `sub`
    , add2 Normal   "split"                      split_
    , add' Normal   "add"                        (arity2 ((+) @Integer))
    , add' Normal   "sub"                        (arity2 ((-) @Integer))
    , add  Normal   "parseDrvName"               parseDrvName
    , add' Normal   "substring"                  substring
    , add' Normal   "stringLength"               (arity1 Text.length)
    , add  Normal   "length"                     length_
    , add' Normal   "attrNames"                  (arity1 (attrNames @m))
    , add' Normal   "attrValues"                 (arity1 (attrValues @m))
    , add2 Normal   "catAttrs"                   catAttrs
    , add' Normal   "concatStringsSep"           (arity2 Text.intercalate)
    , add  Normal   "unsafeDiscardStringContext" unsafeDiscardStringContext
    , add2 Normal   "seq"                        seq_
    , add2 Normal   "deepSeq"                    deepSeq
    , add2 Normal   "elem"                       elem_
    , add2 Normal   "elemAt"                     elemAt_
    , add2 Normal   "genList"                    genList
    , add2 Normal   "filter"                     filter_
    , add3 Normal   "replaceStrings"             replaceStrings
    , add  Normal   "pathExists"                 pathExists_
    , add  Normal   "toPath"                     toPath
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
    , add  Normal   "listToAttrs"                listToAttrs
    , add2 Normal   "intersectAttrs"             intersectAttrs
    , add  Normal   "functionArgs"               functionArgs
    , add' Normal   "hashString"                 hashString
    , add  Normal   "readFile"                   readFile_
    , add  Normal   "readDir"                    readDir_
    , add  Normal   "toXML"                      toXML_
    , add  Normal   "typeOf"                     typeOf
    , add2 Normal   "partition"                  partition_
    , add0 Normal   "currentSystem"              currentSystem
    , add  Normal   "tryEval"                    tryEval
    , add  Normal   "fetchTarball"               fetchTarball
    , add  Normal   "fromJSON"                   fromJSON
    , add' Normal   "toJSON"
      (arity1 $ decodeUtf8 . LBS.toStrict . A.encodingToLazyByteString
                           . toEncodingSorted)
  ]
  where
    wrap t n f = Builtin t (n, f)

    arity1 f = Prim . pure . f
    arity2 f = ((Prim . pure) .) . f

    add0 t n v = wrap t n <$> thunk v
    add  t n v = wrap t n <$> thunk (builtin  (Text.unpack n) v)
    add2 t n v = wrap t n <$> thunk (builtin2 (Text.unpack n) v)
    add3 t n v = wrap t n <$> thunk (builtin3 (Text.unpack n) v)

    add' :: ToBuiltin m a => BuiltinType -> Text -> a -> m (Builtin m)
    add' t n v = wrap t n <$> thunk (toBuiltin (Text.unpack n) v)

-- Helpers

call1 :: MonadBuiltins e m
      => NThunk m -> NThunk m -> m (NValue m)
call1 f arg = force f $ \f' -> force arg (callFunc f' . pure)

call2 :: MonadBuiltins e m
      => NThunk m -> NThunk m -> NThunk m -> m (NValue m)
call2 f arg1 arg2 = force f $ \f' ->
    callFunc f' (force arg1 pure) >>= \g ->
        callFunc g (force arg2 pure)

-- Primops

foldNixPath :: forall e m r. MonadBuiltins e m
            => (FilePath -> Maybe String -> r -> m r) -> r -> m r
foldNixPath f z = do
    mres <- lookupVar @_ @(NThunk m) "__includes"
    dirs <- case mres of
        Nothing -> return []
        Just v -> force v $ \case
            NVList xs -> forM xs $ flip force $ \case
                NVStr s _ -> pure s
                _ -> error "impossible"
            _ -> error "impossible"
    menv <- getEnvVar "NIX_PATH"
    foldrM go z $ dirs ++ case menv of
        Nothing -> []
        Just str -> Text.splitOn ":" (Text.pack str)
  where
    go x rest = case Text.splitOn "=" x of
        [p]    -> f (Text.unpack p) Nothing rest
        [n, p] -> f (Text.unpack p) (Just (Text.unpack n)) rest
        _ -> throwError $ "Unexpected entry in NIX_PATH: " ++ show x

nixPath :: MonadBuiltins e m => m (NValue m)
nixPath = fmap NVList $ flip foldNixPath [] $ \p mn rest -> pure $
    (valueThunk . flip NVSet M.empty . M.fromList $
        [ ("path",   valueThunk $ NVPath p)
        , ("prefix", valueThunk $
              NVStr (Text.pack (fromMaybe "" mn)) mempty) ]) : rest

toString :: MonadBuiltins e m => NThunk m -> m (NValue m)
toString str = do
    (s, d) <- force str $ normalForm >=> valueText False
    return $ NVStr s d

hasAttr :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
hasAttr x y = force x $ \x' -> force y $ \y' -> case (x', y') of
    (NVStr key _, NVSet aset _) ->
        return . NVConstant . NBool $ M.member key aset
    (x, y) -> throwError $ "Invalid types for builtin.hasAttr: "
                 ++ show (x, y)

getAttr :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
getAttr x y = force x $ \x' -> force y $ \y' -> case (x', y') of
    (NVStr key _, NVSet aset _) -> case M.lookup key aset of
        Nothing -> throwError $ "getAttr: field does not exist: "
                      ++ Text.unpack key
        Just action -> force action pure
    (x, y) -> throwError $ "Invalid types for builtin.getAttr: "
                 ++ show (x, y)

unsafeGetAttrPos :: forall e m. MonadBuiltins e m
                 => NThunk m -> NThunk m -> m (NValue m)
unsafeGetAttrPos x y = force x $ \x' -> force y $ \y' -> case (x', y') of
    (NVStr key _, NVSet _ apos) -> case M.lookup key apos of
        Nothing ->
            throwError $ "unsafeGetAttrPos: field '" ++ Text.unpack key
                ++ "' does not exist in attr set: " ++ show apos
        Just delta -> toValue delta
    (x, y) -> throwError $ "Invalid types for builtin.unsafeGetAttrPos: "
                 ++ show (x, y)

-- This function is a bit special in that it doesn't care about the contents
-- of the list.
length_ :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
length_ = flip force $
    toValue . (length :: [NThunk m] -> Int) <=< fromValue

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = return False
anyM p (x:xs)   = do
        q <- p x
        if q then return True
             else anyM p xs

any_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
any_ pred = flip force $ \case
    NVList l ->
        toNix =<< anyM fromNix =<< mapM (call1 pred) l
    arg -> throwError $ "builtins.any takes a list as second argument, not a "
              ++ show arg

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x:xs)   = do
        q <- p x
        if q then allM p xs
             else return False

all_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
all_ pred = flip force $ \case
    NVList l ->
        toNix =<< allM fromNix =<< mapM (call1 pred) l
    arg -> throwError $ "builtins.all takes a list as second argument, not a "
              ++ show arg

--TODO: Strictness
foldl'_ :: MonadBuiltins e m => NThunk m -> NThunk m -> NThunk m -> m (NValue m)
foldl'_ f z = flip force $ \case
    NVList vals -> (`force` pure) =<< foldlM go z vals
    arg -> throwError $ "builtins.foldl' takes a list as third argument, not a "
              ++ show arg
  where
    go b a = thunk $ call2 f a b

head_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
head_ = flip force $ fromValue >=> \case
    [] -> throwError "builtins.head: empty list"
    h:_ -> force h pure

tail_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
tail_ = flip force $ fromValue >=> \case
    [] -> throwError "builtins.tail: empty list"
    _:t -> return $ NVList t

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
splitVersion_ = flip force $ \case
    NVStr s _ -> do
        let vals = flip map (splitVersion s) $ \c ->
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
compareVersions_ t1 t2 = force t1 $ \v1 -> force t2 $ \v2 -> case (v1, v2) of
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

parseDrvName :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
parseDrvName = flip force $ fromValue >=> \(s :: Text) -> do
    let (name :: Text, version :: Text) = splitDrvName s
    -- jww (2018-04-15): There should be an easier way to write this.
    (toValue =<<) $ sequence $ M.fromList
        [ ("name" :: Text, thunk (toValue name))
        , ("version",     thunk (toValue version)) ]

match_ :: forall e m. MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
match_ pat str = force pat $ \pat' -> force str $ \str' ->
    case (pat', str') of
        -- jww (2018-04-05): We should create a fundamental type for compiled
        -- regular expressions if it turns out they get used often.
        (NVStr p _, NVStr s _) -> do
            let re = makeRegex (encodeUtf8 p) :: Regex
            case matchOnceText re (encodeUtf8 s) of
                Just ("", sarr, "") -> do
                    let s = map fst (elems sarr)
                    NVList <$> traverse (toValue . decodeUtf8)
                        (if length s > 1 then tail s else s)
                _ -> pure $ NVConstant NNull
        (p, s) ->
            throwError $ "builtins.match: expected a regex"
                ++ " and a string, but got: " ++ show (p, s)

split_ :: forall e m. MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
split_ pat str = force pat $ \pat' -> force str $ \str' ->
    case (pat', str') of
        (NVStr p _, NVStr s _) ->
          let re = makeRegex (encodeUtf8 p) :: Regex
              haystack = encodeUtf8 s
           in return $ NVList $ splitMatches 0 (map elems $ matchAllText re haystack) haystack
        (p, s) ->
            throwError $ "builtins.match: expected a regex"
                ++ " and a string, but got: " ++ show (p, s)

splitMatches
  :: forall e m. MonadBuiltins e m
  => Int
  -> [[(ByteString, (Int, Int))]]
  -> ByteString
  -> [NThunk m]
splitMatches _ [] haystack = [thunkStr haystack]
splitMatches _ ([]:_) _ = error "Error in splitMatches: this should never happen!"
splitMatches numDropped (((_,(start,len)):captures):mts) haystack =
    thunkStr before : caps : splitMatches (numDropped + relStart + len) mts (B.drop len rest)
  where
    relStart = max 0 start - numDropped
    (before,rest) = B.splitAt relStart haystack
    caps = valueThunk $ NVList (map f captures)
    f (a,(s,_)) = if s < 0 then valueThunk (NVConstant NNull) else thunkStr a

thunkStr s = valueThunk (NVStr (decodeUtf8 s) mempty)

substring :: MonadBuiltins e m => Int -> Int -> Text -> Prim m Text
substring start len str = Prim $
    if start < 0 --NOTE: negative values of 'len' are OK
    then throwError $ "builtins.substring: negative start position: " ++ show start
    else pure $ Text.take len $ Text.drop start str

attrNames :: ValueSet m -> [Text]
attrNames = sort . M.keys

attrValues :: forall m. ValueSet m -> [NThunk m]
attrValues = fmap snd . sortOn (fst @Text @(NThunk m)) . M.toList

map_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
map_ f = flip force $ \case
    NVList l -> NVList <$> traverse (thunk . call1 f) l
    v -> throwError $ "map: Expected list, got " ++ show v

filter_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
filter_ f = flip force $ \case
    NVList l -> NVList <$> filterM (fromNix <=< call1 f) l
    v -> throwError $ "map: Expected list, got " ++ show v

catAttrs :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
catAttrs attrName lt = force lt $ \case
    NVList l -> fmap (NVList . catMaybes) $ forM l $ flip force $ \case
        NVSet m _ -> force attrName $ \case
            NVStr n _ -> return $ M.lookup n m
            v -> throwError $ "builtins.catAttrs: Expected a string, got "
                    ++ show v
        v -> throwError $ "builtins.catAttrs: Expected a set, got "
                ++ show v
    v -> throwError $ "builtins.catAttrs: Expected a list, got "
            ++ show v

baseNameOf :: MonadBuiltins e m => NThunk m -> m (NValue m)
baseNameOf = flip force $ \case
    --TODO: Only allow strings that represent absolute paths
    NVStr path ctx -> pure $ NVStr (Text.pack $ takeFileName $ Text.unpack path) ctx
    NVPath path -> pure $ NVPath $ takeFileName path
    v -> throwError $ "dirOf: expected string or path, got " ++ show v

dirOf :: MonadBuiltins e m => NThunk m -> m (NValue m)
dirOf = flip force $ \case
    --TODO: Only allow strings that represent absolute paths
    NVStr path ctx -> pure $ NVStr (Text.pack $ takeDirectory $ Text.unpack path) ctx
    NVPath path -> pure $ NVPath $ takeDirectory path
    v -> throwError $ "dirOf: expected string or path, got " ++ show v

unsafeDiscardStringContext :: MonadBuiltins e m => NThunk m -> m (NValue m)
unsafeDiscardStringContext = flip force $ \case
    NVStr s _ -> pure $ NVStr s mempty
    v -> throwError $ "builtins.unsafeDiscardStringContext: "
            ++ "Expected a string, got " ++ show v

seq_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
seq_ a b = force a (const (force b pure))

deepSeq :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
deepSeq a b = do
    -- We evaluate 'a' only for its effects, so data cycles are ignored.
    _ <- forceEffects (coerce a) $ \a' ->
        normalFormBy (forceEffects . coerce) a'

    -- Then we evaluate the other argument to deepseq, thus this function
    -- should always produce a result (unlike applying 'deepseq' on infinitely
    -- recursive data structures in Haskell).
    force b pure

elem_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
elem_ x xs = force xs $ \case
    NVList l -> toValue =<< anyM (thunkEq x) l
    v -> throwError $ "builtins.elem: Expected a list, got " ++ show v

elemAt_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
elemAt_ xs n = force n $ fromNix >=> \n' -> force xs $ \case
    NVList l | n' < length l -> force (l !! n') pure
             | otherwise ->
        throwError $ "builtins.elem: Index " ++ show n'
            ++ " too large for list of length " ++ show (length l)
    v -> throwError $ "builtins.elem: Expected a list, got " ++ show v

genList :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
genList generator length = force length $ \case
    NVConstant (NInt n) | n >= 0 -> fmap NVList $ forM [0 .. n - 1] $ \i ->
        thunk $ force generator (`callFunc` toValue i)
    v -> throwError $ "builtins.genList: Expected a non-negative number, got "
            ++ show v

--TODO: Preserve string context
replaceStrings :: MonadBuiltins e m => NThunk m -> NThunk m -> NThunk m -> m (NValue m)
replaceStrings tfrom tto ts =
    force tfrom $ fromNix >=> \(from :: [Text]) ->
     force tto  $ fromNix >=> \(to   :: [Text]) ->
      force ts  $ fromNix >=> \(s :: Text) -> do
        when (length from /= length to) $
            throwError $ "'from' and 'to' arguments to 'replaceStrings'"
                ++ " have different lengths"
        let lookupPrefix s = do
                (prefix, replacement) <-
                    find ((`Text.isPrefixOf` s) . fst) $ zip from to
                let rest = Text.drop (Text.length prefix) s
                return (prefix, replacement, rest)
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
        toNix $ go s mempty

removeAttrs :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
removeAttrs set list = force list $ fromNix >=> \(toRemove :: [Text]) ->
    force set $ \case
        NVSet m p -> return $ NVSet (go m toRemove) (go p toRemove)
        v -> throwError $ "removeAttrs: expected set, got " ++ show v
  where
    go = foldl' (flip M.delete)

intersectAttrs :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
intersectAttrs set1 set2 = force set1 $ \set1' -> force set2 $ \set2' ->
    case (set1', set2') of
        (NVSet s1 p1, NVSet s2 p2) ->
            return $ NVSet (s2 `M.intersection` s1) (p2 `M.intersection` p1)
        (v1, v2) ->
            throwError $ "builtins.intersectAttrs: expected two sets, got "
                ++ show v1 ++ " and " ++ show v2

functionArgs :: MonadBuiltins e m => NThunk m -> m (NValue m)
functionArgs fun = force fun $ \case
    NVClosure p _ ->
        -- jww (2018-04-05): Should we preserve the location where the
        -- function arguments were declared for __unsafeGetAttrPos?
        return $ flip NVSet M.empty $ valueThunk . NVConstant . NBool <$>
            case p of
                Param name -> M.singleton name False
                ParamSet s _ _ -> isJust <$> M.fromList s
    v -> throwError $ "builtins.functionArgs: expected function, got "
            ++ show v

toPath :: MonadBuiltins e m => NThunk m -> m (NValue m)
toPath = flip force $ \case
    NVStr p@(Text.uncons -> Just ('/', _)) _ ->
        return $ NVPath (Text.unpack p)
    v@(NVPath _) -> return v
    v -> throwError $ "builtins.toPath: expected string, got " ++ show v

pathExists_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
pathExists_ = flip force $ \case
    NVPath p  -> toNix =<< pathExists p
    -- jww (2018-04-13): Should this ever be a string?
    NVStr s _ -> toNix =<< pathExists (Text.unpack s)
    v -> throwError $ "builtins.pathExists: expected path, got " ++ show v

hasKind :: forall a e m. (MonadBuiltins e m, FromNix a m (NValue m))
        => NThunk m -> m (NValue m)
hasKind = fromNixMay >=> toNix . \case Just (_ :: a) -> True; _ -> False

isAttrs :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
isAttrs = hasKind @(ValueSet m)

isList :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
isList = hasKind @[NThunk m]

isString :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
isString = hasKind @Text

isInt :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
isInt = hasKind @Int

isFloat :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
isFloat = hasKind @Float

isBool :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
isBool = hasKind @Bool

isNull :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
isNull = hasKind @()

isFunction :: MonadBuiltins e m => NThunk m -> m (NValue m)
isFunction = flip force $ \case
    NVClosure {} -> toValue True
    _ -> toValue False

throw_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
throw_ = fromNix >=> throwError . Text.unpack

import_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
import_ = fromNix >=> importPath M.empty . getPath

scopedImport :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
scopedImport aset path = force aset $ \aset' -> force path $ \path' ->
    case (aset', path') of
        (NVSet s _, NVPath p) -> importPath s p
        (s, p) -> throwError $ "scopedImport: expected a set and a path, got "
                     ++ show s ++ " and " ++ show p

getEnv_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
getEnv_ = fromNix >=> \s -> do
    mres <- getEnvVar (Text.unpack s)
    toNix $ case mres of
        Nothing -> ""
        Just v  -> Text.pack v

sort_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
sort_ comparator list = force list $ \case
    NVList l -> NVList <$> sortByM cmp l
        where
          cmp a b = do
              isLessThan <- call2 comparator a b
              fromValue isLessThan >>= \case
                  True -> pure LT
                  False -> do
                      isGreaterThan <- call2 comparator b a
                      fromValue isGreaterThan >>= \case
                          True -> pure GT
                          False -> pure EQ
    v -> throwError $ "builtins.sort: expected list, got " ++ show v

lessThan :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
lessThan ta tb = force ta $ \va -> force tb $ \vb -> do
    let badType = throwError $ "builtins.lessThan: expected two numbers or two strings, "
            ++ "got " ++ show va ++ " and " ++ show vb
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
concatLists = flip force $ \case
    NVList l -> fmap (NVList . concat) $ forM l $ flip force $ \case
        NVList i -> pure i
        v -> throwError $ "builtins.concatLists: expected list, got " ++ show v
    v -> throwError $ "builtins.concatLists: expected list, got " ++ show v

listToAttrs :: MonadBuiltins e m => NThunk m -> m (NValue m)
listToAttrs = flip force $ \case
    NVList l -> fmap (flip NVSet M.empty . M.fromList . reverse) $
        forM l $ flip force $ \case
            NVSet s _ -> case (M.lookup "name" s, M.lookup "value" s) of
                (Just name, Just value) -> force name $ \case
                    NVStr n _ -> return (n, value)
                    v -> throwError $
                            "builtins.listToAttrs: expected name to be a string, got "
                            ++ show v
                _ -> throwError $
                    "builtins.listToAttrs: expected set with name and value, got"
                        ++ show s
            v -> throwError $ "builtins.listToAttrs: expected set, got " ++ show v
    v -> throwError $ "builtins.listToAttrs: expected list, got " ++ show v

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
        unless (isAbsolute path) $
            throwError $ "string " ++ show path ++ " doesn't represent an absolute path"
        pure path
    NVPath path -> pure path
    v -> throwError $ "expected a path, got " ++ show v

--TODO: Move all liftIO things into MonadNixEnv or similar
readFile_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
readFile_ path = force path $
    toValue <=< Nix.Stack.readFile <=< absolutePathFromValue

data FileType
   = FileType_Regular
   | FileType_Directory
   | FileType_Symlink
   | FileType_Unknown
   deriving (Show, Read, Eq, Ord)

instance Applicative m => ToNix FileType m (NValue m) where
    toNix = toNix . \case
        FileType_Regular   -> "regular" :: Text
        FileType_Directory -> "directory"
        FileType_Symlink   -> "symlink"
        FileType_Unknown   -> "unknown"

readDir_ :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
readDir_ pathThunk = do
    path <- force pathThunk absolutePathFromValue
    items <- listDirectory path
    itemsWithTypes <- forM items $ \item -> do
        s <- Nix.Effects.getSymbolicLinkStatus $ path </> item
        let t = if
                | isRegularFile s -> FileType_Regular
                | isDirectory s -> FileType_Directory
                | isSymbolicLink s -> FileType_Symlink
                | otherwise -> FileType_Unknown
        pure (Text.pack item, t)
    toNix (M.fromList itemsWithTypes)

fromJSON :: MonadBuiltins e m => NThunk m -> m (NValue m)
fromJSON t = force t $ fromValue >=> \encoded ->
    case A.eitherDecodeStrict' @A.Value $ encodeUtf8 encoded of
        Left jsonError -> throwError $ "builtins.fromJSON: " ++ jsonError
        Right v -> toValue v

toXML_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
toXML_ = flip force $ normalForm >=> \x ->
    pure $ NVStr (Text.pack (toXML x)) mempty

typeOf :: MonadBuiltins e m => NThunk m -> m (NValue m)
typeOf = flip force $ toNix @Text . \case
    NVConstant a -> case a of
        NInt _   -> "int"
        NFloat _ -> "float"
        NBool _  -> "bool"
        NNull    -> "null"
        NUri _   -> "string" --TODO: Should we get rid of NUri?
    NVStr _ _     -> "string"
    NVList _      -> "list"
    NVSet _ _     -> "set"
    NVClosure {}  -> "lambda"
    NVPath _      -> "path"
    NVBuiltin _ _ -> "lambda"

tryEval :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
tryEval e = catch (force e (pure . onSuccess)) (pure . onError)
  where
    onSuccess v = flip NVSet M.empty $ M.fromList
        [ ("success", valueThunk (NVConstant (NBool True)))
        , ("value", valueThunk v)
        ]

    onError :: SomeException -> NValue m
    onError _ = flip NVSet M.empty $ M.fromList
        [ ("success", valueThunk (NVConstant (NBool False)))
        , ("value", valueThunk (NVConstant (NBool False)))
        ]

fetchTarball :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
fetchTarball = flip force $ \case
    NVSet s _ -> case M.lookup "url" s of
        Nothing -> throwError "builtins.fetchTarball: Missing url attribute"
        Just url -> force url (go (M.lookup "sha256" s))
    v@NVStr {} -> go Nothing v
    v@(NVConstant (NUri _)) -> go Nothing v
    v -> throwError $ "builtins.fetchTarball: Expected URI or set, got "
            ++ show v
 where
    go :: Maybe (NThunk m) -> NValue m -> m (NValue m)
    go msha = \case
        NVStr uri _ -> fetch uri msha
        NVConstant (NUri uri) -> fetch uri msha
        v -> throwError $ "builtins.fetchTarball: Expected URI or string, got "
                ++ show v

{- jww (2018-04-11): This should be written using pipes in another module
    fetch :: Text -> Maybe (NThunk m) -> m (NValue m)
    fetch uri msha = case takeExtension (Text.unpack uri) of
        ".tgz" -> undefined
        ".gz"  -> undefined
        ".bz2" -> undefined
        ".xz"  -> undefined
        ".tar" -> undefined
        ext -> throwError $ "builtins.fetchTarball: Unsupported extension '"
                  ++ ext ++ "'"
-}

    fetch :: Text -> Maybe (NThunk m) -> m (NValue m)
    fetch uri Nothing =
        nixInstantiateExpr $ "builtins.fetchTarball \"" ++
            Text.unpack uri ++ "\""
    fetch url (Just m) = force m $ \case
        NVStr sha _ ->
            nixInstantiateExpr $ "builtins.fetchTarball { "
              ++ "url    = \"" ++ Text.unpack url ++ "\"; "
              ++ "sha256 = \"" ++ Text.unpack sha ++ "\"; }"
        v -> throwError $ "builtins.fetchTarball: sha256 must be a string, got "
                ++ show v

partition_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
partition_ f = flip force $ \case
    NVList l -> do
      let match t = call1 f t >>= \case
            NVConstant (NBool b) -> return (b, t)
            v -> throwError $ "partition: Expected boolean, got " ++ show v
      selection <- traverse match l
      let (right, wrong) = partition fst selection
      let makeSide = valueThunk . NVList . map snd
      return $ flip NVSet M.empty $
          M.fromList [("right", makeSide right), ("wrong", makeSide wrong)]
    v -> throwError $ "partition: Expected list, got " ++ show v

currentSystem :: MonadBuiltins e m => m (NValue m)
currentSystem = do
  os <- getCurrentSystemOS
  arch <- getCurrentSystemArch
  return $ NVStr (os <> "-" <> arch) mempty

derivationStrict_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
derivationStrict_ = force ?? derivationStrict

newtype Prim m a = Prim { runPrim :: m a }

-- | Types that support conversion to nix in a particular monad
class ToBuiltin m a | a -> m where
    toBuiltin :: String -> a -> m (NValue m)

instance (MonadBuiltins e m, ToNix a m (NValue m)) => ToBuiltin m (Prim m a) where
    toBuiltin _ p = toNix =<< runPrim p

instance (MonadBuiltins e m, FromNix a m (NValue m), ToBuiltin m b)
      => ToBuiltin m (a -> b) where
    toBuiltin name f =
        return $ NVBuiltin name $
            force ?? (fromNix >=> toBuiltin name . f)

toEncodingSorted :: A.Value -> A.Encoding
toEncodingSorted = \case
    A.Object m -> A.pairs $ mconcat $ fmap (\(k, v) -> A.pair k $ toEncodingSorted v) $ sortOn fst $ M.toList m
    A.Array l -> A.list toEncodingSorted $ V.toList l
    v -> A.toEncoding v
