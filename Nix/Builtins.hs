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
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Builtins (MonadBuiltins, baseEnv) where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.ListM (sortByM)
import           Control.Monad.Reader
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512
import           Data.Aeson (toJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import           Data.Align (alignWith)
import           Data.Array
import           Data.ByteString (ByteString)
import           Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import           Data.Char (isDigit)
import           Data.Coerce
import           Data.Foldable (foldlM)
import           Data.Functor.Compose
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashMap.Strict.InsOrd as OM
import           Data.List
import           Data.Maybe
import           Data.Scientific
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import           Data.These (fromThese)
import           Data.Traversable (mapM)
import qualified Data.Vector as V
import           GHC.Stack.Types (HasCallStack)
import           Nix.Atoms
import           Nix.Eval
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Monad
import           Nix.Pretty
import           Nix.Scope
import           Nix.Stack
import           Nix.Thunk
import           Nix.Utils
import           Nix.XML
import           Text.Regex.TDFA
import           System.FilePath
import           System.Posix.Files

type MonadBuiltins e m =
    (MonadEval e m, MonadNix m, MonadFix m, MonadFile m, MonadVar m)

baseEnv :: MonadBuiltins e m => m (Scopes m (NThunk m))
baseEnv = do
    ref <- thunk $ flip NVSet M.empty <$> builtins
    let pos = repeatingThunk curPos -- re-evaluate each time it's forced
    lst <- ([ ("builtins", ref)
           , ("__curPos", pos)
           ] ++)
        <$> topLevelBuiltins
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
      pure $ Builtin Normal ("nixVersion", valueThunk $ NVStr "2.0" mempty)

    , add  TopLevel "toString"                   toString
    , add  TopLevel "import"                     import_
    , add2 TopLevel "map"                        map_
    , add' TopLevel "baseNameOf"                 (arity1 baseNameOf)
    , add  TopLevel "dirOf"                      dirOf
    , add2 TopLevel "removeAttrs"                removeAttrs
    , add  TopLevel "isNull"                     isNull
    , add  TopLevel "abort"                      throw_ -- for now
    , add  TopLevel "throw"                      throw_
    , add2 TopLevel "scopedImport"               scopedImport
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
    --TODO: Support floats for `add` and `sub`
    , add' Normal   "add"                        (arity2 ((+) @Integer))
    , add' Normal   "sub"                        (arity2 ((-) @Integer))
    , add' Normal   "parseDrvName"               parseDrvName
    , add' Normal   "substring"                  substring
    , add' Normal   "stringLength"               (arity1 Text.length)
    , add  Normal   "length"                     length_
    , add  Normal   "attrNames"                  attrNames
    , add  Normal   "attrValues"                 attrValues
    , add2 Normal   "catAttrs"                   catAttrs
    , add' Normal   "concatStringsSep"           (arity2 Text.intercalate)
    , add  Normal   "unsafeDiscardStringContext" unsafeDiscardStringContext
    , add2 Normal   "seq"                        seq_
    , add2 Normal   "deepSeq"                    deepSeq
    , add2 Normal   "elem"                       elem_
    , add2 Normal   "elemAt"                     elemAt_
    , add2 Normal   "genList"                    genList
    , add2 Normal   "filter"                     filter_
    , add' Normal   "replaceStrings"             replaceStrings
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
    , add' Normal   "toJSON"
      (arity1 $ decodeUtf8 . LBS.toStrict . A.encodingToLazyByteString
                           . toEncodingSorted)
    , add  Normal   "fromJSON"                   fromJSON
    , add  Normal   "toXML"                      toXML_
    , add  Normal   "typeOf"                     typeOf
    , add2 Normal   "partition"                  partition_
    , add0 Normal   "currentSystem"              currentSystem
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

mkBool :: Monad m => Bool -> m (NValue m)
mkBool = return . NVConstant . NBool

extractBool :: MonadBuiltins e m => NValue m -> m Bool
extractBool = \case
    NVConstant (NBool b) -> return b
    _ -> throwError "Not a boolean constant"

extractInt :: MonadBuiltins e m => NValue m -> m Int
extractInt = \case
    NVConstant (NInt b) -> return $ fromIntegral b
    _ -> throwError "Not an integer constant"

apply :: MonadBuiltins e m
      => NThunk m -> NThunk m -> m (NValue m)
apply f arg = force f $ \f' -> pure f' `evalApp` arg

-- Primops

deltaInfo :: Delta -> (Text, Int, Int)
deltaInfo = \case
    Columns c _         -> ("<string>", 1, fromIntegral c + 1)
    Tab {}              -> ("<string>", 1, 1)
    Lines l _ _ _       -> ("<string>", fromIntegral l + 1, 1)
    Directed fn l c _ _ -> (decodeUtf8 fn,
                           fromIntegral l + 1, fromIntegral c + 1)

posFromDelta :: Delta -> NValue m
posFromDelta (deltaInfo -> (f, l, c)) =
    flip NVSet M.empty $ M.fromList
        [ ("file", valueThunk $ NVStr f mempty)
        , ("line", valueThunk $ NVConstant (NInt (fromIntegral l)))
        , ("column", valueThunk $ NVConstant (NInt (fromIntegral c)))
        ]

curPos :: forall e m. Framed e m => m (NValue m)
curPos = do
    Compose (Ann (SrcSpan delta _) _):_ <-
        asks (mapMaybe (either (const Nothing) Just)
              . view @_ @Frames hasLens)
    return $ posFromDelta delta

toString :: MonadBuiltins e m => NThunk m -> m (NValue m)
toString str = do
    (s, d) <- force str $ normalForm >=> valueText False
    return $ NVStr s d

hasAttr :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
hasAttr x y = force x $ \x' -> force y $ \y' -> case (x', y') of
    (NVStr key _, NVSet aset _) ->
        return . NVConstant . NBool $ M.member key aset
    (x, y) -> throwError $ "Invalid types for builtin.hasAttr: "
                 ++ show (void x, void y)

getAttr :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
getAttr x y = force x $ \x' -> force y $ \y' -> case (x', y') of
    (NVStr key _, NVSet aset _) -> case M.lookup key aset of
        Nothing -> throwError $ "getAttr: field does not exist: "
                      ++ Text.unpack key
        Just action -> force action pure
    (x, y) -> throwError $ "Invalid types for builtin.getAttr: "
                 ++ show (void x, void y)

unsafeGetAttrPos :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
unsafeGetAttrPos x y = force x $ \x' -> force y $ \y' -> case (x', y') of
    (NVStr key _, NVSet _ apos) -> case M.lookup key apos of
        Nothing ->
            throwError $ "unsafeGetAttrPos: field '" ++ Text.unpack key
                ++ "' does not exist in attr set: " ++ show apos
        Just delta -> return $ posFromDelta delta
    (x, y) -> throwError $ "Invalid types for builtin.unsafeGetAttrPos: "
                 ++ show (void x, void y)

length_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
length_ = flip force $ \case
    NVList l -> return $ NVConstant $ NInt (fromIntegral (length l))
    arg -> throwError $ "builtins.length takes a list, not a "
              ++ show (void arg)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = return False
anyM p (x:xs)   = do
        q <- p x
        if q then return True
             else anyM p xs

any_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
any_ pred = flip force $ \case
    NVList l ->
        mkBool =<< anyM extractBool =<< mapM (apply pred) l
    arg -> throwError $ "builtins.any takes a list as second argument, not a "
              ++ show (void arg)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x:xs)   = do
        q <- p x
        if q then allM p xs
             else return False

all_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
all_ pred = flip force $ \case
    NVList l ->
        mkBool =<< allM extractBool =<< mapM (apply pred) l
    arg -> throwError $ "builtins.all takes a list as second argument, not a "
              ++ show (void arg)

--TODO: Strictness
foldl'_ :: MonadBuiltins e m => NThunk m -> NThunk m -> NThunk m -> m (NValue m)
foldl'_ f z = flip force $ \case
    NVList vals -> (`force` pure) =<< foldlM go z vals
    arg -> throwError $ "builtins.foldl' takes a list as third argument, not a "
              ++ show (void arg)
  where
    go b a = thunk $ f `apply` a `evalApp` b

head_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
head_ = flip force $ \case
    NVList vals -> case vals of
        [] -> throwError "builtins.head: empty list"
        h:_ -> force h pure
    _ -> throwError "builtins.head: not a list"

tail_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
tail_ = flip force $ \case
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

parseDrvName :: Applicative m => Text -> Prim m (HashMap Text Text)
parseDrvName s = Prim $ pure $ M.fromList [("name", name), ("version", version)]
    where (name, version) = splitDrvName s

match_ :: forall e m. MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
match_ pat str = force pat $ \pat' -> force str $ \str' ->
    case (pat', str') of
        -- jww (2018-04-05): We should create a fundamental type for compiled
        -- regular expressions if it turns out they get used often.
        (NVStr p _, NVStr s _) -> return $ NVList $
            let re = makeRegex (encodeUtf8 p) :: Regex
            in case matchOnceText re (encodeUtf8 s) of
                Just ("", sarr, "") -> let s = map fst (elems sarr) in
                    map (valueThunk @m . flip NVStr mempty . decodeUtf8)
                        (if length s > 1 then tail s else s)
                _ -> []
        (p, s) ->
            throwError $ "builtins.match: expected a regex"
                ++ " and a string, but got: " ++ show (p, s)

substring :: Applicative m => Int -> Int -> Text -> Prim m Text
substring start len =
    if start < 0 --NOTE: negative values of 'len' are OK
    then error $ "builtins.substring: negative start position: " ++ show start
    else Prim . pure . Text.take len . Text.drop start

attrNames :: MonadBuiltins e m => NThunk m -> m (NValue m)
attrNames = flip force $ \case
    NVSet m _ -> toValue $ sort $ M.keys m
    v -> error $ "builtins.attrNames: Expected attribute set, got "
            ++ showValue v

attrValues :: MonadBuiltins e m => NThunk m -> m (NValue m)
attrValues = flip force $ \case
    NVSet m _ -> return $ NVList $ fmap snd $ sortOn fst $ M.toList m
    v -> error $ "builtins.attrValues: Expected attribute set, got "
            ++ showValue v

map_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
map_ f = flip force $ \case
    NVList l -> NVList <$> traverse (fmap valueThunk . apply f) l
    v -> error $ "map: Expected list, got " ++ showValue v

filter_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
filter_ f = flip force $ \case
    NVList l -> NVList <$> filterM (extractBool <=< apply f) l
    v -> error $ "map: Expected list, got " ++ showValue v

catAttrs :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
catAttrs attrName lt = force lt $ \case
    NVList l -> fmap (NVList . catMaybes) $ forM l $ flip force $ \case
        NVSet m _ -> force attrName $ \case
            NVStr n _ -> return $ M.lookup n m
            v -> throwError $ "builtins.catAttrs: Expected a string, got "
                    ++ showValue v
        v -> throwError $ "builtins.catAttrs: Expected a set, got "
                ++ showValue v
    v -> throwError $ "builtins.catAttrs: Expected a list, got "
            ++ showValue v

--TODO: Make this have similar logic to dirOf
baseNameOf :: Text -> Text
baseNameOf = Text.pack . takeFileName . Text.unpack

dirOf :: MonadBuiltins e m => NThunk m -> m (NValue m)
dirOf = flip force $ \case
    --TODO: Only allow strings that represent absolute paths
    NVStr path ctx -> pure $ NVStr (Text.pack $ takeDirectory $ Text.unpack path) ctx
    NVLiteralPath path -> pure $ NVLiteralPath $ takeDirectory path
    --TODO: NVEnvPath
    v -> throwError $ "dirOf: expected string or path, got " ++ showValue v

unsafeDiscardStringContext :: MonadBuiltins e m => NThunk m -> m (NValue m)
unsafeDiscardStringContext = flip force $ \case
    NVStr s _ -> pure $ NVStr s mempty
    v -> throwError $ "builtins.unsafeDiscardStringContext: "
            ++ "Expected a string, got " ++ showValue v

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
    v -> throwError $ "builtins.elem: Expected a list, got " ++ showValue v

elemAt_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
elemAt_ xs n = force n $ extractInt >=> \n' -> force xs $ \case
    NVList l | n' < length l -> force (l !! n') pure
             | otherwise ->
        throwError $ "builtins.elem: Index " ++ show n'
            ++ " too large for list of length " ++ show (length l)
    v -> throwError $ "builtins.elem: Expected a list, got " ++ showValue v

genList :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
genList generator length = force length $ \case
    NVConstant (NInt n) | n >= 0 -> fmap NVList $ forM [0 .. n - 1] $ \i ->
        thunk $ apply generator =<< valueThunk <$> toValue i
    v -> throwError $ "builtins.genList: Expected a non-negative number, got "
            ++ showValue v

--TODO: Preserve string context
replaceStrings :: MonadBuiltins e m => [Text] -> [Text] -> Text -> Prim m Text
replaceStrings from to s = Prim $ do
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
    return $ go s mempty

removeAttrs :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
removeAttrs set list = fromThunk @[Text] list $ \toRemove ->
    force set $ \case
        NVSet m p -> return $ NVSet (go m toRemove) (go p toRemove)
        v -> throwError $ "removeAttrs: expected set, got " ++ showValue v
  where
    go = foldl' (flip M.delete)

intersectAttrs :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
intersectAttrs set1 set2 = force set1 $ \set1' -> force set2 $ \set2' ->
    case (set1', set2') of
        (NVSet s1 p1, NVSet s2 p2) ->
            return $ NVSet (s2 `M.intersection` s1) (p2 `M.intersection` p1)
        (v1, v2) ->
            throwError $ "builtins.intersectAttrs: expected two sets, got "
                ++ showValue v1 ++ " and " ++ showValue v2

functionArgs :: MonadBuiltins e m => NThunk m -> m (NValue m)
functionArgs fun = force fun $ \case
    NVClosure _ p _ ->
        -- jww (2018-04-05): Should we preserve the location where the
        -- function arguments were declared for __unsafeGetAttrPos?
        return $ flip NVSet M.empty $ valueThunk . NVConstant . NBool <$>
            case p of
                Param name -> M.singleton name False
                ParamSet s _ _ -> isJust <$> OM.toHashMap s
    v -> throwError $ "builtins.functionArgs: expected function, got "
            ++ showValue v

toPath :: MonadBuiltins e m => NThunk m -> m (NValue m)
toPath = flip force $ \case
    NVStr p@(Text.uncons -> Just ('/', _)) _ ->
        return $ NVLiteralPath (Text.unpack p)
    v@(NVLiteralPath _) -> return v
    v@(NVEnvPath _) -> return v
    v -> throwError $ "builtins.toPath: expected string, got " ++ showValue v

pathExists_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
pathExists_ = flip force $ \case
    NVLiteralPath p -> mkBool =<< pathExists p
    NVEnvPath p -> mkBool =<< pathExists p
    v -> throwError $ "builtins.pathExists: expected path, got " ++ showValue v

isAttrs :: MonadBuiltins e m => NThunk m -> m (NValue m)
isAttrs = flip force $ \case
    NVSet _ _ -> toValue True
    _ -> toValue False

isList :: MonadBuiltins e m => NThunk m -> m (NValue m)
isList = flip force $ \case
    NVList _ -> toValue True
    _ -> toValue False

isFunction :: MonadBuiltins e m => NThunk m -> m (NValue m)
isFunction = flip force $ \case
    NVClosure {} -> toValue True
    _ -> toValue False

isString :: MonadBuiltins e m => NThunk m -> m (NValue m)
isString = flip force $ \case
    NVStr _ _ -> toValue True
    _ -> toValue False

isInt :: MonadBuiltins e m => NThunk m -> m (NValue m)
isInt = flip force $ \case
    NVConstant (NInt _) -> toValue True
    _ -> toValue False

isFloat :: MonadBuiltins e m => NThunk m -> m (NValue m)
isFloat = flip force $ \case
    NVConstant (NFloat _) -> toValue True
    _ -> toValue False

isBool :: MonadBuiltins e m => NThunk m -> m (NValue m)
isBool = flip force $ \case
    NVConstant (NBool _) -> toValue True
    _ -> toValue False

isNull :: MonadBuiltins e m => NThunk m -> m (NValue m)
isNull = flip force $ \case
    NVConstant NNull -> toValue True
    _ -> toValue False

throw_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
throw_ = flip force $ \case
    NVStr t _ -> throwError (Text.unpack t)
    v -> throwError $ "builtins.throw: expected string, got " ++ showValue v

import_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
import_ = flip force $ \case
    NVLiteralPath p -> importFile M.empty p
    NVEnvPath p     -> importFile M.empty p -- jww (2018-04-06): is this right?
    v -> throwError $ "import: expected path, got " ++ showValue v

scopedImport :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
scopedImport aset path = force aset $ \aset' -> force path $ \path' ->
    case (aset', path') of
        (NVSet s _, NVLiteralPath p) -> importFile s p
        (NVSet s _, NVEnvPath p)     -> importFile s p
        (s, p) -> throwError $ "scopedImport: expected a set and a path, got "
                     ++ showValue s ++ " and " ++ showValue p

getEnv_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
getEnv_ = flip force $ \case
    NVStr s _ -> do
        mres <- getEnvVar (Text.unpack s)
        return $ case mres of
            Nothing -> NVStr "" mempty
            Just v  -> NVStr (Text.pack v) mempty
    p -> error $ "Unexpected argument to getEnv: " ++ show (void p)

sort_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
sort_ comparator list = force list $ \case
    NVList l -> NVList <$> sortByM cmp l
        where
          cmp a b = do
              isLessThan <- comparator `apply` a `evalApp` b
              fromValue isLessThan >>= \case
                  True -> pure LT
                  False -> do
                      isGreaterThan <- comparator `apply` b `evalApp` a
                      fromValue isGreaterThan >>= \case
                          True -> pure GT
                          False -> pure EQ
    v -> throwError $ "builtins.sort: expected list, got " ++ showValue v

lessThan :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
lessThan ta tb = force ta $ \va -> force tb $ \vb -> do
    let badType = throwError $ "builtins.lessThan: expected two numbers or two strings, "
            ++ "got " ++ showValue va ++ " and " ++ showValue vb
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
        v -> throwError $ "builtins.concatLists: expected list, got " ++ showValue v
    v -> throwError $ "builtins.concatLists: expected list, got " ++ showValue v

listToAttrs :: MonadBuiltins e m => NThunk m -> m (NValue m)
listToAttrs = flip force $ \case
    NVList l -> fmap (flip NVSet M.empty . M.fromList . reverse) $
        forM l $ flip force $ \case
            NVSet s _ -> case (M.lookup "name" s, M.lookup "value" s) of
                (Just name, Just value) -> force name $ \case
                    NVStr n _ -> return (n, value)
                    v -> throwError $
                            "builtins.listToAttrs: expected name to be a string, got "
                            ++ showValue v
                _ -> throwError $
                    "builtins.listToAttrs: expected set with name and value, got"
                        ++ show s
            v -> throwError $ "builtins.listToAttrs: expected set, got " ++ showValue v
    v -> throwError $ "builtins.listToAttrs: expected list, got " ++ showValue v

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
    NVLiteralPath path -> pure path
    NVEnvPath path -> pure path
    v -> throwError $ "expected a path, got " ++ showValue v

--TODO: Move all liftIO things into MonadNixEnv or similar
readFile_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
readFile_ pathThunk = do
    path <- force pathThunk absolutePathFromValue
    toValue =<< Nix.Stack.readFile path

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
    path <- force pathThunk absolutePathFromValue
    items <- listDirectory path
    itemsWithTypes <- forM items $ \item -> do
        s <- Nix.Monad.getSymbolicLinkStatus $ path </> item
        let t = if
                | isRegularFile s -> FileType_Regular
                | isDirectory s -> FileType_Directory
                | isSymbolicLink s -> FileType_Symlink
                | otherwise -> FileType_Unknown
        pure (Text.pack item, t)
    toValue $ M.fromList itemsWithTypes

fromJSON :: MonadBuiltins e m => NThunk m -> m (NValue m)
fromJSON t = fromThunk t $ \encoded ->
    case A.eitherDecodeStrict' @A.Value $ encodeUtf8 encoded of
        Left jsonError -> throwError $ "builtins.fromJSON: " ++ jsonError
        Right v -> toValue v

toXML_ :: MonadBuiltins e m => NThunk m -> m (NValue m)
toXML_ = flip force $ normalForm >=> \x ->
    pure $ NVStr (Text.pack (toXML x)) mempty

typeOf :: MonadBuiltins e m => NThunk m -> m (NValue m)
typeOf t = force t $ \v -> toValue @Text $ case v of
    NVConstant a -> case a of
        NInt _ -> "int"
        NFloat _ -> "float"
        NBool _ -> "bool"
        NNull -> "null"
        NUri _ -> "string" --TODO: Should we get rid of NUri?
    NVStr _ _ -> "string"
    NVList _ -> "list"
    NVSet _ _ -> "set"
    NVClosure {} -> "lambda"
    NVLiteralPath _ -> "path"
    NVEnvPath _ -> "path"
    NVBuiltin _ _ -> "lambda"

partition_ :: MonadBuiltins e m => NThunk m -> NThunk m -> m (NValue m)
partition_ f = flip force $ \case
    NVList l -> do
      let match t = apply f t >>= \case
            NVConstant (NBool b) -> return (b, t)
            v -> error $ "partition: Expected boolean, got " ++ showValue v
      selection <- traverse match l
      let (right, wrong) = partition fst selection
      let makeSide = valueThunk . NVList . map snd
      return $ flip NVSet M.empty $
          M.fromList [("right", makeSide right), ("wrong", makeSide wrong)]
    v -> error $ "partition: Expected list, got " ++ showValue v

currentSystem :: MonadNix m => m (NValue m)
currentSystem = do
  os <- getCurrentSystemOS
  arch <- getCurrentSystemArch
  return $ NVStr (os <> "-" <> arch) mempty

newtype Prim m a = Prim { runPrim :: m a }

class ToNix a where
    toValue :: MonadBuiltins e m => a -> m (NValue m)

instance ToNix Bool where
    toValue = return . NVConstant . NBool

instance ToNix Text where
    toValue s = return $ NVStr s mempty

instance ToNix ByteString where
    toValue s = return $ NVStr (decodeUtf8 s) mempty

instance ToNix Int where
    toValue = toValue . toInteger

instance ToNix Integer where
    toValue = return . NVConstant . NInt

instance ToNix a => ToNix (HashMap Text a) where
    toValue m = flip NVSet M.empty <$> traverse (thunk . toValue) m

instance ToNix a => ToNix [a] where
    toValue m = NVList <$> traverse (thunk . toValue) m

instance ToNix A.Value where
    toValue = \case
        A.Object m -> flip NVSet M.empty <$> traverse (thunk . toValue) m
        A.Array l -> NVList <$> traverse (thunk . toValue) (V.toList l)
        A.String s -> pure $ NVStr s mempty
        A.Number n -> pure $ NVConstant $ case floatingOrInteger n of
            Left r -> NFloat r
            Right i -> NInt i
        A.Bool b -> pure $ NVConstant $ NBool b
        A.Null -> pure $ NVConstant NNull

-- | Types that support conversion to nix in a particular monad
class ToBuiltin m a | a -> m where
    toBuiltin :: String -> a -> m (NValue m)

instance (MonadBuiltins e m, ToNix a) => ToBuiltin m (Prim m a) where
    toBuiltin _ p = toValue =<< runPrim p

instance (MonadBuiltins e m, FromNix a, ToBuiltin m b)
      => ToBuiltin m (a -> b) where
    toBuiltin name f =
        return $ NVBuiltin name $ fromThunk ?? (toBuiltin name . f)

class FromNix a where
    --TODO: Get rid of the HasCallStack - it should be captured by whatever
    --error reporting mechanism we add
    fromValue :: (HasCallStack, MonadBuiltins e m) => NValue m -> m a

fromThunk :: (FromNix a, HasCallStack, MonadBuiltins e m)
          => NThunk m -> (a -> m r) -> m r
fromThunk t f = force t (f <=< fromValue)

instance FromNix Bool where
    fromValue = \case
        NVConstant (NBool b) -> pure b
        v -> throwError $ "fromValue: Expected bool, got " ++ showValue v

instance FromNix Text where
    fromValue = \case
        NVStr s _ -> pure s
        v -> throwError $ "fromValue: Expected string, got " ++ showValue v

instance FromNix Int where
    fromValue = fmap fromInteger . fromValue

instance FromNix Integer where
    fromValue = \case
        NVConstant (NInt n) -> pure n
        v -> throwError $ "fromValue: Expected number, got " ++ showValue v

instance FromNix a => FromNix [a] where
    fromValue = \case
        NVList l -> traverse (`force` fromValue) l
        v -> throwError $ "fromValue: Expected list, got " ++ showValue v

toEncodingSorted :: A.Value -> A.Encoding
toEncodingSorted = \case
    A.Object m -> A.pairs $ mconcat $ fmap (\(k, v) -> A.pair k $ toEncodingSorted v) $ sortOn fst $ M.toList m
    A.Array l -> A.list toEncodingSorted $ V.toList l
    v -> A.toEncoding v

instance FromNix A.Value where
    fromValue = \case
        NVConstant a -> pure $ case a of
            NInt n -> toJSON n
            NFloat n -> toJSON n
            NBool b -> toJSON b
            NNull -> A.Null
            NUri u -> toJSON u
        NVStr s _ -> pure $ toJSON s
        NVList l -> A.Array . V.fromList <$> traverse (`force` fromValue) l
        NVSet m _ -> A.Object <$> traverse (`force` fromValue) m
        NVClosure {} -> throwError "cannot convert a function to JSON"
        NVLiteralPath p -> toJSON . unStorePath <$> addPath p
        NVEnvPath p -> toJSON . unStorePath <$> addPath p
        NVBuiltin _ _ -> throwError "cannot convert a built-in function to JSON"
