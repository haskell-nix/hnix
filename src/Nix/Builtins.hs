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
{-# LANGUAGE TupleSections #-}
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
import           Data.Foldable (foldrM)
import           Data.HashMap.Lazy (HashMap)
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
import           Language.Haskell.TH.Syntax (addDependentFile, runIO)
import           Nix.Atoms
import           Nix.Convert
import           Nix.Effects
import           Nix.Eval
import           Nix.Exec
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
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

force' :: forall e m. MonadBuiltins e m => NThunk m -> m (NValue m)
force' = force ?? pure

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

    mkThunk n = thunk
        . withStringContext ("While calling builtin " ++ Text.unpack n ++ "\n")

    add0 t n v = wrap t n <$> mkThunk n v
    add  t n v = wrap t n <$> mkThunk n (builtin  (Text.unpack n) v)
    add2 t n v = wrap t n <$> mkThunk n (builtin2 (Text.unpack n) v)
    add3 t n v = wrap t n <$> mkThunk n (builtin3 (Text.unpack n) v)

    add' :: ToBuiltin m a => BuiltinType -> Text -> a -> m (Builtin m)
    add' t n v = wrap t n <$> mkThunk n (toBuiltin (Text.unpack n) v)

-- Primops

foldNixPath :: forall e m r. MonadBuiltins e m
            => (FilePath -> Maybe String -> r -> m r) -> r -> m r
foldNixPath f z = do
    mres <- lookupVar @_ @(NThunk m) "__includes"
    dirs <- case mres of
        Nothing -> return []
        Just v  -> fromNix @[Text] v
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
nixPath = fmap NVList $ flip foldNixPath [] $ \p mn rest ->
    pure $ valueThunk
        (flip NVSet mempty $ M.fromList
            [ ("path",   valueThunk $ NVPath p)
            , ("prefix", valueThunk $
                   NVStr (Text.pack (fromMaybe "" mn)) mempty) ]) : rest

toString :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
toString str =
    str >>= normalForm >>= valueText False >>= toNix @(Text, DList Text)

hasAttr :: MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
hasAttr x y = x >>= \x' -> y >>= \y' -> case (x', y') of
    (NVStr key _, NVSet aset _) ->
        return . NVConstant . NBool $ M.member key aset
    (x, y) -> throwError $ "Invalid types for builtin.hasAttr: "
                 ++ show (x, y)

getAttr :: MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
getAttr x y = x >>= \x' -> y >>= \y' -> case (x', y') of
    (NVStr key _, NVSet aset _) -> case M.lookup key aset of
        Nothing -> throwError $ "getAttr: field does not exist: "
                      ++ Text.unpack key
        Just action -> force' action
    (x, y) -> throwError $ "Invalid types for builtin.getAttr: "
                 ++ show (x, y)

unsafeGetAttrPos :: forall e m. MonadBuiltins e m
                 => m (NValue m) -> m (NValue m) -> m (NValue m)
unsafeGetAttrPos x y = x >>= \x' -> y >>= \y' -> case (x', y') of
    (NVStr key _, NVSet _ apos) -> case M.lookup key apos of
        Nothing ->
            throwError $ "unsafeGetAttrPos: field '" ++ Text.unpack key
                ++ "' does not exist in attr set: " ++ show apos
        Just delta -> toValue delta
    (x, y) -> throwError $ "Invalid types for builtin.unsafeGetAttrPos: "
                 ++ show (x, y)

-- This function is a bit special in that it doesn't care about the contents
-- of the list.
length_ :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
length_ = toValue . (length :: [NThunk m] -> Int) <=< fromValue

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = return False
anyM p (x:xs)   = do
        q <- p x
        if q then return True
             else anyM p xs

any_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
any_ fun xs = fun >>= \f ->
    toNix <=< anyM fromNix <=< mapM ((f `callFunc`) . force')
          <=< fromValue $ xs

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x:xs)   = do
        q <- p x
        if q then allM p xs
             else return False

all_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
all_ fun xs = fun >>= \f ->
    toNix <=< allM fromNix <=< mapM ((f `callFunc`) . force')
          <=< fromValue $ xs

foldl'_ :: forall e m. MonadBuiltins e m
        => m (NValue m) -> m (NValue m) -> m (NValue m) -> m (NValue m)
foldl'_ fun z xs =
    fun >>= \f -> fromValue @[NThunk m] xs >>= foldl' (go f) z
  where
    go f b a = b >>= \b' ->
        f `callFunc` pure b' >>= (`callFunc` force' a)

head_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
head_ = fromValue >=> \case
    [] -> throwError "builtins.head: empty list"
    h:_ -> force' h

tail_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
tail_ = fromValue >=> \case
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

splitVersion_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
splitVersion_ = fromNix >=> \s -> do
    let vals = flip map (splitVersion s) $ \c ->
            valueThunk $ NVStr (versionComponentToString c) mempty
    return $ NVList vals

compareVersions :: Text -> Text -> Ordering
compareVersions s1 s2 =
    mconcat $ alignWith f (splitVersion s1) (splitVersion s2)
  where
    z = VersionComponent_String ""
    f = uncurry compare . fromThese z z

compareVersions_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
compareVersions_ t1 t2 =
    fromNix t1 >>= \s1 ->
    fromNix t2 >>= \s2 ->
        return $ NVConstant $ NInt $ case compareVersions s1 s2 of
            LT -> -1
            EQ -> 0
            GT -> 1

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

parseDrvName :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
parseDrvName = fromValue >=> \(s :: Text) -> do
    let (name :: Text, version :: Text) = splitDrvName s
    -- jww (2018-04-15): There should be an easier way to write this.
    (toValue =<<) $ sequence $ M.fromList
        [ ("name" :: Text, thunk (toValue name))
        , ("version",     thunk (toValue version)) ]

match_ :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
match_ pat str =
    fromNix pat >>= \p ->
    fromNix str >>= \s -> do
        -- jww (2018-04-05): We should create a fundamental type for compiled
        -- regular expressions if it turns out they get used often.
        let re = makeRegex (encodeUtf8 p) :: Regex
        case matchOnceText re (encodeUtf8 s) of
            Just ("", sarr, "") -> do
                let s = map fst (elems sarr)
                NVList <$> traverse (toValue . decodeUtf8)
                    (if length s > 1 then tail s else s)
            _ -> pure $ NVConstant NNull

split_ :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
split_ pat str =
    fromNix pat >>= \p ->
    fromNix str >>= \s -> do
        let re = makeRegex (encodeUtf8 p) :: Regex
            haystack = encodeUtf8 s
        return $ NVList $
            splitMatches 0 (map elems $ matchAllText re haystack) haystack

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

attrNames :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
attrNames = fromValue @(ValueSet m) >=> toNix . sort . M.keys

attrValues :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
attrValues = fromValue @(ValueSet m) >=>
    toValue . fmap snd . sortOn (fst @Text @(NThunk m)) . M.toList

map_ :: forall e m. MonadBuiltins e m
     => m (NValue m) -> m (NValue m) -> m (NValue m)
map_ fun xs = fun >>= \f ->
    toNix <=< traverse (thunk . withStringContext "While applying f in map:\n"
                              . (f `callFunc`) . force')
          <=< fromValue @[NThunk m] $ xs

filter_ :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
filter_ fun xs = fun >>= \f ->
    toNix <=< filterM (fromNix <=< callFunc f . force')
          <=< fromValue @[NThunk m] $ xs

catAttrs :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
catAttrs attrName xs =
    fromNix @Text attrName >>= \n ->
    fromValue @[NThunk m] xs >>= \l ->
        fmap (NVList . catMaybes) $
            forM l $ fmap (M.lookup n) . fromValue

baseNameOf :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
baseNameOf x = x >>= \case
    --TODO: Only allow strings that represent absolute paths
    NVStr path ctx -> pure $ NVStr (Text.pack $ takeFileName $ Text.unpack path) ctx
    NVPath path -> pure $ NVPath $ takeFileName path
    v -> throwError $ "dirOf: expected string or path, got " ++ show v

dirOf :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
dirOf x = x >>= \case
    --TODO: Only allow strings that represent absolute paths
    NVStr path ctx -> pure $ NVStr (Text.pack $ takeDirectory $ Text.unpack path) ctx
    NVPath path -> pure $ NVPath $ takeDirectory path
    v -> throwError $ "dirOf: expected string or path, got " ++ show v

unsafeDiscardStringContext :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
unsafeDiscardStringContext = fromNix @Text >=> toNix

seq_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
seq_ a b = a >> b

deepSeq :: MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
deepSeq a b = do
    -- We evaluate 'a' only for its effects, so data cycles are ignored.
    _ <- normalFormBy (forceEffects . coerce) 0 =<< a

    -- Then we evaluate the other argument to deepseq, thus this function
    -- should always produce a result (unlike applying 'deepseq' on infinitely
    -- recursive data structures in Haskell).
    b

elem_ :: forall e m. MonadBuiltins e m
      => m (NValue m) -> m (NValue m) -> m (NValue m)
elem_ x xs = x >>= \x' ->
    toValue <=< anyM (valueEq x' <=< force') <=< fromValue @[NThunk m] $ xs

elemAt :: [a] -> Int -> Maybe a
elemAt ls i = case drop i ls of
   [] -> Nothing
   a:_ -> Just a

elemAt_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
elemAt_ xs n = fromNix n >>= \n' -> fromValue xs >>= \xs' ->
    case elemAt xs' n' of
      Just a -> force' a
      Nothing -> throwError $ "builtins.elem: Index " ++ show n'
          ++ " too large for list of length " ++ show (length xs')

genList :: MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
genList generator = fromNix @Integer >=> \n ->
    if n >= 0
    then generator >>= \f ->
        toNix =<< forM [0 .. n - 1] (\i -> thunk $ f `callFunc` toNix i)
    else throwError $ "builtins.genList: Expected a non-negative number, got "
             ++ show n

--TODO: Preserve string context
replaceStrings :: MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m) -> m (NValue m)
replaceStrings tfrom tto ts =
    fromNix tfrom >>= \(from :: [Text]) ->
    fromNix tto   >>= \(to   :: [Text]) ->
    fromNix ts    >>= \(s    :: Text) -> do
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

removeAttrs :: forall e m. MonadBuiltins e m
            => m (NValue m) -> m (NValue m) -> m (NValue m)
removeAttrs set = fromNix >=> \(toRemove :: [Text]) ->
    fromValue @(HashMap Text (NThunk m),
                HashMap Text SourcePos) set >>= \(m, p) ->
        toNix (go m toRemove, go p toRemove)
  where
    go = foldl' (flip M.delete)

intersectAttrs :: forall e m. MonadBuiltins e m
               => m (NValue m) -> m (NValue m) -> m (NValue m)
intersectAttrs set1 set2 =
    fromValue @(HashMap Text (NThunk m),
                HashMap Text SourcePos) set1 >>= \(s1, p1) ->
    fromValue @(HashMap Text (NThunk m),
                HashMap Text SourcePos) set2 >>= \(s2, p2) ->
        return $ NVSet (s2 `M.intersection` s1) (p2 `M.intersection` p1)

functionArgs :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
functionArgs fun = fun >>= \case
    NVClosure p _ ->
        -- jww (2018-04-05): Should we preserve the location where the
        -- function arguments were declared for __unsafeGetAttrPos?
        toValue @(HashMap Text (NThunk m)) $
            valueThunk . NVConstant . NBool <$>
                case p of
                    Param name -> M.singleton name False
                    ParamSet s _ _ -> isJust <$> M.fromList s
    v -> throwError $ "builtins.functionArgs: expected function, got "
            ++ show v

toPath :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
toPath = fromNix @Path >=> toNix @Path

pathExists_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
pathExists_ path = path >>= \case
    NVPath p  -> toNix =<< pathExists p
    -- jww (2018-04-13): Should this ever be a string?
    NVStr s _ -> toNix =<< pathExists (Text.unpack s)
    v -> throwError $ "builtins.pathExists: expected path, got " ++ show v

hasKind :: forall a e m. (MonadBuiltins e m, FromNix a m (NValue m))
        => m (NValue m) -> m (NValue m)
hasKind = fromNixMay >=> toNix . \case Just (_ :: a) -> True; _ -> False

isAttrs :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
isAttrs = hasKind @(ValueSet m)

isList :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
isList = hasKind @[NThunk m]

isString :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
isString = hasKind @Text

isInt :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
isInt = hasKind @Int

isFloat :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
isFloat = hasKind @Float

isBool :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
isBool = hasKind @Bool

isNull :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
isNull = hasKind @()

isFunction :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
isFunction func = func >>= \case
    NVClosure {} -> toValue True
    _ -> toValue False

throw_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
throw_ = fromNix >=> throwError . Text.unpack

import_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
import_ = fromNix >=> importPath M.empty . getPath

scopedImport :: forall e m. MonadBuiltins e m
             => m (NValue m) -> m (NValue m) -> m (NValue m)
scopedImport aset path =
    fromValue aset >>= \s ->
    fromNix   path >>= \p -> importPath @m s (getPath p)

getEnv_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
getEnv_ = fromNix >=> \s -> do
    mres <- getEnvVar (Text.unpack s)
    toNix $ case mres of
        Nothing -> ""
        Just v  -> Text.pack v

sort_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
sort_ comparator xs = comparator >>= \comp ->
    fromValue xs >>= sortByM (cmp comp) >>= toValue
  where
    cmp f a b = do
        isLessThan <- f `callFunc` force' a >>= (`callFunc` force' b)
        fromValue isLessThan >>= \case
            True -> pure LT
            False -> do
                isGreaterThan <- f `callFunc` force' b >>= (`callFunc` force' a)
                fromValue isGreaterThan <&> \case
                    True  -> GT
                    False -> EQ

lessThan :: MonadBuiltins e m => m (NValue m) -> m (NValue m) -> m (NValue m)
lessThan ta tb = ta >>= \va -> tb >>= \vb -> do
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

concatLists :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
concatLists = fromValue @[NThunk m]
    >=> mapM (fromValue @[NThunk m] >=> pure)
    >=> toValue . concat

listToAttrs :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
listToAttrs = fromValue @[NThunk m] >=> \l ->
    fmap (flip NVSet M.empty . M.fromList . reverse) $
        forM l $ fromValue @(HashMap Text (NThunk m)) >=> \s ->
            case (M.lookup "name" s, M.lookup "value" s) of
                (Just name, Just value) -> fromNix name <&> (, value)
                _ -> throwError $
                    "builtins.listToAttrs: expected set with name and value, got"
                        ++ show s

hashString :: MonadBuiltins e m => Text -> Text -> Prim m Text
hashString algo s = Prim $ do
    hash <- case algo of
        "md5"    -> pure MD5.hash
        "sha1"   -> pure SHA1.hash
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
readFile_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
readFile_ path =
    path >>= absolutePathFromValue >>= Nix.Stack.readFile >>= toNix

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

readDir_ :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
readDir_ pathThunk = do
    path  <- absolutePathFromValue =<< pathThunk
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

fromJSON :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
fromJSON = fromValue >=> \encoded ->
    case A.eitherDecodeStrict' @A.Value $ encodeUtf8 encoded of
        Left jsonError -> throwError $ "builtins.fromJSON: " ++ jsonError
        Right v -> toValue v

toXML_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
toXML_ v = v >>= normalForm >>= \x ->
    pure $ NVStr (Text.pack (toXML x)) mempty

typeOf :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
typeOf v = v >>= toNix @Text . \case
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

tryEval :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
tryEval e = catch (onSuccess <$> e) (pure . onError)
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

fetchTarball :: forall e m. MonadBuiltins e m => m (NValue m) -> m (NValue m)
fetchTarball v = v >>= \case
    NVSet s _ -> case M.lookup "url" s of
        Nothing -> throwError "builtins.fetchTarball: Missing url attribute"
        Just url -> force url $ go (M.lookup "sha256" s)
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
    fetch url (Just m) = fromNix m >>= \sha ->
        nixInstantiateExpr $ "builtins.fetchTarball { "
          ++ "url    = \"" ++ Text.unpack url ++ "\"; "
          ++ "sha256 = \"" ++ Text.unpack sha ++ "\"; }"

partition_ :: forall e m. MonadBuiltins e m
           => m (NValue m) -> m (NValue m) -> m (NValue m)
partition_ fun xs = fun >>= \f ->
    fromValue @[NThunk m] xs >>= \l -> do
        let match t = f `callFunc` force' t >>= fmap (, t) . fromNix
        selection <- traverse match l
        let (right, wrong) = partition fst selection
        let makeSide = valueThunk . NVList . map snd
        toValue @(HashMap Text (NThunk m)) $
            M.fromList [("right", makeSide right), ("wrong", makeSide wrong)]

currentSystem :: MonadBuiltins e m => m (NValue m)
currentSystem = do
  os <- getCurrentSystemOS
  arch <- getCurrentSystemArch
  return $ NVStr (arch <> "-" <> os) mempty

derivationStrict_ :: MonadBuiltins e m => m (NValue m) -> m (NValue m)
derivationStrict_ = (>>= derivationStrict)

newtype Prim m a = Prim { runPrim :: m a }

-- | Types that support conversion to nix in a particular monad
class ToBuiltin m a | a -> m where
    toBuiltin :: String -> a -> m (NValue m)

instance (MonadBuiltins e m, ToNix a m (NValue m)) => ToBuiltin m (Prim m a) where
    toBuiltin _ p = toNix =<< runPrim p

instance (MonadBuiltins e m, FromNix a m (NValue m), ToBuiltin m b)
      => ToBuiltin m (a -> b) where
    toBuiltin name f = return $ NVBuiltin name (fromNix >=> toBuiltin name . f)
