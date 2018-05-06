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

module Nix.Builtins (builtins) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.ListM (sortByM)
import           Control.Monad.Reader (asks)
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
import           Data.Fix
import           Data.Foldable (foldrM)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as HS
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import           Data.These (fromThese)
import qualified Data.Time.Clock.POSIX as Time
import           Data.Traversable (mapM)
import           Language.Haskell.TH.Syntax (addDependentFile, runIO)
import           Nix.Atoms
import           Nix.Convert
import           Nix.Effects
import qualified Nix.Eval as Eval
import           Nix.Exec
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.Normal
import           Nix.Options
import           Nix.Parser
import           Nix.Render
import           Nix.Scope
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value
import           Nix.XML
import           System.FilePath
import           System.Posix.Files
import           Text.Regex.TDFA

builtins :: (MonadNix e m, Scoped e (NThunk m) m)
         => m (Scopes m (NThunk m))
builtins = do
    ref <- thunk $ flip nvSet M.empty <$> buildMap
    lst <- ([("builtins", ref)] ++) <$> topLevelBuiltins
    pushScope (M.fromList lst) currentScopes
  where
    buildMap = M.fromList . map mapping <$> builtinsList
    topLevelBuiltins = map mapping <$> fullBuiltinsList

    fullBuiltinsList = map go <$> builtinsList
      where
        go b@(Builtin TopLevel _) = b
        go (Builtin Normal (name, builtin)) =
            Builtin TopLevel ("__" <> name, builtin)

data BuiltinType = Normal | TopLevel
data Builtin m = Builtin
    { _kind   :: BuiltinType
    , mapping :: (Text, NThunk m)
    }

valueThunk :: forall e m. MonadNix e m => NValue m -> NThunk m
valueThunk = value @_ @_ @m

force' :: forall e m. MonadNix e m => NThunk m -> m (NValue m)
force' = force ?? pure

builtinsList :: forall e m. MonadNix e m => m [ Builtin m ]
builtinsList = sequence [
      do version <- toValue ("2.0" :: Text)
         pure $ Builtin Normal ("nixVersion", version)

    , do version <- toValue (5 :: Int)
         pure $ Builtin Normal ("langVersion", version)

    , add0 Normal   "nixPath"                    nixPath
    , add  TopLevel "abort"                      throw_ -- for now
    , add2 Normal   "add"                        add_
    , add2 Normal   "all"                        all_
    , add2 Normal   "any"                        any_
    , add  Normal   "attrNames"                  attrNames
    , add  Normal   "attrValues"                 attrValues
    , add  TopLevel "baseNameOf"                 baseNameOf
    , add2 Normal   "catAttrs"                   catAttrs
    , add2 Normal   "compareVersions"            compareVersions_
    , add  Normal   "concatLists"                concatLists
    , add' Normal   "concatStringsSep"           (arity2 Text.intercalate)
    , add0 Normal   "currentSystem"              currentSystem
    , add0 Normal   "currentTime"                currentTime_
    , add2 Normal   "deepSeq"                    deepSeq
    , add0 TopLevel "derivation"                 $(do
          let f = "data/nix/corepkgs/derivation.nix"
          addDependentFile f
          Success expr <- runIO $ parseNixFile f
          [| cata Eval.eval expr |]
      )
    , add  TopLevel "derivationStrict"           derivationStrict_
    , add  TopLevel "dirOf"                      dirOf
    , add2 Normal   "div"                        div_
    , add2 Normal   "elem"                       elem_
    , add2 Normal   "elemAt"                     elemAt_
    , add  Normal   "exec"                       exec_
    , add0 Normal   "false"                      (return $ nvConstant $ NBool False)
    , add  Normal   "fetchTarball"               fetchTarball
    , add  Normal   "fetchurl"                   fetchurl
    , add2 Normal   "filter"                     filter_
    , add3 Normal   "foldl'"                     foldl'_
    , add  Normal   "fromJSON"                   fromJSON
    , add  Normal   "functionArgs"               functionArgs
    , add2 Normal   "genList"                    genList
    , add  Normal   "genericClosure"             genericClosure
    , add2 Normal   "getAttr"                    getAttr
    , add  Normal   "getEnv"                     getEnv_
    , add2 Normal   "hasAttr"                    hasAttr
    , add  Normal   "hasContext"                 hasContext
    , add' Normal   "hashString"                 hashString
    , add  Normal   "head"                       head_
    , add  TopLevel "import"                     import_
    , add2 Normal   "intersectAttrs"             intersectAttrs
    , add  Normal   "isAttrs"                    isAttrs
    , add  Normal   "isBool"                     isBool
    , add  Normal   "isFloat"                    isFloat
    , add  Normal   "isFunction"                 isFunction
    , add  Normal   "isInt"                      isInt
    , add  Normal   "isList"                     isList
    , add  TopLevel "isNull"                     isNull
    , add  Normal   "isString"                   isString
    , add  Normal   "length"                     length_
    , add2 Normal   "lessThan"                   lessThan
    , add  Normal   "listToAttrs"                listToAttrs
    , add2 TopLevel "map"                        map_
    , add2 Normal   "match"                      match_
    , add2 Normal   "mul"                        mul_
    , add0 Normal   "null"                       (return $ nvConstant NNull)
    , add  Normal   "parseDrvName"               parseDrvName
    , add2 Normal   "partition"                  partition_
    , add  Normal   "pathExists"                 pathExists_
    , add  TopLevel "placeholder"                placeHolder
    , add  Normal   "readDir"                    readDir_
    , add  Normal   "readFile"                   readFile_
    , add2 TopLevel "removeAttrs"                removeAttrs
    , add3 Normal   "replaceStrings"             replaceStrings
    , add2 TopLevel "scopedImport"               scopedImport
    , add2 Normal   "seq"                        seq_
    , add2 Normal   "sort"                       sort_
    , add2 Normal   "split"                      split_
    , add  Normal   "splitVersion"               splitVersion_
    , add0 Normal   "storeDir"                   (return $ nvPath "/nix/store")
    , add' Normal   "stringLength"               (arity1 Text.length)
    , add' Normal   "sub"                        (arity2 ((-) @Integer))
    , add' Normal   "substring"                  substring
    , add  Normal   "tail"                       tail_
    , add0 Normal   "true"                       (return $ nvConstant $ NBool True)
    , add  TopLevel "throw"                      throw_
    , add' Normal   "toJSON"
      (arity1 $ decodeUtf8 . LBS.toStrict . A.encodingToLazyByteString
                           . toEncodingSorted)
    , add  Normal   "toPath"                     toPath
    , add  TopLevel "toString"                   toString
    , add  Normal   "toXML"                      toXML_
    , add2 TopLevel "trace"                      trace_
    , add  Normal   "tryEval"                    tryEval
    , add  Normal   "typeOf"                     typeOf
    , add  Normal   "unsafeDiscardStringContext" unsafeDiscardStringContext
    , add2 Normal   "unsafeGetAttrPos"           unsafeGetAttrPos
    , add  Normal   "valueSize"                  getRecursiveSize
  ]
  where
    wrap t n f = Builtin t (n, f)

    arity1 f = Prim . pure . f
    arity2 f = ((Prim . pure) .) . f

    mkThunk n = thunk . withFrame Info
        (ErrorCall $ "While calling builtin " ++ Text.unpack n ++ "\n")

    add0 t n v = wrap t n <$> mkThunk n v
    add  t n v = wrap t n <$> mkThunk n (builtin  (Text.unpack n) v)
    add2 t n v = wrap t n <$> mkThunk n (builtin2 (Text.unpack n) v)
    add3 t n v = wrap t n <$> mkThunk n (builtin3 (Text.unpack n) v)

    add' :: ToBuiltin m a => BuiltinType -> Text -> a -> m (Builtin m)
    add' t n v = wrap t n <$> mkThunk n (toBuiltin (Text.unpack n) v)

-- Primops

foldNixPath :: forall e m r. MonadNix e m
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
        _ -> throwError $ ErrorCall $ "Unexpected entry in NIX_PATH: " ++ show x

nixPath :: MonadNix e m => m (NValue m)
nixPath = fmap nvList $ flip foldNixPath [] $ \p mn rest ->
    pure $ valueThunk
        (flip nvSet mempty $ M.fromList
            [ ("path",   valueThunk $ nvPath p)
            , ("prefix", valueThunk $
                   nvStr (Text.pack (fromMaybe "" mn)) mempty) ]) : rest

toString :: MonadNix e m => m (NValue m) -> m (NValue m)
toString str =
    str >>= normalForm >>= valueText False >>= toNix @NixString

hasAttr :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
hasAttr x y =
    fromValue @Text x >>= \key ->
    fromValue @(AttrSet (NThunk m), AttrSet SourcePos) y >>= \(aset, _) ->
        toNix $ M.member key aset

attrsetGet :: MonadNix e m => Text -> AttrSet t -> m t
attrsetGet k s = case M.lookup k s of
    Just v -> pure v
    Nothing ->
        throwError $ ErrorCall $ "Attribute '" ++ Text.unpack k ++ "' required"

hasContext :: MonadNix e m => m (NValue m) -> m (NValue m)
hasContext =
    toNix . not . HS.null . nsContext <=< fromValue

getAttr :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
getAttr x y = x >>= \x' -> y >>= \y' -> case (x', y') of
    (NVStr key _, NVSet aset _) -> attrsetGet key aset >>= force'
    (x, y) -> throwError $ ErrorCall $ "Invalid types for builtin.getAttr: "
                 ++ show (x, y)

unsafeGetAttrPos :: forall e m. MonadNix e m
                 => m (NValue m) -> m (NValue m) -> m (NValue m)
unsafeGetAttrPos x y = x >>= \x' -> y >>= \y' -> case (x', y') of
    (NVStr key _, NVSet _ apos) -> case M.lookup key apos of
        Nothing -> pure $ nvConstant NNull
        Just delta -> toValue delta
    (x, y) -> throwError $ ErrorCall $ "Invalid types for builtin.unsafeGetAttrPos: "
                 ++ show (x, y)

-- This function is a bit special in that it doesn't care about the contents
-- of the list.
length_ :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
length_ = toValue . (length :: [NThunk m] -> Int) <=< fromValue

add_ :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
add_ x y = x >>= \x' -> y >>= \y' -> case (x', y') of
    (NVConstant (NInt x),   NVConstant (NInt y))   ->
        toNix ( x + y :: Integer)
    (NVConstant (NFloat x), NVConstant (NInt y))   -> toNix (x + fromInteger y)
    (NVConstant (NInt x),   NVConstant (NFloat y)) -> toNix (fromInteger x + y)
    (NVConstant (NFloat x), NVConstant (NFloat y)) -> toNix (x + y)
    (_, _) ->
        throwError $ Addition x' y'

mul_ :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
mul_ x y = x >>= \x' -> y >>= \y' -> case (x', y') of
    (NVConstant (NInt x),   NVConstant (NInt y))   ->
        toNix ( x * y :: Integer)
    (NVConstant (NFloat x), NVConstant (NInt y))   -> toNix (x * fromInteger y)
    (NVConstant (NInt x),   NVConstant (NFloat y)) -> toNix (fromInteger x * y)
    (NVConstant (NFloat x), NVConstant (NFloat y)) -> toNix (x * y)
    (_, _) ->
        throwError $ Multiplication x' y'

div_ :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
div_ x y = x >>= \x' -> y >>= \y' -> case (x', y') of
    (NVConstant (NInt x),   NVConstant (NInt y))   ->
        toNix (floor (fromInteger x / fromInteger y :: Double) :: Integer)
    (NVConstant (NFloat x), NVConstant (NInt y))   -> toNix (x / fromInteger y)
    (NVConstant (NInt x),   NVConstant (NFloat y)) -> toNix (fromInteger x / y)
    (NVConstant (NFloat x), NVConstant (NFloat y)) -> toNix (x / y)
    (_, _) ->
        throwError $ Division x' y'

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = return False
anyM p (x:xs)   = do
        q <- p x
        if q then return True
             else anyM p xs

any_ :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
any_ fun xs = fun >>= \f ->
    toNix <=< anyM fromValue <=< mapM ((f `callFunc`) . force')
          <=< fromValue $ xs

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x:xs)   = do
        q <- p x
        if q then allM p xs
             else return False

all_ :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
all_ fun xs = fun >>= \f ->
    toNix <=< allM fromValue <=< mapM ((f `callFunc`) . force')
          <=< fromValue $ xs

foldl'_ :: forall e m. MonadNix e m
        => m (NValue m) -> m (NValue m) -> m (NValue m) -> m (NValue m)
foldl'_ fun z xs =
    fun >>= \f -> fromValue @[NThunk m] xs >>= foldl' (go f) z
  where
    go f b a = f `callFunc` b >>= (`callFunc` force' a)

head_ :: MonadNix e m => m (NValue m) -> m (NValue m)
head_ = fromValue >=> \case
    [] -> throwError $ ErrorCall "builtins.head: empty list"
    h:_ -> force' h

tail_ :: MonadNix e m => m (NValue m) -> m (NValue m)
tail_ = fromValue >=> \case
    [] -> throwError $ ErrorCall "builtins.tail: empty list"
    _:t -> return $ nvList t

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

splitVersion_ :: MonadNix e m => m (NValue m) -> m (NValue m)
splitVersion_ = fromValue >=> \s -> do
    let vals = flip map (splitVersion s) $ \c ->
            valueThunk $ nvStr (versionComponentToString c) mempty
    return $ nvList vals

compareVersions :: Text -> Text -> Ordering
compareVersions s1 s2 =
    mconcat $ alignWith f (splitVersion s1) (splitVersion s2)
  where
    z = VersionComponent_String ""
    f = uncurry compare . fromThese z z

compareVersions_ :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
compareVersions_ t1 t2 =
    fromValue t1 >>= \s1 ->
    fromValue t2 >>= \s2 ->
        return $ nvConstant $ NInt $ case compareVersions s1 s2 of
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

parseDrvName :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
parseDrvName = fromValue >=> \(s :: Text) -> do
    let (name :: Text, version :: Text) = splitDrvName s
    -- jww (2018-04-15): There should be an easier way to write this.
    (toValue =<<) $ sequence $ M.fromList
        [ ("name" :: Text, thunk (toValue @_ @_ @(NValue m) name))
        , ("version",     thunk (toValue version)) ]

match_ :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
match_ pat str =
    fromValue pat >>= \p ->
    fromValue str >>= \s -> do
        let re = makeRegex (encodeUtf8 p) :: Regex
        case matchOnceText re (encodeUtf8 s) of
            Just ("", sarr, "") -> do
                let s = map fst (elems sarr)
                nvList <$> traverse (toValue . decodeUtf8)
                    (if length s > 1 then tail s else s)
            _ -> pure $ nvConstant NNull

split_ :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
split_ pat str =
    fromValue pat >>= \p ->
    fromValue str >>= \s -> do
        let re = makeRegex (encodeUtf8 p) :: Regex
            haystack = encodeUtf8 s
        return $ nvList $
            splitMatches 0 (map elems $ matchAllText re haystack) haystack

splitMatches
  :: forall e m. MonadNix e m
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
    caps = valueThunk $ nvList (map f captures)
    f (a,(s,_)) = if s < 0 then valueThunk (nvConstant NNull) else thunkStr a

thunkStr s = valueThunk (nvStr (decodeUtf8 s) mempty)

substring :: MonadNix e m => Int -> Int -> Text -> Prim m Text
substring start len str = Prim $
    if start < 0 --NOTE: negative values of 'len' are OK
    then throwError $ ErrorCall $ "builtins.substring: negative start position: " ++ show start
    else pure $ Text.take len $ Text.drop start str

attrNames :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
attrNames = fromValue @(ValueSet m) >=> toNix . sort . M.keys

attrValues :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
attrValues = fromValue @(ValueSet m) >=>
    toValue . fmap snd . sortOn (fst @Text @(NThunk m)) . M.toList

map_ :: forall e m. MonadNix e m
     => m (NValue m) -> m (NValue m) -> m (NValue m)
map_ fun xs = fun >>= \f ->
    toNix <=< traverse (thunk . withFrame Debug
                                    (ErrorCall "While applying f in map:\n")
                              . (f `callFunc`) . force')
          <=< fromValue @[NThunk m] $ xs

filter_ :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
filter_ fun xs = fun >>= \f ->
    toNix <=< filterM (fromValue <=< callFunc f . force')
          <=< fromValue @[NThunk m] $ xs

catAttrs :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
catAttrs attrName xs =
    fromValue @Text attrName >>= \n ->
    fromValue @[NThunk m] xs >>= \l ->
        fmap (nvList . catMaybes) $
            forM l $ fmap (M.lookup n) . fromValue

baseNameOf :: MonadNix e m => m (NValue m) -> m (NValue m)
baseNameOf x = x >>= \case
    NVStr path ctx -> pure $ nvStr (Text.pack $ takeFileName $ Text.unpack path) ctx
    NVPath path -> pure $ nvPath $ takeFileName path
    v -> throwError $ ErrorCall $ "dirOf: expected string or path, got " ++ show v

dirOf :: MonadNix e m => m (NValue m) -> m (NValue m)
dirOf x = x >>= \case
    NVStr path ctx -> pure $ nvStr (Text.pack $ takeDirectory $ Text.unpack path) ctx
    NVPath path -> pure $ nvPath $ takeDirectory path
    v -> throwError $ ErrorCall $ "dirOf: expected string or path, got " ++ show v

-- jww (2018-04-28): This should only be a string argument, and not coerced?
unsafeDiscardStringContext :: MonadNix e m => m (NValue m) -> m (NValue m)
unsafeDiscardStringContext = fromValue @Text >=> toNix

seq_ :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
seq_ a b = a >> b

deepSeq :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
deepSeq a b = do
    -- We evaluate 'a' only for its effects, so data cycles are ignored.
    _ <- normalFormBy (forceEffects . coerce . _baseThunk) 0 =<< a

    -- Then we evaluate the other argument to deepseq, thus this function
    -- should always produce a result (unlike applying 'deepseq' on infinitely
    -- recursive data structures in Haskell).
    b

elem_ :: forall e m. MonadNix e m
      => m (NValue m) -> m (NValue m) -> m (NValue m)
elem_ x xs = x >>= \x' ->
    toValue <=< anyM (valueEq x' <=< force') <=< fromValue @[NThunk m] $ xs

elemAt :: [a] -> Int -> Maybe a
elemAt ls i = case drop i ls of
   [] -> Nothing
   a:_ -> Just a

elemAt_ :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
elemAt_ xs n = fromValue n >>= \n' -> fromValue xs >>= \xs' ->
    case elemAt xs' n' of
      Just a -> force' a
      Nothing -> throwError $ ErrorCall $ "builtins.elem: Index " ++ show n'
          ++ " too large for list of length " ++ show (length xs')

genList :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
genList generator = fromValue @Integer >=> \n ->
    if n >= 0
    then generator >>= \f ->
        toNix =<< forM [0 .. n - 1] (\i -> thunk $ f `callFunc` toNix i)
    else throwError $ ErrorCall $ "builtins.genList: Expected a non-negative number, got "
             ++ show n

genericClosure :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
genericClosure = fromValue @(AttrSet (NThunk m)) >=> \s ->
    case (M.lookup "startSet" s, M.lookup "operator" s) of
      (Nothing, Nothing) ->
          throwError $ ErrorCall $
              "builtins.genericClosure: "
                  ++ "Attributes 'startSet' and 'operator' required"
      (Nothing, Just _) ->
          throwError $ ErrorCall $
              "builtins.genericClosure: Attribute 'startSet' required"
      (Just _, Nothing) ->
          throwError $ ErrorCall $
              "builtins.genericClosure: Attribute 'operator' required"
      (Just startSet, Just operator) ->
          fromValue @[NThunk m] startSet >>= \ss ->
          force operator $ \op ->
              toValue @[NThunk m] =<< snd <$> go op ss S.empty
  where
    go :: NValue m -> [NThunk m] -> Set (NValue m)
       -> m (Set (NValue m), [NThunk m])
    go _ [] ks = pure (ks, [])
    go op (t:ts) ks =
        force t $ \v -> fromValue @(AttrSet (NThunk m)) t >>= \s ->
            case M.lookup "key" s of
                Nothing ->
                    throwError $ ErrorCall $
                        "builtins.genericClosure: Attribute 'key' required"
                Just k -> force k $ \k' ->
                    if S.member k' ks
                        then go op ts ks
                        else do
                            ys <- fromValue @[NThunk m] =<< (op `callFunc` pure v)
                            case S.toList ks of
                                []  -> checkComparable k' k'
                                j:_ -> checkComparable k' j
                            fmap (t:) <$> go op (ts ++ ys) (S.insert k' ks)

replaceStrings :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m) -> m (NValue m)
replaceStrings tfrom tto ts =
    fromNix tfrom >>= \(from :: [Text]) ->
    fromNix tto   >>= \(to   :: [Text]) ->
    fromValue ts  >>= \(s    :: Text) -> do
        when (length from /= length to) $
            throwError $ ErrorCall $
                "'from' and 'to' arguments to 'replaceStrings'"
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

removeAttrs :: forall e m. MonadNix e m
            => m (NValue m) -> m (NValue m) -> m (NValue m)
removeAttrs set = fromNix >=> \(toRemove :: [Text]) ->
    fromValue @(AttrSet (NThunk m),
                AttrSet SourcePos) set >>= \(m, p) ->
        toNix (go m toRemove, go p toRemove)
  where
    go = foldl' (flip M.delete)

intersectAttrs :: forall e m. MonadNix e m
               => m (NValue m) -> m (NValue m) -> m (NValue m)
intersectAttrs set1 set2 =
    fromValue @(AttrSet (NThunk m),
                AttrSet SourcePos) set1 >>= \(s1, p1) ->
    fromValue @(AttrSet (NThunk m),
                AttrSet SourcePos) set2 >>= \(s2, p2) ->
        return $ nvSet (s2 `M.intersection` s1) (p2 `M.intersection` p1)

functionArgs :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
functionArgs fun = fun >>= \case
    NVClosure p _ -> toValue @(AttrSet (NThunk m)) $
        valueThunk . nvConstant . NBool <$>
            case p of
                Param name -> M.singleton name False
                ParamSet s _ _ -> isJust <$> M.fromList s
    v -> throwError $ ErrorCall $
            "builtins.functionArgs: expected function, got " ++ show v

toPath :: MonadNix e m => m (NValue m) -> m (NValue m)
toPath = fromValue @Path >=> toNix @Path

pathExists_ :: MonadNix e m => m (NValue m) -> m (NValue m)
pathExists_ path = path >>= \case
    NVPath p  -> toNix =<< pathExists p
    NVStr s _ -> toNix =<< pathExists (Text.unpack s)
    v -> throwError $ ErrorCall $
            "builtins.pathExists: expected path, got " ++ show v

hasKind :: forall a e m. (MonadNix e m, FromValue a m (NValue m))
        => m (NValue m) -> m (NValue m)
hasKind = fromValueMay >=> toNix . \case Just (_ :: a) -> True; _ -> False

isAttrs :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
isAttrs = hasKind @(ValueSet m)

isList :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
isList = hasKind @[NThunk m]

isString :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
isString = hasKind @Text

isInt :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
isInt = hasKind @Int

isFloat :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
isFloat = hasKind @Float

isBool :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
isBool = hasKind @Bool

isNull :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
isNull = hasKind @()

isFunction :: MonadNix e m => m (NValue m) -> m (NValue m)
isFunction func = func >>= \case
    NVClosure {} -> toValue True
    _ -> toValue False

throw_ :: MonadNix e m => m (NValue m) -> m (NValue m)
throw_ = fromValue >=> throwError . ErrorCall . Text.unpack

import_ :: MonadNix e m => m (NValue m) -> m (NValue m)
import_ = fromValue >=> importPath M.empty . getPath

scopedImport :: forall e m. MonadNix e m
             => m (NValue m) -> m (NValue m) -> m (NValue m)
scopedImport aset path =
    fromValue aset >>= \s ->
    fromValue   path >>= \p -> importPath @m s (getPath p)

getEnv_ :: MonadNix e m => m (NValue m) -> m (NValue m)
getEnv_ = fromValue >=> \s -> do
    mres <- getEnvVar (Text.unpack s)
    toNix $ case mres of
        Nothing -> ""
        Just v  -> Text.pack v

sort_ :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
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

lessThan :: MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
lessThan ta tb = ta >>= \va -> tb >>= \vb -> do
    let badType = throwError $ ErrorCall $
            "builtins.lessThan: expected two numbers or two strings, "
                ++ "got " ++ show va ++ " and " ++ show vb
    nvConstant . NBool <$> case (va, vb) of
        (NVConstant ca, NVConstant cb) -> case (ca, cb) of
            (NInt   a, NInt   b) -> pure $ a < b
            (NFloat a, NInt   b) -> pure $ a < fromInteger b
            (NInt   a, NFloat b) -> pure $ fromInteger a < b
            (NFloat a, NFloat b) -> pure $ a < b
            _ -> badType
        (NVStr a _, NVStr b _) -> pure $ a < b
        _ -> badType

concatLists :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
concatLists = fromValue @[NThunk m]
    >=> mapM (fromValue @[NThunk m] >=> pure)
    >=> toValue . concat

listToAttrs :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
listToAttrs = fromValue @[NThunk m] >=> \l ->
    fmap (flip nvSet M.empty . M.fromList . reverse) $
        forM l $ fromValue @(AttrSet (NThunk m)) >=> \s -> do
            name <- attrsetGet "name" s
            val  <- attrsetGet "value" s
            fromValue name <&> (, val)

hashString :: MonadNix e m => Text -> Text -> Prim m Text
hashString algo s = Prim $ do
    hash <- case algo of
        "md5"    -> pure MD5.hash
        "sha1"   -> pure SHA1.hash
        "sha256" -> pure SHA256.hash
        "sha512" -> pure SHA512.hash
        _ -> throwError $ ErrorCall $ "builtins.hashString: "
            ++ "expected \"md5\", \"sha1\", \"sha256\", or \"sha512\", got " ++ show algo
    pure $ decodeUtf8 $ Base16.encode $ hash $ encodeUtf8 s

placeHolder :: MonadNix e m => m (NValue m) -> m (NValue m)
placeHolder = fromValue @Text >=> \_ -> do
    h <- runPrim (hashString "sha256" "fdasdfas")
    toNix h

absolutePathFromValue :: MonadNix e m => NValue m -> m FilePath
absolutePathFromValue = \case
    NVStr pathText _ -> do
        let path = Text.unpack pathText
        unless (isAbsolute path) $
            throwError $ ErrorCall $ "string " ++ show path ++ " doesn't represent an absolute path"
        pure path
    NVPath path -> pure path
    v -> throwError $ ErrorCall $ "expected a path, got " ++ show v

readFile_ :: MonadNix e m => m (NValue m) -> m (NValue m)
readFile_ path =
    path >>= absolutePathFromValue >>= Nix.Render.readFile >>= toNix

data FileType
   = FileTypeRegular
   | FileTypeDirectory
   | FileTypeSymlink
   | FileTypeUnknown
   deriving (Show, Read, Eq, Ord)

instance Applicative m => ToNix FileType m (NValue m) where
    toNix = toNix . \case
        FileTypeRegular   -> "regular" :: Text
        FileTypeDirectory -> "directory"
        FileTypeSymlink   -> "symlink"
        FileTypeUnknown   -> "unknown"

readDir_ :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
readDir_ pathThunk = do
    path  <- absolutePathFromValue =<< pathThunk
    items <- listDirectory path
    itemsWithTypes <- forM items $ \item -> do
        s <- Nix.Effects.getSymbolicLinkStatus $ path </> item
        let t = if
                | isRegularFile s  -> FileTypeRegular
                | isDirectory s    -> FileTypeDirectory
                | isSymbolicLink s -> FileTypeSymlink
                | otherwise        -> FileTypeUnknown
        pure (Text.pack item, t)
    toNix (M.fromList itemsWithTypes)

fromJSON :: MonadNix e m => m (NValue m) -> m (NValue m)
fromJSON = fromValue >=> \encoded ->
    case A.eitherDecodeStrict' @A.Value $ encodeUtf8 encoded of
        Left jsonError ->
            throwError $ ErrorCall $ "builtins.fromJSON: " ++ jsonError
        Right v -> toValue v

toXML_ :: MonadNix e m => m (NValue m) -> m (NValue m)
toXML_ v = v >>= normalForm >>= \x ->
    pure $ nvStr (Text.pack (toXML x)) mempty

typeOf :: MonadNix e m => m (NValue m) -> m (NValue m)
typeOf v = v >>= toNix @Text . \case
    NVConstant a -> case a of
        NInt _   -> "int"
        NFloat _ -> "float"
        NBool _  -> "bool"
        NNull    -> "null"
        NUri _   -> "string"
    NVStr _ _     -> "string"
    NVList _      -> "list"
    NVSet _ _     -> "set"
    NVClosure {}  -> "lambda"
    NVPath _      -> "path"
    NVBuiltin _ _ -> "lambda"
    _ -> error "Pattern synonyms obscure complete patterns"

tryEval :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
tryEval e = catch (onSuccess <$> e) (pure . onError)
  where
    onSuccess v = flip nvSet M.empty $ M.fromList
        [ ("success", valueThunk (nvConstant (NBool True)))
        , ("value", valueThunk v)
        ]

    onError :: SomeException -> NValue m
    onError _ = flip nvSet M.empty $ M.fromList
        [ ("success", valueThunk (nvConstant (NBool False)))
        , ("value", valueThunk (nvConstant (NBool False)))
        ]

trace_ :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m) -> m (NValue m)
trace_ msg action = do
  traceEffect . Text.unpack =<< fromValue @Text msg
  action

exec_ :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
exec_ xs = do
  ls <- fromValue @[NThunk m] xs
  xs <- traverse (fromValue @Text . force') ls
  exec (map Text.unpack xs)

fetchTarball :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
fetchTarball v = v >>= \case
    NVSet s _ -> case M.lookup "url" s of
        Nothing -> throwError $ ErrorCall
                      "builtins.fetchTarball: Missing url attribute"
        Just url -> force url $ go (M.lookup "sha256" s)
    v@NVStr {} -> go Nothing v
    v@(NVConstant (NUri _)) -> go Nothing v
    v -> throwError $ ErrorCall $
            "builtins.fetchTarball: Expected URI or set, got " ++ show v
 where
    go :: Maybe (NThunk m) -> NValue m -> m (NValue m)
    go msha = \case
        NVStr uri _ -> fetch uri msha
        NVConstant (NUri uri) -> fetch uri msha
        v -> throwError $ ErrorCall $
                "builtins.fetchTarball: Expected URI or string, got " ++ show v

{- jww (2018-04-11): This should be written using pipes in another module
    fetch :: Text -> Maybe (NThunk m) -> m (NValue m)
    fetch uri msha = case takeExtension (Text.unpack uri) of
        ".tgz" -> undefined
        ".gz"  -> undefined
        ".bz2" -> undefined
        ".xz"  -> undefined
        ".tar" -> undefined
        ext -> throwError $ ErrorCall $ "builtins.fetchTarball: Unsupported extension '"
                  ++ ext ++ "'"
-}

    fetch :: Text -> Maybe (NThunk m) -> m (NValue m)
    fetch uri Nothing =
        nixInstantiateExpr $ "builtins.fetchTarball \"" ++
            Text.unpack uri ++ "\""
    fetch url (Just m) = fromValue m >>= \sha ->
        nixInstantiateExpr $ "builtins.fetchTarball { "
          ++ "url    = \"" ++ Text.unpack url ++ "\"; "
          ++ "sha256 = \"" ++ Text.unpack sha ++ "\"; }"

fetchurl :: forall e m. MonadNix e m => m (NValue m) -> m (NValue m)
fetchurl v = v >>= \case
    NVSet s _ -> attrsetGet "url" s >>= force ?? (go (M.lookup "sha256" s))
    v@NVStr {} -> go Nothing v
    v@(NVConstant (NUri _)) -> go Nothing v
    v -> throwError $ ErrorCall $ "builtins.fetchurl: Expected URI or set, got "
            ++ show v
 where
    go :: Maybe (NThunk m) -> NValue m -> m (NValue m)
    go _msha = \case
        NVStr uri _ -> getURL uri -- msha
        NVConstant (NUri uri) -> getURL uri -- msha
        v -> throwError $ ErrorCall $
                 "builtins.fetchurl: Expected URI or string, got " ++ show v

partition_ :: forall e m. MonadNix e m
           => m (NValue m) -> m (NValue m) -> m (NValue m)
partition_ fun xs = fun >>= \f ->
    fromValue @[NThunk m] xs >>= \l -> do
        let match t = f `callFunc` force' t >>= fmap (, t) . fromValue
        selection <- traverse match l
        let (right, wrong) = partition fst selection
        let makeSide = valueThunk . nvList . map snd
        toValue @(AttrSet (NThunk m)) $
            M.fromList [("right", makeSide right), ("wrong", makeSide wrong)]

currentSystem :: MonadNix e m => m (NValue m)
currentSystem = do
  os <- getCurrentSystemOS
  arch <- getCurrentSystemArch
  return $ nvStr (arch <> "-" <> os) mempty

currentTime_ :: MonadNix e m => m (NValue m)
currentTime_ = do
    opts :: Options <- asks (view hasLens)
    toNix @Integer $ round $ Time.utcTimeToPOSIXSeconds (currentTime opts)

derivationStrict_ :: MonadNix e m => m (NValue m) -> m (NValue m)
derivationStrict_ = (>>= derivationStrict)

newtype Prim m a = Prim { runPrim :: m a }

-- | Types that support conversion to nix in a particular monad
class ToBuiltin m a | a -> m where
    toBuiltin :: String -> a -> m (NValue m)

instance (MonadNix e m, ToNix a m (NValue m))
      => ToBuiltin m (Prim m a) where
    toBuiltin _ p = toNix =<< runPrim p

instance (MonadNix e m, FromNix a m (NValue m), ToBuiltin m b)
      => ToBuiltin m (a -> b) where
    toBuiltin name f = return $ nvBuiltin name (fromNix >=> toBuiltin name . f)
