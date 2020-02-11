{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Builtins (withNixContext, builtins) where

import           Control.Comonad
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.ListM            ( sortByM )
import           Control.Monad.Reader           ( asks )

-- Using package imports here because there is a bug in cabal2nix that demands
-- us to put the hashing package in the unconditional dependency list.
-- See https://github.com/NixOS/cabal2nix/issues/348 for more info
#if MIN_VERSION_hashing(0, 1, 0)
import "hashing" Crypto.Hash
import qualified "hashing" Crypto.Hash.MD5     as MD5
import qualified "hashing" Crypto.Hash.SHA1    as SHA1
import qualified "hashing" Crypto.Hash.SHA256  as SHA256
import qualified "hashing" Crypto.Hash.SHA512  as SHA512
#else
import qualified "cryptohash-md5" Crypto.Hash.MD5 as MD5
import qualified "cryptohash-sha1" Crypto.Hash.SHA1 as SHA1
import qualified "cryptohash-sha256" Crypto.Hash.SHA256 as SHA256
import qualified "cryptohash-sha512" Crypto.Hash.SHA512 as SHA512
#endif

import qualified Data.Aeson                    as A
import           Data.Align                     ( alignWith )
import           Data.Array
import           Data.Bits
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import           Data.ByteString.Base16        as Base16
import           Data.Char                      ( isDigit )
import           Data.Fix                       ( cata )
import           Data.Foldable                  ( foldrM )
import qualified Data.HashMap.Lazy             as M
import           Data.List
import           Data.Maybe
import           Data.Scientific
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.String.Interpolate.IsString
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding
import qualified Data.Text.Lazy                as LazyText
import qualified Data.Text.Lazy.Builder        as Builder
import           Data.These                     ( fromThese )
import qualified Data.Time.Clock.POSIX         as Time
import           Data.Traversable               ( for
                                                , mapM
                                                )
import qualified Data.Vector                   as V
import           Nix.Atoms
import           Nix.Convert
import           Nix.Effects
import           Nix.Effects.Basic              ( fetchTarball )
import qualified Nix.Eval                      as Eval
import           Nix.Exec
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.Json
import           Nix.Normal
import           Nix.Options
import           Nix.Parser              hiding ( nixPath )
import           Nix.Render
import           Nix.Scope
import           Nix.String
import           Nix.String.Coerce
import           Nix.Utils
import           Nix.Value
import           Nix.Value.Equal
import           Nix.Value.Monad
import           Nix.XML
import           System.Nix.Base32              as Base32
import           System.FilePath
import           System.Posix.Files             ( isRegularFile
                                                , isDirectory
                                                , isSymbolicLink
                                                )
import           Text.Read
import           Text.Regex.TDFA

newtype EAMkThunk a
  = EMkThunkInBuiltin a
  deriving Show

instance Exception (EAMkThunk Text)
 where
  displayException (EMkThunkInBuiltin n)
    = "While calling builtin " <> Text.unpack n <> "\n"

newtype EAFoldNixPath a
  = EFoldNixPathUnexpectedEntry a
  deriving Show

instance (Show a, Typeable a)
  => Exception (EAFoldNixPath a)
 where
  displayException (EFoldNixPathUnexpectedEntry x)
    = "Unexpected entry in NIX_PATH: " <> show x

newtype EAAttrsetGet a
  = EAttrsetGetAttrRequired a
  deriving Show

instance Exception (EAAttrsetGet Text)
 where
  displayException (EAttrsetGetAttrRequired k)
    = "Attribute '" <> Text.unpack k <> "' required"

data EAUnsafeGetAttrPos a b
  = EUnsafeGetAttrPosInvalidTypes a b
  deriving Show

instance (Show a, Typeable a, Show b, Typeable b)
  => Exception (EAUnsafeGetAttrPos a b)
 where
  displayException (EUnsafeGetAttrPosInvalidTypes x y)
    = "Invalid types for builtins.unsafeGetAttrPos: " <> show (x, y)

data EAHead_
  = EHead_EmptyList
  deriving Show

instance Exception EAHead_
 where
  displayException EHead_EmptyList
    = "builtins.head: empty list"

data EATail_
  = ETail_EmptyList
  deriving Show

instance Exception EATail_
 where
  displayException ETail_EmptyList
    = "builtins.tail: empty list"

newtype EASubString a
  = ESubStringNegativeStartPosition a
  deriving Show

instance (Show a, Typeable a)
  => Exception (EASubString a)
 where
  displayException (ESubStringNegativeStartPosition start)
    = "builtins.substring: negative start position: " <> show start

data EAMap_
  = EMap_
  deriving Show

instance Exception EAMap_
 where
  displayException EMap_
    = "While applying f in map:\n"

data EAMapAttrs_
  = EMapAttrs_
  deriving Show

instance Exception EAMapAttrs_
 where
  displayException EMapAttrs_
    = "While applying f in mapAttrs:\n"

newtype EADirOf a
  = EDirOfWrongArg a
  deriving Show

instance (Show a, Typeable a)
  => Exception (EADirOf a)
 where
  displayException (EDirOfWrongArg v)
    = "dirOf: expected string or path, got " ++ show v

data EAElemAt_ a b
  = EElemAt_IndexOutOfBound a b
  deriving Show

instance (Show a, Typeable a, Foldable t, Show (t b), Typeable (t b))
  => Exception (EAElemAt_ a (t b))
 where
  displayException (EElemAt_IndexOutOfBound n' xs')
    = "builtins.elem: Index "
    <> show n'
    <> " too large for list of length "
    <> show (length xs')

newtype EAGenList a
  = EGenListNegativeNum a
  deriving Show

instance (Show a, Typeable a)
  => Exception (EAGenList a)
 where
  displayException (EGenListNegativeNum n)
    = "builtins.genList: Expected a non-negative number, got "
    <> show n

data EAGenericClosure
 = EGenericClosureNoAttrsStartSetNOperator
 | EGenericClosureNoAttrStartSet
 | EGenericClosureNoAttrOperator
 deriving Show

instance Exception EAGenericClosure
 where
  displayException EGenericClosureNoAttrsStartSetNOperator
    = "builtins.genericClosure: Attributes 'startSet' and 'operator' required"
  displayException EGenericClosureNoAttrStartSet
    = "builtins.genericClosure: Attribute 'startSet' required"
  displayException EGenericClosureNoAttrOperator
    = "builtins.genericClosure: Attribute 'operator' required"

-- | Evaluate a nix expression in the default context
withNixContext
  :: forall e t f m r
   . (MonadNix e t f m, Has e Options)
  => Maybe FilePath
  -> m r
  -> m r
withNixContext mpath action = do
  base            <- builtins
  opts :: Options <- asks (view hasLens)
  let i = nvList $ map
        ( nvStr
        . hackyMakeNixStringWithoutContext
        . Text.pack
        )
        (include opts)
  pushScope (M.singleton "__includes" i) $ pushScopes base $ case mpath of
    Nothing   -> action
    Just path -> do
      traceM $ "Setting __cur_file = " ++ show path
      let ref = nvPath path
      pushScope (M.singleton "__cur_file" ref) action

builtins :: (MonadNix e t f m, Scoped (NValue t f m) m)
         => m (Scopes m (NValue t f m))
builtins = do
  ref <- defer $ flip nvSet M.empty <$> buildMap
  lst <- ([("builtins", ref)] ++) <$> topLevelBuiltins
  pushScope (M.fromList lst) currentScopes
 where
  buildMap         = M.fromList . map mapping <$> builtinsList
  topLevelBuiltins = map mapping <$> fullBuiltinsList

  fullBuiltinsList = map go <$> builtinsList
   where
    go b@(Builtin TopLevel _) = b
    go (Builtin Normal (name, builtin)) =
      Builtin TopLevel ("__" <> name, builtin)

data BuiltinType = Normal | TopLevel
data Builtin v = Builtin
    { _kind   :: BuiltinType
    , mapping :: (Text, v)
    }

builtinsList :: forall e t f m . MonadNix e t f m => m [Builtin (NValue t f m)]
builtinsList = sequence
  [ do
    version <- toValue (principledMakeNixStringWithoutContext "2.0")
    pure $ Builtin Normal ("nixVersion", version)
  , do
    version <- toValue (5 :: Int)
    pure $ Builtin Normal ("langVersion", version)

  , add0 Normal   "nixPath"          nixPath
  , add  TopLevel "abort"            throw_ -- for now
  , add2 Normal   "add"              add_
  , add2 Normal   "addErrorContext"  addErrorContext
  , add2 Normal   "all"              all_
  , add2 Normal   "any"              any_
  , add  Normal   "attrNames"        attrNames
  , add  Normal   "attrValues"       attrValues
  , add  TopLevel "baseNameOf"       baseNameOf
  , add2 Normal   "bitAnd"           bitAnd
  , add2 Normal   "bitOr"            bitOr
  , add2 Normal   "bitXor"           bitXor
  , add2 Normal   "catAttrs"         catAttrs
  , add2 Normal   "compareVersions"  compareVersions_
  , add  Normal   "concatLists"      concatLists
  , add2 Normal   "concatMap"        concatMap_
  , add' Normal   "concatStringsSep" (arity2 principledIntercalateNixString)
  , add0 Normal   "currentSystem"    currentSystem
  , add0 Normal   "currentTime"      currentTime_
  , add2 Normal   "deepSeq"          deepSeq

    -- This is compiled in so that we only parse and evaluate it once, at
    -- compile-time.
  , add0 TopLevel "derivation" $(do
      let Success expr = parseNixText [i|
        drvAttrs @ { outputs ? [ "out" ], ... }:

        let

          strict = derivationStrict drvAttrs;

          commonAttrs = drvAttrs // (builtins.listToAttrs outputsList) //
            { all = map (x: x.value) outputsList;
              inherit drvAttrs;
            };

          outputToAttrListElement = outputName:
            { name = outputName;
              value = commonAttrs // {
                outPath = builtins.getAttr outputName strict;
                drvPath = strict.drvPath;
                type = "derivation";
                inherit outputName;
              };
            };

          outputsList = map outputToAttrListElement outputs;

        in (builtins.head outputsList).value|]
      [| cata Eval.eval expr |]
    )

  , add  TopLevel "derivationStrict" derivationStrict_
  , add  TopLevel "dirOf"            dirOf
  , add2 Normal   "div"              div_
  , add2 Normal   "elem"             elem_
  , add2 Normal   "elemAt"           elemAt_
  , add  Normal   "exec"             exec_
  , add0 Normal   "false"            (return $ nvConstant $ NBool False)
  , add  Normal   "fetchTarball"     fetchTarball
  , add  Normal   "fetchurl"         fetchurl
  , add2 Normal   "filter"           filter_
  , add3 Normal   "foldl'"           foldl'_
  , add  Normal   "fromJSON"         fromJSON
  , add  Normal   "functionArgs"     functionArgs
  , add2 Normal   "genList"          genList
  , add  Normal   "genericClosure"   genericClosure
  , add2 Normal   "getAttr"          getAttr
  , add  Normal   "getEnv"           getEnv_
  , add2 Normal   "hasAttr"          hasAttr
  , add  Normal   "hasContext"       hasContext
  , add' Normal   "hashString"       (hashString @e @t @f @m)
  , add  Normal   "head"             head_
  , add  TopLevel "import"           import_
  , add2 Normal   "intersectAttrs"   intersectAttrs
  , add  Normal   "isAttrs"          isAttrs
  , add  Normal   "isBool"           isBool
  , add  Normal   "isFloat"          isFloat
  , add  Normal   "isFunction"       isFunction
  , add  Normal   "isInt"            isInt
  , add  Normal   "isList"           isList
  , add  TopLevel "isNull"           isNull
  , add  Normal   "isString"         isString
  , add  Normal   "length"           length_
  , add2 Normal   "lessThan"         lessThan
  , add  Normal   "listToAttrs"      listToAttrs
  , add2 TopLevel "map"              map_
  , add2 TopLevel "mapAttrs"         mapAttrs_
  , add2 Normal   "match"            match_
  , add2 Normal   "mul"              mul_
  , add0 Normal   "null"             (return $ nvConstant NNull)
  , add  Normal   "parseDrvName"     parseDrvName
  , add2 Normal   "partition"        partition_
  , add  Normal   "pathExists"       pathExists_
  , add  TopLevel "placeholder"      placeHolder
  , add  Normal   "readDir"          readDir_
  , add  Normal   "readFile"         readFile_
  , add2 Normal   "findFile"         findFile_
  , add2 TopLevel "removeAttrs"      removeAttrs
  , add3 Normal   "replaceStrings"   replaceStrings
  , add2 TopLevel "scopedImport"     scopedImport
  , add2 Normal   "seq"              seq_
  , add2 Normal   "sort"             sort_
  , add2 Normal   "split"            split_
  , add  Normal   "splitVersion"     splitVersion_
  , add0 Normal   "storeDir"         (return $ nvStr $ principledMakeNixStringWithoutContext "/nix/store")
  , add' Normal   "stringLength"     (arity1 $ Text.length . principledStringIgnoreContext)
  , add' Normal   "sub"              (arity2 ((-) @Integer))
  , add' Normal   "substring"        (substring @e @t @f @m)
  , add  Normal   "tail"             tail_
  , add0 Normal   "true"             (return $ nvConstant $ NBool True)
  , add  TopLevel "throw"            throw_
  , add  Normal   "toJSON"           prim_toJSON
  , add2 Normal   "toFile"           toFile
  , add  Normal   "toPath"           toPath
  , add  TopLevel "toString"         toString
  , add  Normal   "toXML"            toXML_
  , add2 TopLevel "trace"            trace_
  , add  Normal   "tryEval"          tryEval
  , add  Normal   "typeOf"           typeOf
  , add  Normal   "valueSize"        getRecursiveSize
  , add  Normal   "getContext"                 getContext
  , add2 Normal   "appendContext"              appendContext

  , add2 Normal   "unsafeGetAttrPos"           unsafeGetAttrPos
  , add  Normal   "unsafeDiscardStringContext" unsafeDiscardStringContext
  ]
 where
  wrap :: BuiltinType -> Text -> v -> Builtin v
  wrap t n f = Builtin t (n, f)

  arity1 :: forall a b. (a -> b) -> (a -> Prim m b)
  arity1 f = Prim . pure . f
  arity2 :: forall a b c. (a -> b -> c) -> (a -> b -> Prim m c)
  arity2 f = ((Prim . pure) .) . f

  mkThunk n = defer . withFrame
    Info
    (ErrorCall $ displayException $ EMkThunkInBuiltin n)

  add0 t n v = wrap t n <$> mkThunk n v
  add  t n v = wrap t n <$> mkThunk n (builtin (Text.unpack n) v)
  add2 t n v = wrap t n <$> mkThunk n (builtin2 (Text.unpack n) v)
  add3 t n v = wrap t n <$> mkThunk n (builtin3 (Text.unpack n) v)

  add' :: forall a. ToBuiltin t f m a
       => BuiltinType -> Text -> a -> m (Builtin (NValue t f m))
  add' t n v = wrap t n <$> mkThunk n (toBuiltin (Text.unpack n) v)

-- Primops

foldNixPath
  :: forall e t f m r
   . MonadNix e t f m
  => (FilePath -> Maybe String -> NixPathEntryType -> r -> m r)
  -> r
  -> m r
foldNixPath f z = do
  mres <- lookupVar "__includes"
  dirs <- case mres of
    Nothing -> return []
    Just v  -> demand v $ fromValue . Deeper
  mPath <- getEnvVar "NIX_PATH"
  mDataDir <- getEnvVar "NIX_DATA_DIR"
  dataDir <- case mDataDir of
    Nothing -> getDataDir
    Just dataDir -> return dataDir
  foldrM go z
    $  map (fromInclude . principledStringIgnoreContext) dirs
    ++ case mPath of
         Nothing  -> []
         Just str -> uriAwareSplit (Text.pack str)
    ++ [ fromInclude $ Text.pack $ "nix=" ++ dataDir ++ "/nix/corepkgs" ]
 where
  fromInclude x | "://" `Text.isInfixOf` x = (x, PathEntryURI)
                | otherwise                = (x, PathEntryPath)
  go (x, ty) rest = case Text.splitOn "=" x of
    [p] -> f (Text.unpack p) Nothing ty rest
    [n, p] -> f (Text.unpack p) (Just (Text.unpack n)) ty rest
    _ -> throwError
      $ ErrorCall $ displayException $ EFoldNixPathUnexpectedEntry x

nixPath :: MonadNix e t f m => m (NValue t f m)
nixPath = fmap nvList $ flip foldNixPath [] $ \p mn ty rest ->
  pure
    $ (flip nvSet mempty $ M.fromList
        [ case ty of
          PathEntryPath -> ("path", nvPath p)
          PathEntryURI ->
            ( "uri"
            , nvStr (hackyMakeNixStringWithoutContext (Text.pack p))
            )
        , ( "prefix"
          , nvStr
            (hackyMakeNixStringWithoutContext $ Text.pack (fromMaybe "" mn))
          )
        ]
      )
    : rest

toString :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
toString = coerceToString callFunc DontCopyToStore CoerceAny >=> toValue

hasAttr
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
hasAttr x y = fromValue x >>= fromStringNoContext >>= \key ->
  fromValue @(AttrSet (NValue t f m), AttrSet SourcePos) y
    >>= \(aset, _) -> toValue $ M.member key aset

attrsetGet :: MonadNix e t f m => Text -> AttrSet (NValue t f m) -> m (NValue t f m)
attrsetGet k s = case M.lookup k s of
  Just v -> pure v
  Nothing ->
    throwError $ ErrorCall $ displayException $ EAttrsetGetAttrRequired k

hasContext :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
hasContext = toValue . stringHasContext <=< fromValue

getAttr
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
getAttr x y = fromValue x >>= fromStringNoContext >>= \key ->
  fromValue @(AttrSet (NValue t f m), AttrSet SourcePos) y
    >>= \(aset, _) -> attrsetGet key aset

unsafeGetAttrPos
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
unsafeGetAttrPos x y = demand x $ \x' -> demand y $ \y' -> case (x', y') of
  (NVStr ns, NVSet _ apos) ->
    case M.lookup (hackyStringIgnoreContext ns) apos of
      Nothing    -> pure $ nvConstant NNull
      Just delta -> toValue delta
  (x, y) ->
    throwError
      $  ErrorCall $ displayException $ EUnsafeGetAttrPosInvalidTypes x y

-- This function is a bit special in that it doesn't care about the contents
-- of the list.
length_
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
length_ = toValue . (length :: [NValue t f m] -> Int) <=< fromValue

add_
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
add_ x y = demand x $ \x' -> demand y $ \y' -> case (x', y') of
  (NVConstant (NInt   x), NVConstant (NInt y)  ) -> toValue (x + y :: Integer)
  (NVConstant (NFloat x), NVConstant (NInt y)  ) -> toValue (x + fromInteger y)
  (NVConstant (NInt   x), NVConstant (NFloat y)) -> toValue (fromInteger x + y)
  (NVConstant (NFloat x), NVConstant (NFloat y)) -> toValue (x + y)
  (_                    , _                    ) -> throwError $ Addition x' y'

mul_
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
mul_ x y = demand x $ \x' -> demand y $ \y' -> case (x', y') of
  (NVConstant (NInt   x), NVConstant (NInt y)  ) -> toValue (x * y :: Integer)
  (NVConstant (NFloat x), NVConstant (NInt y)  ) -> toValue (x * fromInteger y)
  (NVConstant (NInt   x), NVConstant (NFloat y)) -> toValue (fromInteger x * y)
  (NVConstant (NFloat x), NVConstant (NFloat y)) -> toValue (x * y)
  (_, _) -> throwError $ Multiplication x' y'

div_
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
div_ x y = demand x $ \x' -> demand y $ \y' -> case (x', y') of
  (NVConstant (NInt x), NVConstant (NInt y)) | y /= 0 ->
    toValue (floor (fromInteger x / fromInteger y :: Double) :: Integer)
  (NVConstant (NFloat x), NVConstant (NInt y)) | y /= 0 ->
    toValue (x / fromInteger y)
  (NVConstant (NInt x), NVConstant (NFloat y)) | y /= 0 ->
    toValue (fromInteger x / y)
  (NVConstant (NFloat x), NVConstant (NFloat y)) | y /= 0 -> toValue (x / y)
  (_, _) -> throwError $ Division x' y'

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = return False
anyM p (x : xs) = do
  q <- p x
  if q then return True else anyM p xs

any_
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
any_ f = toValue <=< anyM fromValue <=< mapM (f `callFunc`) <=< fromValue

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x : xs) = do
  q <- p x
  if q then allM p xs else return False

all_
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
all_ f = toValue <=< allM fromValue <=< mapM (f `callFunc`) <=< fromValue

foldl'_
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
foldl'_ f z xs = fromValue @[NValue t f m] xs >>= foldM go z
  where go b a = f `callFunc` b >>= (`callFunc` a)

head_ :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
head_ = fromValue >=> \case
  []    -> throwError $ ErrorCall $ displayException EHead_EmptyList
  h : _ -> pure h

tail_ :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
tail_ = fromValue >=> \case
  []    -> throwError $ ErrorCall $ displayException ETail_EmptyList
  _ : t -> return $ nvList t

data VersionComponent
   = VersionComponent_Pre -- ^ The string "pre"
   | VersionComponent_String Text -- ^ A string other than "pre"
   | VersionComponent_Number Integer -- ^ A number
   deriving (Show, Read, Eq, Ord)

versionComponentToString :: VersionComponent -> Text
versionComponentToString = \case
  VersionComponent_Pre      -> "pre"
  VersionComponent_String s -> s
  VersionComponent_Number n -> Text.pack $ show n

-- | Based on https://github.com/NixOS/nix/blob/4ee4fda521137fed6af0446948b3877e0c5db803/src/libexpr/names.cc#L44
versionComponentSeparators :: String
versionComponentSeparators = ".-"

splitVersion :: Text -> [VersionComponent]
splitVersion s = case Text.uncons s of
  Nothing -> []
  Just (h, t)
    | h `elem` versionComponentSeparators
    -> splitVersion t
    | isDigit h
    -> let (digits, rest) = Text.span isDigit s
       in
         VersionComponent_Number
             (fromMaybe (error $ "splitVersion: couldn't parse " <> show digits)
             $ readMaybe
             $ Text.unpack digits
             )
           : splitVersion rest
    | otherwise
    -> let (chars, rest) = Text.span
             (\c -> not $ isDigit c || c `elem` versionComponentSeparators)
             s
           thisComponent = case chars of
             "pre" -> VersionComponent_Pre
             x     -> VersionComponent_String x
       in  thisComponent : splitVersion rest

splitVersion_ :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
splitVersion_ = fromValue >=> fromStringNoContext >=> \s ->
  return
    $ nvList
    $ flip map (splitVersion s)
    $ nvStr
    . principledMakeNixStringWithoutContext
    . versionComponentToString

compareVersions :: Text -> Text -> Ordering
compareVersions s1 s2 = mconcat
  $ alignWith f (splitVersion s1) (splitVersion s2)
 where
  z = VersionComponent_String ""
  f = uncurry compare . fromThese z z

compareVersions_
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
compareVersions_ t1 t2 = fromValue t1 >>= fromStringNoContext >>= \s1 ->
  fromValue t2 >>= fromStringNoContext >>= \s2 ->
    return $ nvConstant $ NInt $ case compareVersions s1 s2 of
      LT -> -1
      EQ -> 0
      GT -> 1

splitDrvName :: Text -> (Text, Text)
splitDrvName s =
  let
    sep    = "-"
    pieces = Text.splitOn sep s
    isFirstVersionPiece p = case Text.uncons p of
      Just (h, _) | isDigit h -> True
      _                       -> False
    -- Like 'break', but always puts the first item into the first result
    -- list
    breakAfterFirstItem :: (a -> Bool) -> [a] -> ([a], [a])
    breakAfterFirstItem f = \case
      h : t -> let (a, b) = break f t in (h : a, b)
      []    -> ([], [])
    (namePieces, versionPieces) =
      breakAfterFirstItem isFirstVersionPiece pieces
  in
    (Text.intercalate sep namePieces, Text.intercalate sep versionPieces)

parseDrvName
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
parseDrvName = fromValue >=> fromStringNoContext >=> \s -> do
  let (name :: Text, version :: Text) = splitDrvName s
  toValue @(AttrSet (NValue t f m)) $ M.fromList
    [ ( "name" :: Text
      , nvStr $ principledMakeNixStringWithoutContext name
      )
    , ( "version"
      , nvStr $ principledMakeNixStringWithoutContext version
      )
    ]

match_
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
match_ pat str = fromValue pat >>= fromStringNoContext >>= \p ->
  fromValue str >>= \ns -> do
        -- NOTE: Currently prim_match in nix/src/libexpr/primops.cc ignores the
        -- context of its second argument. This is probably a bug but we're
        -- going to preserve the behavior here until it is fixed upstream.
        -- Relevant issue: https://github.com/NixOS/nix/issues/2547
    let s  = principledStringIgnoreContext ns

    let re = makeRegex (encodeUtf8 p) :: Regex
    let mkMatch t
          | Text.null t = toValue ()
          | -- Shorthand for Null
            otherwise   = toValue $ principledMakeNixStringWithoutContext t
    case matchOnceText re (encodeUtf8 s) of
      Just ("", sarr, "") -> do
        let s = map fst (elems sarr)
        nvList <$> traverse (mkMatch . decodeUtf8)
                            (if length s > 1 then tail s else s)
      _ -> pure $ nvConstant NNull

split_
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
split_ pat str = fromValue pat >>= fromStringNoContext >>= \p ->
  fromValue str >>= \ns -> do
        -- NOTE: Currently prim_split in nix/src/libexpr/primops.cc ignores the
        -- context of its second argument. This is probably a bug but we're
        -- going to preserve the behavior here until it is fixed upstream.
        -- Relevant issue: https://github.com/NixOS/nix/issues/2547
    let s = principledStringIgnoreContext ns
    let re       = makeRegex (encodeUtf8 p) :: Regex
        haystack = encodeUtf8 s
    return $ nvList $ splitMatches 0
                                   (map elems $ matchAllText re haystack)
                                   haystack

splitMatches
  :: forall e t f m
   . MonadNix e t f m
  => Int
  -> [[(ByteString, (Int, Int))]]
  -> ByteString
  -> [NValue t f m]
splitMatches _ [] haystack = [thunkStr haystack]
splitMatches _ ([] : _) _ =
  error "Error in splitMatches: this should never happen!"
splitMatches numDropped (((_, (start, len)) : captures) : mts) haystack =
  thunkStr before : caps : splitMatches (numDropped + relStart + len)
                                        mts
                                        (B.drop len rest)
 where
  relStart       = max 0 start - numDropped
  (before, rest) = B.splitAt relStart haystack
  caps           = nvList (map f captures)
  f (a, (s, _)) = if s < 0 then nvConstant NNull else thunkStr a

thunkStr s = nvStr (hackyMakeNixStringWithoutContext (decodeUtf8 s))

substring :: forall e t f m. MonadNix e t f m
  => Int -> Int -> NixString -> Prim m NixString
-- 2019-03-17: NOTE: negative values of 'len' are OK
substring start len str = Prim $ if start < 0
  then
    throwError
    $  ErrorCall $ displayException $ ESubStringNegativeStartPosition start
  else pure $ principledModifyNixContents (Text.take len . Text.drop start) str

attrNames
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
attrNames =
  fromValue @(AttrSet (NValue t f m))
    >=> fmap getDeeper
    .   toValue
    .   map principledMakeNixStringWithoutContext
    .   sort
    .   M.keys

attrValues
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
attrValues =
  fromValue @(AttrSet (NValue t f m))
    >=> toValue
    .   fmap snd
    .   sortOn (fst @Text @(NValue t f m))
    .   M.toList

map_
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
map_ f =
  toValue
    <=< traverse
          ( defer @(NValue t f m)
          . withFrame Debug (ErrorCall $ displayException EMap_)
          . (f `callFunc`)
          )
    <=< fromValue @[NValue t f m]

mapAttrs_
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
mapAttrs_ f xs = fromValue @(AttrSet (NValue t f m)) xs >>= \aset -> do
  let pairs = M.toList aset
  values <- for pairs $ \(key, value) ->
    defer @(NValue t f m)
      $   withFrame Debug (ErrorCall $ displayException EMapAttrs_)
      $   callFunc ?? value
      =<< callFunc f (nvStr (principledMakeNixStringWithoutContext key))
  toValue . M.fromList . zip (map fst pairs) $ values

filter_
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
filter_ f =
  toValue
    <=< filterM (fromValue <=< callFunc f)
    <=< fromValue

catAttrs
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
catAttrs attrName xs = fromValue attrName >>= fromStringNoContext >>= \n ->
  fromValue @[NValue t f m] xs >>= \l ->
    fmap (nvList . catMaybes)
      $ forM l
      $ fmap (M.lookup n)
      . flip demand fromValue

baseNameOf :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
baseNameOf x = do
  ns <- coerceToString callFunc DontCopyToStore CoerceStringy x
  pure $ nvStr
    (principledModifyNixContents (Text.pack . takeFileName . Text.unpack) ns)

bitAnd
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
bitAnd x y =
  fromValue @Integer x >>= \a -> fromValue @Integer y >>= \b -> toValue (a .&. b)

bitOr
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
bitOr x y =
  fromValue @Integer x >>= \a -> fromValue @Integer y >>= \b -> toValue (a .|. b)

bitXor
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
bitXor x y = fromValue @Integer x
  >>= \a -> fromValue @Integer y >>= \b -> toValue (a `xor` b)

dirOf :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
dirOf x = demand x $ \case
  NVStr ns -> pure $ nvStr
    (principledModifyNixContents (Text.pack . takeDirectory . Text.unpack) ns)
  NVPath path -> pure $ nvPath $ takeDirectory path
  v ->
    throwError $ ErrorCall $ displayException $ EDirOfWrongArg v

-- jww (2018-04-28): This should only be a string argument, and not coerced?
unsafeDiscardStringContext
  :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
unsafeDiscardStringContext mnv = do
  ns <- fromValue mnv
  toValue $ principledMakeNixStringWithoutContext $ principledStringIgnoreContext
    ns

seq_
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
seq_ a b = demand a $ \_ -> pure b

-- | We evaluate 'a' only for its effects, so data cycles are ignored.
deepSeq
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
deepSeq a b = b <$ normalForm_ a

elem_
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
elem_ x = toValue <=< anyM (valueEqM x) <=< fromValue

elemAt :: [a] -> Int -> Maybe a
elemAt ls i = case drop i ls of
  []    -> Nothing
  a : _ -> Just a

elemAt_
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
elemAt_ xs n = fromValue n >>= \n' -> fromValue xs >>= \xs' ->
  case elemAt xs' n' of
    Just a -> pure a
    Nothing ->
      throwError
        $ ErrorCall
        $ displayException
        $ EElemAt_IndexOutOfBound n' xs'

genList
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
genList f = fromValue @Integer >=> \n -> if n >= 0
  then toValue =<< forM [0 .. n - 1] (\i -> defer $ (f `callFunc`) =<< toValue i)
  else
    throwError
    $  ErrorCall
    $  displayException
    $  EGenListNegativeNum n

-- We wrap values solely to provide an Ord instance for genericClosure
newtype WValue t f m = WValue (NValue t f m)

instance Comonad f => Eq (WValue t f m) where
  WValue (NVConstant (NFloat x)) == WValue (NVConstant (NInt y)) =
    x == fromInteger y
  WValue (NVConstant (NInt   x)) == WValue (NVConstant (NFloat y)) =
    fromInteger x == y
  WValue (NVConstant (NInt   x)) == WValue (NVConstant (NInt   y)) = x == y
  WValue (NVConstant (NFloat x)) == WValue (NVConstant (NFloat y)) = x == y
  WValue (NVPath     x         ) == WValue (NVPath     y         ) = x == y
  WValue (NVStr x) == WValue (NVStr y) =
    hackyStringIgnoreContext x == hackyStringIgnoreContext y
  _ == _ = False

instance Comonad f => Ord (WValue t f m) where
  WValue (NVConstant (NFloat x)) <= WValue (NVConstant (NInt y)) =
    x <= fromInteger y
  WValue (NVConstant (NInt   x)) <= WValue (NVConstant (NFloat y)) =
    fromInteger x <= y
  WValue (NVConstant (NInt   x)) <= WValue (NVConstant (NInt   y)) = x <= y
  WValue (NVConstant (NFloat x)) <= WValue (NVConstant (NFloat y)) = x <= y
  WValue (NVPath     x         ) <= WValue (NVPath     y         ) = x <= y
  WValue (NVStr x) <= WValue (NVStr y) =
    hackyStringIgnoreContext x <= hackyStringIgnoreContext y
  _ <= _ = False

genericClosure
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
genericClosure = fromValue @(AttrSet (NValue t f m)) >=> \s ->
  case (M.lookup "startSet" s, M.lookup "operator" s) of
    (Nothing, Nothing) ->
      throwError
        $ ErrorCall
        $ displayException EGenericClosureNoAttrsStartSetNOperator
    (Nothing, Just _) ->
      throwError
        $ ErrorCall
        $ displayException EGenericClosureNoAttrStartSet
    (Just _, Nothing) ->
      throwError
        $ ErrorCall
        $ displayException EGenericClosureNoAttrOperator
    (Just startSet, Just operator) ->
      demand startSet $ fromValue @[NValue t f m] >=> \ss ->
        demand operator $ \op -> toValue @[NValue t f m] =<< snd <$> go op ss S.empty
 where
  go
    :: NValue t f m
    -> [NValue t f m]
    -> Set (WValue t f m)
    -> m (Set (WValue t f m), [NValue t f m])
  go _  []       ks = pure (ks, [])
  go op (t : ts) ks = demand t $ \v -> fromValue @(AttrSet (NValue t f m)) v >>= \s -> do
    k <- attrsetGet "key" s
    demand k $ \k' -> do
      if S.member (WValue k') ks
        then go op ts ks
        else do
          ys <- fromValue @[NValue t f m] =<< (op `callFunc` v)
          case S.toList ks of
            []           -> checkComparable k' k'
            WValue j : _ -> checkComparable k' j
          fmap (t :) <$> go op (ts ++ ys) (S.insert (WValue k') ks)

replaceStrings
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
replaceStrings tfrom tto ts = fromValue (Deeper tfrom) >>= \(nsFrom :: [NixString]) ->
  fromValue (Deeper tto) >>= \(nsTo :: [NixString]) ->
    fromValue ts >>= \(ns :: NixString) -> do
      let from = map principledStringIgnoreContext nsFrom
      when (length nsFrom /= length nsTo)
        $  throwError
        $  ErrorCall
        $  "'from' and 'to' arguments to 'replaceStrings'"
        ++ " have different lengths"
      let
        lookupPrefix s = do
          (prefix, replacement) <- find ((`Text.isPrefixOf` s) . fst)
            $ zip from nsTo
          let rest = Text.drop (Text.length prefix) s
          return (prefix, replacement, rest)
        finish b =
          principledMakeNixString (LazyText.toStrict $ Builder.toLazyText b)
        go orig result ctx = case lookupPrefix orig of
          Nothing -> case Text.uncons orig of
            Nothing     -> finish result ctx
            Just (h, t) -> go t (result <> Builder.singleton h) ctx
          Just (prefix, replacementNS, rest) ->
            let replacement = principledStringIgnoreContext replacementNS
                newCtx      = principledGetContext replacementNS
            in  case prefix of
                  "" -> case Text.uncons rest of
                    Nothing -> finish
                      (result <> Builder.fromText replacement)
                      (ctx <> newCtx)
                    Just (h, t) -> go
                      t
                      (mconcat
                        [ result
                        , Builder.fromText replacement
                        , Builder.singleton h
                        ]
                      )
                      (ctx <> newCtx)
                  _ -> go rest
                          (result <> Builder.fromText replacement)
                          (ctx <> newCtx)
      toValue
        $ go (principledStringIgnoreContext ns) mempty
        $ principledGetContext ns

removeAttrs
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
removeAttrs set = fromValue . Deeper >=> \(nsToRemove :: [NixString]) ->
  fromValue @(AttrSet (NValue t f m), AttrSet SourcePos) set >>= \(m, p) -> do
    toRemove <- mapM fromStringNoContext nsToRemove
    toValue (go m toRemove, go p toRemove)
  where go = foldl' (flip M.delete)

intersectAttrs
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
intersectAttrs set1 set2 =
  fromValue @(AttrSet (NValue t f m), AttrSet SourcePos) set1 >>= \(s1, p1) ->
    fromValue @(AttrSet (NValue t f m), AttrSet SourcePos) set2 >>= \(s2, p2) ->
      return $ nvSet (s2 `M.intersection` s1) (p2 `M.intersection` p1)

functionArgs
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
functionArgs fun = demand fun $ \case
  NVClosure p _ ->
    toValue @(AttrSet (NValue t f m)) $ nvConstant . NBool <$> case p of
      Param name     -> M.singleton name False
      ParamSet s _ _ -> isJust <$> M.fromList s
  v ->
    throwError
      $  ErrorCall
      $  "builtins.functionArgs: expected function, got "
      ++ show v

toFile
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
toFile name s = do
  name' <- fromStringNoContext =<< fromValue name
  s'    <- fromValue s
  -- TODO Using hacky here because we still need to turn the context into
  -- runtime references of the resulting file.
  -- See prim_toFile in nix/src/libexpr/primops.cc
  mres  <- toFile_ (Text.unpack name')
                   (Text.unpack $ hackyStringIgnoreContext s')
  let t  = Text.pack $ unStorePath mres
      sc = StringContext t DirectPath
  toValue $ principledMakeNixStringWithSingletonContext t sc

toPath :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
toPath = fromValue @Path >=> toValue @Path

pathExists_ :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
pathExists_ path = demand path $ \case
  NVPath p  -> toValue =<< pathExists p
  NVStr  ns -> toValue =<< pathExists (Text.unpack (hackyStringIgnoreContext ns))
  v ->
    throwError
      $  ErrorCall
      $  "builtins.pathExists: expected path, got "
      ++ show v

hasKind
  :: forall a e t f m
   . (MonadNix e t f m, FromValue a m (NValue t f m))
  => NValue t f m
  -> m (NValue t f m)
hasKind = fromValueMay >=> toValue . \case
  Just (_ :: a) -> True
  _             -> False

isAttrs
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isAttrs = hasKind @(AttrSet (NValue t f m))

isList
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isList = hasKind @[NValue t f m]

isString
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isString = hasKind @NixString

isInt
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isInt = hasKind @Int

isFloat
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isFloat = hasKind @Float

isBool
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isBool = hasKind @Bool

isNull
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isNull = hasKind @()

isFunction :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
isFunction func = demand func $ \case
  NVClosure{} -> toValue True
  _           -> toValue False

throw_ :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
throw_ mnv = do
  ns <- coerceToString callFunc CopyToStore CoerceStringy mnv
  throwError . ErrorCall . Text.unpack $ principledStringIgnoreContext ns

import_
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
import_ = scopedImport (nvSet M.empty M.empty)

scopedImport
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
scopedImport asetArg pathArg = fromValue @(AttrSet (NValue t f m)) asetArg >>= \s ->
  fromValue pathArg >>= \(Path p) -> do
    path  <- pathToDefaultNix @t @f @m p
    mres  <- lookupVar "__cur_file"
    path' <- case mres of
      Nothing -> do
        traceM "No known current directory"
        return path
      Just p -> demand p $ fromValue >=> \(Path p') -> do
        traceM $ "Current file being evaluated is: " ++ show p'
        return $ takeDirectory p' </> path
    clearScopes @(NValue t f m)
      $ withNixContext (Just path')
      $ pushScope s
      $ importPath @t @f @m path'

getEnv_ :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
getEnv_ = fromValue >=> fromStringNoContext >=> \s -> do
  mres <- getEnvVar (Text.unpack s)
  toValue $ principledMakeNixStringWithoutContext $ case mres of
    Nothing -> ""
    Just v  -> Text.pack v

sort_
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
sort_ comp = fromValue >=> sortByM (cmp comp) >=> toValue
 where
  cmp f a b = do
    isLessThan <- f `callFunc` a >>= (`callFunc` b)
    fromValue isLessThan >>= \case
      True  -> pure LT
      False -> do
        isGreaterThan <- f `callFunc` b >>= (`callFunc` a)
        fromValue isGreaterThan <&> \case
          True  -> GT
          False -> EQ

lessThan
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
lessThan ta tb = demand ta $ \va -> demand tb $ \vb -> do
  let badType =
        throwError
          $  ErrorCall
          $  "builtins.lessThan: expected two numbers or two strings, "
          ++ "got "
          ++ show va
          ++ " and "
          ++ show vb
  nvConstant . NBool <$> case (va, vb) of
    (NVConstant ca, NVConstant cb) -> case (ca, cb) of
      (NInt   a, NInt b  ) -> pure $ a < b
      (NFloat a, NInt b  ) -> pure $ a < fromInteger b
      (NInt   a, NFloat b) -> pure $ fromInteger a < b
      (NFloat a, NFloat b) -> pure $ a < b
      _                    -> badType
    (NVStr a, NVStr b) ->
      pure $ principledStringIgnoreContext a < principledStringIgnoreContext b
    _ -> badType

concatLists
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
concatLists =
  fromValue @[NValue t f m]
    >=> mapM (flip demand $ fromValue @[NValue t f m] >=> pure)
    >=> toValue
    .   concat

concatMap_
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
concatMap_ f =
  fromValue @[NValue t f m]
    >=> traverse applyFunc
    >=> toValue . concat
  where
    applyFunc :: NValue t f m  -> m [NValue t f m]
    applyFunc =  (f `callFunc`) >=> fromValue

listToAttrs
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
listToAttrs = fromValue @[NValue t f m] >=> \l ->
  fmap (flip nvSet M.empty . M.fromList . reverse)
    $   forM l
    $   flip demand
    $   fromValue @(AttrSet (NValue t f m))
    >=> \s -> do
          t <- attrsetGet "name" s
          demand t $ fromValue >=> \n -> do
            name <- fromStringNoContext n
            val  <- attrsetGet "value" s
            pure (name, val)

-- prim_hashString from nix/src/libexpr/primops.cc
-- fail if context in the algo arg
-- propagate context from the s arg
hashString
  :: forall e t f m. MonadNix e t f m => NixString -> NixString -> Prim m NixString
hashString nsAlgo ns = Prim $ do
  algo <- fromStringNoContext nsAlgo
  let f g = pure $ principledModifyNixContents g ns
  case algo of
    "md5" ->
      f $ \s ->
#if MIN_VERSION_hashing(0, 1, 0)
                Text.pack $ show (hash (encodeUtf8 s) :: MD5.MD5)
#else
          decodeUtf8 $ Base16.encode $ MD5.hash $ encodeUtf8 s
#endif
    "sha1" ->
      f $ \s ->
#if MIN_VERSION_hashing(0, 1, 0)
                Text.pack $ show (hash (encodeUtf8 s) :: SHA1.SHA1)
#else
          decodeUtf8 $ Base16.encode $ SHA1.hash $ encodeUtf8 s
#endif
    "sha256" ->
      f $ \s ->
#if MIN_VERSION_hashing(0, 1, 0)
                Text.pack $ show (hash (encodeUtf8 s) :: SHA256.SHA256)
#else
          decodeUtf8 $ Base16.encode $ SHA256.hash $ encodeUtf8 s
#endif
    "sha512" ->
      f $ \s ->
#if MIN_VERSION_hashing(0, 1, 0)
                Text.pack $ show (hash (encodeUtf8 s) :: SHA512.SHA512)
#else
          decodeUtf8 $ Base16.encode $ SHA512.hash $ encodeUtf8 s
#endif
    _ ->
      throwError
        $  ErrorCall
        $  "builtins.hashString: "
        ++ "expected \"md5\", \"sha1\", \"sha256\", or \"sha512\", got "
        ++ show algo

placeHolder :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
placeHolder = fromValue >=> fromStringNoContext >=> \t -> do
  h <- runPrim
    (hashString (principledMakeNixStringWithoutContext "sha256")
                (principledMakeNixStringWithoutContext ("nix-output:" <> t))
    )
  toValue
    $ principledMakeNixStringWithoutContext
    $ Text.cons '/'
    $ Base32.encode
    $ fst             -- The result coming out of hashString is base16 encoded
    $ Base16.decode
    $ encodeUtf8
    $ principledStringIgnoreContext h

absolutePathFromValue :: MonadNix e t f m => NValue t f m -> m FilePath
absolutePathFromValue = \case
  NVStr ns -> do
    let path = Text.unpack $ hackyStringIgnoreContext ns
    unless (isAbsolute path)
      $  throwError
      $  ErrorCall
      $  "string "
      ++ show path
      ++ " doesn't represent an absolute path"
    pure path
  NVPath path -> pure path
  v           -> throwError $ ErrorCall $ "expected a path, got " ++ show v

readFile_ :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
readFile_ path = demand path $
  absolutePathFromValue >=> Nix.Render.readFile >=> toValue

findFile_
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
findFile_ aset filePath = demand aset $ \aset' -> demand filePath $ \filePath' ->
  case (aset', filePath') of
    (NVList x, NVStr ns) -> do
      mres <- findPath @t @f @m x (Text.unpack (hackyStringIgnoreContext ns))
      pure $ nvPath mres
    (NVList _, y) ->
      throwError $ ErrorCall $ "expected a string, got " ++ show y
    (x, NVStr _) -> throwError $ ErrorCall $ "expected a list, got " ++ show x
    (x, y) ->
      throwError $ ErrorCall $ "Invalid types for builtins.findFile: " ++ show
        (x, y)

data FileType
   = FileTypeRegular
   | FileTypeDirectory
   | FileTypeSymlink
   | FileTypeUnknown
   deriving (Show, Read, Eq, Ord)

instance Convertible e t f m => ToValue FileType m (NValue t f m) where
  toValue = toValue . principledMakeNixStringWithoutContext . \case
    FileTypeRegular   -> "regular" :: Text
    FileTypeDirectory -> "directory"
    FileTypeSymlink   -> "symlink"
    FileTypeUnknown   -> "unknown"

readDir_
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
readDir_ p = demand p $ \path' -> do
  path           <- absolutePathFromValue path'
  items          <- listDirectory path
  itemsWithTypes <- forM items $ \item -> do
    s <- getSymbolicLinkStatus $ path </> item
    let t = if
          | isRegularFile s  -> FileTypeRegular
          | isDirectory s    -> FileTypeDirectory
          | isSymbolicLink s -> FileTypeSymlink
          | otherwise        -> FileTypeUnknown
    pure (Text.pack item, t)
  getDeeper <$> toValue (M.fromList itemsWithTypes)

fromJSON
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
fromJSON arg = demand arg $ fromValue >=> fromStringNoContext >=> \encoded ->
  case A.eitherDecodeStrict' @A.Value $ encodeUtf8 encoded of
    Left jsonError ->
      throwError $ ErrorCall $ "builtins.fromJSON: " ++ jsonError
    Right v -> jsonToNValue v
 where
  jsonToNValue = \case
    A.Object m -> flip nvSet M.empty <$> traverse jsonToNValue m
    A.Array  l -> nvList <$> traverse jsonToNValue (V.toList l)
    A.String s -> pure $ nvStr $ hackyMakeNixStringWithoutContext s
    A.Number n -> pure $ nvConstant $ case floatingOrInteger n of
      Left  r -> NFloat r
      Right i -> NInt i
    A.Bool   b -> pure $ nvConstant $ NBool b
    A.Null     -> pure $ nvConstant NNull

prim_toJSON :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
prim_toJSON x = demand x $ fmap nvStr . nvalueToJSONNixString

toXML_ :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
toXML_ v = demand v $ fmap (nvStr . toXML) . normalForm

typeOf :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
typeOf v = demand v $ toValue . principledMakeNixStringWithoutContext . \case
  NVConstant a -> case a of
    NInt   _ -> "int"
    NFloat _ -> "float"
    NBool  _ -> "bool"
    NNull    -> "null"
  NVStr  _      -> "string"
  NVList _      -> "list"
  NVSet _ _     -> "set"
  NVClosure{}   -> "lambda"
  NVPath _      -> "path"
  NVBuiltin _ _ -> "lambda"
  _             -> error "Pattern synonyms obscure complete patterns"

tryEval
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
tryEval e = catch (demand e (pure . onSuccess)) (pure . onError)
 where
  onSuccess v = flip nvSet M.empty $ M.fromList
    [("success", nvConstant (NBool True)), ("value", v)]

  onError :: SomeException -> NValue t f m
  onError _ = flip nvSet M.empty $ M.fromList
    [ ("success", nvConstant (NBool False))
    , ("value"  , nvConstant (NBool False))
    ]

trace_
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
trace_ msg action = do
  traceEffect @t @f @m
    .   Text.unpack
    .   principledStringIgnoreContext
    =<< fromValue msg
  pure action

-- TODO: remember error context
addErrorContext
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
addErrorContext _ action = pure action

exec_
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
exec_ xs = do
  ls <- fromValue @[NValue t f m] xs
  xs <- traverse (coerceToString callFunc DontCopyToStore CoerceStringy) ls
  -- TODO Still need to do something with the context here
  -- See prim_exec in nix/src/libexpr/primops.cc
  -- Requires the implementation of EvalState::realiseContext
  exec (map (Text.unpack . hackyStringIgnoreContext) xs)

fetchurl
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
fetchurl v = demand v $ \case
  NVSet s _ -> attrsetGet "url" s >>= demand ?? (go (M.lookup "sha256" s))
  v@NVStr{} -> go Nothing v
  v ->
    throwError
      $  ErrorCall
      $  "builtins.fetchurl: Expected URI or set, got "
      ++ show v
 where
  go :: Maybe (NValue t f m) -> NValue t f m -> m (NValue t f m)
  go _msha = \case
    NVStr ns -> noContextAttrs ns >>= getURL >>= \case -- msha
      Left  e -> throwError e
      Right p -> toValue p
    v ->
      throwError
        $  ErrorCall
        $  "builtins.fetchurl: Expected URI or string, got "
        ++ show v

  noContextAttrs ns = case principledGetStringNoContext ns of
    Nothing ->
      throwError $ ErrorCall $ "builtins.fetchurl: unsupported arguments to url"
    Just t -> pure t

partition_
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
partition_ f = fromValue @[NValue t f m] >=> \l -> do
  let match t = f `callFunc` t >>= fmap (, t) . fromValue
  selection <- traverse match l
  let (right, wrong) = partition fst selection
  let makeSide       = nvList . map snd
  toValue @(AttrSet (NValue t f m))
    $ M.fromList [("right", makeSide right), ("wrong", makeSide wrong)]

currentSystem :: MonadNix e t f m => m (NValue t f m)
currentSystem = do
  os   <- getCurrentSystemOS
  arch <- getCurrentSystemArch
  return $ nvStr $ principledMakeNixStringWithoutContext (arch <> "-" <> os)

currentTime_ :: MonadNix e t f m => m (NValue t f m)
currentTime_ = do
  opts :: Options <- asks (view hasLens)
  toValue @Integer $ round $ Time.utcTimeToPOSIXSeconds (currentTime opts)

derivationStrict_ :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
derivationStrict_ = derivationStrict

getRecursiveSize :: (MonadIntrospect m, Applicative f) => a -> m (NValue t f m)
getRecursiveSize = fmap (nvConstant . NInt . fromIntegral) . recursiveSize

getContext
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
getContext x = demand x $ \case
  (NVStr ns) -> do
    let context =
          getNixLikeContext $ toNixLikeContext $ principledGetContext ns
    valued :: M.HashMap Text (NValue t f m) <- sequenceA $ M.map toValue context
    pure $ flip nvSet M.empty $ valued
  x ->
    throwError $ ErrorCall $ "Invalid type for builtins.getContext: " ++ show x

appendContext
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
appendContext x y = demand x $ \x' -> demand y $ \y' -> case (x', y') of
  (NVStr ns, NVSet attrs _) -> do
    newContextValues <- forM attrs $ \attr -> demand attr $ \case
      NVSet attrs _ -> do
        -- TODO: Fail for unexpected keys.
        path <- maybe (return False) (demand ?? fromValue)
          $ M.lookup "path" attrs
        allOutputs <- maybe (return False) (demand ?? fromValue)
          $ M.lookup "allOutputs" attrs
        outputs <- case M.lookup "outputs" attrs of
          Nothing -> return []
          Just os -> demand os $ \case
            NVList vs ->
              forM vs $ fmap principledStringIgnoreContext . fromValue
            x ->
              throwError
                $ ErrorCall
                $ "Invalid types for context value outputs in builtins.appendContext: "
                ++ show x
        return $ NixLikeContextValue path allOutputs outputs
      x ->
        throwError
          $  ErrorCall
          $  "Invalid types for context value in builtins.appendContext: "
          ++ show x
    toValue
      $ principledMakeNixString (principledStringIgnoreContext ns)
      $ fromNixLikeContext
      $ NixLikeContext
      $ M.unionWith (<>) newContextValues
      $ getNixLikeContext
      $ toNixLikeContext
      $ principledGetContext ns
  (x, y) ->
    throwError
      $  ErrorCall
      $  "Invalid types for builtins.appendContext: "
      ++ show (x, y)

newtype Prim m a = Prim { runPrim :: m a }

-- | Types that support conversion to nix in a particular monad
class ToBuiltin t f m a | a -> m where
    toBuiltin :: String -> a -> m (NValue t f m)

instance (MonadNix e t f m, ToValue a m (NValue t f m))
      => ToBuiltin t f m (Prim m a) where
  toBuiltin _ p = toValue =<< runPrim p

instance ( MonadNix e t f m
         , FromValue a m (Deeper (NValue t f m))
         , ToBuiltin t f m b
         )
      => ToBuiltin t f m (a -> b) where
  toBuiltin name f =
    return $ nvBuiltin name (fromValue . Deeper >=> toBuiltin name . f)
