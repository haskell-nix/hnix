{-# language CPP #-}
{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language FunctionalDependencies #-}
{-# language KindSignatures #-}
{-# language MonoLocalBinds #-}
{-# language MultiWayIf #-}
{-# language PartialTypeSignatures #-}
{-# language QuasiQuotes #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}
{-# language PackageImports #-} -- 2021-07-05: Due to hashing Haskell IT system situation, in HNix we currently ended-up with 2 hash package dependencies @{hashing, cryptonite}@

{-# options_ghc -fno-warn-name-shadowing #-}


-- | Code that implements Nix builtins. Lists the functions that are built into the Nix expression evaluator. Some built-ins (aka `derivation`), are always in the scope, so they can be accessed by the name. To keap the namespace clean, most built-ins are inside the `builtins` scope - a set that contains all what is a built-in.
module Nix.Builtins
  ( withNixContext
  , builtins
  )
where

import           Nix.Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Control.Monad                  ( foldM )
import           Control.Monad.Catch            ( MonadCatch(catch) )
import           Control.Monad.ListM            ( sortByM )
import           "hashing" Crypto.Hash
import qualified "hashing" Crypto.Hash.MD5     as MD5
import qualified "hashing" Crypto.Hash.SHA1    as SHA1
import qualified "hashing" Crypto.Hash.SHA256  as SHA256
import qualified "hashing" Crypto.Hash.SHA512  as SHA512
import qualified Data.Aeson                    as A
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key                as AKM
import qualified Data.Aeson.KeyMap             as AKM
#endif
import           Data.Align                     ( alignWith )
import           Data.Array
import           Data.Bits
import qualified Data.ByteString               as B
import           Data.ByteString.Base16        as Base16
import           Data.Char                      ( isDigit )
import           Data.Foldable                  ( foldrM )
import           Data.Fix                       ( foldFix )
import           Data.List                      ( partition )
import qualified Data.HashSet                  as HS
import qualified Data.HashMap.Lazy             as M
import           Data.Scientific
import qualified Data.Set                      as S
import qualified Data.Text                     as Text
import           Data.Text.Read                 ( decimal )
import qualified Data.Text.Lazy.Builder        as Builder
import           Data.These                     ( fromThese, These )
import qualified Data.Time.Clock.POSIX         as Time
import qualified Data.Vector                   as V
import           NeatInterpolation              ( text )
import           Nix.Atoms
import           Nix.Convert
import           Nix.Effects
import           Nix.Effects.Basic              ( fetchTarball )
import           Nix.Exec
import           Nix.Expr.Types
import qualified Nix.Eval                      as Eval
import           Nix.Frames
import           Nix.Json
import           Nix.Normal
import           Nix.Options
import           Nix.Parser
import           Nix.Render
import           Nix.Scope
import           Nix.String
import           Nix.String.Coerce
import           Nix.Value
import           Nix.Value.Equal
import           Nix.Value.Monad
import           Nix.XML
import           System.Nix.Base32             as Base32
import           System.Nix.Store.Types         ( FileIngestionMethod(..)
                                                , RepairMode(..)
                                                )
import           System.PosixCompat.Files       ( isRegularFile
                                                , isDirectory
                                                , isSymbolicLink
                                                )
import qualified Text.Show
import           Text.Regex.TDFA                ( Regex
                                                , makeRegex
                                                , matchOnceText
                                                , matchAllText
                                                )

-- This is a big module. There is recursive reuse:
-- @builtins -> builtinsList -> scopedImport -> withNixContext -> builtins@,
-- since @builtins@ is self-recursive: aka we ship @builtins.builtins.builtins...@.

-- * Internal

-- ** Nix Builtins Haskell type level

newtype Prim m a = Prim (m a)

data BuiltinType = Normal | TopLevel
data Builtin v =
  Builtin
    { _kind   :: BuiltinType
    , mapping :: (VarName, v)
    }

-- *** @class ToBuiltin@ and its instances

-- | Types that support conversion to nix in a particular monad
class ToBuiltin t f m a | a -> m where
  toBuiltin :: Text -> a -> m (NValue t f m)

instance
  ( MonadNix e t f m
  , ToValue a m (NValue t f m)
  )
  => ToBuiltin t f m (Prim m a) where
  toBuiltin _ p = toValue @a @m =<< coerce p

instance
  ( MonadNix e t f m
  , FromValue a m (Deeper (NValue t f m))
  , ToBuiltin t f m b
  )
  => ToBuiltin t f m (a -> b) where
  toBuiltin name f =
    pure $ NVBuiltin (coerce name) $ toBuiltin name . f <=< fromValue . Deeper

-- *** @WValue@ closure wrapper to have @Ord@

-- We wrap values solely to provide an Ord instance for genericClosure
newtype WValue t f m = WValue (NValue t f m)

instance NVConstraint f => Eq (WValue t f m) where
  WValue (NVConstant (NFloat x)) == WValue (NVConstant (NInt y)) =
    x == fromInteger y
  WValue (NVConstant (NInt   x)) == WValue (NVConstant (NFloat y)) =
    fromInteger x == y
  WValue (NVConstant (NInt   x)) == WValue (NVConstant (NInt   y)) = x == y
  WValue (NVConstant (NFloat x)) == WValue (NVConstant (NFloat y)) = x == y
  WValue (NVPath     x         ) == WValue (NVPath     y         ) = x == y
  WValue (NVStr x) == WValue (NVStr y) =
    ignoreContext x == ignoreContext y
  _ == _ = False

instance NVConstraint f => Ord (WValue t f m) where
  WValue (NVConstant (NFloat x)) <= WValue (NVConstant (NInt y)) =
    x <= fromInteger y
  WValue (NVConstant (NInt   x)) <= WValue (NVConstant (NFloat y)) =
    fromInteger x <= y
  WValue (NVConstant (NInt   x)) <= WValue (NVConstant (NInt   y)) = x <= y
  WValue (NVConstant (NFloat x)) <= WValue (NVConstant (NFloat y)) = x <= y
  WValue (NVPath     x         ) <= WValue (NVPath     y         ) = x <= y
  WValue (NVStr x) <= WValue (NVStr y) =
    ignoreContext x <= ignoreContext y
  _ <= _ = False

-- ** Helpers

pattern NVBool :: MonadNix e t f m => Bool -> NValue t f m
pattern NVBool a = NVConstant (NBool a)

data NixPathEntryType
  = PathEntryPath
  | PathEntryURI
 deriving (Show, Eq)

-- | @NIX_PATH@ is colon-separated, but can also contain URLs, which have a colon
-- (i.e. @https://...@)
uriAwareSplit :: Text -> [(Text, NixPathEntryType)]
uriAwareSplit txt =
  case Text.break (== ':') txt of
    (e1, e2)
      | Text.null e2                              -> one (e1, PathEntryPath)
      | "://" `Text.isPrefixOf` e2      ->
        let ((suffix, _) : path) = uriAwareSplit (Text.drop 3 e2) in
        (e1 <> "://" <> suffix, PathEntryURI) : path
      | otherwise                                 -> (e1, PathEntryPath) : uriAwareSplit (Text.drop 1 e2)

foldNixPath
  :: forall e t f m r
   . MonadNix e t f m
  => r
  -> (Path -> Maybe Text -> NixPathEntryType -> r -> m r)
  -> m r
foldNixPath z f =
  do
    mres <- lookupVar "__includes"
    dirs <-
      maybe
        stub
        ((fromValue . Deeper) <=< demand)
        mres
    mPath    <- getEnvVar "NIX_PATH"
    mDataDir <- getEnvVar "NIX_DATA_DIR"
    dataDir  <-
      maybe
        getDataDir
        (pure . coerce . toString)
        mDataDir

    foldrM
      fun
      z
      $ (fromInclude . ignoreContext <$> dirs)
        <> uriAwareSplit `whenJust` mPath
        <> one (fromInclude $ "nix=" <> fromString (coerce dataDir) <> "/nix/corepkgs")
 where

  fromInclude :: Text -> (Text, NixPathEntryType)
  fromInclude x =
    (x, ) $
      bool
        PathEntryPath
        PathEntryURI
        ("://" `Text.isInfixOf` x)

  fun :: (Text, NixPathEntryType) -> r -> m r
  fun (x, ty) rest =
    case Text.splitOn "=" x of
      [p] -> f (coerce $ toString p) mempty ty rest
      [n, p] -> f (coerce $ toString p) (pure n) ty rest
      _ -> throwError $ ErrorCall $ "Unexpected entry in NIX_PATH: " <> show x

attrsetGet :: MonadNix e t f m => VarName -> AttrSet (NValue t f m) -> m (NValue t f m)
attrsetGet k s =
  maybe
    (throwError $ ErrorCall $ toString @Text $ "Attribute '" <> coerce k <> "' required")
    pure
    (M.lookup k s)

data VersionComponent
  = VersionComponentPre -- ^ The string "pre"
  | VersionComponentString !Text -- ^ A string other than "pre"
  | VersionComponentNumber !Integer -- ^ A number
  deriving (Read, Eq, Ord)

instance Show VersionComponent where
  show =
    \case
      VersionComponentPre      -> "pre"
      VersionComponentString s -> show s
      VersionComponentNumber n -> show n

splitVersion :: Text -> [VersionComponent]
splitVersion s =
  (\ (x, xs) -> if
    | isRight eDigitsPart ->
        either
          (\ e -> error $ "splitVersion: did hit impossible: '" <> fromString e <> "' while parsing '" <> s <> "'.")
          (\ res ->
            one (VersionComponentNumber $ fst res)
            <> splitVersion (snd res)
          )
          eDigitsPart

    | x `elem` separators -> splitVersion xs

    | otherwise -> one charsPart <> splitVersion rest2
  ) `whenJust` Text.uncons s
 where
  -- | Based on https://github.com/NixOS/nix/blob/4ee4fda521137fed6af0446948b3877e0c5db803/src/libexpr/names.cc#L44
  separators :: String
  separators = ".-"

  eDigitsPart :: Either String (Integer, Text)
  eDigitsPart = decimal @Integer $ s

  (charsSpan, rest2) =
    Text.span
      (\c -> not $ isDigit c || c `elem` separators)
      s

  charsPart :: VersionComponent
  charsPart =
    case charsSpan of
      "pre" -> VersionComponentPre
      xs'   -> VersionComponentString xs'


compareVersions :: Text -> Text -> Ordering
compareVersions s1 s2 =
  fold $ (alignWith cmp `on` splitVersion) s1 s2
 where
  cmp :: These VersionComponent VersionComponent -> Ordering
  cmp = uncurry compare . join fromThese (VersionComponentString mempty)

splitDrvName :: Text -> (Text, Text)
splitDrvName s =
  both (Text.intercalate sep) (namePieces, versionPieces)
 where
  sep    = "-"
  pieces :: [Text]
  pieces = Text.splitOn sep s
  isFirstVersionPiece :: Text -> Bool
  isFirstVersionPiece p =
    maybe
      False
      (isDigit . fst)
      (Text.uncons p)
  -- Like 'break', but always puts the first item into the first result
  -- list
  breakAfterFirstItem :: (a -> Bool) -> [a] -> ([a], [a])
  breakAfterFirstItem f =
    handlePresence
      mempty
      (\ (h : t) -> let (a, b) = break f t in (h : a, b))
  (namePieces, versionPieces) =
    breakAfterFirstItem isFirstVersionPiece pieces

splitMatches
  :: forall e t f m
   . MonadNix e t f m
  => Int
  -> [[(ByteString, (Int, Int))]]
  -> ByteString
  -> [NValue t f m]
splitMatches _ [] haystack = one $ thunkStr haystack
splitMatches _ ([] : _) _ =
  fail "Fail in splitMatches: this should never happen!"
splitMatches numDropped (((_, (start, len)) : captures) : mts) haystack =
  thunkStr before : caps : splitMatches (numDropped + relStart + len)
                                        mts
                                        (B.drop len rest)
 where
  relStart       = max 0 start - numDropped
  (before, rest) = B.splitAt relStart haystack
  caps :: NValue t f m
  caps           = NVList (f <$> captures)
  f :: (ByteString, (Int, b)) -> NValue t f m
  f (a, (s, _))  =
    bool
      NVNull
      (thunkStr a)
      (s >= 0)

thunkStr :: NVConstraint f => ByteString -> NValue t f m
thunkStr s = mkNVStrWithoutContext $ decodeUtf8 s

hasKind
  :: forall a e t f m
   . (MonadNix e t f m, FromValue a m (NValue t f m))
  => NValue t f m
  -> m (NValue t f m)
hasKind =
  inHaskMay
    (isJust @a)


absolutePathFromValue :: MonadNix e t f m => NValue t f m -> m Path
absolutePathFromValue =
  \case
    NVStr ns ->
      do
        let
          path = coerce . toString $ ignoreContext ns

        when (not (isAbsolute path)) $ throwError $ ErrorCall $ "string " <> show path <> " doesn't represent an absolute path"
        pure path

    NVPath path -> pure path
    v           -> throwError $ ErrorCall $ "expected a path, got " <> show v


data FileType
  = FileTypeRegular
  | FileTypeDirectory
  | FileTypeSymlink
  | FileTypeUnknown
  deriving (Show, Read, Eq, Ord)

instance Convertible e t f m => ToValue FileType m (NValue t f m) where
  toValue =
    toValue . mkNixStringWithoutContext .
      \case
        FileTypeRegular   -> "regular" :: Text
        FileTypeDirectory -> "directory"
        FileTypeSymlink   -> "symlink"
        FileTypeUnknown   -> "unknown"

-- ** Builtin functions

derivationNix
  :: forall e t f m. (MonadNix e t f m, Scoped (NValue t f m) m)
  => m (NValue t f m)
derivationNix = foldFix Eval.eval $$(do
    -- This is compiled in so that we only parse it once at compile-time.
    let Right expr = parseNixText [text|
      drvAttrs @ { outputs ? [ "out" ], ... }:

      let

        strict = derivationStrict drvAttrs;

        commonAttrs = drvAttrs
          // (builtins.listToAttrs outputsList)
          // { all = map (x: x.value) outputsList;
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
    [|| expr ||]
  )

nixPathNix :: forall e t f m . MonadNix e t f m => m (NValue t f m)
nixPathNix =
  fmap
    NVList
    $ foldNixPath mempty $
        \p mn ty rest ->
          pure $
            pure
              (NVSet
                mempty
                (M.fromList
                  [case ty of
                    PathEntryPath -> ("path", NVPath  p)
                    PathEntryURI  -> ( "uri", mkNVStrWithoutContext $ fromString $ coerce p)

                  , ( "prefix", mkNVStrWithoutContext $ maybeToMonoid mn)
                  ]
                )
              )
            <> rest

toStringNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
toStringNix = toValue <=< coerceAnyToNixString callFunc DontCopyToStore

hasAttrNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
hasAttrNix x y =
  do
    (coerce -> key) <- fromStringNoContext =<< fromValue x
    (aset, _) <- fromValue @(AttrSet (NValue t f m), PositionSet) y

    toValue $ M.member key aset

hasContextNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
hasContextNix = inHask hasContext

getAttrNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
getAttrNix x y =
  do
    (coerce -> key) <- fromStringNoContext =<< fromValue x
    (aset, _) <- fromValue @(AttrSet (NValue t f m), PositionSet) y

    attrsetGet key aset

unsafeDiscardOutputDependencyNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> m (NValue t f m)
unsafeDiscardOutputDependencyNix nv =
  do
    (nc, ns) <- (getStringContext &&& ignoreContext) <$> fromValue nv
    toValue $ mkNixString (HS.map discard nc) ns
 where
  discard :: StringContext -> StringContext
  discard (StringContext AllOutputs a) = StringContext DirectPath a
  discard x                            = x

unsafeGetAttrPosNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
unsafeGetAttrPosNix nvX nvY =
  do
    x <- demand nvX
    y <- demand nvY

    case (x, y) of
      (NVStr ns, NVSet apos _) ->
        maybe
          (pure NVNull)
          toValue
          (M.lookup @VarName (coerce $ ignoreContext ns) apos)
      _xy -> throwError $ ErrorCall $ "Invalid types for builtins.unsafeGetAttrPosNix: " <> show _xy

-- This function is a bit special in that it doesn't care about the contents
-- of the list.
lengthNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
lengthNix = inHask (length :: [NValue t f m] -> Int)

addNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
addNix nvX nvY =
  do
    x' <- demand nvX
    y' <- demand nvY

    case (x', y') of
      (NVConstant (NInt   x), NVConstant (NInt   y)) -> toValue (             x + y :: Integer       )
      (NVConstant (NFloat x), NVConstant (NInt   y)) -> toValue $             x + fromInteger y
      (NVConstant (NInt   x), NVConstant (NFloat y)) -> toValue $ fromInteger x + y
      (NVConstant (NFloat x), NVConstant (NFloat y)) -> toValue $             x + y
      (_x                   , _y                   ) -> throwError $ Addition _x _y

mulNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
mulNix nvX nvY =
  do
    x' <- demand nvX
    y' <- demand nvY

    case (x', y') of
      (NVConstant (NInt   x), NVConstant (NInt   y)) -> toValue (x * y :: Integer       )
      (NVConstant (NFloat x), NVConstant (NInt   y)) -> toValue (x * fromInteger y)
      (NVConstant (NInt   x), NVConstant (NFloat y)) -> toValue (fromInteger x * y)
      (NVConstant (NFloat x), NVConstant (NFloat y)) -> toValue (x * y            )
      (_x                   , _y                   ) -> throwError $ Multiplication _x _y

divNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
divNix nvX nvY =
  do
    x' <- demand nvX
    y' <- demand nvY
    case (x', y') of
      (NVConstant (NInt   x), NVConstant (NInt   y)) | y /= 0 -> toValue (  floor (fromInteger x / fromInteger y :: Double) :: Integer)
      (NVConstant (NFloat x), NVConstant (NInt   y)) | y /= 0 -> toValue $                     x / fromInteger y
      (NVConstant (NInt   x), NVConstant (NFloat y)) | y /= 0 -> toValue $         fromInteger x / y
      (NVConstant (NFloat x), NVConstant (NFloat y)) | y /= 0 -> toValue $                     x / y
      (_x                   , _y                   )         -> throwError $ Division _x _y

anyNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
anyNix f = toValue <=< anyMNix fromValue <=< traverse (callFunc f) <=< fromValue
 where
  anyMNix :: Monad m => (a -> m Bool) -> [a] -> m Bool
  anyMNix _ []       = pure False
  anyMNix p (x : xs) =
    bool
      (anyMNix p xs)
      (pure True)
      =<< p x

allNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
allNix f = toValue <=< allMNix fromValue <=< traverse (callFunc f) <=< fromValue
 where
  allMNix :: Monad m => (a -> m Bool) -> [a] -> m Bool
  allMNix _ []       = pure True
  allMNix p (x : xs) =
    bool
      (pure False)
      (allMNix p xs)
      =<< p x

foldl'Nix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
foldl'Nix f z xs =  foldM go z =<< fromValue @[NValue t f m] xs
 where
  go b a = (`callFunc` a) =<< callFunc f b

headNix :: forall e t f m. MonadNix e t f m => NValue t f m -> m (NValue t f m)
headNix =
  maybe
    (throwError $ ErrorCall "builtins.head: empty list")
    pure
  . viaNonEmpty head <=< fromValue @[NValue t f m]

tailNix :: forall e t f m. MonadNix e t f m => NValue t f m -> m (NValue t f m)
tailNix =
  maybe
    (throwError $ ErrorCall "builtins.tail: empty list")
    (pure . NVList)
  . viaNonEmpty tail <=< fromValue @[NValue t f m]

splitVersionNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
splitVersionNix v =
  do
    version <- fromStringNoContext =<< fromValue v
    pure $
      NVList $
        mkNVStrWithoutContext . show <$>
          splitVersion version

compareVersionsNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
compareVersionsNix t1 t2 =
  do
    s1 <- mkText t1
    s2 <- mkText t2

    let
      cmpVers =
        case compareVersions s1 s2 of
          LT -> -1
          EQ -> 0
          GT -> 1

    pure $ NVConstant $ NInt cmpVers

 where
  mkText = fromStringNoContext <=< fromValue

parseDrvNameNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
parseDrvNameNix drvname =
  do
    s <- fromStringNoContext =<< fromValue drvname

    let
      (name :: Text, version :: Text) = splitDrvName s

    toValue @(AttrSet (NValue t f m)) $
      M.fromList
        [ ( "name" :: VarName
          , mkNVStr name
          )
        , ( "version"
          , mkNVStr version
          )
        ]

 where
  mkNVStr = mkNVStrWithoutContext

matchNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
matchNix pat str =
  do
    p <- fromStringNoContext =<< fromValue pat
    ns <- fromValue str

    -- NOTE: 2018-11-19: Currently prim_match in nix/src/libexpr/primops.cc
    -- ignores the context of its second argument. This is probably a bug but we're
    -- going to preserve the behavior here until it is fixed upstream.
    -- Relevant issue: https://github.com/NixOS/nix/issues/2547
    let
      s  = ignoreContext ns
      re = makeRegex p :: Regex
      mkMatch t =
        bool
          (pure NVNull)
          (toValue $ mkNixStringWithoutContext t)
          (not $ Text.null t)

    case matchOnceText re s of
      Just ("", sarr, "") ->
        do
          let submatches = fst <$> elems sarr
          NVList <$>
            traverse
              mkMatch
              (case submatches of
                 [] -> mempty
                 [a] -> one a
                 _:xs -> xs -- return only the matched groups, drop the full string
              )
      _ -> pure NVNull

splitNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
splitNix pat str =
  do
    p <- fromStringNoContext =<< fromValue pat
    ns <- fromValue str
        -- NOTE: Currently prim_split in nix/src/libexpr/primops.cc ignores the
        -- context of its second argument. This is probably a bug but we're
        -- going to preserve the behavior here until it is fixed upstream.
        -- Relevant issue: https://github.com/NixOS/nix/issues/2547
    let
      s = ignoreContext ns
      regex       = makeRegex p :: Regex
      haystack = encodeUtf8 s

    pure $ NVList $ splitMatches 0 (elems <$> matchAllText regex haystack) haystack

substringNix :: forall e t f m. MonadNix e t f m => Int -> Int -> NixString -> Prim m NixString
substringNix start len str =
  Prim $
    bool
      (throwError $ ErrorCall $ "builtins.substring: negative start position: " <> show start)
      (pure $ modifyNixContents (take . Text.drop start) str)
      (start >= 0)
 where
  take =
    bool
      id  --NOTE: negative values of 'len' are OK, and mean "take everything"
      (Text.take len)
      (len >= 0)

attrNamesNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
attrNamesNix =
    coersion . inHask @(AttrSet (NValue t f m))
      (fmap (mkNixStringWithoutContext . coerce) . sort . M.keys)
 where
  coersion = fmap (coerce :: CoerceDeeperToNValue t f m)

attrValuesNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
attrValuesNix nvattrs =
  do
    attrs <- fromValue @(AttrSet (NValue t f m)) nvattrs
    toValue $
      snd <$>
        sortOn
          (fst @VarName @(NValue t f m))
          (M.toList attrs)

mapNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
mapNix f =
  inHaskM @[NValue t f m]
    (traverse
      (defer
      . withFrame Debug (ErrorCall "While applying f in map:\n")
      . callFunc f
      )
    )

mapAttrsNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
mapAttrsNix f xs =
  do
    nixAttrset <- fromValue @(AttrSet (NValue t f m)) xs
    let
      keyVals = M.toList nixAttrset
      keys = fst <$> keyVals

      applyFunToKeyVal (key, val) =
        do
          runFunForKey <- callFunc f $ mkNVStrWithoutContext (coerce key)
          callFunc runFunForKey val

    newVals <-
      traverse
        (defer @(NValue t f m) . withFrame Debug (ErrorCall "While applying f in mapAttrs:\n") . applyFunToKeyVal)
        keyVals

    toValue $ M.fromList $ zip keys newVals

filterNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
filterNix f =
  inHaskM
    (filterM fh)
 where
  fh :: NValue t f m -> m Bool
  fh = fromValue <=< callFunc f

catAttrsNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
catAttrsNix attrName xs =
  do
    n <- fromStringNoContext =<< fromValue attrName
    l <- fromValue @[NValue t f m] xs

    NVList . catMaybes <$>
      traverse
        (fmap (M.lookup @VarName $ coerce n) . fromValue <=< demand)
        l

baseNameOfNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
baseNameOfNix x =
  do
    ns <- coerceStringlikeToNixString DontCopyToStore x
    pure $
      NVStr $
        modifyNixContents
          (fromString . coerce takeFileName . toString)
          ns

bitAndNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
bitAndNix x y =
  do
    a <- fromValue @Integer x
    b <- fromValue @Integer y

    toValue $ a .&. b

bitOrNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
bitOrNix x y =
  do
    a <- fromValue @Integer x
    b <- fromValue @Integer y

    toValue $ a .|. b

bitXorNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
bitXorNix x y =
  do
    a <- fromValue @Integer x
    b <- fromValue @Integer y

    toValue $ a `xor` b

builtinsBuiltinNix
  :: forall e t f m
   . MonadNix e t f m
  => m (NValue t f m)
builtinsBuiltinNix = throwError $ ErrorCall "HNix does not provide builtins.builtins at the moment. Using builtins directly should be preferred"

-- a safer version of `attrsetGet`
attrGetOr
  :: forall e t f m v a
   . (MonadNix e t f m, FromValue v m (NValue t f m))
  => a
  -> (v -> m a)
  -> VarName
  -> AttrSet (NValue t f m)
  -> m a
attrGetOr fallback fun name attrs =
  maybe
    (pure fallback)
    (fun <=< fromValue)
    (M.lookup name attrs)


--  NOTE: It is a part of the implementation taken from:
--  https://github.com/haskell-nix/hnix/pull/755
--  look there for `sha256` and/or `filterSource`
pathNix :: forall e t f m. MonadNix e t f m => NValue t f m -> m (NValue t f m)
pathNix arg =
  do
    attrs <- fromValue @(AttrSet (NValue t f m)) arg
    path      <- fmap (coerce . toString) $ fromStringNoContext =<< coerceToPath =<< attrsetGet "path" attrs

    -- TODO: Fail on extra args
    -- XXX: This is a very common pattern, we could factor it out
    name      <- toText <$> attrGetOr (takeFileName path) (fmap (coerce . toString) . fromStringNoContext) "name" attrs
    recursive <- attrGetOr True pure "recursive" attrs

    Right (coerce . toText . coerce @StorePath @String -> s) 
      <- addToStore
          name
          (NarFile path)
          (if recursive
           then FileIngestionMethod_FileRecursive
           else FileIngestionMethod_Flat
          )
          RepairMode_DontRepair
    -- TODO: Ensure that s matches sha256 when not empty
    pure $ NVStr $ mkNixStringWithSingletonContext (StringContext DirectPath s) s
 where
  coerceToPath = coerceToString callFunc DontCopyToStore CoerceAny

dirOfNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
dirOfNix nvdir =
  do
    dir <- demand nvdir

    case dir of
      NVStr ns -> pure $ NVStr $ modifyNixContents (fromString . coerce takeDirectory . toString) ns
      NVPath path -> pure $ NVPath $ takeDirectory path
      v -> throwError $ ErrorCall $ "dirOf: expected string or path, got " <> show v

-- jww (2018-04-28): This should only be a string argument, and not coerced?
unsafeDiscardStringContextNix
  :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
unsafeDiscardStringContextNix =
  inHask (mkNixStringWithoutContext . ignoreContext)

-- | Evaluate `a` to WHNF to collect its topmost effect.
seqNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
seqNix a b = b <$ demand a

-- | Evaluate 'a' to NF to collect all of its effects, therefore data cycles are ignored.
deepSeqNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
deepSeqNix a b = b <$ normalForm_ a

elemNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
elemNix x = inHaskM (anyMNix $ valueEqM x)
 where
  anyMNix :: Monad m => (a -> m Bool) -> [a] -> m Bool
  anyMNix p =
    handlePresence
      (pure False)
      (\ (x : xss) ->
        bool
          (anyMNix p xss)
          (pure True)
          =<< p x
      )

elemAtNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
elemAtNix xs n =
  do
    n' <- fromValue n
    xs' <- fromValue xs
    maybe
      (throwError $ ErrorCall $ "builtins.elem: Index " <> show n' <> " too large for list of length " <> show (length xs'))
      pure
      (xs' !!? n')

genListNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
genListNix f nixN =
  do
    n <- fromValue @Integer nixN
    bool
      (throwError $ ErrorCall $ "builtins.genList: Expected a non-negative number, got " <> show n)
      (toValue =<< traverse (defer . callFunc f <=< toValue) [0 .. n - 1])
      (n >= 0)

genericClosureNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
genericClosureNix c =
  do
  s <- fromValue @(AttrSet (NValue t f m)) c

  case (M.lookup "startSet" s, M.lookup "operator" s) of
    (Nothing    , Nothing        ) -> throwError $ ErrorCall "builtins.genericClosure: Attributes 'startSet' and 'operator' required"
    (Nothing    , Just _         ) -> throwError $ ErrorCall "builtins.genericClosure: Attribute 'startSet' required"
    (Just _     , Nothing        ) -> throwError $ ErrorCall "builtins.genericClosure: Attribute 'operator' required"
    (Just startSet, Just operator) ->
      do
        ss <- fromValue @[NValue t f m] =<< demand startSet
        op <- demand operator
        let
          go
            :: Set (WValue t f m)
            -> [NValue t f m]
            -> m (Set (WValue t f m), [NValue t f m])
          go ks []       = pure (ks, mempty)
          go ks (t : ts) =
            do
              v <- demand t
              k <- demand =<< attrsetGet "key" =<< fromValue @(AttrSet (NValue t f m)) v

              bool
                (do
                  checkComparable k $
                    handlePresence
                      k
                      (\ (WValue j:_) -> j)
                      (S.toList ks)

                  (<<$>>) (v :) . go (S.insert (WValue k) ks) . (<>) ts =<< fromValue @[NValue t f m] =<< callFunc op v
                )
                (go ks ts)
                (S.member (WValue k) ks)

        toValue @[NValue t f m] =<< snd <$> go mempty ss

-- | Takes:
-- 1. List of strings to match.
-- 2. List of strings to replace corresponding match occurance. (arg 1 & 2 lists matched by index)
-- 3. String to process
-- -> returns the string with requested replacements.
--
-- Example:
-- builtins.replaceStrings ["ll" "e"] [" " "i"] "Hello world" == "Hi o world".
replaceStringsNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
replaceStringsNix tfrom tto ts =
  do
    -- NixStrings have context - remember
    (fromKeys :: [NixString]) <- fromValue (Deeper tfrom)
    (toVals   :: [NixString]) <- fromValue (Deeper tto)
    (string   ::  NixString ) <- fromValue ts

    when (length fromKeys /= length toVals) $ throwError $ ErrorCall "builtins.replaceStrings: Arguments `from`&`to` construct a key-value map, so the number of their elements must always match."

    let
      --  2021-02-18: NOTE: if there is no match - the process does not changes the context, simply slides along the string.
      --  So it isbe more effective to pass the context as the first argument.
      --  And moreover, the `passOneCharNgo` passively passes the context, to context can be removed from it and inherited directly.
      --  Then the solution would've been elegant, but the Nix bug prevents elegant implementation.
      go ctx input output =
        maybe
            -- Passively pass the chars
          passOneChar
          replace
          maybePrefixMatch

       where
        -- When prefix matched something - returns (match, replacement, remainder)
        maybePrefixMatch :: Maybe (Text, NixString, Text)
        maybePrefixMatch = formMatchReplaceTailInfo <$> find ((`Text.isPrefixOf` input) . fst) fromKeysToValsMap
         where
          formMatchReplaceTailInfo (m, r) = (m, r, Text.drop (Text.length m) input)

          fromKeysToValsMap = zip (ignoreContext <$> fromKeys) toVals

        -- Not passing args => It is constant that gets embedded into `go` => It is simple `go` tail recursion
        passOneChar =
          maybe
            (finish ctx output)  -- The base case - there is no chars left to process -> finish
            (\(c, i) -> go ctx i (output <> Builder.singleton c)) -- If there are chars - pass one char & continue
            (Text.uncons input)  -- chip first char

        --  2021-02-18: NOTE: rly?: toStrict . toLazyText
        --  Maybe `text-builder`, `text-show`?
        finish ctx output = mkNixString ctx (toStrict $ Builder.toLazyText output)

        replace (key, replacementNS, unprocessedInput) = replaceWithNixBug unprocessedInput updatedOutput

         where
          replaceWithNixBug =
            bool
              (go updatedCtx)  -- tail recursion
              -- Allowing match on "" is a inherited bug of Nix,
              -- when "" is checked - it always matches. And so - when it checks - it always insers a replacement, and then process simply passesthrough the char that was under match.
              --
              -- repl> builtins.replaceStrings ["" "e"] [" " "i"] "Hello world"
              -- " H e l l o   w o r l d "
              -- repl> builtins.replaceStrings ["ll" ""] [" " "i"] "Hello world"
              -- "iHie ioi iwioirilidi"
              --  2021-02-18: NOTE: There is no tests for this
              bugPassOneChar  -- augmented recursion
              isNixBugCase

          isNixBugCase = key == mempty

          updatedOutput  = output <> replacement
          updatedCtx     = ctx <> replacementCtx

          replacement    = Builder.fromText $ ignoreContext replacementNS
          replacementCtx = getStringContext replacementNS

          -- The bug modifies the content => bug demands `pass` to be a real function =>
          -- `go` calls `pass` function && `pass` calls `go` function
          -- => mutual recusion case, so placed separately.
          bugPassOneChar input output =
            maybe
              (finish updatedCtx output)  -- The base case - there is no chars left to process -> finish
              (\(c, i) -> go updatedCtx i $ output <> Builder.singleton c) -- If there are chars - pass one char & continue
              (Text.uncons input)  -- chip first char

    toValue $ go (getStringContext string) (ignoreContext string) mempty

removeAttrsNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
removeAttrsNix set v =
  do
    (m, p) <- fromValue @(AttrSet (NValue t f m), PositionSet) set
    (nsToRemove :: [NixString]) <- fromValue $ Deeper v
    (coerce -> toRemove) <- traverse fromStringNoContext nsToRemove
    toValue (fun m toRemove, fun p toRemove)
 where
  fun :: forall k v . (Eq k, Hashable k) => HashMap k v -> [k] -> HashMap k v
  fun = foldl' (flip M.delete)

intersectAttrsNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
intersectAttrsNix set1 set2 =
  do
    (s1, p1) <- fromValue @(AttrSet (NValue t f m), PositionSet) set1
    (s2, p2) <- fromValue @(AttrSet (NValue t f m), PositionSet) set2

    pure $ NVSet (p2 `M.intersection` p1) (s2 `M.intersection` s1)

functionArgsNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
functionArgsNix nvfun =
  do
    fun <- demand nvfun
    case fun of
      NVClosure p _ ->
        toValue @(AttrSet (NValue t f m)) $ NVBool <$>
          case p of
            Param name     -> one (name, False)
            ParamSet _ _ pset -> isJust <$> M.fromList pset
      _v -> throwError $ ErrorCall $ "builtins.functionArgs: expected function, got " <> show _v

toFileNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
toFileNix name s =
  do
    name' <- fromStringNoContext =<< fromValue name
    s'    <- fromValue s
    mres  <-
      toFile_
        (coerce $ toString name')
        (ignoreContext s')

    let
      storepath  = coerce (fromString @Text) mres
      sc = StringContext DirectPath storepath

    toValue $ mkNixStringWithSingletonContext sc storepath

toPathNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
toPathNix = inHask @Path id

pathExistsNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
pathExistsNix nvpath =
  do
    path <- demand nvpath
    toValue =<<
      case path of
        NVPath p  -> doesPathExist p
        NVStr  ns -> doesPathExist $ coerce $ toString $ ignoreContext ns
        _v -> throwError $ ErrorCall $ "builtins.pathExists: expected path, got " <> show _v

isPathNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isPathNix = hasKind @Path

isAttrsNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isAttrsNix = hasKind @(AttrSet (NValue t f m))

isListNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isListNix = hasKind @[NValue t f m]

isIntNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isIntNix = hasKind @Int

isFloatNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isFloatNix = hasKind @Float

isBoolNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isBoolNix = hasKind @Bool

isNullNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
isNullNix = hasKind @()

-- isString cannot use `hasKind` because it coerces derivationNixs to strings.
isStringNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
isStringNix nv =
  do
    v <- demand nv

    toValue $
      case v of
        NVStr{} -> True
        _       -> False

isFunctionNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
isFunctionNix nv =
  do
    v <- demand nv

    toValue $
      case v of
        NVClosure{} -> True
        _           -> False

throwNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
throwNix =
  throwError . ErrorCall . toString . ignoreContext
    <=< coerceStringlikeToNixString CopyToStore

-- | Implementation of Nix @import@ clause.
--
-- Because Nix @import@s work strictly
-- (import gets fully evaluated befor bringing it into the scope it was called from)
-- - that property raises a requirement for execution phase of the interpreter go into evaluation phase
-- & then also go into parsing phase on the imports.
-- So it is not possible (more precise - not practical) to do a full parse Nix code phase fully & then go into evaluation phase.
-- As it is not possible to "import them lazily", as import is strict & it is not possible to establish
-- what imports whould be needed up until where it would be determined & they import strictly
--
importNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
importNix = scopedImportNix $ NVSet mempty mempty

-- | @scopedImport scope path@
-- An undocumented secret powerful function.
--
-- At the same time it is strongly forbidden to be used, as prolonged use of it would bring devastating consequences.
-- As it is essentially allows rewriting(redefinition) paradigm.
--
-- Allows to import the environment into the scope of a file expression that gets imported.
-- It is as if the contents at @path@ were given to @import@ wrapped as: @with scope; path@
-- meaning:
--
-- > -- Nix pseudocode:
-- > import (with scope; path)
--
-- For example, it allows to use itself as:
-- > bar = scopedImport pkgs ./bar.nix;
-- > -- & declare @./bar.nix@ without a header, so as:
-- > stdenv.mkDerivation { ... buildInputs = [ libfoo ]; }
--
-- But that breaks the evaluation/execution sharing of the @import@s.
--
-- Function also allows to redefine or extend the builtins.
--
-- For instance, to trace all calls to function ‘map’:
--
-- >  let
-- >    overrides = {
-- >      map = f: xs: builtins.trace "call of map!" (map f xs);
--
-- >      # Propagate override by calls to import&scopedImport.
-- >      import = fn: scopedImport overrides fn;
-- >      scopedImport = attrs: fn: scopedImport (overrides // attrs) fn;
--
-- >      # Update ‘builtins’.
-- >      builtins = builtins // overrides;
-- >    };
-- >  in scopedImport overrides ./bla.nix
--
-- In the related matter the function can be added and passed around as builtin.
scopedImportNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
scopedImportNix asetArg pathArg =
  do
    (coerce -> scope) <- fromValue @(AttrSet (NValue t f m)) asetArg
    p <- fromValue pathArg

    path  <- pathToDefaultNix @t @f @m p
    path' <-
      maybe
        (do
          traceM "No known current directory"
          pure path
        )
        (\ res ->
          do
            p' <- fromValue @Path =<< demand res

            traceM $ "Current file being evaluated is: " <> show p'
            pure $ takeDirectory p' </> path
        )
        =<< lookupVar "__cur_file"

    clearScopes @(NValue t f m)
      $ withNixContext (pure path')
      $ pushScope scope
      $ importPath @t @f @m path'

getEnvNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
getEnvNix v =
  (toValue . mkNixStringWithoutContext . maybeToMonoid) =<< getEnvVar =<< fromStringNoContext =<< fromValue v

sortNix
  :: forall e t f m
  . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
sortNix comp =
  inHaskM (sortByM cmp)
 where
  cmp :: NValue t f m -> NValue t f m -> m Ordering
  cmp a b =
    bool
      (fmap
         (bool EQ GT)
         (compare b a)
      )
      (pure LT)
      =<< compare a b
   where
    compare :: NValue t f m -> NValue t f m -> m Bool
    compare a2 a1 = fromValue =<< (`callFunc` a1) =<< callFunc comp a2

lessThanNix
  :: MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
lessThanNix ta tb =
  do
    va <- demand ta
    vb <- demand tb

    let
      badType = throwError $ ErrorCall $ "builtins.lessThan: expected two numbers or two strings, got '" <> show va <> "' and '" <> show vb <> "'."

    NVBool <$>
      case (va, vb) of
        (NVConstant ca, NVConstant cb) ->
          case (ca, cb) of
            (NInt   a, NInt   b) -> pure $             a < b
            (NInt   a, NFloat b) -> pure $ fromInteger a < b
            (NFloat a, NInt   b) -> pure $             a < fromInteger b
            (NFloat a, NFloat b) -> pure $             a < b
            _                    -> badType
        (NVStr a, NVStr b) -> pure $ ignoreContext a < ignoreContext b
        _ -> badType

-- | Helper function, generalization of @concat@ operations.
concatWith
  :: forall e t f m
   . MonadNix e t f m
  => (NValue t f m -> m (NValue t f m))
  -> NValue t f m
  -> m (NValue t f m)
concatWith f =
  toValue .
    concat <=<
      traverse
        (fromValue @[NValue t f m] <=< f)
        <=< fromValue @[NValue t f m]

-- | Nix function of Haskell:
-- > concat :: [[a]] -> [a]
--
-- Concatenate a list of lists into a single list.
concatListsNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
concatListsNix = concatWith demand

-- | Nix function of Haskell:
-- > concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMapNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
concatMapNix f = concatWith (callFunc f)

listToAttrsNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
listToAttrsNix lst =
  do
    l <- fromValue @[NValue t f m] lst
    fmap
      (NVSet mempty . M.fromList . reverse)
      (traverse
        (\ nvattrset ->
          do
            a <- fromValue @(AttrSet (NValue t f m)) =<< demand nvattrset
            (coerce -> name) <- fromStringNoContext =<< fromValue =<< demand =<< attrsetGet "name" a
            val  <- attrsetGet "value" a

            pure (name, val)
        )
        l
      )

-- prim_hashString from nix/src/libexpr/primops.cc
-- fail if context in the algo arg
-- propagate context from the s arg
-- | The result coming out of hashString is base16 encoded
hashStringNix
  :: forall e t f m. MonadNix e t f m => NixString -> NixString -> Prim m NixString
hashStringNix nsAlgo ns =
  Prim $
    do
      algo <- fromStringNoContext nsAlgo
      let
        f g = pure $ modifyNixContents g ns

      case algo of
        --  2021-03-04: Pattern can not be taken-out because hashes represented as different types
        "md5"    -> f (show . mkHash @MD5.MD5)
        "sha1"   -> f (show . mkHash @SHA1.SHA1)
        "sha256" -> f (show . mkHash @SHA256.SHA256)
        "sha512" -> f (show . mkHash @SHA512.SHA512)

        _ -> throwError $ ErrorCall $ "builtins.hashString: expected \"md5\", \"sha1\", \"sha256\", or \"sha512\", got " <> show algo

       where
        -- This intermidiary `a` is only needed because of the type application
        mkHash :: (Show a, HashAlgorithm a) => Text -> a
        mkHash s = hash $ encodeUtf8 s


-- | hashFileNix
-- use hashStringNix to hash file content
hashFileNix
  :: forall e t f m . MonadNix e t f m => NixString -> Path -> Prim m NixString
hashFileNix nsAlgo nvfilepath = Prim $ hash =<< fileContent
 where
  hash = outPrim . hashStringNix nsAlgo
  outPrim (Prim x) = x
  fileContent :: m NixString
  fileContent = mkNixStringWithoutContext <$> Nix.Render.readFile nvfilepath


-- | groupByNix
-- Groups elements of list together by the string returned from the function f called on 
-- each element. It returns an attribute set where each attribute value contains the 
-- elements of list that are mapped to the same corresponding attribute name returned by f.
groupByNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
groupByNix nvfun nvlist = do
  list   <- demand nvlist
  fun    <- demand nvfun
  (f, l) <- extractP (fun, list)
  NVSet mempty
    .   fmap (NVList . reverse)
    .   M.fromListWith (<>)
    <$> traverse (app f) l
 where
  app f x = do
    name <- fromValue @Text =<< f x
    pure (VarName name, one x)
  extractP (NVBuiltin _ f, NVList l) = pure (f, l)
  extractP (NVClosure _ f, NVList l) = pure (f, l)
  extractP _v =
    throwError
      $  ErrorCall
      $  "builtins.groupBy: expected function and list, got "
      <> show _v


placeHolderNix :: forall t f m e . MonadNix e t f m => NValue t f m -> m (NValue t f m)
placeHolderNix p =
  do
    t <- fromStringNoContext =<< fromValue p
    h <-
      coerce @(Prim m NixString) @(m NixString) $
        (hashStringNix `on` mkNixStringWithoutContext)
          "sha256"
          ("nix-output:" <> t)
    toValue
      $ mkNixStringWithoutContext
      $ Text.cons '/'
      $ Base32.encode
      -- Please, stop Text -> Bytestring here after migration to Text
      $ case Base16.decode (bytes h) of -- The result coming out of hashString is base16 encoded
#if MIN_VERSION_base16_bytestring(1,0,0)
        -- Please, stop Text -> String here after migration to Text
        Left e -> error $ "Couldn't Base16 decode the text: '" <> body h <> "'.\nThe Left fail content: '" <> show e <> "'."
        Right d -> d
#else
        (d, "") -> d
        (_, e) -> error $ "Couldn't Base16 decode the text: '" <> body h <> "'.\nUndecodable remainder: '" <> show e <> "'."
#endif
    where
      bytes :: NixString -> ByteString
      bytes = encodeUtf8 . body

      body = ignoreContext

readFileNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
readFileNix = toValue <=< Nix.Render.readFile <=< absolutePathFromValue <=< demand

findFileNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
findFileNix nvaset nvfilepath =
  do
    aset <- demand nvaset
    filePath <- demand nvfilepath

    case (aset, filePath) of
      (NVList x, NVStr ns) ->
        do
          mres <- findPath @t @f @m x $ coerce $ toString $ ignoreContext ns

          pure $ NVPath mres

      (NVList _, _y     ) -> throwError $ ErrorCall $ "expected a string, got " <> show _y
      (_x      , NVStr _) -> throwError $ ErrorCall $ "expected a list, got " <> show _x
      (_x      , _y     ) -> throwError $ ErrorCall $ "Invalid types for builtins.findFile: " <> show (_x, _y)

readDirNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
readDirNix nvpath =
  do
    path           <- absolutePathFromValue =<< demand nvpath
    items          <- listDirectory path

    let
      -- | Function indeed binds filepaths as keys ('VarNames') in Nix attrset.
      detectFileTypes :: Path -> m (VarName, FileType)
      detectFileTypes item =
        do
          s <- getSymbolicLinkStatus $ path </> item
          let
            t =
              if
                | isRegularFile s  -> FileTypeRegular
                | isDirectory s    -> FileTypeDirectory
                | isSymbolicLink s -> FileTypeSymlink
                | otherwise        -> FileTypeUnknown

          pure (coerce @(String -> Text) fromString item, t)

    itemsWithTypes <-
      traverse
        detectFileTypes
        items

    (coerce :: CoerceDeeperToNValue t f m) <$> toValue (M.fromList itemsWithTypes)

fromJSONNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
fromJSONNix nvjson =
  do
    j <- demand nvjson
    jText <- fromStringNoContext =<< fromValue j

    either
      (\ jsonError -> throwError $ ErrorCall $ "builtins.fromJSON: " <> jsonError)
      jsonToNValue
      -- do we really need to marshall Text -> ByteString -> Aeson.Value (that is a Text)
      (A.eitherDecodeStrict' @A.Value $ encodeUtf8 jText)

 where
  jsonToNValue :: (A.Value -> m (NValue t f m))
  jsonToNValue =
    \case
      A.Object m ->
        traverseToNValue
          (NVSet mempty)
#if MIN_VERSION_aeson(2,0,0)
          (M.mapKeys (coerce . AKM.toText)  $ AKM.toHashMap m)
#else
          (M.mapKeys coerce m)
#endif
      A.Array  l -> traverseToNValue NVList (V.toList l)
      A.String s -> pure $ mkNVStrWithoutContext s
      A.Number n ->
        pure $
          NVConstant $
            either
              NFloat
              NInt
              (floatingOrInteger n)
      A.Bool   b -> pure $ NVBool b
      A.Null     -> pure NVNull
   where
    traverseToNValue :: Traversable t0 => (t0 (NValue t f m) -> b) -> t0 A.Value -> m b
    traverseToNValue f v = f <$> traverse jsonToNValue v

toJSONNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
toJSONNix = (fmap NVStr . toJSONNixString) <=< demand

toXMLNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
toXMLNix = (fmap (NVStr . toXML) . normalForm) <=< demand

typeOfNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
typeOfNix nvv =
  do
    v <- demand nvv
    let
      detectType =
        case v of
          NVConstant a ->
            case a of
              NURI   _ -> "string"
              NInt   _ -> "int"
              NFloat _ -> "float"
              NBool  _ -> "bool"
              NNull    -> "null"
          NVStr     _   -> "string"
          NVList    _   -> "list"
          NVSet     _ _ -> "set"
          NVClosure{}   -> "lambda"
          NVPath    _   -> "path"
          NVBuiltin _ _ -> "lambda"
          _             -> error "Pattern synonyms obscure complete patterns"

    toValue $ mkNixStringWithoutContext detectType

tryEvalNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
tryEvalNix e = (`catch` (pure . onError))
  (onSuccess <$> demand e)
 where
  onSuccess v =
    NVSet
      mempty
      $ M.fromList
        [ ("success", NVBool True)
        , ("value"  , v            )
        ]

  onError :: SomeException -> NValue t f m
  onError _ =
    NVSet
      mempty
      $ M.fromList
        $ (, NVBool False) <$>
          [ "success"
          , "value"
          ]

traceNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
traceNix msg action =
  do
    traceEffect @t @f @m . toString . ignoreContext =<< fromValue msg
    pure action

-- Please, can function remember fail context
addErrorContextNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m  -- action
  -> m (NValue t f m)
addErrorContextNix _ = pure

execNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
execNix xs =
  -- 2018-11-19: NOTE: Still need to do something with the context here
  -- See prim_exec in nix/src/libexpr/primops.cc
  -- Requires the implementation of EvalState::realiseContext
  (exec . fmap ignoreContext) =<< traverse (coerceStringlikeToNixString DontCopyToStore) =<< fromValue @[NValue t f m] xs

fetchurlNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
fetchurlNix =
  (\case
    NVSet _ s -> fetch (M.lookup "sha256" s) =<< demand =<< attrsetGet "url" s
    v@NVStr{} -> fetch Nothing v
    v -> throwError $ ErrorCall $ "builtins.fetchurl: Expected URI or set, got " <> show v
  ) <=< demand

 where
  --  2022-01-21: NOTE: Needs to check the hash match.
  fetch :: Maybe (NValue t f m) -> NValue t f m -> m (NValue t f m)
  fetch _msha =
    \case
      NVStr ns ->
        either -- msha
          throwError
          toValue
          =<< getURL
            =<< maybe
              (throwError $ ErrorCall "builtins.fetchurl: unsupported arguments to url")
              pure
              (getStringNoContext ns)

      v -> throwError $ ErrorCall $ "builtins.fetchurl: Expected URI or string, got " <> show v

partitionNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
partitionNix f nvlst =
  do
    let
      match t = (, t) <$> (fromValue =<< callFunc f t)
    selection <- traverse match =<< fromValue @[NValue t f m] nvlst

    let
      (right, wrong) = partition fst selection
      makeSide       = NVList . fmap snd

    toValue @(AttrSet (NValue t f m))
      $ M.fromList
          [ ("right", makeSide right)
          , ("wrong", makeSide wrong)
          ]

currentSystemNix :: MonadNix e t f m => m (NValue t f m)
currentSystemNix =
  do
    os   <- getCurrentSystemOS
    arch <- getCurrentSystemArch

    pure $ mkNVStrWithoutContext $ arch <> "-" <> os

currentTimeNix :: MonadNix e t f m => m (NValue t f m)
currentTimeNix =
  do
    opts <- askOptions
    toValue @Integer $ round $ Time.utcTimeToPOSIXSeconds $ getTime opts

derivationStrictNix :: MonadNix e t f m => NValue t f m -> m (NValue t f m)
derivationStrictNix = derivationStrict

getRecursiveSizeNix :: (MonadIntrospect m, NVConstraint f) => a -> m (NValue t f m)
getRecursiveSizeNix = fmap (NVConstant . NInt . fromIntegral) . recursiveSize

getContextNix
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
getContextNix =
  \case
    (NVStr ns) ->
      NVSet mempty <$> traverseToValue (getNixLikeContext $ toNixLikeContext $ getStringContext ns)
    x -> throwError $ ErrorCall $ "Invalid type for builtins.getContext: " <> show x
  <=< demand

appendContextNix
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
appendContextNix tx ty =
  do
    x <- demand tx
    y <- demand ty

    case (x, y) of
      (NVStr ns, NVSet _ attrs) ->
        do
          let
            getPathNOuts :: NValue t f m -> m NixLikeContextValue
            getPathNOuts tx =
              do
                x <- demand tx

                case x of
                  NVSet _ atts ->
                    do
                      -- TODO: Fail for unexpected keys.

                      let
                        getK :: VarName -> m Bool
                        getK k =
                          maybe
                            (pure False)
                            (fromValue <=< demand)
                            $ M.lookup k atts

                        getOutputs :: m [Text]
                        getOutputs =
                          maybe
                            stub
                            (\ touts ->
                              do
                                outs <- demand touts

                                case outs of
                                  NVList vs -> traverse (fmap ignoreContext . fromValue) vs
                                  _x -> throwError $ ErrorCall $ "Invalid types for context value outputs in builtins.appendContext: " <> show _x
                            )
                            (M.lookup "outputs" atts)

                      path <- getK "path"
                      allOutputs <- getK "allOutputs"

                      NixLikeContextValue path allOutputs <$> getOutputs

                  _x -> throwError $ ErrorCall $ "Invalid types for context value in builtins.appendContext: " <> show _x
            addContext :: HashMap VarName NixLikeContextValue -> NixString
            addContext newContextValues =
              mkNixString
                (fromNixLikeContext $
                  NixLikeContext $
                    M.unionWith
                      (<>)
                      newContextValues
                      $ getNixLikeContext $
                          toNixLikeContext $
                            getStringContext ns
                )
                $ ignoreContext ns

          toValue . addContext =<< traverse getPathNOuts attrs

      _xy -> throwError $ ErrorCall $ "Invalid types for builtins.appendContext: " <> show _xy


nixVersionNix :: MonadNix e t f m => m (NValue t f m)
nixVersionNix = toValue $ mkNixStringWithoutContext "2.3"

langVersionNix :: MonadNix e t f m => m (NValue t f m)
langVersionNix = toValue (5 :: Int)

-- ** @builtinsList@

builtinsList :: forall e t f m . MonadNix e t f m => m [Builtin (NValue t f m)]
builtinsList =
  sequenceA
    [ add  TopLevel "abort"            throwNix -- for now
    , add  TopLevel "baseNameOf"       baseNameOfNix
    , add0 TopLevel "derivation"       derivationNix
    , add  TopLevel "derivationStrict" derivationStrictNix
    , add  TopLevel "dirOf"            dirOfNix
    , add  TopLevel "import"           importNix
    , add  TopLevel "isNull"           isNullNix
    , add2 TopLevel "map"              mapNix
    , add2 TopLevel "mapAttrs"         mapAttrsNix
    , add  TopLevel "placeholder"      placeHolderNix
    , add2 TopLevel "removeAttrs"      removeAttrsNix
    , add2 TopLevel "scopedImport"     scopedImportNix
    , add  TopLevel "throw"            throwNix
    , add  TopLevel "toString"         toStringNix
    , add2 TopLevel "trace"            traceNix
    , add0 Normal   "nixVersion"       nixVersionNix
    , add0 Normal   "langVersion"      langVersionNix
    , add2 Normal   "add"              addNix
    , add2 Normal   "addErrorContext"  addErrorContextNix
    , add2 Normal   "all"              allNix
    , add2 Normal   "any"              anyNix
    , add2 Normal   "appendContext"    appendContextNix
    , add  Normal   "attrNames"        attrNamesNix
    , add  Normal   "attrValues"       attrValuesNix
    , add2 Normal   "bitAnd"           bitAndNix
    , add2 Normal   "bitOr"            bitOrNix
    , add2 Normal   "bitXor"           bitXorNix
    , add0 Normal   "builtins"         builtinsBuiltinNix
    , add2 Normal   "catAttrs"         catAttrsNix
    , add' Normal   "ceil"             (arity1 (ceiling @Float @Integer))
    , add2 Normal   "compareVersions"  compareVersionsNix
    , add  Normal   "concatLists"      concatListsNix
    , add2 Normal   "concatMap"        concatMapNix
    , add' Normal   "concatStringsSep" (arity2 intercalateNixString)
    , add0 Normal   "currentSystem"    currentSystemNix
    , add0 Normal   "currentTime"      currentTimeNix
    , add2 Normal   "deepSeq"          deepSeqNix
    , add2 Normal   "div"              divNix
    , add2 Normal   "elem"             elemNix
    , add2 Normal   "elemAt"           elemAtNix
    , add  Normal   "exec"             execNix
    , add0 Normal   "false"            (pure $ NVBool False)
    --, add  Normal   "fetchGit"         fetchGit
    --, add  Normal   "fetchMercurial"   fetchMercurial
    , add  Normal   "fetchTarball"     fetchTarball
    , add  Normal   "fetchurl"         fetchurlNix
    , add2 Normal   "filter"           filterNix
    --, add  Normal   "filterSource"     filterSource
    , add2 Normal   "findFile"         findFileNix
    , add' Normal   "floor"            (arity1 (floor @Float @Integer))
    , add3 Normal   "foldl'"           foldl'Nix
    , add  Normal   "fromJSON"         fromJSONNix
    --, add  Normal   "fromTOML"         fromTOML
    , add  Normal   "functionArgs"     functionArgsNix
    , add  Normal   "genericClosure"   genericClosureNix
    , add2 Normal   "genList"          genListNix
    , add2 Normal   "getAttr"          getAttrNix
    , add  Normal   "getContext"       getContextNix
    , add  Normal   "getEnv"           getEnvNix
    , add2 Normal   "groupBy"          groupByNix
    , add2 Normal   "hasAttr"          hasAttrNix
    , add  Normal   "hasContext"       hasContextNix
    , add' Normal   "hashString"       (hashStringNix @e @t @f @m)
    , add' Normal   "hashFile"         hashFileNix
    , add  Normal   "head"             headNix
    , add2 Normal   "intersectAttrs"   intersectAttrsNix
    , add  Normal   "isAttrs"          isAttrsNix
    , add  Normal   "isBool"           isBoolNix
    , add  Normal   "isFloat"          isFloatNix
    , add  Normal   "isFunction"       isFunctionNix
    , add  Normal   "isInt"            isIntNix
    , add  Normal   "isList"           isListNix
    , add  Normal   "isString"         isStringNix
    , add  Normal   "isPath"           isPathNix
    , add  Normal   "length"           lengthNix
    , add2 Normal   "lessThan"         lessThanNix
    , add  Normal   "listToAttrs"      listToAttrsNix
    , add2 Normal   "match"            matchNix
    , add2 Normal   "mul"              mulNix
    , add0 Normal   "nixPath"          nixPathNix
    , add0 Normal   "null"             (pure NVNull)
    , add  Normal   "parseDrvName"     parseDrvNameNix
    , add2 Normal   "partition"        partitionNix
    , add  Normal   "path"             pathNix
    , add  Normal   "pathExists"       pathExistsNix
    , add  Normal   "readDir"          readDirNix
    , add  Normal   "readFile"         readFileNix
    , add3 Normal   "replaceStrings"   replaceStringsNix
    , add2 Normal   "seq"              seqNix
    , add2 Normal   "sort"             sortNix
    , add2 Normal   "split"            splitNix
    , add  Normal   "splitVersion"     splitVersionNix
    , add0 Normal   "storeDir"         (pure $ mkNVStrWithoutContext "/nix/store")
    --, add  Normal   "storePath"        storePath
    , add' Normal   "stringLength"     (arity1 $ Text.length . ignoreContext)
    , add' Normal   "sub"              (arity2 ((-) @Integer))
    , add' Normal   "substring"        substringNix
    , add  Normal   "tail"             tailNix
    , add2 Normal   "toFile"           toFileNix
    , add  Normal   "toJSON"           toJSONNix
    , add  Normal   "toPath"           toPathNix -- Deprecated in Nix: https://github.com/NixOS/nix/pull/2524
    , add  Normal   "toXML"            toXMLNix
    , add0 Normal   "true"             (pure $ NVBool True)
    , add  Normal   "tryEval"          tryEvalNix
    , add  Normal   "typeOf"           typeOfNix
    , add  Normal   "unsafeDiscardOutputDependency" unsafeDiscardOutputDependencyNix
    , add  Normal   "unsafeDiscardStringContext"    unsafeDiscardStringContextNix
    , add2 Normal   "unsafeGetAttrPos"              unsafeGetAttrPosNix
    , add  Normal   "valueSize"        getRecursiveSizeNix
    ]
 where

  arity0 :: a -> Prim m a
  arity0 = Prim . pure

  arity1 :: (a -> b) -> (a -> Prim m b)
  arity1 g = arity0 . g

  arity2 :: (a -> b -> c) -> (a -> b -> Prim m c)
  arity2 f = arity1 . f

  mkBuiltin :: BuiltinType -> VarName -> m (NValue t f m) -> m (Builtin (NValue t f m))
  mkBuiltin t n v = wrap t n <$> mkThunk n v
   where
    wrap :: BuiltinType -> VarName -> v -> Builtin v
    wrap t n f = Builtin t (n, f)

    mkThunk :: VarName -> m (NValue t f m) -> m (NValue t f m)
    mkThunk n = defer . withFrame Info (ErrorCall $ "While calling builtin " <> toString n <> "\n")

  hAdd
    :: ( VarName
      -> fun
      -> m (NValue t f m)
      )
    -> BuiltinType
    -> VarName
    -> fun
    -> m (Builtin (NValue t f m))
  hAdd f t n v = mkBuiltin t n $ f n v

  add0
    :: BuiltinType
    -> VarName
    -> m (NValue t f m)
    -> m (Builtin (NValue t f m))
  add0 = hAdd (\ _ x -> x)

  add
    :: BuiltinType
    -> VarName
    -> ( NValue t f m
      -> m (NValue t f m)
      )
    -> m (Builtin (NValue t f m))
  add = hAdd builtin

  add2
    :: BuiltinType
    -> VarName
    -> ( NValue t f m
      -> NValue t f m
      -> m (NValue t f m)
      )
    -> m (Builtin (NValue t f m))
  add2 = hAdd builtin2

  add3
    :: BuiltinType
    -> VarName
    -> ( NValue t f m
      -> NValue t f m
      -> NValue t f m
      -> m (NValue t f m)
      )
    -> m (Builtin (NValue t f m))
  add3 = hAdd builtin3

  add'
    :: ToBuiltin t f m a
    => BuiltinType
    -> VarName
    -> a
    -> m (Builtin (NValue t f m))
  add' = hAdd (toBuiltin . coerce)


-- * Exported

-- | Evaluate expression in the default context.
withNixContext
  :: forall e t f m r
   . (MonadNix e t f m, Has e Options)
  => Maybe Path
  -> m r
  -> m r
withNixContext mpath action =
  do
    base <- builtins
    opts <- askOptions

    pushScope
      (one ("__includes", NVList $ mkNVStrWithoutContext . fromString . coerce <$> getInclude opts))
      (pushScopes
        base $
        maybe
          id
          (\ path act ->
            do
              traceM $ "Setting __cur_file = " <> show path
              pushScope (one ("__cur_file", NVPath path)) act
          )
          mpath
          action
      )

builtins
  :: forall e t f m
  . ( MonadNix e t f m
     , Scoped (NValue t f m) m
     )
  => m (Scopes m (NValue t f m))
builtins =
  do
    ref <- defer $ NVSet mempty <$> buildMap
    (`pushScope` askScopes) . coerce . M.fromList . (one ("builtins", ref) <>) =<< topLevelBuiltins
 where
  buildMap :: m (HashMap VarName (NValue t f m))
  buildMap         =  M.fromList . (mapping <$>) <$> builtinsList

  topLevelBuiltins :: m [(VarName, NValue t f m)]
  topLevelBuiltins = mapping <<$>> fullBuiltinsList

  fullBuiltinsList :: m [Builtin (NValue t f m)]
  fullBuiltinsList = nameBuiltins <<$>> builtinsList
   where
    nameBuiltins :: Builtin v -> Builtin v
    nameBuiltins b@(Builtin TopLevel _) = b
    nameBuiltins (Builtin Normal nB) =
      Builtin TopLevel $ first (coerce @(Text -> Text) ("__" <>)) nB

