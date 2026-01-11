{-# language CPP #-}
{-# language DataKinds #-}
{-# language ExistentialQuantification #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language Strict #-}
{-# language TypeApplications #-}

module Nix.Effects.Derivation ( defaultDerivationStrict ) where

import           Nix.Prelude             hiding ( readFile )
import           Data.ByteArray                 ( convert )
import           Data.ByteArray.Encoding        ( Base(Base16), convertToBase )
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Data.Char                      ( isAscii
                                                , isAlphaNum
                                                )
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import           Data.Foldable                  ( foldl )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified Data.ByteString               as B
import qualified System.Directory              as Directory
import qualified System.FilePath               as FP
import qualified System.IO.Temp                as Temp
import           System.Exit                    ( ExitCode(..) )
import           System.Process                 ( readProcessWithExitCode )
import qualified System.PosixCompat.Files      as Posix

import           Text.Megaparsec
import           Text.Megaparsec.Char

import qualified Crypto.Hash                   as Hash

import           Nix.Atoms
import           Nix.Expr.Types          hiding ( Recursive )
import           Nix.Convert
import           Nix.Effects
import           Nix.Exec                       ( MonadNix
                                                , callFunc
                                                )
import           Nix.Context                    ( askEvalStats )
import           Nix.EvalStats                  ( recordDrvRead
                                                , recordHttpFetch
                                                , recordStoreAdd
                                                )
import qualified GHC.Clock                     as Clock
import           Nix.Options
import           Nix.Frames
import           Nix.Json                       ( toJSONNixString )
import           Nix.Render
import           Nix.String
import           Nix.String.Coerce
import           Nix.Value
import           Nix.Value.Monad

import qualified System.Nix.Hash               as Store
import qualified System.Nix.StorePath          as Store
import qualified System.Nix.Store.ReadOnly     as StoreRO
import qualified System.Nix.Nar                as Nar
import           System.Nix.ContentAddress      ( ContentAddressMethod(..) )
import           Data.Dependent.Sum             ( DSum(..) )
import qualified Data.Dependent.Sum            as DSum
import qualified Control.Monad.State           as State

-- Type for fixed-output derivation hash digest
-- Digest comes from crypton (Crypto.Hash.Digest)
type HashDigest = DSum Store.HashAlgo Hash.Digest

-- Helper functions to work with HashDigest
hashDigestAlgoText :: HashDigest -> Text
hashDigestAlgoText (hashAlgo DSum.:=> _) = Store.algoToText hashAlgo

hashDigestText :: HashDigest -> Text
hashDigestText (_ DSum.:=> hashDigest) = Store.encodeDigestWith Store.Base16 hashDigest

-- | Compute the hash of content using the specified mode and algorithm
-- For Flat mode: hash the file bytes directly
-- For Recursive mode: compute NAR hash of the path
computeContentHash :: MonadIO m => HashMode -> HashDigest -> FilePath -> m HashDigest
computeContentHash mode expectedHash path = liftIO $ case expectedHash of
  (Store.HashAlgo_SHA256 DSum.:=> _) -> case mode of
    Flat -> do
      bytes <- B.readFile path
      pure $ Store.HashAlgo_SHA256 DSum.:=> Hash.hash bytes
    Recursive -> do
      digest <- computeNarHashSHA256 path
      pure $ Store.HashAlgo_SHA256 DSum.:=> digest
  -- For other algorithms, we'd need to add similar cases
  -- For now, error on unsupported algorithms
  _ -> error $ "computeContentHash: unsupported hash algorithm: " <> show (hashDigestAlgoText expectedHash)
 where
  -- Compute NAR hash using SHA256 (streaming to avoid loading entire NAR into memory)
  computeNarHashSHA256 :: FilePath -> IO (Hash.Digest Hash.SHA256)
  computeNarHashSHA256 p =
    Hash.hashFinalize
      <$> State.execStateT
            (Nar.streamNarIO Nar.narEffectsIO p updateHash)
            (Hash.hashInit @Hash.SHA256)
   where
    updateHash chunk = State.modify (\ctx -> Hash.hashUpdate ctx chunk)

--  2021-07-17: NOTE: Derivation consists of @"keys"@ @"vals"@ (of text), so underlining type boundary currently stops here.
data Derivation = Derivation
  { name :: Text
  , outputs :: Map Text Text
  , inputs :: (Set Text, Map Text [Text])
  , platform :: Text
  , builder :: Text -- should be typed as a store path
  , args :: [ Text ]
  , env :: Map Text Text
  , mFixed :: Maybe HashDigest  -- Opaque type for hash digest
  , hashMode :: HashMode
  , useJson :: Bool
  }
  deriving Show

data HashMode = Flat | Recursive
  deriving (Show, Eq)

-- | Configuration for builtin:fetchurl builder
data BuiltinFetchurlConfig = BuiltinFetchurlConfig
  { bfUrl :: !Text
  , bfName :: !Text
  , bfExecutable :: !Bool
  , bfUnpack :: !Bool
  , bfExpectedHash :: !(Maybe HashDigest)
  , bfHashMode :: !HashMode
  }
  deriving Show

-- | Parse configuration from derivation environment for builtin:fetchurl
parseBuiltinFetchurlConfig :: Derivation -> Either Text BuiltinFetchurlConfig
parseBuiltinFetchurlConfig drv = do
  let envMap = env drv

  -- URL is required
  url <- maybe (Left "builtin:fetchurl: missing 'url' in environment") Right
    $ Map.lookup "url" envMap

  when (Text.null url) $
    Left "builtin:fetchurl: 'url' is empty"

  -- Parse boolean environment variables (Nix uses "1" for true, "" or missing for false)
  let parseBool key = case Map.lookup key envMap of
        Nothing -> False
        Just "" -> False
        Just "0" -> False
        Just "1" -> True
        Just "true" -> True
        Just _ -> False  -- Conservative default

  pure $ BuiltinFetchurlConfig
    { bfUrl = url
    , bfName = name drv
    , bfExecutable = parseBool "executable"
    , bfUnpack = parseBool "unpack"
    , bfExpectedHash = mFixed drv
    , bfHashMode = hashMode drv
    }

makeStorePathName :: (Framed e m) => Text -> m Store.StorePathName
makeStorePathName name = case Store.mkStorePathName name of
  Left err -> throwError $ ErrorCall $ "Invalid name '" <> show name <> "' for use in a store path: " <> show err
  Right spname -> pure spname

storeDirFromOptions :: forall e m. (MonadReader e m, Has e Options) => m Store.StoreDir
storeDirFromOptions = do
  opts <- askOptions
  pure $ Store.StoreDir $ encodeUtf8 $ toText $ getStoreDir opts

parsePath :: (Framed e m) => Store.StoreDir -> Text -> m Store.StorePath
parsePath storeDir p = case Store.parsePath storeDir (encodeUtf8 p) of
  Left err -> throwError $ ErrorCall $ "Cannot parse store path " <> show p <> ":\n" <> show err
  Right path -> pure path

writeDerivation :: (Framed e m, MonadStore m, MonadReader e m, Has e Options) => Derivation -> m Store.StorePath
writeDerivation drv@Derivation{inputs, name} = do
  storeDir <- storeDirFromOptions
  let (inputSrcs, inputDrvs) = inputs
  referencePaths <- traverse (parsePath storeDir) (Set.toList $ inputSrcs <> Map.keysSet inputDrvs)
  let references = HS.fromList $ fmap (StorePath . fromString . decodeUtf8 . Store.storePathToRawFilePath storeDir) referencePaths
  path <- addTextToStore (Text.append name ".drv") (unparseDrv drv) references False
  parsePath storeDir $ fromString $ coerce path

-- | Traverse the graph of inputDrvs to replace fixed output derivations with their fixed output hash.
-- this avoids propagating changes to their .drv when the output hash stays the same.
hashDerivationModulo :: (MonadNix e t f m, MonadState (b, KeyMap Text) m) => Derivation -> m (Hash.Digest Hash.SHA256)
hashDerivationModulo
  Derivation
    { mFixed = Just digest
    , outputs
    , hashMode
    , name
    } =
  -- For fixed-output derivations, hash a special string encoding the content address
  -- Format: "fixed:out:<hashMode>:<hashAlgo>:<hash>:<outputPath>"
  -- This allows multiple derivations to share the same hash if they produce the same output
  case Map.lookup "out" outputs of
    Just outputPath -> do
      let algoText = hashDigestAlgoText digest
      let hashText = hashDigestText digest
      let modePrefix = case hashMode of
            Recursive -> "r:"
            Flat -> mempty
      let toHash = "fixed:out:" <> modePrefix <> algoText <> ":" <> hashText <> ":" <> outputPath
      pure $ Hash.hash @ByteString @Hash.SHA256 $ encodeUtf8 toHash
    Nothing ->
      throwError $ ErrorCall $
        "Fixed-output derivation '" <> toString name <> "' must have exactly one output named 'out'"
hashDerivationModulo
  drv@Derivation
    { inputs = ( inputSrcs
               , inputDrvs
               )
    } =
  do
    cache <- gets snd
    mstats <- askEvalStats
    inputsModulo <-
      Map.fromList <$>
        traverse
          (\(path, outs) ->
            maybe
              (do
                -- Time the .drv file read (this is IO that should be excluded from pure eval time)
                start <- liftIO Clock.getMonotonicTimeNSec
                drv' <- readDerivation $ coerce $ toString path
                end <- liftIO Clock.getMonotonicTimeNSec
                -- Record the IO time so it's excluded from expression exclusive time
                for_ mstats $ \stats -> recordDrvRead stats (end - start)

                digestValue <- hashDerivationModulo drv'
                -- Nix uses base16 when substituting derivation hashes in .drv serialization
                let hashBytes = convert digestValue :: ByteString
                let hash = decodeUtf8 (convertToBase Base16 hashBytes :: ByteString)
                pure (hash, outs)
              )
              (\ hash -> pure (hash, outs))
              (HM.lookup path cache)
          )
          (Map.toList inputDrvs)
    pure $ Hash.hash @ByteString @Hash.SHA256 $ encodeUtf8 $ unparseDrv $ drv {inputs = (inputSrcs, inputsModulo)}

unparseDrv :: Derivation -> Text
unparseDrv Derivation{..} =
  Text.append
    "Derive"
    $ parens
      [ -- outputs: [("out", "/nix/store/.....-out", "", ""), ...]
        serializeList $
          produceOutputInfo <$> Map.toList outputs
      , -- inputDrvs
        serializeList $
          (\(path, outs) ->
            parens [s path, serializeList $ s <$> sort outs]
          ) <$> Map.toList (snd inputs)
      , -- inputSrcs
        serializeList $ s <$> Set.toList (fst inputs)
      , s platform
      , s builder
      , -- run script args
        serializeList $ s <$> args
      , -- env (key value pairs)
        serializeList $ (\(k, v) -> parens [s k, s v]) <$> Map.toList env
      ]
  where
    produceOutputInfo (outputName, outputPath) =
      parens $ (s <$>) $ ([outputName, outputPath] <>) $
        maybe
          [mempty, mempty]
          (\digest ->
            let algoText = hashDigestAlgoText digest
                hashText = hashDigestText digest
                modePrefix = if hashMode == Recursive then "r:" else mempty
                hashType = modePrefix <> algoText
            in [hashType, hashText]
          )
          mFixed
    parens :: [Text] -> Text
    parens ts = Text.concat ["(", Text.intercalate "," ts, ")"]

    serializeList   :: [Text] -> Text
    serializeList   ls = Text.concat ["[", Text.intercalate "," ls, "]"]

    s = Text.cons '\"' . (`Text.snoc` '\"') . Text.concatMap escape

    escape :: Char -> Text
    escape '\\' = "\\\\"
    escape '\"' = "\\\""
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c = one c

readDerivation :: (Framed e m, MonadFile m) => Path -> m Derivation
readDerivation path = do
  content <- readFile path
  either
    (\ err -> throwError $ ErrorCall $ "Failed to parse " <> show path <> ":\n" <> show err)
    pure
    (parse derivationParser (coerce path) content)

derivationParser :: Parsec () Text Derivation
derivationParser = do
  _ <- "Derive("
  fullOutputs <- serializeList $
    (\[n, p, ht, h] -> (n, p, ht, h)) <$> parens s
  _ <- ","
  inputDrvs   <- Map.fromList <$> serializeList
    (liftA2 (,) ("(" *> s <* ",") (serializeList s <* ")"))
  _ <- ","
  inputSrcs   <- Set.fromList <$> serializeList s
  _ <- ","
  platform    <- s
  _ <- ","
  builder     <- s
  _ <- ","
  args        <- serializeList s
  _ <- ","
  env         <- fmap Map.fromList $ serializeList $ (\[a, b] -> (a, b)) <$> parens s
  _ <- ")"
  eof

  let outputs = Map.fromList $ (\(a, b, _, _) -> (a, b)) <$> fullOutputs
  let (mFixed, hashMode) = parseFixed fullOutputs
  let name = mempty -- FIXME (extract from file path ?)
  let useJson = one "__json" == Map.keys env

  pure $ Derivation {inputs = (inputSrcs, inputDrvs), ..}
 where
  s :: Parsec () Text Text
  s = fmap fromString $ string "\"" *> manyTill (escaped <|> regular) (string "\"")
  escaped = char '\\' *>
    (   '\n' <$ string "n"
    <|> '\r' <$ string "r"
    <|> '\t' <$ string "t"
    <|> anySingle
    )
  regular = noneOf ['\\', '"']

  wrap o c p =
    string o *> sepBy p (string ",") <* string c

  parens :: Parsec () Text a -> Parsec () Text [a]
  parens = wrap "(" ")"
  serializeList :: Parsec () Text a -> Parsec () Text [a]
  serializeList = wrap "[" "]"

  parseFixed :: [(Text, Text, Text, Text)] -> (Maybe HashDigest, HashMode)
  parseFixed fullOutputs = case fullOutputs of
    [("out", _path, rht, hash)] | rht /= mempty && hash /= mempty ->
      let
        (hashType, hashMode) = case Text.splitOn ":" rht of
          ["r", ht] -> (ht, Recursive)
          [ht] ->      (ht, Flat)
          _ -> error $ "Unsupported hash type for output of fixed-output derivation in .drv file: " <> show fullOutputs
      in
        case Store.mkNamedDigest hashType hash of
          Left err -> error $ show $ "Unsupported hash " <> show (hashType <> ":" <> hash) <> " in .drv file: " <> err
          Right digest -> (Just digest, hashMode)
    _ -> (Nothing, Flat)


-- | Execute the builtin:fetchurl builder
-- This downloads a URL and optionally unpacks it, adding the result to the store
-- Uses proper hash verification: compares declared hash vs actual content hash
executeBuiltinFetchurl
  :: forall e t f m
   . (MonadNix e t f m)
  => BuiltinFetchurlConfig
  -> Text                  -- ^ Expected output path (computed from declared hash)
  -> m ()
executeBuiltinFetchurl cfg expectedPath = do
  -- 1. Check if output already exists (cache hit)
  let expectedStorePath = coerce @String @Path $ toString expectedPath
  exists <- storePathExists expectedStorePath
  when exists $ pure ()

  -- 2. Download the URL content to bytes (not using addToStore)
  mstats <- askEvalStats
  start <- liftIO Clock.getMonotonicTimeNSec
  downloadResult <- downloadUrlBytes (bfUrl cfg)
  end <- liftIO Clock.getMonotonicTimeNSec
  for_ mstats $ \stats -> recordHttpFetch stats (end - start)

  contentBytes <- case downloadResult of
    Left err -> throwError err
    Right bytes -> pure bytes

  -- 3. Verify hash of downloaded content BEFORE unpacking
  -- The declared hash is always for the downloaded bytes, not unpacked content
  case bfExpectedHash cfg of
    Nothing -> pure ()
    Just expectedHash -> do
      let actualHash = Store.HashAlgo_SHA256 DSum.:=> Hash.hash contentBytes
      when (actualHash /= expectedHash) $
        throwError $ ErrorCall $
          "builtin:fetchurl: hash mismatch for " <> toString (bfUrl cfg) <>
          "\n  specified: " <> toString (hashDigestAlgoText expectedHash) <> ":" <> toString (hashDigestText expectedHash) <>
          "\n       got: " <> toString (hashDigestAlgoText actualHash) <> ":" <> toString (hashDigestText actualHash)

  -- 4. Write to temp file and handle unpacking/executable
  tmpBase <- liftIO Temp.getCanonicalTemporaryDirectory
  tmpDir <- liftIO $ Temp.createTempDirectory tmpBase "hnix-builtin-fetchurl"

  finalContentPath <- if bfUnpack cfg
    then do
      -- Write archive to temp file for unpacking
      let tarFile = tmpDir FP.</> "archive"
      liftIO $ B.writeFile tarFile contentBytes
      -- Unpack
      let unpackDir = tmpDir FP.</> "unpack"
      liftIO $ Directory.createDirectory unpackDir
      (exitCode, _out, err) <- liftIO $ readProcessWithExitCode "tar" ["-xf", tarFile, "-C", unpackDir] ""
      when (exitCode /= ExitSuccess) $
        throwError $ ErrorCall $ "builtin:fetchurl: failed to unpack archive: " <> err
      -- Find root directory
      rootDir <- liftIO $ pickUnpackRoot unpackDir
      -- Set executable if needed
      when (bfExecutable cfg) $
        liftIO $ setExecutableRecursive rootDir
      pure rootDir
    else do
      -- Write content directly
      let contentFile = tmpDir FP.</> "content"
      liftIO $ B.writeFile contentFile contentBytes
      when (bfExecutable cfg) $
        liftIO $ setExecutableFile contentFile
      pure contentFile

  -- 5. Store at the expected path (computed from declared hash)
  let storePath = StorePath expectedStorePath
  storeResult <- addToStoreAt storePath (NarFile $ coerce finalContentPath)
  liftIO $ Directory.removeDirectoryRecursive tmpDir
  case storeResult of
    Left err -> throwError err
    Right () -> pure ()
 where
  -- Find the root directory after unpacking (handle single-dir archives)
  pickUnpackRoot :: FilePath -> IO FilePath
  pickUnpackRoot unpackDir = do
    entries <- Directory.listDirectory unpackDir
    case entries of
      [single] -> do
        let singlePath = unpackDir FP.</> single
        isDir <- Directory.doesDirectoryExist singlePath
        pure $ if isDir then singlePath else unpackDir
      _ -> pure unpackDir

  -- Set executable bit on a file
  setExecutableFile :: FilePath -> IO ()
  setExecutableFile f = do
    st <- Posix.getSymbolicLinkStatus f
    let p = Posix.fileMode st
            `Posix.unionFileModes` Posix.ownerExecuteMode
            `Posix.unionFileModes` Posix.groupExecuteMode
            `Posix.unionFileModes` Posix.otherExecuteMode
    Posix.setFileMode f p

  -- Set executable bit recursively
  setExecutableRecursive :: FilePath -> IO ()
  setExecutableRecursive dir = do
    entries <- Directory.listDirectory dir
    for_ entries $ \entry -> do
      let path = dir FP.</> entry
      isDir <- Directory.doesDirectoryExist path
      if isDir
        then setExecutableRecursive path
        else setExecutableFile path

-- | Unpack an archive and add the result to the store
unpackAndAddToStore
  :: forall e t f m
   . (MonadNix e t f m)
  => BuiltinFetchurlConfig
  -> StorePath             -- ^ Downloaded archive path
  -> m StorePath
unpackAndAddToStore cfg archivePath = do
  -- Read the archive content
  archiveBytes <- either throwError pure =<< readStoreFile (coerce archivePath)

  -- Create temp directory for unpacking
  tmpBase <- liftIO Temp.getCanonicalTemporaryDirectory
  tmpDir <- liftIO $ Temp.createTempDirectory tmpBase "hnix-builtin-fetchurl"

  -- Write archive to temp file (tar needs a file)
  let tarFile = tmpDir FP.</> "archive"
  liftIO $ B.writeFile tarFile archiveBytes

  -- Create unpack directory
  let unpackDir = tmpDir FP.</> "unpack"
  liftIO $ Directory.createDirectory unpackDir

  -- Try to unpack (supporting .tar, .tar.gz, .tar.bz2, .tar.xz via tar auto-detection)
  (exitCode, _out, err) <- liftIO $
    readProcessWithExitCode "tar" ["-xf", tarFile, "-C", unpackDir] ""

  when (exitCode /= ExitSuccess) $ do
    liftIO $ Directory.removeDirectoryRecursive tmpDir
    throwError $ ErrorCall $
      "builtin:fetchurl: failed to unpack archive: " <> err

  -- Pick the root directory (single directory -> descend into it)
  rootDir <- liftIO $ pickUnpackRoot unpackDir

  -- Set executable permissions if needed
  when (bfExecutable cfg) $
    liftIO $ setTreeExecutable rootDir

  -- Add to store (always recursive for unpacked content)
  result <- addToStore (bfName cfg) (NarFile $ coerce rootDir) True False

  -- Cleanup temp directory
  liftIO $ Directory.removeDirectoryRecursive tmpDir

  either throwError pure result

-- | Pick the root of an unpacked archive
-- If there's a single directory inside, descend into it (common pattern for tarballs)
pickUnpackRoot :: FilePath -> IO FilePath
pickUnpackRoot unpackDir = do
  entries <- Directory.listDirectory unpackDir
  case entries of
    [single] -> do
      let singlePath = unpackDir FP.</> single
      isDir <- Directory.doesDirectoryExist singlePath
      pure $ if isDir then singlePath else unpackDir
    _ -> pure unpackDir

-- | Recursively set executable permissions on all regular files in a directory tree
setTreeExecutable :: FilePath -> IO ()
setTreeExecutable path = do
  isFile <- Directory.doesFileExist path
  if isFile
    then setExecutableFile path
    else do
      isDir <- Directory.doesDirectoryExist path
      when isDir $ do
        entries <- Directory.listDirectory path
        traverse_ (setTreeExecutable . (path FP.</>)) entries


defaultDerivationStrict :: forall e t f m b. (MonadNix e t f m, MonadState (b, KeyMap Text) m) => NValue t f m -> m (NValue t f m)
defaultDerivationStrict val = do
    s <- HM.mapKeys varNameText <$> fromValue @(AttrSet (NValue t f m)) val
    (drv, ctx) <- runWithStringContextT' $ buildDerivationWithContext s
    drvName <- makeStorePathName $ name drv
    storeDir <- storeDirFromOptions
    let
      inputs = toStorePaths ctx
      modEnv f = f (env drv)

    -- Compute the output paths, and add them to the environment if needed.
    -- Also add the inputs, just computed from the strings contexts.
    drv' <- case mFixed drv of
      Just digest -> do
        -- Fixed-output derivation: output path is content-addressed
        let outputPath = makeFixedOutputPath storeDir drvName digest (hashMode drv)
            outputs' = one ("out", outputPath)
        pure $ drv
          { inputs
          , outputs = outputs'
          , env = modEnv (outputs' <>)
          }

      Nothing -> do
        hash <- hashDerivationModulo $ drv
          { inputs
        --, outputs = Map.map (const "") (outputs drv)  -- not needed, this is already the case
          , env =
              modEnv
                (\ baseEnv ->
                  foldl'
                    (\m k -> Map.insert k mempty m)
                    baseEnv
                    (Map.keys $ outputs drv)
                )
          }
        outputs' <- sequenceA $ Map.mapWithKey (\o _ -> makeOutputPath storeDir o hash drvName) $ outputs drv
        pure $ drv
          { inputs
          , outputs = outputs'
          , env = modEnv (outputs' <>)
          }

    -- Note: builtin builders (like builtin:fetchurl) are NOT executed during evaluation.
    -- The .drv file records what should happen at build time. For fixed-output derivations,
    -- the output path is already computed from the declared hash (makeFixedOutputPath above).

    -- Time the store add operation (writeDerivation calls addTextToStore)
    mstats' <- askEvalStats
    writeStart <- liftIO Clock.getMonotonicTimeNSec
    drvStorePath <- writeDerivation drv'
    writeEnd <- liftIO Clock.getMonotonicTimeNSec
    -- Record the IO time so it's excluded from expression exclusive time
    for_ mstats' $ \stats -> recordStoreAdd stats (writeEnd - writeStart)

    let (mkVarName -> drvPath) = pathToText storeDir drvStorePath

    -- Memoize here, as it may be our last chance in case of readonly stores.
    digestValue <- hashDerivationModulo drv'
    -- Nix uses base16 for derivation hashes
    let drvHashBytes = convert digestValue :: ByteString
    let drvHash = decodeUtf8 (convertToBase Base16 drvHashBytes :: ByteString)
    modify $ second $ HM.insert (varNameText drvPath) drvHash

    let
      outputsWithContext =
        Map.mapWithKey
          (\out (mkVarName -> path) -> mkNixStringWithSingletonContext (StringContext (DerivationOutput out) drvPath) path)
          (outputs drv')
      drvPathWithContext = mkNixStringWithSingletonContext (StringContext AllOutputs drvPath) drvPath
      attrSet = NVStr <$> HM.insert "drvPath" drvPathWithContext (Map.foldrWithKey HM.insert HM.empty outputsWithContext)
    -- TODO: Add location information for all the entries.
    --              here --v
    pure $ NVSet mempty $ HM.mapKeys mkVarName attrSet

  where

    pathToText storeDir' = decodeUtf8 . Store.storePathToRawFilePath storeDir'

    makeFixedOutputPath :: Store.StoreDir -> Store.StorePathName -> HashDigest -> HashMode -> Text
    makeFixedOutputPath storeDir' name digest mode =
      let method = case mode of
            Recursive -> ContentAddressMethod_NixArchive
            Flat -> ContentAddressMethod_Flat
          storePath = StoreRO.makeFixedOutputPath storeDir' method digest mempty name
      in pathToText storeDir' storePath

    makeOutputPath storeDir' o h n = do
      -- Output path name is drvName for "out", drvName-outputName for other outputs
      outputPathName <- makeStorePathName $ Store.unStorePathName n <> if o == "out" then mempty else "-" <> o
      -- Convert cryptonite digest to crypton digest (both have same byte representation)
      let bytes :: ByteString
          bytes = convert h
          Just cryptonDigest = Hash.digestFromByteString bytes
          storePath = StoreRO.makeOutputPath storeDir' o cryptonDigest outputPathName
      pure $ pathToText storeDir' storePath

    toStorePaths :: HashSet StringContext -> (Set Text, Map Text [Text])
    toStorePaths = foldl (flip addToInputs) mempty

    addToInputs :: Bifunctor p => StringContext -> p (Set Text) (Map Text [Text])  -> p (Set Text) (Map Text [Text])
    addToInputs (StringContext kind (varNameText -> path)) =
      case kind of
        DirectPath -> first $ Set.insert path
        DerivationOutput o -> second $ Map.insertWith (<>) path $ one o
        AllOutputs ->
          -- TODO: recursive lookup. See prim_derivationStrict
          -- XXX: When is this really used ?
          error "Not implemented: derivations depending on a .drv file are not yet supported."


-- | Build a derivation in a context collecting string contexts.
-- This is complex from a typing standpoint, but it allows to perform the
-- full computation without worrying too much about all the string's contexts.
buildDerivationWithContext :: forall e t f m. (MonadNix e t f m) => KeyMap (NValue t f m) -> WithStringContextT m Derivation
buildDerivationWithContext drvAttrs = do
    -- Detect nulls for required string attributes early to improve diagnostics.
    let requireNonNullAttr attr = do
          raw <- getAttrRaw attr
          case raw of
            NVConstant NNull ->
              lift $ throwError $ ErrorCall $
                "Derivation attribute '" <> show attr <> "' is null"
            _ -> pure ()

        requireNonNullAttrIfPresent attr = case HM.lookup attr drvAttrs of
          Nothing -> pure ()
          Just v  -> do
            raw <- lift $ demand v
            case raw of
              NVConstant NNull ->
                lift $ throwError $ ErrorCall $
                  "Derivation attribute '" <> show attr <> "' is null"
              _ -> pure ()

    requireNonNullAttr "name"
    requireNonNullAttr "system"
    requireNonNullAttr "builder"
    requireNonNullAttrIfPresent "outputHash"
    requireNonNullAttrIfPresent "outputHashMode"
    requireNonNullAttrIfPresent "outputs"

    -- Parse name first, so we can add an informative frame
    drvName     <- getAttr   "name"                      $ assertDrvStoreName <=< extractNixString
    withFrame' Info (ErrorCall $ "While evaluating derivation " <> show drvName) $ do

      useJson     <- getAttrOr "__structuredAttrs" False     pure
      ignoreNulls <- getAttrOr "__ignoreNulls"     False     pure

      args        <- getAttrOr "args"              mempty  $ traverse (extractNixString <=< fromValue')
      builder     <- getAttr   "builder"                     extractNixString
      platform    <- getAttr   "system"                    $ assertNonNull <=< extractNoCtx
      mHash       <- getAttrOr "outputHash"        mempty  $ (pure . pure) <=< extractNoCtx
      mHashAlgo   <- getAttrMaybeNoCtx "outputHashAlgo"
      hashMode    <- getAttrOr "outputHashMode"    Flat    $ parseHashMode <=< extractNoCtx
      outputs     <- getAttrOr "outputs"       (one "out") $ traverse (extractNoCtx <=< fromValue')

      mFixedOutput <-
        maybe
          (pure Nothing)
          (\ hash -> do
            when (outputs /= one "out") $ lift $ throwError $ ErrorCall "Multiple outputs are not supported for fixed-output derivations"
            -- mkNamedDigest returns Either String (DSum HashAlgo Digest)
            -- Note: Empty outputHashAlgo should be treated as missing (infer from hash)
            -- This handles SRI hashes like "sha256-..." where outputHashAlgo is ""
            let mHashAlgo' = mHashAlgo >>= \a -> if Text.null a then Nothing else Just a
            digest <- lift $ either (throwError . ErrorCall) pure $
              maybe (inferHashDigest hash) (`Store.mkNamedDigest` hash) mHashAlgo'
            pure $ Just digest)
          mHash

      -- filter out null values if needed.
      attrs <-
        lift $
          bool
            (pure drvAttrs)
            (HM.mapMaybe id <$>
              traverse
                (fmap
                  (\case
                    NVConstant NNull -> Nothing
                    _value           -> Just _value
                  )
                  . demand
                )
                drvAttrs
            )
            ignoreNulls

      env <- if useJson
        then do
          jsonString :: NixString <- lift $ toJSONNixString $ NVSet mempty $ HM.mapKeys mkVarName $
            deleteKeys [ "args", "__ignoreNulls", "__structuredAttrs" ] attrs
          rawString :: Text <- extractNixString jsonString
          pure $ one ("__json", rawString)
        else
          traverse (extractNixString <=< lift . coerceAnyToNixString callFunc CopyToStore) $
            HM.foldrWithKey Map.insert Map.empty $ deleteKeys [ "args", "__ignoreNulls" ] attrs

      pure $ Derivation { platform, builder, args, env,  hashMode, useJson
        , name = drvName
        , outputs = Map.fromList $ (, mempty) <$> outputs
        , mFixed = mFixedOutput
        , inputs = mempty -- stub for now
        }
  where

    -- common functions, lifted to WithStringContextT

    fromValue' :: (FromValue a m (NValue' t f m (NValue t f m)), MonadNix e t f m) => NValue t f m -> WithStringContextT m a
    fromValue' = lift . fromValue

    withFrame' :: (Framed e m, Exception s) => NixLevel -> s -> WithStringContextT m a -> WithStringContextT m a
    withFrame' level f = join . lift . withFrame level f . pure

    -- shortcuts to get the (forced) value of an KeyMap field

    getAttrOr' :: forall v a. (MonadNix e t f m, FromValue v m (NValue' t f m (NValue t f m)))
      => Text -> m a -> (v -> WithStringContextT m a) -> WithStringContextT m a
    getAttrOr' n d f = case HM.lookup n drvAttrs of
      Nothing -> lift d
      Just v  -> withFrame' Info (ErrorCall $ "While evaluating attribute '" <> show n <> "'") $
                   f =<< fromValue' v

    getAttrOr n = getAttrOr' n . pure

    getAttr n = getAttrOr' n (throwError $ ErrorCall $ "Required attribute '" <> show n <> "' not found.")

    getAttrRaw :: Text -> WithStringContextT m (NValue t f m)
    getAttrRaw n = case HM.lookup n drvAttrs of
      Nothing -> lift $ throwError $ ErrorCall $ "Required attribute '" <> show n <> "' not found."
      Just v  -> lift $ demand v

    getAttrMaybeNoCtx :: Text -> WithStringContextT m (Maybe Text)
    getAttrMaybeNoCtx n = case HM.lookup n drvAttrs of
      Nothing -> pure Nothing
      Just v  -> withFrame' Info (ErrorCall $ "While evaluating attribute '" <> show n <> "'") $ do
        raw <- lift $ demand v
        case raw of
          NVConstant NNull -> pure Nothing
          _ -> Just <$> (extractNoCtx =<< fromValue' raw)

    inferHashDigest :: Text -> Either String HashDigest
    inferHashDigest hash
      | Text.null hash = Left "empty outputHash requires explicit outputHashAlgo"
      | otherwise = tryAlgos ["sha256", "sha512", "sha1", "md5"]
      where
        tryAlgos [] = Left $ "outputHashAlgo missing and outputHash is not a valid SRI/known hash: " <> Text.unpack hash
        tryAlgos (a:as) = case Store.mkNamedDigest a hash of
          Right d -> Right d
          Left _  -> tryAlgos as

    -- Test validity for fields

    assertDrvStoreName :: MonadNix e t f m => Text -> WithStringContextT m Text
    assertDrvStoreName name = lift $ do
      let invalid c = not $ isAscii c && (isAlphaNum c || c `elem` ("+-._?=" :: String)) -- isAlphaNum allows non-ascii chars.
      let failWith reason = throwError $ ErrorCall $ "Store name " <> show name <> " " <> reason
      when ("." `Text.isPrefixOf` name)    $ failWith "cannot start with a period"
      when (Text.length name > 211)        $ failWith "must be no longer than 211 characters"
      when (Text.any invalid name)         $ failWith "contains some invalid character"
      when (".drv" `Text.isSuffixOf` name) $ failWith "is not allowed to end in '.drv'"
      pure name

    extractNoCtx :: MonadNix e t f m => NixString -> WithStringContextT m Text
    extractNoCtx ns =
      maybe
        (lift $ throwError $ ErrorCall $ "The string " <> show ns <> " is not allowed to have a context.")
        pure
        (getStringNoContext ns)

    assertNonNull :: MonadNix e t f m => Text -> WithStringContextT m Text
    assertNonNull t = do
      when (Text.null t) $ lift $ throwError $ ErrorCall "Value must not be empty"
      pure t

    parseHashMode :: MonadNix e t f m => Text -> WithStringContextT m HashMode
    parseHashMode = \case
      "flat" ->      pure Flat
      "recursive" -> pure Recursive
      other -> lift $ throwError $ ErrorCall $ "Hash mode " <> show other <> " is not valid. It must be either 'flat' or 'recursive'"

    -- Other helpers

    deleteKeys :: [Text] -> KeyMap a -> KeyMap a
    deleteKeys keys attrSet = foldl' (flip HM.delete) attrSet keys
