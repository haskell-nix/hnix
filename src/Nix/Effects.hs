{-# language AllowAmbiguousTypes #-}
{-# language CPP #-}
{-# language DefaultSignatures #-}
{-# language TypeFamilies #-}
{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language UndecidableInstances #-}
{-# language PackageImports #-} -- 2021-07-05: Due to hashing Haskell IT system situation, in HNix we currently ended-up with 2 hash package dependencies @{hashing, cryptonite}@
{-# language TypeOperators #-}

{-# options_ghc -Wno-orphans #-}


module Nix.Effects where

import           Nix.Prelude             hiding ( putStrLn
                                                , print
                                                )
import qualified Nix.Prelude                   as Prelude
import qualified Prelude                       as HPrelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import qualified Data.HashSet                  as HS
import qualified Data.Text                     as Text
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import qualified Data.ByteString.Char8         as BS8
import           Network.HTTP.Client     hiding ( path, Proxy )
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import qualified Network.Connection          as NC
import qualified Network.TLS                 as TLS
import           Nix.Utils.Fix1
import           Nix.Expr.Types.Annotated
import           Nix.Frames              hiding ( Proxy )
import           Nix.Parser
import           Nix.Render
import           Nix.Options                   ( Options
                                                , askOptions
                                                , getStoreDir
                                                )
import           Nix.Value
import           Nix.FileType
import qualified Paths_hnix
import           System.Exit
import qualified System.Info
import           System.Process
import qualified Control.Exception            as Exception
import qualified Control.Concurrent           as Concurrent
import           System.IO.Unsafe             ( unsafePerformIO )
import qualified System.Directory             as Directory
import qualified System.PosixCompat.Files     as Posix
import qualified Data.ByteString              as B
import qualified System.FilePath              as FP
import qualified System.IO.Temp               as Temp

import qualified System.Nix.Store.Remote       as Store.Remote
import qualified System.Nix.StorePath          as Store
import qualified System.Nix.Nar                as Store.Nar
import qualified System.Nix.Hash               as Store.Hash
import           System.Nix.FileContentAddress  ( FileIngestionMethod(..) )
import           Data.Some.Newtype              ( Some(..) )
import           Data.Default.Class             ( def )

-- | A path into the nix store
newtype StorePath = StorePath Path
  deriving (Eq, Ord, Show, Hashable)


-- All of the following type classes defer to the underlying 'm'.

-- * @class MonadEffects t f m@

class
  ( MonadFile m
  , MonadStore m
  , MonadStoreRead m
  , MonadPutStr m
  , MonadHttp m
  , MonadEnv m
  , MonadPaths m
  , MonadInstantiate m
  , MonadExec m
  , MonadIntrospect m
  )
  => MonadEffects t f m where

  -- | Determine the absolute path in the current context.
  toAbsolutePath :: Path -> m Path
  findEnvPath :: String -> m Path

  -- | Having an explicit list of sets corresponding to the @NIX_PATH@ and a file path try to find an existing path.
  findPath :: Vector (NValue t f m) -> Path -> m Path

  importPath :: Path -> m (NValue t f m)
  pathToDefaultNix :: Path -> m Path

  derivationStrict :: NValue t f m -> m (NValue t f m)

  --  2021-04-01: for trace, so leaving String here
  traceEffect :: String -> m ()


-- ** Instances

instance
  ( MonadFix1T t m
  , MonadStore m
  )
  => MonadStore (Fix1T t m)
 where
  addToStore a b c d = lift $ addToStore a b c d
  addTextToStore' a b c d = lift $ addTextToStore' a b c d

-- * @class MonadIntrospect m@

class
  Monad m
  => MonadIntrospect m
 where
  recursiveSize :: a -> m Word
  default recursiveSize :: (MonadTrans t, MonadIntrospect m', m ~ t m') => a -> m Word
  recursiveSize = lift . recursiveSize


-- ** Instances

instance MonadIntrospect IO where
  recursiveSize =
#ifdef MIN_VERSION_ghc_datasize
    recursiveSize
#else
    const $ pure 0
#endif

deriving
  instance
    MonadIntrospect (t (Fix1 t))
    => MonadIntrospect (Fix1 t)

deriving
  instance
    MonadIntrospect (t (Fix1T t m) m)
    => MonadIntrospect (Fix1T t m)


-- * @class MonadExec m@

class
  Monad m
  => MonadExec m where

    exec' :: Vector Text -> m (Either ErrorCall NExprLoc)
    default exec' :: (MonadTrans t, MonadExec m', m ~ t m')
                  => Vector Text -> m (Either ErrorCall NExprLoc)
    exec' = lift . exec'


-- ** Instances

instance MonadExec IO where
  exec' v = case V.uncons v of
    Nothing            -> pure $ Left $ ErrorCall "exec: missing program"
    Just (prog, argsV) -> do
      let args = V.toList argsV
      (exitCode, out, _) <- liftIO $ readProcessWithExitCode (toString prog) (toString <$> args) mempty
      let
        t    = Text.strip $ fromString out
        emsg = "program[" <> prog <> "] args=" <> show args
      case exitCode of
        ExitSuccess ->
          pure $
          if Text.null t
            then Left $ ErrorCall $ toString $ "exec has no output :" <> emsg
            else
              either
                (\ err -> Left $ ErrorCall $ toString $ "Error parsing output of exec: " <> show err <> " " <> emsg)
                pure
                (parseNixTextLoc t)
        err -> pure $ Left $ ErrorCall $ toString $ "exec  failed: " <> show err <> " " <> emsg

deriving
  instance
    MonadExec (t (Fix1 t))
    => MonadExec (Fix1 t)

deriving
  instance
    MonadExec (t (Fix1T t m) m)
    => MonadExec (Fix1T t m)


-- * @class MonadInstantiate m@

class
  Monad m
  => MonadInstantiate m where

    instantiateExpr :: Text -> m (Either ErrorCall NExprLoc)
    default instantiateExpr :: (MonadTrans t, MonadInstantiate m', m ~ t m') => Text -> m (Either ErrorCall NExprLoc)
    instantiateExpr = lift . instantiateExpr


-- ** Instances

instance MonadInstantiate IO where

  instantiateExpr expr =
    do
      traceM $
        "Executing: " <> show ["nix-instantiate", "--eval", "--expr ", expr]

      (exitCode, out, err) <-
        readProcessWithExitCode
          "nix-instantiate"
          ["--eval", "--expr", toString expr]
          mempty

      pure $
        case exitCode of
          ExitSuccess ->
            either
              (\ e -> Left $ ErrorCall $ "Error parsing output of nix-instantiate: " <> show e)
              pure
              (parseNixTextLoc $ fromString out)
          status -> Left $ ErrorCall $ "nix-instantiate failed: " <> show status <> ": " <> err

deriving
  instance
    MonadInstantiate (t (Fix1 t))
    => MonadInstantiate (Fix1 t)

deriving
  instance
    MonadInstantiate (t (Fix1T t m) m)
    => MonadInstantiate (Fix1T t m)


-- * @class MonadEnv m@

class
  Monad m
  => MonadEnv m where

  getEnvVar :: Text -> m (Maybe Text)
  default getEnvVar :: (MonadTrans t, MonadEnv m', m ~ t m') => Text -> m (Maybe Text)
  getEnvVar = lift . getEnvVar

  getCurrentSystemOS :: m Text
  default getCurrentSystemOS :: (MonadTrans t, MonadEnv m', m ~ t m') => m Text
  getCurrentSystemOS = lift getCurrentSystemOS

  getCurrentSystemArch :: m Text
  default getCurrentSystemArch :: (MonadTrans t, MonadEnv m', m ~ t m') => m Text
  getCurrentSystemArch = lift getCurrentSystemArch


-- ** Instances

instance MonadEnv IO where
  getEnvVar            = (<<$>>) fromString . lookupEnv . toString

  getCurrentSystemOS   = pure $ fromString System.Info.os

  -- Invert the conversion done by GHC_CONVERT_CPU in GHC's aclocal.m4
  getCurrentSystemArch = pure $ fromString $ case System.Info.arch of
    "i386" -> "i686"
    arch   -> arch

deriving
  instance
    MonadEnv (t (Fix1 t))
    => MonadEnv (Fix1 t)

deriving
  instance
    MonadEnv (t (Fix1T t m) m)
    => MonadEnv (Fix1T t m)


-- * @class MonadPaths m@

class
  Monad m
  => MonadPaths m where
  getDataDir :: m Path
  default getDataDir :: (MonadTrans t, MonadPaths m', m ~ t m') => m Path
  getDataDir = lift getDataDir


-- ** Instances

instance MonadPaths IO where
  getDataDir = coerce Paths_hnix.getDataDir

deriving
  instance
    MonadPaths (t (Fix1 t))
    => MonadPaths (Fix1 t)

deriving
  instance
    MonadPaths (t (Fix1T t m) m)
    => MonadPaths (Fix1T t m)


-- * @class MonadHttp m@

class
  Monad m
  => MonadHttp m where

  getURL :: Text -> m (Either ErrorCall StorePath)
  default getURL :: (MonadTrans t, MonadHttp m', m ~ t m') => Text -> m (Either ErrorCall StorePath)
  getURL = lift . getURL

baseNameOf :: Text -> Text
baseNameOf a = Text.takeWhileEnd (/='/') $ Text.dropWhileEnd (=='/') a

{-# NOINLINE nixUserAgent #-}
nixUserAgent :: ByteString
nixUserAgent = unsafePerformIO $ do
  let nixVer = "2.18"
  verResult <- Exception.try (readProcess "curl" ["--version"] "")
  let curlVer = case verResult of
        Left (_ :: Exception.SomeException) -> "unknown"
        Right out -> case HPrelude.words out of
          (_:ver:_) -> ver
          _ -> "unknown"
  pure $ BS8.pack $ "curl/" <> curlVer <> " Nix/" <> nixVer

fetchURLWithNameAndExecutable :: (MonadIO m, MonadStore m) => Text -> Text -> Bool -> m (Either ErrorCall StorePath)
fetchURLWithNameAndExecutable url name executable =
  do
    let urlstr = toString url
    traceM $ "fetching HTTP URL: " <> urlstr
    req     <- liftIO $ parseRequest urlstr
    -- Use TLS manager that tolerates servers without Extended Master Secret.
    let supported = (def :: TLS.Supported)
          { TLS.supportedExtendedMainSecret = TLS.AllowEMS
          }
    let tlsSettings = NC.TLSSettingsSimple
          { NC.settingDisableCertificateValidation = False
          , NC.settingDisableSession = False
          , NC.settingUseServerName = False
          , NC.settingClientSupported = supported
          }
    manager <- liftIO $ newTlsManagerWith (mkManagerSettings tlsSettings Nothing)
    let req' = req
          { method = "GET"
          , requestHeaders = ("User-Agent", nixUserAgent) : requestHeaders req
          }
    let maxAttempts = 5 :: Int
    let baseDelayUs = 500000 :: Int
    let maxDelayUs = 8000000 :: Int

    let retryDelay attempt multiplier =
          let raw = baseDelayUs * multiplier * (2 ^ (attempt - 1))
          in min maxDelayUs raw

    let isRetryableStatus s =
          s == 408
            || s == 429
            || (s >= 500 && s < 600 && s /= 501 && s /= 505 && s /= 511)

    let isRetryableHttpException = \case
          HttpExceptionRequest _ (StatusCodeException response _) ->
            isRetryableStatus (statusCode $ responseStatus response)
          HttpExceptionRequest _ ResponseTimeout -> True
          HttpExceptionRequest _ ConnectionTimeout -> True
          HttpExceptionRequest _ (ConnectionFailure _) -> True
          HttpExceptionRequest _ ConnectionClosed -> True
          HttpExceptionRequest _ (ProxyConnectException _ _ _) -> True
          HttpExceptionRequest _ NoResponseDataReceived -> True
          _ -> False

    let fetchOnce =
          liftIO $ Exception.try (httpLbs req' manager)

    let handleSuccess response = do
          let body = toStrict (responseBody response)
          if executable
            then do
              tmpBase <- liftIO Temp.getCanonicalTemporaryDirectory
              tmpDir <- liftIO $ Temp.createTempDirectory tmpBase "hnix-fetchurl"
              let file = tmpDir FP.</> "content"
              liftIO $ B.writeFile file body
              liftIO $ setExecutableFile file
              -- executable output hashes are recursive (NAR) in nix/fetchurl.nix
              res <- addToStore name (NarFile $ coerce file) True False
              liftIO $ Directory.removeDirectoryRecursive tmpDir
              pure res
            else
              -- using addTextToStore' result in different hash from the addToStore.
              -- see https://github.com/haskell-nix/hnix/pull/1051#issuecomment-1031380804
              addToStore name (NarText body) False False

    let go attempt = do
          respOrErr <- fetchOnce
          case respOrErr of
            Right response -> do
              let status = statusCode $ responseStatus response
              if status == 200
                then handleSuccess response
                else if attempt < maxAttempts && isRetryableStatus status
                  then do
                    let delayUs =
                          retryDelay
                            attempt
                            (if status == 429 then 4 else 1)
                    traceM $ "fetchurl: got " <> show status <> ", retrying in " <> show (delayUs `div` 1000) <> "ms"
                    liftIO $ Concurrent.threadDelay delayUs
                    go (attempt + 1)
                  else
                    pure $ Left $ ErrorCall $ "fail, got " <> show status <> " when fetching url = " <> urlstr
            Left (e :: HttpException) ->
              if attempt < maxAttempts && isRetryableHttpException e
                then do
                  let delayUs = retryDelay attempt 1
                  traceM $ "fetchurl: " <> show e <> ", retrying in " <> show (delayUs `div` 1000) <> "ms"
                  liftIO $ Concurrent.threadDelay delayUs
                  go (attempt + 1)
                else
                  pure $ Left $ ErrorCall $ "fetchurl: " <> show e

    go 1

fetchURLWithName :: (MonadIO m, MonadStore m) => Text -> Text -> m (Either ErrorCall StorePath)
fetchURLWithName url name = fetchURLWithNameAndExecutable url name False

fetchURLWith :: (MonadIO m, MonadStore m) => Text -> m (Either ErrorCall StorePath)
fetchURLWith url = fetchURLWithName url (baseNameOf url)

-- | Download URL content and return raw bytes (for hash verification before storing)
-- This is used by builtin:fetchurl to verify hashes before storing at the expected path
downloadUrlBytes :: MonadIO m => Text -> m (Either ErrorCall B.ByteString)
downloadUrlBytes url = do
  let urlstr = toString url
  traceM $ "fetching HTTP URL: " <> urlstr
  req <- liftIO $ parseRequest urlstr
  let supported = (def :: TLS.Supported)
        { TLS.supportedExtendedMainSecret = TLS.AllowEMS
        }
  let tlsSettings = NC.TLSSettingsSimple
        { NC.settingDisableCertificateValidation = False
        , NC.settingDisableSession = False
        , NC.settingUseServerName = False
        , NC.settingClientSupported = supported
        }
  manager <- liftIO $ newTlsManagerWith (mkManagerSettings tlsSettings Nothing)
  let req' = req
        { method = "GET"
        , requestHeaders = ("User-Agent", nixUserAgent) : requestHeaders req
        }
  let maxAttempts = 5 :: Int
  let baseDelayUs = 500000 :: Int
  let maxDelayUs = 8000000 :: Int

  let retryDelay attempt multiplier =
        let raw = baseDelayUs * multiplier * (2 ^ (attempt - 1))
        in min maxDelayUs raw

  let isRetryableStatus s =
        s == 408
          || s == 429
          || (s >= 500 && s < 600 && s /= 501 && s /= 505 && s /= 511)

  let isRetryableHttpException = \case
        HttpExceptionRequest _ (StatusCodeException response _) ->
          isRetryableStatus (statusCode $ responseStatus response)
        HttpExceptionRequest _ ResponseTimeout -> True
        HttpExceptionRequest _ ConnectionTimeout -> True
        HttpExceptionRequest _ (ConnectionFailure _) -> True
        HttpExceptionRequest _ ConnectionClosed -> True
        HttpExceptionRequest _ (ProxyConnectException _ _ _) -> True
        HttpExceptionRequest _ NoResponseDataReceived -> True
        _ -> False

  let fetchOnce = liftIO $ Exception.try (httpLbs req' manager)

  let go attempt = do
        respOrErr <- fetchOnce
        case respOrErr of
          Right response -> do
            let status = statusCode $ responseStatus response
            if status == 200
              then pure $ Right $ toStrict (responseBody response)
              else if attempt < maxAttempts && isRetryableStatus status
                then do
                  let delayUs = retryDelay attempt (if status == 429 then 4 else 1)
                  traceM $ "fetchurl: got " <> show status <> ", retrying in " <> show (delayUs `div` 1000) <> "ms"
                  liftIO $ Concurrent.threadDelay delayUs
                  go (attempt + 1)
                else
                  pure $ Left $ ErrorCall $ "fail, got " <> show status <> " when fetching url = " <> urlstr
          Left (e :: HttpException) ->
            if attempt < maxAttempts && isRetryableHttpException e
              then do
                let delayUs = retryDelay attempt 1
                traceM $ "fetchurl: " <> show e <> ", retrying in " <> show (delayUs `div` 1000) <> "ms"
                liftIO $ Concurrent.threadDelay delayUs
                go (attempt + 1)
              else
                pure $ Left $ ErrorCall $ "fetchurl: " <> show e

  go 1

setExecutableFile :: FilePath -> IO ()
setExecutableFile f = do
  st <- Posix.getSymbolicLinkStatus f
  let p =
        Posix.fileMode st
          `Posix.unionFileModes` Posix.ownerExecuteMode
          `Posix.unionFileModes` Posix.groupExecuteMode
          `Posix.unionFileModes` Posix.otherExecuteMode
  Posix.setFileMode f p

-- conversion from Store.StorePath to Effects.StorePath, different type with the same name.
toStorePathWithDir :: Store.StoreDir -> Store.StorePath -> StorePath
toStorePathWithDir storeDir =
  StorePath
    . coerce
    . decodeUtf8 @FilePath @ByteString
    . Store.storePathToRawFilePath storeDir

toStorePath :: Store.StorePath -> StorePath
toStorePath = toStorePathWithDir def

-- ** Instances

instance MonadHttp IO where
  getURL = fetchURLWith


deriving
  instance
    MonadHttp (t (Fix1 t))
    => MonadHttp (Fix1 t)

deriving
  instance
    MonadHttp (t (Fix1T t m) m)
    => MonadHttp (Fix1T t m)


-- * @class MonadPutStr m@

class
  (Monad m, MonadIO m)
  => MonadPutStr m where

  --TODO: Should this be used *only* when the Nix to be evaluated invokes a
  --`trace` operation?
  --  2021-04-01: Due to trace operation here, leaving it as String.
  putStr :: String -> m ()
  default putStr :: (MonadTrans t, MonadPutStr m', m ~ t m') => String -> m ()
  putStr = lift . Prelude.putStr


-- ** Instances

instance MonadPutStr IO where
  putStr = Prelude.putStr

deriving
  instance
    MonadPutStr (t (Fix1 t))
    => MonadPutStr (Fix1 t)

deriving
  instance
    MonadPutStr (t (Fix1T t m) m)
    => MonadPutStr (Fix1T t m)


-- ** Functions

putStrLn :: MonadPutStr m => String -> m ()
putStrLn = Nix.Effects.putStr . (<> "\n")

print :: (MonadPutStr m, Show a) => a -> m ()
print = putStrLn . show

-- * Store effects

-- ** Data type synonyms

type RecursiveFlag = Bool
type RepairFlag = Bool
type StorePathName = Text
type PathFilter m = Path -> m Bool
type StorePathSet = HS.HashSet StorePath

-- ** @class MonadStoreRead m@

class
  Monad m
  => MonadStoreRead m where

  storePathExists :: Path -> m Bool
  default storePathExists :: (MonadTrans t, MonadStoreRead m', m ~ t m') => Path -> m Bool
  storePathExists = lift . storePathExists

  readStoreFile :: Path -> m (Either ErrorCall ByteString)
  default readStoreFile :: (MonadTrans t, MonadStoreRead m', m ~ t m') => Path -> m (Either ErrorCall ByteString)
  readStoreFile = lift . readStoreFile

  readStoreDir :: Path -> m (Either ErrorCall [(Path, FileType)])
  default readStoreDir :: (MonadTrans t, MonadStoreRead m', m ~ t m') => Path -> m (Either ErrorCall [(Path, FileType)])
  readStoreDir = lift . readStoreDir

  readStoreFileType :: Path -> m (Either ErrorCall FileType)
  default readStoreFileType :: (MonadTrans t, MonadStoreRead m', m ~ t m') => Path -> m (Either ErrorCall FileType)
  readStoreFileType = lift . readStoreFileType

instance (MonadFix1T t m, MonadStoreRead m) => MonadStoreRead (Fix1T t m) where
  storePathExists = lift . storePathExists
  readStoreFile = lift . readStoreFile
  readStoreDir = lift . readStoreDir
  readStoreFileType = lift . readStoreFileType

instance MonadStoreRead IO where
  storePathExists path = do
    res <- (Exception.try (Directory.doesPathExist (coerce path)) :: IO (Either Exception.SomeException Bool))
    pure $ either (const False) id res

  readStoreFile path = do
    res <- Exception.try (B.readFile (coerce path))
    pure $ case res of
      Left (e :: Exception.SomeException) ->
        Left $ ErrorCall $ "readStoreFile failed for " <> show path <> ": " <> show e
      Right bytes -> Right bytes

  readStoreDir path = do
    let base = coerce path :: FilePath
    res <- Exception.try (Directory.listDirectory base)
    case res of
      Left (e :: Exception.SomeException) ->
        pure $ Left $ ErrorCall $ "readStoreDir failed for " <> show path <> ": " <> show e
      Right items -> do
        entries <- forM items $ \item -> do
          status <- Posix.getSymbolicLinkStatus (base FP.</> item)
          pure (coerce item, fileTypeFromStatus status)
        pure $ Right entries

  readStoreFileType path = do
    res <- Exception.try (Posix.getSymbolicLinkStatus (coerce path))
    pure $ case res of
      Left (e :: Exception.SomeException) ->
        Left $ ErrorCall $ "readStoreFileType failed for " <> show path <> ": " <> show e
      Right status -> Right $ fileTypeFromStatus status


-- ** @class MonadStore m@

data NarContent = NarFile Path | NarText ByteString
-- | convert NarContent to NarSource needed in the store API
toNarSource :: MonadIO m => NarContent -> Store.Nar.NarSource m
toNarSource (NarFile path) = Store.Nar.dumpPath $ coerce path
toNarSource (NarText text) = Store.Nar.dumpString text

class
  Monad m
  => MonadStore m where

  -- | Copy the contents of a local path(Or pure text) to the store.  The resulting store
  -- path is returned.  Note: This does not support yet support the expected
  -- `filter` function that allows excluding some files.
  addToStore :: StorePathName -> NarContent -> RecursiveFlag -> RepairFlag -> m (Either ErrorCall StorePath)
  default addToStore :: (MonadTrans t, MonadStore m', m ~ t m') => StorePathName -> NarContent -> RecursiveFlag -> RepairFlag -> m (Either ErrorCall StorePath)
  addToStore a b c d = lift $ addToStore a b c d

  -- | Like addToStore, but the contents written to the output path is a
  -- regular file containing the given string.
  addTextToStore' :: StorePathName -> Text -> StorePathSet -> RepairFlag -> m (Either ErrorCall StorePath)
  default addTextToStore' :: (MonadTrans t, MonadStore m', m ~ t m') => StorePathName -> Text -> StorePathSet -> RepairFlag -> m (Either ErrorCall StorePath)
  addTextToStore' a b c d = lift $ addTextToStore' a b c d

  -- | Store content at a pre-computed path (for fixed-output derivations)
  -- This is used when the store path is computed from a declared hash rather than content hash
  addToStoreAt :: StorePath -> NarContent -> m (Either ErrorCall ())
  default addToStoreAt :: (MonadTrans t, MonadStore m', m ~ t m') => StorePath -> NarContent -> m (Either ErrorCall ())
  addToStoreAt sp content = lift $ addToStoreAt sp content


-- *** Instances

instance MonadStore IO where

  addToStore name content recursive repair =
    case Store.mkStorePathName name of
      Left err -> pure $ Left $ ErrorCall $ "String '" <> show name <> "' is not a valid path name: " <> show err
      Right pathName -> do
        let ingestionMethod = if recursive
                              then FileIngestionMethod_NixArchive
                              else FileIngestionMethod_Flat
            repairMode = if repair
                         then Store.Remote.RepairMode_DoRepair
                         else Store.Remote.RepairMode_DontRepair
            -- Use Some constructor with HashAlgo_SHA256
            hashAlgo = Some Store.Hash.HashAlgo_SHA256
        res <- Store.Remote.runStore $ Store.Remote.addToStore
                 pathName
                 (toNarSource content)
                 ingestionMethod
                 hashAlgo
                 repairMode
        either
          Left -- err
          (pure . toStorePath) -- store path
          <$> parseStoreResult "addToStore" res

  addTextToStore' name text references repair =
    case Store.mkStorePathName name of
      Left err -> pure $ Left $ ErrorCall $ "String '" <> show name <> "' is not a valid path name: " <> show err
      Right pathName -> do
        let storeText = Store.Remote.StoreText pathName text
            repairMode = if repair
                         then Store.Remote.RepairMode_DoRepair
                         else Store.Remote.RepairMode_DontRepair
            -- Convert our StorePathSet (HashSet of StorePath) to HashSet of Store.StorePath
            storeDir = Store.StoreDir "/nix/store"
            storeRefs = HS.map (\(StorePath p) -> Store.parsePath storeDir (encodeUtf8 $ toText p)) references
            -- Filter out any parse failures and extract successful paths
            validRefs = HS.foldl' (\acc -> either (const acc) (`HS.insert` acc)) HS.empty storeRefs
        res <- Store.Remote.runStore $ Store.Remote.addTextToStore storeText validRefs repairMode
        either
          Left -- err
          (pure . toStorePath) -- path
          <$> parseStoreResult "addTextToStore" res

  -- For real store, addToStoreAt would need to write content to the specified path
  -- This is mainly used for fixed-output derivations in overlay mode
  -- For now, error in IO mode since it's not implemented
  addToStoreAt (StorePath path) _content =
    pure $ Left $ ErrorCall $
      "addToStoreAt not implemented for real store. Path: " <> show path


-- ** Functions

-- Import DList type (it's re-exported as part of the Run type signature)
parseStoreResult :: Monad m => Text -> (Either Store.Remote.RemoteStoreError a, loggers) -> m (Either ErrorCall a)
parseStoreResult name (res, _logs) =
  pure $
    either
      (\ err -> Left $ ErrorCall $ "Failed to execute '" <> toString name <> "': " <> show err)
      pure
      res

addTextToStore :: (Framed e m, MonadStore m) => StorePathName -> Text -> StorePathSet -> RepairFlag -> m StorePath
addTextToStore a b c d =
  either
    throwError
    pure
    =<< addTextToStore' a b c d

--  2021-10-30: NOTE: Misleading name, please rename.
-- | Add @Path@ into the Nix Store
addPath :: (Framed e m, MonadStore m, Has e Options) => Path -> m StorePath
addPath p = do
  opts <- askOptions
  let
    storeDir = Store.StoreDir (encodeUtf8 (toText (getStoreDir opts)))
    parsed = Store.parsePath storeDir (encodeUtf8 (toText p))
  case parsed of
    Right sp -> pure $ toStorePathWithDir storeDir sp
    Left _ ->
      either
        throwError
        pure
        =<< addToStore (fromString $ coerce takeFileName p) (NarFile p) True False

toFile_ :: (Framed e m, MonadStore m) => Path -> Text -> m StorePath
toFile_ p contents = addTextToStore (fromString $ coerce p) contents mempty False
