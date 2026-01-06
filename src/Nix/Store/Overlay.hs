{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language PackageImports #-}
{-# language TypeFamilies #-}

module Nix.Store.Overlay
  ( OverlayStoreT(..)
  , OverlayStoreConfig(..)
  , OverlayStoreState(..)
  , StoredObject(..)
  , defaultOverlayStoreState
  , runOverlayStoreT
  , evalOverlayStoreT
  ) where

import           Nix.Prelude
import           Control.Monad.Ref             ( MonadRef(..)
                                                , MonadAtomicRef(..)
                                                )
import           Control.Monad.Fix             ( MonadFix )
import           Control.Monad.Catch           ( MonadCatch
                                                , MonadThrow
                                                , MonadMask
                                                )
import qualified Control.Exception            as Exception
import           GHC.Exception                 ( ErrorCall(ErrorCall) )
import qualified Data.ByteString               as B
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TextEncoding
import qualified System.Directory              as Directory
import qualified System.FilePath               as FP
import qualified System.IO.Temp                as Temp
import qualified System.PosixCompat.Files      as Posix

import           Nix.Effects
import           Nix.FileType
import           Nix.Render                    ( MonadFile )

import qualified System.Nix.Store.ReadOnly     as StoreRO
import qualified System.Nix.Store.Types        as StoreTypes
import qualified System.Nix.StorePath          as Store

data OverlayStoreConfig = OverlayStoreConfig
  { overlayStoreDir :: Store.StoreDir
  , overlayReadThrough :: Bool
  }

newtype OverlayStoreState = OverlayStoreState
  { overlayObjects :: HM.HashMap StorePath StoredObject
  }

data StoredObject
  = StoredFile B.ByteString
  | StoredDir (Map.Map FilePath StoredObject)
  | StoredSymlink FilePath
  deriving (Eq, Show)

defaultOverlayStoreState :: OverlayStoreState
defaultOverlayStoreState = OverlayStoreState mempty

newtype OverlayStoreT m a =
  OverlayStoreT
    (ReaderT OverlayStoreConfig (StateT OverlayStoreState m) a)
  deriving
    ( Functor, Applicative, Monad, MonadFail, MonadPlus, Alternative
    , MonadIO, MonadFix, MonadCatch, MonadThrow, MonadMask
    , MonadReader OverlayStoreConfig, MonadState OverlayStoreState
    )

instance MonadTrans OverlayStoreT where
  lift = OverlayStoreT . lift . lift
  {-# inline lift #-}

instance MonadRef m => MonadRef (OverlayStoreT m) where
  type Ref (OverlayStoreT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (OverlayStoreT m) where
  atomicModifyRef r = lift . atomicModifyRef r

runOverlayStoreT :: OverlayStoreConfig -> OverlayStoreState -> OverlayStoreT m a -> m (a, OverlayStoreState)
runOverlayStoreT cfg st (OverlayStoreT m) = runStateT (runReaderT m cfg) st

evalOverlayStoreT :: Monad m => OverlayStoreConfig -> OverlayStoreState -> OverlayStoreT m a -> m a
evalOverlayStoreT cfg st = fmap fst . runOverlayStoreT cfg st

instance MonadFile m => MonadFile (OverlayStoreT m)
instance MonadPutStr m => MonadPutStr (OverlayStoreT m)
instance MonadEnv m => MonadEnv (OverlayStoreT m)
instance MonadPaths m => MonadPaths (OverlayStoreT m)
instance MonadInstantiate m => MonadInstantiate (OverlayStoreT m)
instance MonadExec m => MonadExec (OverlayStoreT m)
instance MonadIntrospect m => MonadIntrospect (OverlayStoreT m)

instance MonadIO m => MonadHttp (OverlayStoreT m) where
  getURL = fetchURLWith

instance MonadIO m => MonadStore (OverlayStoreT m) where
  addToStore name content recursive _repair = do
    storeDir <- asks overlayStoreDir
    storeName <- case Store.mkStorePathName name of
      Left err -> pure $ Left $ ErrorCall $ "String '" <> show name <> "' is not a valid path name: " <> show err
      Right pathName -> do
        let method = if recursive
              then StoreTypes.FileIngestionMethod_FileRecursive
              else StoreTypes.FileIngestionMethod_Flat
        result <- case content of
          NarText bytes ->
            liftIO $
              Temp.withSystemTempDirectory "hnix-store" $ \tmpDir -> do
                let file = tmpDir FP.</> "content"
                B.writeFile file bytes
                StoreRO.computeStorePathForPath
                  storeDir
                  pathName
                  file
                  method
                  (StoreTypes.PathFilter (const True))
                  StoreTypes.RepairMode_DontRepair
          NarFile path ->
            liftIO $
              StoreRO.computeStorePathForPath
                storeDir
                pathName
                (coerce path)
                method
                (StoreTypes.PathFilter (const True))
                StoreTypes.RepairMode_DontRepair

        let storePath = toEffectsStorePath storeDir result
        objRes <- case content of
          NarText bytes -> pure $ Right $ StoredFile bytes
          NarFile path -> liftIO $ Exception.try $ readStoredObjectFromDisk (coerce path)
        case objRes of
          Left (e :: Exception.SomeException) ->
            pure $ Left $ ErrorCall $ "addToStore failed for " <> show name <> ": " <> show e
          Right obj -> do
            modify' $ \st -> st { overlayObjects = HM.insert storePath obj (overlayObjects st) }
            pure $ Right storePath
    pure storeName

  addTextToStore' name text references _repair = do
    storeDir <- asks overlayStoreDir
    case Store.mkStorePathName name of
      Left err -> pure $ Left $ ErrorCall $ "String '" <> show name <> "' is not a valid path name: " <> show err
      Right pathName -> do
        let textBytes = TextEncoding.encodeUtf8 text
        let refs = parseReferences storeDir references
        let storePath =
              StoreRO.computeStorePathForText
                storeDir
                pathName
                textBytes
                refs
        let effectsPath = toEffectsStorePath storeDir storePath
        modify' $ \st -> st { overlayObjects = HM.insert effectsPath (StoredFile textBytes) (overlayObjects st) }
        pure $ Right effectsPath

instance MonadIO m => MonadStoreRead (OverlayStoreT m) where
  storePathExists path = do
    mobj <- resolveStoredObject path
    case mobj of
      Just _ -> pure True
      Nothing -> do
        readThrough <- asks overlayReadThrough
        if readThrough
          then do
            res <- liftIO $ (Exception.try (Directory.doesPathExist (coerce path)) :: IO (Either Exception.SomeException Bool))
            pure $ either (const False) id res
          else pure False

  readStoreFile path = do
    mobj <- resolveStoredObject path
    case mobj of
      Just (StoredFile bytes) -> pure $ Right bytes
      Just (StoredDir _) ->
        pure $ Left $ ErrorCall $ "readStoreFile: " <> show path <> " is a directory"
      Just (StoredSymlink _) ->
        pure $ Left $ ErrorCall $ "readStoreFile: " <> show path <> " is a symlink"
      Nothing -> do
        readThrough <- asks overlayReadThrough
        if readThrough
          then do
            res <- liftIO $ Exception.try (B.readFile (coerce path))
            pure $ case res of
              Left (e :: Exception.SomeException) ->
                Left $ ErrorCall $ "readStoreFile failed for " <> show path <> ": " <> show e
              Right bytes -> Right bytes
          else
            pure $ Left $ ErrorCall $ "readStoreFile: " <> show path <> " not found in overlay store"

  readStoreDir path = do
    mobj <- resolveStoredObject path
    case mobj of
      Just (StoredDir entries) ->
        pure $ Right $
          (\(k, v) -> (coerce k, storedObjectFileType v)) <$> Map.toList entries
      Just _ ->
        pure $ Left $ ErrorCall $ "readStoreDir: " <> show path <> " is not a directory"
      Nothing -> do
        readThrough <- asks overlayReadThrough
        if readThrough
          then do
            let base = coerce path :: FilePath
            res <- liftIO $ Exception.try (Directory.listDirectory base)
            case res of
              Left (e :: Exception.SomeException) ->
                pure $ Left $ ErrorCall $ "readStoreDir failed for " <> show path <> ": " <> show e
              Right items -> do
                entries <- liftIO $ traverse (fileTypeForEntry base) items
                pure $ Right entries
          else
            pure $ Left $ ErrorCall $ "readStoreDir: " <> show path <> " not found in overlay store"

  readStoreFileType path = do
    mobj <- resolveStoredObject path
    case mobj of
      Just obj -> pure $ Right $ storedObjectFileType obj
      Nothing -> do
        readThrough <- asks overlayReadThrough
        if readThrough
          then do
            res <- liftIO $ Exception.try (Posix.getSymbolicLinkStatus (coerce path))
            pure $ case res of
              Left (e :: Exception.SomeException) ->
                Left $ ErrorCall $ "readStoreFileType failed for " <> show path <> ": " <> show e
              Right status -> Right $ fileTypeFromStatus status
          else
            pure $ Left $ ErrorCall $ "readStoreFileType: " <> show path <> " not found in overlay store"

toEffectsStorePath :: Store.StoreDir -> Store.StorePath -> StorePath
toEffectsStorePath storeDir =
  StorePath
    . coerce
    . Text.unpack
    . TextEncoding.decodeUtf8
    . Store.storePathToRawFilePath storeDir

parseReferences :: Store.StoreDir -> StorePathSet -> HS.HashSet Store.StorePath
parseReferences storeDir refs =
  let
    parseOne (StorePath p) = Store.parsePath storeDir (TextEncoding.encodeUtf8 $ toText p)
  in HS.foldl' (\acc r -> either (const acc) (`HS.insert` acc) (parseOne r)) HS.empty refs

storedObjectFileType :: StoredObject -> FileType
storedObjectFileType = \case
  StoredFile _    -> FileTypeRegular
  StoredDir _     -> FileTypeDirectory
  StoredSymlink _ -> FileTypeSymlink

resolveStoredObject :: MonadIO m => Path -> OverlayStoreT m (Maybe StoredObject)
resolveStoredObject path = do
  storeDir <- asks overlayStoreDir
  case splitStorePath storeDir path of
    Nothing -> pure Nothing
    Just (root, rel) -> do
      objects <- gets overlayObjects
      pure $ HM.lookup root objects >>= flip lookupStoredObject rel

lookupStoredObject :: StoredObject -> [FilePath] -> Maybe StoredObject
lookupStoredObject obj [] = Just obj
lookupStoredObject (StoredDir entries) (seg:segs) =
  Map.lookup seg entries >>= flip lookupStoredObject segs
lookupStoredObject _ _ = Nothing

splitStorePath :: Store.StoreDir -> Path -> Maybe (StorePath, [FilePath])
splitStorePath storeDir path =
  let
    storeDirText = TextEncoding.decodeUtf8 (Store.unStoreDir storeDir)
    storeDirNorm = Text.dropWhileEnd (== '/') storeDirText
    pathText = Text.dropWhileEnd (== '/') $ toText path
    prefix = storeDirNorm <> "/"
  in case Text.stripPrefix prefix pathText of
    Nothing -> Nothing
    Just rest ->
      case Text.splitOn "/" rest of
        [] -> Nothing
        (root:segs) ->
          let
            rootPath = storeDirNorm <> "/" <> root
            rootStorePath = StorePath $ coerce $ Text.unpack rootPath
            rel = Text.unpack <$> filter (not . Text.null) segs
          in Just (rootStorePath, rel)

readStoredObjectFromDisk :: FilePath -> IO StoredObject
readStoredObjectFromDisk path = do
  status <- Posix.getSymbolicLinkStatus path
  if Posix.isSymbolicLink status
    then do
      target <- Posix.readSymbolicLink path
      pure $ StoredSymlink target
    else if Posix.isRegularFile status
      then StoredFile <$> B.readFile path
      else if Posix.isDirectory status
        then do
          entries <- Directory.listDirectory path
          children <- traverse (\name -> (name,) <$> readStoredObjectFromDisk (path FP.</> name)) entries
          pure $ StoredDir $ Map.fromList children
        else
          pure $ StoredFile mempty

fileTypeForEntry :: FilePath -> FilePath -> IO (Path, FileType)
fileTypeForEntry base item = do
  status <- Posix.getSymbolicLinkStatus (base FP.</> item)
  pure (coerce item, fileTypeFromStatus status)
