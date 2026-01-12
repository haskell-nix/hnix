{-# language CPP #-}
{-# language DataKinds #-}
{-# language PackageImports #-}
{-# language Strict #-}

module Nix.Effects.Basic where

import           Nix.Prelude             hiding ( head
                                                )
import           Relude.Unsafe                  ( head )
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Control.Monad                  ( foldM )
import qualified Control.Monad.State           as State
import qualified "crypton" Crypto.Hash        as Crypto.Hash
import qualified Data.HashMap.Strict           as HM
import           Data.Dependent.Sum            ( DSum((:=>)) )
import           Data.Vector                    ( Vector )
import           Data.List.Split                ( splitOn )
import qualified Data.Text                     as Text
import qualified Data.ByteString               as B
import qualified Data.ByteString               as BS
import qualified Data.Text.Encoding            as TE
import           Data.Time.Clock.POSIX         ( posixSecondsToUTCTime )
import           Data.Time.Format              ( defaultTimeLocale
                                                , formatTime
                                                )
import qualified System.Environment            as Env
import           Prettyprinter                  ( fillSep )
import           Nix.Convert
import           Nix.Effects
import           Nix.Exec                       ( MonadNix
                                                , evalExprLoc
                                                )
import           Nix.Context                    ( askEvalStats
                                                , CtxCfg
                                                )
import           Nix.Config.Singleton           ( HasProvCfg )
import           Nix.EvalStats                  ( recordHttpFetch, recordStoreAdd, recordFileRead )
import qualified GHC.Clock                     as Clock
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.Parser
import           Nix.Render
import           Nix.Scope
import           Nix.String
import           Nix.Value
import           Nix.Value.Monad
import           Nix.Options

import qualified System.Directory              as Directory
import qualified System.FilePath               as FP
import qualified System.IO.Temp                as Temp
import           System.Exit                   ( ExitCode(..) )
import           System.Process                ( readProcessWithExitCode )
import qualified System.Posix.Files            as Posix
import qualified System.Nix.Base               as StoreBase
import qualified System.Nix.Nar                as Nar
import qualified System.Nix.Store.ReadOnly     as StoreRO
import qualified System.Nix.StorePath          as Store
import qualified System.Nix.Store.Types        as StoreTypes
import qualified System.Nix.Hash               as StoreHash
import           System.Nix.ContentAddress      ( ContentAddressMethod(..) )

#ifdef MIN_VERSION_ghc_datasize
import           GHC.DataSize
#endif




defaultToAbsolutePath :: forall e t f m . MonadNix e t f m => Path -> m Path
defaultToAbsolutePath origPath =
  do
    origPathExpanded <- expandHomePath origPath
    fmap
      removeDotDotIndirections
      . canonicalizePath
        =<< bool
            (fmap
              (<///> origPathExpanded)
              $ maybe
                  getCurrentDirectory
                  ( (\case
                      NVPath s -> pure $ takeDirectory s
                      val -> throwError $ ErrorCall $ "when resolving relative path, __cur_file is in scope, but is not a path; it is: " <> show val
                    ) <=< demand
                  )
                  =<< lookupVar "__cur_file"
            )
            (pure origPathExpanded)
            (isAbsolute origPathExpanded)

expandHomePath :: MonadFile m => Path -> m Path
expandHomePath (coerce -> ('~' : xs)) = (<> coerce xs) <$> getHomeDirectory
expandHomePath p          = pure p

-- | Incorrectly normalize paths by rewriting patterns like @a/b/..@ to @a@.
--   This is incorrect on POSIX systems, because if @b@ is a symlink, its
--   parent may be a different directory from @a@. See the discussion at
--   https://hackage.haskell.org/package/directory-1.3.1.5/docs/System-Directory.html#v:canonicalizePath
removeDotDotIndirections :: Path -> Path
removeDotDotIndirections = coerce . intercalate "/" . go mempty . splitOn "/" . coerce
 where
  go s       []            = reverse s
  go (_ : s) (".." : rest) = go s rest
  go s       (this : rest) = go (this : s) rest

infixr 9 <///>
(<///>) :: Path -> Path -> Path
x <///> y
  | isAbsolute y || "." `isPrefixOf` coerce y = x </> y
  | otherwise                          = joinByLargestOverlap x y
 where
  joinByLargestOverlap :: Path -> Path -> Path
  joinByLargestOverlap (splitDirectories -> xs) (splitDirectories -> ys) =
    joinPath $ head
      [ xs <> drop (length tx) ys | tx <- tails xs, tx `elem` inits ys ]

defaultFindEnvPath :: MonadNix e t f m => String -> m Path
defaultFindEnvPath = findEnvPathM . coerce

findEnvPathM :: forall e t f m . MonadNix e t f m => Path -> m Path
findEnvPathM name =
  maybe
    (fail "impossible")
    (\ v ->
      do
        l <- fromValue @(Vector (NValue t f m)) =<< demand v
        findPathBy nixFilePath l name
    )
    =<< lookupVar "__nixPath"

 where
  nixFilePath :: MonadEffects t f m => Path -> m (Maybe Path)
  nixFilePath path =
    do
      absPath <- toAbsolutePath @t @f path
      isDir   <- doesDirectoryExist absPath
      absFile <-
        bool
          (pure absPath)
          (toAbsolutePath @t @f $ absPath </> "default.nix")
          isDir

      (pure absFile `whenTrue`) <$> doesFileExist absFile

findPathBy
  :: forall e t f m
   . MonadNix e t f m
  => (Path -> m (Maybe Path))
  -> Vector (NValue t f m)
  -> Path
  -> m Path
findPathBy finder ls name =
  maybe
    (throwError $ ErrorCall $ "file ''" <> coerce name <> "'' was not found in the Nix search path (add it's using $NIX_PATH or -I)")
    pure
    =<< foldM fun mempty ls
 where
  fun
    :: MonadNix e t f m
    => Maybe Path
    -> NValue t f m
    -> m (Maybe Path)
  fun =
    maybe
      (\ nv ->
        do
          (s :: HashMap VarName (NValue t f m)) <- fromValue =<< demand nv
          p <- resolvePath s
          path <- fromValue =<< demand p

          maybe
            (tryPath path mempty)
            (\ nv' ->
              do
                mns <- fromValueMay @NixString =<< demand nv'
                tryPath path $
                  whenJust
                    (\ nsPfx ->
                      let pfx = ignoreContext nsPfx in
                      pure $ coerce $ toString pfx `whenFalse` Text.null pfx
                    )
                    mns
            )
            (HM.lookup "prefix" s)
      )
      (const . pure . pure)

  tryPath :: Path -> Maybe Path -> m (Maybe Path)
  tryPath p (Just n) | n' : ns <- splitDirectories name, n == n' =
    finder $ p <///> joinPath ns
  tryPath p _ = finder $ p <///> name

  resolvePath :: HashMap VarName (NValue t f m) -> m (NValue t f m)
  resolvePath s =
    maybe
      (maybe
        (throwError $ ErrorCall $ "__nixPath must be a list of attr sets with 'path' elements, but received: " <> show s)
        (defer . fetchTarball)
        (HM.lookup "uri" s)
      )
      pure
      (HM.lookup "path" s)

fetchTarball
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> m (NValue t f m)
fetchTarball =
  \case
    NVSet _ s -> do
      urlVal <- maybe (throwError $ ErrorCall "builtins.fetchTarball: Missing url attribute") pure (HM.lookup "url" s)
      url <- fromValue =<< demand urlVal
      mShaVal <- traverse (fromValue <=< demand) (HM.lookup "sha256" s <|> HM.lookup "hash" s)
      mNameVal <- traverse (fromValue <=< demand) (HM.lookup "name" s)
      fetch url mShaVal mNameVal
    NVStr ns -> fetch (ignoreContext ns) Nothing Nothing
    v -> throwError $ ErrorCall $ "builtins.fetchTarball: Expected URI or set, got " <> show v
  <=< demand
 where
  fetch :: Text -> Maybe Text -> Maybe Text -> m (NValue t f m)
  fetch uri mSha mName = do
    opts <- askOptions
    mstats <- askEvalStats
    let defaultName = "source"
    let name0 = fromMaybe defaultName mName
    let name = if Text.null name0 then baseNameOf uri else name0

    storeName <- case Store.mkStorePathName name of
      Left err ->
        throwError $ ErrorCall $ "builtins.fetchTarball: invalid store path name '" <> show name <> "': " <> show err
      Right n -> pure n

    -- Instrument getURL for IO timing
    start <- liftIO Clock.getMonotonicTimeNSec
    tarPath <- either throwError pure =<< getURL uri
    end <- liftIO Clock.getMonotonicTimeNSec
    for_ mstats $ \stats -> recordHttpFetch stats (end - start)
    tarBytes <- either throwError pure =<< readStoreFile (coerce tarPath)

    tmpBase <- liftIO Temp.getCanonicalTemporaryDirectory
    tmpDir <- liftIO $ Temp.createTempDirectory tmpBase "hnix-fetchTarball"
    let tarFile = tmpDir FP.</> "source.tar"
    liftIO $ B.writeFile tarFile tarBytes
    let unpackDir = tmpDir FP.</> "unpack"
    liftIO $ Directory.createDirectory unpackDir
    (exitCode, _out, err) <- liftIO $ readProcessWithExitCode "tar" ["-xf", tarFile, "-C", unpackDir] ""
    when (exitCode /= ExitSuccess) $
      throwError $ ErrorCall $ "builtins.fetchTarball: failed to unpack " <> show uri <> ": " <> err

    rootDir <- liftIO $ pickRoot unpackDir

    let storeDir = Store.StoreDir $ encodeUtf8 $ toText $ getStoreDir opts
    actualPath <- liftIO $
      StoreRO.computeStorePathForPath
        storeDir
        storeName
        rootDir
        ContentAddressMethod_NixArchive
        (StoreTypes.PathFilter (const True))
        StoreTypes.RepairMode_DontRepair

    case mSha of
      Nothing -> pure ()
      Just shaText -> do
        case StoreHash.mkNamedDigest "sha256" shaText of
          Left shaErr -> throwError $ ErrorCall $ "builtins.fetchTarball: " <> shaErr
          Right (StoreHash.HashAlgo_SHA256 :=> digest) -> do
            let expectedPath =
                  StoreRO.makeFixedOutputPath
                    storeDir
                    ContentAddressMethod_NixArchive
                    (StoreHash.HashAlgo_SHA256 :=> digest)
                    mempty
                    storeName
            when (expectedPath /= actualPath) $
              throwError $ ErrorCall $
                "builtins.fetchTarball: hash mismatch for " <> show uri
          Right _ ->
            throwError $ ErrorCall "builtins.fetchTarball: unsupported hash algorithm"

    -- Instrument addToStore for IO timing
    storeStart <- liftIO Clock.getMonotonicTimeNSec
    res <- addToStore name (NarFile $ coerce rootDir) True False
    storeEnd <- liftIO Clock.getMonotonicTimeNSec
    for_ mstats $ \stats -> recordStoreAdd stats (storeEnd - storeStart)
    storePath <- either throwError pure res
    liftIO $ Directory.removeDirectoryRecursive tmpDir
    toValue storePath

  pickRoot :: FilePath -> IO FilePath
  pickRoot unpackDir = do
    entries <- Directory.listDirectory unpackDir
    case entries of
      [single] -> do
        let singlePath = unpackDir FP.</> single
        isDir <- Directory.doesDirectoryExist singlePath
        pure $ if isDir then singlePath else unpackDir
      _ -> pure unpackDir

data FetchGitMode = FetchGitMode
  { fgEmptyRevFallback   :: Bool
  , fgDefaultShallow     :: Bool
  , fgDefaultExportIgnore :: Bool
  , fgAllowName          :: Bool
  }

data PublicKey = PublicKey
  { pkType :: Text
  , pkKey  :: Text
  }

data FetchGitArgs = FetchGitArgs
  { fgUrl          :: Text
  , fgName         :: Text
  , fgRev          :: Maybe Text
  , fgRef          :: Maybe Text
  , fgSubmodules   :: Bool
  , fgShallow      :: Bool
  , fgExportIgnore :: Bool
  , fgAllRefs      :: Bool
  , fgLfs          :: Bool
  , fgNarHash      :: Maybe Text
  , fgVerifyCommit :: Bool
  , fgPublicKeys   :: [PublicKey]
  }

data RevInfo = RevInfo
  { riRev         :: Text
  , riRevCount    :: Maybe Integer
  , riLastModSecs :: Maybe Integer
  }

data FetchGitResult = FetchGitResult
  { fgrStorePath     :: StorePath
  , fgrRevInfo       :: Maybe RevInfo
  , fgrDirtyRev      :: Maybe Text
  , fgrDirtyShortRev :: Maybe Text
  , fgrLastModified  :: Integer
  , fgrNarHash       :: Text
  }

data GitLocation
  = GitWorkdir FilePath
  | GitLocalRepo FilePath
  | GitRemote Text

fetchGit
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> m (NValue t f m)
fetchGit =
  let mode = FetchGitMode
        { fgEmptyRevFallback = True
        , fgDefaultShallow = False
        , fgDefaultExportIgnore = True
        , fgAllowName = True
        }
  in
  \case
    NVSet _ s -> fetchGitFromSet mode s
    NVPath p -> fetchGitFromUrl mode (toText p)
    NVStr ns ->
      maybe
        (throwError $ ErrorCall "builtins.fetchGit: unsupported arguments to url")
        (fetchGitFromUrl mode)
        (getStringNoContext ns)
    v -> throwError $ ErrorCall $ "builtins.fetchGit: Expected URI or set, got " <> show v
  <=< demand

fetchTree
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> m (NValue t f m)
fetchTree =
  let mode = FetchGitMode
        { fgEmptyRevFallback = False
        , fgDefaultShallow = True
        , fgDefaultExportIgnore = False
        , fgAllowName = False
        }
  in
  \case
    NVSet _ s -> do
      typeVal <- maybe (throwError $ ErrorCall "builtins.fetchTree: missing type") pure (HM.lookup "type" s)
      typ <- fromValue =<< demand typeVal
      if typ == ("git" :: Text)
        then fetchGitFromSet mode s
        else throwError $ ErrorCall $ "builtins.fetchTree: unsupported type " <> show typ
    NVStr ns ->
      case getStringNoContext ns of
        Just url
          | Just rest <- Text.stripPrefix "git+" url
          -> fetchGitFromUrl mode rest
        _ -> throwError $ ErrorCall "builtins.fetchTree: Expected set or git+ URL string"
    v -> throwError $ ErrorCall $ "builtins.fetchTree: Expected set, got " <> show v
  <=< demand

fetchGitFromUrl
  :: forall e t f m
   . MonadNix e t f m
  => FetchGitMode
  -> Text
  -> m (NValue t f m)
fetchGitFromUrl mode url = do
  let args = FetchGitArgs
        { fgUrl = stripGitPrefix url
        , fgName = "source"
        , fgRev = Nothing
        , fgRef = Nothing
        , fgSubmodules = False
        , fgShallow = fgDefaultShallow mode
        , fgExportIgnore = fgDefaultExportIgnore mode
        , fgAllRefs = False
        , fgLfs = False
        , fgNarHash = Nothing
        , fgVerifyCommit = False
        , fgPublicKeys = []
        }
  fetchGitWith mode args

fetchGitFromSet
  :: forall e t f m
   . MonadNix e t f m
  => FetchGitMode
  -> AttrSet (NValue t f m)
  -> m (NValue t f m)
fetchGitFromSet mode s = do
  urlVal <- maybe (throwError $ ErrorCall "builtins.fetchGit: Missing url attribute") pure (HM.lookup "url" s)
  url <- extractUrlLike =<< demand urlVal
  mNameVal <- traverse (fromValue <=< demand) (HM.lookup "name" s)
  when (not (fgAllowName mode) && isJust mNameVal) $
    throwError $ ErrorCall "builtins.fetchTree: argument 'name' isn’t supported"
  mRevVal <- traverse (fromValue <=< demand) (HM.lookup "rev" s)
  mRefVal <- traverse (fromValue <=< demand) (HM.lookup "ref" s)
  mSubVal <- traverse (fromValue <=< demand) (HM.lookup "submodules" s)
  mShallowVal <- traverse (fromValue <=< demand) (HM.lookup "shallow" s)
  mExportIgnoreVal <- traverse (fromValue <=< demand) (HM.lookup "exportIgnore" s)
  mAllRefsVal <- traverse (fromValue <=< demand) (HM.lookup "allRefs" s)
  mLfsVal <- traverse (fromValue <=< demand) (HM.lookup "lfs" s)
  mNarHashVal <- traverse (fromValue <=< demand) (HM.lookup "narHash" s)
  mVerifyCommitVal <- traverse (fromValue <=< demand) (HM.lookup "verifyCommit" s)
  mKeyTypeVal <- traverse (fromValue <=< demand) (HM.lookup "keytype" s)
  mPublicKeyVal <- traverse (fromValue <=< demand) (HM.lookup "publicKey" s)
  mPublicKeysVal <- traverse (fromValue @[NValue t f m] <=< demand) (HM.lookup "publicKeys" s)

  pubKeys <- parsePublicKeys (fromMaybe "ssh-ed25519" mKeyTypeVal) mPublicKeyVal mPublicKeysVal

  let name = fromMaybe "source" (nonEmptyText mNameVal)
  let submodules = fromMaybe False mSubVal
  let exportIgnore = case mExportIgnoreVal of
        Just v -> v
        Nothing ->
          fgDefaultExportIgnore mode && not submodules

  let verifyCommitFlag = fromMaybe (not (null pubKeys)) mVerifyCommitVal

  let args = FetchGitArgs
        { fgUrl = stripGitPrefix url
        , fgName = name
        , fgRev = nonEmptyText mRevVal
        , fgRef = nonEmptyText mRefVal
        , fgSubmodules = submodules
        , fgShallow = fromMaybe (fgDefaultShallow mode) mShallowVal
        , fgExportIgnore = exportIgnore
        , fgAllRefs = fromMaybe False mAllRefsVal
        , fgLfs = fromMaybe False mLfsVal
        , fgNarHash = nonEmptyText mNarHashVal
        , fgVerifyCommit = verifyCommitFlag
        , fgPublicKeys = pubKeys
        }
  fetchGitWith mode args

fetchGitWith
  :: forall e t f m
   . MonadNix e t f m
  => FetchGitMode
  -> FetchGitArgs
  -> m (NValue t f m)
fetchGitWith mode args = do
  res <- fetchGitToStore args
  buildFetchGitAttrs mode args res

buildFetchGitAttrs
  :: forall e t f m
   . MonadNix e t f m
  => FetchGitMode
  -> FetchGitArgs
  -> FetchGitResult
  -> m (NValue t f m)
buildFetchGitAttrs mode args res = do
  outPathVal <- (toValue (fgrStorePath res) :: m (NValue t f m))
  narHashVal <- (toValue (fgrNarHash res) :: m (NValue t f m))
  subVal <- (toValue (fgSubmodules args) :: m (NValue t f m))
  let attrsBase :: [(VarName, NValue t f m)] =
        [ ("outPath", outPathVal)
        , ("narHash", narHashVal)
        , ("submodules", subVal)
        ]

  let emptyRev = "0000000000000000000000000000000000000000"
  let (revText, revCountValText) =
        case fgrRevInfo res of
          Just ri -> (riRev ri, riRevCount ri)
          Nothing -> (emptyRev, Nothing)

  revAttrs <-
    if fgEmptyRevFallback mode
      then do
        revVal <- (toValue revText :: m (NValue t f m))
        shortRevVal <- (toValue (Text.take 7 revText) :: m (NValue t f m))
        let revCountText = fromMaybe (0 :: Integer) revCountValText
        revCountVal <- (toValue revCountText :: m (NValue t f m))
        pure [("rev", revVal), ("shortRev", shortRevVal), ("revCount", revCountVal)]
      else case fgrRevInfo res of
        Nothing -> pure []
        Just ri -> do
          revVal <- (toValue (riRev ri) :: m (NValue t f m))
          shortRevVal <- (toValue (Text.take 7 (riRev ri)) :: m (NValue t f m))
          revCountAttrs <- case riRevCount ri of
            Nothing -> pure []
            Just rc -> do
              rcVal <- (toValue rc :: m (NValue t f m))
              pure [("revCount", rcVal)]
          pure $ [("rev", revVal), ("shortRev", shortRevVal)] <> revCountAttrs

  dirtyAttrs <- do
    let mk k v = (k,) <$> (toValue v :: m (NValue t f m))
    d1 <- traverse (mk "dirtyRev") (fgrDirtyRev res)
    d2 <- traverse (mk "dirtyShortRev") (fgrDirtyShortRev res)
    pure $ catMaybes [d1, d2]

  let lastMod = fgrLastModified res
  let ts = posixSecondsToUTCTime (fromInteger lastMod)
  let dateText = Text.pack $ formatTime defaultTimeLocale "%Y%m%d%H%M%S" ts
  lastModVal <- (toValue (lastMod :: Integer) :: m (NValue t f m))
  lastModDateVal <- (toValue dateText :: m (NValue t f m))
  let lastModAttrs =
        [ ("lastModified", lastModVal)
        , ("lastModifiedDate", lastModDateVal)
        ]

  toValue (HM.fromList (attrsBase <> revAttrs <> dirtyAttrs <> lastModAttrs) :: AttrSet (NValue t f m))

fetchGitToStore
  :: forall e t f m
   . MonadNix e t f m
  => FetchGitArgs
  -> m FetchGitResult
fetchGitToStore args = do
  when (fgExportIgnore args && fgSubmodules args) $
    throwError $ ErrorCall "builtins.fetchGit: exportIgnore and submodules are not supported together yet"
  loc <- classifyGitLocation args
  res <- case loc of
    GitWorkdir path -> fetchGitWorkingTree args path
    GitLocalRepo path -> fetchGitFromLocalRepo args path
    GitRemote url -> fetchGitCloneRepo args url
  validateNarHash args res

validateNarHash
  :: forall e t f m
   . MonadNix e t f m
  => FetchGitArgs
  -> FetchGitResult
  -> m FetchGitResult
validateNarHash args res =
  case fgNarHash args of
    Nothing -> pure res
    Just expectedText -> do
      expected <- parseSha256Digest expectedText
      actual <- parseSha256Digest (fgrNarHash res)
      when (expected /= actual) $
        throwError $ ErrorCall "builtins.fetchGit: NAR hash mismatch"
      pure res

fetchGitWorkingTree
  :: forall e t f m
   . MonadNix e t f m
  => FetchGitArgs
  -> FilePath
  -> m FetchGitResult
fetchGitWorkingTree args path = do
  mstats <- askEvalStats
  repoRoot <- runGitIn path ["rev-parse", "--show-toplevel"]
  let repoRootFp = toString repoRoot
  dirty <- isRepoDirty repoRootFp
  mHeadRev <- gitHeadRev repoRootFp

  when (fgVerifyCommit args && dirty) $
    throwError $ ErrorCall "builtins.fetchGit: commit verification is required, but the working tree is dirty"

  tmpBase <- liftIO Temp.getCanonicalTemporaryDirectory
  tmpDir <- liftIO $ Temp.createTempDirectory tmpBase "hnix-fetchGit"
  files0 <- gitLsFiles repoRootFp (fgSubmodules args)
  files <- if fgExportIgnore args
    then filterExportIgnore repoRootFp files0
    else pure files0
  liftIO $ traverse_ (copyTracked repoRootFp tmpDir) files

  narHash <- liftIO $ computeNarHashSRI tmpDir
  storeName <- mkStoreName "builtins.fetchGit" (fgName args)
  -- Instrument addToStore for IO timing
  storeStart <- liftIO Clock.getMonotonicTimeNSec
  res <- addToStore storeName (NarFile $ coerce tmpDir) True False
  storeEnd <- liftIO Clock.getMonotonicTimeNSec
  for_ mstats $ \stats -> recordStoreAdd stats (storeEnd - storeStart)
  storePath <- either throwError pure res
  liftIO $ Directory.removeDirectoryRecursive tmpDir

  let dirtyRev = (\r -> r <> "-dirty") <$> mHeadRev
  let dirtyShort = (\r -> Text.take 7 r <> "-dirty") <$> mHeadRev

  revInfo <- case (dirty, mHeadRev) of
    (True, _) -> pure Nothing
    (False, Nothing) ->
      pure $ Just RevInfo
        { riRev = "0000000000000000000000000000000000000000"
        , riRevCount = Just 0
        , riLastModSecs = Just 0
        }
    (False, Just rev) -> do
      info <- gitRevInfo repoRootFp rev (not (fgShallow args))
      when (fgVerifyCommit args) $
        verifyCommit repoRootFp rev (fgPublicKeys args)
      pure $ Just info

  lastMod <- case mHeadRev of
    Nothing -> pure 0
    Just rev -> do
      secs <- gitLastModified repoRootFp rev
      pure $ fromMaybe 0 secs

  pure FetchGitResult
    { fgrStorePath = storePath
    , fgrRevInfo = revInfo
    , fgrDirtyRev = if dirty then dirtyRev else Nothing
    , fgrDirtyShortRev = if dirty then dirtyShort else Nothing
    , fgrLastModified = lastMod
    , fgrNarHash = narHash
    }

fetchGitFromLocalRepo
  :: forall e t f m
   . MonadNix e t f m
  => FetchGitArgs
  -> FilePath
  -> m FetchGitResult
fetchGitFromLocalRepo args path = do
  mstats <- askEvalStats
  repoRoot <- runGitIn path ["rev-parse", "--show-toplevel"]
  let repoRootFp = toString repoRoot

  mRev <- resolveRev repoRootFp (fgRev args) (fgRef args)
  case (fgExportIgnore args, fgSubmodules args, mRev) of
    (True, False, Just rev) -> do
      tmpBase <- liftIO Temp.getCanonicalTemporaryDirectory
      tmpDir <- liftIO $ Temp.createTempDirectory tmpBase "hnix-fetchGit"
      let tarFile = tmpDir FP.</> "archive.tar"
      runGitArchive repoRootFp rev tarFile
      let unpackDir = tmpDir FP.</> "unpack"
      liftIO $ Directory.createDirectory unpackDir
      (exitCode, _out, err) <- liftIO $ readProcessWithExitCode "tar" ["-xf", tarFile, "-C", unpackDir] ""
      when (exitCode /= ExitSuccess) $
        throwError $ ErrorCall $ "builtins.fetchGit: failed to unpack archive: " <> err
      narHash <- liftIO $ computeNarHashSRI unpackDir
      storeName <- mkStoreName "builtins.fetchGit" (fgName args)
      -- Instrument addToStore for IO timing
      storeStart <- liftIO Clock.getMonotonicTimeNSec
      res <- addToStore storeName (NarFile $ coerce unpackDir) True False
      storeEnd <- liftIO Clock.getMonotonicTimeNSec
      for_ mstats $ \stats -> recordStoreAdd stats (storeEnd - storeStart)
      storePath <- either throwError pure res
      liftIO $ Directory.removeDirectoryRecursive tmpDir

      when (fgVerifyCommit args) $
        verifyCommit repoRootFp rev (fgPublicKeys args)

      info <- gitRevInfo repoRootFp rev (not (fgShallow args))
      lastMod <- maybe 0 id <$> pure (riLastModSecs info)
      pure FetchGitResult
        { fgrStorePath = storePath
        , fgrRevInfo = Just info
        , fgrDirtyRev = Nothing
        , fgrDirtyShortRev = Nothing
        , fgrLastModified = lastMod
        , fgrNarHash = narHash
        }
    _ ->
      fetchGitCloneRepo args (stripGitPrefix $ fgUrl args)

fetchGitCloneRepo
  :: forall e t f m
   . MonadNix e t f m
  => FetchGitArgs
  -> Text
  -> m FetchGitResult
fetchGitCloneRepo args cloneUrl = do
  mstats <- askEvalStats
  tmpBase <- liftIO Temp.getCanonicalTemporaryDirectory
  tmpDir <- liftIO $ Temp.createTempDirectory tmpBase "hnix-fetchGit"
  let repoDir = tmpDir FP.</> "repo"
  runGit ["init", repoDir]
  runGitIn repoDir ["remote", "add", "origin", toString cloneUrl]

  effectiveRef <- case fgRev args of
    Just _ -> pure Nothing
    Nothing -> case fgRef args of
      Just r -> pure $ Just r
      Nothing -> Just <$> getDefaultRef cloneUrl (fgShallow args)

  let refspec = buildRefspec (fgRev args) (fgAllRefs args) effectiveRef
  runGitFetch repoDir cloneUrl refspec (fgShallow args)

  case fgRev args of
    Just rev -> void $ runGitIn repoDir ["checkout", "--detach", toString rev]
    Nothing -> case effectiveRef of
      Nothing -> pure ()
      Just ref -> void $ runGitIn repoDir ["checkout", toString ref]

  when (fgSubmodules args) $
    void $ runGitIn repoDir ["submodule", "update", "--init", "--recursive"]

  when (fgLfs args) $
    runGitLfs repoDir

  rev <- runGitIn repoDir ["rev-parse", "HEAD"]
  when (fgVerifyCommit args) $
    verifyCommit repoDir rev (fgPublicKeys args)

  info <- gitRevInfo repoDir rev (not (fgShallow args))
  let lastMod = fromMaybe 0 (riLastModSecs info)

  contentDir <- if fgExportIgnore args
    then do
      let tarFile = tmpDir FP.</> "archive.tar"
      runGitArchive repoDir rev tarFile
      let unpackDir = tmpDir FP.</> "archive"
      liftIO $ Directory.createDirectory unpackDir
      (exitCode, _out, err) <- liftIO $ readProcessWithExitCode "tar" ["-xf", tarFile, "-C", unpackDir] ""
      when (exitCode /= ExitSuccess) $
        throwError $ ErrorCall $ "builtins.fetchGit: failed to unpack archive: " <> err
      pure unpackDir
    else do
      liftIO $ removeGitMetadata repoDir
      pure repoDir

  narHash <- liftIO $ computeNarHashSRI contentDir

  storeName <- mkStoreName "builtins.fetchGit" (fgName args)
  -- Instrument addToStore for IO timing
  storeStart <- liftIO Clock.getMonotonicTimeNSec
  res <- addToStore storeName (NarFile $ coerce contentDir) True False
  storeEnd <- liftIO Clock.getMonotonicTimeNSec
  for_ mstats $ \stats -> recordStoreAdd stats (storeEnd - storeStart)
  storePath <- either throwError pure res
  liftIO $ Directory.removeDirectoryRecursive tmpDir
  pure FetchGitResult
    { fgrStorePath = storePath
    , fgrRevInfo = Just info
    , fgrDirtyRev = Nothing
    , fgrDirtyShortRev = Nothing
    , fgrLastModified = lastMod
    , fgrNarHash = narHash
    }

buildRefspec :: Maybe Text -> Bool -> Maybe Text -> Text
buildRefspec mRev allRefs mRef =
  case mRev of
    Just rev -> rev
    Nothing ->
      if allRefs
        then "refs/*:refs/*"
        else case mRef of
          Nothing -> "HEAD:HEAD"
          Just ref
            | ref == "HEAD" -> "HEAD:HEAD"
            | Text.isPrefixOf "refs/" ref -> ref <> ":" <> ref
            | otherwise -> "refs/heads/" <> ref <> ":refs/heads/" <> ref

getDefaultRef
  :: forall e t f m
   . MonadNix e t f m
  => Text
  -> Bool
  -> m Text
getDefaultRef url _shallow = do
  mPath <- liftIO $ urlToLocalPath (stripGitPrefix url)
  case mPath of
    Just path -> do
      mRef <- runGitInMaybe path ["symbolic-ref", "--quiet", "--short", "HEAD"]
      pure $ fromMaybe "master" mRef
    Nothing -> do
      mRef <- gitLsRemoteHead url
      pure $ fromMaybe "master" mRef

gitLsRemoteHead
  :: forall e t f m
   . MonadNix e t f m
  => Text
  -> m (Maybe Text)
gitLsRemoteHead url = do
  mstats <- askEvalStats
  -- Instrument git ls-remote for IO timing
  start <- liftIO Clock.getMonotonicTimeNSec
  (exitCode, out, _err) <- liftIO $
    readProcessWithExitCode "git" ["ls-remote", "--symref", toString url, "HEAD"] ""
  end <- liftIO Clock.getMonotonicTimeNSec
  for_ mstats $ \stats -> recordHttpFetch stats (end - start)
  case exitCode of
    ExitSuccess -> pure $ parseLsRemoteHead out
    ExitFailure _ -> pure Nothing
 where
  parseLsRemoteHead :: String -> Maybe Text
  parseLsRemoteHead s =
    let ls = Text.lines (Text.pack s)
        mLine = find (Text.isPrefixOf "ref:") ls
    in mLine >>= \line ->
        case Text.words line of
          ("ref:":ref:_) -> Just ref
          _ -> Nothing

gitRevInfo
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> Text
  -> Bool
  -> m RevInfo
gitRevInfo repo rev includeRevCount = do
  mCount <-
    if includeRevCount
      then do
        revCountText <- runGitIn repo ["rev-list", "--count", toString rev]
        pure $ readMaybe (toString revCountText)
      else pure Nothing
  lastModText <- runGitIn repo ["show", "-s", "--format=%ct", toString rev]
  let mLastMod = readMaybe (toString lastModText)
  pure $ RevInfo { riRev = rev, riRevCount = mCount, riLastModSecs = mLastMod }

gitLastModified
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> Text
  -> m (Maybe Integer)
gitLastModified repo rev = do
  lastModText <- runGitIn repo ["show", "-s", "--format=%ct", toString rev]
  pure $ readMaybe (toString lastModText)

runGitArchive
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> Text
  -> FilePath
  -> m ()
runGitArchive repo rev outFile = do
  (exitCode, _out, err) <- liftIO $
    readProcessWithExitCode "git" ["-C", repo, "archive", "--format=tar", "-o", outFile, toString rev] ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      throwError $ ErrorCall $ "builtins.fetchGit: git archive failed: " <> err

resolveRev
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> Maybe Text
  -> Maybe Text
  -> m (Maybe Text)
resolveRev _ (Just rev) _ = pure (Just rev)
resolveRev repo Nothing mRef =
  traverse (\ref -> runGitIn repo ["rev-parse", toString ref]) mRef

gitLsFiles
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> Bool
  -> m [FilePath]
gitLsFiles repo submodules = do
  let args = ["ls-files", "-z"] <> ["--recurse-submodules" | submodules]
  raw <- runGitRaw repo args
  pure $ filter (not . null) (splitNull raw)

filterExportIgnore
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> [FilePath]
  -> m [FilePath]
filterExportIgnore repo files = do
  let input = intercalate "\0" files <> "\0"
  (exitCode, out, err) <- liftIO $
    readProcessWithExitCode "git" ["-C", repo, "check-attr", "-z", "export-ignore", "--stdin"] input
  case exitCode of
    ExitFailure _ ->
      throwError $ ErrorCall $ "builtins.fetchGit: git check-attr failed: " <> err
    ExitSuccess -> do
      let triples = toTriples (splitNull out)
      let ignored = [p | (p, _, v) <- triples, v == "set"]
      pure $ filter (`notElem` ignored) files
 where
  toTriples :: [String] -> [(String, String, String)]
  toTriples (a:b:c:rest) = (a, b, c) : toTriples rest
  toTriples _ = []

splitNull :: String -> [String]
splitNull [] = []
splitNull s =
  let (chunk, rest) = break (== '\0') s
      rest' = drop 1 rest
  in chunk : splitNull rest'

copyTracked :: FilePath -> FilePath -> FilePath -> IO ()
copyTracked repoRoot dest rel = do
  let src = repoRoot FP.</> rel
  let dst = dest FP.</> rel
  Directory.createDirectoryIfMissing True (FP.takeDirectory dst)
  st <- Posix.getSymbolicLinkStatus src
  if Posix.isSymbolicLink st
    then do
      target <- Posix.readSymbolicLink src
      Posix.createSymbolicLink target dst
    else do
      BS.readFile src >>= BS.writeFile dst
      Posix.setFileMode dst (Posix.fileMode st)

isRepoDirty
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> m Bool
isRepoDirty repo = do
  (exitCode, out, _err) <- liftIO $ readProcessWithExitCode "git" ["-C", repo, "status", "--porcelain"] ""
  case exitCode of
    ExitSuccess -> pure $ not (null out)
    ExitFailure _ -> pure False

gitHeadRev
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> m (Maybe Text)
gitHeadRev repo = runGitInMaybe repo ["rev-parse", "HEAD"]

runGitIn
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> [String]
  -> m Text
runGitIn repo args = do
  (exitCode, out, err) <- liftIO $ readProcessWithExitCode "git" (["-C", repo] <> args) ""
  case exitCode of
    ExitSuccess -> pure $ Text.strip $ Text.pack out
    ExitFailure _ ->
      throwError $ ErrorCall $ "builtins.fetchGit: git failed: " <> err

runGitInMaybe
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> [String]
  -> m (Maybe Text)
runGitInMaybe repo args = do
  (exitCode, out, _err) <- liftIO $ readProcessWithExitCode "git" (["-C", repo] <> args) ""
  case exitCode of
    ExitSuccess -> pure $ Just $ Text.strip $ Text.pack out
    ExitFailure _ -> pure Nothing

runGitRaw
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> [String]
  -> m String
runGitRaw repo args = do
  (exitCode, out, err) <- liftIO $ readProcessWithExitCode "git" (["-C", repo] <> args) ""
  case exitCode of
    ExitSuccess -> pure out
    ExitFailure _ ->
      throwError $ ErrorCall $ "builtins.fetchGit: git failed: " <> err

runGit
  :: forall e t f m
   . MonadNix e t f m
  => [String]
  -> m ()
runGit args = do
  (exitCode, _out, err) <- liftIO $ readProcessWithExitCode "git" args ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      throwError $ ErrorCall $ "builtins.fetchGit: git failed: " <> err

runGitFetch
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> Text
  -> Text
  -> Bool
  -> m ()
runGitFetch repo url refspec shallow = do
  mstats <- askEvalStats
  let baseArgs = ["-C", repo, "fetch", "--progress", "--force"]
  let depthArgs = if shallow then ["--depth", "1"] else []
  let args = baseArgs <> depthArgs <> ["--", toString url, toString refspec]
  -- Instrument git fetch for IO timing
  start <- liftIO Clock.getMonotonicTimeNSec
  runGit args
  end <- liftIO Clock.getMonotonicTimeNSec
  for_ mstats $ \stats -> recordHttpFetch stats (end - start)

runGitLfs
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> m ()
runGitLfs repo = do
  mstats <- askEvalStats
  -- Instrument git lfs fetch for IO timing
  start <- liftIO Clock.getMonotonicTimeNSec
  _ <- runGitIn repo ["lfs", "fetch"]
  end <- liftIO Clock.getMonotonicTimeNSec
  for_ mstats $ \stats -> recordHttpFetch stats (end - start)
  _ <- runGitIn repo ["lfs", "checkout"]
  pure ()

verifyCommit
  :: forall e t f m
   . MonadNix e t f m
  => FilePath
  -> Text
  -> [PublicKey]
  -> m ()
verifyCommit _ _ [] = pure ()
verifyCommit repo rev publicKeys = do
  tmpBase <- liftIO Temp.getCanonicalTemporaryDirectory
  tmpDir <- liftIO $ Temp.createTempDirectory tmpBase "hnix-fetchGit-keys"
  let keyFile = tmpDir FP.</> "allowed_signers"
  allowed <- either (throwError . ErrorCall . ("builtins.fetchGit: " <>)) pure $
    buildAllowedSigners publicKeys
  liftIO $ BS.writeFile keyFile (TE.encodeUtf8 allowed)
  (exitCode, out, _err) <- liftIO $
    readProcessWithExitCode "git"
      [ "-c"
      , "gpg.ssh.allowedSignersFile=" <> keyFile
      , "-C"
      , repo
      , "verify-commit"
      , toString rev
      ] ""
  liftIO $ Directory.removeDirectoryRecursive tmpDir
  case exitCode of
    ExitSuccess -> do
      let output = Text.pack out
      fingerprints <- either (throwError . ErrorCall . ("builtins.fetchGit: " <>)) pure $
        traverse computeKeyFingerprint publicKeys
      if any (\fp -> Text.isInfixOf ("SHA256:" <> fp) output) fingerprints
        then pure ()
        else throwError $ ErrorCall $ "builtins.fetchGit: commit signature verification failed"
    ExitFailure _ ->
      throwError $ ErrorCall $ "builtins.fetchGit: commit signature verification failed"

buildAllowedSigners :: [PublicKey] -> Either String Text
buildAllowedSigners keys = do
  let mapType t = case t of
        "ssh-dsa" -> Right "ssh-dsa"
        "ssh-ecdsa" -> Right "ssh-ecdsa"
        "ssh-ecdsa-sk" -> Right "sk-ecdsa-sha2-nistp256@openssh.com"
        "ssh-ed25519" -> Right "ssh-ed25519"
        "ssh-ed25519-sk" -> Right "sk-ssh-ed25519@openssh.com"
        "ssh-rsa" -> Right "ssh-rsa"
        _ -> Left ("Invalid SSH key type '" <> toString t <> "'")
  entries <- traverse
    (\(PublicKey t k) -> do
      t' <- mapType t
      pure $ "* " <> t' <> " " <> k
    ) keys
  pure $ Text.unlines entries

computeKeyFingerprint :: PublicKey -> Either String Text
computeKeyFingerprint (PublicKey _ pubKey) = do
  bytes <- StoreBase.decodeWith StoreBase.Base64 pubKey
  let digest = Crypto.Hash.hash @BS.ByteString @Crypto.Hash.SHA256 bytes
      b64 = StoreHash.encodeDigestWith StoreBase.Base64 digest
  pure $ Text.dropWhileEnd (== '=') b64

parsePublicKeys
  :: forall e t f m
   . MonadNix e t f m
  => Text
  -> Maybe Text
  -> Maybe [NValue t f m]
  -> m [PublicKey]
parsePublicKeys defaultType mKey mKeysList = do
  let single = maybe [] (\k -> [PublicKey defaultType k]) mKey
  keysFromList <- case mKeysList of
    Nothing -> pure []
    Just vals -> traverse parseEntry vals
  pure (single <> keysFromList)
 where
  parseEntry val = do
    v <- demand val
    case v of
      NVSet _ attrs -> do
        keyVal <- maybe (throwError $ ErrorCall "builtins.fetchGit: publicKeys entry missing 'key'") pure (HM.lookup "key" attrs)
        keyText <- fromValue =<< demand keyVal
        let typeVal = HM.lookup "type" attrs
        typ <- maybe (pure defaultType) (\tv -> fromValue =<< demand tv) typeVal
        pure $ PublicKey typ keyText
      _ ->
        throwError $ ErrorCall $ "builtins.fetchGit: publicKeys entry must be a set"

extractUrlLike
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> m Text
extractUrlLike = \case
  NVStr ns ->
    maybe
      (throwError $ ErrorCall "builtins.fetchGit: unsupported arguments to url")
      pure
      (getStringNoContext ns)
  NVPath p -> pure $ toText p
  v -> throwError $ ErrorCall $ "builtins.fetchGit: Expected URI string, got " <> show v

mkStoreName
  :: forall e t f m
   . MonadNix e t f m
  => Text
  -> Text
  -> m Text
mkStoreName label name =
  case Store.mkStorePathName name of
    Left err ->
      throwError $ ErrorCall $ toString label <> ": invalid store path name '" <> show name <> "': " <> show err
    Right _ -> pure name

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText = \case
  Just t | Text.null t -> Nothing
  other -> other

stripGitPrefix :: Text -> Text
stripGitPrefix url = fromMaybe url (Text.stripPrefix "git+" url)

classifyGitLocation
  :: forall e t f m
   . MonadNix e t f m
  => FetchGitArgs
  -> m GitLocation
classifyGitLocation args = do
  let url = stripGitPrefix (fgUrl args)
  let hasScheme = "://" `Text.isInfixOf` url
  let isFile = "file://" `Text.isPrefixOf` url
  forceHttp <- liftIO $ lookupEnvBool "_NIX_FORCE_HTTP"
  case () of
    _
      | isFile -> do
          let path = toString $ stripFileQuery $ Text.drop (Text.length "file://") url
          bare <- liftIO $ isBareRepository path
          if forceHttp || bare
            then pure $ GitRemote (sanitizeFileUrl url)
            else chooseLocal path
      | hasScheme -> pure $ GitRemote url
      | otherwise -> chooseLocal (toString url)
 where
  chooseLocal path = do
    let hasRev = isJust (fgRev args) || isJust (fgRef args)
    bare <- liftIO $ isBareRepository path
    if hasRev || bare
      then pure $ GitLocalRepo path
      else pure $ GitWorkdir path

lookupEnvBool :: String -> IO Bool
lookupEnvBool name = do
  res <- Env.lookupEnv name
  pure $ res == Just "1"

stripFileQuery :: Text -> Text
stripFileQuery t = Text.takeWhile (/= '?') t

sanitizeFileUrl :: Text -> Text
sanitizeFileUrl url =
  case Text.stripPrefix "file://" url of
    Just rest -> "file://" <> stripFileQuery rest
    Nothing -> url

isBareRepository :: FilePath -> IO Bool
isBareRepository path = do
  exists <- Directory.doesDirectoryExist path
  if not exists
    then pure False
    else do
      hasDotGit <- Directory.doesDirectoryExist (path FP.</> ".git")
      pure $ not hasDotGit

urlToLocalPath :: Text -> IO (Maybe FilePath)
urlToLocalPath url =
  case Text.stripPrefix "file://" url of
    Just rest -> do
      let fp = toString $ stripFileQuery rest
      exists <- Directory.doesDirectoryExist fp
      pure $ if exists then Just fp else Nothing
    Nothing -> do
      let fp = toString url
      exists <- Directory.doesDirectoryExist fp
      pure $ if exists then Just fp else Nothing

removeGitMetadata :: FilePath -> IO ()
removeGitMetadata root = do
  let go dir = do
        entries <- Directory.listDirectory dir
        for_ entries $ \e -> do
          let path = dir FP.</> e
          if e == ".git"
            then do
              isDir <- Directory.doesDirectoryExist path
              if isDir
                then Directory.removeDirectoryRecursive path
                else Directory.removeFile path
            else do
              isDir <- Directory.doesDirectoryExist path
              when isDir (go path)
  go root

computeNarHashSRI :: FilePath -> IO Text
computeNarHashSRI path = do
  digest <- computeNarHash path
  pure $ "sha256-" <> StoreHash.encodeDigestWith StoreBase.Base64 digest

computeNarHash :: FilePath -> IO (Crypto.Hash.Digest Crypto.Hash.SHA256)
computeNarHash path =
  Crypto.Hash.hashFinalize
    <$> State.execStateT (streamNarUpdate path) (Crypto.Hash.hashInit @Crypto.Hash.SHA256)
 where
  streamNarUpdate p =
    Nar.streamNarIO
      Nar.narEffectsIO
      p
      (\chunk -> State.modify (\ctx -> Crypto.Hash.hashUpdate @BS.ByteString @Crypto.Hash.SHA256 ctx chunk))

parseSha256Digest :: MonadNix e t f m => Text -> m (Crypto.Hash.Digest Crypto.Hash.SHA256)
parseSha256Digest t =
  case StoreHash.mkNamedDigest "sha256" t of
    Left err -> throwError $ ErrorCall $ "builtins.fetchGit: " <> err
    Right (StoreHash.HashAlgo_SHA256 :=> d) -> pure d
    Right _ -> throwError $ ErrorCall "builtins.fetchGit: unsupported hash algorithm"

defaultFindPath :: MonadNix e t f m => Vector (NValue t f m) -> Path -> m Path
defaultFindPath = findPathM

findPathM
  :: forall e t f m
   . MonadNix e t f m
  => Vector (NValue t f m)
  -> Path
  -> m Path
findPathM = findPathBy existingPath
 where
  existingPath :: MonadEffects t f m => Path -> m (Maybe Path)
  existingPath path =
    do
      apath  <- toAbsolutePath @t @f path
      doesExist <- doesPathExist apath
      pure $ pure apath `whenTrue` doesExist

defaultImportPath
  :: (MonadNix e t f m, MonadState (HashMap Path NExprLoc, b) m, HasProvCfg (CtxCfg e))
  => Path
  -> m (NValue t f m)
defaultImportPath path =
  do
    traceM $ "Importing file " <> coerce path
    withFrame
      Info
      (ErrorCall $ "While importing file " <> show path)
      $ evalExprLoc =<<
          (maybe
            (either
              (\ err -> throwError $ ErrorCall . show $ fillSep ["Parse during import failed:", err])
              (\ expr ->
                do
                  modify $ first $ HM.insert path expr
                  pure expr
              )
              =<< parseNixFileLocWithTiming path
            )
            pure  -- return expr
            . HM.lookup path
          ) =<< gets fst
 where
  -- Parse a file with timing instrumentation for eval-stats
  parseNixFileLocWithTiming :: MonadNix e t f m => Path -> m (Result NExprLoc)
  parseNixFileLocWithTiming p = do
    mstats <- askEvalStats
    case mstats of
      Nothing -> parseNixFileLoc p
      Just stats -> do
        start <- liftIO Clock.getMonotonicTimeNSec
        result <- parseNixFileLoc p
        end <- liftIO Clock.getMonotonicTimeNSec
        recordFileRead stats (end - start)
        pure result

defaultPathToDefaultNix :: MonadNix e t f m => Path -> m Path
defaultPathToDefaultNix = pathToDefaultNixFile

-- Given a path, determine the nix file to load
pathToDefaultNixFile :: MonadFile m => Path -> m Path
pathToDefaultNixFile p =
  do
    isDir <- doesDirectoryExist p
    pure $ p </> "default.nix" `whenTrue` isDir

defaultTraceEffect :: MonadPutStr m => String -> m ()
defaultTraceEffect = Nix.Effects.putStrLn
