{-# language AllowAmbiguousTypes #-}
{-# language CPP #-}
{-# language DefaultSignatures #-}
{-# language TypeFamilies #-}
{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language UndecidableInstances #-}
{-# language PackageImports #-} -- 2021-07-05: Due to hashing Haskell IT system situation, in HNix we currently ended-up with 2 hash package dependencies @{hashing, cryptonite}@
-- {-# language OverloadedStrings#-}

{-# options_ghc -Wno-orphans #-}


module Nix.Effects where

import           Nix.Prelude             hiding ( putStrLn
                                                , print
                                                )
import qualified Nix.Prelude                   as Prelude
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import qualified Data.HashSet                  as HS
import qualified Data.Text                     as Text
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import           Network.HTTP.Client     hiding ( path, Proxy )
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import qualified "cryptonite" Crypto.Hash      as Hash
import           Nix.Utils.Fix1
import           Nix.Expr.Types.Annotated
import           Nix.Frames              hiding ( Proxy )
import           Nix.Parser
import           Nix.Render
import           Nix.Value
import qualified Paths_hnix
import           System.Exit
import qualified System.Info
import           System.Process

import qualified System.Nix.Store.Remote       as Store.Remote
import qualified System.Nix.StorePath          as Store
import qualified System.Nix.Nar                as Store.Nar

-- | A path into the nix store
newtype StorePath = StorePath Path


-- All of the following type classes defer to the underlying 'm'.

-- * @class MonadEffects t f m@

class
  ( MonadFile m
  , MonadStore m
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
  findPath :: [NValue t f m] -> Path -> m Path

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

    exec' :: [Text] -> m (Either ErrorCall NExprLoc)
    default exec' :: (MonadTrans t, MonadExec m', m ~ t m')
                  => [Text] -> m (Either ErrorCall NExprLoc)
    exec' = lift . exec'


-- ** Instances

instance MonadExec IO where
  exec' = \case
    []            -> pure $ Left $ ErrorCall "exec: missing program"
    (prog : args) -> do
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

-- ** Instances

instance MonadHttp IO where
  getURL url =
    do
      let urlstr = toString url
      traceM $ "fetching HTTP URL: " <> urlstr
      req     <- parseRequest urlstr
      manager <-
        bool
          (newManager defaultManagerSettings)
          newTlsManager
          (secure req)
      response <- httpLbs (req { method = "GET" }) manager
      let status = statusCode $ responseStatus response
      let body = responseBody response
      let digest::Hash.Digest Hash.SHA256 = Hash.hash $ (B.concat . BL.toChunks) body
      let name = baseNameOf url
      bool 
        (pure $ Left $ ErrorCall $ "fail, got " <> show status <> " when fetching url = " <> urlstr) 
        -- using addTextToStore' result in different hash from the nix-instantiate.
        -- have no idea why.
        -- (addTextToStore' name (decodeUtf8 body) mempty False)
        -- the current computation of hash is the same with nix-instantiate.
        (either (\ err -> pure $ Left $ ErrorCall $ "name: '" <> toString name <> "' is not a valid path name: " <> err)
              (pure . Right. toStorePath . Store.makeFixedOutputPath "/nix/store" False digest)
              (Store.makeStorePathName name))
        (status == 200)


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

-- ** @class MonadStore m@

class
  Monad m
  => MonadStore m where

  -- | Copy the contents of a local path to the store.  The resulting store
  -- path is returned.  Note: This does not support yet support the expected
  -- `filter` function that allows excluding some files.
  addToStore :: StorePathName -> Path -> RecursiveFlag -> RepairFlag -> m (Either ErrorCall StorePath)
  default addToStore :: (MonadTrans t, MonadStore m', m ~ t m') => StorePathName -> Path -> RecursiveFlag -> RepairFlag -> m (Either ErrorCall StorePath)
  addToStore a b c d = lift $ addToStore a b c d

  -- | Like addToStore, but the contents written to the output path is a
  -- regular file containing the given string.
  addTextToStore' :: StorePathName -> Text -> Store.StorePathSet -> RepairFlag -> m (Either ErrorCall StorePath)
  default addTextToStore' :: (MonadTrans t, MonadStore m', m ~ t m') => StorePathName -> Text -> Store.StorePathSet -> RepairFlag -> m (Either ErrorCall StorePath)
  addTextToStore' a b c d = lift $ addTextToStore' a b c d

-- conversion from Store.StorePath to Effects.StorePath, different type with the same name.
toStorePath :: Store.StorePath -> StorePath
toStorePath = StorePath . coerce . decodeUtf8 @FilePath @ByteString . Store.storePathToRawFilePath
-- *** Instances

instance MonadStore IO where

  addToStore name path recursive repair =
    either
      (\ err -> pure $ Left $ ErrorCall $ "String '" <> show name <> "' is not a valid path name: " <> err)
      (\ pathName ->
        do
          -- TODO: redesign the filter parameter
          res <- Store.Remote.runStore $ Store.Remote.addToStore @Hash.SHA256 pathName (Store.Nar.dumpPath $ coerce path) recursive repair 
          either
            Left -- err
            (pure . toStorePath) -- store path
            <$> parseStoreResult "addToStore" res
      )
      (Store.makeStorePathName name)

  addTextToStore' name text references repair =
    do
      res <- Store.Remote.runStore $ Store.Remote.addTextToStore name text references repair
      either
        Left -- err
        (pure . toStorePath) -- path
        <$> parseStoreResult "addTextToStore" res


-- ** Functions

parseStoreResult :: Monad m => Text -> (Either String a, [Store.Remote.Logger]) -> m (Either ErrorCall a)
parseStoreResult name (res, logs) =
  pure $
    either
      (\ msg -> Left $ ErrorCall $ "Failed to execute '" <> toString name <> "': " <> msg <> "\n" <> show logs)
      pure
      res

addTextToStore :: (Framed e m, MonadStore m) => StorePathName -> Text -> Store.StorePathSet -> RepairFlag -> m StorePath
addTextToStore a b c d =
  either
    throwError
    pure
    =<< addTextToStore' a b c d

--  2021-10-30: NOTE: Misleading name, please rename.
-- | Add @Path@ into the Nix Store
addPath :: (Framed e m, MonadStore m) => Path -> m StorePath
addPath p =
  either
    throwError
    pure
    =<< addToStore (fromString $ coerce takeFileName p) p True False

toFile_ :: (Framed e m, MonadStore m) => Path -> Text -> m StorePath
toFile_ p contents = addTextToStore (fromString $ coerce p) contents mempty False
