{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Monad.Lazy where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Trans.Reader
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import           Data.Fix
import qualified Data.HashMap.Lazy as M
import           Data.IORef
import           Data.List
import           Data.List.Split
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding
import           Nix.Atoms
import           Nix.Eval
import           Nix.Monad
import           Nix.Monad.Context
import           Nix.Normal
import           Nix.Parser
import           Nix.Pretty
import           Nix.Scope
import           Nix.Stack
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value
import           System.Directory
import           System.Environment
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath
import qualified System.Info
import           System.Posix.Files
import           System.Process (readProcessWithExitCode)

newtype Lazy m a = Lazy
    { runLazy :: ReaderT (Context (Lazy m) (NThunk (Lazy m))) m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO,
              MonadReader (Context (Lazy m) (NThunk (Lazy m))))

instance (MonadFix m, MonadNix (Lazy m), MonadThrow m, MonadIO m)
      => MonadExpr (NThunk (Lazy m)) (NValue (Lazy m)) (Lazy m) where
    embedSet    = return . flip NVSet M.empty
    projectSet  = \case
        NVSet s _ -> return $ Just s
        _ -> return Nothing
    projectSetWithPos = \case
        NVSet s p -> return $ Just (s, p)
        _ -> return Nothing

    type MText (Lazy m) = (Text, DList Text)

    wrapText   = return . (, mempty)
    unwrapText = return . fst

    embedText   = return . uncurry NVStr
    projectText = \case
        NVConstant NNull -> return $ Just Nothing
        v -> fmap (Just . Just) . valueText True =<< normalForm v

instance MonadIO m => MonadVar (Lazy m) where
    type Var (Lazy m) = IORef

    newVar = liftIO . newIORef
    readVar = liftIO . readIORef
    writeVar = (liftIO .) . writeIORef
    atomicModifyVar = (liftIO .) . atomicModifyIORef

instance MonadIO m => MonadFile (Lazy m) where
    readFile = liftIO . BS.readFile

instance MonadCatch m => MonadCatch (Lazy m) where
    catch (Lazy (ReaderT m)) f = Lazy $ ReaderT $ \e ->
        catch (m e) ((`runReaderT` e) . runLazy . f)

instance MonadThrow m => MonadThrow (Lazy m) where
    throwM = Lazy . throwM

instance (MonadFix m, MonadThrow m, MonadIO m) => MonadNix (Lazy m) where
    addPath path = do
        (exitCode, out, _) <-
            liftIO $ readProcessWithExitCode "nix-store" ["--add", path] ""
        case exitCode of
          ExitSuccess -> do
            let dropTrailingLinefeed p = take (length p - 1) p
            return $ StorePath $ dropTrailingLinefeed out
          _ -> throwError $ "addPath: failed: nix-store --add " ++ show path

    makeAbsolutePath origPath = do
        absPath <- if isAbsolute origPath then pure origPath else do
            cwd <- do
                mres <- lookupVar @_ @(NThunk (Lazy m)) "__cur_file"
                case mres of
                    Nothing -> liftIO getCurrentDirectory
                    Just v -> force v $ \case
                        NVPath s -> return $ takeDirectory s
                        v -> throwError $ "when resolving relative path,"
                                ++ " __cur_file is in scope,"
                                ++ " but is not a path; it is: "
                                ++ show (void v)
            pure $ cwd </> origPath
        liftIO $ removeDotDotIndirections <$> canonicalizePath absPath

    findEnvPath name = getEnvVar name >>= \case
        Nothing ->
            throwError $ "file '" ++ name
                ++ "' was not found in the Nix search path"
                ++ " (add it using $NIX_PATH or -I)"
        Just path -> makeAbsolutePath path

    pathExists = liftIO . fileExist

    -- jww (2018-03-29): Cache which files have been read in.
    importPath scope origPath = do
        path <- liftIO $ pathToDefaultNixFile origPath
        mres <- lookupVar @(Context (Lazy m) (NThunk (Lazy m)))
                         "__cur_file"
        path' <- case mres of
            Nothing  -> do
                traceM "No known current directory"
                return path
            Just p -> force p $ normalForm >=> \case
                Fix (NVPath p') -> do
                    traceM $ "Current file being evaluated is: "
                        ++ show p'
                    return $ takeDirectory p' </> path
                x -> error $ "How can the current directory be: " ++ show x

        traceM $ "Importing file " ++ path'

        withStringContext ("While importing file " ++ show path') $ do
            eres <- Lazy $ parseNixFileLoc path'
            case eres of
                Failure err  -> error $ "Parse failed: " ++ show err
                Success expr -> do
                    let ref = valueThunk @(Lazy m) (NVPath path')
                    -- Use this cookie so that when we evaluate the next
                    -- import, we'll remember which directory its containing
                    -- file was in.
                    pushScope (M.singleton "__cur_file" ref)
                        (pushScope scope (framedEvalExpr eval expr))

    getEnvVar = liftIO . lookupEnv

    getCurrentSystemOS = return $ Text.pack System.Info.os

    -- Invert the conversion done by GHC_CONVERT_CPU in GHC's aclocal.m4
    getCurrentSystemArch = return $ Text.pack $ case System.Info.arch of
      "i386" -> "i686"
      arch -> arch

    listDirectory         = liftIO . System.Directory.listDirectory
    getSymbolicLinkStatus = liftIO . System.Posix.Files.getSymbolicLinkStatus

    derivationStrict v = liftIO $ do
        (exitCode, out, _) <-
            readProcessWithExitCode "nix-instantiate"
              [ "--eval"
              , "--json"
              , "-E", "derivationStrict " ++ show (prettyNixValue v) --TODO: use prettyNix to generate this
              ] ""
        case exitCode of
            ExitSuccess ->
                case A.eitherDecodeStrict $ encodeUtf8 $ Text.pack out of
                    Left e -> error $ "derivationStrict: error parsing JSON output of nix-instantiate: " ++ show e
                    Right v -> pure v
            _ -> error "derivationStrict: nix-instantiate failed"

runLazyM :: MonadIO m => Lazy m a -> m a
runLazyM = flip runReaderT (Context emptyScopes []) . runLazy

-- | Incorrectly normalize paths by rewriting patterns like @a/b/..@ to @a@.
--   This is incorrect on POSIX systems, because if @b@ is a symlink, its
--   parent may be a different directory from @a@. See the discussion at
--   https://hackage.haskell.org/package/directory-1.3.1.5/docs/System-Directory.html#v:canonicalizePath
removeDotDotIndirections :: FilePath -> FilePath
removeDotDotIndirections = intercalate "/" . go [] . splitOn "/"
    where go s [] = reverse s
          go (_:s) ("..":rest) = go s rest
          go s (this:rest) = go (this:s) rest
