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

module Nix.Monad.Instance where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import           Data.Fix
import qualified Data.HashMap.Lazy as M
import           Data.IORef
import           Data.List
import           Data.List.Split
import           Data.Text (Text)
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Eval
import           Nix.Monad
import           Nix.Parser
import           Nix.Scope
import           Nix.Stack
import           Nix.Thunk
import           Nix.Utils
import           System.Directory
import           System.Environment
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath
import qualified System.Info
import           System.Process (readProcessWithExitCode)
import           System.Posix.Files

data Context m v = Context
    { scopes :: Scopes m v
    , frames :: Frames
    }

newtype Lazy m a = Lazy
    { runLazy :: ReaderT (Context (Lazy m) (NThunk (Lazy m))) m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO,
              MonadReader (Context (Lazy m) (NThunk (Lazy m))))

instance Has (Context m v) (Scopes m v) where
    hasLens f (Context x y) = flip Context y <$> f x

instance Has (Context m v) Frames where
    hasLens f (Context x y) = Context x <$> f y

-- | Incorrectly normalize paths by rewriting patterns like @a/b/..@ to @a@.
-- This is incorrect on POSIX systems, because if @b@ is a symlink, its parent
-- may be a different directory from @a@.  See the discussion at
-- https://hackage.haskell.org/package/directory-1.3.1.5/docs/System-Directory.html#v:canonicalizePath
removeDotDotIndirections :: FilePath -> FilePath
removeDotDotIndirections = intercalate "/" . go [] . splitOn "/"
    where go s [] = reverse s
          go (_:s) ("..":rest) = go s rest
          go s (this:rest) = go (this:s) rest

instance (MonadFix m, MonadNix (Lazy m), MonadIO m)
      => MonadExpr (NThunk (Lazy m)) (NValue (Lazy m)) (Lazy m) where
    embedSet    = return . NVSet
    projectSet  = \case
        NVSet s -> return $ Just s
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

    newVar   = liftIO . newIORef
    readVar  = liftIO . readIORef
    writeVar = (liftIO .) . writeIORef

instance MonadIO m => MonadFile (Lazy m) where
    readFile = liftIO . BS.readFile

instance (MonadFix m, MonadIO m) => MonadNix (Lazy m) where
    addPath path = liftIO $ do
        (exitCode, out, _) <-
            readProcessWithExitCode "nix-store" ["--add", path] ""
        case exitCode of
          ExitSuccess -> do
            let dropTrailingLinefeed p = take (length p - 1) p
            return $ StorePath $ dropTrailingLinefeed out
          _ -> error $ "No such file or directory: " ++ show path

    makeAbsolutePath origPath = do
        absPath <- if isAbsolute origPath then pure origPath else do
            cwd <- do
                mres <- lookupVar @_ @(NThunk (Lazy m)) "__cur_file"
                case mres of
                    Nothing -> liftIO getCurrentDirectory
                    Just v -> force v >>= \case
                        NVLiteralPath s -> return $ takeDirectory s
                        v -> throwError $ "when resolving relative path,"
                                ++ " __cur_file is in scope,"
                                ++ " but is not a path; it is: "
                                ++ show (void v)
            pure $ cwd </> origPath
        liftIO $ removeDotDotIndirections <$> canonicalizePath absPath

    -- jww (2018-03-29): Cache which files have been read in.
    importFile = force >=> \case
        NVLiteralPath path -> do
            mres <- lookupVar @(Context (Lazy m) (NThunk (Lazy m)))
                             "__cur_file"
            path' <- case mres of
                Nothing  -> do
                    traceM "No known current directory"
                    return path
                Just p -> force p >>= normalForm >>= \case
                    Fix (NVLiteralPath p') -> do
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
                        let ref = valueThunk @(Lazy m) (NVLiteralPath path')
                        -- Use this cookie so that when we evaluate the next
                        -- import, we'll remember which directory its containing
                        -- file was in.
                        pushScope (M.singleton "__cur_file" ref)
                                  (framedEvalExpr eval expr)
        p -> error $ "Unexpected argument to import: " ++ show (void p)

    getEnvVar = force >=> \case
        NVStr s _ -> do
            mres <- liftIO $ lookupEnv (Text.unpack s)
            return $ case mres of
                Nothing -> NVStr "" mempty
                Just v  -> NVStr (Text.pack v) mempty
        p -> error $ "Unexpected argument to getEnv: " ++ show (void p)

    getCurrentSystemOS = return $ Text.pack System.Info.os

    -- Invert the conversion done by GHC_CONVERT_CPU in GHC's aclocal.m4
    getCurrentSystemArch = return $ Text.pack $ case System.Info.arch of
      "i386" -> "i686"
      arch -> arch

    listDirectory         = liftIO . System.Directory.listDirectory
    getSymbolicLinkStatus = liftIO . System.Posix.Files.getSymbolicLinkStatus

runLazyM :: MonadIO m => Lazy m a -> m a
runLazyM = flip runReaderT (Context emptyScopes []) . runLazy
