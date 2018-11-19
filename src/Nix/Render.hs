{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nix.Render where

import           Prelude hiding (readFile)

import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Set as Set
import           Data.Void
import           Nix.Expr.Types.Annotated
import qualified System.Posix.Files as S
import qualified System.Directory as S
import           Text.Megaparsec.Error
import           Text.Megaparsec.Pos (SourcePos(..))
import           Text.PrettyPrint.ANSI.Leijen

class Monad m => MonadFile m where
    readFile :: FilePath -> m ByteString
    default readFile :: (MonadTrans t, MonadFile m', m ~ t m') => FilePath -> m ByteString
    readFile = lift . readFile
    listDirectory :: FilePath -> m [FilePath]
    default listDirectory :: (MonadTrans t, MonadFile m', m ~ t m') => FilePath -> m [FilePath]
    listDirectory = lift . listDirectory
    getCurrentDirectory :: m FilePath
    default getCurrentDirectory :: (MonadTrans t, MonadFile m', m ~ t m') => m FilePath
    getCurrentDirectory = lift getCurrentDirectory
    canonicalizePath :: FilePath -> m FilePath
    default canonicalizePath :: (MonadTrans t, MonadFile m', m ~ t m') => FilePath -> m FilePath
    canonicalizePath = lift . canonicalizePath
    getHomeDirectory :: m FilePath
    default getHomeDirectory :: (MonadTrans t, MonadFile m', m ~ t m') => m FilePath
    getHomeDirectory = lift getHomeDirectory
    doesPathExist :: FilePath -> m Bool
    default doesPathExist :: (MonadTrans t, MonadFile m', m ~ t m') => FilePath -> m Bool
    doesPathExist = lift . doesPathExist
    doesFileExist :: FilePath -> m Bool
    default doesFileExist :: (MonadTrans t, MonadFile m', m ~ t m') => FilePath -> m Bool
    doesFileExist = lift . doesFileExist
    doesDirectoryExist :: FilePath -> m Bool
    default doesDirectoryExist :: (MonadTrans t, MonadFile m', m ~ t m') => FilePath -> m Bool
    doesDirectoryExist = lift . doesDirectoryExist
    getSymbolicLinkStatus :: FilePath -> m S.FileStatus
    default getSymbolicLinkStatus :: (MonadTrans t, MonadFile m', m ~ t m') => FilePath -> m S.FileStatus
    getSymbolicLinkStatus = lift . getSymbolicLinkStatus

instance MonadFile IO where
    readFile = BS.readFile
    listDirectory = S.listDirectory
    getCurrentDirectory = S.getCurrentDirectory
    canonicalizePath = S.canonicalizePath
    getHomeDirectory = S.getHomeDirectory
    doesPathExist = S.doesPathExist
    doesFileExist = S.doesFileExist
    doesDirectoryExist = S.doesDirectoryExist
    getSymbolicLinkStatus = S.getSymbolicLinkStatus

posAndMsg :: SourcePos -> Doc -> ParseError t Void
posAndMsg beg msg =
    FancyError (beg :| [])
        (Set.fromList [ErrorFail (show msg) :: ErrorFancy Void])

renderLocation :: MonadFile m => SrcSpan -> Doc -> m Doc
renderLocation (SrcSpan beg@(SourcePos "<string>" _ _) _) msg =
    return $ text $ init $ parseErrorPretty @Char (posAndMsg beg msg)

renderLocation (SrcSpan beg@(SourcePos path _ _) _) msg = do
    contents <- Nix.Render.readFile path
    return $ text $ init $ parseErrorPretty' contents (posAndMsg beg msg)
