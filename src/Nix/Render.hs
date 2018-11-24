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
import           Data.Text.Prettyprint.Doc
import           Data.Void
import           Nix.Expr.Types.Annotated
import qualified System.Posix.Files as S
import qualified System.Directory as S
import           Text.Megaparsec.Error
import           Text.Megaparsec.Pos (SourcePos(..))

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

posAndMsg :: SourcePos -> Doc a -> ParseError s Void
posAndMsg (SourcePos _ lineNo _) msg =
    FancyError (unPos lineNo)
        (Set.fromList [ErrorFail (show msg) :: ErrorFancy Void])

renderLocation :: Monad m => SrcSpan -> Doc a -> m (Doc a)
renderLocation (SrcSpan beg@(SourcePos _ _ _) _) msg =
    return $ pretty $ init $ parseErrorPretty @String (posAndMsg beg msg)
