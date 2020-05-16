{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Nix.Render where

import           Prelude                 hiding ( readFile )

import           Control.Monad.Fail             ( MonadFail )
import           Control.Monad.Trans
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Text.Prettyprint.Doc
import           Data.Void
import           Nix.Expr.Types.Annotated
import qualified System.Directory              as S
import qualified System.Posix.Files            as S
import           Text.Megaparsec.Error
import           Text.Megaparsec.Pos

class MonadFail m => MonadFile m where
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
  readFile              = BS.readFile
  listDirectory         = S.listDirectory
  getCurrentDirectory   = S.getCurrentDirectory
  canonicalizePath      = S.canonicalizePath
  getHomeDirectory      = S.getHomeDirectory
  doesPathExist         = S.doesPathExist
  doesFileExist         = S.doesFileExist
  doesDirectoryExist    = S.doesDirectoryExist
  getSymbolicLinkStatus = S.getSymbolicLinkStatus

posAndMsg :: SourcePos -> Doc a -> ParseError s Void
posAndMsg (SourcePos _ lineNo _) msg = FancyError
  (unPos lineNo)
  (Set.fromList [ErrorFail (show msg) :: ErrorFancy Void])

renderLocation :: MonadFile m => SrcSpan -> Doc a -> m (Doc a)
renderLocation (SrcSpan (SourcePos file begLine begCol) (SourcePos file' endLine endCol)) msg
  | file /= "<string>" && file == file'
  = do
    exist <- doesFileExist file
    if exist
      then do
        txt <- sourceContext file begLine begCol endLine endCol msg
        return
          $ vsep
              [ "In file "
              <> errorContext file begLine begCol endLine endCol
              <> ":"
              , txt
              ]
      else return msg
renderLocation (SrcSpan beg end) msg =
  fail
    $  "Don't know how to render range from "
    ++ show beg
    ++ " to "
    ++ show end
    ++ " for error: "
    ++ show msg

errorContext :: FilePath -> Pos -> Pos -> Pos -> Pos -> Doc a
errorContext path bl bc _el _ec =
  pretty path <> ":" <> pretty (unPos bl) <> ":" <> pretty (unPos bc)

sourceContext
  :: MonadFile m => FilePath -> Pos -> Pos -> Pos -> Pos -> Doc a -> m (Doc a)
sourceContext path (unPos -> begLine) (unPos -> _begCol) (unPos -> endLine) (unPos -> _endCol) msg
  = do
    let beg' = max 1 (min begLine (begLine - 3))
        end' = max endLine (endLine + 3)
    ls <-
      map pretty
      .   take (end' - beg')
      .   drop (pred beg')
      .   T.lines
      .   T.decodeUtf8
      <$> readFile path
    let
      nums    = map (show . fst) $ zip [beg' ..] ls
      longest = maximum (map length nums)
      nums'   = flip map nums $ \n -> replicate (longest - length n) ' ' ++ n
      pad n | read n == begLine = "==> " ++ n
            | otherwise         = "    " ++ n
      ls' = zipWith (<+>)
                    (map (pretty . pad) nums')
                    (zipWith (<+>) (repeat "| ") ls)
    pure $ vsep $ ls' ++ [msg]
