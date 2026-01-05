module Nix.FileType
  ( FileType(..)
  , fileTypeFromStatus
  ) where

import           Nix.Prelude
import qualified System.PosixCompat.Files      as S

data FileType
  = FileTypeRegular
  | FileTypeDirectory
  | FileTypeSymlink
  | FileTypeUnknown
  deriving (Show, Read, Eq, Ord)

fileTypeFromStatus :: S.FileStatus -> FileType
fileTypeFromStatus s
  | S.isRegularFile s  = FileTypeRegular
  | S.isDirectory s    = FileTypeDirectory
  | S.isSymbolicLink s = FileTypeSymlink
  | otherwise          = FileTypeUnknown
