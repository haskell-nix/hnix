{-# LANGUAGE CPP #-}

-- | Reading and writing Nix cache files
module Nix.Cache where

import qualified Data.ByteString.Lazy          as BS
import           Nix.Expr.Types.Annotated

#if defined (__linux__)
#define USE_COMPACT 1
#endif

#ifdef USE_COMPACT
import qualified Data.Compact as C
import qualified Data.Compact.Serialize as C
#endif
#ifdef MIN_VERSION_serialise
import qualified Codec.Serialise               as S
#endif

readCache :: FilePath -> IO NExprLoc
readCache path = do
#if USE_COMPACT
    eres <- C.unsafeReadCompact path
    case eres of
        Left err -> error $ "Error reading cache file: " ++ err
        Right expr -> return $ C.getCompact expr
#else
#ifdef MIN_VERSION_serialise
  eres <- S.deserialiseOrFail <$> BS.readFile path
  case eres of
    Left  err  -> error $ "Error reading cache file: " ++ show err
    Right expr -> return expr
#else
    error "readCache not implemented for this platform"
#endif
#endif

writeCache :: FilePath -> NExprLoc -> IO ()
writeCache path expr =
#ifdef USE_COMPACT
  C.writeCompact path =<< C.compact expr
#else
#ifdef MIN_VERSION_serialise
  BS.writeFile path (S.serialise expr)
#else
    error "writeCache not implemented for this platform"
#endif
#endif
