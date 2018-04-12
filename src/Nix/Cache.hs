{-# LANGUAGE CPP #-}

module Nix.Cache where

import qualified Data.ByteString.Lazy as BS
import           Nix.Expr.Types.Annotated

#ifdef __linux__
#define USE_COMPACT 1
#endif

#ifdef USE_COMPACT
import qualified Data.Compact as C
import qualified Data.Compact.Serialize as C
#else
import qualified Codec.Serialise as S
#endif

readCache :: FilePath -> IO NExprLoc
readCache path = do
#ifdef USE_COMPACT
    eres <- C.unsafeReadCompact path
    case eres of
        Left err -> error $ "Error reading cache file: " ++ err
        Right expr -> return $ C.getCompact expr
#else
    eres <- S.deserialiseOrFail <$> BS.readFile path
    case eres of
        Left err -> error $ "Error reading cache file: " ++ show err
        Right expr -> return expr
#endif

writeCache :: FilePath -> NExprLoc -> IO ()
writeCache path expr =
#ifdef USE_COMPACT
    C.writeCompact path =<< C.compact expr
#else
    BS.writeFile path (S.serialise expr)
#endif
