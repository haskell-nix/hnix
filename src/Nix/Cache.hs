{-# language CPP #-}

-- | Reading and writing Nix cache files
module Nix.Cache where

import           Nix.Prelude
import qualified Data.ByteString.Lazy          as BSL
import           Nix.Expr.Types.Annotated

#if defined (__linux__)
-- This is about: https://hackage.haskell.org/package/compact
#define USE_COMPACT 1
#endif

#ifdef USE_COMPACT
import qualified Data.Compact                  as C
import qualified Data.Compact.Serialize        as C
#endif
import qualified Codec.Serialise               as S

readCache :: Path -> IO NExprLoc
readCache path = do
#if USE_COMPACT
  eres <- C.unsafeReadCompact path
  either
    (\ err  -> fail $ "Error reading cache file: " <> err)
    (\ expr -> pure $ C.getCompact expr)
    eres
#else
  eres <- S.deserialiseOrFail <$> BSL.readFile (coerce path)
  either
    (\ err  -> fail $ "Error reading cache file: " <> show err)
    pure
    eres
#endif

writeCache :: Path -> NExprLoc -> IO ()
writeCache path expr =
#ifdef USE_COMPACT
  C.writeCompact path =<< C.compact expr
#else
  BSL.writeFile (coerce path) (S.serialise expr)
#endif
