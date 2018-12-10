{-# LANGUAGE FlexibleContexts #-}

module Nix.Json where

import qualified Data.Aeson as A
import qualified Data.HashSet as HS
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Nix.Atoms
import           Nix.Effects
import           Nix.Exec
import           Nix.Frames
import           Nix.String
import           Nix.Thunk
import           Nix.Value

nvalueToJSON
  :: MonadNix e m
  => NValue m
  -> m (HS.HashSet StringContext, A.Value)
nvalueToJSON v = case v of
    NVConstant a -> retEmpty $ case a of
        NInt n   -> A.toJSON n
        NFloat n -> A.toJSON n
        NBool b  -> A.toJSON b
        NNull    -> A.Null
    NVStr ns  -> pure (principledGetContext ns, A.toJSON $ principledStringIgnoreContext ns)
    NVList l  -> do
        (ctxs, vals) <- unzip <$> traverse (`force` nvalueToJSON) l
        return (HS.unions ctxs, A.Array $ V.fromList vals)
    NVSet m _ ->
        fmap A.Object . sequence <$> traverse (`force` nvalueToJSON) m
    NVPath p  -> do
      fp <- unStorePath <$> addPath p
      return (HS.singleton $ StringContext (Text.pack fp) DirectPath, A.toJSON fp)
    _ -> throwError $ CoercionToJson v
  where
    retEmpty a = pure (mempty, a)
