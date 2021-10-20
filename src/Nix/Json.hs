
module Nix.Json where

import qualified Data.Aeson                    as A
import qualified Data.Aeson.Encoding           as A
import qualified Data.Vector                   as V
import qualified Data.HashMap.Strict           as HM
import           Nix.Atoms
import           Nix.Effects
import           Nix.Exec
import           Nix.Frames
import           Nix.String
import           Nix.Value
import           Nix.Value.Monad
import           Nix.Expr.Types

-- This was moved from Utils.
toEncodingSorted :: A.Value -> A.Encoding
toEncodingSorted = \case
  A.Object m ->
    A.pairs
      . fold
      . ((\(k, v) -> A.pair k $ toEncodingSorted v) <$>)
      . sortWith fst
      $ HM.toList m
  A.Array l -> A.list toEncodingSorted $ V.toList l
  v         -> A.toEncoding v

nvalueToJSONNixString :: MonadNix e t f m => NValue t f m -> m NixString
nvalueToJSONNixString =
  runWithStringContextT .
    fmap
      ( decodeUtf8
      -- This is completely not optimal, but seems we do not have better encoding analog (except for @unsafe*@), Aeson gatekeeps through this.
      . A.encodingToLazyByteString
      . toEncodingSorted
      )

      . nvalueToJSON

nvalueToJSON :: MonadNix e t f m => NValue t f m -> WithStringContextT m A.Value
nvalueToJSON = \case
  NVConstant (NInt   n) -> pure $ A.toJSON n
  NVConstant (NFloat n) -> pure $ A.toJSON n
  NVConstant (NBool  b) -> pure $ A.toJSON b
  NVConstant NNull      -> pure   A.Null
  NVStr      ns         -> A.toJSON <$> extractNixString ns
  NVList l -> A.Array . V.fromList <$> traverse intoJson l
  NVSet _ m ->
    maybe
      (A.Object <$> traverse intoJson (HM.mapKeys (coerce @VarName @Text) m))
      intoJson
      (HM.lookup "outPath" m)
  NVPath p ->
    do
      fp <- lift $ coerce <$> addPath p
      addSingletonStringContext $ StringContext (fromString fp) DirectPath
      pure $ A.toJSON fp
  v -> lift $ throwError $ CoercionToJson v

 where
  intoJson :: MonadNix e t f m => NValue t f m -> WithStringContextT m A.Value
  intoJson nv = join $ lift $ nvalueToJSON <$> demand nv
