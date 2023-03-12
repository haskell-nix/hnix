{-# language CPP #-}

module Nix.Json where

import           Nix.Prelude
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Encoding           as A
import qualified Data.Vector                   as V
import qualified Data.HashMap.Strict           as HM
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key                as AKM
import qualified Data.Aeson.KeyMap             as AKM
#endif
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
      . foldMap
      (\(k, v) -> A.pair k $ toEncodingSorted v)
      . sortWith fst $
#if MIN_VERSION_aeson(2,0,0)
          AKM.toList
#else
          HM.toList
#endif
            m
  A.Array l -> A.list toEncodingSorted $ V.toList l
  v         -> A.toEncoding v

toJSONNixString :: MonadNix e t f m => NValue t f m -> m NixString
toJSONNixString =
  runWithStringContextT .
    fmap
      ( decodeUtf8
      -- This is completely not optimal, but seems we do not have better encoding analog (except for @unsafe*@), Aeson gatekeeps through this.
      . A.encodingToLazyByteString
      . toEncodingSorted
      )

      . toJSON

toJSON :: MonadNix e t f m => NValue t f m -> WithStringContextT m A.Value
toJSON = \case
  NVConstant (NInt   n) -> pure $ A.toJSON n
  NVConstant (NFloat n) -> pure $ A.toJSON n
  NVConstant (NBool  b) -> pure $ A.toJSON b
  NVConstant NNull      -> pure   A.Null
  NVStr      ns         -> A.toJSON <$> extractNixString ns
  NVList l -> A.Array . V.fromList <$> traverse intoJson l
  NVSet _ m ->
    maybe
      (A.Object <$> traverse intoJson kmap)
      intoJson
      (lkup "outPath" kmap)
   where
#if MIN_VERSION_aeson(2,0,0)
    lkup = AKM.lookup
    kmap = AKM.fromHashMap $ HM.mapKeys (AKM.fromText . coerce) m
#else
    lkup = HM.lookup
    kmap = HM.mapKeys (coerce @VarName @Text) m
#endif
  NVPath p ->
    do
      fp <- lift $ coerce <$> addPath p
      addSingletonStringContext $ StringContext DirectPath $ fromString fp
      pure $ A.toJSON fp
  v -> lift $ throwError $ CoercionToJson v

 where
  intoJson :: MonadNix e t f m => NValue t f m -> WithStringContextT m A.Value
  intoJson nv = join $ lift $ toJSON <$> demand nv
