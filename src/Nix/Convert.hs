{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Convert where

import           Control.Monad
import           Data.Aeson (toJSON)
import qualified Data.Aeson as A
import           Data.ByteString
import           Data.Fix
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Vector as V
import           Nix.Atoms
import           Nix.Effects
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Normal
import           Nix.Stack
import           Nix.Thunk
import           Nix.Value

class FromNix a m v where
    fromNix    :: (Framed e m, MonadVar m, MonadFile m) => v -> m a
    fromNixMay :: (Framed e m, MonadVar m, MonadFile m) => v -> m (Maybe a)

instance FromNix Bool m (NValueNF m) where
    fromNixMay = \case
        Fix (NVConstant (NBool b)) -> pure $ Just b
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a bool, but saw: " ++ show v

instance FromNix Bool m (NValue m) where
    fromNixMay = \case
        NVConstant (NBool b) -> pure $ Just b
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a bool, but saw: " ++ show v

instance FromNix Int m (NValueNF m) where
    fromNixMay = \case
        Fix (NVConstant (NInt b)) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an integer, but saw: " ++ show v

instance FromNix Int m (NValue m) where
    fromNixMay = \case
        NVConstant (NInt b) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an integer, but saw: " ++ show v

instance FromNix Integer m (NValueNF m) where
    fromNixMay = \case
        Fix (NVConstant (NInt b)) -> pure $ Just b
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an integer, but saw: " ++ show v

instance FromNix Integer m (NValue m) where
    fromNixMay = \case
        NVConstant (NInt b) -> pure $ Just b
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an integer, but saw: " ++ show v

instance FromNix Float m (NValueNF m) where
    fromNixMay = \case
        Fix (NVConstant (NFloat b)) -> pure $ Just b
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a float, but saw: " ++ show v

instance FromNix Float m (NValue m) where
    fromNixMay = \case
        NVConstant (NFloat b) -> pure $ Just b
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a float, but saw: " ++ show v

instance FromNix Text m (NValueNF m) where
    fromNixMay = \case
        Fix (NVStr t _) -> pure $ Just t
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a string, but saw: " ++ show v

instance FromNix Text m (NValue m) where
    fromNixMay = \case
        NVStr t _ -> pure $ Just t
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a string, but saw: " ++ show v

instance FromNix ByteString m (NValueNF m) where
    fromNixMay = \case
        Fix (NVStr t _) -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a string, but saw: " ++ show v

instance FromNix ByteString m (NValue m) where
    fromNixMay = \case
        NVStr t _ -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a string, but saw: " ++ show v

newtype Path = Path { getPath :: FilePath }
    deriving Show

instance FromNix Path m (NValueNF m) where
    fromNixMay = \case
        Fix (NVPath p) -> pure $ Just (Path p)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a path, but saw: " ++ show v

instance FromNix Path m (NValue m) where
    fromNixMay = \case
        NVPath p -> pure $ Just (Path p)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a path, but saw: " ++ show v

instance (FromNix a m (NValueNF m), Show a)
      => FromNix [a] m (NValueNF m) where
    fromNixMay = \case
        Fix (NVList l) -> sequence <$> traverse fromNixMay l
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

-- instance (MonadThunk (NValue m) (NThunk m) m,
--           FromNix a m (NValue m), Show a) => FromNix [a] m (NValue m) where
--     fromNixMay = \case
--         NVList l -> sequence <$> traverse (`force` fromNixMay) l
--         _ -> pure Nothing
--     fromNix = fromNixMay >=> \case
--         Just b -> pure b
--         v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance FromNix [NThunk m] m (NValue m) where
    fromNixMay = \case
        NVList l -> pure $ Just l
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance FromNix (HashMap Text (NValueNF m)) m (NValueNF m) where
    fromNixMay = \case
        Fix (NVSet s _) -> pure $ Just s
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

-- instance (MonadThunk (NValue m) (NThunk m) m,
--           FromNix a m (NValue m), Show a)
--       => FromNix (HashMap Text a) m (NValue m) where
--     fromNixMay = \case
--         NVSet s _ -> sequence <$> traverse (`force` fromNixMay) s
--         _ -> pure Nothing
--     fromNix = fromNixMay >=> \case
--         Just b -> pure b
--         v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance FromNix (HashMap Text (NThunk m)) m (NValue m) where
    fromNixMay = \case
        NVSet s _ -> pure $ Just s
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance FromNix (HashMap Text (NValueNF m),
                  HashMap Text SourcePos) m (NValueNF m) where
    fromNixMay = \case
        Fix (NVSet s p) -> pure $ Just (s, p)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance FromNix (HashMap Text (NThunk m),
                  HashMap Text SourcePos) m (NValue m) where
    fromNixMay = \case
        NVSet s p -> pure $ Just (s, p)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (MonadThunk (NValue m) (NThunk m) m)
      => FromNix (NThunk m) m (NValue m) where
    fromNixMay = pure . Just . value @_ @_ @m
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a thunk, but saw: " ++ show v

instance MonadEffects m => FromNix A.Value m (NValueNF m) where
    fromNixMay = \case
        Fix (NVConstant a) -> pure $ Just $ case a of
            NInt n   -> toJSON n
            NFloat n -> toJSON n
            NBool b  -> toJSON b
            NNull    -> A.Null
            NUri u   -> toJSON u
        Fix (NVStr s _)     -> pure $ Just $ toJSON s
        Fix (NVList l)      -> fmap (A.Array . V.fromList) . sequence
                                  <$> traverse fromNixMay l
        Fix (NVSet m _)     -> fmap A.Object . sequence <$> traverse fromNixMay m
        Fix NVClosure {}    -> pure Nothing
        Fix (NVPath p)      -> Just . toJSON . unStorePath <$> addPath p
        Fix (NVBuiltin _ _) -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Cannot convert value to JSON: " ++ show v

instance (MonadThunk (NValue m) (NThunk m) m, MonadEffects m)
      => FromNix A.Value m (NValue m) where
    fromNixMay = normalForm >=> fromNixMay
    fromNix    = normalForm >=> fromNix

class ToNix a v where
    toNix :: a -> v

instance ToNix Bool (NValueNF m) where
    toNix = Fix . NVConstant . NBool

instance ToNix Bool (NValue m) where
    toNix = NVConstant . NBool

instance ToNix Int (NValueNF m) where
    toNix = Fix . NVConstant . NInt . toInteger

instance ToNix Int (NValue m) where
    toNix = NVConstant . NInt . toInteger

instance ToNix Integer (NValueNF m) where
    toNix = Fix . NVConstant . NInt

instance ToNix Integer (NValue m) where
    toNix = NVConstant . NInt

instance ToNix Float (NValueNF m) where
    toNix = Fix . NVConstant . NFloat

instance ToNix Float (NValue m) where
    toNix = NVConstant . NFloat

instance ToNix Text (NValueNF m) where
    toNix = Fix . flip NVStr mempty

instance ToNix Text (NValue m) where
    toNix = flip NVStr mempty

instance ToNix ByteString (NValueNF m) where
    toNix = Fix . flip NVStr mempty . decodeUtf8

instance ToNix ByteString (NValue m) where
    toNix = flip NVStr mempty . decodeUtf8

instance ToNix Path (NValueNF m) where
    toNix = Fix . NVPath . getPath

instance ToNix Path (NValue m) where
    toNix = NVPath . getPath

instance MonadThunk (NValue m) (NThunk m) m
      => ToNix SourcePos (NValue m) where
    toNix (SourcePos f l c) =
        let f' = toNix @_ @(NValue m) (Text.pack f)
            l' = toNix (unPos l)
            c' = toNix (unPos c)
            pos = M.fromList
                [ ("file" :: Text, value @_ @_ @m f')
                , ("line",        value @_ @_ @m l')
                , ("column",      value @_ @_ @m c') ]
        in NVSet pos mempty

instance ToNix a (NValueNF m) => ToNix [a] (NValueNF m) where
    toNix = Fix . NVList . fmap toNix

-- instance (MonadThunk (NValue m) (NThunk m) m,
--           ToNix a (NValue m)) => ToNix [a] (NValue m) where
--     toNix = NVList . fmap toNix

instance ToNix [NThunk m] (NValue m) where
    toNix = NVList

instance ToNix (HashMap Text (NValueNF m)) (NValueNF m) where
    toNix = Fix . flip NVSet M.empty

-- instance (MonadThunk (NValue m) (NThunk m) m,
--           ToNix a (NValue m))
--       => ToNix (HashMap Text a) (NValue m) where
--     toNix = flip NVSet M.empty . fmap toNix

instance ToNix (HashMap Text (NThunk m)) (NValue m) where
    toNix = flip NVSet M.empty

instance ToNix (HashMap Text (NValueNF m),
                HashMap Text SourcePos) (NValueNF m) where
    toNix (s, p) = Fix $ NVSet s p

instance ToNix (HashMap Text (NThunk m),
                HashMap Text SourcePos) (NValue m) where
    toNix (s, p) = NVSet s p

instance (MonadThunk (NValue m) (NThunk m) m, ToNix a (NValue m))
      => ToNix a (NThunk m) where
    toNix = value @(NValue m) @_ @m . toNix

instance ToNix Bool (NExprF r) where
    toNix = NConstant . NBool

instance MonadThunk (NValue m) (NThunk m) m
      => ToNix A.Value (NValue m) where
    toNix = \case
        A.Object m -> flip NVSet M.empty $ fmap (value @_ @_ @m . toNix @_ @(NValue m)) m
        A.Array l -> NVList $ fmap (value @_ @_ @m . toNix) (V.toList l)
        A.String s -> NVStr s mempty
        A.Number n -> NVConstant $ case floatingOrInteger n of
            Left r -> NFloat r
            Right i -> NInt i
        A.Bool b -> NVConstant $ NBool b
        A.Null -> NVConstant NNull
