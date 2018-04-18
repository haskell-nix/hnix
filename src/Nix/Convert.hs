{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
import           Nix.Utils
import           Nix.Value

class FromValue a m v where
    fromValue    :: v -> m a
    fromValueMay :: v -> m (Maybe a)

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue () m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant NNull) -> pure $ Just ()
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a null, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue () m (NValue m) where
    fromValueMay = \case
        NVConstant NNull -> pure $ Just ()
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a null, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Bool m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NBool b)) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a bool, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Bool m (NValue m) where
    fromValueMay = \case
        NVConstant (NBool b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a bool, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Int m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NInt b)) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Int m (NValue m) where
    fromValueMay = \case
        NVConstant (NInt b) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Integer m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NInt b)) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Integer m (NValue m) where
    fromValueMay = \case
        NVConstant (NInt b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Float m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NFloat b)) -> pure $ Just b
        Fix (NVConstant (NInt i)) -> pure $ Just (fromInteger i)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a float, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Float m (NValue m) where
    fromValueMay = \case
        NVConstant (NFloat b) -> pure $ Just b
        NVConstant (NInt i) -> pure $ Just (fromInteger i)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a float, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m, MonadEffects m)
      => FromValue Text m (NValueNF m) where
    fromValueMay = \case
        Fix (NVStr t _) -> pure $ Just t
        Fix (NVPath p) -> Just . Text.pack . unStorePath <$> addPath p
        Fix (NVSet s _) -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> Just <$> fromNix @Text p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m, MonadEffects m,
          MonadThunk (NValue m) (NThunk m) m)
      => FromValue Text m (NValue m) where
    fromValueMay = \case
        NVStr t _ -> pure $ Just t
        NVPath p -> Just . Text.pack . unStorePath <$> addPath p
        NVSet s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> Just <$> fromNix @Text p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m, MonadEffects m)
      => FromValue (Text, DList Text) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVStr t d) -> pure $ Just (t, d)
        Fix (NVPath p) -> Just . (,mempty) . Text.pack . unStorePath <$> addPath p
        Fix (NVSet s _) -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> Just . (,mempty) <$> fromNix @Text p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m, MonadEffects m,
          MonadThunk (NValue m) (NThunk m) m)
      => FromValue (Text, DList Text) m (NValue m) where
    fromValueMay = \case
        NVStr t d -> pure $ Just (t, d)
        NVPath p -> Just . (,mempty) . Text.pack . unStorePath <$> addPath p
        NVSet s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> Just . (,mempty) <$> fromNix @Text p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue ByteString m (NValueNF m) where
    fromValueMay = \case
        Fix (NVStr t _) -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue ByteString m (NValue m) where
    fromValueMay = \case
        NVStr t _ -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

newtype Path = Path { getPath :: FilePath }
    deriving Show

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Path m (NValueNF m) where
    fromValueMay = \case
        Fix (NVPath p) -> pure $ Just (Path p)
        Fix (NVStr s _) -> pure $ Just (Path (Text.unpack s))
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a path, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Path m (NValue m) where
    fromValueMay = \case
        NVPath p -> pure $ Just (Path p)
        NVStr s _ -> pure $ Just (Path (Text.unpack s))
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a path, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m,
          FromValue a m (NValueNF m), Show a)
      => FromValue [a] m (NValueNF m) where
    fromValueMay = \case
        Fix (NVList l) -> sequence <$> traverse fromValueMay l
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue [NThunk m] m (NValue m) where
    fromValueMay = \case
        NVList l -> pure $ Just l
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue (HashMap Text (NValueNF m)) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVSet s _) -> pure $ Just s
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue (HashMap Text (NThunk m)) m (NValue m) where
    fromValueMay = \case
        NVSet s _ -> pure $ Just s
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue (HashMap Text (NValueNF m),
                 HashMap Text SourcePos) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVSet s p) -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue (HashMap Text (NThunk m),
                 HashMap Text SourcePos) m (NValue m) where
    fromValueMay = \case
        NVSet s p -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (MonadThunk (NValue m) (NThunk m) m,
          Framed e m, MonadVar m, MonadFile m)
      => FromValue (NThunk m) m (NValue m) where
    fromValueMay = pure . Just . value @_ @_ @m
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a thunk, but saw: " ++ show v

instance (Monad m, FromValue a m (NValue m))
      => FromValue a m (m (NValue m)) where
    fromValueMay = (>>= fromValueMay)
    fromValue    = (>>= fromValue)

instance (MonadThunk (NValue m) (NThunk m) m, FromValue a m (NValue m))
      => FromValue a m (NThunk m) where
    fromValueMay = force ?? fromValueMay
    fromValue    = force ?? fromValue

instance (Framed e m, MonadVar m, MonadFile m, MonadEffects m)
      => FromValue A.Value m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant a) -> pure $ Just $ case a of
            NInt n   -> toJSON n
            NFloat n -> toJSON n
            NBool b  -> toJSON b
            NNull    -> A.Null
            NUri u   -> toJSON u
        Fix (NVStr s _)     -> pure $ Just $ toJSON s
        Fix (NVList l)      -> fmap (A.Array . V.fromList) . sequence
                                  <$> traverse fromValueMay l
        Fix (NVSet m _)     -> fmap A.Object . sequence <$> traverse fromValueMay m
        Fix NVClosure {}    -> pure Nothing
        Fix (NVPath p)      -> Just . toJSON . unStorePath <$> addPath p
        Fix (NVBuiltin _ _) -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Cannot convert value to JSON: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m,
          MonadThunk (NValue m) (NThunk m) m, MonadEffects m)
      => FromValue A.Value m (NValue m) where
    fromValueMay = normalForm >=> fromValueMay
    fromValue    = normalForm >=> fromValue

class ToValue a m v where
    toValue :: a -> m v

instance Applicative m => ToValue () m (NValueNF m) where
    toValue _ = pure . Fix . NVConstant $ NNull

instance Applicative m => ToValue () m (NValue m) where
    toValue _ = pure . NVConstant $ NNull

instance Applicative m => ToValue Bool m (NValueNF m) where
    toValue = pure . Fix . NVConstant . NBool

instance Applicative m => ToValue Bool m (NValue m) where
    toValue = pure . NVConstant . NBool

instance Applicative m => ToValue Int m (NValueNF m) where
    toValue = pure . Fix . NVConstant . NInt . toInteger

instance Applicative m => ToValue Int m (NValue m) where
    toValue = pure . NVConstant . NInt . toInteger

instance Applicative m => ToValue Integer m (NValueNF m) where
    toValue = pure . Fix . NVConstant . NInt

instance Applicative m => ToValue Integer m (NValue m) where
    toValue = pure . NVConstant . NInt

instance Applicative m => ToValue Float m (NValueNF m) where
    toValue = pure . Fix . NVConstant . NFloat

instance Applicative m => ToValue Float m (NValue m) where
    toValue = pure . NVConstant . NFloat

instance Applicative m => ToValue Text m (NValueNF m) where
    toValue = pure . Fix . flip NVStr mempty

instance Applicative m => ToValue Text m (NValue m) where
    toValue = pure . flip NVStr mempty

instance Applicative m => ToValue (Text, DList Text) m (NValueNF m) where
    toValue = pure . Fix . uncurry NVStr

instance Applicative m => ToValue (Text, DList Text) m (NValue m) where
    toValue = pure . uncurry NVStr

instance Applicative m => ToValue ByteString m (NValueNF m) where
    toValue = pure . Fix . flip NVStr mempty . decodeUtf8

instance Applicative m => ToValue ByteString m (NValue m) where
    toValue = pure . flip NVStr mempty . decodeUtf8

instance Applicative m => ToValue Path m (NValueNF m) where
    toValue = pure . Fix . NVPath . getPath

instance Applicative m => ToValue Path m (NValue m) where
    toValue = pure . NVPath . getPath

instance MonadThunk (NValue m) (NThunk m) m
      => ToValue SourcePos m (NValue m) where
    toValue (SourcePos f l c) = do
        f' <- toValue @_ @_ @(NValue m) (Text.pack f)
        l' <- toValue (unPos l)
        c' <- toValue (unPos c)
        let pos = M.fromList
                [ ("file" :: Text, value @_ @_ @m f')
                , ("line",        value @_ @_ @m l')
                , ("column",      value @_ @_ @m c') ]
        pure $ NVSet pos mempty

instance (ToValue a m (NValueNF m), Applicative m)
      => ToValue [a] m (NValueNF m) where
    toValue = fmap (Fix . NVList) . traverse toValue

instance Applicative m => ToValue [NThunk m] m (NValue m) where
    toValue = pure . NVList

instance Applicative m
      => ToValue (HashMap Text (NValueNF m)) m (NValueNF m) where
    toValue = pure . Fix . flip NVSet M.empty

instance Applicative m => ToValue (HashMap Text (NThunk m)) m (NValue m) where
    toValue = pure . flip NVSet M.empty

instance Applicative m => ToValue (HashMap Text (NValueNF m),
                HashMap Text SourcePos) m (NValueNF m) where
    toValue (s, p) = pure $ Fix $ NVSet s p

instance Applicative m => ToValue (HashMap Text (NThunk m),
                HashMap Text SourcePos) m (NValue m) where
    toValue (s, p) = pure $ NVSet s p

instance (MonadThunk (NValue m) (NThunk m) m, ToValue a m (NValue m))
      => ToValue a m (NThunk m) where
    toValue = fmap (value @(NValue m) @_ @m) . toValue

instance Applicative m => ToValue Bool m (NExprF r) where
    toValue = pure . NConstant . NBool

instance Applicative m => ToValue () m (NExprF r) where
    toValue _ = pure . NConstant $ NNull

instance MonadThunk (NValue m) (NThunk m) m
      => ToValue A.Value m (NValue m) where
    toValue = \case
        A.Object m -> flip NVSet M.empty
            <$> traverse (thunk . toValue @_ @_ @(NValue m)) m
        A.Array l -> NVList <$> traverse (thunk . toValue) (V.toList l)
        A.String s -> pure $ NVStr s mempty
        A.Number n -> pure $ NVConstant $ case floatingOrInteger n of
            Left r -> NFloat r
            Right i -> NInt i
        A.Bool b -> pure $ NVConstant $ NBool b
        A.Null -> pure $ NVConstant NNull

class FromNix a m v where
    fromNix :: v -> m a
    default fromNix :: FromValue a m v => v -> m a
    fromNix = fromValue

    fromNixMay :: v -> m (Maybe a)
    default fromNixMay :: FromValue a m v => v -> m (Maybe a)
    fromNixMay = fromValueMay

instance (Framed e m, MonadVar m, MonadFile m,
          MonadThunk (NValue m) (NThunk m) m,
          FromNix a m (NValue m), Show a) => FromNix [a] m (NValue m) where
    fromNixMay = \case
        NVList l -> sequence <$> traverse (`force` fromNixMay) l
        _ -> pure Nothing
    fromNix v = fromNixMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m,
          MonadThunk (NValue m) (NThunk m) m,
          FromNix a m (NValue m), Show a)
      => FromNix (HashMap Text a) m (NValue m) where
    fromNixMay = \case
        NVSet s _ -> sequence <$> traverse (`force` fromNixMay) s
        _ -> pure Nothing
    fromNix v = fromNixMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m) => FromNix () m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix () m (NValue m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix Bool m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix Bool m (NValue m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix Int m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix Int m (NValue m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix Integer m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix Integer m (NValue m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix Float m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix Float m (NValue m) where
instance (Framed e m, MonadVar m, MonadFile m, MonadEffects m) => FromNix Text m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m, MonadEffects m,
          MonadThunk (NValue m) (NThunk m) m) => FromNix Text m (NValue m) where
instance (Framed e m, MonadVar m, MonadFile m, MonadEffects m) => FromNix (Text, DList Text) m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m, MonadEffects m,
          MonadThunk (NValue m) (NThunk m) m) => FromNix (Text, DList Text) m (NValue m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix ByteString m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix ByteString m (NValue m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix Path m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix Path m (NValue m) where
instance (Framed e m, MonadVar m, MonadFile m, FromValue a m (NValueNF m), Show a) => FromNix [a] m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix (HashMap Text (NValueNF m)) m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix (HashMap Text (NValueNF m), HashMap Text SourcePos) m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m) => FromNix (HashMap Text (NThunk m), HashMap Text SourcePos) m (NValue m) where
instance (Framed e m, MonadVar m, MonadFile m, MonadThunk (NValue m) (NThunk m) m) => FromNix (NThunk m) m (NValue m) where
instance (Framed e m, MonadVar m, MonadFile m, MonadEffects m, MonadThunk (NValue m) (NThunk m) m) => FromNix A.Value m (NValueNF m) where
instance (Framed e m, MonadVar m, MonadFile m, MonadEffects m, MonadThunk (NValue m) (NThunk m) m) => FromNix A.Value m (NValue m) where

instance (Monad m, FromNix a m (NValue m)) => FromNix a m (m (NValue m)) where
    fromNixMay = (>>= fromNixMay)
    fromNix    = (>>= fromNix)

instance (MonadThunk (NValue m) (NThunk m) m,
          FromNix a m (NValue m)) => FromNix a m (NThunk m) where
    fromNixMay = force ?? fromNixMay
    fromNix    = force ?? fromNix

class ToNix a m v where
    toNix :: a -> m v
    default toNix :: ToValue a m v => a -> m v
    toNix = toValue

instance (MonadThunk (NValue m) (NThunk m) m, ToNix a m (NValue m))
      => ToNix [a] m (NValue m) where
    toNix = fmap NVList . traverse (thunk . toNix)

instance (MonadThunk (NValue m) (NThunk m) m, ToNix a m (NValue m))
      => ToNix (HashMap Text a) m (NValue m) where
    toNix = fmap (flip NVSet M.empty) . traverse (thunk . toNix)

instance Applicative m => ToNix () m (NValueNF m) where
instance Applicative m => ToNix () m (NValue m) where
instance Applicative m => ToNix Bool m (NValueNF m) where
instance Applicative m => ToNix Bool m (NValue m) where
instance Applicative m => ToNix Int m (NValueNF m) where
instance Applicative m => ToNix Int m (NValue m) where
instance Applicative m => ToNix Integer m (NValueNF m) where
instance Applicative m => ToNix Integer m (NValue m) where
instance Applicative m => ToNix Float m (NValueNF m) where
instance Applicative m => ToNix Float m (NValue m) where
instance Applicative m => ToNix Text m (NValueNF m) where
instance Applicative m => ToNix Text m (NValue m) where
instance Applicative m => ToNix (Text, DList Text) m (NValueNF m) where
instance Applicative m => ToNix (Text, DList Text) m (NValue m) where
instance Applicative m => ToNix ByteString m (NValueNF m) where
instance Applicative m => ToNix ByteString m (NValue m) where
instance Applicative m => ToNix Path m (NValueNF m) where
instance Applicative m => ToNix Path m (NValue m) where
instance MonadThunk (NValue m) (NThunk m) m => ToNix SourcePos m (NValue m) where
instance (Applicative m, ToNix a m (NValueNF m), ToValue a m (NValueNF m)) => ToNix [a] m (NValueNF m) where
instance Applicative m => ToNix (HashMap Text (NValueNF m)) m (NValueNF m) where
instance Applicative m => ToNix (HashMap Text (NValueNF m), HashMap Text SourcePos) m (NValueNF m) where
instance Applicative m => ToNix (HashMap Text (NThunk m), HashMap Text SourcePos) m (NValue m) where
instance (MonadThunk (NValue m) (NThunk m) m, ToNix a m (NValue m), ToValue a m (NValue m)) => ToNix a m (NThunk m) where
instance Applicative m => ToNix Bool m (NExprF r) where
instance Applicative m => ToNix () m (NExprF r) where
instance MonadThunk (NValue m) (NThunk m) m => ToNix A.Value m (NValue m) where

instance MonadThunk (NValue m) (NThunk m) m => ToNix (NThunk m) m (NValue m) where
    toNix = force ?? pure
