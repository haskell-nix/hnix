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

class FromValue a m v where
    fromValue    :: v -> m a
    fromValueMay :: v -> m (Maybe a)

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Bool m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NBool b)) -> pure $ Just b
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a bool, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Bool m (NValue m) where
    fromValueMay = \case
        NVConstant (NBool b) -> pure $ Just b
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a bool, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Int m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NInt b)) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Int m (NValue m) where
    fromValueMay = \case
        NVConstant (NInt b) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Integer m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NInt b)) -> pure $ Just b
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Integer m (NValue m) where
    fromValueMay = \case
        NVConstant (NInt b) -> pure $ Just b
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Float m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NFloat b)) -> pure $ Just b
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a float, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Float m (NValue m) where
    fromValueMay = \case
        NVConstant (NFloat b) -> pure $ Just b
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a float, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Text m (NValueNF m) where
    fromValueMay = \case
        Fix (NVStr t _) -> pure $ Just t
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a string, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Text m (NValue m) where
    fromValueMay = \case
        NVStr t _ -> pure $ Just t
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a string, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue ByteString m (NValueNF m) where
    fromValueMay = \case
        Fix (NVStr t _) -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a string, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue ByteString m (NValue m) where
    fromValueMay = \case
        NVStr t _ -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a string, but saw: " ++ show v

newtype Path = Path { getPath :: FilePath }
    deriving Show

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Path m (NValueNF m) where
    fromValueMay = \case
        Fix (NVPath p) -> pure $ Just (Path p)
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a path, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue Path m (NValue m) where
    fromValueMay = \case
        NVPath p -> pure $ Just (Path p)
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a path, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m,
          FromValue a m (NValueNF m), Show a)
      => FromValue [a] m (NValueNF m) where
    fromValueMay = \case
        Fix (NVList l) -> sequence <$> traverse fromValueMay l
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

-- jww (2018-04-15): This instance does not work, because when the desired
-- conversion is FromValue [NThunk m] m (NValue m), we then use traverse with
-- FromValue (NThunk m) m (NValue m), and this use of 'traverse' causes the
-- monadic effects to be sequence'd too early.

-- instance (Framed e m, MonadVar m, MonadFile m) => (MonadThunk (NValue m) (NThunk m) m,
--           FromValue a m (NValue m), Show a) => FromValue [a] m (NValue m) where
--     fromValueMay = \case
--         NVList l -> sequence <$> traverse (`force` fromValueMay) l
--         _ -> pure Nothing
--     fromValue = fromValueMay >=> \case
--         Just b -> pure b
--         v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue [NThunk m] m (NValue m) where
    fromValueMay = \case
        NVList l -> pure $ Just l
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue (HashMap Text (NValueNF m)) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVSet s _) -> pure $ Just s
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

-- instance (Framed e m, MonadVar m, MonadFile m) => (MonadThunk (NValue m) (NThunk m) m,
--           FromValue a m (NValue m), Show a)
--       => FromValue (HashMap Text a) m (NValue m) where
--     fromValueMay = \case
--         NVSet s _ -> sequence <$> traverse (`force` fromValueMay) s
--         _ -> pure Nothing
--     fromValue = fromValueMay >=> \case
--         Just b -> pure b
--         v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue (HashMap Text (NThunk m)) m (NValue m) where
    fromValueMay = \case
        NVSet s _ -> pure $ Just s
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue (HashMap Text (NValueNF m),
                 HashMap Text SourcePos) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVSet s p) -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromValue (HashMap Text (NThunk m),
                 HashMap Text SourcePos) m (NValue m) where
    fromValueMay = \case
        NVSet s p -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (MonadThunk (NValue m) (NThunk m) m,
          Framed e m, MonadVar m, MonadFile m)
      => FromValue (NThunk m) m (NValue m) where
    fromValueMay = pure . Just . value @_ @_ @m
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a thunk, but saw: " ++ show v

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
    fromValue = fromValueMay >=> \case
        Just b -> pure b
        v -> throwError $ "Cannot convert value to JSON: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m,
          MonadThunk (NValue m) (NThunk m) m, MonadEffects m)
      => FromValue A.Value m (NValue m) where
    fromValueMay = normalForm >=> fromValueMay
    fromValue    = normalForm >=> fromValue

class ToValue a m v where
    toValue :: a -> m v

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

-- instance Applicative m => (MonadThunk (NValue m) (NThunk m) m,
--           ToValue a m (NValue m)) => ToValue [a] m (NValue m) where
--     toValue = pure . NVList . fmap toValue

instance Applicative m => ToValue [NThunk m] m (NValue m) where
    toValue = pure . NVList

instance Applicative m
      => ToValue (HashMap Text (NValueNF m)) m (NValueNF m) where
    toValue = pure . Fix . flip NVSet M.empty

-- instance Applicative m => (MonadThunk (NValue m) (NThunk m) m,
--           ToValue a m (NValue m))
--       => ToValue (HashMap Text a) m (NValue m) where
--     toValue = pure . flip NVSet M.empty . fmap toValue

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
