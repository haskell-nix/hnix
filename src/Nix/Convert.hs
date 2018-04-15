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
    fromNix    :: v -> m a
    fromNixMay :: v -> m (Maybe a)

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix Bool m (NValueNF m) where
    fromNixMay = \case
        Fix (NVConstant (NBool b)) -> pure $ Just b
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a bool, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix Bool m (NValue m) where
    fromNixMay = \case
        NVConstant (NBool b) -> pure $ Just b
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a bool, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix Int m (NValueNF m) where
    fromNixMay = \case
        Fix (NVConstant (NInt b)) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix Int m (NValue m) where
    fromNixMay = \case
        NVConstant (NInt b) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix Integer m (NValueNF m) where
    fromNixMay = \case
        Fix (NVConstant (NInt b)) -> pure $ Just b
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix Integer m (NValue m) where
    fromNixMay = \case
        NVConstant (NInt b) -> pure $ Just b
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix Float m (NValueNF m) where
    fromNixMay = \case
        Fix (NVConstant (NFloat b)) -> pure $ Just b
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a float, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix Float m (NValue m) where
    fromNixMay = \case
        NVConstant (NFloat b) -> pure $ Just b
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a float, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix Text m (NValueNF m) where
    fromNixMay = \case
        Fix (NVStr t _) -> pure $ Just t
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a string, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix Text m (NValue m) where
    fromNixMay = \case
        NVStr t _ -> pure $ Just t
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a string, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix ByteString m (NValueNF m) where
    fromNixMay = \case
        Fix (NVStr t _) -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a string, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix ByteString m (NValue m) where
    fromNixMay = \case
        NVStr t _ -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a string, but saw: " ++ show v

newtype Path = Path { getPath :: FilePath }
    deriving Show

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix Path m (NValueNF m) where
    fromNixMay = \case
        Fix (NVPath p) -> pure $ Just (Path p)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a path, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix Path m (NValue m) where
    fromNixMay = \case
        NVPath p -> pure $ Just (Path p)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a path, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m,
          FromNix a m (NValueNF m), Show a)
      => FromNix [a] m (NValueNF m) where
    fromNixMay = \case
        Fix (NVList l) -> sequence <$> traverse fromNixMay l
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

-- jww (2018-04-15): This instance does not work, because when the desired
-- conversion is FromNix [NThunk m] m (NValue m), we then use traverse with
-- FromNix (NThunk m) m (NValue m), and this use of 'traverse' causes the
-- monadic effects to be sequence'd too early.

-- instance (Framed e m, MonadVar m, MonadFile m) => (MonadThunk (NValue m) (NThunk m) m,
--           FromNix a m (NValue m), Show a) => FromNix [a] m (NValue m) where
--     fromNixMay = \case
--         NVList l -> sequence <$> traverse (`force` fromNixMay) l
--         _ -> pure Nothing
--     fromNix = fromNixMay >=> \case
--         Just b -> pure b
--         v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix [NThunk m] m (NValue m) where
    fromNixMay = \case
        NVList l -> pure $ Just l
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix (HashMap Text (NValueNF m)) m (NValueNF m) where
    fromNixMay = \case
        Fix (NVSet s _) -> pure $ Just s
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

-- instance (Framed e m, MonadVar m, MonadFile m) => (MonadThunk (NValue m) (NThunk m) m,
--           FromNix a m (NValue m), Show a)
--       => FromNix (HashMap Text a) m (NValue m) where
--     fromNixMay = \case
--         NVSet s _ -> sequence <$> traverse (`force` fromNixMay) s
--         _ -> pure Nothing
--     fromNix = fromNixMay >=> \case
--         Just b -> pure b
--         v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix (HashMap Text (NThunk m)) m (NValue m) where
    fromNixMay = \case
        NVSet s _ -> pure $ Just s
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix (HashMap Text (NValueNF m),
                 HashMap Text SourcePos) m (NValueNF m) where
    fromNixMay = \case
        Fix (NVSet s p) -> pure $ Just (s, p)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m)
      => FromNix (HashMap Text (NThunk m),
                 HashMap Text SourcePos) m (NValue m) where
    fromNixMay = \case
        NVSet s p -> pure $ Just (s, p)
        _ -> pure Nothing
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (MonadThunk (NValue m) (NThunk m) m,
          Framed e m, MonadVar m, MonadFile m)
      => FromNix (NThunk m) m (NValue m) where
    fromNixMay = pure . Just . value @_ @_ @m
    fromNix = fromNixMay >=> \case
        Just b -> pure b
        v -> throwError $ "Expected a thunk, but saw: " ++ show v

instance (Framed e m, MonadVar m, MonadFile m, MonadEffects m)
      => FromNix A.Value m (NValueNF m) where
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

instance (Framed e m, MonadVar m, MonadFile m,
          MonadThunk (NValue m) (NThunk m) m, MonadEffects m)
      => FromNix A.Value m (NValue m) where
    fromNixMay = normalForm >=> fromNixMay
    fromNix    = normalForm >=> fromNix

class ToNix a m v where
    toNix :: a -> m v

instance Applicative m => ToNix Bool m (NValueNF m) where
    toNix = pure . Fix . NVConstant . NBool

instance Applicative m => ToNix Bool m (NValue m) where
    toNix = pure . NVConstant . NBool

instance Applicative m => ToNix Int m (NValueNF m) where
    toNix = pure . Fix . NVConstant . NInt . toInteger

instance Applicative m => ToNix Int m (NValue m) where
    toNix = pure . NVConstant . NInt . toInteger

instance Applicative m => ToNix Integer m (NValueNF m) where
    toNix = pure . Fix . NVConstant . NInt

instance Applicative m => ToNix Integer m (NValue m) where
    toNix = pure . NVConstant . NInt

instance Applicative m => ToNix Float m (NValueNF m) where
    toNix = pure . Fix . NVConstant . NFloat

instance Applicative m => ToNix Float m (NValue m) where
    toNix = pure . NVConstant . NFloat

instance Applicative m => ToNix Text m (NValueNF m) where
    toNix = pure . Fix . flip NVStr mempty

instance Applicative m => ToNix Text m (NValue m) where
    toNix = pure . flip NVStr mempty

instance Applicative m => ToNix ByteString m (NValueNF m) where
    toNix = pure . Fix . flip NVStr mempty . decodeUtf8

instance Applicative m => ToNix ByteString m (NValue m) where
    toNix = pure . flip NVStr mempty . decodeUtf8

instance Applicative m => ToNix Path m (NValueNF m) where
    toNix = pure . Fix . NVPath . getPath

instance Applicative m => ToNix Path m (NValue m) where
    toNix = pure . NVPath . getPath

instance MonadThunk (NValue m) (NThunk m) m
      => ToNix SourcePos m (NValue m) where
    toNix (SourcePos f l c) = do
        f' <- toNix @_ @_ @(NValue m) (Text.pack f)
        l' <- toNix (unPos l)
        c' <- toNix (unPos c)
        let pos = M.fromList
                [ ("file" :: Text, value @_ @_ @m f')
                , ("line",        value @_ @_ @m l')
                , ("column",      value @_ @_ @m c') ]
        pure $ NVSet pos mempty

instance (ToNix a m (NValueNF m), Applicative m)
      => ToNix [a] m (NValueNF m) where
    toNix = fmap (Fix . NVList) . traverse toNix

-- instance Applicative m => (MonadThunk (NValue m) (NThunk m) m,
--           ToNix a m (NValue m)) => ToNix [a] m (NValue m) where
--     toNix = pure . NVList . fmap toNix

instance Applicative m => ToNix [NThunk m] m (NValue m) where
    toNix = pure . NVList

instance Applicative m
      => ToNix (HashMap Text (NValueNF m)) m (NValueNF m) where
    toNix = pure . Fix . flip NVSet M.empty

-- instance Applicative m => (MonadThunk (NValue m) (NThunk m) m,
--           ToNix a m (NValue m))
--       => ToNix (HashMap Text a) m (NValue m) where
--     toNix = pure . flip NVSet M.empty . fmap toNix

instance Applicative m => ToNix (HashMap Text (NThunk m)) m (NValue m) where
    toNix = pure . flip NVSet M.empty

instance Applicative m => ToNix (HashMap Text (NValueNF m),
                HashMap Text SourcePos) m (NValueNF m) where
    toNix (s, p) = pure $ Fix $ NVSet s p

instance Applicative m => ToNix (HashMap Text (NThunk m),
                HashMap Text SourcePos) m (NValue m) where
    toNix (s, p) = pure $ NVSet s p

instance (MonadThunk (NValue m) (NThunk m) m, ToNix a m (NValue m))
      => ToNix a m (NThunk m) where
    toNix = fmap (value @(NValue m) @_ @m) . toNix

instance Applicative m => ToNix Bool m (NExprF r) where
    toNix = pure . NConstant . NBool

instance MonadThunk (NValue m) (NThunk m) m
      => ToNix A.Value m (NValue m) where
    toNix = \case
        A.Object m -> flip NVSet M.empty
            <$> traverse (thunk . toNix @_ @_ @(NValue m)) m
        A.Array l -> NVList <$> traverse (thunk . toNix) (V.toList l)
        A.String s -> pure $ NVStr s mempty
        A.Number n -> pure $ NVConstant $ case floatingOrInteger n of
            Left r -> NFloat r
            Right i -> NInt i
        A.Bool b -> pure $ NVConstant $ NBool b
        A.Null -> pure $ NVConstant NNull
