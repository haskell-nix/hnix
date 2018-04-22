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

type Convertible e m = (Framed e m, MonadVar m, MonadFile m)

instance Convertible e m => FromValue () m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant NNull) -> pure $ Just ()
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a null, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue () m (NValueF m r) where
    fromValueMay = \case
        NVConstant NNull -> pure $ Just ()
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a null, but saw: " ++ show v

instance Convertible e m
      => FromValue Bool m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NBool b)) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a bool, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue Bool m (NValueF m r) where
    fromValueMay = \case
        NVConstant (NBool b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a bool, but saw: " ++ show v

instance Convertible e m
      => FromValue Int m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NInt b)) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue Int m (NValueF m r) where
    fromValueMay = \case
        NVConstant (NInt b) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an integer, but saw: " ++ show v

instance Convertible e m
      => FromValue Integer m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NInt b)) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue Integer m (NValueF m r) where
    fromValueMay = \case
        NVConstant (NInt b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an integer, but saw: " ++ show v

instance Convertible e m
      => FromValue Float m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NFloat b)) -> pure $ Just b
        Fix (NVConstant (NInt i)) -> pure $ Just (fromInteger i)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a float, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue Float m (NValueF m r) where
    fromValueMay = \case
        NVConstant (NFloat b) -> pure $ Just b
        NVConstant (NInt i) -> pure $ Just (fromInteger i)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a float, but saw: " ++ show v

instance (Convertible e m, MonadEffects m)
      => FromValue Text m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NUri u)) -> pure $ Just u
        Fix (NVStr t _) -> pure $ Just t
        Fix (NVPath p) -> Just . Text.pack . unStorePath <$> addPath p
        Fix (NVSet s _) -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay @Text p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Convertible e m, MonadEffects m,
          MonadThunk (NValueF m r) r m,
          FromValue Text m r, Show (NValueF m r))
      => FromValue Text m (NValueF m r) where
    fromValueMay = \case
        NVConstant (NUri u) -> pure $ Just u
        NVStr t _ -> pure $ Just t
        NVPath p -> Just . Text.pack . unStorePath <$> addPath p
        NVSet s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay @Text p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Convertible e m, MonadEffects m)
      => FromValue (Text, DList Text) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NUri u)) -> pure $ Just (u, mempty)
        Fix (NVStr t d) -> pure $ Just (t, d)
        Fix (NVPath p) -> Just . (,mempty) . Text.pack . unStorePath <$> addPath p
        Fix (NVSet s _) -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fmap (,mempty) <$> fromValueMay @Text p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Convertible e m, MonadEffects m,
          MonadThunk (NValueF m r) r m,
          FromValue Text m r, Show (NValueF m r))
      => FromValue (Text, DList Text) m (NValueF m r) where
    fromValueMay = \case
        NVConstant (NUri u) -> pure $ Just (u, mempty)
        NVStr t d -> pure $ Just (t, d)
        NVPath p -> Just . (,mempty) . Text.pack . unStorePath <$> addPath p
        NVSet s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fmap (,mempty) <$> fromValueMay @Text p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance Convertible e m
      => FromValue ByteString m (NValueNF m) where
    fromValueMay = \case
        Fix (NVStr t _) -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue ByteString m (NValueF m r) where
    fromValueMay = \case
        NVStr t _ -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

newtype Path = Path { getPath :: FilePath }
    deriving Show

instance Convertible e m
      => FromValue Path m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstant (NUri u)) -> pure $ Just (Path (Text.unpack u))
        Fix (NVPath p) -> pure $ Just (Path p)
        Fix (NVStr s _) -> pure $ Just (Path (Text.unpack s))
        Fix (NVSet s _) -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay @Path p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a path, but saw: " ++ show v

instance (Convertible e m, MonadThunk (NValueF m r) r m,
          FromValue Path m r, Show (NValueF m r))
      => FromValue Path m (NValueF m r) where
    fromValueMay = \case
        NVConstant (NUri u) -> pure $ Just (Path (Text.unpack u))
        NVPath p -> pure $ Just (Path p)
        NVStr s _ -> pure $ Just (Path (Text.unpack s))
        NVSet s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay @Path p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a path, but saw: " ++ show v

instance (Convertible e m,
          FromValue a m (NValueNF m), Show a)
      => FromValue [a] m (NValueNF m) where
    fromValueMay = \case
        Fix (NVList l) -> sequence <$> traverse fromValueMay l
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue [r] m (NValueF m r) where
    fromValueMay = \case
        NVList l -> pure $ Just l
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance Convertible e m
      => FromValue (HashMap Text (NValueNF m)) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVSet s _) -> pure $ Just s
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue (HashMap Text r) m (NValueF m r) where
    fromValueMay = \case
        NVSet s _ -> pure $ Just s
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance Convertible e m
      => FromValue (HashMap Text (NValueNF m),
                 HashMap Text SourcePos) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVSet s p) -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue (HashMap Text r,
                   HashMap Text SourcePos) m (NValueF m r) where
    fromValueMay = \case
        NVSet s p -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (MonadThunk (NValueF m r) r m, Convertible e m,
          Show (NValueF m r))
      => FromValue r m (NValueF m r) where
    fromValueMay = pure . Just . value @_ @_ @m
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a thunk, but saw: " ++ show v

instance (Monad m, FromValue a m (NValueF m r))
      => FromValue a m (m (NValueF m r)) where
    fromValueMay = (>>= fromValueMay)
    fromValue    = (>>= fromValue)

instance (MonadThunk (NValueF m r) (NThunk m) m, FromValue a m (NValueF m r))
      => FromValue a m (NThunk m) where
    fromValueMay = force ?? fromValueMay
    fromValue    = force ?? fromValue

{-
instance (MonadThunk (NValueLoc m) (NThunkLoc m) m,
          FromValue a m (NValueLoc m))
      => FromValue a m (NValueLoc m) where
    fromValueMay (NValueLoc _ x) = fromValueMay x
    fromValue    (NValueLoc _ x) = fromValue x
-}

instance (Convertible e m, MonadEffects m)
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

instance (Convertible e m,
          MonadThunk (NValue m) (NThunk m) m, MonadEffects m)
      => FromValue A.Value m (NValueF m (NThunk m)) where
    fromValueMay = normalForm >=> fromValueMay
    fromValue    = normalForm >=> fromValue

class ToValue a m v where
    toValue :: a -> m v

instance Applicative m => ToValue () m (NValueNF m) where
    toValue _ = pure . Fix . NVConstant $ NNull

instance Applicative m => ToValue () m (NValueF m r) where
    toValue _ = pure . NVConstant $ NNull

instance Applicative m => ToValue Bool m (NValueNF m) where
    toValue = pure . Fix . NVConstant . NBool

instance Applicative m => ToValue Bool m (NValueF m r) where
    toValue = pure . NVConstant . NBool

instance Applicative m => ToValue Int m (NValueNF m) where
    toValue = pure . Fix . NVConstant . NInt . toInteger

instance Applicative m => ToValue Int m (NValueF m r) where
    toValue = pure . NVConstant . NInt . toInteger

instance Applicative m => ToValue Integer m (NValueNF m) where
    toValue = pure . Fix . NVConstant . NInt

instance Applicative m => ToValue Integer m (NValueF m r) where
    toValue = pure . NVConstant . NInt

instance Applicative m => ToValue Float m (NValueNF m) where
    toValue = pure . Fix . NVConstant . NFloat

instance Applicative m => ToValue Float m (NValueF m r) where
    toValue = pure . NVConstant . NFloat

instance Applicative m => ToValue Text m (NValueNF m) where
    toValue = pure . Fix . flip NVStr mempty

instance Applicative m => ToValue Text m (NValueF m r) where
    toValue = pure . flip NVStr mempty

instance Applicative m => ToValue (Text, DList Text) m (NValueNF m) where
    toValue = pure . Fix . uncurry NVStr

instance Applicative m => ToValue (Text, DList Text) m (NValueF m r) where
    toValue = pure . uncurry NVStr

instance Applicative m => ToValue ByteString m (NValueNF m) where
    toValue = pure . Fix . flip NVStr mempty . decodeUtf8

instance Applicative m => ToValue ByteString m (NValueF m r) where
    toValue = pure . flip NVStr mempty . decodeUtf8

instance Applicative m => ToValue Path m (NValueNF m) where
    toValue = pure . Fix . NVPath . getPath

instance Applicative m => ToValue Path m (NValueF m r) where
    toValue = pure . NVPath . getPath

instance MonadThunk (NValueF m r) r m
      => ToValue SourcePos m (NValueF m r) where
    toValue (SourcePos f l c) = do
        f' <- toValue @_ @_ @(NValueF m r) (Text.pack f)
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

instance Applicative m => ToValue [r] m (NValueF m r) where
    toValue = pure . NVList

instance Applicative m
      => ToValue (HashMap Text (NValueNF m)) m (NValueNF m) where
    toValue = pure . Fix . flip NVSet M.empty

instance Applicative m => ToValue (HashMap Text r) m (NValueF m r) where
    toValue = pure . flip NVSet M.empty

instance Applicative m => ToValue (HashMap Text (NValueNF m),
                HashMap Text SourcePos) m (NValueNF m) where
    toValue (s, p) = pure $ Fix $ NVSet s p

instance Applicative m => ToValue (HashMap Text r,
                HashMap Text SourcePos) m (NValueF m r) where
    toValue (s, p) = pure $ NVSet s p

instance (MonadThunk (NValue m) (NThunk m) m, ToValue a m (NValue m))
      => ToValue a m (NThunk m) where
    toValue = fmap (value @(NValue m) @_ @m) . toValue

instance Applicative m => ToValue Bool m (NExprF r) where
    toValue = pure . NConstant . NBool

instance Applicative m => ToValue () m (NExprF r) where
    toValue _ = pure . NConstant $ NNull

instance (Framed e m, MonadThunk (NValueF m r) r m)
      => ToValue A.Value m (NValueF m r) where
    toValue = \case
        A.Object m -> flip NVSet M.empty
            <$> traverse (thunk . toValue @_ @_ @(NValueF m r)) m
        A.Array l -> NVList <$>
            traverse (thunk . withStringContext "While coercing to a JSON value"
                            . toValue) (V.toList l)
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

instance (Convertible e m, MonadThunk (NValueF m r) r m,
          FromNix a m (NValueF m r), Show (NValueF m r), Show a)
      => FromNix [a] m (NValueF m r) where
    fromNixMay = \case
        NVList l -> sequence <$> traverse (`force` fromNixMay) l
        _ -> pure Nothing
    fromNix v = fromNixMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Convertible e m, MonadThunk (NValueF m r) r m,
          FromNix a m (NValueF m r), Show (NValueF m r), Show a)
      => FromNix (HashMap Text a) m (NValueF m r) where
    fromNixMay = \case
        NVSet s _ -> sequence <$> traverse (`force` fromNixMay) s
        _ -> pure Nothing
    fromNix v = fromNixMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance Convertible e m => FromNix () m (NValueNF m) where
instance (Convertible e m, Show (NValueF m r)) => FromNix () m (NValueF m r) where
instance Convertible e m => FromNix Bool m (NValueNF m) where
instance (Convertible e m, Show (NValueF m r)) => FromNix Bool m (NValueF m r) where
instance Convertible e m => FromNix Int m (NValueNF m) where
instance (Convertible e m, Show (NValueF m r)) => FromNix Int m (NValueF m r) where
instance Convertible e m => FromNix Integer m (NValueNF m) where
instance (Convertible e m, Show (NValueF m r)) => FromNix Integer m (NValueF m r) where
instance Convertible e m => FromNix Float m (NValueNF m) where
instance (Convertible e m, Show (NValueF m r)) => FromNix Float m (NValueF m r) where
instance (Convertible e m, MonadEffects m) => FromNix Text m (NValueNF m) where
instance (Convertible e m, MonadEffects m, MonadThunk (NValueF m r) r m, FromValue Text m r, Show (NValueF m r)) => FromNix Text m (NValueF m r) where
instance (Convertible e m, MonadEffects m) => FromNix (Text, DList Text) m (NValueNF m) where
instance (Convertible e m, MonadEffects m, MonadThunk (NValueF m r) r m, FromValue Text m r, Show (NValueF m r)) => FromNix (Text, DList Text) m (NValueF m r) where
instance Convertible e m => FromNix ByteString m (NValueNF m) where
instance (Convertible e m, Show (NValueF m r)) => FromNix ByteString m (NValueF m r) where
instance Convertible e m => FromNix Path m (NValueNF m) where
instance (Convertible e m, MonadThunk (NValueF m r) r m, FromValue Path m r, Show (NValueF m r)) => FromNix Path m (NValueF m r) where
instance (Convertible e m, FromValue a m (NValueNF m), Show a) => FromNix [a] m (NValueNF m) where
instance Convertible e m => FromNix (HashMap Text (NValueNF m)) m (NValueNF m) where
instance Convertible e m => FromNix (HashMap Text (NValueNF m), HashMap Text SourcePos) m (NValueNF m) where
instance (Convertible e m, Show (NValueF m r)) => FromNix (HashMap Text r, HashMap Text SourcePos) m (NValueF m r) where
instance (Convertible e m, MonadThunk (NValueF m r) r m, Show (NValueF m r)) => FromNix r m (NValueF m r) where
instance (Convertible e m, MonadEffects m, MonadThunk (NValueF m r) r m) => FromNix A.Value m (NValueNF m) where
instance (Convertible e m, MonadEffects m, MonadThunk (NValue m) (NThunk m) m) => FromNix A.Value m (NValue m) where

instance (Monad m, FromNix a m (NValueF m r)) => FromNix a m (m (NValueF m r)) where
    fromNixMay = (>>= fromNixMay)
    fromNix    = (>>= fromNix)

instance (MonadThunk (NValueF m r) (NThunk m) m,
          FromNix a m (NValueF m r)) => FromNix a m (NThunk m) where
    fromNixMay = force ?? fromNixMay
    fromNix    = force ?? fromNix

class ToNix a m v where
    toNix :: a -> m v
    default toNix :: ToValue a m v => a -> m v
    toNix = toValue

instance (Framed e m, MonadThunk (NValueF m r) r m, ToNix a m (NValueF m r))
      => ToNix [a] m (NValueF m r) where
    toNix = fmap NVList
        . traverse (thunk . withStringContext "While coercing to a list" . toNix)

instance (Framed e m, MonadThunk (NValueF m r) r m, ToNix a m (NValueF m r))
      => ToNix (HashMap Text a) m (NValueF m r) where
    toNix = fmap (flip NVSet M.empty)
        . traverse (thunk . withStringContext "While coercing to a set" . toNix)

instance Applicative m => ToNix () m (NValueNF m) where
instance Applicative m => ToNix () m (NValueF m r) where
instance Applicative m => ToNix Bool m (NValueNF m) where
instance Applicative m => ToNix Bool m (NValueF m r) where
instance Applicative m => ToNix Int m (NValueNF m) where
instance Applicative m => ToNix Int m (NValueF m r) where
instance Applicative m => ToNix Integer m (NValueNF m) where
instance Applicative m => ToNix Integer m (NValueF m r) where
instance Applicative m => ToNix Float m (NValueNF m) where
instance Applicative m => ToNix Float m (NValueF m r) where
instance Applicative m => ToNix Text m (NValueNF m) where
instance Applicative m => ToNix Text m (NValueF m r) where
instance Applicative m => ToNix (Text, DList Text) m (NValueNF m) where
instance Applicative m => ToNix (Text, DList Text) m (NValueF m r) where
instance Applicative m => ToNix ByteString m (NValueNF m) where
instance Applicative m => ToNix ByteString m (NValueF m r) where
instance Applicative m => ToNix Path m (NValueNF m) where
instance Applicative m => ToNix Path m (NValueF m r) where
instance MonadThunk (NValueF m r) r m => ToNix SourcePos m (NValueF m r) where
instance (Applicative m, ToNix a m (NValueNF m), ToValue a m (NValueNF m)) => ToNix [a] m (NValueNF m) where
instance Applicative m => ToNix (HashMap Text (NValueNF m)) m (NValueNF m) where
instance Applicative m => ToNix (HashMap Text (NValueNF m), HashMap Text SourcePos) m (NValueNF m) where
instance Applicative m => ToNix (HashMap Text r, HashMap Text SourcePos) m (NValueF m r) where
instance (MonadThunk (NValue m) (NThunk m) m, ToValue a m (NValue m)) => ToNix a m (NThunk m) where
instance Applicative m => ToNix Bool m (NExprF r) where
instance Applicative m => ToNix () m (NExprF r) where
instance (Framed e m, MonadThunk (NValueF m r) r m) => ToNix A.Value m (NValueF m r) where

instance MonadThunk (NValueF m r) r m => ToNix r m (NValueF m r) where
    toNix = force ?? pure
