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

-- | Although there are a lot of instances in this file, really it's just a
--   combinatorial explosion of the following combinations:
--
--   - Several Haskell types being converted to/from Nix wrappers
--   - Several types of Nix wrappers
--   - Whether to be shallow or deep while unwrapping

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
        Fix (NVConstantF NNull) -> pure $ Just ()
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a null, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue () m (NValueF m r) where
    fromValueMay = \case
        NVConstantF NNull -> pure $ Just ()
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a null, but saw: " ++ show v

instance Convertible e m
      => FromValue Bool m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF (NBool b)) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a bool, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue Bool m (NValueF m r) where
    fromValueMay = \case
        NVConstantF (NBool b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a bool, but saw: " ++ show v

instance Convertible e m
      => FromValue Int m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF (NInt b)) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue Int m (NValueF m r) where
    fromValueMay = \case
        NVConstantF (NInt b) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an integer, but saw: " ++ show v

instance Convertible e m
      => FromValue Integer m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF (NInt b)) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an integer, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue Integer m (NValueF m r) where
    fromValueMay = \case
        NVConstantF (NInt b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an integer, but saw: " ++ show v

instance Convertible e m
      => FromValue Float m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF (NFloat b)) -> pure $ Just b
        Fix (NVConstantF (NInt i)) -> pure $ Just (fromInteger i)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a float, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue Float m (NValueF m r) where
    fromValueMay = \case
        NVConstantF (NFloat b) -> pure $ Just b
        NVConstantF (NInt i) -> pure $ Just (fromInteger i)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a float, but saw: " ++ show v

instance (Convertible e m, MonadEffects m)
      => FromValue Text m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF (NUri u)) -> pure $ Just u
        Fix (NVStrF t _) -> pure $ Just t
        Fix (NVPathF p) -> Just . Text.pack . unStorePath <$> addPath p
        Fix (NVSetF s _) -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay @Text p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Convertible e m, MonadEffects m,
          FromValue Text m r, Show (NValueF m r))
      => FromValue Text m (NValueF m r) where
    fromValueMay = \case
        NVConstantF (NUri u) -> pure $ Just u
        NVStrF t _ -> pure $ Just t
        NVPathF p -> Just . Text.pack . unStorePath <$> addPath p
        NVSetF s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay @Text p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Convertible e m, MonadEffects m)
      => FromValue (Text, DList Text) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF (NUri u)) -> pure $ Just (u, mempty)
        Fix (NVStrF t d) -> pure $ Just (t, d)
        Fix (NVPathF p) -> Just . (,mempty) . Text.pack . unStorePath <$> addPath p
        Fix (NVSetF s _) -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fmap (,mempty) <$> fromValueMay @Text p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Convertible e m, MonadEffects m,
          FromValue Text m r, Show (NValueF m r))
      => FromValue (Text, DList Text) m (NValueF m r) where
    fromValueMay = \case
        NVConstantF (NUri u) -> pure $ Just (u, mempty)
        NVStrF t d -> pure $ Just (t, d)
        NVPathF p -> Just . (,mempty) . Text.pack . unStorePath <$> addPath p
        NVSetF s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fmap (,mempty) <$> fromValueMay @Text p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance Convertible e m
      => FromValue ByteString m (NValueNF m) where
    fromValueMay = \case
        Fix (NVStrF t _) -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue ByteString m (NValueF m r) where
    fromValueMay = \case
        NVStrF t _ -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a string, but saw: " ++ show v

newtype Path = Path { getPath :: FilePath }
    deriving Show

instance Convertible e m
      => FromValue Path m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF (NUri u)) -> pure $ Just (Path (Text.unpack u))
        Fix (NVPathF p) -> pure $ Just (Path p)
        Fix (NVStrF s _) -> pure $ Just (Path (Text.unpack s))
        Fix (NVSetF s _) -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay @Path p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a path, but saw: " ++ show v

instance (Convertible e m, FromValue Path m r, Show (NValueF m r))
      => FromValue Path m (NValueF m r) where
    fromValueMay = \case
        NVConstantF (NUri u) -> pure $ Just (Path (Text.unpack u))
        NVPathF p -> pure $ Just (Path p)
        NVStrF s _ -> pure $ Just (Path (Text.unpack s))
        NVSetF s _ -> case M.lookup "outPath" s of
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
        Fix (NVListF l) -> sequence <$> traverse fromValueMay l
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue [r] m (NValueF m r) where
    fromValueMay = \case
        NVListF l -> pure $ Just l
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance Convertible e m
      => FromValue (HashMap Text (NValueNF m)) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVSetF s _) -> pure $ Just s
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue (HashMap Text r) m (NValueF m r) where
    fromValueMay = \case
        NVSetF s _ -> pure $ Just s
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance Convertible e m
      => FromValue (HashMap Text (NValueNF m),
                 HashMap Text SourcePos) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVSetF s p) -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Convertible e m, Show (NValueF m r))
      => FromValue (HashMap Text r,
                   HashMap Text SourcePos) m (NValueF m r) where
    fromValueMay = \case
        NVSetF s p -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (MonadThunk (NValue m) (NThunk m) m, Convertible e m)
      => FromValue (NThunk m) m (NValueF m (NThunk m)) where
    fromValueMay = pure . Just . value @_ @_ @m . NValue Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected a thunk, but saw: " ++ show v

instance (Monad m, FromValue a m v) => FromValue a m (m v) where
    fromValueMay = (>>= fromValueMay)
    fromValue    = (>>= fromValue)

instance (MonadThunk (NValue m) (NThunk m) m,
          FromValue a m (NValueF m (NThunk m)))
      => FromValue a m (NThunk m) where
    fromValueMay = force ?? fromValueMay
    fromValue    = force ?? fromValue

instance FromValue a m (NValueF m (NThunk m))
      => FromValue a m (NValue m) where
    fromValueMay = fromValueMay . baseValue
    fromValue    = fromValue . baseValue

instance (Convertible e m, MonadEffects m)
      => FromValue A.Value m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF a) -> pure $ Just $ case a of
            NInt n   -> toJSON n
            NFloat n -> toJSON n
            NBool b  -> toJSON b
            NNull    -> A.Null
            NUri u   -> toJSON u
        Fix (NVStrF s _)     -> pure $ Just $ toJSON s
        Fix (NVListF l)      -> fmap (A.Array . V.fromList) . sequence
                                  <$> traverse fromValueMay l
        Fix (NVSetF m _)     -> fmap A.Object . sequence <$> traverse fromValueMay m
        Fix NVClosureF {}    -> pure Nothing
        Fix (NVPathF p)      -> Just . toJSON . unStorePath <$> addPath p
        Fix (NVBuiltinF _ _) -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Cannot convert value to JSON: " ++ show v

class ToValue a m v where
    toValue :: a -> m v

instance Applicative m => ToValue () m (NValueNF m) where
    toValue _ = pure . Fix . NVConstantF $ NNull

instance Applicative m => ToValue () m (NValueF m r) where
    toValue _ = pure . NVConstantF $ NNull

instance Applicative m => ToValue Bool m (NValueNF m) where
    toValue = pure . Fix . NVConstantF . NBool

instance Applicative m => ToValue Bool m (NValueF m r) where
    toValue = pure . NVConstantF . NBool

instance Applicative m => ToValue Int m (NValueNF m) where
    toValue = pure . Fix . NVConstantF . NInt . toInteger

instance Applicative m => ToValue Int m (NValueF m r) where
    toValue = pure . NVConstantF . NInt . toInteger

instance Applicative m => ToValue Integer m (NValueNF m) where
    toValue = pure . Fix . NVConstantF . NInt

instance Applicative m => ToValue Integer m (NValueF m r) where
    toValue = pure . NVConstantF . NInt

instance Applicative m => ToValue Float m (NValueNF m) where
    toValue = pure . Fix . NVConstantF . NFloat

instance Applicative m => ToValue Float m (NValueF m r) where
    toValue = pure . NVConstantF . NFloat

instance Applicative m => ToValue Text m (NValueNF m) where
    toValue = pure . Fix . flip NVStrF mempty

instance Applicative m => ToValue Text m (NValueF m r) where
    toValue = pure . flip NVStrF mempty

instance Applicative m => ToValue (Text, DList Text) m (NValueNF m) where
    toValue = pure . Fix . uncurry NVStrF

instance Applicative m => ToValue (Text, DList Text) m (NValueF m r) where
    toValue = pure . uncurry NVStrF

instance Applicative m => ToValue ByteString m (NValueNF m) where
    toValue = pure . Fix . flip NVStrF mempty . decodeUtf8

instance Applicative m => ToValue ByteString m (NValueF m r) where
    toValue = pure . flip NVStrF mempty . decodeUtf8

instance Applicative m => ToValue Path m (NValueNF m) where
    toValue = pure . Fix . NVPathF . getPath

instance Applicative m => ToValue Path m (NValueF m r) where
    toValue = pure . NVPathF . getPath

instance MonadThunk (NValue m) (NThunk m) m
      => ToValue SourcePos m (NValueF m (NThunk m)) where
    toValue (SourcePos f l c) = do
        f' <- NValue Nothing <$> toValue (Text.pack f)
        l' <- NValue Nothing <$> toValue (unPos l)
        c' <- NValue Nothing <$> toValue (unPos c)
        let pos = M.fromList
                [ ("file" :: Text, value @_ @_ @m f')
                , ("line",        value @_ @_ @m l')
                , ("column",      value @_ @_ @m c') ]
        pure $ NVSetF pos mempty

instance (ToValue a m (NValueNF m), Applicative m)
      => ToValue [a] m (NValueNF m) where
    toValue = fmap (Fix . NVListF) . traverse toValue

instance Applicative m => ToValue [r] m (NValueF m r) where
    toValue = pure . NVListF

instance Applicative m
      => ToValue (HashMap Text (NValueNF m)) m (NValueNF m) where
    toValue = pure . Fix . flip NVSetF M.empty

instance Applicative m => ToValue (HashMap Text r) m (NValueF m r) where
    toValue = pure . flip NVSetF M.empty

instance Applicative m => ToValue (HashMap Text (NValueNF m),
                HashMap Text SourcePos) m (NValueNF m) where
    toValue (s, p) = pure $ Fix $ NVSetF s p

instance Applicative m => ToValue (HashMap Text r,
                HashMap Text SourcePos) m (NValueF m r) where
    toValue (s, p) = pure $ NVSetF s p

instance (MonadThunk (NValue m) (NThunk m) m, ToValue a m (NValue m))
      => ToValue a m (NThunk m) where
    toValue = fmap (value @(NValue m) @_ @m) . toValue

instance Applicative m => ToValue Bool m (NExprF r) where
    toValue = pure . NConstant . NBool

instance Applicative m => ToValue () m (NExprF r) where
    toValue _ = pure . NConstant $ NNull

instance (Framed e m, MonadThunk (NValue m) (NThunk m) m)
      => ToValue A.Value m (NValueF m (NThunk m)) where
    toValue = \case
        A.Object m -> flip NVSetF M.empty
            <$> traverse (thunk . fmap (NValue Nothing)
                                . toValue @_ @_ @(NValueF m (NThunk m))) m
        A.Array l -> NVListF <$>
            traverse (thunk . withStringContext "While coercing to a JSON value"
                            . toValue) (V.toList l)
        A.String s -> pure $ NVStrF s mempty
        A.Number n -> pure $ NVConstantF $ case floatingOrInteger n of
            Left r -> NFloat r
            Right i -> NInt i
        A.Bool b -> pure $ NVConstantF $ NBool b
        A.Null -> pure $ NVConstantF NNull

instance (MonadThunk (NValue m) (NThunk m) m,
          ToValue a m (NValueF m (NThunk m)))
      => ToValue a m (NValue m) where
    toValue = fmap (NValue Nothing) . toValue

class FromNix a m v where
    fromNix :: v -> m a
    default fromNix :: FromValue a m v => v -> m a
    fromNix = fromValue

    fromNixMay :: v -> m (Maybe a)
    default fromNixMay :: FromValue a m v => v -> m (Maybe a)
    fromNixMay = fromValueMay

instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m,
          FromNix a m (NValueF m (NThunk m)), Show a)
      => FromNix [a] m (NValueF m (NThunk m)) where
    fromNixMay = \case
        NVListF l -> sequence <$> traverse (`force` fromNixMay . baseValue) l
        _ -> pure Nothing
    fromNix v = fromNixMay v >>= \case
        Just b -> pure b
        _ -> throwError $ "Expected an attrset, but saw: " ++ show v

instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m,
          FromNix a m (NValueF m (NThunk m)), Show a)
      => FromNix (HashMap Text a) m (NValueF m (NThunk m)) where
    fromNixMay = \case
        NVSetF s _ -> sequence <$> traverse (`force` fromNixMay . baseValue) s
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
instance (Convertible e m, MonadEffects m, MonadThunk (NValue m) (NThunk m) m, FromValue Text m r, Show (NValueF m r)) => FromNix Text m (NValueF m r) where
instance (Convertible e m, MonadEffects m) => FromNix (Text, DList Text) m (NValueNF m) where
instance (Convertible e m, MonadEffects m, MonadThunk (NValue m) (NThunk m) m, FromValue Text m r, Show (NValueF m r)) => FromNix (Text, DList Text) m (NValueF m r) where
instance Convertible e m => FromNix ByteString m (NValueNF m) where
instance (Convertible e m, Show (NValueF m r)) => FromNix ByteString m (NValueF m r) where
instance Convertible e m => FromNix Path m (NValueNF m) where
instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m) => FromNix Path m (NValueF m (NThunk m)) where
instance (Convertible e m, FromValue a m (NValueNF m), Show a) => FromNix [a] m (NValueNF m) where
instance Convertible e m => FromNix (HashMap Text (NValueNF m)) m (NValueNF m) where
instance Convertible e m => FromNix (HashMap Text (NValueNF m), HashMap Text SourcePos) m (NValueNF m) where
instance (Convertible e m, Show (NValueF m r)) => FromNix (HashMap Text r, HashMap Text SourcePos) m (NValueF m r) where
instance (Convertible e m, MonadEffects m, MonadThunk (NValue m) (NThunk m) m) => FromNix A.Value m (NValueNF m) where

instance (Convertible e m, MonadEffects m, MonadThunk (NValue m) (NThunk m) m) => FromNix A.Value m (NValueF m (NThunk m)) where
    fromNixMay = fromNixMay <=< normalForm . NValue Nothing
    fromNix    = fromNix <=< normalForm . NValue Nothing

instance FromNix a m (NValueF m (NThunk m)) => FromNix a m (NValue m) where
    fromNixMay = fromNixMay . baseValue
    fromNix    = fromNix . baseValue

instance (Monad m, FromNix a m v) => FromNix a m (m v) where
    fromNixMay = (>>= fromNixMay)
    fromNix    = (>>= fromNix)

instance (MonadThunk (NValue m) (NThunk m) m, FromNix a m (NValue m))
      => FromNix a m (NThunk m) where
    fromNixMay = force ?? fromNixMay
    fromNix    = force ?? fromNix

instance MonadThunk (NValue m) (NThunk m) m
      => FromNix (NThunk m) m (NValueF m (NThunk m)) where
    fromNixMay = pure . Just . value . NValue Nothing
    fromNix    = pure . value . NValue Nothing

class ToNix a m v where
    toNix :: a -> m v
    default toNix :: ToValue a m v => a -> m v
    toNix = toValue

instance (Framed e m, MonadThunk (NValue m) (NThunk m) m,
          ToNix a m (NValueF m (NThunk m)))
      => ToNix [a] m (NValueF m (NThunk m)) where
    toNix = fmap NVListF
        . traverse (thunk . withStringContext "While coercing to a list"
                          . fmap (NValue Nothing)
                          . toNix)

instance (Framed e m, MonadThunk (NValue m) (NThunk m) m,
          ToNix a m (NValueF m (NThunk m)))
      => ToNix (HashMap Text a) m (NValueF m (NThunk m)) where
    toNix = fmap (flip NVSetF M.empty)
        . traverse (thunk . withStringContext "While coercing to a set"
                          . fmap (NValue Nothing)
                          . toNix)

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
instance Applicative m => ToNix (HashMap Text (NValueNF m)) m (NValueNF m) where
instance Applicative m => ToNix (HashMap Text (NValueNF m), HashMap Text SourcePos) m (NValueNF m) where
instance Applicative m => ToNix (HashMap Text r, HashMap Text SourcePos) m (NValueF m r) where
instance (Framed e m, MonadThunk (NValue m) (NThunk m) m) => ToNix A.Value m (NValueF m (NThunk m)) where
instance Applicative m => ToNix Bool m (NExprF r) where
instance Applicative m => ToNix () m (NExprF r) where

instance (MonadThunk (NValue m) (NThunk m) m, ToNix a m (NValueF m (NThunk m)))
      => ToNix a m (NThunk m) where
    toNix = thunk . fmap (NValue Nothing) . toNix

instance (MonadThunk (NValue m) (NThunk m) m, ToNix a m (NValueF m (NThunk m)))
      => ToNix a m (NValue m) where
    toNix = fmap (NValue Nothing) . toNix

instance (Applicative m, ToNix a m (NValueNF m)) => ToNix [a] m (NValueNF m) where
    toNix = fmap (Fix . NVListF) . traverse toNix

instance MonadThunk (NValue m) (NThunk m) m => ToNix (NThunk m) m (NValue m) where
    toNix = force ?? pure

instance MonadThunk (NValue m) (NThunk m) m
      => ToNix (NThunk m) m (NValueF m (NThunk m)) where
    toNix = force ?? (pure . baseValue)
