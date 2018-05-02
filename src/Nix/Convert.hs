{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
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
import           Nix.Frames
import           Nix.Normal
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

class FromValue a m v where
    fromValue    :: v -> m a
    fromValueMay :: v -> m (Maybe a)

type Convertible e m = (Framed e m, MonadVar m, Typeable m)

instance Convertible e m => FromValue () m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF NNull) -> pure $ Just ()
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TNull v

instance Convertible e m
      => FromValue () m (NValue m) where
    fromValueMay = \case
        NVConstant NNull -> pure $ Just ()
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TNull v

instance Convertible e m
      => FromValue Bool m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF (NBool b)) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TBool v

instance Convertible e m
      => FromValue Bool m (NValue m) where
    fromValueMay = \case
        NVConstant (NBool b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TBool v

instance Convertible e m
      => FromValue Int m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF (NInt b)) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TInt v

instance Convertible e m
      => FromValue Int m (NValue m) where
    fromValueMay = \case
        NVConstant (NInt b) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TInt v

instance Convertible e m
      => FromValue Integer m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF (NInt b)) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TInt v

instance Convertible e m
      => FromValue Integer m (NValue m) where
    fromValueMay = \case
        NVConstant (NInt b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TInt v

instance Convertible e m
      => FromValue Float m (NValueNF m) where
    fromValueMay = \case
        Fix (NVConstantF (NFloat b)) -> pure $ Just b
        Fix (NVConstantF (NInt i)) -> pure $ Just (fromInteger i)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TFloat v

instance Convertible e m
      => FromValue Float m (NValue m) where
    fromValueMay = \case
        NVConstant (NFloat b) -> pure $ Just b
        NVConstant (NInt i) -> pure $ Just (fromInteger i)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TFloat v

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
        _ -> throwError $ ExpectationNF TString v

instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m, MonadEffects m)
      => FromValue Text m (NValue m) where
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
        _ -> throwError $ Expectation TString v

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
        _ -> throwError $ ExpectationNF TString v

instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m, MonadEffects m)
      => FromValue (Text, DList Text) m (NValue m) where
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
        _ -> throwError $ Expectation TString v

instance Convertible e m
      => FromValue ByteString m (NValueNF m) where
    fromValueMay = \case
        Fix (NVStrF t _) -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TString v

instance Convertible e m
      => FromValue ByteString m (NValue m) where
    fromValueMay = \case
        NVStr t _ -> pure $ Just (encodeUtf8 t)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TString v

newtype Path = Path { getPath :: FilePath }
    deriving Show

instance Convertible e m => FromValue Path m (NValueNF m) where
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
        _ -> throwError $ ExpectationNF TPath v

instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m)
      => FromValue Path m (NValue m) where
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
        _ -> throwError $ Expectation TPath v

instance (Convertible e m, FromValue a m (NValueNF m), Show a)
      => FromValue [a] m (NValueNF m) where
    fromValueMay = \case
        Fix (NVListF l) -> sequence <$> traverse fromValueMay l
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TList v

instance Convertible e m => FromValue [NThunk m] m (NValue m) where
    fromValueMay = \case
        NVList l -> pure $ Just l
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TList v

instance Convertible e m
      => FromValue (HashMap Text (NValueNF m)) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVSetF s _) -> pure $ Just s
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TSet v

instance Convertible e m
      => FromValue (HashMap Text (NThunk m)) m (NValue m) where
    fromValueMay = \case
        NVSet s _ -> pure $ Just s
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TSet v

instance Convertible e m
      => FromValue (HashMap Text (NValueNF m),
                 HashMap Text SourcePos) m (NValueNF m) where
    fromValueMay = \case
        Fix (NVSetF s p) -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TSet v

instance Convertible e m
      => FromValue (HashMap Text (NThunk m),
                   HashMap Text SourcePos) m (NValue m) where
    fromValueMay = \case
        NVSet s p -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TSet v

instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m)
      => FromValue (NThunk m) m (NValue m) where
    fromValueMay = pure . Just . value @_ @_ @m
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> error "Impossible, see fromValueMay"

instance (Monad m, FromValue a m v) => FromValue a m (m v) where
    fromValueMay = (>>= fromValueMay)
    fromValue    = (>>= fromValue)

instance (MonadThunk (NValue m) (NThunk m) m, FromValue a m (NValue m))
      => FromValue a m (NThunk m) where
    fromValueMay = force ?? fromValueMay
    fromValue    = force ?? fromValue

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
        _ -> throwError $ CoercionToJsonNF v

class ToValue a m v where
    toValue :: a -> m v

instance Applicative m => ToValue () m (NValueNF m) where
    toValue _ = pure . Fix . NVConstantF $ NNull

instance Applicative m => ToValue () m (NValue m) where
    toValue _ = pure . nvConstant $ NNull

instance Applicative m => ToValue Bool m (NValueNF m) where
    toValue = pure . Fix . NVConstantF . NBool

instance Applicative m => ToValue Bool m (NValue m) where
    toValue = pure . nvConstant . NBool

instance Applicative m => ToValue Int m (NValueNF m) where
    toValue = pure . Fix . NVConstantF . NInt . toInteger

instance Applicative m => ToValue Int m (NValue m) where
    toValue = pure . nvConstant . NInt . toInteger

instance Applicative m => ToValue Integer m (NValueNF m) where
    toValue = pure . Fix . NVConstantF . NInt

instance Applicative m => ToValue Integer m (NValue m) where
    toValue = pure . nvConstant . NInt

instance Applicative m => ToValue Float m (NValueNF m) where
    toValue = pure . Fix . NVConstantF . NFloat

instance Applicative m => ToValue Float m (NValue m) where
    toValue = pure . nvConstant . NFloat

instance Applicative m => ToValue Text m (NValueNF m) where
    toValue = pure . Fix . flip NVStrF mempty

instance Applicative m => ToValue Text m (NValue m) where
    toValue = pure . flip nvStr mempty

instance Applicative m => ToValue (Text, DList Text) m (NValueNF m) where
    toValue = pure . Fix . uncurry NVStrF

instance Applicative m => ToValue (Text, DList Text) m (NValue m) where
    toValue = pure . uncurry nvStr

instance Applicative m => ToValue ByteString m (NValueNF m) where
    toValue = pure . Fix . flip NVStrF mempty . decodeUtf8

instance Applicative m => ToValue ByteString m (NValue m) where
    toValue = pure . flip nvStr mempty . decodeUtf8

instance Applicative m => ToValue Path m (NValueNF m) where
    toValue = pure . Fix . NVPathF . getPath

instance Applicative m => ToValue Path m (NValue m) where
    toValue = pure . nvPath . getPath

instance MonadThunk (NValue m) (NThunk m) m
      => ToValue SourcePos m (NValue m) where
    toValue (SourcePos f l c) = do
        f' <- toValue (Text.pack f)
        l' <- toValue (unPos l)
        c' <- toValue (unPos c)
        let pos = M.fromList
                [ ("file" :: Text, value @_ @_ @m f')
                , ("line",        value @_ @_ @m l')
                , ("column",      value @_ @_ @m c') ]
        pure $ nvSet pos mempty

instance (ToValue a m (NValueNF m), Applicative m)
      => ToValue [a] m (NValueNF m) where
    toValue = fmap (Fix . NVListF) . traverse toValue

instance Applicative m => ToValue [NThunk m] m (NValue m) where
    toValue = pure . nvList

instance Applicative m
      => ToValue (HashMap Text (NValueNF m)) m (NValueNF m) where
    toValue = pure . Fix . flip NVSetF M.empty

instance Applicative m => ToValue (HashMap Text (NThunk m)) m (NValue m) where
    toValue = pure . flip nvSet M.empty

instance Applicative m => ToValue (HashMap Text (NValueNF m),
                HashMap Text SourcePos) m (NValueNF m) where
    toValue (s, p) = pure $ Fix $ NVSetF s p

instance Applicative m => ToValue (HashMap Text (NThunk m),
                HashMap Text SourcePos) m (NValue m) where
    toValue (s, p) = pure $ nvSet s p

instance (MonadThunk (NValue m) (NThunk m) m, ToValue a m (NValue m))
      => ToValue a m (NThunk m) where
    toValue = fmap (value @(NValue m) @_ @m) . toValue

instance Applicative m => ToValue Bool m (NExprF r) where
    toValue = pure . NConstant . NBool

instance Applicative m => ToValue () m (NExprF r) where
    toValue _ = pure . NConstant $ NNull

whileForcingThunk :: forall s e m r. (Framed e m, Exception s, Typeable m)
                  => s -> m r -> m r
whileForcingThunk frame =
    withFrame Debug (ForcingThunk @m) . withFrame Debug frame

instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m)
      => ToValue A.Value m (NValue m) where
    toValue = \case
        A.Object m -> flip nvSet M.empty
            <$> traverse (thunk . toValue @_ @_ @(NValue m)) m
        A.Array l -> nvList <$>
            traverse (\x -> thunk . whileForcingThunk (CoercionFromJson @m x)
                                 . toValue $ x) (V.toList l)
        A.String s -> pure $ nvStr s mempty
        A.Number n -> pure $ nvConstant $ case floatingOrInteger n of
            Left r -> NFloat r
            Right i -> NInt i
        A.Bool b -> pure $ nvConstant $ NBool b
        A.Null -> pure $ nvConstant NNull

class FromNix a m v where
    fromNix :: v -> m a
    default fromNix :: FromValue a m v => v -> m a
    fromNix = fromValue

    fromNixMay :: v -> m (Maybe a)
    default fromNixMay :: FromValue a m v => v -> m (Maybe a)
    fromNixMay = fromValueMay

instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m,
          FromNix a m (NValue m))
      => FromNix [a] m (NValue m) where
    fromNixMay = \case
        NVList l -> sequence <$> traverse (`force` fromNixMay) l
        _ -> pure Nothing
    fromNix v = fromNixMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TList v

instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m,
          FromNix a m (NValue m))
      => FromNix (HashMap Text a) m (NValue m) where
    fromNixMay = \case
        NVSet s _ -> sequence <$> traverse (`force` fromNixMay) s
        _ -> pure Nothing
    fromNix v = fromNixMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TSet v

instance Convertible e m => FromNix () m (NValueNF m) where
instance Convertible e m => FromNix () m (NValue m) where
instance Convertible e m => FromNix Bool m (NValueNF m) where
instance Convertible e m => FromNix Bool m (NValue m) where
instance Convertible e m => FromNix Int m (NValueNF m) where
instance Convertible e m => FromNix Int m (NValue m) where
instance Convertible e m => FromNix Integer m (NValueNF m) where
instance Convertible e m => FromNix Integer m (NValue m) where
instance Convertible e m => FromNix Float m (NValueNF m) where
instance Convertible e m => FromNix Float m (NValue m) where
instance (Convertible e m, MonadEffects m) => FromNix Text m (NValueNF m) where
instance (Convertible e m, MonadEffects m, MonadThunk (NValue m) (NThunk m) m) => FromNix Text m (NValue m) where
instance (Convertible e m, MonadEffects m) => FromNix (Text, DList Text) m (NValueNF m) where
instance (Convertible e m, MonadEffects m, MonadThunk (NValue m) (NThunk m) m) => FromNix (Text, DList Text) m (NValue m) where
instance Convertible e m => FromNix ByteString m (NValueNF m) where
instance Convertible e m => FromNix ByteString m (NValue m) where
instance Convertible e m => FromNix Path m (NValueNF m) where
instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m) => FromNix Path m (NValue m) where
instance (Convertible e m, FromValue a m (NValueNF m), Show a) => FromNix [a] m (NValueNF m) where
instance Convertible e m => FromNix (HashMap Text (NValueNF m)) m (NValueNF m) where
instance Convertible e m => FromNix (HashMap Text (NValueNF m), HashMap Text SourcePos) m (NValueNF m) where
instance Convertible e m => FromNix (HashMap Text (NThunk m), HashMap Text SourcePos) m (NValue m) where
instance (Convertible e m, MonadEffects m, MonadThunk (NValue m) (NThunk m) m) => FromNix A.Value m (NValueNF m) where

instance (Convertible e m, MonadEffects m,
          MonadThunk (NValue m) (NThunk m) m) => FromNix A.Value m (NValue m) where
    fromNixMay = fromNixMay <=< normalForm
    fromNix    = fromNix <=< normalForm

instance (Monad m, FromNix a m v) => FromNix a m (m v) where
    fromNixMay = (>>= fromNixMay)
    fromNix    = (>>= fromNix)

instance (MonadThunk (NValue m) (NThunk m) m, FromNix a m (NValue m))
      => FromNix a m (NThunk m) where
    fromNixMay = force ?? fromNixMay
    fromNix    = force ?? fromNix

instance MonadThunk (NValue m) (NThunk m) m
      => FromNix (NThunk m) m (NValue m) where
    fromNixMay = pure . Just . value
    fromNix    = pure . value

class ToNix a m v where
    toNix :: a -> m v
    default toNix :: ToValue a m v => a -> m v
    toNix = toValue

instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m,
          ToNix a m (NValue m))
      => ToNix [a] m (NValue m) where
    toNix = fmap nvList
        . traverse (thunk . ((\v -> whileForcingThunk (ConcerningValue v) (pure v))
                                <=< toNix))

instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m,
          ToNix a m (NValue m))
      => ToNix (HashMap Text a) m (NValue m) where
    toNix = fmap (flip nvSet M.empty)
        . traverse (thunk . ((\v -> whileForcingThunk (ConcerningValue v) (pure v))
                                <=< toNix))

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
instance Applicative m => ToNix (HashMap Text (NValueNF m)) m (NValueNF m) where
instance Applicative m => ToNix (HashMap Text (NValueNF m), HashMap Text SourcePos) m (NValueNF m) where
instance Applicative m => ToNix (HashMap Text (NThunk m), HashMap Text SourcePos) m (NValue m) where
instance (Convertible e m, MonadThunk (NValue m) (NThunk m) m) => ToNix A.Value m (NValue m) where
instance Applicative m => ToNix Bool m (NExprF r) where
instance Applicative m => ToNix () m (NExprF r) where

instance (MonadThunk (NValue m) (NThunk m) m, ToNix a m (NValue m))
      => ToNix a m (NThunk m) where
    toNix = thunk . toNix

instance (Applicative m, ToNix a m (NValueNF m)) => ToNix [a] m (NValueNF m) where
    toNix = fmap (Fix . NVListF) . traverse toNix

instance MonadThunk (NValue m) (NThunk m) m => ToNix (NThunk m) m (NValue m) where
    toNix = force ?? pure

convertNix :: forall a t m v. (FromNix a m t, ToNix a m v, Monad m) => t -> m v
convertNix = fromNix @a >=> toNix
