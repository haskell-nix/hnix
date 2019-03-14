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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import           Data.ByteString
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Nix.Atoms
import           Nix.Effects
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.String
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

{-

IMPORTANT NOTE

We used to have Text instances of FromValue, ToValue, FromNix, and ToNix.
However, we're removing these instances because they are dangerous due to the
fact that they hide the way string contexts are handled. It's better to have to
explicitly handle string context in a way that is appropriate for the situation.

Do not add these instances back!

-}

class FromValue a m v where
    fromValue    :: v -> m a
    fromValueMay :: v -> m (Maybe a)

type Convertible e f (g :: * -> *) m =
    (Framed e m,
     Typeable m,
     Typeable f,
     Typeable g,
     MonadThunk (NValue f g m) (NThunk f g m) m,
     MonadDataContext f m)

instance Convertible e f g m => FromValue () m (NValueNF f g m) where
    fromValueMay = \case
        NVConstantNF NNull -> pure $ Just ()
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TNull v

instance Convertible e f g m
      => FromValue () m (NValue f g m) where
    fromValueMay = \case
        NVConstant NNull -> pure $ Just ()
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TNull v

instance Convertible e f g m
      => FromValue Bool m (NValueNF f g m) where
    fromValueMay = \case
        NVConstantNF (NBool b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TBool v

instance Convertible e f g m
      => FromValue Bool m (NValue f g m) where
    fromValueMay = \case
        NVConstant (NBool b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TBool v

instance Convertible e f g m
      => FromValue Int m (NValueNF f g m) where
    fromValueMay = \case
        NVConstantNF (NInt b) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TInt v

instance Convertible e f g m
      => FromValue Int m (NValue f g m) where
    fromValueMay = \case
        NVConstant (NInt b) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TInt v

instance Convertible e f g m
      => FromValue Integer m (NValueNF f g m) where
    fromValueMay = \case
        NVConstantNF (NInt b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TInt v

instance Convertible e f g m
      => FromValue Integer m (NValue f g m) where
    fromValueMay = \case
        NVConstant (NInt b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TInt v

instance Convertible e f g m
      => FromValue Float m (NValueNF f g m) where
    fromValueMay = \case
        NVConstantNF (NFloat b) -> pure $ Just b
        NVConstantNF (NInt i) -> pure $ Just (fromInteger i)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TFloat v

instance Convertible e f g m
      => FromValue Float m (NValue f g m) where
    fromValueMay = \case
        NVConstant (NFloat b) -> pure $ Just b
        NVConstant (NInt i) -> pure $ Just (fromInteger i)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TFloat v

instance (Convertible e f g m, MonadEffects f g m)
      => FromValue NixString m (NValueNF f g m) where
    fromValueMay = \case
        NVStrNF ns -> pure $ Just ns
        NVPathNF p -> Just . hackyMakeNixStringWithoutContext . Text.pack . unStorePath <$> addPath p
        NVSetNF s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF (TString NoContext) v

instance (Convertible e f g m, MonadThunk (NValue f g m) (NThunk f g m) m, MonadEffects f g m)
      => FromValue NixString m (NValue f g m) where
    fromValueMay = \case
        NVStr ns -> pure $ Just ns
        NVPath p -> Just . hackyMakeNixStringWithoutContext . Text.pack . unStorePath <$> addPath p
        NVSet s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation (TString NoContext) v

instance Convertible e f g m
      => FromValue ByteString m (NValueNF f g m) where
    fromValueMay = \case
        NVStrNF ns -> pure $ encodeUtf8 <$> hackyGetStringNoContext ns
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF (TString NoContext) v

instance Convertible e f g m
      => FromValue ByteString m (NValue f g m) where
    fromValueMay = \case
        NVStr ns -> pure $ encodeUtf8 <$> hackyGetStringNoContext ns
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation (TString NoContext) v

newtype Path = Path { getPath :: FilePath }
    deriving Show

instance Convertible e f g m => FromValue Path m (NValueNF f g m) where
    fromValueMay = \case
        NVPathNF p -> pure $ Just (Path p)
        NVStrNF ns -> pure $ Path . Text.unpack <$> hackyGetStringNoContext ns
        NVSetNF s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay @Path p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TPath v

instance (Convertible e f g m, MonadThunk (NValue f g m) (NThunk f g m) m)
      => FromValue Path m (NValue f g m) where
    fromValueMay = \case
        NVPath p -> pure $ Just (Path p)
        NVStr ns -> pure $ Path . Text.unpack <$> hackyGetStringNoContext ns
        NVSet s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay @Path p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TPath v

instance (Convertible e f g m, FromValue a m (NValueNF f g m), Show a)
      => FromValue [a] m (NValueNF f g m) where
    fromValueMay = \case
        NVListNF l -> sequence <$> traverse fromValueMay l
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TList v

instance Convertible e f g m => FromValue [NThunk f g m] m (NValue f g m) where
    fromValueMay = \case
        NVList l -> pure $ Just l
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TList v

instance Convertible e f g m
      => FromValue (HashMap Text (NValueNF f g m)) m (NValueNF f g m) where
    fromValueMay = \case
        NVSetNF s _ -> pure $ Just s
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TSet v

instance Convertible e f g m
      => FromValue (HashMap Text (NThunk f g m)) m (NValue f g m) where
    fromValueMay = \case
        NVSet s _ -> pure $ Just s
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TSet v

instance Convertible e f g m
      => FromValue (HashMap Text (NValueNF f g m),
                 HashMap Text SourcePos) m (NValueNF f g m) where
    fromValueMay = \case
        NVSetNF s p -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TSet v

instance Convertible e f g m
      => FromValue (HashMap Text (NThunk f g m),
                   HashMap Text SourcePos) m (NValue f g m) where
    fromValueMay = \case
        NVSet s p -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TSet v

instance (Convertible e f g m, MonadThunk (NValue f g m) (NThunk f g m) m)
      => FromValue (NThunk f g m) m (NValue f g m) where
    fromValueMay = pure . Just . wrapValue @_ @_ @m
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> error "Impossible, see fromValueMay"

instance (Monad m, FromValue a m v) => FromValue a m (m v) where
    fromValueMay = (>>= fromValueMay)
    fromValue    = (>>= fromValue)

instance (MonadThunk (NValue f g m) (NThunk f g m) m, FromValue a m (NValue f g m))
      => FromValue a m (NThunk f g m) where
    fromValueMay = force ?? fromValueMay
    fromValue    = force ?? fromValue

class ToValue a m v where
    toValue :: a -> m v

instance Convertible e f g m => ToValue () m (NValueNF f g m) where
    toValue _ = pure . nvConstantNF $ NNull

instance Convertible e f g m => ToValue () m (NValue f g m) where
    toValue _ = pure . nvConstant $ NNull

instance Convertible e f g m => ToValue Bool m (NValueNF f g m) where
    toValue = pure . nvConstantNF . NBool

instance Convertible e f g m => ToValue Bool m (NValue f g m) where
    toValue = pure . nvConstant . NBool

instance Convertible e f g m => ToValue Int m (NValueNF f g m) where
    toValue = pure . nvConstantNF . NInt . toInteger

instance Convertible e f g m => ToValue Int m (NValue f g m) where
    toValue = pure . nvConstant . NInt . toInteger

instance Convertible e f g m => ToValue Integer m (NValueNF f g m) where
    toValue = pure . nvConstantNF . NInt

instance Convertible e f g m => ToValue Integer m (NValue f g m) where
    toValue = pure . nvConstant . NInt

instance Convertible e f g m => ToValue Float m (NValueNF f g m) where
    toValue = pure . nvConstantNF . NFloat

instance Convertible e f g m => ToValue Float m (NValue f g m) where
    toValue = pure . nvConstant . NFloat

instance Convertible e f g m => ToValue NixString m (NValueNF f g m) where
    toValue = pure . nvStrNF

instance Convertible e f g m => ToValue NixString m (NValue f g m) where
    toValue = pure . nvStr

instance Convertible e f g m => ToValue ByteString m (NValueNF f g m) where
    toValue = pure . nvStrNF . hackyMakeNixStringWithoutContext . decodeUtf8

instance Convertible e f g m => ToValue ByteString m (NValue f g m) where
    toValue = pure . nvStr . hackyMakeNixStringWithoutContext . decodeUtf8

instance Convertible e f g m => ToValue Path m (NValueNF f g m) where
    toValue = pure . nvPathNF . getPath

instance Convertible e f g m => ToValue Path m (NValue f g m) where
    toValue = pure . nvPath . getPath

instance Convertible e f g m => ToValue StorePath m (NValueNF f g m) where
    toValue = toValue . Path . unStorePath

instance Convertible e f g m => ToValue StorePath m (NValue f g m) where
    toValue = toValue . Path . unStorePath

instance Convertible e f g m => ToValue SourcePos m (NValue f g m) where
    toValue (SourcePos f l c) = do
        f' <- pure $ nvStr $ principledMakeNixStringWithoutContext (Text.pack f)
        l' <- toValue (unPos l)
        c' <- toValue (unPos c)
        let pos = M.fromList
                [ ("file" :: Text, wrapValue @_ @_ @m f')
                , ("line",        wrapValue @_ @_ @m l')
                , ("column",      wrapValue @_ @_ @m c') ]
        pure $ nvSet pos mempty

instance (Convertible e f g m, ToValue a m (NValueNF f g m))
      => ToValue [a] m (NValueNF f g m) where
    toValue = fmap nvListNF . traverse toValue

instance Convertible e f g m => ToValue [NThunk f g m] m (NValue f g m) where
    toValue = pure . nvList

instance Convertible e f g m
      => ToValue (HashMap Text (NValueNF f g m)) m (NValueNF f g m) where
    toValue = pure . flip nvSetNF M.empty

instance Convertible e f g m => ToValue (HashMap Text (NThunk f g m)) m (NValue f g m) where
    toValue = pure . flip nvSet M.empty

instance Convertible e f g m => ToValue (HashMap Text (NValueNF f g m),
                HashMap Text SourcePos) m (NValueNF f g m) where
    toValue (s, p) = pure $ nvSetNF s p

instance Convertible e f g m => ToValue (HashMap Text (NThunk f g m),
                HashMap Text SourcePos) m (NValue f g m) where
    toValue (s, p) = pure $ nvSet s p

instance (MonadThunk (NValue f g m) (NThunk f g m) m, ToValue a m (NValue f g m))
      => ToValue a m (NThunk f g m) where
    toValue = fmap (wrapValue @(NValue f g m) @_ @m) . toValue

instance Convertible e f g m => ToValue Bool m (NExprF r) where
    toValue = pure . NConstant . NBool

instance Convertible e f g m => ToValue () m (NExprF r) where
    toValue _ = pure . NConstant $ NNull

whileForcingThunk :: forall f g s e m r. (Exception s, Convertible e f g m)
                  => s -> m r -> m r
whileForcingThunk frame =
    withFrame Debug (ForcingThunk @f @g @m) . withFrame Debug frame

class FromNix a m v where
    fromNix :: v -> m a
    default fromNix :: FromValue a m v => v -> m a
    fromNix = fromValue

    fromNixMay :: v -> m (Maybe a)
    default fromNixMay :: FromValue a m v => v -> m (Maybe a)
    fromNixMay = fromValueMay

instance (Convertible e f g m, MonadThunk (NValue f g m) (NThunk f g m) m,
          FromNix a m (NValue f g m))
      => FromNix [a] m (NValue f g m) where
    fromNixMay = \case
        NVList l -> sequence <$> traverse (`force` fromNixMay) l
        _ -> pure Nothing
    fromNix v = fromNixMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TList v

instance (Convertible e f g m, MonadThunk (NValue f g m) (NThunk f g m) m,
          FromNix a m (NValue f g m))
      => FromNix (HashMap Text a) m (NValue f g m) where
    fromNixMay = \case
        NVSet s _ -> sequence <$> traverse (`force` fromNixMay) s
        _ -> pure Nothing
    fromNix v = fromNixMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TSet v

instance Convertible e f g m => FromNix () m (NValueNF f g m) where
instance Convertible e f g m => FromNix () m (NValue f g m) where
instance Convertible e f g m => FromNix Bool m (NValueNF f g m) where
instance Convertible e f g m => FromNix Bool m (NValue f g m) where
instance Convertible e f g m => FromNix Int m (NValueNF f g m) where
instance Convertible e f g m => FromNix Int m (NValue f g m) where
instance Convertible e f g m => FromNix Integer m (NValueNF f g m) where
instance Convertible e f g m => FromNix Integer m (NValue f g m) where
instance Convertible e f g m => FromNix Float m (NValueNF f g m) where
instance Convertible e f g m => FromNix Float m (NValue f g m) where
instance (Convertible e f g m, MonadEffects f g m) => FromNix NixString m (NValueNF f g m) where
instance (Convertible e f g m, MonadEffects f g m, MonadThunk (NValue f g m) (NThunk f g m) m) => FromNix NixString m (NValue f g m) where
instance Convertible e f g m => FromNix ByteString m (NValueNF f g m) where
instance Convertible e f g m => FromNix ByteString m (NValue f g m) where
instance Convertible e f g m => FromNix Path m (NValueNF f g m) where
instance (Convertible e f g m, MonadThunk (NValue f g m) (NThunk f g m) m) => FromNix Path m (NValue f g m) where
instance (Convertible e f g m, FromValue a m (NValueNF f g m), Show a) => FromNix [a] m (NValueNF f g m) where
instance Convertible e f g m => FromNix (HashMap Text (NValueNF f g m)) m (NValueNF f g m) where
instance Convertible e f g m => FromNix (HashMap Text (NValueNF f g m), HashMap Text SourcePos) m (NValueNF f g m) where
instance Convertible e f g m => FromNix (HashMap Text (NThunk f g m), HashMap Text SourcePos) m (NValue f g m) where

instance (Monad m, FromNix a m v) => FromNix a m (m v) where
    fromNixMay = (>>= fromNixMay)
    fromNix    = (>>= fromNix)

instance (MonadThunk (NValue f g m) (NThunk f g m) m, FromNix a m (NValue f g m))
      => FromNix a m (NThunk f g m) where
    fromNixMay = force ?? fromNixMay
    fromNix    = force ?? fromNix

instance MonadThunk (NValue f g m) (NThunk f g m) m
      => FromNix (NThunk f g m) m (NValue f g m) where
    fromNixMay = pure . Just . wrapValue
    fromNix    = pure . wrapValue

class ToNix a m v where
    toNix :: a -> m v
    default toNix :: ToValue a m v => a -> m v
    toNix = toValue

instance (Convertible e f g m, MonadThunk (NValue f g m) (NThunk f g m) m,
          ToNix a m (NValue f g m))
      => ToNix [a] m (NValue f g m) where
    toNix = fmap nvList . traverse
        (thunk . ((\v -> whileForcingThunk @f @g (ConcerningValue v) (pure v))
                     <=< toNix))

instance (Convertible e f g m, MonadThunk (NValue f g m) (NThunk f g m) m,
          ToNix a m (NValue f g m))
      => ToNix (HashMap Text a) m (NValue f g m) where
    toNix = fmap (flip nvSet M.empty) . traverse
        (thunk . ((\v -> whileForcingThunk @f @g (ConcerningValue v) (pure v))
                     <=< toNix))

instance Convertible e f g m => ToNix () m (NValueNF f g m) where
instance Convertible e f g m => ToNix () m (NValue f g m) where
instance Convertible e f g m => ToNix Bool m (NValueNF f g m) where
instance Convertible e f g m => ToNix Bool m (NValue f g m) where
instance Convertible e f g m => ToNix Int m (NValueNF f g m) where
instance Convertible e f g m => ToNix Int m (NValue f g m) where
instance Convertible e f g m => ToNix Integer m (NValueNF f g m) where
instance Convertible e f g m => ToNix Integer m (NValue f g m) where
instance Convertible e f g m => ToNix Float m (NValueNF f g m) where
instance Convertible e f g m => ToNix Float m (NValue f g m) where
instance Convertible e f g m => ToNix NixString m (NValueNF f g m) where
instance Convertible e f g m => ToNix NixString m (NValue f g m) where
instance Convertible e f g m => ToNix ByteString m (NValueNF f g m) where
instance Convertible e f g m => ToNix ByteString m (NValue f g m) where
instance Convertible e f g m => ToNix Path m (NValueNF f g m) where
instance Convertible e f g m => ToNix Path m (NValue f g m) where
instance Convertible e f g m => ToNix (HashMap Text (NValueNF f g m)) m (NValueNF f g m) where
instance Convertible e f g m => ToNix (HashMap Text (NValueNF f g m), HashMap Text SourcePos) m (NValueNF f g m) where
instance Convertible e f g m => ToNix (HashMap Text (NThunk f g m), HashMap Text SourcePos) m (NValue f g m) where

instance Convertible e f g m => ToNix Bool m (NExprF r) where
    toNix = pure . NConstant . NBool

instance Convertible e f g m => ToNix () m (NExprF r) where
    toNix _ = pure $ NConstant NNull

instance (MonadThunk (NValue f g m) (NThunk f g m) m, ToNix a m (NValue f g m))
      => ToNix a m (NThunk f g m) where
    toNix = thunk . toNix

instance (Convertible e f g m, ToNix a m (NValueNF f g m)) => ToNix [a] m (NValueNF f g m) where
    toNix = fmap nvListNF . traverse toNix

instance MonadThunk (NValue f g m) (NThunk f g m) m => ToNix (NThunk f g m) m (NValue f g m) where
    toNix = force ?? pure

convertNix :: forall a t m v. (FromNix a m t, ToNix a m v, Monad m) => t -> m v
convertNix = fromNix @a >=> toNix
