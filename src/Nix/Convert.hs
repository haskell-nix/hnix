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
-- import           Nix.Utils
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

type Convertible e t f m =
    (Framed e m, MonadThunk t m (NValue t f m), MonadDataErrorContext t f m)

instance Convertible e t f m => FromValue () m (NValueNF t f m) where
    fromValueMay = \case
        NVConstantNF NNull -> pure $ Just ()
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TNull v

instance Convertible e t f m => FromValue () m (NValue t f m) where
    fromValueMay = \case
        NVConstant NNull -> pure $ Just ()
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TNull v

instance Convertible e t f m => FromValue Bool m (NValueNF t f m) where
    fromValueMay = \case
        NVConstantNF (NBool b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TBool v

instance Convertible e t f m => FromValue Bool m (NValue t f m) where
    fromValueMay = \case
        NVConstant (NBool b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TBool v

instance Convertible e t f m => FromValue Int m (NValueNF t f m) where
    fromValueMay = \case
        NVConstantNF (NInt b) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TInt v

instance Convertible e t f m => FromValue Int m (NValue t f m) where
    fromValueMay = \case
        NVConstant (NInt b) -> pure $ Just (fromInteger b)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TInt v

instance Convertible e t f m => FromValue Integer m (NValueNF t f m) where
    fromValueMay = \case
        NVConstantNF (NInt b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TInt v

instance Convertible e t f m => FromValue Integer m (NValue t f m) where
    fromValueMay = \case
        NVConstant (NInt b) -> pure $ Just b
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TInt v

instance Convertible e t f m => FromValue Float m (NValueNF t f m) where
    fromValueMay = \case
        NVConstantNF (NFloat b) -> pure $ Just b
        NVConstantNF (NInt i) -> pure $ Just (fromInteger i)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TFloat v

instance Convertible e t f m => FromValue Float m (NValue t f m) where
    fromValueMay = \case
        NVConstant (NFloat b) -> pure $ Just b
        NVConstant (NInt i) -> pure $ Just (fromInteger i)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TFloat v

instance (Convertible e t f m, MonadEffects t f m)
      => FromValue NixString m (NValueNF t f m) where
    fromValueMay = \case
        NVStrNF ns -> pure $ Just ns
        NVPathNF p ->
            Just . hackyMakeNixStringWithoutContext
                 . Text.pack . unStorePath <$> addPath p
        NVSetNF s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF (TString NoContext) v

instance (Convertible e t f m, MonadEffects t f m, FromValue NixString m t)
      => FromValue NixString m (NValue t f m) where
    fromValueMay = \case
        NVStr ns -> pure $ Just ns
        NVPath p ->
            Just . hackyMakeNixStringWithoutContext
                 . Text.pack . unStorePath <$> addPath p
        NVSet s _ -> case M.lookup "outPath" s of
            Nothing -> pure Nothing
            Just p -> fromValueMay p
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation (TString NoContext) v

instance Convertible e t f m
      => FromValue ByteString m (NValueNF t f m) where
    fromValueMay = \case
        NVStrNF ns -> pure $ encodeUtf8 <$> hackyGetStringNoContext ns
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF (TString NoContext) v

instance Convertible e t f m
      => FromValue ByteString m (NValue t f m) where
    fromValueMay = \case
        NVStr ns -> pure $ encodeUtf8 <$> hackyGetStringNoContext ns
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation (TString NoContext) v

newtype Path = Path { getPath :: FilePath }
    deriving Show

instance Convertible e t f m => FromValue Path m (NValueNF t f m) where
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

instance (Convertible e t f m, FromValue Path m t)
  => FromValue Path m (NValue t f m) where
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

instance (Convertible e t f m, FromValue a m (NValueNF t f m), Show a)
      => FromValue [a] m (NValueNF t f m) where
    fromValueMay = \case
        NVListNF l -> sequence <$> traverse fromValueMay l
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TList v

instance Convertible e t f m => FromValue [t] m (NValue t f m) where
    fromValueMay = \case
        NVList l -> pure $ Just l
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TList v

instance Convertible e t f m
      => FromValue (HashMap Text (NValueNF t f m)) m (NValueNF t f m) where
    fromValueMay = \case
        NVSetNF s _ -> pure $ Just s
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TSet v

instance Convertible e t f m
      => FromValue (HashMap Text t) m (NValue t f m) where
    fromValueMay = \case
        NVSet s _ -> pure $ Just s
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TSet v

instance Convertible e t f m
      => FromValue (HashMap Text (NValueNF t f m),
                 HashMap Text SourcePos) m (NValueNF t f m) where
    fromValueMay = \case
        NVSetNF s p -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ ExpectationNF TSet v

instance Convertible e t f m
      => FromValue (HashMap Text t,
                   HashMap Text SourcePos) m (NValue t f m) where
    fromValueMay = \case
        NVSet s p -> pure $ Just (s, p)
        _ -> pure Nothing
    fromValue v = fromValueMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TSet v

-- instance Convertible e t f m => FromValue t m (NValue t f m) where
--     fromValueMay = pure . Just . wrapValue @_ @_ @m
--     fromValue v = fromValueMay v >>= \case
--         Just b -> pure b
--         _ -> error "Impossible, see fromValueMay"

instance (Monad m, FromValue a m v) => FromValue a m (m v) where
    fromValueMay = (>>= fromValueMay)
    fromValue    = (>>= fromValue)

-- instance FromValue a m (NValue t f m) => FromValue a m t where
--     fromValueMay = force ?? fromValueMay
--     fromValue    = force ?? fromValue

class ToValue a m v where
    toValue :: a -> m v

instance Convertible e t f m => ToValue () m (NValueNF t f m) where
    toValue _ = pure . nvConstantNF $ NNull

instance Convertible e t f m => ToValue () m (NValue t f m) where
    toValue _ = pure . nvConstant $ NNull

instance Convertible e t f m => ToValue Bool m (NValueNF t f m) where
    toValue = pure . nvConstantNF . NBool

instance Convertible e t f m => ToValue Bool m (NValue t f m) where
    toValue = pure . nvConstant . NBool

instance Convertible e t f m => ToValue Int m (NValueNF t f m) where
    toValue = pure . nvConstantNF . NInt . toInteger

instance Convertible e t f m => ToValue Int m (NValue t f m) where
    toValue = pure . nvConstant . NInt . toInteger

instance Convertible e t f m => ToValue Integer m (NValueNF t f m) where
    toValue = pure . nvConstantNF . NInt

instance Convertible e t f m => ToValue Integer m (NValue t f m) where
    toValue = pure . nvConstant . NInt

instance Convertible e t f m => ToValue Float m (NValueNF t f m) where
    toValue = pure . nvConstantNF . NFloat

instance Convertible e t f m => ToValue Float m (NValue t f m) where
    toValue = pure . nvConstant . NFloat

instance Convertible e t f m => ToValue NixString m (NValueNF t f m) where
    toValue = pure . nvStrNF

instance Convertible e t f m => ToValue NixString m (NValue t f m) where
    toValue = pure . nvStr

instance Convertible e t f m => ToValue ByteString m (NValueNF t f m) where
    toValue = pure . nvStrNF . hackyMakeNixStringWithoutContext . decodeUtf8

instance Convertible e t f m => ToValue ByteString m (NValue t f m) where
    toValue = pure . nvStr . hackyMakeNixStringWithoutContext . decodeUtf8

instance Convertible e t f m => ToValue Path m (NValueNF t f m) where
    toValue = pure . nvPathNF . getPath

instance Convertible e t f m => ToValue Path m (NValue t f m) where
    toValue = pure . nvPath . getPath

instance Convertible e t f m => ToValue StorePath m (NValueNF t f m) where
    toValue = toValue . Path . unStorePath

instance Convertible e t f m => ToValue StorePath m (NValue t f m) where
    toValue = toValue . Path . unStorePath

instance Convertible e t f m => ToValue SourcePos m (NValue t f m) where
    toValue (SourcePos f l c) = do
        f' <- pure $ nvStr $ principledMakeNixStringWithoutContext (Text.pack f)
        l' <- toValue (unPos l)
        c' <- toValue (unPos c)
        let pos = M.fromList
                [ ("file" :: Text, wrapValue f')
                , ("line",        wrapValue l')
                , ("column",      wrapValue c') ]
        pure $ nvSet pos mempty

instance (Convertible e t f m, ToValue a m (NValueNF t f m))
      => ToValue [a] m (NValueNF t f m) where
    toValue = fmap nvListNF . traverse toValue

instance Convertible e t f m => ToValue [t] m (NValue t f m) where
    toValue = pure . nvList

instance Convertible e t f m
      => ToValue (HashMap Text (NValueNF t f m)) m (NValueNF t f m) where
    toValue = pure . flip nvSetNF M.empty

instance Convertible e t f m => ToValue (HashMap Text t) m (NValue t f m) where
    toValue = pure . flip nvSet M.empty

instance Convertible e t f m => ToValue (HashMap Text (NValueNF t f m),
                HashMap Text SourcePos) m (NValueNF t f m) where
    toValue (s, p) = pure $ nvSetNF s p

instance Convertible e t f m => ToValue (HashMap Text t,
                HashMap Text SourcePos) m (NValue t f m) where
    toValue (s, p) = pure $ nvSet s p

-- instance (MonadThunk t m (NValue t f m), ToValue a m (NValue t f m))
--       => ToValue a m t where
--     toValue = fmap (wrapValue @(NValue t f m) @_ @m) . toValue

instance Convertible e t f m => ToValue Bool m (NExprF r) where
    toValue = pure . NConstant . NBool

instance Convertible e t f m => ToValue () m (NExprF r) where
    toValue _ = pure . NConstant $ NNull

whileForcingThunk :: forall t f m s e r. (Exception s, Convertible e t f m)
                  => s -> m r -> m r
whileForcingThunk frame =
    withFrame Debug (ForcingThunk @t @f @m) . withFrame Debug frame

class FromNix a m v where
    fromNix :: v -> m a
    default fromNix :: FromValue a m v => v -> m a
    fromNix = fromValue

    fromNixMay :: v -> m (Maybe a)
    default fromNixMay :: FromValue a m v => v -> m (Maybe a)
    fromNixMay = fromValueMay

instance (Convertible e t f m, FromNix a m (NValue t f m))
      => FromNix [a] m (NValue t f m) where
    fromNixMay = \case
        NVList l -> sequence <$> traverse (`force` fromNixMay) l
        _ -> pure Nothing
    fromNix v = fromNixMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TList v

instance (Convertible e t f m, FromNix a m (NValue t f m))
      => FromNix (HashMap Text a) m (NValue t f m) where
    fromNixMay = \case
        NVSet s _ -> sequence <$> traverse (`force` fromNixMay) s
        _ -> pure Nothing
    fromNix v = fromNixMay v >>= \case
        Just b -> pure b
        _ -> throwError $ Expectation TSet v

instance Convertible e t f m => FromNix () m (NValueNF t f m) where
instance Convertible e t f m => FromNix () m (NValue t f m) where
instance Convertible e t f m => FromNix Bool m (NValueNF t f m) where
instance Convertible e t f m => FromNix Bool m (NValue t f m) where
instance Convertible e t f m => FromNix Int m (NValueNF t f m) where
instance Convertible e t f m => FromNix Int m (NValue t f m) where
instance Convertible e t f m => FromNix Integer m (NValueNF t f m) where
instance Convertible e t f m => FromNix Integer m (NValue t f m) where
instance Convertible e t f m => FromNix Float m (NValueNF t f m) where
instance Convertible e t f m => FromNix Float m (NValue t f m) where
instance (Convertible e t f m, MonadEffects t f m)
  => FromNix NixString m (NValueNF t f m) where
instance (Convertible e t f m, MonadEffects t f m, FromValue NixString m t)
  => FromNix NixString m (NValue t f m) where
instance Convertible e t f m => FromNix ByteString m (NValueNF t f m) where
instance Convertible e t f m => FromNix ByteString m (NValue t f m) where
instance Convertible e t f m => FromNix Path m (NValueNF t f m) where
instance (Convertible e t f m, FromValue Path m t)
  => FromNix Path m (NValue t f m) where
instance (Convertible e t f m, FromValue a m (NValueNF t f m), Show a)
  => FromNix [a] m (NValueNF t f m) where
instance Convertible e t f m
  => FromNix (HashMap Text (NValueNF t f m)) m (NValueNF t f m) where
instance Convertible e t f m
  => FromNix (HashMap Text (NValueNF t f m),
             HashMap Text SourcePos) m (NValueNF t f m) where
instance Convertible e t f m
  => FromNix (HashMap Text t, HashMap Text SourcePos) m (NValue t f m) where

instance (Monad m, FromNix a m v) => FromNix a m (m v) where
    fromNixMay = (>>= fromNixMay)
    fromNix    = (>>= fromNix)

-- instance (MonadThunk t m (NValue t f m), FromNix a m (NValue t f m))
--       => FromNix a m t where
--     fromNixMay = force ?? fromNixMay
--     fromNix    = force ?? fromNix

-- instance MonadThunk t m (NValue t f m) => FromNix t m (NValue t f m) where
--     fromNixMay = pure . Just . wrapValue
--     fromNix    = pure . wrapValue

class ToNix a m v where
    toNix :: a -> m v
    default toNix :: ToValue a m v => a -> m v
    toNix = toValue

instance (Convertible e t f m, ToNix a m (NValue t f m))
      => ToNix [a] m (NValue t f m) where
    toNix = fmap nvList . traverse (thunk . go)
      where
        go = (\v -> whileForcingThunk @t @f @m (ConcerningValue v) (pure v))
            <=< toNix

instance (Convertible e t f m, ToNix a m (NValue t f m))
      => ToNix (HashMap Text a) m (NValue t f m) where
    toNix = fmap (flip nvSet M.empty) . traverse (thunk . go)
      where
        go = (\v -> whileForcingThunk @t @f @m (ConcerningValue v) (pure v))
            <=< toNix

instance Convertible e t f m => ToNix () m (NValueNF t f m) where
instance Convertible e t f m => ToNix () m (NValue t f m) where
instance Convertible e t f m => ToNix Bool m (NValueNF t f m) where
instance Convertible e t f m => ToNix Bool m (NValue t f m) where
instance Convertible e t f m => ToNix Int m (NValueNF t f m) where
instance Convertible e t f m => ToNix Int m (NValue t f m) where
instance Convertible e t f m => ToNix Integer m (NValueNF t f m) where
instance Convertible e t f m => ToNix Integer m (NValue t f m) where
instance Convertible e t f m => ToNix Float m (NValueNF t f m) where
instance Convertible e t f m => ToNix Float m (NValue t f m) where
instance Convertible e t f m => ToNix NixString m (NValueNF t f m) where
instance Convertible e t f m => ToNix NixString m (NValue t f m) where
instance Convertible e t f m => ToNix ByteString m (NValueNF t f m) where
instance Convertible e t f m => ToNix ByteString m (NValue t f m) where
instance Convertible e t f m => ToNix Path m (NValueNF t f m) where
instance Convertible e t f m => ToNix Path m (NValue t f m) where
instance Convertible e t f m
  => ToNix (HashMap Text (NValueNF t f m)) m (NValueNF t f m) where
instance Convertible e t f m
  => ToNix (HashMap Text (NValueNF t f m),
           HashMap Text SourcePos) m (NValueNF t f m) where
instance Convertible e t f m
  => ToNix (HashMap Text t, HashMap Text SourcePos) m (NValue t f m) where

instance Convertible e t f m => ToNix Bool m (NExprF r) where
    toNix = pure . NConstant . NBool

instance Convertible e t f m => ToNix () m (NExprF r) where
    toNix _ = pure $ NConstant NNull

-- instance (MonadThunk t m (NValue t f m), ToNix a m (NValue t f m))
--       => ToNix a m t where
--     toNix = thunk . toNix

instance (Convertible e t f m, ToNix a m (NValueNF t f m))
  => ToNix [a] m (NValueNF t f m) where
    toNix = fmap nvListNF . traverse toNix

-- instance MonadThunk t m (NValue t f m) => ToNix t m (NValue t f m) where
--     toNix = force ?? pure

convertNix :: forall a t m v. (FromNix a m t, ToNix a m v, Monad m) => t -> m v
convertNix = fromNix @a >=> toNix
