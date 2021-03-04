{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Although there are a lot of instances in this file, really it's just a
--   combinatorial explosion of the following combinations:
--
--   - Several Haskell types being converted to/from Nix wrappers
--   - Several types of Nix wrappers
--   - Whether to be shallow or deep while unwrapping

module Nix.Convert where

import           Control.Monad                  ( (<=<) )
import           Control.Monad.Free
import           Data.ByteString
import qualified Data.HashMap.Lazy             as M
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Nix.Atoms
import           Nix.Effects
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.String
import           Nix.Value
import           Nix.Value.Monad
import           Nix.Thunk
import           Nix.Utils

newtype Deeper a = Deeper { getDeeper :: a }
  deriving (Typeable, Functor, Foldable, Traversable)

{-

IMPORTANT NOTE

We used to have Text instances of FromValue, ToValue, FromNix, and ToNix.
However, we're removing these instances because they are dangerous due to the
fact that they hide the way string contexts are handled. It's better to have to
explicitly handle string context in a way that is appropriate for the situation.

Do not add these instances back!

-}


-- * FromValue

class FromValue a m v where
  fromValue    :: v -> m a
  fromValueMay :: v -> m (Maybe a)


-- Please, hide these helper function from export, to be sure they get optimized away.
fromMayToValue
  :: forall t f m a e
  . ( Convertible e t f m
    , FromValue a m (NValue' t f m (NValue t f m))
    )
  => ValueType
  -> NValue' t f m (NValue t f m)
  -> m a
fromMayToValue t v =
  do
    v' <- fromValueMay v
    maybe
      (throwError $ Expectation @t @f @m t (Free v))
      pure
      v'

fromMayToDeeperValue
  :: forall t f m a e m1
  . ( Convertible e t f m
    , FromValue (m1 a) m (Deeper (NValue' t f m (NValue t f m)))
    )
  => ValueType
  -> Deeper (NValue' t f m (NValue t f m))
  -> m (m1 a)
fromMayToDeeperValue t v =
  do
    v' <- fromValueMay v
    maybe
      (throwError $ Expectation @t @f @m t (Free $ getDeeper v))
      pure
      v'

type Convertible e t f m
  = (Framed e m, MonadDataErrorContext t f m, MonadThunk t m (NValue t f m))

instance ( Convertible e t f m
         , MonadValueF (NValue t f m) m
         , FromValue a m (NValue' t f m (NValue t f m))
         )
  => FromValue a m (NValue t f m) where

  fromValueMay =
    demandF $
      free
        (fromValueMay <=< force)
        fromValueMay

  fromValue =
    demandF $
      free
        (fromValue <=< force)
        fromValue

instance ( Convertible e t f m
         , MonadValueF (NValue t f m) m
         , FromValue a m (Deeper (NValue' t f m (NValue t f m)))
         )
  => FromValue a m (Deeper (NValue t f m)) where

  fromValueMay (Deeper v) =
    demandF
      (free
        ((fromValueMay . Deeper) <=< force)
        (fromValueMay . Deeper)
      )
      v

  fromValue (Deeper v) =
    demandF
      (free
        ((fromValue . Deeper) <=< force)
        (fromValue . Deeper)
      )
      v

instance Convertible e t f m
  => FromValue () m (NValue' t f m (NValue t f m)) where

  fromValueMay =
    pure .
      \case
        NVConstant' NNull -> pure ()
        _                 -> mempty

  fromValue = fromMayToValue TNull

instance Convertible e t f m
  => FromValue Bool m (NValue' t f m (NValue t f m)) where

  fromValueMay =
    pure .
      \case
        NVConstant' (NBool b) -> pure b
        _                     -> Nothing

  fromValue = fromMayToValue TBool

instance Convertible e t f m
  => FromValue Int m (NValue' t f m (NValue t f m)) where

  fromValueMay =
    pure .
      \case
        NVConstant' (NInt b) -> pure (fromInteger b)
        _                    -> Nothing

  fromValue = fromMayToValue TInt

instance Convertible e t f m
  => FromValue Integer m (NValue' t f m (NValue t f m)) where

  fromValueMay =
    pure .
    \case
      NVConstant' (NInt b) -> pure b
      _                    -> Nothing

  fromValue = fromMayToValue TInt

instance Convertible e t f m
  => FromValue Float m (NValue' t f m (NValue t f m)) where

  fromValueMay =
    pure .
      \case
        NVConstant' (NFloat b) -> pure b
        NVConstant' (NInt   i) -> pure (fromInteger i)
        _                      -> Nothing

  fromValue = fromMayToValue TFloat

instance ( Convertible e t f m
         , MonadValueF (NValue t f m) m
         , MonadEffects t f m
         )
  => FromValue NixString m (NValue' t f m (NValue t f m)) where

  fromValueMay =
    \case
      NVStr' ns -> pure $ pure ns
      NVPath' p ->
        fmap
          (pure . (\s -> makeNixStringWithSingletonContext s (StringContext s DirectPath)) . Text.pack . unStorePath)
          (addPath p)
      NVSet' s _ ->
        maybe
          (pure mempty)
          fromValueMay
          (M.lookup "outPath" s)
      _ -> pure mempty

  fromValue = fromMayToValue (TString NoContext)

instance Convertible e t f m
  => FromValue ByteString m (NValue' t f m (NValue t f m)) where

  fromValueMay =
    pure.
      \case
        NVStr' ns -> encodeUtf8 <$> getStringNoContext  ns
        _         -> mempty

  fromValue = fromMayToValue (TString NoContext)

newtype Path = Path { getPath :: FilePath }
    deriving Show

instance ( Convertible e t f m
         , MonadValueF (NValue t f m) m
         )
  => FromValue Path m (NValue' t f m (NValue t f m)) where

  fromValueMay =
    \case
      NVPath' p  -> pure $ pure $ Path p
      NVStr'  ns -> pure $ Path . Text.unpack <$> getStringNoContext  ns
      NVSet' s _ ->
        maybe
          (pure Nothing)
          (fromValueMay @Path)
          (M.lookup "outPath" s)
      _ -> pure Nothing

  fromValue = fromMayToValue TPath

instance Convertible e t f m
  => FromValue [NValue t f m] m (NValue' t f m (NValue t f m)) where

  fromValueMay =
    pure.
      \case
        NVList' l -> pure l
        _         -> mempty

  fromValue = fromMayToValue TList

instance ( Convertible e t f m
         , FromValue a m (NValue t f m)
         )
  => FromValue [a] m (Deeper (NValue' t f m (NValue t f m))) where
  fromValueMay =
    \case
      Deeper (NVList' l) -> sequence <$> traverse fromValueMay l
      _                  -> pure mempty


  fromValue = fromMayToDeeperValue TList

instance Convertible e t f m
  => FromValue (AttrSet (NValue t f m)) m (NValue' t f m (NValue t f m)) where

  fromValueMay =
    pure .
      \case
        NVSet' s _ -> pure s
        _          -> mempty

  fromValue = fromMayToValue TSet

instance ( Convertible e t f m
         , FromValue a m (NValue t f m)
         )
  => FromValue (AttrSet a) m (Deeper (NValue' t f m (NValue t f m))) where

  fromValueMay =
    \case
      Deeper (NVSet' s _) -> sequence <$> traverse fromValueMay s
      _                   -> pure mempty

  fromValue = fromMayToDeeperValue TSet

instance Convertible e t f m
  => FromValue (AttrSet (NValue t f m), AttrSet SourcePos) m
              (NValue' t f m (NValue t f m)) where

  fromValueMay =
    pure .
      \case
        NVSet' s p -> pure (s, p)
        _          -> mempty

  fromValue = fromMayToValue TSet

instance ( Convertible e t f m
         , FromValue a m (NValue t f m)
         )
  => FromValue (AttrSet a, AttrSet SourcePos) m
              (Deeper (NValue' t f m (NValue t f m))) where

  fromValueMay =
    \case
      Deeper (NVSet' s p) -> fmap (, p) . sequence <$> traverse fromValueMay s
      _                   -> pure mempty

  fromValue = fromMayToDeeperValue TSet

-- This instance needs IncoherentInstances, and only because of ToBuiltin
instance ( Convertible e t f m
         , FromValue a m (NValue' t f m (NValue t f m))
         )
  => FromValue a m (Deeper (NValue' t f m (NValue t f m))) where
  fromValueMay = fromValueMay . getDeeper
  fromValue    = fromValue . getDeeper


-- * ToValue

class ToValue a m v where
    toValue :: a -> m v

instance (Convertible e t f m, ToValue a m (NValue' t f m (NValue t f m)))
  => ToValue a m (NValue t f m) where
  toValue = fmap Free . toValue

instance ( Convertible e t f m
         , ToValue a m (Deeper (NValue' t f m (NValue t f m)))
         )
  => ToValue a m (Deeper (NValue t f m)) where
  toValue = fmap (fmap Free) . toValue

instance Convertible e t f m
  => ToValue () m (NValue' t f m (NValue t f m)) where
  toValue _ = pure . nvConstant' $ NNull

instance Convertible e t f m
  => ToValue Bool m (NValue' t f m (NValue t f m)) where
  toValue = pure . nvConstant' . NBool

instance Convertible e t f m
  => ToValue Int m (NValue' t f m (NValue t f m)) where
  toValue = pure . nvConstant' . NInt . toInteger

instance Convertible e t f m
  => ToValue Integer m (NValue' t f m (NValue t f m)) where
  toValue = pure . nvConstant' . NInt

instance Convertible e t f m
  => ToValue Float m (NValue' t f m (NValue t f m)) where
  toValue = pure . nvConstant' . NFloat

instance Convertible e t f m
  => ToValue NixString m (NValue' t f m (NValue t f m)) where
  toValue = pure . nvStr'

instance Convertible e t f m
  => ToValue ByteString m (NValue' t f m (NValue t f m)) where
  toValue = pure . nvStr' . makeNixStringWithoutContext . decodeUtf8

instance Convertible e t f m
  => ToValue Path m (NValue' t f m (NValue t f m)) where
  toValue = pure . nvPath' . getPath

instance Convertible e t f m
  => ToValue StorePath m (NValue' t f m (NValue t f m)) where
  toValue = toValue . Path . unStorePath

instance ( Convertible e t f m
         )
  => ToValue SourcePos m (NValue' t f m (NValue t f m)) where
  toValue (SourcePos f l c) = do
    f' <- toValue (makeNixStringWithoutContext (Text.pack f))
    l' <- toValue (unPos l)
    c' <- toValue (unPos c)
    let pos = M.fromList [("file" :: Text, f'), ("line", l'), ("column", c')]
    pure $ nvSet' pos mempty

-- | With 'ToValue', we can always act recursively
instance Convertible e t f m
  => ToValue [NValue t f m] m (NValue' t f m (NValue t f m)) where
  toValue = pure . nvList'

instance (Convertible e t f m, ToValue a m (NValue t f m))
  => ToValue [a] m (Deeper (NValue' t f m (NValue t f m))) where
  toValue = fmap (Deeper . nvList') . traverse toValue

instance Convertible e t f m
  => ToValue (AttrSet (NValue t f m)) m (NValue' t f m (NValue t f m)) where
  toValue s = pure $ nvSet' s mempty

instance (Convertible e t f m, ToValue a m (NValue t f m))
  => ToValue (AttrSet a) m (Deeper (NValue' t f m (NValue t f m))) where
  toValue s = (Deeper .) . nvSet' <$> traverse toValue s <*> pure mempty

instance Convertible e t f m
  => ToValue (AttrSet (NValue t f m), AttrSet SourcePos) m
            (NValue' t f m (NValue t f m)) where
  toValue (s, p) = pure $ nvSet' s p

instance (Convertible e t f m, ToValue a m (NValue t f m))
  => ToValue (AttrSet a, AttrSet SourcePos) m
            (Deeper (NValue' t f m (NValue t f m))) where
  toValue (s, p) = (Deeper .) . nvSet' <$> traverse toValue s <*> pure p

instance Convertible e t f m
  => ToValue NixLikeContextValue m (NValue' t f m (NValue t f m)) where
  toValue nlcv = do
    path <-
      if nlcvPath nlcv
        then pure <$> toValue True
        else pure Nothing
    allOutputs <-
      if nlcvAllOutputs nlcv
        then pure <$> toValue True
        else pure Nothing
    outputs <- do
      let
        outputs = makeNixStringWithoutContext <$> nlcvOutputs nlcv

      ts :: [NValue t f m] <- traverse toValue outputs
      list
        (pure Nothing)
        (fmap pure . toValue)
        ts
    pure $ flip nvSet' M.empty $ M.fromList $ catMaybes
      [ ("path",) <$> path
      , ("allOutputs",) <$> allOutputs
      , ("outputs",) <$> outputs
      ]

instance Convertible e t f m => ToValue () m (NExprF (NValue t f m)) where
  toValue _ = pure . NConstant $ NNull

instance Convertible e t f m => ToValue Bool m (NExprF (NValue t f m)) where
  toValue = pure . NConstant . NBool
