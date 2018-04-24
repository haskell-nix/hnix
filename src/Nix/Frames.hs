{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Frames (NixLevel(..), Frames, Framed, Frame(..), NixFrame(..),
                   NixException(..), SomeFrame(..), withFrame, throwError,
                   module Data.Typeable,
                   module Control.Exception) where

import Control.Exception hiding (catch, evaluate)
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Typeable hiding (typeOf)
import Nix.Utils
import Text.PrettyPrint.ANSI.Leijen (Doc)

data NixLevel = Fatal | Error | Warning | Info | Debug
    deriving (Ord, Eq, Bounded, Enum, Show)

data SomeFrame = forall e. Frame e => SomeFrame e

instance Show SomeFrame where
    show (SomeFrame f) = show f

class (Typeable e, Show e) => Frame e where
    toFrame   :: e -> SomeFrame
    fromFrame :: SomeFrame -> Maybe e

    toFrame = SomeFrame
    fromFrame (SomeFrame e) = cast e

-- jww (2018-04-24): These two are temporary instance for now.
instance Frame [Char]
instance Frame Doc

data NixFrame = NixFrame
    { frameLevel :: NixLevel
    , frame      :: SomeFrame
    }

instance Show NixFrame where
    show (NixFrame level f) =
        "Nix frame at level " ++ show level ++ ": "++ show f

type Frames = [NixFrame]

type Framed e m = (MonadReader e m, Has e Frames, MonadThrow m)

newtype NixException = NixException Frames
    deriving Show

instance Exception NixException

withFrame :: forall s e m a. (Framed e m, Frame s) => NixLevel -> s -> m a -> m a
withFrame level f = local (over hasLens (NixFrame level (toFrame f) :))

throwError :: forall s e m a. (Framed e m, Frame s, MonadThrow m) => s -> m a
throwError err = do
    context <- asks (view hasLens)
    traceM "Throwing error..."
    throwM $ NixException (NixFrame Error (toFrame err):context)
