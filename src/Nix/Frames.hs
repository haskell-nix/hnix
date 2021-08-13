{-# language ConstraintKinds #-}
{-# language ExistentialQuantification #-}

-- | Definitions of Frames. Frames are messages that gather and ship themself with a context related to the message. For example - the message about some exception would also gather, keep and bring with it the tracing information.
module Nix.Frames
  ( NixLevel(..)
  , Frames
  , Framed
  , NixFrame(..)
  , NixException(..)
  , withFrame
  , throwError
  , module Data.Typeable
  )
where

import           Data.Typeable           hiding ( typeOf )
import           Control.Monad.Catch            ( MonadThrow(..) )
import qualified Text.Show

data NixLevel = Fatal | Error | Warning | Info | Debug
  deriving (Ord, Eq, Bounded, Enum, Show)

data NixFrame = NixFrame
  { frameLevel :: NixLevel
  , frame      :: SomeException
  }

instance Show NixFrame where
  show (NixFrame level f) =
    "Nix frame at level " <> show level <> ": " <> show f

type Frames = [NixFrame]

type Framed e m = (MonadReader e m, Has e Frames, MonadThrow m)

newtype NixException = NixException Frames
  deriving Show

instance Exception NixException

withFrame
  :: forall s e m a . (Framed e m, Exception s) => NixLevel -> s -> m a -> m a
withFrame level f = local $ over hasLens (NixFrame level (toException f) :)

throwError
  :: forall s e m a . (Framed e m, Exception s, MonadThrow m) => s -> m a
throwError err = do
  context <- asks (view hasLens)
  traceM "Throwing fail..."
  throwM $ NixException $ NixFrame Error (toException err) : context
