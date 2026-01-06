{-# language ConstraintKinds #-}
{-# language ExistentialQuantification #-}
{-# language Strict #-}

-- | Definitions of Frames. Frames are messages that gather and ship themself
-- with a context related to the message. For example - the message about some
-- exception would also gather, keep and bring with it the tracing information.
--
-- == Performance Note
-- The 'Frames' type tracks depth with an unboxed counter for O(1) depth checks.
-- This is critical for 'callFunc' which checks stack depth on every function call.
-- The list of frames is only traversed on errors or when debugging options are enabled.
module Nix.Frames
  ( NixLevel(..)
  , Frames
  , emptyFrames
  , pushFrame
  , framesToList
  , exceedsDepth
  , askFrames
  , Framed
  , NixFrame(..)
  , NixException(..)
  , withFrame
  , throwError
  , module Data.Typeable
  )
where

import           Nix.Prelude
import           Data.Typeable           hiding ( typeOf )
import           Control.Monad.Catch            ( MonadThrow(..) )
import qualified Text.Show

data NixLevel = Fatal | Error | Warning | Info | Debug
  deriving (Ord, Eq, Bounded, Enum, Show)

data NixFrame =
  NixFrame
    { frameLevel :: NixLevel
    , frame      :: SomeException
    }

instance Show NixFrame where
  show (NixFrame level f) =
    "Nix frame at level " <> show level <> ": " <> show f

-- | Stack frames with O(1) depth tracking.
-- The depth counter is kept in sync with the list length via 'pushFrame'.
-- Use 'framesToList' to access the underlying list for traversal.
data Frames = Frames
  { framesDepth :: {-# UNPACK #-} !Int  -- ^ Current stack depth (inline, no boxing)
  , framesList  :: [NixFrame]           -- ^ The actual frame stack
  }

instance Show Frames where
  show (Frames depth frames) =
    "Frames (depth=" <> show depth <> "): " <> show frames

instance Semigroup Frames where
  Frames d1 l1 <> Frames d2 l2 = Frames (d1 + d2) (l1 <> l2)

instance Monoid Frames where
  mempty = emptyFrames

-- | Empty frames (no stack). O(1).
emptyFrames :: Frames
emptyFrames = Frames 0 []

-- | Push a frame onto the stack. O(1).
pushFrame :: NixFrame -> Frames -> Frames
pushFrame f (Frames depth frames) = Frames (depth + 1) (f : frames)
{-# INLINABLE pushFrame #-}

-- | Convert to list for traversal operations. O(1).
framesToList :: Frames -> [NixFrame]
framesToList = framesList
{-# INLINABLE framesToList #-}

-- | O(1) depth check. Returns True if frames exceed the limit.
-- Used in callFunc for stack overflow detection.
exceedsDepth :: Int -> Frames -> Bool
exceedsDepth limit (Frames depth _) = depth > limit
{-# INLINABLE exceedsDepth #-}

askFrames :: forall e m . (MonadReader e m, Has e Frames) => m Frames
askFrames = askLocal

type Framed e m = (MonadReader e m, Has e Frames, MonadThrow m)

newtype NixException = NixException Frames
  deriving Show

instance Exception NixException

withFrame
  :: forall s e m a . (Framed e m, Exception s) => NixLevel -> s -> m a -> m a
withFrame level f = local $ over hasLens (pushFrame (NixFrame level (toException f)))

throwError
  :: forall s e m a . (Framed e m, Exception s, MonadThrow m) => s -> m a
throwError err =
  do
    frames <- askLocal
    throwM $ NixException $ pushFrame (NixFrame Error (toException err)) frames
