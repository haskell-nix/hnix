{-# language ConstraintKinds #-}
{-# language ExistentialQuantification #-}
{-# language BangPatterns #-}

-- | Definitions of Frames. Frames are messages that gather and ship themself
-- with a context related to the message. For example - the message about some
-- exception would also gather, keep and bring with it the tracing information.
--
-- == Performance Note
-- callFunc uses 'exceedsDepth' for O(min(2001, depth)) stack overflow detection
-- instead of O(n) 'length' check. For shallow stacks (typical case), this is
-- much faster. For deep stacks approaching the 2000 limit, it's bounded.
module Nix.Frames
  ( NixLevel(..)
  , Frames
  , emptyFrames
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
    { frameLevel :: !NixLevel
    , frame      :: !SomeException
    }

instance Show NixFrame where
  show (NixFrame level f) =
    "Nix frame at level " <> show level <> ": " <> show f

-- | Stack frames - simple list for minimal allocation overhead.
type Frames = [NixFrame]

-- | Empty frames (no stack)
emptyFrames :: Frames
emptyFrames = []

-- | O(min(limit+1, n)) depth check - faster than 'length' for shallow stacks.
-- Returns True if the list has more than @limit@ elements.
-- Used in callFunc for stack overflow detection.
exceedsDepth :: Int -> [a] -> Bool
exceedsDepth !limit = go limit
  where
    go 0 _      = True
    go _ []     = False
    go !n (_:xs) = go (n - 1) xs
{-# INLINE exceedsDepth #-}

askFrames :: forall e m . (MonadReader e m, Has e Frames) => m Frames
askFrames = askLocal

type Framed e m = (MonadReader e m, Has e Frames, MonadThrow m)

newtype NixException = NixException Frames
  deriving Show

instance Exception NixException

withFrame
  :: forall s e m a . (Framed e m, Exception s) => NixLevel -> s -> m a -> m a
withFrame !level !f = local $ over hasLens (NixFrame level (toException f) :)

throwError
  :: forall s e m a . (Framed e m, Exception s, MonadThrow m) => s -> m a
throwError err =
  do
    !frames <- askLocal
    throwM $ NixException $ NixFrame Error (toException err) : frames
