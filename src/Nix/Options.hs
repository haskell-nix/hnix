{-# language StrictData #-}

-- | Definitions & defaults for the CLI options
module Nix.Options where

import           Nix.Prelude
import           Data.Time

--  2021-07-15: NOTE: What these are? They need to be documented.
-- Also need better names. Foe example, Maybes & lists names need to show their type in the name.
data Options =
  Options
    { getVerbosity   :: Verbosity
    , isTrace        :: Bool
    , isThunks       :: Bool
    , isValues       :: Bool
    , isShowScopes   :: Bool
    , getReduce      :: Maybe Path
    , isReduceSets   :: Bool
    , isReduceLists  :: Bool
    , isParse        :: Bool
    , isParseOnly    :: Bool
    , isFinder       :: Bool
    , getFindFile    :: Maybe Path
    , isStrict       :: Bool
    , isEvaluate     :: Bool
    , isJson         :: Bool
    , isXml          :: Bool
    , getAttr        :: Maybe Text
    , getInclude     :: [Path]
    , isCheck        :: Bool
    , getReadFrom    :: Maybe Path
    , isCache        :: Bool
    , isRepl         :: Bool
    , isIgnoreErrors :: Bool
    , getExpression  :: Maybe Text
    , getArg         :: [(Text, Text)]
    , getArgstr      :: [(Text, Text)]
    , getFromFile    :: Maybe Path
    , getTime        :: UTCTime
    -- ^ The time can be set to reproduce time-dependent states.
    , getFilePaths   :: [Path]
    }
    deriving Show

defaultOptions :: UTCTime -> Options
defaultOptions currentTime =
  Options
    { getVerbosity   = ErrorsOnly
    , isTrace        = False
    , isThunks       = False
    , isValues       = False
    , isShowScopes   = False
    , getReduce      = mempty
    , isReduceSets   = False
    , isReduceLists  = False
    , isParse        = False
    , isParseOnly    = False
    , isFinder       = False
    , getFindFile    = mempty
    , isStrict       = False
    , isEvaluate     = False
    , isJson         = False
    , isXml          = False
    , getAttr        = mempty
    , getInclude     = mempty
    , isCheck        = False
    , getReadFrom    = mempty
    , isCache        = False
    , isRepl         = False
    , isIgnoreErrors = False
    , getExpression  = mempty
    , getArg         = mempty
    , getArgstr      = mempty
    , getFromFile    = mempty
    , getTime        = currentTime
    , getFilePaths   = mempty
    }

data Verbosity
    = ErrorsOnly
    | Informational
    | Talkative
    | Chatty
    | DebugInfo
    | Vomit
    deriving (Eq, Ord, Enum, Bounded, Show)

askOptions :: forall e m . (MonadReader e m, Has e Options) => m Options
askOptions = askLocal
