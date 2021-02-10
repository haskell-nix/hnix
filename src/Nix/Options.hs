-- | Definitions & defaults for the CLI options
module Nix.Options where

import           Data.Text                      ( Text )
import           Data.Time

data Options = Options
    { verbose      :: Verbosity
    , tracing      :: Bool
    , thunks       :: Bool
    , values       :: Bool
    , showScopes   :: Bool
    , reduce       :: Maybe FilePath
    , reduceSets   :: Bool
    , reduceLists  :: Bool
    , parse        :: Bool
    , parseOnly    :: Bool
    , finder       :: Bool
    , findFile     :: Maybe FilePath
    , strict       :: Bool
    , evaluate     :: Bool
    , json         :: Bool
    , xml          :: Bool
    , attr         :: Maybe Text
    , include      :: [FilePath]
    , check        :: Bool
    , readFrom     :: Maybe FilePath
    , cache        :: Bool
    , repl         :: Bool
    , ignoreErrors :: Bool
    , expression   :: Maybe Text
    , arg          :: [(Text, Text)]
    , argstr       :: [(Text, Text)]
    , fromFile     :: Maybe FilePath
    , currentTime  :: UTCTime
    , filePaths    :: [FilePath]
    }
    deriving Show

defaultOptions :: UTCTime -> Options
defaultOptions current = Options { verbose      = ErrorsOnly
                                 , tracing      = False
                                 , thunks       = False
                                 , values       = False
                                 , showScopes   = False
                                 , reduce       = Nothing
                                 , reduceSets   = False
                                 , reduceLists  = False
                                 , parse        = False
                                 , parseOnly    = False
                                 , finder       = False
                                 , findFile     = Nothing
                                 , strict       = False
                                 , evaluate     = False
                                 , json         = False
                                 , xml          = False
                                 , attr         = Nothing
                                 , include      = mempty
                                 , check        = False
                                 , readFrom     = Nothing
                                 , cache        = False
                                 , repl         = False
                                 , ignoreErrors = False
                                 , expression   = Nothing
                                 , arg          = mempty
                                 , argstr       = mempty
                                 , fromFile     = Nothing
                                 , currentTime  = current
                                 , filePaths    = mempty
                                 }

data Verbosity
    = ErrorsOnly
    | Informational
    | Talkative
    | Chatty
    | DebugInfo
    | Vomit
    deriving (Eq, Ord, Enum, Bounded, Show)
