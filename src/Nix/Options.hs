module Nix.Options where

import           Data.Text (Text)

data Options = Options
    { verbose      :: Verbosity
    , tracing      :: Bool
    , thunks       :: Bool
    , values       :: Bool
    , reduce       :: Maybe FilePath
    , reduceSets   :: Bool
    , reduceLists  :: Bool
    , parse        :: Bool
    , parseOnly    :: Bool
    , findFile     :: Maybe FilePath
    , strict       :: Bool
    , normalize    :: Bool
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
    , filePaths    :: [FilePath]
    }
    deriving Show

defaultOptions :: Options
defaultOptions = Options
    { verbose      = ErrorsOnly
    , tracing      = False
    , thunks       = False
    , values       = False
    , reduce       = Nothing
    , reduceSets   = False
    , reduceLists  = False
    , parse        = False
    , parseOnly    = False
    , findFile     = Nothing
    , strict       = False
    , normalize    = False
    , evaluate     = False
    , json         = False
    , xml          = False
    , attr         = Nothing
    , include      = []
    , check        = False
    , readFrom     = Nothing
    , cache        = False
    , repl         = False
    , ignoreErrors = False
    , expression   = Nothing
    , arg          = []
    , argstr       = []
    , fromFile     = Nothing
    , filePaths    = []
    }

data Verbosity
    = ErrorsOnly
    | Informational
    | Talkative
    | Chatty
    | DebugInfo
    | Vomit
    deriving (Eq, Ord, Enum, Bounded, Show)

