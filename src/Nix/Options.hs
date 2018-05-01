module Nix.Options where

import           Control.Arrow (second)
import           Data.Aeson (decode)
import           Data.Char (isDigit)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock
import           Options.Applicative hiding (ParserResult(..))
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

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
    , currentTime  :: Maybe UTCTime
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
    , currentTime  = Nothing
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

decodeVerbosity :: Int -> Verbosity
decodeVerbosity 0 = ErrorsOnly
decodeVerbosity 1 = Informational
decodeVerbosity 2 = Talkative
decodeVerbosity 3 = Chatty
decodeVerbosity 4 = DebugInfo
decodeVerbosity _ = Vomit

argPair :: Mod OptionFields (Text, Text) -> Parser (Text, Text)
argPair = option $ str >>= \s ->
    case Text.findIndex (== '=') s of
        Nothing -> errorWithoutStackTrace
            "Format of --arg/--argstr in hnix is: name=expr"
        Just i -> return $ second Text.tail $ Text.splitAt i s

nixOptions :: Parser Options
nixOptions = Options
    <$> (fromMaybe ErrorsOnly <$>
         optional
           (option (do a <- str
                       if all isDigit a
                       then pure $ decodeVerbosity (read a)
                       else fail "Argument to -v/--verbose must be a number")
            (   short 'v'
             <> long "verbose"
             <> help "Verbose output")))
    <*> switch
        (   long "trace"
         <> help "Enable tracing code (even more can be seen if built with --flags=tracing)")
    <*> switch
        (   long "thunks"
         <> help "Enable reporting of thunk tracing as well as regular evaluation")
    <*> switch
        (   long "values"
         <> help "Enable reporting of value provenance in error messages")
    <*> optional (strOption
        (   long "reduce"
         <> help "When done evaluating, output the evaluated part of the expression to FILE"))
    <*> switch
        (   long "reduce-sets"
         <> help "Reduce set members that aren't used; breaks if hasAttr is used")
    <*> switch
        (   long "reduce-lists"
         <> help "Reduce list members that aren't used; breaks if elemAt is used")
    <*> switch
        (   long "parse"
         <> help "Whether to parse the file (also the default right now)")
    <*> switch
        (   long "parse-only"
         <> help "Whether to parse only, no pretty printing or checking")
    <*> optional (strOption
        (   long "find-file"
         <> help "Look up the given files in Nix's search path"))
    <*> switch
        (   long "strict"
         <> help "When used with --eval, recursively evaluate list elements and attributes")
    <*> switch
        (   long "force"
         <> help "Whether to force the results of evaluation to normal form")
    <*> switch
        (   long "eval"
         <> help "Whether to evaluate, or just pretty-print")
    <*> switch
        (   long "json"
         <> help "Print the resulting value as an JSON representation")
    <*> switch
        (   long "xml"
         <> help "Print the resulting value as an XML representation")
    <*> optional (strOption
        (   short 'A'
         <> long "attr"
         <> help "Select an attribute from the top-level Nix expression being evaluated"))
    <*> many (strOption
        (   short 'I'
         <> long "include"
         <> help "Add a path to the Nix expression search path"))
    <*> switch
        (   long "check"
         <> help "Whether to check for syntax errors after parsing")
    <*> optional (strOption
        (   long "read"
         <> help "Read in an expression tree from a binary cache"))
    <*> switch
        (   long "cache"
         <> help "Write out the parsed expression tree to a binary cache")
    <*> switch
        (   long "repl"
         <> help "After performing any indicated actions, enter the REPL")
    <*> switch
        (   long "ignore-errors"
         <> help "Continue parsing files, even if there are errors")
    <*> optional (strOption
        (   short 'E'
         <> long "expr"
         <> help "Expression to parse or evaluate"))
    <*> many (argPair
        (   long "arg"
         <> help "Argument to pass to an evaluated lambda"))
    <*> many (argPair
        (   long "argstr"
         <> help "Argument string to pass to an evaluated lambda"))
    <*> optional (strOption
        (   short 'f'
         <> long "file"
         <> help "Parse all of the files given in FILE; - means stdin"))
    <*> (fmap (decode =<<) $ optional $ strOption
        (   long "setTime"
         <> help "Set current time for testing purposes"))
    <*> many (strArgument (metavar "FILE" <> help "Path of file to parse"))

nixOptionsInfo :: ParserInfo Options
nixOptionsInfo = info (helper <*> nixOptions)
                      (fullDesc <> progDesc "" <> header "hnix")
