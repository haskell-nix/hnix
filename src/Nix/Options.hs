module Nix.Options where

import           Control.Arrow (second)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Options.Applicative hiding (ParserResult(..))
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

data Options = Options
    { verbose      :: Bool
    , debug        :: Bool
    , parse        :: Bool
    , parseOnly    :: Bool
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
    , filePaths    :: [FilePath]
    }
    deriving Show

argPair :: Mod OptionFields (Text, Text) -> Parser (Text, Text)
argPair = option $ str >>= \s ->
    case Text.findIndex (== '=') s of
        Nothing -> errorWithoutStackTrace
            "Format of --arg/--argstr in hnix is: name=expr"
        Just i -> return $ second Text.tail $ Text.splitAt i s

nixOptions :: Parser Options
nixOptions = Options
    <$> switch
        (   short 'v'
         <> long "verbose"
         <> help "Verbose output")
    <*> switch
        (   short 'd'
         <> long "debug"
         <> help "Debug output")
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
    <*> many (strArgument (metavar "FILE" <> help "Path of file to parse"))

nixOptionsInfo :: ParserInfo Options
nixOptionsInfo = info (helper <*> nixOptions)
                      (fullDesc <> progDesc "" <> header "hnix")
