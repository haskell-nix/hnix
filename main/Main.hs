{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Arrow (second)
import           Control.DeepSeq
import qualified Control.Exception as Exc
import           Control.Monad
import           Control.Monad.ST
import           Data.Fix
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Nix
import           Nix.Cache
import           Nix.Eval
import           Nix.Exec (Lazy, runLazyM, callFunc)
import           Nix.Expr
import           Nix.Lint
import           Nix.Normal
import           Nix.Parser
import           Nix.Pretty
import           Nix.Stack (NixException(..))
import qualified Nix.Value as V
-- import           Nix.TH
import           Options.Applicative hiding (ParserResult(..))
import           System.IO
import           System.FilePath
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

data Options = Options
    { verbose      :: Bool
    , debug        :: Bool
    , evaluate     :: Bool
    , check        :: Bool
    , readFrom     :: Maybe FilePath
    , cache        :: Bool
    , parse        :: Bool
    , parseOnly    :: Bool
    , ignoreErrors :: Bool
    , expression   :: Maybe Text
    , arg          :: [(Text, Text)]
    , argstr       :: [(Text, Text)]
    , fromFile     :: Maybe FilePath
    , filePaths    :: [FilePath]
    }

argPair :: Mod OptionFields (Text, Text) -> Parser (Text, Text)
argPair = option $ str >>= \s ->
    case Text.findIndex (== '=') s of
        Nothing -> errorWithoutStackTrace
            "Format of --arg/--argstr in hnix is: name=expr"
        Just i -> return $ second Text.tail $ Text.splitAt i s

mainOptions :: Parser Options
mainOptions = Options
    <$> switch
        (   short 'v'
         <> long "verbose"
         <> help "Verbose output")
    <*> switch
        (   short 'd'
         <> long "debug"
         <> help "Debug output")
    <*> switch
        (   long "eval"
         <> help "Whether to evaluate, or just pretty-print")
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
        (   long "parse"
         <> help "Whether to parse the file (also the default right now)")
    <*> switch
        (   long "parse-only"
         <> help "Whether to parse only, no pretty printing or checking")
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

main :: IO ()
main = do
    opts <- execParser optsDef
    case readFrom opts of
        Just path -> do
            let file = addExtension (dropExtension path) "nix"
            process opts (Just file) =<< readCache path
        Nothing -> case expression opts of
            Just s -> handleResult opts Nothing (parseNixTextLoc s)
            Nothing  -> case fromFile opts of
                Just "-" ->
                    mapM_ (processFile opts) =<< (lines <$> getContents)
                Just path ->
                    mapM_ (processFile opts) =<< (lines <$> readFile path)
                Nothing -> case filePaths opts of
                    [] ->
                        handleResult opts Nothing . parseNixTextLoc
                            =<< Text.getContents
                    ["-"] ->
                        handleResult opts Nothing . parseNixTextLoc
                            =<< Text.getContents
                    paths ->
                        mapM_ (processFile opts) paths
  where
    optsDef :: ParserInfo Options
    optsDef = info (helper <*> mainOptions)
                   (fullDesc <> progDesc "" <> header "hnix")

    processFile opts path = do
        -- putStrLn "Parsing file..."
        eres <- parseNixFileLoc path
        handleResult opts (Just path) eres

    -- print . printNix =<< Nix.eval [nix|1 + 3|]

    handleResult opts mpath = \case
        Failure err ->
            (if ignoreErrors opts
             then hPutStrLn stderr
             else errorWithoutStackTrace) $ "Parse failed: " ++ show err

        Success expr -> Exc.catch (process opts mpath expr) $ \case
            NixEvalException msg -> errorWithoutStackTrace msg

    process opts mpath expr = do
        -- expr <- Exc.evaluate $ force expr
        -- putStrLn "Parsing file...done"

        when (check opts) $
            putStrLn $ runST $ Nix.runLintM . renderSymbolic
                =<< Nix.lint expr

        let parseArg s = case parseNixText s of
                Success x -> x
                Failure err -> errorWithoutStackTrace (show err)

        args <- traverse (traverse (Nix.eval Nothing)) $
            map (second parseArg) (arg opts) ++
            map (second mkStr) (argstr opts)

        let argmap :: Lazy IO (V.NValue (Lazy IO))
            argmap = embed $ Fix $ V.NVSet (M.fromList args) mempty

            compute ev x p = do
                 f <- ev mpath x
                 p =<< case f of
                     Fix (V.NVClosure _ g) ->
                         runLazyM $ normalForm =<< g argmap
                     _ -> pure f

        if | evaluate opts, debug opts ->
                 compute Nix.tracingEvalLoc expr print
           | evaluate opts, not (null args) ->
                 compute Nix.evalLoc expr (putStrLn . printNix)
           | evaluate opts ->
                 putStrLn . printNix =<< Nix.evalLoc mpath expr
           | debug opts ->
                 print $ stripAnnotation expr
           | cache opts, Just path <- mpath -> do
                let file = addExtension (dropExtension path) "nixc"
                writeCache file expr
           | parseOnly opts ->
                 void $ Exc.evaluate $ force expr
           | otherwise ->
                 displayIO stdout
                     . renderPretty 0.4 80
                     . prettyNix
                     . stripAnnotation $ expr
