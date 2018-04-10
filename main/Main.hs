{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.DeepSeq
import qualified Control.Exception as Exc
import           Control.Monad
import           Control.Monad.ST
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Nix
import           Nix.Expr.Types.Annotated (stripAnnotation)
import           Nix.Lint
import           Nix.Parser
import           Nix.Pretty
-- import           Nix.TH
import           Options.Applicative hiding (ParserResult(..))
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

data Options = Options
    { verbose      :: Bool
    , debug        :: Bool
    , evaluate     :: Bool
    , check        :: Bool
    , parseOnly    :: Bool
    , ignoreErrors :: Bool
    , expression   :: Maybe Text
    , fromFile     :: Maybe FilePath
    , filePaths    :: [FilePath]
    }

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
    <*> switch
        (   long "parse-only"
         <> help "Whether to parse only, no pretty printing or checking")
    <*> switch
        (   long "ignore-errors"
         <> help "Continue parsing files, even if there are errors")
    <*> optional (strOption
        (   short 'e'
         <> long "expr"
         <> help "Expression to parse or evaluate"))
    <*> optional (strOption
        (   short 'f'
         <> long "file"
         <> help "Parse all of the files given in FILE; - means stdin"))
    <*> many (strArgument (metavar "FILE" <> help "Path of file to parse"))

main :: IO ()
main = do
    opts <- execParser optsDef
    case expression opts of
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

        Success expr -> do
            -- expr <- Exc.evaluate $ force expr
            -- putStrLn "Parsing file...done"

            when (check opts) $
                putStrLn $ runST $ Nix.runLintM . renderSymbolic
                    =<< Nix.lint (stripAnnotation expr)

            if | evaluate opts, debug opts ->
                     print =<< Nix.tracingEvalLoc mpath expr
               | evaluate opts ->
                     putStrLn . printNix =<< Nix.evalLoc mpath expr
               | debug opts ->
                     print $ stripAnnotation expr
               | parseOnly opts ->
                     void $ Exc.evaluate $ force expr
               | otherwise ->
                     displayIO stdout
                         . renderPretty 0.4 80
                         . prettyNix
                         . stripAnnotation $ expr
