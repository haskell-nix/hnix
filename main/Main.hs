{-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Monad
import           Control.Monad.ST
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
    { verbose    :: Bool
    , debug      :: Bool
    , evaluate   :: Bool
    , check      :: Bool
    , expression :: Maybe String
    , filePaths  :: [FilePath]
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
    <*> optional (strOption
        (   short 'e'
         <> long "expr"
         <> help "Expression to parse or evaluate"))
    <*> many (strArgument (metavar "FILE" <> help "Path of file to parse"))

main :: IO ()
main = do
    opts <- execParser optsDef
    (eres, mpath) <- case expression opts of
        Just s -> return (parseNixStringLoc s, Nothing)
        Nothing  -> case filePaths opts of
            []     -> (, Nothing) . parseNixStringLoc <$> getContents
            ["-"]  -> (, Nothing) . parseNixStringLoc <$> getContents
            [path] -> (, Just path) <$> parseNixFileLoc path
            _      -> error "hnix doesn't support multiple path arguments yet"

    -- print . printNix =<< Nix.eval [nix|1 + 3|]

    case eres of
        Failure err -> hPutStrLn stderr $ "Parse failed: " ++ show err
        Success expr -> do
            when (check opts) $
                putStrLn $ runST $ Nix.runLintM . renderSymbolic
                    =<< Nix.lint (stripAnnotation expr)

            if | evaluate opts, debug opts ->
                     print =<< Nix.tracingEvalLoc mpath expr
               | evaluate opts ->
                     putStrLn . printNix =<< Nix.evalLoc mpath expr
               | debug opts ->
                     print expr
               | otherwise ->
                     displayIO stdout
                         . renderPretty 0.4 80
                         . prettyNix
                         . stripAnnotation $ expr
  where
    optsDef :: ParserInfo Options
    optsDef = info (helper <*> mainOptions)
                   (fullDesc <> progDesc "" <> header "hnix")
