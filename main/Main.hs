{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Nix.Builtins
import Nix.Parser
import Nix.Pretty
import Nix.Lint
import Options.Applicative hiding (ParserResult(..))
import System.FilePath
import System.IO
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

data Options = Options
    { verbose    :: Bool
    , debug      :: Bool
    , evaluate   :: Bool
    , check      :: Bool
    , filePath   :: Maybe FilePath
    , expression :: Maybe String
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
        (   short 'f'
         <> long "file"
         <> help "File to parse or evaluate"))
    <*> optional (strOption
        (   short 'e'
         <> long "expr"
         <> help "Expression to parse or evaluate"))

main :: IO ()
main = do
    opts <- execParser optsDef
    (eres, mdir) <- case expression opts of
        Just s -> return (parseNixString s, Nothing)
        Nothing  -> case filePath opts of
            Nothing   -> (, Nothing) . parseNixString <$> getContents
            Just "-"  -> (, Nothing) . parseNixString <$> getContents
            Just path -> (, Just (takeDirectory path)) <$> parseNixFile path

    case eres of
        Failure err -> hPutStrLn stderr $ "Parse failed: " ++ show err
        Success expr -> do
            when (check opts) $ lintExpr expr
            if | evaluate opts, debug opts ->
                     print =<< tracingEvalTopLevelExprIO mdir expr
               | evaluate opts ->
                     putStrLn . printNix =<< evalTopLevelExprIO mdir expr
               | debug opts ->
                     print expr
               | otherwise ->
                     displayIO stdout $ renderPretty 0.4 80 (prettyNix expr)
  where
    optsDef :: ParserInfo Options
    optsDef = info (helper <*> mainOptions)
                   (fullDesc <> progDesc "" <> header "hnix")
