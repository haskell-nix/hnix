{-# LANGUAGE MultiWayIf #-}

module Main where

import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.Map as Map
import           Nix.Builtins
import           Nix.Eval
import           Nix.Parser
import           Nix.Pretty
import           Options.Applicative hiding (ParserResult(..))
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

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
    eres <- case expression opts of
        Just s -> return $ parseNixString s
        Nothing  -> case filePath opts of
            Nothing   -> parseNixString <$> getContents
            Just "-"  -> parseNixString <$> getContents
            Just path -> parseNixFile path

    case eres of
        Failure err -> hPutStrLn stderr $ "Parse failed: " ++ show err
        Success expr -> do
            base <- run baseEnv Map.empty
            when (check opts) $
                run (checkExpr expr) base
            if | evaluate opts, debug opts -> do
                     expr' <- tracingExprEval expr
                     thnk  <- run expr' base
                     val   <- run (normalForm thnk) base
                     print val
               | evaluate opts ->
                     putStrLn . printNix =<< evalTopLevelExprIO expr
               | debug opts ->
                     print expr
               | otherwise ->
                     displayIO stdout $ renderPretty 0.4 80 (prettyNix expr)
  where
    run expr = evalStateT (runCyclic expr)

    optsDef :: ParserInfo Options
    optsDef = info (helper <*> mainOptions)
                   (fullDesc <> progDesc "" <> header "hnix")
