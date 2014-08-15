module Main where

import Data.Map as Map
import Nix.Eval
import Nix.Parser
import Nix.Pretty
import Nix.Types
import Text.PrettyPrint.ANSI.Leijen
import System.Environment
import System.IO

nix :: FilePath -> IO ()
nix path = do
    res <- parseNixFile path
    case res of
        Failure e -> error $ "Parse failed: " ++ show e
        Success n -> do
            displayIO stdout $ renderPretty 0.4 80 (prettyNix n)
            top <- evalExpr n (Fix (NVSet Map.empty)) -- evaluate top level
            putStrLn ""
            print top

main :: IO ()
main = do
    [path] <- getArgs
    nix path
