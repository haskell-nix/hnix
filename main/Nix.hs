module Main where

import Nix.Parser
import Nix.Pretty
import Nix.Eval
import Nix.Types

import Text.PrettyPrint.ANSI.Leijen
import System.Environment
import System.IO

import qualified Data.Map as Map

nix :: FilePath -> IO ()
nix path = do
  res <- parseNixFile path
  case res of
    Failure e -> hPutStrLn stderr $ "Parse failed: " ++ show e
    Success n -> do
      displayIO stdout $ renderPretty 0.4 80 (prettyNix n)
      putStrLn ""
      evalExpr n (Fix $ NVSet Map.empty) >>= print

main :: IO ()
main = do
    [path] <- getArgs
    nix path
