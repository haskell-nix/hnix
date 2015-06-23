{-# LANGUAGE LambdaCase #-}
module Main where

import Nix.Parser
import Nix.Pretty

import System.Environment
import System.IO
import Text.PrettyPrint.ANSI.Leijen

nix :: FilePath -> IO ()
nix path = do
  res <- parseNixFile path
  case res of
    Failure e -> hPutStrLn stderr $ "Parse failed: " ++ show e
    Success n -> do
      displayIO stdout $ renderPretty 0.4 80 (prettyNix n)

main :: IO ()
main = do
  let usageStr = "Parses a nix file and prints to stdout.\n\
                 \\n\
                 \Usage:\n\
                 \  hnix --help\n\
                 \  hnix <path>\n"
  let argErr msg = error $ "Invalid arguments: " ++ msg ++ "\n" ++ usageStr
  getArgs >>= \case
    "--help":_ -> putStrLn usageStr
    ('-':_):_ -> argErr "Provide a path to a nix file."
    path:_ -> nix path
    _ -> argErr "Provide a path to a nix file."
