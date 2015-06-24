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

nixString :: String -> IO ()
nixString s = case parseNixString s of
  Success n -> displayIO stdout $ renderPretty 0.4 80 (prettyNix n)
  Failure e -> hPutStrLn stderr $ "Parse failed: " ++ show e

main :: IO ()
main = do
  let usageStr = "Parses a nix file and prints to stdout.\n\
                 \\n\
                 \Usage:\n\
                 \  hnix --help\n\
                 \  hnix <path>\n\
                 \  hnix --expr <expr>\n"
  let argErr msg = error $ "Invalid arguments: " ++ msg ++ "\n" ++ usageStr
  getArgs >>= \case
    "--help":_ -> putStrLn usageStr
    "--expr":expr:_ -> nixString expr
    "--expr":_ -> argErr "Provide an expression."
    ('-':_):_ -> argErr "Provide a path to a nix file."
    path:_ -> nix path
    _ -> argErr "Provide a path to a nix file."
