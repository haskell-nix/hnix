{-# LANGUAGE LambdaCase #-}
module Main where

import Nix.Parser
import Nix.Pretty
import Nix.Expr

import System.Environment
import System.IO
import Text.PrettyPrint.ANSI.Leijen

nix :: FilePath -> IO ()
nix path = parseNixFile path >>= displayNExpr

nixString :: String -> IO ()
nixString = displayNExpr . parseNixString

displayNExpr :: Result NExpr -> IO ()
displayNExpr = \case
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
