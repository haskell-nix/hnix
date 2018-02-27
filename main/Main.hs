{-# LANGUAGE LambdaCase #-}
module Main where

import Nix.Parser
import Nix.Pretty
import Nix.Expr
import Nix.Eval (evalExpr)
import Nix.Builtins (baseEnv)

import System.Environment
import System.IO
import Text.PrettyPrint.ANSI.Leijen

nix :: FilePath -> IO ()
nix path = parseNixFile path >>= displayAndEval

nixString :: String -> IO ()
nixString = displayAndEval . parseNixString

displayAndEval :: Result NExpr -> IO ()
displayAndEval = \case
  Failure e -> hPutStrLn stderr $ "Parse failed: " ++ show e
  Success expr -> do
    display (prettyNix expr)
    putStrLn ""
    value <- evalExpr expr baseEnv
    display (prettyNixValue value)

display :: Doc -> IO ()
display = displayIO stdout . renderPretty 0.4 80

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
