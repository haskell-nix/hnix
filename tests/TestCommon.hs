module TestCommon where

import Nix
import Nix.Monad.Lazy
import Nix.Parser
import Nix.Pretty
import Nix.Value
import System.Environment
import System.IO
import System.Posix.Files
import System.Posix.Temp
import System.Process
import Test.Tasty.HUnit

hnixEvalFile :: FilePath -> IO (NValueNF (Lazy IO))
hnixEvalFile file =  do
  parseResult <- parseNixFileLoc file
  case parseResult of
    Failure err        ->
        error $ "Parsing failed for file `" ++ file ++ "`.\n" ++ show err
    Success expression -> do
        setEnv "TEST_VAR" "foo"
        evalLoc (Just file) expression

hnixEvalString :: String -> IO (NValueNF (Lazy IO))
hnixEvalString expr = case parseNixString expr of
    Failure err        ->
        error $ "Parsing failed for expressien `" ++ expr ++ "`.\n" ++ show err
    Success expression -> eval Nothing expression

nixEvalString :: String -> IO String
nixEvalString expr = do
  (fp,h) <- mkstemp "nix-test-eval"
  hPutStr h expr
  hClose h
  res <- nixEvalFile fp
  removeLink fp
  return res

nixEvalFile :: FilePath -> IO String
nixEvalFile fp = readProcess "nix-instantiate" ["--eval", fp] ""

assertEvalFileMatchesNix :: FilePath -> Assertion
assertEvalFileMatchesNix fp = do
  hnixVal <- (++"\n") . printNix <$> hnixEvalFile fp
  nixVal <- nixEvalFile fp
  assertEqual fp nixVal hnixVal

assertEvalMatchesNix :: String -> Assertion
assertEvalMatchesNix expr = do
  hnixVal <- (++"\n") . printNix <$> hnixEvalString expr
  nixVal <- nixEvalString expr
  assertEqual expr nixVal hnixVal
