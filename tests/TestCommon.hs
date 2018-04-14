module TestCommon where

import Data.Text (Text, unpack)
import Nix
import Nix.Exec
import Nix.Normal
import Nix.Parser
import Nix.Pretty
import Nix.Value
import System.Environment
import System.IO
import System.Posix.Files
import System.Posix.Temp
import System.Process
import Test.Tasty.HUnit

hnixEvalFile :: FilePath -> [String] -> IO (NValueNF (Lazy IO))
hnixEvalFile file incls =  do
  parseResult <- parseNixFileLoc file
  case parseResult of
    Failure err        ->
        error $ "Parsing failed for file `" ++ file ++ "`.\n" ++ show err
    Success expression -> do
        setEnv "TEST_VAR" "foo"
        runLazyM $ normalForm =<< evalLoc (Just file) incls expression

hnixEvalText :: Text -> [String] -> IO (NValueNF (Lazy IO))
hnixEvalText expr incls = case parseNixText expr of
    Failure err        ->
        error $ "Parsing failed for expressien `"
            ++ unpack expr ++ "`.\n" ++ show err
    Success expression ->
        runLazyM $ normalForm =<< eval Nothing incls expression

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
  hnixVal <- (++"\n") . printNix <$> hnixEvalFile fp []
  nixVal <- nixEvalFile fp
  assertEqual fp nixVal hnixVal

assertEvalMatchesNix :: Text -> Assertion
assertEvalMatchesNix expr = do
  hnixVal <- (++"\n") . printNix <$> hnixEvalText expr []
  nixVal <- nixEvalString expr'
  assertEqual expr' nixVal hnixVal
 where
  expr' = unpack expr
