module TestCommon where

import           Nix
import           Nix.Monad
import           Nix.Monad.Instance
import           Nix.Parser
import           Nix.Pretty
import           System.Directory
import           System.Environment
import           System.IO
import           System.Posix.Temp
import           System.Process
import           Test.Tasty.HUnit

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
hnixEvalString expr =  do
  case parseNixString expr of
    Failure err        ->
        error $ "Parsing failed for expressien `" ++ expr ++ "`.\n" ++ show err
    Success expression -> eval Nothing expression

nixEvalString :: String -> IO String
nixEvalString expr = do
  (fp,h) <- mkstemp "nix-test-eval"
  hPutStr h expr
  hClose h
  res <- nixEvalFile fp
  removeFile fp
  return res

nixEvalFile :: FilePath -> IO String
nixEvalFile fp = do
  readProcess "nix-instantiate" ["--eval", fp] ""


assertEvalMatchesNix :: String -> Assertion
assertEvalMatchesNix expr = do
  hnixVal <- (++"\n") . printNix <$> hnixEvalString expr
  nixVal <- nixEvalString expr
  assertEqual expr nixVal hnixVal
