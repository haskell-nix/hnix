module TestCommon where

import Data.Text (Text, unpack)
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

hnixEvalText :: Text -> IO (NValueNF (Lazy IO))
hnixEvalText expr = case parseNixText expr of
    Failure err        ->
        error $ "Parsing failed for expressien `"
            ++ unpack expr ++ "`.\n" ++ show err
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

assertEvalMatchesNix :: Text -> Assertion
assertEvalMatchesNix expr = do
  hnixVal <- (++"\n") . printNix <$> hnixEvalText expr
  nixVal <- nixEvalString expr'
  assertEqual expr' nixVal hnixVal
 where
  expr' = unpack expr
