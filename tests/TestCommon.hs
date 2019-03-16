{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module TestCommon where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Text (Text, unpack)
import Data.Time
import Nix
import Nix.Thunk.Standard
import System.Environment
import System.IO
import System.Posix.Files
import System.Posix.Temp
import System.Process
import Test.Tasty.HUnit

hnixEvalFile :: Options -> FilePath -> IO (StdValueNF (StdLazy IO))
hnixEvalFile opts file = do
  parseResult <- parseNixFileLoc file
  case parseResult of
    Failure err        ->
        error $ "Parsing failed for file `" ++ file ++ "`.\n" ++ show err
    Success expr -> do
        setEnv "TEST_VAR" "foo"
        runStdLazyM opts $
            catch (evaluateExpression (Just file) nixEvalExprLoc
                                      normalForm expr) $ \case
                NixException frames ->
                    errorWithoutStackTrace . show
                        =<< renderFrames frames

hnixEvalText :: Options -> Text -> IO (StdValueNF (StdLazy IO))
hnixEvalText opts src = case parseNixText src of
    Failure err        ->
        error $ "Parsing failed for expressien `"
            ++ unpack src ++ "`.\n" ++ show err
    Success expr ->
        runStdLazyM opts $ normalForm =<< nixEvalExpr Nothing expr

nixEvalString :: String -> IO String
nixEvalString expr = do
  (fp,h) <- mkstemp "nix-test-eval"
  hPutStr h expr
  hClose h
  res <- nixEvalFile fp
  removeLink fp
  return res

nixEvalFile :: FilePath -> IO String
nixEvalFile fp = readProcess "nix-instantiate" ["--eval", "--strict", fp] ""

assertEvalFileMatchesNix :: FilePath -> Assertion
assertEvalFileMatchesNix fp = do
  time <- liftIO getCurrentTime
  hnixVal <- (++"\n") . printNix <$> hnixEvalFile (defaultOptions time) fp
  nixVal <- nixEvalFile fp
  assertEqual fp nixVal hnixVal

assertEvalMatchesNix :: Text -> Assertion
assertEvalMatchesNix expr = do
  time <- liftIO getCurrentTime
  hnixVal <- (++"\n") . printNix <$> hnixEvalText (defaultOptions time) expr
  nixVal <- nixEvalString expr'
  assertEqual expr' nixVal hnixVal
 where
  expr' = unpack expr
