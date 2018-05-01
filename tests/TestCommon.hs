{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module TestCommon where

import Control.Monad.Catch
import Data.Text (Text, unpack)
import Nix
import System.Environment
import System.IO
import System.Posix.Files
import System.Posix.Temp
import System.Process
import Test.Tasty.HUnit

hnixEvalFile :: Options -> FilePath -> IO (NValueNF (Lazy IO))
hnixEvalFile opts file = do
  parseResult <- parseNixFileLoc file
  case parseResult of
    Failure err        ->
        error $ "Parsing failed for file `" ++ file ++ "`.\n" ++ show err
    Success expr -> do
        setEnv "TEST_VAR" "foo"
        runLazyM opts $
            catch (evaluateExpression (Just file) nixEvalExprLoc
                                      normalForm expr) $ \case
                NixException frames ->
                    errorWithoutStackTrace . show
                        =<< renderFrames @(NThunk (Lazy IO)) frames

hnixEvalText :: Options -> Text -> IO (NValueNF (Lazy IO))
hnixEvalText opts src = case parseNixText src of
    Failure err        ->
        error $ "Parsing failed for expressien `"
            ++ unpack src ++ "`.\n" ++ show err
    Success expr -> do
        runLazyM opts $ normalForm =<< nixEvalExpr Nothing expr

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
  hnixVal <- (++"\n") . printNix <$> hnixEvalFile defaultOptions fp
  nixVal <- nixEvalFile fp
  assertEqual fp nixVal hnixVal

assertEvalMatchesNix :: Text -> Assertion
assertEvalMatchesNix expr = do
  hnixVal <- (++"\n") . printNix <$> hnixEvalText defaultOptions expr
  nixVal <- nixEvalString expr'
  assertEqual expr' nixVal hnixVal
 where
  expr' = unpack expr
