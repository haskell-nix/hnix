{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestCommon where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Time
import           Nix
import           Nix.Standard
import           Nix.Fresh.Basic
import           System.Environment
import           System.IO
import           System.Posix.Files
import           System.Posix.Temp
import           System.Process
import           Test.Tasty.HUnit

hnixEvalFile :: Options -> FilePath -> IO (StdValue (StandardT (StdIdT IO)))
hnixEvalFile opts file =
  do
    parseResult <- parseNixFileLoc file
    either
      (\ err -> fail $ "Parsing failed for file `" <> file <> "`.\n" <> show err)
      (\ expr ->
        do
          setEnv "TEST_VAR" "foo"
          runWithBasicEffects opts $
            catch (evaluateExpression (pure file) nixEvalExprLoc normalForm expr) $
            \case
              NixException frames ->
                errorWithoutStackTrace . show
                  =<< renderFrames
                        @(StdValue (StandardT (StdIdT IO)))
                        @(StdThunk (StandardT (StdIdT IO)))
                        frames
      )
      parseResult

hnixEvalText :: Options -> Text -> IO (StdValue (StandardT (StdIdT IO)))
hnixEvalText opts src =
  either
    (\ err -> fail $ "Parsing failed for expression `" <> unpack src <> "`.\n" <> show err)
    (\ expr ->
      runWithBasicEffects opts $ normalForm =<< nixEvalExpr mempty expr
    )
    (parseNixText src)

nixEvalString :: String -> IO String
nixEvalString expr = do
  (fp, h) <- mkstemp "nix-test-eval"
  hPutStr h expr
  hClose h
  res <- nixEvalFile fp
  removeLink fp
  pure res

nixEvalFile :: FilePath -> IO String
nixEvalFile fp = readProcess "nix-instantiate" ["--eval", "--strict", fp] ""

assertEvalFileMatchesNix :: FilePath -> Assertion
assertEvalFileMatchesNix fp = do
  time    <- liftIO getCurrentTime
  hnixVal <- (<> "\n") . printNix <$> hnixEvalFile (defaultOptions time) fp
  nixVal  <- nixEvalFile fp
  assertEqual fp nixVal hnixVal

assertEvalMatchesNix :: Text -> Assertion
assertEvalMatchesNix expr = do
  time    <- liftIO getCurrentTime
  hnixVal <- (<> "\n") . printNix <$> hnixEvalText (defaultOptions time) expr
  nixVal  <- nixEvalString expr'
  assertEqual expr' nixVal hnixVal
 where
  expr' = unpack expr
