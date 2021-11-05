
module TestCommon where

import           GHC.Err                        ( errorWithoutStackTrace )
import           Control.Monad.Catch
import           Data.Time
import           Data.Text.IO as Text
import           Nix
import           Nix.Standard
import           System.Environment
import           System.IO
import           System.PosixCompat.Files
import           System.PosixCompat.Temp
import           System.Process
import           Test.Tasty.HUnit

hnixEvalFile :: Options -> Path -> IO StdVal
hnixEvalFile opts file =
  do
    parseResult <- parseNixFileLoc file
    either
      (\ err -> fail $ "Parsing failed for file `" <> coerce file <> "`.\n" <> show err)
      (\ expr ->
        do
          setEnv "TEST_VAR" "foo"
          runWithBasicEffects opts $
            catch (evaluateExpression (pure $ coerce file) nixEvalExprLoc normalForm expr) $
            \case
              NixException frames ->
                errorWithoutStackTrace . show
                  =<< renderFrames
                      @StdVal
                      @StdThun
                      frames
      )
      parseResult

hnixEvalText :: Options -> Text -> IO StdVal
hnixEvalText opts src =
  either
    (\ err -> fail $ toString $ "Parsing failed for expression `" <> src <> "`.\n" <> show err)
    (runWithBasicEffects opts . (normalForm <=< nixEvalExpr mempty))
    $ parseNixText src

nixEvalString :: Text -> IO Text
nixEvalString expr =
  do
    (fp, h) <- mkstemp "nix-test-eval"
    Text.hPutStr h expr
    hClose h
    res <- nixEvalFile $ coerce fp
    removeLink fp
    pure res

nixEvalFile :: Path -> IO Text
nixEvalFile fp = fromString <$> readProcess "nix-instantiate" ["--eval", "--strict", coerce fp] mempty

assertEvalFileMatchesNix :: Path -> Assertion
assertEvalFileMatchesNix fp =
  do
    time    <- liftIO getCurrentTime
    hnixVal <- (<> "\n") . printNix <$> hnixEvalFile (defaultOptions time) fp
    nixVal  <- nixEvalFile fp
    assertEqual (coerce fp) nixVal hnixVal

assertEvalMatchesNix :: Text -> Assertion
assertEvalMatchesNix expr =
  do
    time    <- liftIO getCurrentTime
    hnixVal <- (<> "\n") . printNix <$> hnixEvalText (defaultOptions time) expr
    nixVal  <- nixEvalString expr
    assertEqual (toString expr) nixVal hnixVal
