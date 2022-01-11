
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
            evaluateExpression (pure $ coerce file) nixEvalExprLoc normalForm expr
              `catch`
                \case
                  NixException frames ->
                    errorWithoutStackTrace . show
                      =<< renderFrames
                          @StdVal
                          @StdThun
                          frames
      )
      parseResult

nixEvalFile :: Path -> IO Text
nixEvalFile (coerce -> fp) = fromString <$> readProcess "nix-instantiate" ["--eval", "--strict", fp] mempty
hnixEvalText :: Options -> Text -> IO StdVal
hnixEvalText opts src =
  either
    (\ err -> fail $ toString $ "Parsing failed for expression `" <> src <> "`.\n" <> show err)
    (runWithBasicEffects opts . (normalForm <=< nixEvalExpr mempty))
    $ parseNixText src

nixEvalText :: Text -> IO Text
nixEvalText expr =
  do
    (fp, h) <- mkstemp "nix-test-eval"
    Text.hPutStr h expr
    hClose h
    res <- nixEvalFile $ coerce fp
    removeLink fp
    pure res

assertEvalMatchesNix
  :: ( Options
    -> Text -> IO (NValue t (StdCited StandardIO) StandardIO)
    )
  -> (Text -> IO Text)
  -> Text
  -> IO ()
assertEvalMatchesNix evalHNix evalNix fp =
  do
    time    <- liftIO getCurrentTime
    hnixVal <- (<> "\n") . printNix <$> evalHNix (defaultOptions time) fp
    nixVal  <- evalNix fp
    assertEqual (toString fp) nixVal hnixVal

-- | Compares @HNix@ & @Nix@ return results.
assertEvalFileMatchesNix :: Path -> Assertion
assertEvalFileMatchesNix fp =
  assertEvalMatchesNix
    (\ o -> hnixEvalFile o . coerce . toString)
    (nixEvalFile . coerce . toString)
    $ fromString $ coerce fp

assertEvalTextMatchesNix :: Text -> Assertion
assertEvalTextMatchesNix =
  assertEvalMatchesNix hnixEvalText nixEvalText
