{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NixLanguageTests (genTests) where

import           Control.Arrow ((&&&))
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.ST
import           Data.List (delete, sort)
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           GHC.Exts
import           Nix.Frames
import           Nix.Lint
import           Nix.Options
import           Nix.Parser
import           Nix.Pretty
import           Nix.Render.Frame
import           Nix.Utils
import           Nix.XML
import qualified Options.Applicative as Opts
import           System.Environment
import           System.FilePath
import           System.FilePath.Glob (compile, globDir1)
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestCommon

{-
From (git://nix)/tests/lang.sh we see that

    lang/parse-fail-*.nix -> parsing should fail
    lang/parse-okay-*.nix -> parsing should succeed
    lang/eval-fail-*.nix -> eval should fail

    lang/eval-okay-*.{nix,xml} -> eval should succeed,
        xml dump should be the same as the .xml
    lang/eval-okay-*.{nix,exp} -> eval should succeed,
        plain text output should be the same as the .exp
    lang/eval-okay-*.{nix,exp,flags} -> eval should succeed,
        plain text output should be the same as the .exp,
        pass the extra flags to nix-instantiate

    NIX_PATH=lang/dir3:lang/dir4 should be in the environment of all
        eval-okay-*.nix evaluations
    TEST_VAR=foo should be in all the environments # for eval-okay-getenv.nix
-}

groupBy :: Ord k => (v -> k) -> [v] -> Map k [v]
groupBy key = Map.fromListWith (++) . map (key &&& pure)

genTests :: IO TestTree
genTests = do
  testFiles <- sort . filter ((/= ".xml") . takeExtension)
      <$> globDir1 (compile "*-*-*.*") "data/nix/tests/lang"
  let testsByName = groupBy (takeFileName . dropExtensions) testFiles
  let testsByType = groupBy testType (Map.toList testsByName)
  let testGroups  = map mkTestGroup (Map.toList testsByType)
  return $ localOption (mkTimeout 2000000)
         $ testGroup "Nix (upstream) language tests" testGroups
  where
    testType (fullpath, _files) =
        take 2 $ splitOn "-" $ takeFileName fullpath
    mkTestGroup (kind, tests) =
        testGroup (unwords kind) $ map (mkTestCase kind) tests
    mkTestCase kind (basename, files) =
        testCase (takeFileName basename) $ case kind of
            ["parse", "okay"] -> assertParse defaultOptions $ the files
            ["parse", "fail"] -> assertParseFail defaultOptions $ the files
            ["eval", "okay"]  -> assertEval defaultOptions files
            ["eval", "fail"]  -> assertEvalFail $ the files
            _ -> error $ "Unexpected: " ++ show kind

assertParse :: Options -> FilePath -> Assertion
assertParse _opts file = parseNixFileLoc file >>= \case
  Success _expr -> return () -- pure $! runST $ void $ lint opts expr
  Failure err  ->
      assertFailure $ "Failed to parse " ++ file ++ ":\n" ++ show err

assertParseFail :: Options -> FilePath -> Assertion
assertParseFail opts file = do
    eres <- parseNixFileLoc file
    catch (case eres of
               Success expr -> do
                   _ <- pure $! runST $ void $ lint opts expr
                   assertFailure $ "Unexpected success parsing `"
                       ++ file ++ ":\nParsed value: " ++ show expr
               Failure _ -> return ()) $ \(_ :: SomeException) ->
        return ()

assertLangOk :: Options -> FilePath -> Assertion
assertLangOk opts file = do
  actual <- printNix <$> hnixEvalFile opts (file ++ ".nix")
  expected <- Text.readFile $ file ++ ".exp"
  assertEqual "" expected $ Text.pack (actual ++ "\n")

assertLangOkXml :: Options -> FilePath -> Assertion
assertLangOkXml opts file = do
  actual <- toXML <$> hnixEvalFile opts (file ++ ".nix")
  expected <- Text.readFile $ file ++ ".exp.xml"
  assertEqual "" expected $ Text.pack actual

assertEval :: Options -> [FilePath] -> Assertion
assertEval opts files = catch go $ \case
    NixException frames -> do
        -- msg <- runReaderT (renderFrames frames) opts
        -- error $ "Evaluation error: " ++ show msg
        error "Evaluation error"
  where
    go = case delete ".nix" $ sort $ map takeExtensions files of
        [] -> assertLangOkXml defaultOptions name
        [".exp"] -> assertLangOk defaultOptions name
        [".exp.disabled"] -> return ()
        [".exp-disabled"] -> return ()
        [".exp", ".flags"] -> do
            liftIO $ unsetEnv "NIX_PATH"
            flags <- Text.readFile (name ++ ".flags")
            let flags' | Text.last flags == '\n' = Text.init flags
                       | otherwise = flags
            case Opts.execParserPure Opts.defaultPrefs nixOptionsInfo
                     (fixup (map Text.unpack (Text.splitOn " " flags'))) of
                Opts.Failure err -> errorWithoutStackTrace $
                    "Error parsing flags from " ++ name ++ ".flags: "
                        ++ show err
                Opts.Success opts' ->
                    assertLangOk
                        (opts' { include = include opts' ++
                                   [ "nix=../../../../data/nix/corepkgs"
                                   , "lang/dir4"
                                   , "lang/dir5" ] })
                        name
                Opts.CompletionInvoked _ -> error "unused"
        _ -> assertFailure $ "Unknown test type " ++ show files
      where
        name = "data/nix/tests/lang/"
            ++ the (map (takeFileName . dropExtensions) files)

        fixup ("--arg":x:y:rest) = "--arg":(x ++ "=" ++ y):fixup rest
        fixup ("--argstr":x:y:rest) = "--argstr":(x ++ "=" ++ y):fixup rest
        fixup (x:rest) = x:fixup rest
        fixup [] = []

assertEvalFail :: FilePath -> Assertion
assertEvalFail file = catch ?? (\(_ :: SomeException) -> return ()) $ do
  evalResult <- printNix <$> hnixEvalFile defaultOptions file
  evalResult `seq` assertFailure $
      file ++ " should not evaluate.\nThe evaluation result was `"
           ++ evalResult ++ "`."
