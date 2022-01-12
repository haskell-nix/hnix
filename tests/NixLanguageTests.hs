module NixLanguageTests (genTests) where

import           Nix.Prelude
import           Control.Exception
import           GHC.Err                        ( errorWithoutStackTrace )
import           Control.Monad.ST
import           Data.List                      ( delete )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.String                   as String
import qualified Data.Text                     as Text
import           Data.Time
import           GHC.Exts
import           Nix.Lint
import           Nix.Options
import           Nix.Options.Parser
import           Nix.Parser
import           Nix.Pretty
import           Nix.String
import           Nix.XML
import qualified Options.Applicative           as Opts
import           System.Environment             ( setEnv )
import           System.FilePath.Glob           ( compile
                                                , globDir1
                                                )
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
groupBy key = Map.fromListWith (<>) . fmap (key &&& pure)

-- | New tests, which have never yet passed.  Once any of these is passing,
-- please remove it from this list.  Do not add tests to this list if they have
-- previously passed.
newFailingTests :: Set String
newFailingTests = Set.fromList
  [ 
    "eval-okay-hash"
  , "eval-okay-path"  -- #128
  , "eval-okay-fromTOML"
  , "eval-okay-ind-string" -- #1000 #610 
  ]

-- | Upstream tests that test cases that HNix disaded as a misfeature that is used so rarely
-- that it more effective to fix it & lint it out of existance.
deprecatedRareNixQuirkTests :: Set String
deprecatedRareNixQuirkTests = Set.fromList $
  one
    -- A rare quirk of Nix that is proper to fix&enforce then to support (see git commit history)
    "eval-okay-strings-as-attrs-names"

genTests :: IO TestTree
genTests =
  do
    testFiles <- getTestFiles
    let
      testsGroupedByName :: Map Path [Path]
      testsGroupedByName = groupBy (takeFileName . dropExtensions) testFiles

      testsGroupedByTypeThenName :: Map [String] [(Path, [Path])]
      testsGroupedByTypeThenName = groupBy testType $ Map.toList testsGroupedByName

      testTree :: [TestTree]
      testTree = mkTestGroup <$> Map.toList testsGroupedByTypeThenName

    pure $
      localOption
        (mkTimeout 2000000)
        $ testGroup
            "Nix (upstream) language tests"
            testTree
 where

  getTestFiles :: IO [Path]
  getTestFiles = sortTestFiles <$> collectTestFiles
   where
    collectTestFiles :: IO [Path]
    collectTestFiles = coerce (globDir1 (compile "*-*-*.*") nixTestDir)

    sortTestFiles :: [Path] -> [Path]
    sortTestFiles =
      sort
        -- Disabling the not yet done tests cases.
        . filter withoutDisabledTests
        . filter withoutXml
     where
      withoutDisabledTests :: Path -> Bool
      withoutDisabledTests = (`Set.notMember` (newFailingTests `Set.union` deprecatedRareNixQuirkTests)) . takeBaseName

      withoutXml :: Path -> Bool
      withoutXml = (/= ".xml") . takeExtension

  testType :: (Path, b) -> [String]
  testType (fullpath, _files) = coerce (take 2 . splitOn "-") $ takeFileName fullpath

  mkTestGroup :: ([String], [(Path, [Path])]) -> TestTree
  mkTestGroup (tType, tests) =
    testGroup (String.unwords tType) $ mkTestCase <$> tests
   where
    mkTestCase :: (Path, [Path]) -> TestTree
    mkTestCase (basename, files) =
      testCase
        (coerce $ takeFileName basename)
        $ do
            time <- liftIO getCurrentTime
            let opts = defaultOptions time
            case tType of
              ["parse", "okay"] -> assertParse opts $ the files
              ["parse", "fail"] -> assertParseFail opts $ the files
              ["eval" , "okay"] -> assertEval opts files
              ["eval" , "fail"] -> assertEvalFail $ the files
              _                 -> fail $ "Unexpected: " <> show tType


assertParse :: Options -> Path -> Assertion
assertParse _opts file =
  either
    (\ err -> assertFailure $ "Failed to parse " <> coerce file <> ":\n" <> show err)
    (const stub)  -- pure $! runST $ void $ lint opts expr
    =<< parseNixFileLoc file

assertParseFail :: Options -> Path -> Assertion
assertParseFail opts file =
  (`catch` \(_ :: SomeException) -> stub) $
    either
      (const stub)
      (\ expr ->
        do
          _ <- pure $! runST $ void $ lint opts expr
          assertFailure $ "Unexpected success parsing `" <> coerce file <> ":\nParsed value: " <> show expr
      )
      =<< parseNixFileLoc file

assertLangOk :: Options -> Path -> Assertion
assertLangOk opts fileBaseName =
  do
    actual   <- printNix <$> hnixEvalFile opts (addNixExt fileBaseName)
    expected <- read fileBaseName ".exp"
    assertEqual mempty expected (actual <> "\n")

assertLangOkXml :: Options -> Path -> Assertion
assertLangOkXml opts fileBaseName =
  do
    actual <- ignoreContext . toXML <$> hnixEvalFile opts (addNixExt fileBaseName)
    expected <- read fileBaseName ".exp.xml"
    assertEqual mempty expected actual

assertEval :: Options -> [Path] -> Assertion
assertEval _opts files =
  do
    time <- liftIO getCurrentTime
    let opts = defaultOptions time
    case delete ".nix" $ sort $ fromString @Text . takeExtensions <$> files of
      []                  -> void $ hnixEvalFile opts $ addNixExt name
      [".exp"          ]  -> assertLangOk    opts name
      [".exp.xml"      ]  -> assertLangOkXml opts name
      [".exp.disabled" ]  -> stub
      [".exp-disabled" ]  -> stub
      [".exp", ".flags"]  ->
        do
          liftIO $ setEnv "NIX_PATH" "lang/dir4:lang/dir5"
          flags <- read name ".flags"
          let
            flags' :: Text
            flags' =
              bool
                id
                Text.init
                (Text.last flags == '\n')
                flags
          case runParserGetResult time flags' of
            Opts.Failure           err   -> errorWithoutStackTrace $ "Error parsing flags from " <> coerce name <> ".flags: " <> show err
            Opts.CompletionInvoked _     -> fail "unused"
            Opts.Success           opts' -> assertLangOk opts' name
      _ -> assertFailure $ "Unknown test type " <> show files
 where
  runParserGetResult :: UTCTime -> Text -> Opts.ParserResult Options
  runParserGetResult time flags' =
    Opts.execParserPure
      Opts.defaultPrefs
      (nixOptionsInfo time)
      (fmap toString $ fixup $ Text.splitOn " " flags')

  name :: Path
  name = coerce nixTestDir <> the (takeFileName . dropExtensions <$> files)

  fixup :: [Text] -> [Text]
  fixup ("--arg"    : x : y : rest) = "--arg"    : (x <> "=" <> y) : fixup rest
  fixup ("--argstr" : x : y : rest) = "--argstr" : (x <> "=" <> y) : fixup rest
  fixup (x                  : rest) =                          x  : fixup rest
  fixup []                          = mempty

assertEvalFail :: Path -> Assertion
assertEvalFail file =
  (`catch` (\(_ :: SomeException) -> stub)) $
  do
    time       <- liftIO getCurrentTime
    evalResult <- printNix <$> hnixEvalFile (defaultOptions time) file
    evalResult `seq` assertFailure $ "File: ''" <> coerce file <> "'' should not evaluate.\nThe evaluation result was `" <> toString evalResult <> "`."

nixTestDir :: FilePath
nixTestDir = "data/nix/tests/lang/"

addNixExt :: Path -> Path
addNixExt path = addExtension path ".nix"

read :: Path -> String -> IO Text
read path ext = readFile $ addExtension path ext
