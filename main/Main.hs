{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.DeepSeq
import qualified Control.Exception as Exc
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Compact as C
import qualified Data.Compact.Serialize as C
import           Data.Text (Text, pack)
import qualified Data.Text.IO as Text
import qualified Nix
import           Nix.Expr
import           Nix.Lint
import           Nix.Parser
import           Nix.Pretty
import           Nix.Stack (NixException(..))
-- import           Nix.TH
import           Options.Applicative hiding (ParserResult(..))
import           System.IO
import           System.FilePath
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

data Options = Options
    { verbose      :: Bool
    , debug        :: Bool
    , evaluate     :: Bool
    , check        :: Bool
    , readFrom     :: Maybe FilePath
    , compact      :: Bool
    , parse        :: Bool
    , parseOnly    :: Bool
    , ignoreErrors :: Bool
    , expression   :: Maybe Text
    , arg          :: [NExpr]
    , argstr       :: [Text]
    , fromFile     :: Maybe FilePath
    , filePaths    :: [FilePath]
    }

mainOptions :: Parser Options
mainOptions = Options
    <$> switch
        (   short 'v'
         <> long "verbose"
         <> help "Verbose output")
    <*> switch
        (   short 'd'
         <> long "debug"
         <> help "Debug output")
    <*> switch
        (   long "eval"
         <> help "Whether to evaluate, or just pretty-print")
    <*> switch
        (   long "check"
         <> help "Whether to check for syntax errors after parsing")
    <*> optional (strOption
        (   long "read"
         <> help "Read in an expression tree from a compacted file"))
    <*> switch
        (   long "compact"
         <> help "Write out the expression tree as a compact region")
    <*> switch
        (   long "parse"
         <> help "Whether to parse the file (also the default right now)")
    <*> switch
        (   long "parse-only"
         <> help "Whether to parse only, no pretty printing or checking")
    <*> switch
        (   long "ignore-errors"
         <> help "Continue parsing files, even if there are errors")
    <*> optional (strOption
        (   short 'E'
         <> long "expr"
         <> help "Expression to parse or evaluate"))
    <*> multiString
            (\s -> case parseNixText (pack s) of
                      Success x -> pure x
                      Failure err -> errorWithoutStackTrace (show err))
        (   long "arg"
         <> help "Argument to pass to an evaluated lambda")
    <*> multiString (pure . pack)
        (   long "argstr"
         <> help "Argument string to pass to an evaluated lambda")
    <*> optional (strOption
        (   short 'f'
         <> long "file"
         <> help "Parse all of the files given in FILE; - means stdin"))
    <*> many (strArgument (metavar "FILE" <> help "Path of file to parse"))
  where
    multiString f desc = many (option (str >>= f) desc)

main :: IO ()
main = do
    opts <- execParser optsDef
    case readFrom opts of
        Just path -> do
            eres <- C.unsafeReadCompact path
            case eres of
                Left err -> error $ "Error reading compact file: " ++ err
                Right expr -> do
                    let file = addExtension (dropExtension path) "nix"
                    process opts (Just file) (C.getCompact expr)
        Nothing -> case expression opts of
            Just s -> handleResult opts Nothing (parseNixTextLoc s)
            Nothing  -> case fromFile opts of
                Just "-" ->
                    mapM_ (processFile opts) =<< (lines <$> getContents)
                Just path ->
                    mapM_ (processFile opts) =<< (lines <$> readFile path)
                Nothing -> case filePaths opts of
                    [] ->
                        handleResult opts Nothing . parseNixTextLoc
                            =<< Text.getContents
                    ["-"] ->
                        handleResult opts Nothing . parseNixTextLoc
                            =<< Text.getContents
                    paths ->
                        mapM_ (processFile opts) paths
  where
    optsDef :: ParserInfo Options
    optsDef = info (helper <*> mainOptions)
                   (fullDesc <> progDesc "" <> header "hnix")

    processFile opts path = do
        -- putStrLn "Parsing file..."
        eres <- parseNixFileLoc path
        handleResult opts (Just path) eres

    -- print . printNix =<< Nix.eval [nix|1 + 3|]

    handleResult opts mpath = \case
        Failure err ->
            (if ignoreErrors opts
             then hPutStrLn stderr
             else errorWithoutStackTrace) $ "Parse failed: " ++ show err

        Success expr -> Exc.catch (process opts mpath expr) $ \case
            NixEvalException msg -> errorWithoutStackTrace msg

    process opts mpath expr = do
        -- expr <- Exc.evaluate $ force expr
        -- putStrLn "Parsing file...done"

        when (check opts) $
            putStrLn $ runST $ Nix.runLintM . renderSymbolic
                =<< Nix.lint expr

        let _args = arg opts ++ map mkStr (argstr opts)

        if | evaluate opts, debug opts ->
                 print =<< Nix.tracingEvalLoc mpath expr
           | evaluate opts ->
                 putStrLn . printNix =<< Nix.evalLoc mpath expr
           | debug opts ->
                 print $ stripAnnotation expr
           | compact opts -> do
                 cx <- C.compact expr
                 case mpath of
                     Nothing -> return ()
                     Just path -> do
                         let file = addExtension (dropExtension path) "nixc"
                         C.writeCompact file cx
           | parseOnly opts ->
                 void $ Exc.evaluate $ force expr
           | otherwise ->
                 displayIO stdout
                     . renderPretty 0.4 80
                     . prettyNix
                     . stripAnnotation $ expr
