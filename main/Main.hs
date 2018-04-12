{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Arrow (second)
import           Control.DeepSeq
import qualified Control.Exception as Exc
import           Control.Monad
import           Control.Monad.ST
import           Data.Fix
import qualified Data.HashMap.Lazy as M
import qualified Data.Text.IO as Text
import qualified Nix
import           Nix.Cache
import           Nix.Exec (Lazy, runLazyM)
import           Nix.Expr
import           Nix.Lint
import           Nix.Normal
import           Nix.Options
import           Nix.Parser
import           Nix.Pretty
import           Nix.Stack (NixException(..))
import qualified Nix.Value as V
-- import           Nix.TH
import           Options.Applicative hiding (ParserResult(..))
import           System.IO
import           System.FilePath
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

main :: IO ()
main = do
    opts <- execParser nixOptionsInfo
    case readFrom opts of
        Just path -> do
            let file = addExtension (dropExtension path) "nix"
            process opts (Just file) =<< readCache path
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
    processFile opts path = do
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
        when (check opts) $
            putStrLn $ runST $ Nix.runLintM . renderSymbolic
                =<< Nix.lint expr

        let parseArg s = case parseNixText s of
                Success x -> x
                Failure err -> errorWithoutStackTrace (show err)

        args <- traverse (traverse (Nix.eval Nothing)) $
            map (second parseArg) (arg opts) ++
            map (second mkStr) (argstr opts)

        let argmap :: Lazy IO (V.NValue (Lazy IO))
            argmap = embed $ Fix $ V.NVSet (M.fromList args) mempty

            compute ev x p = do
                 f <- ev mpath x
                 p =<< case f of
                     Fix (V.NVClosure _ g) ->
                         runLazyM $ normalForm =<< g argmap
                     _ -> pure f

        if | evaluate opts, debug opts ->
                 compute Nix.tracingEvalLoc expr print
           | evaluate opts, not (null args) ->
                 compute Nix.evalLoc expr (putStrLn . printNix)
           | evaluate opts ->
                 putStrLn . printNix =<< Nix.evalLoc mpath expr
           | debug opts ->
                 print $ stripAnnotation expr
           | cache opts, Just path <- mpath -> do
                let file = addExtension (dropExtension path) "nixc"
                writeCache file expr
           | parseOnly opts ->
                 void $ Exc.evaluate $ force expr
           | otherwise ->
                 displayIO stdout
                     . renderPretty 0.4 80
                     . prettyNix
                     . stripAnnotation $ expr
