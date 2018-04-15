{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Arrow (second)
import qualified Control.DeepSeq as Deep
import qualified Control.Exception as Exc
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.ST
import           Data.Fix
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import           Nix
import qualified Repl
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
                    [] -> Repl.shell (pure ())
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
            putStrLn $ runST $ runLintM . renderSymbolic =<< lint expr

        if | evaluate opts, debug opts ->
                 runLazyM $ evaluateExpression opts mpath
                     Nix.tracingEvalLoc (liftIO . print) expr

           | evaluate opts, not (null (arg opts) && null (argstr opts)) ->
                 runLazyM $ evaluateExpression opts mpath
                     Nix.evalLoc (liftIO . print) expr

           | evaluate opts -> runLazyM $
                 processResult opts (liftIO . print)
                     =<< Nix.evalLoc mpath (include opts) expr

           | debug opts -> print $ stripAnnotation expr

           | cache opts, Just path <- mpath -> do
                let file = addExtension (dropExtension path) "nixc"
                writeCache file expr

           | parseOnly opts -> void $ Exc.evaluate $ Deep.force expr

           | otherwise ->
                 displayIO stdout
                     . renderPretty 0.4 80
                     . prettyNix
                     . stripAnnotation $ expr

        when (repl opts) $ Repl.shell (pure ())
