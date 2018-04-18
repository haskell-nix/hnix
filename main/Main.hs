{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.DeepSeq as Deep
import qualified Control.Exception as Exc
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.ST
import qualified Data.Aeson.Encoding as A
import qualified Data.Aeson.Text as A
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import           Nix
import           Nix.Convert
import           Nix.Utils
import           Options.Applicative hiding (ParserResult(..))
import qualified Repl
import           System.FilePath
import           System.IO
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

    handleResult opts mpath = \case
        Failure err ->
            (if ignoreErrors opts
             then hPutStrLn stderr
             else errorWithoutStackTrace) $ "Parse failed: " ++ show err

        Success expr -> Exc.catch (process opts mpath expr) $ \case
            NixEvalException msg -> errorWithoutStackTrace msg

    process opts mpath expr = do
        when (check opts) $
            putStrLn $ runST $
                runLintM opts . renderSymbolic =<< lint opts expr

        let printer :: (MonadNix e m, MonadIO m) => NValue m -> m ()
            printer | xml opts =
                      liftIO . putStrLn . toXML <=< normalForm
                    | json opts =
                      liftIO . TL.putStrLn
                             . TL.decodeUtf8
                             . A.encodingToLazyByteString
                             . toEncodingSorted
                             <=< fromNix
                    | otherwise = liftIO . print

        if | evaluate opts, verbose opts >= Debug ->
                 runLazyM opts $ evaluateExpression mpath
                     Nix.tracingEvalLoc printer expr

           | evaluate opts, not (null (arg opts) && null (argstr opts)) ->
                 runLazyM opts $ evaluateExpression mpath
                     Nix.evalLoc printer expr

           | evaluate opts -> runLazyM opts $
                 processResult printer =<< Nix.evalLoc mpath expr

           | xml opts ->
                 error "Rendering expression trees to XML is not yet implemented"

           | json opts ->
                 TL.putStrLn $ A.encodeToLazyText (stripAnnotation expr)

           | verbose opts >= Debug -> print $ stripAnnotation expr

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
