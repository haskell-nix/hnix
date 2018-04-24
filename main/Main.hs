{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.DeepSeq as Deep
import qualified Control.Exception as Exc
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.ST
import           Control.Monad.Trans.Reader
import qualified Data.Aeson.Encoding as A
import qualified Data.Aeson.Text as A
import           Data.Functor.Compose
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import           Nix
import           Nix.Convert
import qualified Nix.Eval as Eval
import           Nix.Lint
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
            NixException frames ->
                errorWithoutStackTrace . show
                    =<< runReaderT (renderFrames frames) opts

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
                    | normalize opts =
                      liftIO . print . prettyNixValue <=< normalForm
                    | otherwise  =
                      liftIO . print

        if | evaluate opts, tracing opts ->
                 runLazyM opts $ evaluateExpression mpath
                     Nix.nixTracingEvalExprLoc printer expr

           | evaluate opts, Just path <- reduce opts ->
                 runLazyM opts $
                 evaluateExpression mpath (reduction path) printer expr

           | evaluate opts, not (null (arg opts) && null (argstr opts)) ->
                 runLazyM opts $ evaluateExpression mpath
                     Nix.nixEvalExprLoc printer expr

           | evaluate opts -> runLazyM opts $
                 processResult printer =<< Nix.nixEvalExprLoc mpath expr

           | xml opts ->
                 error "Rendering expression trees to XML is not yet implemented"

           | json opts ->
                 TL.putStrLn $ A.encodeToLazyText (stripAnnotation expr)

           | verbose opts >= DebugInfo -> print $ stripAnnotation expr

           | cache opts, Just path <- mpath ->
                writeCache (addExtension (dropExtension path) "nixc") expr

           | parseOnly opts -> void $ Exc.evaluate $ Deep.force expr

           | otherwise ->
                 displayIO stdout
                     . renderPretty 0.4 80
                     . prettyNix
                     . stripAnnotation $ expr

        when (repl opts) $ Repl.shell (pure ())

    reduction path mp x = do
        eres <- Nix.withNixContext mp $
            Nix.reducingEvalExpr (Eval.eval . annotated . getCompose) mp x
        handleReduced path eres

    handleReduced :: (MonadThrow m, MonadIO m)
                  => FilePath
                  -> (NExprLoc, Either SomeException (NValue m))
                  -> m (NValue m)
    handleReduced path (expr', eres) = do
        liftIO $ do
            putStrLn $ "Wrote winnowed expression tree to " ++ path
            writeFile path $ show $ prettyNix (stripAnnotation expr')
        case eres of
            Left err -> throwM err
            Right v  -> return v
