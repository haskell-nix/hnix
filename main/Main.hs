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
    runLazyM opts $ case readFrom opts of
        Just path -> do
            let file = addExtension (dropExtension path) "nix"
            process opts (Just file) =<< liftIO (readCache path)
        Nothing -> case expression opts of
            Just s -> handleResult opts Nothing (parseNixTextLoc s)
            Nothing  -> case fromFile opts of
                Just "-" ->
                    mapM_ (processFile opts)
                        =<< (lines <$> liftIO getContents)
                Just path ->
                    mapM_ (processFile opts)
                        =<< (lines <$> liftIO (readFile path))
                Nothing -> case filePaths opts of
                    [] -> liftIO $ Repl.shell (pure ())
                    ["-"] ->
                        handleResult opts Nothing . parseNixTextLoc
                            =<< liftIO Text.getContents
                    paths ->
                        mapM_ (processFile opts) paths
  where
    processFile opts path = do
        eres <- parseNixFileLoc path
        handleResult opts (Just path) eres

    handleResult opts mpath = \case
        Failure err ->
            (if ignoreErrors opts
             then liftIO . hPutStrLn stderr
             else errorWithoutStackTrace) $ "Parse failed: " ++ show err

        Success expr -> do
            when (check opts) $
                liftIO $ putStrLn $ runST $
                    runLintM opts . renderSymbolic =<< lint opts expr

            catch (process opts mpath expr) $ \case
                NixException frames ->
                    errorWithoutStackTrace . show
                        =<< renderFrames frames

            -- jww (2018-04-24): This shouldn't be in IO, or else it can't
            -- share the environment with the evaluation done above.
            when (repl opts) $ liftIO $ Repl.shell (pure ())

    process opts mpath expr = do
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
                      liftIO . print <=< renderNValue

        if | evaluate opts, tracing opts ->
                 evaluateExpression mpath
                     Nix.nixTracingEvalExprLoc printer expr

           | evaluate opts, Just path <- reduce opts ->
                 evaluateExpression mpath (reduction path) printer expr

           | evaluate opts, not (null (arg opts) && null (argstr opts)) ->
                 evaluateExpression mpath
                     Nix.nixEvalExprLoc printer expr

           | evaluate opts ->
                 processResult printer =<< Nix.nixEvalExprLoc mpath expr

           | xml opts ->
                 error "Rendering expression trees to XML is not yet implemented"

           | json opts ->
                 liftIO $ TL.putStrLn $
                     A.encodeToLazyText (stripAnnotation expr)

           | verbose opts >= DebugInfo ->
                 liftIO $ print $ stripAnnotation expr

           | cache opts, Just path <- mpath ->
                 liftIO $ writeCache (addExtension (dropExtension path) "nixc") expr

           | parseOnly opts ->
                 void $ liftIO $ Exc.evaluate $ Deep.force expr

           | otherwise ->
                 liftIO $ displayIO stdout
                     . renderPretty 0.4 80
                     . prettyNix
                     . stripAnnotation $ expr

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
