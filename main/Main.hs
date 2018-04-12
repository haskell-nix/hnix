{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Arrow (second)
import           Control.DeepSeq
import qualified Control.Exception as Exc
import           Control.Monad
import           Control.Monad.ST
import           Data.Fix
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
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

        let parseArg s = case parseNixText s of
                Success x -> x
                Failure err -> errorWithoutStackTrace (show err)

        args <- traverse (traverse (Nix.eval Nothing (include opts))) $
            map (second parseArg) (arg opts) ++
            map (second mkStr) (argstr opts)

        let argmap :: Lazy IO (V.NValue (Lazy IO))
            argmap = embed $ Fix $ V.NVSet (M.fromList args) mempty

            compute ev x p = do
                 f <- ev mpath (include opts) x
                 p =<< case f of
                     Fix (V.NVClosure _ g) ->
                         runLazyM $ normalForm =<< g argmap
                     _ -> pure f

            result h = case attr opts of
                Nothing -> h
                Just (Text.splitOn "." -> keys) -> go keys
              where
                go [] v = h v
                go ((Text.decimal -> Right (n,_)):ks) v = case v of
                    Fix (V.NVList xs) -> case ks of
                        [] -> h (xs !! n)
                        _  -> go ks (xs !! n)
                    _ -> errorWithoutStackTrace $
                            "Expected a list for selector '" ++ show n
                                ++ "', but got: " ++ show v
                go (k:ks) v = case v of
                    Fix (V.NVSet xs _) ->
                        case M.lookup k xs of
                            Nothing ->
                                errorWithoutStackTrace $
                                    "Set does not contain key '"
                                        ++ Text.unpack k ++ "'"
                            Just v' -> case ks of
                                [] -> h v'
                                _  -> go ks v'
                    _ -> errorWithoutStackTrace $
                        "Expected a set for selector '" ++ Text.unpack k
                            ++ "', but got: " ++ show v

        if | evaluate opts, debug opts ->
                 compute Nix.tracingEvalLoc expr (result print)

           | evaluate opts, not (null args) ->
                 compute Nix.evalLoc expr (result (putStrLn . printNix))

           | evaluate opts ->
                 result (putStrLn . printNix)
                     =<< Nix.evalLoc mpath (include opts) expr

           | debug opts -> print $ stripAnnotation expr

           | cache opts, Just path <- mpath -> do
                let file = addExtension (dropExtension path) "nixc"
                writeCache file expr

           | parseOnly opts -> void $ Exc.evaluate $ force expr

           | otherwise ->
                 displayIO stdout
                     . renderPretty 0.4 80
                     . prettyNix
                     . stripAnnotation $ expr

        when (repl opts) $
            Repl.shell (pure ())
