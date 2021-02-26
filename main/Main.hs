{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Comonad                ( extract )
import qualified Control.DeepSeq               as Deep
import qualified Control.Exception             as Exc
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Free
import           Control.Monad.IO.Class
import           Data.Bool                      ( bool )
import qualified Data.HashMap.Lazy             as M
import qualified Data.Map                      as Map
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromJust )
import           Data.Time
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Nix
import           Nix.Convert
import qualified Nix.Eval                      as Eval
import           Nix.Fresh.Basic
import           Nix.Json
import           Nix.Options.Parser
import           Nix.Standard
import           Nix.Thunk.Basic
import qualified Nix.Type.Env                  as Env
import qualified Nix.Type.Infer                as HM
import           Nix.Utils
import           Nix.Var
import           Nix.Value.Monad
import           Options.Applicative     hiding ( ParserResult(..) )
import           Prettyprinter
import           Prettyprinter.Render.Text
import qualified Repl
import           System.FilePath
import           System.IO
import qualified Text.Show.Pretty              as PS

main :: IO ()
main = do
  time <- getCurrentTime
  opts <- execParser (nixOptionsInfo time)
  runWithBasicEffectsIO opts $
    case readFrom opts of
      Nothing -> case expression opts of
        Nothing -> case fromFile opts of
          Nothing -> case filePaths opts of
            [] -> withNixContext mempty Repl.main
            ["-"] ->
              handleResult opts mempty
                .   parseNixTextLoc
                =<< liftIO Text.getContents
            paths -> mapM_ (processFile opts) paths
          Just "-" -> mapM_ (processFile opts) . lines =<< liftIO getContents
          Just path ->
            mapM_ (processFile opts) . lines =<< liftIO (readFile path)
        Just s  -> handleResult opts mempty (parseNixTextLoc s)
      Just path -> do
        let file = addExtension (dropExtension path) "nixc"
        process opts (pure file) =<< liftIO (readCache path)
 where
  processFile opts path = do
    eres <- parseNixFileLoc path
    handleResult opts (pure path) eres

  handleResult opts mpath = \case
    Failure err ->
      (bool
        errorWithoutStackTrace
        (liftIO . hPutStrLn stderr)
        (ignoreErrors opts)
        )
        $  "Parse failed: "
        <> show err

    Success expr -> do
      when (check opts) $ do
        expr' <- liftIO (reduceExpr mpath expr)
        either
          (\ err -> errorWithoutStackTrace $ "Type error: " <> PS.ppShow err)
          (\ ty  -> liftIO $ putStrLn $ "Type of expression: " <> PS.ppShow
            (fromJust $ Map.lookup "it" $ Env.types ty)
          )
          (HM.inferTop Env.empty [("it", stripAnnotation expr')])

          -- liftIO $ putStrLn $ runST $
          --     runLintM opts . renderSymbolic =<< lint opts expr

      catch (process opts mpath expr) $ \case
        NixException frames ->
          errorWithoutStackTrace
            .   show
            =<< renderFrames @(StdValue (StandardT (StdIdT IO)))
                  @(StdThunk (StandardT (StdIdT IO)))
                  frames

      when (repl opts) $
        withNixContext mempty $
          bool
            Repl.main
            (do
              val <- Nix.nixEvalExprLoc mpath expr
              Repl.main' $ pure val
            )
            (evaluate opts)

  process opts mpath expr
    | evaluate opts
    , tracing opts
    = evaluateExpression mpath Nix.nixTracingEvalExprLoc printer expr
    | evaluate opts
    , Just path <- reduce opts
    = evaluateExpression mpath (reduction path) printer expr
    | evaluate opts
    , not (null (arg opts) && null (argstr opts))
    = evaluateExpression mpath Nix.nixEvalExprLoc printer expr
    | evaluate opts
    = processResult printer =<< Nix.nixEvalExprLoc mpath expr
    | xml opts
    = error "Rendering expression trees to XML is not yet implemented"
    | json opts
    = error "Rendering expression trees to JSON is not implemented"
    | verbose opts >= DebugInfo
    = liftIO $ putStr $ PS.ppShow $ stripAnnotation expr
    | cache opts
    , Just path <- mpath
    = liftIO $ writeCache (addExtension (dropExtension path) "nixc") expr
    | parseOnly opts
    = void $ liftIO $ Exc.evaluate $ Deep.force expr
    | otherwise
    = liftIO
      $ renderIO stdout
      . layoutPretty (LayoutOptions $ AvailablePerLine 80 0.4)
      . prettyNix
      . stripAnnotation
      $ expr
   where
    printer
      | finder opts
      = fromValue @(AttrSet (StdValue (StandardT (StdIdT IO)))) >=> findAttrs
      | xml opts
      = liftIO
        .   putStrLn
        .   Text.unpack
        .   stringIgnoreContext
        .   toXML
        <=< normalForm
      | json opts
      = liftIO
        .   Text.putStrLn
        .   stringIgnoreContext
        <=< nvalueToJSONNixString
      | strict opts
      = liftIO . print . prettyNValue <=< normalForm
      | values opts
      = liftIO . print . prettyNValueProv <=< removeEffects
      | otherwise
      = liftIO . print . prettyNValue <=< removeEffects
     where
      findAttrs
        :: AttrSet (StdValue (StandardT (StdIdT IO)))
        -> StandardT (StdIdT IO) ()
      findAttrs = go mempty
       where
        go prefix s = do
          xs <- forM (sortOn fst (M.toList s)) $ \(k, nv) ->
            free
              (\ (StdThunk (extract -> Thunk _ _ ref)) -> do
                let path         = prefix <> Text.unpack k
                    (_, descend) = filterEntry path k
                val <- readVar @(StandardT (StdIdT IO)) ref
                case val of
                  Computed _    -> pure (k, Nothing)
                  _ | descend   -> (k,        ) <$> forceEntry path nv
                    | otherwise -> pure (k, Nothing)
              )
              (\ v -> pure (k, pure (Free v)))
              nv
          forM_ xs $ \(k, mv) -> do
            let path              = prefix <> Text.unpack k
                (report, descend) = filterEntry path k
            when report $ do
              liftIO $ putStrLn path
              when descend $
                maybe
                  (pure ())
                  (\case
                    NVSet s' _ -> go (path <> ".") s'
                    _          -> pure ()
                  )
                  mv
         where
          filterEntry path k = case (path, k) of
            ("stdenv", "stdenv"          ) -> (True, True)
            (_       , "stdenv"          ) -> (False, False)
            (_       , "out"             ) -> (True, False)
            (_       , "src"             ) -> (True, False)
            (_       , "mirrorsFile"     ) -> (True, False)
            (_       , "buildPhase"      ) -> (True, False)
            (_       , "builder"         ) -> (False, False)
            (_       , "drvPath"         ) -> (False, False)
            (_       , "outPath"         ) -> (False, False)
            (_       , "__impureHostDeps") -> (False, False)
            (_       , "__sandboxProfile") -> (False, False)
            ("pkgs"  , "pkgs"            ) -> (True, True)
            (_       , "pkgs"            ) -> (False, False)
            (_       , "drvAttrs"        ) -> (False, False)
            _                              -> (True, True)

          forceEntry k v =
            catch (pure <$> demand v pure) $ \(NixException frames) -> do
              liftIO
                .   putStrLn
                .   ("Exception forcing " <>)
                .   (k <>)
                .   (": " <>)
                .   show
                =<< renderFrames @(StdValue (StandardT (StdIdT IO)))
                      @(StdThunk (StandardT (StdIdT IO)))
                      frames
              pure Nothing

  reduction path mp x = do
    eres <- Nix.withNixContext mp
      $ Nix.reducingEvalExpr (Eval.eval . annotated . getCompose) mp x
    handleReduced path eres

  handleReduced
    :: (MonadThrow m, MonadIO m)
    => FilePath
    -> (NExprLoc, Either SomeException (NValue t f m))
    -> m (NValue t f m)
  handleReduced path (expr', eres) = do
    liftIO $ do
      putStrLn $ "Wrote winnowed expression tree to " <> path
      writeFile path $ show $ prettyNix (stripAnnotation expr')
    either
      throwM
      pure
      eres
