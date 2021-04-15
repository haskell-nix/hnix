{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Nix.Utils
import           Control.Comonad                ( extract )
import qualified Control.DeepSeq               as Deep
import qualified Control.Exception             as Exc
import           GHC.Err                        ( errorWithoutStackTrace )
import           Control.Monad.Catch
import           System.IO                      ( hPutStrLn, getContents )
import           Control.Monad.Free
import qualified Data.HashMap.Lazy             as M
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )
import qualified Data.String                   as String
import           Data.Time
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
import           Nix.Var
import           Nix.Value.Monad
import           Options.Applicative     hiding ( ParserResult(..) )
import           Prettyprinter           hiding ( list )
import           Prettyprinter.Render.Text
import qualified Repl
import           System.FilePath
import qualified Text.Show.Pretty              as PS
import           Nix.Utils.Fix1                 ( Fix1T )

main :: IO ()
main =
  do
    time <- getCurrentTime
    opts <- execParser (nixOptionsInfo time)

    runWithBasicEffectsIO opts $ execContentsFilesOrRepl opts

 where
  execContentsFilesOrRepl opts =
    maybe
      (maybe
        (maybe
          (list
            (withNixContext mempty Repl.main) -- run REPL
            (\case
              ["-"] -> handleResult opts mempty . parseNixTextLoc =<< liftIO Text.getContents
              _paths -> traverse_ (processFile opts) _paths
            )
            (filePaths opts)
          )
          (\ x ->
            -- We can start use Text as in the base case, requires changing FilePath -> Text
            traverse_ (processFile opts) . String.lines =<< liftIO
              (case x of
                "-" ->  getContents  -- get user input
                _path -> readFile _path
              )
          )
          (fromFile opts)
        )
        (handleResult opts mempty . parseNixTextLoc)
        (expression opts)
      )
      (\ path ->
        do
          let file = addExtension (dropExtension path) "nixc"
          process opts (pure file) =<< liftIO (readCache path)
      )
      (readFrom opts)

  processFile opts path =
    do
      eres <- parseNixFileLoc path
      handleResult opts (pure path) eres

  handleResult opts mpath =
    either
      (\ err ->
        bool
          errorWithoutStackTrace
          (liftIO . hPutStrLn stderr)
          (ignoreErrors opts)
          $ "Parse failed: " <> show err
      )

      (\ expr ->
        do
          when (check opts) $
            do
              expr' <- liftIO (reduceExpr mpath expr)
              either
                (\ err -> errorWithoutStackTrace $ "Type error: " <> PS.ppShow err)
                (\ ty  -> liftIO $ putStrLn $ "Type of expression: " <> PS.ppShow
                  (fromJust $ Map.lookup "it" $ Env.types ty)
                )
                (HM.inferTop Env.empty [("it", stripAnnotation expr')])

                -- liftIO $ putStrLn $ runST $
                --     runLintM opts . renderSymbolic =<< lint opts expr

          catch (process opts mpath expr) $
            \case
              NixException frames ->
                errorWithoutStackTrace . show =<<
                  renderFrames
                    @(StdValue (StandardT (StdIdT IO)))
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
      )

  process opts mpath expr
    | evaluate opts =
      if
        | tracing opts             -> evaluateExpression mpath Nix.nixTracingEvalExprLoc printer expr
        | Just path <- reduce opts -> evaluateExpression mpath (reduction path) printer expr
        | not (  null (arg opts)
              && null (argstr opts)
              )                    -> evaluateExpression mpath Nix.nixEvalExprLoc printer expr
        | otherwise                -> processResult printer =<< Nix.nixEvalExprLoc mpath expr
    | xml opts                     =  fail "Rendering expression trees to XML is not yet implemented"
    | json opts                    =  fail "Rendering expression trees to JSON is not implemented"
    | verbose opts >= DebugInfo    =  liftIO $ putStr $ PS.ppShow $ stripAnnotation expr
    | cache opts
      , Just path <- mpath         =  liftIO $ writeCache (addExtension (dropExtension path) "nixc") expr
    | parseOnly opts               =  void $ liftIO $ Exc.evaluate $ Deep.force expr
    | otherwise                    =
      liftIO $
        renderIO
          stdout
          . layoutPretty (LayoutOptions $ AvailablePerLine 80 0.4)
          . prettyNix
          . stripAnnotation $
            expr
   where
    printer
      | finder opts = findAttrs <=< fromValue @(AttrSet (StdValue (StandardT (StdIdT IO))))
      | xml    opts = liftIO . Text.putStrLn . stringIgnoreContext . toXML <=< normalForm
      | json   opts = liftIO . Text.putStrLn . stringIgnoreContext         <=< nvalueToJSONNixString
      | strict opts = liftIO . print         . prettyNValue                <=< normalForm
      | values opts = liftIO . print         . prettyNValueProv            <=< removeEffects
      | otherwise   = liftIO . print         . prettyNValue                <=< removeEffects
     where
      findAttrs
        :: AttrSet (StdValue (StandardT (StdIdT IO)))
        -> StandardT (StdIdT IO) ()
      findAttrs = go mempty
       where
        go prefix s =
          do
            xs <-
              traverse
                (\ (k, nv) ->
                  free
                    (\ (StdThunk (extract -> Thunk _ _ ref)) ->
                      do
                        let
                          path         = prefix <> k
                          (_, descend) = filterEntry path k

                        val <- readVar @(StandardT (StdIdT IO)) ref
                        case val of
                          Computed _    -> pure (k, Nothing)
                          _ ->
                            bool
                              (pure (k, Nothing))
                              ((k, ) <$> forceEntry path nv)
                              descend
                    )
                    (\ v -> pure (k, pure (Free v)))
                    nv
                )
                (sortWith fst (M.toList s))
            traverse_
              (\ (k, mv) ->
                do
                  let
                    path              = prefix <> k
                    (report, descend) = filterEntry path k
                  when report $
                    do
                      liftIO $ Text.putStrLn path
                      when descend $
                        maybe
                          pass
                          (\case
                            NVSet s' _ -> go (path <> ".") s'
                            _          -> pass
                          )
                          mv
              )
              xs
         where
          filterEntry path k = case (path, k) of
            ("stdenv", "stdenv"          ) -> (True , True )
            (_       , "stdenv"          ) -> (False, False)
            (_       , "out"             ) -> (True , False)
            (_       , "src"             ) -> (True , False)
            (_       , "mirrorsFile"     ) -> (True , False)
            (_       , "buildPhase"      ) -> (True , False)
            (_       , "builder"         ) -> (False, False)
            (_       , "drvPath"         ) -> (False, False)
            (_       , "outPath"         ) -> (False, False)
            (_       , "__impureHostDeps") -> (False, False)
            (_       , "__sandboxProfile") -> (False, False)
            ("pkgs"  , "pkgs"            ) -> (True , True )
            (_       , "pkgs"            ) -> (False, False)
            (_       , "drvAttrs"        ) -> (False, False)
            _                              -> (True , True )

          forceEntry
            :: MonadValue a (Fix1T StandardTF (StdIdT IO))
            => Text
            -> a
            -> Fix1T StandardTF (StdIdT IO) (Maybe a)
          forceEntry k v =
            catch
              (pure <$> demand v)
              (\ (NixException frames) ->
                do
                  liftIO
                    . Text.putStrLn
                    . ("Exception forcing " <>) . (k <>) . (": " <>)
                    . show =<<
                      renderFrames
                        @(StdValue (StandardT (StdIdT IO)))
                        @(StdThunk (StandardT (StdIdT IO)))
                        frames
                  pure Nothing
              )

  reduction path mp x =
    do
      eres <-
        Nix.withNixContext
          mp
          (Nix.reducingEvalExpr
            (Eval.eval . annotated . getCompose)
            mp
            x
          )
      handleReduced path eres

  handleReduced
    :: (MonadThrow m, MonadIO m)
    => FilePath
    -> (NExprLoc, Either SomeException (NValue t f m))
    -> m (NValue t f m)
  handleReduced path (expr', eres) =
    do
      liftIO $
        do
          putStrLn $ "Wrote winnowed expression tree to " <> path
          writeFile path $ show $ prettyNix (stripAnnotation expr')
      either
        throwM
        pure
        eres
