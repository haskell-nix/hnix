{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main ( main ) where

import           Nix.Utils
import           Control.Comonad                ( extract )
import qualified Control.DeepSeq               as Deep
import qualified Control.Exception             as Exc
import           GHC.Err                        ( errorWithoutStackTrace )
import           Control.Monad.Free
import           Control.Monad.Ref              ( MonadRef(readRef) )
import           Control.Monad.Catch
import           System.IO                      ( hPutStrLn, getContents )
import qualified Data.HashMap.Lazy             as M
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )
import qualified Data.String                   as String
import           Data.Time
import qualified Data.Text.IO                  as Text
import           Nix
import           Nix.Convert
import           Nix.Fresh.Basic
import           Nix.Json
import           Nix.Options.Parser
import           Nix.Standard
import           Nix.Thunk.Basic
import           Nix.Type.Env                   ( Env(..) )
import           Nix.Type.Type                  ( Scheme )
import qualified Nix.Type.Infer                as HM
import           Nix.Value.Monad
import           Options.Applicative     hiding ( ParserResult(..) )
import           Prettyprinter           hiding ( list )
import           Prettyprinter.Render.Text
import qualified Repl
import           System.FilePath
import qualified Text.Show.Pretty              as PS
import           Nix.Utils.Fix1                 ( Fix1T )
import           Nix.Eval

main :: IO ()
main =
  do
    time <- getCurrentTime
    opts <- execParser $ nixOptionsInfo time

    main' opts

main' :: Options -> IO ()
main' opts@Options{..} = runWithBasicEffectsIO opts execContentsFilesOrRepl
 where
  --  2021-07-15: NOTE: This logic should be weaved stronger through CLI options logic (OptParse-Applicative code)
  -- As this logic is not stated in the CLI documentation, for example. So user has no knowledge of these.
  execContentsFilesOrRepl :: StandardT (StdIdT IO) ()
  execContentsFilesOrRepl =
    fromMaybe
      loadFromCLIFilePathList
      ( loadBinaryCacheFile <|>
        loadLiteralExpression <|>
        loadExpressionFromFile
      )
   where
    -- | The base case: read expressions from the last CLI directive (@[FILE]@) listed on the command line.
    loadFromCLIFilePathList =
      case filePaths of
        []     -> runRepl
        ["-"]  -> readExpressionFromStdin
        _paths -> processSeveralFiles _paths
     where
      -- | Fall back to running the REPL
      runRepl = withEmptyNixContext Repl.main

      readExpressionFromStdin =
        do
          expr <- liftIO Text.getContents
          processExpr expr

    processSeveralFiles files = traverse_ processFile files
     where
      processFile path = handleResult (pure path) =<< parseNixFileLoc path

    -- |  The `--read` option: load expression from a serialized file.
    loadBinaryCacheFile =
      (\binaryCacheFile ->
        do
          let file = replaceExtension binaryCacheFile "nixc"
          processCLIOptions (Just file) =<< liftIO (readCache binaryCacheFile)
      ) <$> readFrom

    -- | The `--expr` option: read expression from the argument string
    loadLiteralExpression = processExpr <$> expression

    -- | The `--file` argument: read expressions from the files listed in the argument file
    loadExpressionFromFile =
      -- We can start use Text as in the base case, requires changing FilePath -> Text
      -- But that is a gradual process:
      -- https://github.com/haskell-nix/hnix/issues/912
      (processSeveralFiles . String.lines <=< liftIO) .
        (\case
          "-" -> getContents
          _fp -> readFile _fp
        ) <$> fromFile

  processExpr text = handleResult Nothing     $   parseNixTextLoc text

  withEmptyNixContext = withNixContext mempty

  --  2021-07-15: NOTE: @handleResult@ & @process@ - are atrocious size, they need to be decomposed & refactored.
  handleResult mpath =
    either
      (\ err ->
        bool
          errorWithoutStackTrace
          (liftIO . hPutStrLn stderr)
          ignoreErrors
          $ "Parse failed: " <> show err
      )

      (\ expr ->
        do
          when check $
            do
              expr' <- liftIO $ reduceExpr mpath expr
              either
                (\ err -> errorWithoutStackTrace $ "Type error: " <> PS.ppShow err)
                (\ ty  -> liftIO $ putStrLn $ "Type of expression: " <> PS.ppShow
                  (fromJust $ Map.lookup "it" (coerce ty :: Map Text [Scheme]))
                )
                (HM.inferTop mempty [("it", stripAnnotation expr')])

                -- liftIO $ putStrLn $ runST $
                --     runLintM opts . renderSymbolic =<< lint opts expr

          catch (processCLIOptions mpath expr) $
            \case
              NixException frames ->
                errorWithoutStackTrace . show =<<
                  renderFrames
                    @(StdValue (StandardT (StdIdT IO)))
                    @(StdThunk (StandardT (StdIdT IO)))
                    frames

          when repl $
            withEmptyNixContext $
              bool
                Repl.main
                (do
                  val <- nixEvalExprLoc mpath expr
                  Repl.main' $ pure val
                )
                evaluate
      )

  --  2021-07-15: NOTE: Logic of CLI Option processing is scattered over several functions, needs to be consolicated.
  processCLIOptions mpath expr
    | evaluate =
      if
        | tracing                       -> evaluateExpression mpath nixTracingEvalExprLoc printer expr
        | Just path <- reduce           -> evaluateExpression mpath (reduction path) printer expr
        | not (null arg && null argstr) -> evaluateExpression mpath nixEvalExprLoc printer expr
        | otherwise                     -> processResult printer =<< nixEvalExprLoc mpath expr
    | xml                        =  fail "Rendering expression trees to XML is not yet implemented"
    | json                       =  fail "Rendering expression trees to JSON is not implemented"
    | verbose >= DebugInfo       =  liftIO $ putStr $ PS.ppShow $ stripAnnotation expr
    | cache , Just path <- mpath =  liftIO $ writeCache (replaceExtension path "nixc") expr
    | parseOnly                  =  void $ liftIO $ Exc.evaluate $ Deep.force expr
    | otherwise                  =
      liftIO $
        renderIO
          stdout
          . layoutPretty (LayoutOptions $ AvailablePerLine 80 0.4)
          . prettyNix
          . stripAnnotation
          $ expr
   where
    printer
      | finder    = findAttrs <=< fromValue @(AttrSet (StdValue (StandardT (StdIdT IO))))
      | xml       = liftIO . Text.putStrLn . stringIgnoreContext . toXML <=< normalForm
      -- 2021-05-27: NOTE: With naive fix of the #941
      -- This is overall a naive printer implementation, as options should interact/respect one another.
      -- A nice question: "Should respect one another to what degree?": Go full combinator way, for which
      -- old Nix CLI is nototrious for (and that would mean to reimplement the old Nix CLI),
      -- OR: https://github.com/haskell-nix/hnix/issues/172 and have some sane standart/default behaviour for (most) keys.
      | json      = liftIO . Text.putStrLn . stringIgnoreContext         <=< nvalueToJSONNixString <=< normalForm
      | strict    = liftIO . print         . prettyNValue                <=< normalForm
      | values    = liftIO . print         . prettyNValueProv            <=< removeEffects
      | otherwise = liftIO . print         . prettyNValue                <=< removeEffects
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
                  (k, ) <$>
                  free
                    (\ (StdThunk (extract -> Thunk _ _ ref)) ->
                      do
                        let
                          path         = prefix <> k
                          (_, descend) = filterEntry path k

                        val <- readRef @(StandardT (StdIdT IO)) ref
                        bool
                          (pure Nothing)
                          (forceEntry path nv)
                          (descend &&
                           deferred
                            (const False)
                            (const True)
                            val
                          )
                    )
                    (pure . pure . Free)
                    nv
                )
                (sortWith fst $ M.toList s)
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
                    . (("Exception forcing " <> k <> ": ") <>)
                    . show =<<
                      renderFrames
                        @(StdValue (StandardT (StdIdT IO)))
                        @(StdThunk (StandardT (StdIdT IO)))
                        frames
                  pure Nothing
              )

  reduction path mpathToContext annExpr =
    do
      eres <-
        withNixContext
          mpathToContext
          (reducingEvalExpr
            evalContent
            mpathToContext
            annExpr
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
          putStrLn $ "Wrote sifted expression tree to " <> path
          writeFile path $ show $ prettyNix $ stripAnnotation expr'
      either throwM pure eres
