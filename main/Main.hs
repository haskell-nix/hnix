{-# language MultiWayIf #-}
{-# language TypeFamilies #-}
{-# language RecordWildCards #-}

module Main ( main ) where

import           Relude                        as Prelude ( force )
import           Control.Comonad                ( extract )
import qualified Control.Exception             as Exception
import           GHC.Err                        ( errorWithoutStackTrace )
import           Control.Monad.Free
import           Control.Monad.Ref              ( MonadRef(readRef) )
import           Control.Monad.Catch
import           System.IO                      ( hPutStrLn )
import qualified Data.HashMap.Lazy             as M
import qualified Data.Map                      as Map
import           Data.Time
import qualified Data.Text.IO                  as Text
import           Text.Show.Pretty               ( ppShow )
import           Nix                     hiding ( force )
import           Nix.Convert
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
import           Prettyprinter.Render.Text      ( renderIO )
import qualified Repl
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
  execContentsFilesOrRepl :: StdIO
  execContentsFilesOrRepl =
    fromMaybe
      loadFromCliFilePathList
      $ loadBinaryCacheFile <|>
        loadLiteralExpression <|>
        loadExpressionFromFile
   where
    -- | The base case: read expressions from the last CLI directive (@[FILE]@) listed on the command line.
    loadFromCliFilePathList :: StdIO
    loadFromCliFilePathList =
      case filePaths of
        []     -> runRepl
        ["-"]  -> readExpressionFromStdin
        _paths -> processSeveralFiles (coerce _paths)
     where
      -- | Fall back to running the REPL
      runRepl = withEmptyNixContext Repl.main

      readExpressionFromStdin =
        processExpr =<< liftIO Text.getContents

    processSeveralFiles :: [Path] -> StdIO
    processSeveralFiles = traverse_ processFile
     where
      processFile path = handleResult (pure path) =<< parseNixFileLoc path

    -- |  The `--read` option: load expression from a serialized file.
    loadBinaryCacheFile :: Maybe StdIO
    loadBinaryCacheFile =
      (\ (binaryCacheFile :: Path) ->
        do
          let file = replaceExtension binaryCacheFile "nixc"
          processCLIOptions (pure file) =<< liftIO (readCache binaryCacheFile)
      ) <$> readFrom

    -- | The `--expr` option: read expression from the argument string
    loadLiteralExpression :: Maybe StdIO
    loadLiteralExpression = processExpr <$> expression

    -- | The `--file` argument: read expressions from the files listed in the argument file
    loadExpressionFromFile :: Maybe StdIO
    loadExpressionFromFile =
      -- We can start use Text as in the base case, requires changing Path -> Text
      -- But that is a gradual process:
      -- https://github.com/haskell-nix/hnix/issues/912
      (processSeveralFiles . (coerce . toString <$>) . lines <=< liftIO) .
        (\case
          "-" -> Text.getContents
          _fp -> readFile _fp
        ) <$> fromFile

  processExpr :: Text -> StdIO
  processExpr = handleResult mempty . parseNixTextLoc

  withEmptyNixContext = withNixContext mempty

  --  2021-07-15: NOTE: @handleResult@ & @process@ - have atrocious size & compexity, they need to be decomposed & refactored.
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
                (\ err -> errorWithoutStackTrace $ "Type error: " <> ppShow err)
                (liftIO . putStrLn . (<>) "Type of expression: " .
                  ppShow . maybeToMonoid . Map.lookup @VarName @[Scheme] "it" . coerce
                )
                $ HM.inferTop mempty $ curry one "it" $ stripAnnotation expr'

                -- liftIO $ putStrLn $ runST $
                --     runLintM opts . renderSymbolic =<< lint opts expr

          catch (processCLIOptions mpath expr) $
            \case
              NixException frames ->
                errorWithoutStackTrace . show =<<
                  renderFrames
                    @StdVal
                    @StdThun
                    frames

          when repl $
            withEmptyNixContext $
              bool
                Repl.main
                ((Repl.main' . pure) =<< nixEvalExprLoc (coerce mpath) expr)
                evaluate
      )

  --  2021-07-15: NOTE: Logic of CLI Option processing is scattered over several functions, needs to be consolicated.
  processCLIOptions :: Maybe Path -> NExprLoc -> StdIO
  processCLIOptions mpath expr
    | evaluate =
      if
        | tracing                       -> evaluateExprWithEvaluator nixTracingEvalExprLoc expr
        | Just path <- reduce           -> evaluateExprWithEvaluator (reduction path . coerce) expr
        | null arg || null argstr       -> evaluateExprWithEvaluator nixEvalExprLoc expr
        | otherwise                     -> processResult printer <=< nixEvalExprLoc (coerce mpath) $ expr
    | xml                        = fail "Rendering expression trees to XML is not yet implemented"
    | json                       = fail "Rendering expression trees to JSON is not implemented"
    | verbose >= DebugInfo       =  liftIO . putStr . ppShow . stripAnnotation $ expr
    | cache , Just path <- mpath =  liftIO . writeCache (replaceExtension path "nixc") $ expr
    | parseOnly                  =  void . liftIO . Exception.evaluate . force $ expr
    | otherwise                  =
      liftIO .
        renderIO
          stdout
          . layoutPretty (LayoutOptions $ AvailablePerLine 80 0.4)
          . prettyNix
          . stripAnnotation
          $ expr
   where
    evaluateExprWithEvaluator evaluator = evaluateExpression (coerce mpath) evaluator printer

    printer
      :: StdVal
      -> StdIO
    printer
      | finder    = findAttrs <=< fromValue @(AttrSet StdVal)
      | otherwise = printer'
     where
      printer'
        | xml       = fun (ignoreContext . toXML)                     normalForm
        -- 2021-05-27: NOTE: With naive fix of the #941
        -- This is overall a naive printer implementation, as options should interact/respect one another.
        -- A nice question: "Should respect one another to what degree?": Go full combinator way, for which
        -- old Nix CLI is nototrious for (and that would mean to reimplement the old Nix CLI),
        -- OR: https://github.com/haskell-nix/hnix/issues/172 and have some sane standart/default behaviour for (most) keys.
        | json      = fun (ignoreContext . mempty . nvalueToJSONNixString) normalForm
        | strict    = fun (show . prettyNValue)                             normalForm
        | values    = fun (show . prettyNValueProv)                         removeEffects
        | otherwise = fun (show . prettyNValue)                             removeEffects
       where
        fun
          :: (b -> Text)
          -> (a -> StandardIO b)
          -> a
          -> StdIO
        fun g f = liftIO . Text.putStrLn . g <=< f

      findAttrs
        :: AttrSet StdVal
        -> StdIO
      findAttrs = go mempty
       where
        go :: Text -> AttrSet StdVal -> StdIO
        go prefix s =
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
                        stub
                        (\case
                          NVSet _ s' -> go (path <> ".") s'
                          _          -> stub
                        )
                        mv
            )
            =<< traverse
                (\ (k, nv) ->
                  (k, ) <$>
                  free
                    (\ (StdThunk (extract -> Thunk _ _ ref)) ->
                      do
                        let
                          path         = prefix <> k
                          (_, descend) = filterEntry path k

                        val <- readRef @StandardIO ref
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
                (sortWith fst $ M.toList $ M.mapKeys coerce s)
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
            :: MonadValue a StandardIO
            => Text
            -> a
            -> StandardIO (Maybe a)
          forceEntry k v =
            catch
              (pure <$> demand v)
              fun
           where
            fun :: NixException -> StandardIO (Maybe a)
            fun (coerce -> frames) =
              do
                liftIO
                  . Text.putStrLn
                  . (("Exception forcing " <> k <> ": ") <>)
                  . show =<<
                    renderFrames
                      @StdVal
                      @StdThun
                      frames
                pure Nothing

  reduction path mpathToContext annExpr =
    do
      eres <-
        withNixContext
          mpathToContext
          $ reducingEvalExpr
              evalContent
              mpathToContext
              annExpr
      handleReduced path eres

  handleReduced
    :: (MonadThrow m, MonadIO m)
    => Path
    -> (NExprLoc, Either SomeException (NValue t f m))
    -> m (NValue t f m)
  handleReduced (coerce -> path) (expr', eres) =
    do
      liftIO $
        do
          putStrLn $ "Wrote sifted expression tree to " <> path
          writeFile path $ show $ prettyNix $ stripAnnotation expr'
      either throwM pure eres
