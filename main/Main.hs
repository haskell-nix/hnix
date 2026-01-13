{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language MultiWayIf #-}
{-# language TypeFamilies #-}
{-# language RecordWildCards #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}

module Main ( main ) where

import           Nix.Prelude
import           Relude                        as Prelude ( force )
import           Control.Comonad                ( extract )
import qualified Control.Exception             as Exception
import           GHC.Err                        ( errorWithoutStackTrace )
import           Control.Monad.Free
import           Control.Monad.Ref              ( MonadRef(readRef) )
import           Control.Monad.Catch
import           System.IO                      ( hPutStrLn, hIsTerminalDevice )
import qualified Data.HashMap.Strict           as HM
import           Data.Time
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text.IO
import           Text.Show.Pretty               ( ppShow )
import           Nix                     hiding ( force )
import           Nix.Convert
import           Nix.Effects.Derivation        ( Derivation(..), readDerivation )
import           Nix.Json
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Aeson.Key                as AK
import qualified Data.Aeson.KeyMap             as AKM
import qualified Data.ByteString.Lazy          as LBS
import           Nix.Options.Parser             ( nixCommandInfo )
import           Nix.Standard
import           Nix.Thunk.Basic
import           Nix.Type.Env                   ( Env(..) )
import           Nix.Type.Type                  ( Scheme )
import qualified Nix.Type.Infer                as TI
import           Nix.Value.Monad
import           Options.Applicative     hiding ( ParserResult(..) )
import           Prettyprinter           hiding ( list )
import           Prettyprinter.Render.Text      ( renderIO )
import qualified Repl
import           Nix.Eval

main :: IO ()
main =
  do
    currentTime <- getCurrentTime
    cmd <- execParser $ nixCommandInfo currentTime

    case cmd of
      LegacyCommand opts -> main' opts
      DerivationCmd dcmd -> runDerivationCommand currentTime dcmd

-- | Handle derivation subcommands
runDerivationCommand :: UTCTime -> DerivationCommand -> IO ()
runDerivationCommand currentTime = \case
  DerivationShow DerivationShowOpts{..} -> do
    let opts = defaultOptions currentTime
    -- Use minimal config for derivation show (no stats, no tracing)
    withEvalCfg False False False $ \(_ :: Proxy cfg) ->
      runWithStoreEffectsIOT @cfg opts $ do
        -- Get initial paths from either --expr or positional arguments
        initialPaths <- case drvShowExpr of
          Just expr -> do
            -- Evaluate expression to get derivation
            drvPath <- evalExprToDrvPath @cfg expr
            pure [drvPath]
          Nothing -> pure drvShowPaths

        -- Collect all derivation paths (including dependencies if --recursive)
        allPaths <- if drvShowRecursive
          then collectRecursiveDrvPaths initialPaths
          else pure initialPaths

        -- Read all derivations and build JSON output
        -- Format: {"derivations": {"hash-name.drv": {...}}, "version": 4}
        drvs <- forM allPaths $ \drvPath -> do
          drv <- readDerivation drvPath
          let drvKey = fromMaybe (toText $ coerce @Path @FilePath drvPath) $
                         Text.stripPrefix "/nix/store/" (toText $ coerce @Path @FilePath drvPath)
          pure (AK.fromText drvKey, A.toJSON drv)

        let jsonOutput = A.object
              [ "derivations" A..= A.Object (AKM.fromList drvs)
              , "version" A..= (4 :: Int)
              ]

        -- Format based on --pretty/--no-pretty (default: pretty if terminal)
        liftIO $ do
          isTerm <- hIsTerminalDevice stdout
          let shouldPretty = fromMaybe isTerm drvShowPretty
          if shouldPretty
            then LBS.putStr $ A.encodePretty' prettyConfig jsonOutput
            else LBS.putStr $ A.encode jsonOutput
          putStrLn ""
   where
    -- Configure aeson-pretty to match Nix's output format (2-space indentation)
    prettyConfig = A.defConfig { A.confIndent = A.Spaces 2 }

-- | Evaluate an expression and extract its drvPath
evalExprToDrvPath
  :: forall (cfg :: EvalCfg) m. (StdBase m, KnownEvalCfg cfg)
  => Text -> StdM cfg m Path
evalExprToDrvPath expr = do
  case parseNixTextLoc expr of
    Left err -> fail $ "Parse error: " <> show err
    Right parsed -> do
      val <- withNixContext mempty $ nixEvalExprLocT mempty parsed
      demanded <- demand val
      case demanded of
        NVSet _ attrs -> case HM.lookup "drvPath" attrs of
          Just drvPathVal -> do
            drvPathStr <- ignoreContext <$> (fromValue drvPathVal :: StdM cfg m NixString)
            pure $ coerce $ toString drvPathStr
          Nothing -> fail "Expression does not evaluate to a derivation (missing drvPath)"
        _ -> fail "Expression does not evaluate to an attribute set"

-- | Recursively collect all derivation paths including dependencies
collectRecursiveDrvPaths
  :: forall (cfg :: EvalCfg) m. (StdBase m, KnownEvalCfg cfg)
  => [Path] -> StdM cfg m [Path]
collectRecursiveDrvPaths initialPaths = do
  -- Use a set to track visited paths and avoid duplicates
  let go :: Set Path -> [Path] -> StdM cfg m (Set Path)
      go visited [] = pure visited
      go visited (p:ps)
        | p `Set.member` visited = go visited ps
        | otherwise = do
            drv <- readDerivation p
            let (_, inputDrvs) = inputs drv
            let depPaths = coerce . toString <$> Map.keys inputDrvs
            go (Set.insert p visited) (depPaths <> ps)
  visitedSet <- go Set.empty initialPaths
  pure $ Set.toList visitedSet

main' :: Options -> IO ()
main' opts@Options{..} =
  -- Bridge runtime options to type-level configuration.
  -- This enables compile-time specialization in evaluation hot paths.
  withEvalCfg isEvalStats isValues isTrace $
    \(_ :: Proxy cfg) ->
      runWithStoreEffectsIOT @cfg opts (execContentsFilesOrRepl @cfg)
 where
  --  2021-07-15: NOTE: This logic should be weaved stronger through CLI options logic (OptParse-Applicative code)
  -- As this logic is not stated in the CLI documentation, for example. So user has no knowledge of these.
  execContentsFilesOrRepl
    :: forall (cfg :: EvalCfg) m. (StdBase m, KnownEvalCfg cfg) => StdM cfg m ()
  execContentsFilesOrRepl =
    fromMaybe
      loadFromCliFilePathList
      $ loadBinaryCacheFile <|>
        loadLiteralExpression <|>
        loadExpressionFromFile
   where
    -- | The base case: read expressions from the last CLI directive (@[FILE]@) listed on the command line.
    loadFromCliFilePathList :: StdM cfg m ()
    loadFromCliFilePathList =
      case getFilePaths of
        []     -> runRepl
        ["-"]  -> readExpressionFromStdin
        _paths -> processSeveralFiles (coerce _paths)
     where
      -- | Fall back to running the REPL
      runRepl :: StdM cfg m ()
      runRepl = withEmptyNixContext Repl.main

      readExpressionFromStdin :: StdM cfg m ()
      readExpressionFromStdin =
        processExpr @cfg =<< liftIO Text.IO.getContents

    processSeveralFiles :: [Path] -> StdM cfg m ()
    processSeveralFiles = traverse_ processFile
     where
      processFile path = handleResult @cfg (pure path) =<< parseNixFileLoc path

    -- |  The `--read` option: load expression from a serialized file.
    loadBinaryCacheFile :: Maybe (StdM cfg m ())
    loadBinaryCacheFile =
      (\ (binaryCacheFile :: Path) ->
        do
          let file = replaceExtension binaryCacheFile "nixc"
          processCLIOptions @cfg (pure file) =<< liftIO (readCache binaryCacheFile)
      ) <$> getReadFrom

    -- | The `--expr` option: read expression from the argument string
    loadLiteralExpression :: Maybe (StdM cfg m ())
    loadLiteralExpression = processExpr @cfg <$> getExpression

    -- | The `--file` argument: read expressions from the files listed in the argument file
    loadExpressionFromFile :: Maybe (StdM cfg m ())
    loadExpressionFromFile =
      -- We can start use Text as in the base case, requires changing Path -> Text
      -- But that is a gradual process:
      -- https://github.com/haskell-nix/hnix/issues/912
      (processSeveralFiles . (coerce . toString <$>) . lines <=< liftIO) .
        (\case
          "-" -> Text.IO.getContents
          _fp -> readFile _fp
        ) <$> getFromFile

  processExpr
    :: forall (cfg :: EvalCfg) m. (StdBase m, KnownEvalCfg cfg)
    => Text -> StdM cfg m ()
  processExpr = handleResult @cfg mempty . parseNixTextLoc

  withEmptyNixContext :: (StdBase m, KnownEvalCfg cfg) => StdM cfg m a -> StdM cfg m a
  withEmptyNixContext = withNixContext mempty

  --  2021-07-15: NOTE: @handleResult@ & @process@ - have atrocious size & compexity, they need to be decomposed & refactored.
  handleResult
    :: forall (cfg :: EvalCfg) m err. (StdBase m, KnownEvalCfg cfg, Show err)
    => Maybe Path -> Either err NExprLoc -> StdM cfg m ()
  handleResult mpath =
    either
      (\ err ->
        bool
          errorWithoutStackTrace
          (liftIO . hPutStrLn stderr)
          isIgnoreErrors
          $ "Parse failed: " <> show err
      )

      (\ expr ->
        do
          when isCheck $
            do
              expr' <- liftIO $ reduceExpr mpath expr
              either
                (\ err -> errorWithoutStackTrace $ "Type error: " <> ppShow err)
                (liftIO . putStrLn . (<>) "Type of expression: " .
                  ppShow . maybeToMonoid . HM.lookup @VarName @[Scheme] "it" . coerce
                )
                $ TI.inferTop mempty $ curry one "it" $ stripAnnotation expr'

                -- liftIO $ putStrLn $ runST $
                --     runLintM opts . renderSymbolic =<< lint opts expr

          catch (processCLIOptions @cfg mpath expr) $
            \case
              NixException frames ->
                errorWithoutStackTrace . show =<<
                  renderFrames
                    @(StdValM cfg m)
                    @(StdThunM cfg m)
                    frames

          when isRepl $
            withEmptyNixContext $
              bool
                Repl.main
                ((Repl.main' . pure) =<< nixEvalExprLocT (coerce mpath) expr)
                isEvaluate
      )

  --  2021-07-15: NOTE: Logic of CLI Option processing is scattered over several functions, needs to be consolicated.
  -- Now uses type-level dispatch for stats/tracing via KnownEvalCfg cfg.
  processCLIOptions
    :: forall (cfg :: EvalCfg) m. (StdBase m, KnownEvalCfg cfg)
    => Maybe Path -> NExprLoc -> StdM cfg m ()
  processCLIOptions mpath expr
    | isEvaluate =
      if
        -- Type-level dispatch: when CfgTrace cfg ~ 'True, use tracing evaluator
        | Just path <- getReduce        -> evaluateExprWith (reduction path . coerce) expr
        | null getArg || null getArgstr -> evaluateExprWith nixEvalExprLocT expr
        | otherwise                     -> processResult printer <=< nixEvalExprLocT (coerce mpath) $ expr
    | isXml                        = fail "Rendering expression trees to XML is not yet implemented"
    | isJson                       = fail "Rendering expression trees to JSON is not implemented"
    | getVerbosity >= DebugInfo    = liftIO . putStr . ppShow . stripAnnotation $ expr
    | isCache , Just path <- mpath = liftIO . writeCache (replaceExtension path "nixc") $ expr
    | isParseOnly                  = void . liftIO . Exception.evaluate . force $ expr
    | otherwise                    =
      liftIO .
        renderIO
          stdout
          . layoutPretty (LayoutOptions $ AvailablePerLine 80 0.4)
          . prettyNix
          . stripAnnotation
          $ expr
   where
    evaluateExprWith evaluator = evaluateExpression (coerce mpath) evaluator printer

    printer
      :: StdValM cfg m
      -> StdM cfg m ()
    printer
      | isFinder    = findAttrs <=< fromValue @(AttrSet (StdValM cfg m))
      | otherwise = printer'
     where
      -- 2021-05-27: NOTE: With naive fix of the #941
      -- This is overall a naive printer implementation, as options should interact/respect one another.
      -- A nice question: "Should respect one another to what degree?": Go full combinator way, for which
      -- old Nix CLI is nototrious for (and that would mean to reimplement the old Nix CLI),
      -- OR: https://github.com/haskell-nix/hnix/issues/172 and have some sane standart/default behaviour for (most) keys.
      printer'
        | isXml     = out (ignoreContext . toXML)       normalForm
        | isJson    = outJson toJSONNixString           normalForm
        | isStrict  = out (show . prettyNValue)         normalForm
        | isValues  = out (show . prettyNValueProv)     removeEffects
        | otherwise = out (show . prettyNValue)         removeEffects
       where
        out
          :: (b -> Text)
          -> (a -> StdM cfg m b)
          -> a
          -> StdM cfg m ()
        out transform val = liftIO . Text.IO.putStrLn . transform <=< val

        -- | Special case for JSON: toJSONNixString is monadic, not pure
        outJson
          :: (b -> StdM cfg m NixString)
          -> (a -> StdM cfg m b)
          -> a
          -> StdM cfg m ()
        outJson transform val a = liftIO . Text.IO.putStrLn . ignoreContext =<< transform =<< val a

      findAttrs
        :: AttrSet (StdValM cfg m)
        -> StdM cfg m ()
      findAttrs = go mempty
       where
        go :: Text -> AttrSet (StdValM cfg m) -> StdM cfg m ()
        go prefix s =
          traverse_
            (\ (k, mv) ->
              do
                let
                  path              = prefix <> k
                  (report, descend) = filterEntry path k
                when report $
                  do
                    liftIO $ Text.IO.putStrLn path
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
                    (\ (StdThunk (extract -> Thunk _ ref)) ->
                      do
                        let
                          path         = prefix <> k
                          (_, descend) = filterEntry path k

                        val <- readRef ref
                        bool
                          (pure Nothing)
                          (forceEntry path nv)
                          (descend &&
                            thunkState
                              (const False)  -- computed: don't descend
                              (const True)   -- deferred: descend
                              (const True)   -- computing: descend
                              val
                          )
                    )
                    (pure . pure . Free)
                    nv
                )
                (sortWith fst $ HM.toList $ HM.mapKeys varNameText s)
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
            :: MonadValue a (StdM cfg m)
            => Text
            -> a
            -> StdM cfg m (Maybe a)
          forceEntry k v =
            catch
              (pure <$> demand v)
              fun
           where
            fun :: NixException -> StdM cfg m (Maybe a)
            fun (coerce -> frames) =
              do
                liftIO
                  . Text.IO.putStrLn
                  . (("Exception forcing " <> k <> ": ") <>)
                  . show =<<
                  renderFrames
                      @(StdValM cfg m)
                      @(StdThunM cfg m)
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

