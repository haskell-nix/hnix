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
import qualified Nix.Eval                      as Eval
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

main :: IO ()
main =
  do
    time <- getCurrentTime
    opts <- execParser $ nixOptionsInfo time

    main' opts

main' :: Options -> IO ()
main' (opts@Options{..}) = runWithBasicEffectsIO opts execContentsFilesOrRepl
 where
  execContentsFilesOrRepl =
    firstJust
      -- The `--read` option: load expression from a serialized file.
      [ readFrom <&> \path -> do
          let file = addExtension (dropExtension path) "nixc"
          process (Just file) =<< liftIO (readCache path)

      -- The `--expr` option: read expression from the argument string
      , expression <&> processText

      -- The `--file` argument: read expressions from the files listed in the argument file
      , fromFile <&> \x ->
          -- We can start use Text as in the base case, requires changing FilePath -> Text
          traverse_ processFile . String.lines =<< liftIO
            (case x of
              "-" -> getContents
              fp -> readFile fp
            )
      ]
    `orElse`
      -- The base case: read expressions from the files listed on the command line
      case filePaths of
        -- With no files, fall back to running the REPL
        [] -> withNixContext mempty Repl.main
        ["-"] -> processText =<< liftIO Text.getContents
        _paths -> traverse_ processFile _paths

  firstJust :: [Maybe a] -> Maybe a
  firstJust = asum

  orElse :: Maybe a -> a -> a
  orElse = flip fromMaybe

  processText text = handleResult Nothing     $   parseNixTextLoc text

  processFile path = handleResult (Just path) =<< parseNixFileLoc path

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
              expr' <- liftIO (reduceExpr mpath expr)
              either
                (\ err -> errorWithoutStackTrace $ "Type error: " <> PS.ppShow err)
                (\ ty  -> liftIO $ putStrLn $ "Type of expression: " <> PS.ppShow
                  (fromJust $ Map.lookup "it" (coerce ty :: Map Text [Scheme]))
                )
                (HM.inferTop mempty [("it", stripAnnotation expr')])

                -- liftIO $ putStrLn $ runST $
                --     runLintM opts . renderSymbolic =<< lint opts expr

          catch (process mpath expr) $
            \case
              NixException frames ->
                errorWithoutStackTrace . show =<<
                  renderFrames
                    @(StdValue (StandardT (StdIdT IO)))
                    @(StdThunk (StandardT (StdIdT IO)))
                    frames

          when repl $
            withNixContext mempty $
              bool
                Repl.main
                (do
                  val <- Nix.nixEvalExprLoc mpath expr
                  Repl.main' $ pure val
                )
                evaluate
      )

  process mpath expr
    | evaluate =
      if
        | tracing                       -> evaluateExpression mpath Nix.nixTracingEvalExprLoc printer expr
        | Just path <- reduce           -> evaluateExpression mpath (reduction path) printer expr
        | not (null arg && null argstr) -> evaluateExpression mpath Nix.nixEvalExprLoc printer expr
        | otherwise                     -> processResult printer =<< Nix.nixEvalExprLoc mpath expr
    | xml                        =  fail "Rendering expression trees to XML is not yet implemented"
    | json                       =  fail "Rendering expression trees to JSON is not implemented"
    | verbose >= DebugInfo       =  liftIO $ putStr $ PS.ppShow $ stripAnnotation expr
    | cache , Just path <- mpath =  liftIO $ writeCache (addExtension (dropExtension path) "nixc") expr
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

  reduction path mp x =
    do
      eres <-
        Nix.withNixContext
          mp
          (Nix.reducingEvalExpr
            Eval.evalContent
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
          putStrLn $ "Wrote sifted expression tree to " <> path
          writeFile path $ show $ prettyNix $ stripAnnotation expr'
      either throwM pure eres
