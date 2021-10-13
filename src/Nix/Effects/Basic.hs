{-# language CPP #-}

module Nix.Effects.Basic where

import           Prelude                 hiding ( head
                                                )
import           Relude.Unsafe                  ( head )
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Control.Monad                  ( foldM )
import qualified Data.HashMap.Lazy             as M
import           Data.List.Split                ( splitOn )
import qualified Data.Text                     as Text
import           Prettyprinter                  ( fillSep )
import           System.FilePath                ( takeDirectory
                                                , isAbsolute
                                                , splitDirectories
                                                , (</>)
                                                , joinPath
                                                )
import           Nix.Convert
import           Nix.Effects
import           Nix.Exec                       ( MonadNix
                                                , evalExprLoc
                                                , nixInstantiateExpr
                                                )
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.Parser
import           Nix.Render
import           Nix.Scope
import           Nix.String
import           Nix.Value
import           Nix.Value.Monad

#ifdef MIN_VERSION_ghc_datasize
import           GHC.DataSize
#endif

defaultToAbsolutePath :: MonadNix e t f m => Path -> m Path
defaultToAbsolutePath origPath = do
  origPathExpanded <- expandHomePath origPath
  absPath          <-
    bool
      (do
        cwd <- do
          mres <- lookupVar "__cur_file"
          maybe
            getCurrentDirectory
            (
              (\case
                NVPath s -> pure $ coerce takeDirectory s
                val -> throwError $ ErrorCall $ "when resolving relative path, __cur_file is in scope, but is not a path; it is: " <> show val
              ) <=< demand
            )
            mres
        pure $ cwd <///> origPathExpanded
      )
      (pure origPathExpanded)
      (isAbsolute $ coerce origPathExpanded)
  removeDotDotIndirections <$> canonicalizePath absPath

expandHomePath :: MonadFile m => Path -> m Path
expandHomePath (coerce -> ('~' : xs)) = (<> coerce xs) <$> getHomeDirectory
expandHomePath p          = pure p

-- | Incorrectly normalize paths by rewriting patterns like @a/b/..@ to @a@.
--   This is incorrect on POSIX systems, because if @b@ is a symlink, its
--   parent may be a different directory from @a@. See the discussion at
--   https://hackage.haskell.org/package/directory-1.3.1.5/docs/System-Directory.html#v:canonicalizePath
removeDotDotIndirections :: Path -> Path
removeDotDotIndirections = coerce . intercalate "/" . go mempty . splitOn "/" . coerce
 where
  go s       []            = reverse s
  go (_ : s) (".." : rest) = go s rest
  go s       (this : rest) = go (this : s) rest

infixr 9 <///>
(<///>) :: Path -> Path -> Path
(coerce -> x) <///> (coerce -> y)
  | isAbsolute y || "." `isPrefixOf` y = coerce $ x </> y
  | otherwise                          = joinByLargestOverlap x y
 where
  joinByLargestOverlap (splitDirectories -> xs) (splitDirectories -> ys) = coerce $
    joinPath $ head
      [ xs <> drop (length tx) ys | tx <- tails xs, tx `elem` inits ys ]

defaultFindEnvPath :: MonadNix e t f m => String -> m Path
defaultFindEnvPath = findEnvPathM . coerce

findEnvPathM :: forall e t f m . MonadNix e t f m => Path -> m Path
findEnvPathM name =
  maybe
    (fail "impossible")
    (\ v ->
      do
        nv <- demand v
        l <- fromValue @[NValue t f m] nv
        findPathBy nixFilePath l name
    )
    =<< lookupVar "__nixPath"

 where
  nixFilePath :: MonadEffects t f m => Path -> m (Maybe Path)
  nixFilePath path = do
    absPath <- toAbsolutePath @t @f path
    isDir   <- doesDirectoryExist absPath
    absFile <-
      bool
        (pure absPath)
        (toAbsolutePath @t @f $ coerce $ coerce absPath </> "default.nix")
        isDir
    exists <- doesFileExist absFile
    pure $ pure absFile `whenTrue` exists

findPathBy
  :: forall e t f m
   . MonadNix e t f m
  => (Path -> m (Maybe Path))
  -> [NValue t f m]
  -> Path
  -> m Path
findPathBy finder ls name = do
  maybe
    (throwError $ ErrorCall $ "file ''" <> coerce name <> "'' was not found in the Nix search path (add it's using $NIX_PATH or -I)")
    pure
    =<< foldM go mempty ls
 where
  go :: MonadNix e t f m => Maybe Path -> NValue t f m -> m (Maybe Path)
  go =
    maybe
      (\ nv ->
        do
          (s :: HashMap VarName (NValue t f m)) <- fromValue =<< demand nv
          p <- resolvePath s
          nvpath <- demand p
          path <- fromValue nvpath

          maybe
            (tryPath path mempty)
            (\ nv' ->
              do
                mns <- fromValueMay @NixString =<< demand nv'
                tryPath path $
                  whenJust
                    (\ nsPfx ->
                      let pfx = stringIgnoreContext nsPfx in
                      pure $ coerce $ toString pfx `whenFalse` Text.null pfx
                    )
                    mns
            )
            (M.lookup "prefix" s)
      )
      (const . pure . pure)

  tryPath :: Path -> Maybe Path -> m (Maybe Path)
  tryPath p (Just n) | n' : ns <- splitDirectories (coerce name), n == coerce n' =
    finder $ p <///> coerce (joinPath ns)
  tryPath p _ = finder $ p <///> name

  resolvePath s =
    maybe
      (maybe
        (throwError $ ErrorCall $ "__nixPath must be a list of attr sets with 'path' elements, but received: " <> show s)
        (defer . fetchTarball)
        (M.lookup "uri" s)
      )
      pure
      (M.lookup "path" s)

fetchTarball
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
fetchTarball =
  \case
    NVSet _ s ->
      maybe
        (throwError $ ErrorCall "builtins.fetchTarball: Missing url attribute")
        (go (M.lookup "sha256" s) <=< demand)
        (M.lookup "url" s)
    v@NVStr{} -> go Nothing v
    v -> throwError $ ErrorCall $ "builtins.fetchTarball: Expected URI or set, got " <> show v
  <=< demand
 where
  go :: Maybe (NValue t f m) -> NValue t f m -> m (NValue t f m)
  go msha = \case
    NVStr ns -> fetch (stringIgnoreContext ns) msha
    v -> throwError $ ErrorCall $ "builtins.fetchTarball: Expected URI or string, got " <> show v

{- jww (2018-04-11): This should be written using pipes in another module
    fetch :: Text -> Maybe (NThunk m) -> m (NValue t f m)
    fetch uri msha = case takeExtension uri of
        ".tgz" -> undefined
        ".gz"  -> undefined
        ".bz2" -> undefined
        ".xz"  -> undefined
        ".tar" -> undefined
        ext -> throwError $ ErrorCall $ "builtins.fetchTarball: Unsupported extension '"
                  <> ext <> "'"
-}

  fetch :: Text -> Maybe (NValue t f m) -> m (NValue t f m)
  fetch uri =
    maybe
      (nixInstantiateExpr $
        "builtins.fetchTarball \"" <> uri <> "\""
      )
      (\ v ->
        do
          nv <- demand v
          nsSha <- fromValue nv

          let sha = stringIgnoreContext nsSha

          nixInstantiateExpr $
            "builtins.fetchTarball { " <> "url    = \"" <> uri <> "\"; " <> "sha256 = \"" <> sha <> "\"; }"
      )

defaultFindPath :: MonadNix e t f m => [NValue t f m] -> Path -> m Path
defaultFindPath = findPathM

findPathM
  :: forall e t f m
   . MonadNix e t f m
  => [NValue t f m]
  -> Path
  -> m Path
findPathM = findPathBy existingPath
 where
  existingPath :: MonadEffects t f m => Path -> m (Maybe Path)
  existingPath path =
    do
      apath  <- toAbsolutePath @t @f path
      doesExist <- doesPathExist apath
      pure $ pure apath `whenTrue` doesExist

defaultImportPath
  :: (MonadNix e t f m, MonadState (HashMap Path NExprLoc, b) m)
  => Path
  -> m (NValue t f m)
defaultImportPath path = do
  traceM $ coerce $ "Importing file " <> path
  withFrame
    Info
    (ErrorCall $ "While importing file " <> show path)
    $ do
        imports <- gets fst
        evalExprLoc =<<
          maybe
            (do
              either
                (\ err -> throwError $ ErrorCall . show $ fillSep ["Parse during import failed:", err])
                (\ expr ->
                  do
                    modify $ first $ M.insert path expr
                    pure expr
                )
                =<< parseNixFileLoc path
            )
            pure  -- return expr
            (M.lookup path imports)

defaultPathToDefaultNix :: MonadNix e t f m => Path -> m Path
defaultPathToDefaultNix = pathToDefaultNixFile

-- Given a path, determine the nix file to load
pathToDefaultNixFile :: MonadFile m => Path -> m Path
pathToDefaultNixFile p = do
  isDir <- doesDirectoryExist p
  pure $ coerce $ coerce p </> "default.nix" `whenTrue` isDir

defaultTraceEffect :: MonadPutStr m => String -> m ()
defaultTraceEffect = Nix.Effects.putStrLn
