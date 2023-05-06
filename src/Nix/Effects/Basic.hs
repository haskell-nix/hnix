{-# language CPP #-}

module Nix.Effects.Basic where

import           Nix.Prelude             hiding ( head
                                                )
import           Relude.Unsafe                  ( head )
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Control.Monad                  ( foldM )
import qualified Data.HashMap.Lazy             as M
import           Data.List.Split                ( splitOn )
import qualified Data.Text                     as Text
import           Prettyprinter                  ( fillSep )
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




defaultToAbsolutePath :: forall e t f m . MonadNix e t f m => Path -> m Path
defaultToAbsolutePath origPath =
  do
    origPathExpanded <- expandHomePath origPath
    fmap
      removeDotDotIndirections
      . canonicalizePath
        =<< bool
            (fmap
              (<///> origPathExpanded)
              $ maybe
                  getCurrentDirectory
                  ( (\case
                      NVPath s -> pure $ takeDirectory s
                      val -> throwError $ ErrorCall $ "when resolving relative path, __cur_file is in scope, but is not a path; it is: " <> show val
                    ) <=< demand
                  )
                  =<< lookupVar Unknown "__cur_file"
            )
            (pure origPathExpanded)
            (isAbsolute origPathExpanded)

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
x <///> y
  | isAbsolute y || "." `isPrefixOf` coerce y = x </> y
  | otherwise                          = joinByLargestOverlap x y
 where
  joinByLargestOverlap :: Path -> Path -> Path
  joinByLargestOverlap (splitDirectories -> xs) (splitDirectories -> ys) =
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
        l <- fromValue @[NValue t f m] =<< demand v
        findPathBy nixFilePath l name
    )
    =<< lookupVar Unknown "__nixPath"

 where
  nixFilePath :: MonadEffects t f m => Path -> m (Maybe Path)
  nixFilePath path =
    do
      absPath <- toAbsolutePath @t @f path
      isDir   <- doesDirectoryExist absPath
      absFile <-
        bool
          (pure absPath)
          (toAbsolutePath @t @f $ absPath </> "default.nix")
          isDir

      (pure absFile `whenTrue`) <$> doesFileExist absFile

findPathBy
  :: forall e t f m
   . MonadNix e t f m
  => (Path -> m (Maybe Path))
  -> [NValue t f m]
  -> Path
  -> m Path
findPathBy finder ls name =
  maybe
    (throwError $ ErrorCall $ "file ''" <> coerce name <> "'' was not found in the Nix search path (add it's using $NIX_PATH or -I)")
    pure
    =<< foldM fun mempty ls
 where
  fun
    :: MonadNix e t f m
    => Maybe Path
    -> NValue t f m
    -> m (Maybe Path)
  fun =
    maybe
      (\ nv ->
        do
          (s :: HashMap VarName (NValue t f m)) <- fromValue =<< demand nv
          p <- resolvePath s
          path <- fromValue =<< demand p

          maybe
            (tryPath path mempty)
            (\ nv' ->
              do
                mns <- fromValueMay @NixString =<< demand nv'
                tryPath path $
                  whenJust
                    (\ nsPfx ->
                      let pfx = ignoreContext nsPfx in
                      pure $ coerce $ toString pfx `whenFalse` Text.null pfx
                    )
                    mns
            )
            (M.lookup "prefix" s)
      )
      (const . pure . pure)

  tryPath :: Path -> Maybe Path -> m (Maybe Path)
  tryPath p (Just n) | n' : ns <- splitDirectories name, n == n' =
    finder $ p <///> joinPath ns
  tryPath p _ = finder $ p <///> name

  resolvePath :: HashMap VarName (NValue t f m) -> m (NValue t f m)
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
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> m (NValue t f m)
fetchTarball =
  \case
    NVSet _ s ->
      maybe
        (throwError $ ErrorCall "builtins.fetchTarball: Missing url attribute")
        (fetchFromString (M.lookup "sha256" s) <=< demand)
        (M.lookup "url" s)
    v@NVStr{} -> fetchFromString Nothing v
    v -> throwError $ ErrorCall $ "builtins.fetchTarball: Expected URI or set, got " <> show v
  <=< demand
 where
  fetchFromString
    :: Maybe (NValue t f m)
    -> NValue t f m
    -> m (NValue t f m)
  fetchFromString msha =
    \case
      NVStr ns -> fetch (ignoreContext ns) msha
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
          nsSha <- fromValue =<< demand v

          let sha = ignoreContext nsSha

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
defaultImportPath path =
  do
    traceM $ "Importing file " <> coerce path
    withFrame
      Info
      (ErrorCall $ "While importing file " <> show path)
      $ evalExprLoc =<<
          (maybe
            (either
              (\ err -> throwError $ ErrorCall . show $ fillSep ["Parse during import failed:", err])
              (\ expr ->
                do
                  modify $ first $ M.insert path expr
                  pure expr
              )
              =<< parseNixFileLoc path
            )
            pure  -- return expr
            . M.lookup path
          ) =<< gets fst

defaultPathToDefaultNix :: MonadNix e t f m => Path -> m Path
defaultPathToDefaultNix = pathToDefaultNixFile

-- Given a path, determine the nix file to load
pathToDefaultNixFile :: MonadFile m => Path -> m Path
pathToDefaultNixFile p =
  do
    isDir <- doesDirectoryExist p
    pure $ p </> "default.nix" `whenTrue` isDir

defaultTraceEffect :: MonadPutStr m => String -> m ()
defaultTraceEffect = Nix.Effects.putStrLn
