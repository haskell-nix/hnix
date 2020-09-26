{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Effects.Basic where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.HashMap.Lazy              ( HashMap )
import qualified Data.HashMap.Lazy             as M
import           Data.List
import           Data.List.Split
import           Data.Maybe                     ( maybeToList )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Nix.Atoms
import           Nix.Convert
import           Nix.Effects
import           Nix.Exec                       ( MonadNix
                                                , callFunc
                                                , evalExprLoc
                                                , nixInstantiateExpr
                                                )
import           Nix.Expr
import           Nix.Frames
import           Nix.Normal
import           Nix.Parser
import           Nix.Pretty
import           Nix.Render
import           Nix.Scope
import           Nix.String
import           Nix.String.Coerce
import           Nix.Utils
import           Nix.Value
import           Nix.Value.Monad
import           Prettyprinter
import           System.FilePath

#ifdef MIN_VERSION_ghc_datasize
#if MIN_VERSION_ghc_datasize(0,2,0)
import           GHC.DataSize
#endif
#endif

defaultMakeAbsolutePath :: MonadNix e t f m => FilePath -> m FilePath
defaultMakeAbsolutePath origPath = do
  origPathExpanded <- expandHomePath origPath
  absPath          <- if isAbsolute origPathExpanded
    then pure origPathExpanded
    else do
      cwd <- do
        mres <- lookupVar "__cur_file"
        case mres of
          Nothing -> getCurrentDirectory
          Just v  -> demand v $ \case
            NVPath s -> pure $ takeDirectory s
            v ->
              throwError
                $  ErrorCall
                $  "when resolving relative path,"
                ++ " __cur_file is in scope,"
                ++ " but is not a path; it is: "
                ++ show v
      pure $ cwd <///> origPathExpanded
  removeDotDotIndirections <$> canonicalizePath absPath

expandHomePath :: MonadFile m => FilePath -> m FilePath
expandHomePath ('~' : xs) = flip (++) xs <$> getHomeDirectory
expandHomePath p          = pure p

-- | Incorrectly normalize paths by rewriting patterns like @a/b/..@ to @a@.
--   This is incorrect on POSIX systems, because if @b@ is a symlink, its
--   parent may be a different directory from @a@. See the discussion at
--   https://hackage.haskell.org/package/directory-1.3.1.5/docs/System-Directory.html#v:canonicalizePath
removeDotDotIndirections :: FilePath -> FilePath
removeDotDotIndirections = intercalate "/" . go [] . splitOn "/"
 where
  go s       []            = reverse s
  go (_ : s) (".." : rest) = go s rest
  go s       (this : rest) = go (this : s) rest

infixr 9 <///>
(<///>) :: FilePath -> FilePath -> FilePath
x <///> y | isAbsolute y || "." `isPrefixOf` y = x </> y
          | otherwise                          = joinByLargestOverlap x y
 where
  joinByLargestOverlap (splitDirectories -> xs) (splitDirectories -> ys) =
    joinPath $ head
      [ xs ++ drop (length tx) ys | tx <- tails xs, tx `elem` inits ys ]

defaultFindEnvPath :: MonadNix e t f m => String -> m FilePath
defaultFindEnvPath = findEnvPathM

findEnvPathM :: forall e t f m . MonadNix e t f m => FilePath -> m FilePath
findEnvPathM name = do
  mres <- lookupVar "__nixPath"
  case mres of
    Nothing -> error "impossible"
    Just x  -> demand x $ fromValue >=> \(l :: [NValue t f m]) ->
      findPathBy nixFilePath l name
 where
  nixFilePath :: MonadEffects t f m => FilePath -> m (Maybe FilePath)
  nixFilePath path = do
    path   <- makeAbsolutePath @t @f path
    exists <- doesDirectoryExist path
    path'  <- if exists
      then makeAbsolutePath @t @f $ path </> "default.nix"
      else pure path
    exists <- doesFileExist path'
    pure $ if exists then Just path' else Nothing

findPathBy
  :: forall e t f m
   . MonadNix e t f m
  => (FilePath -> m (Maybe FilePath))
  -> [NValue t f m]
  -> FilePath
  -> m FilePath
findPathBy finder l name = do
  mpath <- foldM go Nothing l
  case mpath of
    Nothing ->
      throwError
        $  ErrorCall
        $  "file '"
        ++ name
        ++ "' was not found in the Nix search path"
        ++ " (add it's using $NIX_PATH or -I)"
    Just path -> pure path
 where
  go :: Maybe FilePath -> NValue t f m -> m (Maybe FilePath)
  go p@(Just _) _ = pure p
  go Nothing l =
    demand l $ fromValue >=> \(s :: HashMap Text (NValue t f m)) -> do
      p <- resolvePath s
      demand p $ fromValue >=> \(Path path) -> case M.lookup "prefix" s of
        Nothing -> tryPath path Nothing
        Just pf -> demand pf $ fromValueMay >=> \case
          Just (nsPfx :: NixString) ->
            let pfx = hackyStringIgnoreContext nsPfx
            in  if not (Text.null pfx)
                  then tryPath path (Just (Text.unpack pfx))
                  else tryPath path Nothing
          _ -> tryPath path Nothing

  tryPath p (Just n) | n' : ns <- splitDirectories name, n == n' =
    finder $ p <///> joinPath ns
  tryPath p _ = finder $ p <///> name

  resolvePath s = case M.lookup "path" s of
    Just t  -> pure t
    Nothing -> case M.lookup "uri" s of
      Just ut -> defer $ fetchTarball ut
      Nothing ->
        throwError
          $  ErrorCall
          $  "__nixPath must be a list of attr sets"
          ++ " with 'path' elements, but received: "
          ++ show s

fetchTarball
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
fetchTarball = flip demand $ \case
  NVSet s _ -> case M.lookup "url" s of
    Nothing ->
      throwError $ ErrorCall "builtins.fetchTarball: Missing url attribute"
    Just url -> demand url $ go (M.lookup "sha256" s)
  v@NVStr{} -> go Nothing v
  v ->
    throwError
      $  ErrorCall
      $  "builtins.fetchTarball: Expected URI or set, got "
      ++ show v
 where
  go :: Maybe (NValue t f m) -> NValue t f m -> m (NValue t f m)
  go msha = \case
    NVStr ns -> fetch (hackyStringIgnoreContext ns) msha
    v ->
      throwError
        $  ErrorCall
        $  "builtins.fetchTarball: Expected URI or string, got "
        ++ show v

{- jww (2018-04-11): This should be written using pipes in another module
    fetch :: Text -> Maybe (NThunk m) -> m (NValue t f m)
    fetch uri msha = case takeExtension (Text.unpack uri) of
        ".tgz" -> undefined
        ".gz"  -> undefined
        ".bz2" -> undefined
        ".xz"  -> undefined
        ".tar" -> undefined
        ext -> throwError $ ErrorCall $ "builtins.fetchTarball: Unsupported extension '"
                  ++ ext ++ "'"
-}

  fetch :: Text -> Maybe (NValue t f m) -> m (NValue t f m)
  fetch uri Nothing =
    nixInstantiateExpr $ "builtins.fetchTarball \"" ++ Text.unpack uri ++ "\""
  fetch url (Just t) = demand t $ fromValue >=> \nsSha ->
    let sha = hackyStringIgnoreContext nsSha
    in  nixInstantiateExpr
          $  "builtins.fetchTarball { "
          ++ "url    = \""
          ++ Text.unpack url
          ++ "\"; "
          ++ "sha256 = \""
          ++ Text.unpack sha
          ++ "\"; }"

defaultFindPath :: MonadNix e t f m => [NValue t f m] -> FilePath -> m FilePath
defaultFindPath = findPathM

findPathM
  :: forall e t f m
   . MonadNix e t f m
  => [NValue t f m]
  -> FilePath
  -> m FilePath
findPathM = findPathBy path
 where
  path :: MonadEffects t f m => FilePath -> m (Maybe FilePath)
  path path = do
    path   <- makeAbsolutePath @t @f path
    exists <- doesPathExist path
    pure $ if exists then Just path else Nothing

defaultImportPath
  :: (MonadNix e t f m, MonadState (HashMap FilePath NExprLoc) m)
  => FilePath
  -> m (NValue t f m)
defaultImportPath path = do
  traceM $ "Importing file " ++ path
  withFrame Info (ErrorCall $ "While importing file " ++ show path) $ do
    imports <- get
    evalExprLoc =<< case M.lookup path imports of
      Just expr -> pure expr
      Nothing   -> do
        eres <- parseNixFileLoc path
        case eres of
          Failure err ->
            throwError
              $ ErrorCall
              . show $ fillSep ["Parse during import failed:", err]
          Success expr -> do
            modify (M.insert path expr)
            pure expr

defaultPathToDefaultNix :: MonadNix e t f m => FilePath -> m FilePath
defaultPathToDefaultNix = pathToDefaultNixFile

-- Given a path, determine the nix file to load
pathToDefaultNixFile :: MonadFile m => FilePath -> m FilePath
pathToDefaultNixFile p = do
  isDir <- doesDirectoryExist p
  pure $ if isDir then p </> "default.nix" else p

defaultDerivationStrict
  :: forall e t f m . MonadNix e t f m => NValue t f m -> m (NValue t f m)
defaultDerivationStrict = fromValue @(AttrSet (NValue t f m)) >=> \s -> do
  nn <- maybe (pure False) (demand ?? fromValue) (M.lookup "__ignoreNulls" s)
  s' <- M.fromList <$> mapMaybeM (handleEntry nn) (M.toList s)
  v' <- normalForm =<< toValue @(AttrSet (NValue t f m)) @_ @(NValue t f m) s'
  nixInstantiateExpr $ "derivationStrict " ++ show (prettyNValue v')
 where
  mapMaybeM :: (a -> m (Maybe b)) -> [a] -> m [b]
  mapMaybeM op = foldr f (pure [])
    where f x xs = op x >>= (<$> xs) . (++) . maybeToList

  handleEntry :: Bool -> (Text, NValue t f m) -> m (Maybe (Text, NValue t f m))
  handleEntry ignoreNulls (k, v) = fmap (k, ) <$> case k of
      -- The `args' attribute is special: it supplies the command-line
      -- arguments to the builder.
      -- TODO This use of coerceToString is probably not right and may
      -- not have the right arguments.
    "args"          -> demand v $ fmap Just . coerceNixList
    "__ignoreNulls" -> pure Nothing
    _               -> demand v $ \case
      NVConstant NNull | ignoreNulls -> pure Nothing
      v'                             -> Just <$> coerceNix v'
   where
    coerceNix :: NValue t f m -> m (NValue t f m)
    coerceNix = toValue <=< coerceToString callFunc CopyToStore CoerceAny

    coerceNixList :: NValue t f m -> m (NValue t f m)
    coerceNixList v = do
      xs <- fromValue @[NValue t f m] v
      ys <- traverse (`demand` coerceNix) xs
      toValue @[NValue t f m] ys

defaultTraceEffect :: MonadPutStr m => String -> m ()
defaultTraceEffect = Nix.Effects.putStrLn
