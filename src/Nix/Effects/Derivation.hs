{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}


module Nix.Effects.Derivation ( defaultDerivationStrict ) where

import           Prelude                 hiding ( readFile )

import           Control.Arrow                  ( first, second )
import           Control.Monad                  ( (>=>), forM, when )
import           Control.Monad.Writer           ( join, lift )
import           Control.Monad.State            ( MonadState, gets, modify )

import           Data.Char                      ( isAscii, isAlphaNum )
import qualified Data.HashMap.Lazy             as M
import qualified Data.HashMap.Strict           as MS
import qualified Data.HashSet                  as S
import           Data.List
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text

import           Nix.Atoms
import           Nix.Convert
import           Nix.Effects
import           Nix.Exec                       ( MonadNix , callFunc)
import           Nix.Frames
import           Nix.Json                       ( nvalueToJSONNixString )
import           Nix.Render
import           Nix.String
import           Nix.String.Coerce
import           Nix.Utils               hiding ( readFile )
import           Nix.Value
import           Nix.Value.Monad

import qualified System.Nix.ReadonlyStore      as Store
import qualified System.Nix.Hash               as Store
import qualified System.Nix.StorePath          as Store

import           Text.Megaparsec
import           Text.Megaparsec.Char


data Derivation = Derivation
  { name :: Text
  , outputs :: Map Text Text
  , inputs :: (Set Text, Map Text [Text])
  , platform :: Text
  , builder :: Text -- should be typed as a store path
  , args :: [ Text ]
  , env :: Map Text Text
  , mFixed :: Maybe Store.SomeNamedDigest
  , hashMode :: HashMode
  , useJson :: Bool
  }
  deriving Show

defaultDerivation :: Derivation
defaultDerivation = Derivation
  { name        = undefined
  , outputs     = Map.empty
  , inputs      = (Set.empty, Map.empty)
  , platform    = undefined
  , builder     = undefined
  , args        = mempty
  , env         = Map.empty
  , mFixed      = Nothing
  , hashMode    = Flat
  , useJson     = False
  }

data HashMode = Flat | Recursive
  deriving (Show, Eq)

makeStorePathName :: (Framed e m) => Text -> m Store.StorePathName
makeStorePathName name = case Store.makeStorePathName name of
  Left err -> throwError $ ErrorCall $ "Invalid name '" <> show name <> "' for use in a store path: " <> err
  Right spname -> pure spname

parsePath :: (Framed e m) => Text -> m Store.StorePath
parsePath p = case Store.parsePath "/nix/store" (Text.encodeUtf8 p) of
  Left err -> throwError $ ErrorCall $ "Cannot parse store path " <> show p <> ":\n" <> show err
  Right path -> pure path

writeDerivation :: (Framed e m, MonadStore m) => Derivation -> m Store.StorePath
writeDerivation drv@Derivation{inputs, name} = do
  let (inputSrcs, inputDrvs) = inputs
  references <- fmap Set.fromList $ traverse parsePath $ Set.toList $ Set.union inputSrcs $ Set.fromList $ Map.keys inputDrvs
  path <- addTextToStore (Text.append name ".drv") (unparseDrv drv) (S.fromList $ Set.toList references) False
  parsePath $ Text.pack $ unStorePath path

-- | Traverse the graph of inputDrvs to replace fixed output derivations with their fixed output hash.
-- this avoids propagating changes to their .drv when the output hash stays the same.
hashDerivationModulo :: (MonadNix e t f m, MonadState (b, MS.HashMap Text Text) m) => Derivation -> m (Store.Digest 'Store.SHA256)
hashDerivationModulo (Derivation {
    mFixed = Just (Store.SomeDigest (digest :: Store.Digest hashType)),
    outputs,
    hashMode
  }) = case Map.toList outputs of
    [("out", path)] -> pure $ Store.hash @'Store.SHA256 $ Text.encodeUtf8
      $  "fixed:out"
      <> (if hashMode == Recursive then ":r" else "")
      <> ":" <> (Store.algoName @hashType)
      <> ":" <> Store.encodeInBase Store.Base16 digest
      <> ":" <> path
    outputsList -> throwError $ ErrorCall $ "This is weird. A fixed output drv should only have one output named 'out'. Got " <> show outputsList
hashDerivationModulo drv@Derivation{inputs = (inputSrcs, inputDrvs)} = do
  cache <- gets snd
  inputsModulo <- Map.fromList <$> forM (Map.toList inputDrvs) (\(path, outs) ->
    case MS.lookup path cache of
      Just hash -> pure (hash, outs)
      Nothing -> do
        drv' <- readDerivation $ Text.unpack path
        hash <- Store.encodeInBase Store.Base16 <$> hashDerivationModulo drv'
        pure (hash, outs)
    )
  pure $ Store.hash @'Store.SHA256 $ Text.encodeUtf8 $ unparseDrv (drv {inputs = (inputSrcs, inputsModulo)})

unparseDrv :: Derivation -> Text
unparseDrv Derivation{..} = Text.append "Derive" $ parens
    [ -- outputs: [("out", "/nix/store/.....-out", "", ""), ...]
      serializeList $ flip fmap (Map.toList outputs) (\(outputName, outputPath) ->
        let prefix = if hashMode == Recursive then "r:" else "" in
        maybe
          (parens [s outputName, s outputPath, s "", s ""])
          (\ (Store.SomeDigest (digest :: Store.Digest hashType)) ->
            parens [s outputName, s outputPath, s $ prefix <> Store.algoName @hashType, s $ Store.encodeInBase Store.Base16 digest]
          )
          mFixed
        )
    , -- inputDrvs
      serializeList $ flip fmap (Map.toList $ snd inputs) (\(path, outs) ->
        parens [s path, serializeList $ fmap s $ sort outs])
    , -- inputSrcs
      serializeList (fmap s $ Set.toList $ fst inputs)
    , s platform
    , s builder
    , -- run script args
      serializeList $ fmap s args
    , -- env (key value pairs)
      serializeList $ flip fmap (Map.toList env) (\(k, v) ->
        parens [s k, s v])
    ]
  where
    parens :: [Text] -> Text
    parens ts = Text.concat ["(", Text.intercalate "," ts, ")"]
    serializeList   :: [Text] -> Text
    serializeList   ls = Text.concat ["[", Text.intercalate "," ls, "]"]
    s = (Text.cons '\"') . (`Text.snoc` '\"') . Text.concatMap escape
    escape :: Char -> Text
    escape '\\' = "\\\\"
    escape '\"' = "\\\""
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c = Text.singleton c

readDerivation :: (Framed e m, MonadFile m) => FilePath -> m Derivation
readDerivation path = do
  content <- Text.decodeUtf8 <$> readFile path
  either
    (\ err -> throwError $ ErrorCall $ "Failed to parse " <> show path <> ":\n" <> show err)
    pure
    (parse derivationParser path content)

derivationParser :: Parsec () Text Derivation
derivationParser = do
  _ <- "Derive("
  fullOutputs <- serializeList $
    fmap (\[n, p, ht, h] -> (n, p, ht, h)) $ parens s
  _ <- ","
  inputDrvs   <- fmap Map.fromList $ serializeList $
    fmap (,) ("(" *> s <* ",") <*> (serializeList s <* ")")
  _ <- ","
  inputSrcs   <- fmap Set.fromList $ serializeList s
  _ <- ","
  platform    <- s
  _ <- ","
  builder     <- s
  _ <- ","
  args        <- serializeList s
  _ <- ","
  env         <- fmap Map.fromList $ serializeList $ fmap (\[a, b] -> (a, b)) $ parens s
  _ <- ")"
  eof

  let outputs = Map.fromList $ fmap (\(a, b, _, _) -> (a, b)) fullOutputs
  let (mFixed, hashMode) = parseFixed fullOutputs
  let name = "" -- FIXME (extract from file path ?)
  let useJson = ["__json"] == Map.keys env

  pure $ Derivation {inputs = (inputSrcs, inputDrvs), ..}
 where
  s :: Parsec () Text Text
  s = fmap Text.pack $ string "\"" *> manyTill (escaped <|> regular) (string "\"")
  escaped = char '\\' *>
    (   '\n' <$ string "n"
    <|> '\r' <$ string "r"
    <|> '\t' <$ string "t"
    <|> anySingle
    )
  regular = noneOf ['\\', '"']

  parens :: Parsec () Text a -> Parsec () Text [a]
  parens p = (string "(") *> sepBy p (string ",") <* (string ")")
  serializeList   p = (string "[") *> sepBy p (string ",") <* (string "]")

  parseFixed :: [(Text, Text, Text, Text)] -> (Maybe Store.SomeNamedDigest, HashMode)
  parseFixed fullOutputs = case fullOutputs of
    [("out", _path, rht, hash)] | rht /= "" && hash /= "" ->
      let (hashType, hashMode) = case Text.splitOn ":" rht of
            ["r", ht] -> (ht, Recursive)
            [ht] ->      (ht, Flat)
            _ -> error $ "Unsupported hash type for output of fixed-output derivation in .drv file: " <> show fullOutputs
      in case Store.mkNamedDigest hashType hash of
        Right digest -> (pure digest, hashMode)
        Left err -> error $ "Unsupported hash " <> show (hashType <> ":" <> hash) <> "in .drv file: " <> err
    _ -> (Nothing, Flat)


defaultDerivationStrict :: forall e t f m b. (MonadNix e t f m, MonadState (b, MS.HashMap Text Text) m) => NValue t f m -> m (NValue t f m)
defaultDerivationStrict = fromValue @(AttrSet (NValue t f m)) >=> \s -> do
    (drv, ctx) <- runWithStringContextT' $ buildDerivationWithContext s
    drvName <- makeStorePathName $ name drv
    let inputs = toStorePaths ctx

    -- Compute the output paths, and add them to the environment if needed.
    -- Also add the inputs, just computed from the strings contexts.
    drv' <- case mFixed drv of
      Just (Store.SomeDigest digest) -> do
        let out = pathToText $ Store.makeFixedOutputPath "/nix/store" (hashMode drv == Recursive) digest drvName
        let env' = if useJson drv then env drv else Map.insert "out" out (env drv)
        pure $ drv { inputs, env = env', outputs = Map.singleton "out" out }

      Nothing -> do
        hash <- hashDerivationModulo $ drv
          { inputs
        --, outputs = Map.map (const "") (outputs drv)  -- not needed, this is already the case
          , env = if useJson drv then env drv
                  else foldl' (\m k -> Map.insert k "" m) (env drv) (Map.keys $ outputs drv)
          }
        outputs' <- sequence $ Map.mapWithKey (\o _ -> makeOutputPath o hash drvName) (outputs drv)
        pure $ drv
          { inputs
          , outputs = outputs'
          , env = if useJson drv then env drv else Map.union outputs' (env drv)
          }

    drvPath <- pathToText <$> writeDerivation drv'

    -- Memoize here, as it may be our last chance in case of readonly stores.
    drvHash <- Store.encodeInBase Store.Base16 <$> hashDerivationModulo drv'
    modify (second (MS.insert drvPath drvHash))

    let outputsWithContext = Map.mapWithKey (\out path -> makeNixStringWithSingletonContext path (StringContext drvPath (DerivationOutput out))) (outputs drv')
        drvPathWithContext = makeNixStringWithSingletonContext drvPath (StringContext drvPath AllOutputs)
        attrSet = M.map nvStr $ M.fromList $ ("drvPath", drvPathWithContext): Map.toList outputsWithContext
    -- TODO: Add location information for all the entries.
    --              here --v
    pure $ nvSet mempty attrSet

  where

    pathToText = Text.decodeUtf8 . Store.storePathToRawFilePath

    makeOutputPath o h n = do
      name <- makeStorePathName (Store.unStorePathName n <> if o == "out" then "" else "-" <> o)
      pure $ pathToText $ Store.makeStorePath "/nix/store" ("output:" <> Text.encodeUtf8 o) h name

    toStorePaths ctx = foldl (flip addToInputs) (Set.empty, Map.empty) ctx
    addToInputs (StringContext path kind) = case kind of
      DirectPath -> first (Set.insert path)
      DerivationOutput o -> second (Map.insertWith (<>) path [o])
      AllOutputs ->
        -- TODO: recursive lookup. See prim_derivationStrict
        -- XXX: When is this really used ?
        error "Not implemented: derivations depending on a .drv file are not yet supported."


-- | Build a derivation in a context collecting string contexts.
-- This is complex from a typing standpoint, but it allows to perform the
-- full computation without worrying too much about all the string's contexts.
buildDerivationWithContext :: forall e t f m. (MonadNix e t f m) => AttrSet (NValue t f m) -> WithStringContextT m Derivation
buildDerivationWithContext drvAttrs = do
    -- Parse name first, so we can add an informative frame
    drvName     <- getAttr   "name"                      $ extractNixString >=> assertDrvStoreName
    withFrame' Info (ErrorCall $ "While evaluating derivation " <> show drvName) $ do

      useJson     <- getAttrOr "__structuredAttrs" False   $ pure
      ignoreNulls <- getAttrOr "__ignoreNulls"     False   $ pure

      args        <- getAttrOr "args"              mempty  $ traverse (fromValue' >=> extractNixString)
      builder     <- getAttr   "builder"                   $ extractNixString
      platform    <- getAttr   "system"                    $ extractNoCtx >=> assertNonNull
      mHash       <- getAttrOr "outputHash"        mempty  $ extractNoCtx >=> (pure . pure)
      hashMode    <- getAttrOr "outputHashMode"    Flat    $ extractNoCtx >=> parseHashMode
      outputs     <- getAttrOr "outputs"           ["out"] $ traverse (fromValue' >=> extractNoCtx)

      mFixedOutput <-
        maybe
          (pure Nothing)
          (\ hash -> do
            when (outputs /= ["out"]) $ lift $ throwError $ ErrorCall $ "Multiple outputs are not supported for fixed-output derivations"
            hashType <- getAttr "outputHashAlgo" extractNoCtx
            digest <- lift $ either (throwError . ErrorCall) pure $ Store.mkNamedDigest hashType hash
            pure $ pure digest)
          mHash

      -- filter out null values if needed.
      attrs <- lift $ if not ignoreNulls
        then pure drvAttrs
        else M.mapMaybe id <$> forM drvAttrs (
            demand >=> pure . \case
              NVConstant NNull -> Nothing
              value            -> Just value
          )

      env <- if useJson
        then do
          jsonString :: NixString <- lift $ nvalueToJSONNixString $ nvSet mempty $
            deleteKeys [ "args", "__ignoreNulls", "__structuredAttrs" ] attrs
          rawString :: Text <- extractNixString jsonString
          pure $ Map.singleton "__json" rawString
        else
          traverse (lift . coerceToString callFunc CopyToStore CoerceAny >=> extractNixString) $
            Map.fromList $ M.toList $ deleteKeys [ "args", "__ignoreNulls" ] attrs

      pure $ defaultDerivation { platform, builder, args, env,  hashMode, useJson
        , name = drvName
        , outputs = Map.fromList $ fmap (, mempty) outputs
        , mFixed = mFixedOutput
        }
  where

    -- common functions, lifted to WithStringContextT

    fromValue' :: (FromValue a m (NValue' t f m (NValue t f m)), MonadNix e t f m) => NValue t f m -> WithStringContextT m a
    fromValue' = lift . fromValue

    withFrame' :: (Framed e m, Exception s) => NixLevel -> s -> WithStringContextT m a -> WithStringContextT m a
    withFrame' level f = join . lift . withFrame level f . pure

    -- shortcuts to get the (forced) value of an AttrSet field

    getAttrOr' :: forall v a. (MonadNix e t f m, FromValue v m (NValue' t f m (NValue t f m)))
      => Text -> m a -> (v -> WithStringContextT m a) -> WithStringContextT m a
    getAttrOr' n d f = case M.lookup n drvAttrs of
      Nothing -> lift d
      Just v  -> withFrame' Info (ErrorCall $ "While evaluating attribute '" <> show n <> "'") $
                   fromValue' v >>= f

    getAttrOr n d f = getAttrOr' n (pure d) f

    getAttr n = getAttrOr' n (throwError $ ErrorCall $ "Required attribute '" <> show n <> "' not found.")

    -- Test validity for fields

    assertDrvStoreName :: MonadNix e t f m => Text -> WithStringContextT m Text
    assertDrvStoreName name = lift $ do
      let invalid c = not $ isAscii c && (isAlphaNum c || c `elem` ("+-._?=" :: String)) -- isAlphaNum allows non-ascii chars.
      let failWith reason = throwError $ ErrorCall $ "Store name " <> show name <> " " <> reason
      when ("." `Text.isPrefixOf` name)    $ failWith "cannot start with a period"
      when (Text.length name > 211)        $ failWith "must be no longer than 211 characters"
      when (Text.any invalid name)         $ failWith "contains some invalid character"
      when (".drv" `Text.isSuffixOf` name) $ failWith "is not allowed to end in '.drv'"
      pure name

    extractNoCtx :: MonadNix e t f m => NixString -> WithStringContextT m Text
    extractNoCtx ns =
      maybe
        (lift $ throwError $ ErrorCall $ "The string " <> show ns <> " is not allowed to have a context.")
        pure
        (getStringNoContext ns)

    assertNonNull :: MonadNix e t f m => Text -> WithStringContextT m Text
    assertNonNull t = do
      when (Text.null t) $ lift $ throwError $ ErrorCall "Value must not be empty"
      pure t

    parseHashMode :: MonadNix e t f m => Text -> WithStringContextT m HashMode
    parseHashMode = \case
      "flat" ->      pure Flat
      "recursive" -> pure Recursive
      other -> lift $ throwError $ ErrorCall $ "Hash mode " <> show other <> " is not valid. It must be either 'flat' or 'recursive'"

    -- Other helpers

    deleteKeys :: [Text] -> AttrSet a -> AttrSet a
    deleteKeys keys attrSet = foldl' (flip M.delete) attrSet keys

