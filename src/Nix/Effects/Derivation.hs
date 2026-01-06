{-# language CPP #-}
{-# language DataKinds #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language PackageImports #-} -- 2021-07-05: Due to hashing Haskell IT system situation, in HNix we currently ended-up with 2 hash package dependencies @{hashing, cryptonite}@
{-# language TypeApplications #-}
{-# language ExistentialQuantification #-}

module Nix.Effects.Derivation ( defaultDerivationStrict ) where

import           Nix.Prelude             hiding ( readFile )
import           Data.ByteArray                 ( convert )
import           Data.ByteArray.Encoding        ( Base(Base16), convertToBase )
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Data.Char                      ( isAscii
                                                , isAlphaNum
                                                )
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import           Data.Foldable                  ( foldl )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text

import           Text.Megaparsec
import           Text.Megaparsec.Char

import qualified "cryptonite" Crypto.Hash      as Hash -- 2021-07-05: Attrocity of Haskell hashing situation, in HNix we ended-up with 2 hash package dependencies @{hashing, cryptonite}@
import qualified System.Nix.Base32             as Base32

import           Nix.Atoms
import           Nix.Expr.Types          hiding ( Recursive )
import           Nix.Convert
import           Nix.Effects
import           Nix.Exec                       ( MonadNix
                                                , callFunc
                                                )
import           Nix.Options
import           Nix.Frames
import           Nix.Json                       ( toJSONNixString )
import           Nix.Render
import           Nix.String
import           Nix.String.Coerce
import           Nix.Value
import           Nix.Value.Monad

import qualified System.Nix.Hash               as Store
import qualified System.Nix.StorePath          as Store
import qualified "crypton" Crypto.Hash         as CryptonHash
import           Data.Dependent.Sum             ( DSum(..) )
import qualified Data.Dependent.Sum            as DSum

-- Type for fixed-output derivation hash digest
-- Digest comes from crypton (Crypto.Hash.Digest)
type HashDigest = DSum Store.HashAlgo CryptonHash.Digest

-- Helper functions to work with HashDigest
hashDigestAlgoText :: HashDigest -> Text
hashDigestAlgoText (hashAlgo DSum.:=> _) = Store.algoToText hashAlgo

hashDigestText :: HashDigest -> Text
hashDigestText (_ DSum.:=> hashDigest) = Store.encodeDigestWith Store.Base16 hashDigest

--  2021-07-17: NOTE: Derivation consists of @"keys"@ @"vals"@ (of text), so underlining type boundary currently stops here.
data Derivation = Derivation
  { name :: Text
  , outputs :: Map Text Text
  , inputs :: (Set Text, Map Text [Text])
  , platform :: Text
  , builder :: Text -- should be typed as a store path
  , args :: [ Text ]
  , env :: Map Text Text
  , mFixed :: Maybe HashDigest  -- Opaque type for hash digest
  , hashMode :: HashMode
  , useJson :: Bool
  }
  deriving Show

data HashMode = Flat | Recursive
  deriving (Show, Eq)

makeStorePathName :: (Framed e m) => Text -> m Store.StorePathName
makeStorePathName name = case Store.mkStorePathName name of
  Left err -> throwError $ ErrorCall $ "Invalid name '" <> show name <> "' for use in a store path: " <> show err
  Right spname -> pure spname

storeDirFromOptions :: forall e m. (MonadReader e m, Has e Options) => m Store.StoreDir
storeDirFromOptions = do
  opts <- askOptions
  pure $ Store.StoreDir $ encodeUtf8 $ toText $ getStoreDir opts

parsePath :: (Framed e m) => Store.StoreDir -> Text -> m Store.StorePath
parsePath storeDir p = case Store.parsePath storeDir (encodeUtf8 p) of
  Left err -> throwError $ ErrorCall $ "Cannot parse store path " <> show p <> ":\n" <> show err
  Right path -> pure path

writeDerivation :: (Framed e m, MonadStore m, MonadReader e m, Has e Options) => Derivation -> m Store.StorePath
writeDerivation drv@Derivation{inputs, name} = do
  storeDir <- storeDirFromOptions
  let (inputSrcs, inputDrvs) = inputs
  referencePaths <- traverse (parsePath storeDir) (Set.toList $ inputSrcs <> Map.keysSet inputDrvs)
  let references = HS.fromList $ fmap (StorePath . fromString . decodeUtf8 . Store.storePathToRawFilePath storeDir) referencePaths
  path <- addTextToStore (Text.append name ".drv") (unparseDrv drv) references False
  parsePath storeDir $ fromString $ coerce path

-- | Traverse the graph of inputDrvs to replace fixed output derivations with their fixed output hash.
-- this avoids propagating changes to their .drv when the output hash stays the same.
hashDerivationModulo :: (MonadNix e t f m, MonadState (b, KeyMap Text) m) => Derivation -> m (Hash.Digest Hash.SHA256)
hashDerivationModulo
  Derivation
    { mFixed = Just digest
    , outputs
    , hashMode
    , name
    } =
  do
    -- For fixed-output derivations, hash a special string encoding the content address
    -- Format: "fixed:out:<hashMode>:<hashAlgo>:<hash>"
    -- This allows multiple derivations to share the same hash if they produce the same output
    let algoText = hashDigestAlgoText digest
    let hashText = hashDigestText digest
    let modePrefix = case hashMode of
          Recursive -> "r:"
          Flat -> mempty
    let toHash = "fixed:out:" <> modePrefix <> algoText <> ":" <> hashText
    pure $ Hash.hash @ByteString @Hash.SHA256 $ encodeUtf8 toHash
hashDerivationModulo
  drv@Derivation
    { inputs = ( inputSrcs
               , inputDrvs
               )
    } =
  do
    cache <- gets snd
    inputsModulo <-
      Map.fromList <$>
        traverse
          (\(path, outs) ->
            maybe
              (do
                drv' <- readDerivation $ coerce $ toString path
                digestValue <- hashDerivationModulo drv'
                -- Nix uses base16 when substituting derivation hashes in .drv serialization
                let hashBytes = convert digestValue :: ByteString
                let hash = decodeUtf8 (convertToBase Base16 hashBytes :: ByteString)
                pure (hash, outs)
              )
              (\ hash -> pure (hash, outs))
              (HM.lookup path cache)
          )
          (Map.toList inputDrvs)
    pure $ Hash.hash @ByteString @Hash.SHA256 $ encodeUtf8 $ unparseDrv $ drv {inputs = (inputSrcs, inputsModulo)}

unparseDrv :: Derivation -> Text
unparseDrv Derivation{..} =
  Text.append
    "Derive"
    $ parens
      [ -- outputs: [("out", "/nix/store/.....-out", "", ""), ...]
        serializeList $
          produceOutputInfo <$> Map.toList outputs
      , -- inputDrvs
        serializeList $
          (\(path, outs) ->
            parens [s path, serializeList $ s <$> sort outs]
          ) <$> Map.toList (snd inputs)
      , -- inputSrcs
        serializeList $ s <$> Set.toList (fst inputs)
      , s platform
      , s builder
      , -- run script args
        serializeList $ s <$> args
      , -- env (key value pairs)
        serializeList $ (\(k, v) -> parens [s k, s v]) <$> Map.toList env
      ]
  where
    produceOutputInfo (outputName, outputPath) =
      parens $ (s <$>) $ ([outputName, outputPath] <>) $
        maybe
          [mempty, mempty]
          (\digest ->
            let algoText = hashDigestAlgoText digest
                hashText = hashDigestText digest
                modePrefix = if hashMode == Recursive then "r:" else mempty
                hashType = modePrefix <> algoText
            in [hashType, hashText]
          )
          mFixed
    parens :: [Text] -> Text
    parens ts = Text.concat ["(", Text.intercalate "," ts, ")"]

    serializeList   :: [Text] -> Text
    serializeList   ls = Text.concat ["[", Text.intercalate "," ls, "]"]

    s = Text.cons '\"' . (`Text.snoc` '\"') . Text.concatMap escape

    escape :: Char -> Text
    escape '\\' = "\\\\"
    escape '\"' = "\\\""
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c = one c

readDerivation :: (Framed e m, MonadFile m) => Path -> m Derivation
readDerivation path = do
  content <- readFile path
  either
    (\ err -> throwError $ ErrorCall $ "Failed to parse " <> show path <> ":\n" <> show err)
    pure
    (parse derivationParser (coerce path) content)

derivationParser :: Parsec () Text Derivation
derivationParser = do
  _ <- "Derive("
  fullOutputs <- serializeList $
    (\[n, p, ht, h] -> (n, p, ht, h)) <$> parens s
  _ <- ","
  inputDrvs   <- Map.fromList <$> serializeList
    (liftA2 (,) ("(" *> s <* ",") (serializeList s <* ")"))
  _ <- ","
  inputSrcs   <- Set.fromList <$> serializeList s
  _ <- ","
  platform    <- s
  _ <- ","
  builder     <- s
  _ <- ","
  args        <- serializeList s
  _ <- ","
  env         <- fmap Map.fromList $ serializeList $ (\[a, b] -> (a, b)) <$> parens s
  _ <- ")"
  eof

  let outputs = Map.fromList $ (\(a, b, _, _) -> (a, b)) <$> fullOutputs
  let (mFixed, hashMode) = parseFixed fullOutputs
  let name = mempty -- FIXME (extract from file path ?)
  let useJson = one "__json" == Map.keys env

  pure $ Derivation {inputs = (inputSrcs, inputDrvs), ..}
 where
  s :: Parsec () Text Text
  s = fmap fromString $ string "\"" *> manyTill (escaped <|> regular) (string "\"")
  escaped = char '\\' *>
    (   '\n' <$ string "n"
    <|> '\r' <$ string "r"
    <|> '\t' <$ string "t"
    <|> anySingle
    )
  regular = noneOf ['\\', '"']

  wrap o c p =
    string o *> sepBy p (string ",") <* string c

  parens :: Parsec () Text a -> Parsec () Text [a]
  parens = wrap "(" ")"
  serializeList :: Parsec () Text a -> Parsec () Text [a]
  serializeList = wrap "[" "]"

  parseFixed :: [(Text, Text, Text, Text)] -> (Maybe HashDigest, HashMode)
  parseFixed fullOutputs = case fullOutputs of
    [("out", _path, rht, hash)] | rht /= mempty && hash /= mempty ->
      let
        (hashType, hashMode) = case Text.splitOn ":" rht of
          ["r", ht] -> (ht, Recursive)
          [ht] ->      (ht, Flat)
          _ -> error $ "Unsupported hash type for output of fixed-output derivation in .drv file: " <> show fullOutputs
      in
        case Store.mkNamedDigest hashType hash of
          Left err -> error $ show $ "Unsupported hash " <> show (hashType <> ":" <> hash) <> " in .drv file: " <> err
          Right digest -> (Just digest, hashMode)
    _ -> (Nothing, Flat)


defaultDerivationStrict :: forall e t f m b. (MonadNix e t f m, MonadState (b, KeyMap Text) m) => NValue t f m -> m (NValue t f m)
defaultDerivationStrict val = do
    s <- HM.mapKeys varNameText <$> fromValue @(AttrSet (NValue t f m)) val
    (drv, ctx) <- runWithStringContextT' $ buildDerivationWithContext s
    drvName <- makeStorePathName $ name drv
    storeDir <- storeDirFromOptions
    let
      inputs = toStorePaths ctx
      modEnv f = f (env drv)

    -- Compute the output paths, and add them to the environment if needed.
    -- Also add the inputs, just computed from the strings contexts.
    drv' <- case mFixed drv of
      Just digest -> do
        -- Fixed-output derivation: output path is content-addressed
        outputPath <- makeFixedOutputPath storeDir drvName digest (hashMode drv)
        let outputs' = one ("out", outputPath)
        pure $ drv
          { inputs
          , outputs = outputs'
          , env = modEnv (outputs' <>)
          }

      Nothing -> do
        hash <- hashDerivationModulo $ drv
          { inputs
        --, outputs = Map.map (const "") (outputs drv)  -- not needed, this is already the case
          , env =
              modEnv
                (\ baseEnv ->
                  foldl'
                    (\m k -> Map.insert k mempty m)
                    baseEnv
                    (Map.keys $ outputs drv)
                )
          }
        outputs' <- sequenceA $ Map.mapWithKey (\o _ -> makeOutputPath storeDir o hash drvName) $ outputs drv
        pure $ drv
          { inputs
          , outputs = outputs'
          , env = modEnv (outputs' <>)
          }

    (mkVarName -> drvPath) <- pathToText storeDir <$> writeDerivation drv'

    -- Memoize here, as it may be our last chance in case of readonly stores.
    digestValue <- hashDerivationModulo drv'
    -- Nix uses base16 for derivation hashes
    let drvHashBytes = convert digestValue :: ByteString
    let drvHash = decodeUtf8 (convertToBase Base16 drvHashBytes :: ByteString)
    modify $ second $ HM.insert (varNameText drvPath) drvHash

    let
      outputsWithContext =
        Map.mapWithKey
          (\out (mkVarName -> path) -> mkNixStringWithSingletonContext (StringContext (DerivationOutput out) drvPath) path)
          (outputs drv')
      drvPathWithContext = mkNixStringWithSingletonContext (StringContext AllOutputs drvPath) drvPath
      attrSet = NVStr <$> HM.insert "drvPath" drvPathWithContext (Map.foldrWithKey HM.insert HM.empty outputsWithContext)
    -- TODO: Add location information for all the entries.
    --              here --v
    pure $ NVSet mempty $ HM.mapKeys mkVarName attrSet

  where

    pathToText storeDir' = decodeUtf8 . Store.storePathToRawFilePath storeDir'

    storeDirTextFrom :: Store.StoreDir -> Text
    storeDirTextFrom = Text.dropWhileEnd (== '/') . decodeUtf8 . Store.unStoreDir

    makeFixedOutputPath :: (Framed e m) => Store.StoreDir -> Store.StorePathName -> HashDigest -> HashMode -> m Text
    makeFixedOutputPath storeDir' name digest@(hashAlgo DSum.:=> hashDigest) mode = do
      -- Nix's makeFixedOutputPath has TWO cases:
      --
      -- Case 1: SHA256 + Recursive (NixArchive)
      --   This is the common case for fetchurl, addToStore, etc.
      --   Uses: makeStorePath("source", hash, name) directly
      --   Format: "source:<hash_hex>:<storeDir>:<name>"
      --
      -- Case 2: Everything else (flat hash, or non-SHA256)
      --   Uses a two-step process:
      --   Step 1: digest1 = SHA256("fixed:out:<modePrefix><algo>:<hash>:")
      --   Step 2: makeStorePath("output:out", digest1, name)
      --
      let storeDirText = storeDirTextFrom storeDir'
      let storeDirPrefix = storeDirText <> "/"
      let hashText = hashDigestText digest

      case (hashAlgo, mode) of
        (Store.HashAlgo_SHA256, Recursive) -> do
          -- Case 1: SHA256 + Recursive -> use "source" type directly
          -- Format: "source:<hash_hex>:<storeDir>:<name>"
          let toHash = "source:" <> hashText <> ":" <> storeDirText <> ":" <> Store.unStorePathName name
          let hashPart = Store.mkStorePathHashPart @CryptonHash.SHA256 (encodeUtf8 toHash)
          let hashBase32 = Base32.encode $ Store.unStorePathHashPart hashPart
          pure $ storeDirPrefix <> hashBase32 <> "-" <> Store.unStorePathName name

        _ -> do
          -- Case 2: Other cases -> two-step "fixed:out:" process
          let algoText = hashDigestAlgoText digest
          let modePrefix = case mode of
                Recursive -> "r:"
                Flat -> mempty
          -- Step 1: Create the fixed-output content hash (no storeDir, no name, trailing colon)
          -- Format: "fixed:out:<modePrefix><algo>:<hash>:"
          let fixedPayload = "fixed:out:" <> modePrefix <> algoText <> ":" <> hashText <> ":"
          let fixedDigest = CryptonHash.hash @ByteString @CryptonHash.SHA256 $ encodeUtf8 fixedPayload
          let fixedDigestHex = decodeUtf8 (convertToBase Base16 (convert fixedDigest :: ByteString) :: ByteString)
          -- Step 2: makeStorePath with the intermediate digest
          -- Nix's makeStorePath(type, hash, name) uses hash.to_string(Base16, true) which includes "sha256:"
          -- Format: "output:out:sha256:<digest1_hex>:<storeDir>:<name>"
          let toHash = "output:out:sha256:" <> fixedDigestHex <> ":" <> storeDirText <> ":" <> Store.unStorePathName name
          let hashPart = Store.mkStorePathHashPart @CryptonHash.SHA256 (encodeUtf8 toHash)
          let hashBase32 = Base32.encode $ Store.unStorePathHashPart hashPart
          let result = storeDirPrefix <> hashBase32 <> "-" <> Store.unStorePathName name
          pure result

    makeOutputPath storeDir' o h n = do
      name <- makeStorePathName $ Store.unStorePathName n <> if o == "out" then mempty else "-" <> o
      -- Compute the output path hash according to Nix's algorithm:
      -- For non-fixed outputs, hash = sha256("output:<outputName>:sha256:<drv_modulo_hex>:<storeDir>:<outputPathName>")
      -- where outputPathName is the base name for "out", or base name + "-" + output name for other outputs
      -- Convert the hash digest to hex string (lowercase)
      let storeDirText = storeDirTextFrom storeDir'
      let storeDirPrefix = storeDirText <> "/"
      let drvHashHex = decodeUtf8 (convertToBase Base16 h :: ByteString)
      -- The final name in the hash input must match the output-specific name (same as 'name')
      -- This is critical: Nix's makeOutputPath uses outputPathName(drvName, outputName) here
      let toHash = "output:" <> o <> ":sha256:" <> drvHashHex <> ":" <> storeDirText <> ":" <> Store.unStorePathName name
      -- Use hnix-store-core's mkStorePathHashPart which handles truncation and returns raw bytes
      -- Use crypton's SHA256 (compatible with hnix-store-core)
      let hashPart = Store.mkStorePathHashPart @CryptonHash.SHA256 (encodeUtf8 toHash)
      -- Extract raw bytes and base32-encode using Nix base32
      let hashText = Base32.encode $ Store.unStorePathHashPart hashPart
      -- Build the store path
      pure $ storeDirPrefix <> hashText <> "-" <> Store.unStorePathName name

    toStorePaths :: HashSet StringContext -> (Set Text, Map Text [Text])
    toStorePaths = foldl (flip addToInputs) mempty

    addToInputs :: Bifunctor p => StringContext -> p (Set Text) (Map Text [Text])  -> p (Set Text) (Map Text [Text])
    addToInputs (StringContext kind (varNameText -> path)) =
      case kind of
        DirectPath -> first $ Set.insert path
        DerivationOutput o -> second $ Map.insertWith (<>) path $ one o
        AllOutputs ->
          -- TODO: recursive lookup. See prim_derivationStrict
          -- XXX: When is this really used ?
          error "Not implemented: derivations depending on a .drv file are not yet supported."


-- | Build a derivation in a context collecting string contexts.
-- This is complex from a typing standpoint, but it allows to perform the
-- full computation without worrying too much about all the string's contexts.
buildDerivationWithContext :: forall e t f m. (MonadNix e t f m) => KeyMap (NValue t f m) -> WithStringContextT m Derivation
buildDerivationWithContext drvAttrs = do
    -- Detect nulls for required string attributes early to improve diagnostics.
    let requireNonNullAttr attr = do
          raw <- getAttrRaw attr
          case raw of
            NVConstant NNull ->
              lift $ throwError $ ErrorCall $
                "Derivation attribute '" <> show attr <> "' is null"
            _ -> pure ()

        requireNonNullAttrIfPresent attr = case HM.lookup attr drvAttrs of
          Nothing -> pure ()
          Just v  -> do
            raw <- lift $ demand v
            case raw of
              NVConstant NNull ->
                lift $ throwError $ ErrorCall $
                  "Derivation attribute '" <> show attr <> "' is null"
              _ -> pure ()

    requireNonNullAttr "name"
    requireNonNullAttr "system"
    requireNonNullAttr "builder"
    requireNonNullAttrIfPresent "outputHash"
    requireNonNullAttrIfPresent "outputHashMode"
    requireNonNullAttrIfPresent "outputs"

    -- Parse name first, so we can add an informative frame
    drvName     <- getAttr   "name"                      $ assertDrvStoreName <=< extractNixString
    withFrame' Info (ErrorCall $ "While evaluating derivation " <> show drvName) $ do

      useJson     <- getAttrOr "__structuredAttrs" False     pure
      ignoreNulls <- getAttrOr "__ignoreNulls"     False     pure

      args        <- getAttrOr "args"              mempty  $ traverse (extractNixString <=< fromValue')
      builder     <- getAttr   "builder"                     extractNixString
      platform    <- getAttr   "system"                    $ assertNonNull <=< extractNoCtx
      mHash       <- getAttrOr "outputHash"        mempty  $ (pure . pure) <=< extractNoCtx
      mHashAlgo   <- getAttrMaybeNoCtx "outputHashAlgo"
      hashMode    <- getAttrOr "outputHashMode"    Flat    $ parseHashMode <=< extractNoCtx
      outputs     <- getAttrOr "outputs"       (one "out") $ traverse (extractNoCtx <=< fromValue')

      mFixedOutput <-
        maybe
          (pure Nothing)
          (\ hash -> do
            when (outputs /= one "out") $ lift $ throwError $ ErrorCall "Multiple outputs are not supported for fixed-output derivations"
            -- mkNamedDigest returns Either String (DSum HashAlgo Digest)
            digest <- lift $ either (throwError . ErrorCall) pure $
              maybe (inferHashDigest hash) (`Store.mkNamedDigest` hash) mHashAlgo
            pure $ Just digest)
          mHash

      -- filter out null values if needed.
      attrs <-
        lift $
          bool
            (pure drvAttrs)
            (HM.mapMaybe id <$>
              traverse
                (fmap
                  (\case
                    NVConstant NNull -> Nothing
                    _value           -> Just _value
                  )
                  . demand
                )
                drvAttrs
            )
            ignoreNulls

      env <- if useJson
        then do
          jsonString :: NixString <- lift $ toJSONNixString $ NVSet mempty $ HM.mapKeys mkVarName $
            deleteKeys [ "args", "__ignoreNulls", "__structuredAttrs" ] attrs
          rawString :: Text <- extractNixString jsonString
          pure $ one ("__json", rawString)
        else
          traverse (extractNixString <=< lift . coerceAnyToNixString callFunc CopyToStore) $
            HM.foldrWithKey Map.insert Map.empty $ deleteKeys [ "args", "__ignoreNulls" ] attrs

      pure $ Derivation { platform, builder, args, env,  hashMode, useJson
        , name = drvName
        , outputs = Map.fromList $ (, mempty) <$> outputs
        , mFixed = mFixedOutput
        , inputs = mempty -- stub for now
        }
  where

    -- common functions, lifted to WithStringContextT

    fromValue' :: (FromValue a m (NValue' t f m (NValue t f m)), MonadNix e t f m) => NValue t f m -> WithStringContextT m a
    fromValue' = lift . fromValue

    withFrame' :: (Framed e m, Exception s) => NixLevel -> s -> WithStringContextT m a -> WithStringContextT m a
    withFrame' level f = join . lift . withFrame level f . pure

    -- shortcuts to get the (forced) value of an KeyMap field

    getAttrOr' :: forall v a. (MonadNix e t f m, FromValue v m (NValue' t f m (NValue t f m)))
      => Text -> m a -> (v -> WithStringContextT m a) -> WithStringContextT m a
    getAttrOr' n d f = case HM.lookup n drvAttrs of
      Nothing -> lift d
      Just v  -> withFrame' Info (ErrorCall $ "While evaluating attribute '" <> show n <> "'") $
                   f =<< fromValue' v

    getAttrOr n = getAttrOr' n . pure

    getAttr n = getAttrOr' n (throwError $ ErrorCall $ "Required attribute '" <> show n <> "' not found.")

    getAttrRaw :: Text -> WithStringContextT m (NValue t f m)
    getAttrRaw n = case HM.lookup n drvAttrs of
      Nothing -> lift $ throwError $ ErrorCall $ "Required attribute '" <> show n <> "' not found."
      Just v  -> lift $ demand v

    getAttrMaybeNoCtx :: Text -> WithStringContextT m (Maybe Text)
    getAttrMaybeNoCtx n = case HM.lookup n drvAttrs of
      Nothing -> pure Nothing
      Just v  -> withFrame' Info (ErrorCall $ "While evaluating attribute '" <> show n <> "'") $ do
        raw <- lift $ demand v
        case raw of
          NVConstant NNull -> pure Nothing
          _ -> Just <$> (extractNoCtx =<< fromValue' raw)

    inferHashDigest :: Text -> Either String HashDigest
    inferHashDigest hash
      | Text.null hash = Left "empty outputHash requires explicit outputHashAlgo"
      | otherwise = tryAlgos ["sha256", "sha512", "sha1", "md5"]
      where
        tryAlgos [] = Left $ "outputHashAlgo missing and outputHash is not a valid SRI/known hash: " <> Text.unpack hash
        tryAlgos (a:as) = case Store.mkNamedDigest a hash of
          Right d -> Right d
          Left _  -> tryAlgos as

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

    deleteKeys :: [Text] -> KeyMap a -> KeyMap a
    deleteKeys keys attrSet = foldl' (flip HM.delete) attrSet keys
