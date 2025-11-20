{-# language CPP #-}
{-# language DataKinds #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language PackageImports #-} -- 2021-07-05: Due to hashing Haskell IT system situation, in HNix we currently ended-up with 2 hash package dependencies @{hashing, cryptonite}@
{-# language TypeApplications #-}
{-# language ExistentialQuantification #-}
{-# language StandaloneDeriving #-}

module Nix.Effects.Derivation ( defaultDerivationStrict ) where

import           Nix.Prelude             hiding ( readFile )
import           Data.ByteArray                 ( convert )
import           Data.ByteArray.Encoding        ( Base(Base16), convertToBase )
import           Data.Default.Class             ( def )
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Data.Char                      ( isAscii
                                                , isAlphaNum
                                                )
import qualified Data.HashMap.Lazy             as M
import qualified Data.HashMap.Strict           as MS ( insert )
import qualified Data.HashSet                  as S
import           Data.Foldable                  ( foldl )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text

import           Text.Megaparsec
import           Text.Megaparsec.Char

import qualified "cryptonite" Crypto.Hash      as Hash -- 2021-07-05: Attrocity of Haskell hashing situation, in HNix we ended-up with 2 hash package dependencies @{hashing, cryptonite}@
import qualified Data.ByteString               as Data.ByteString
import qualified System.Nix.Base32             as Base32

import           Nix.Atoms
import           Nix.Expr.Types          hiding ( Recursive )
import           Nix.Convert
import           Nix.Effects
import           Nix.Exec                       ( MonadNix
                                                , callFunc
                                                )
import           Nix.Frames
import           Nix.Json                       ( toJSONNixString )
import           Nix.Render
import           Nix.String
import           Nix.String.Coerce
import           Nix.Value
import           Nix.Value.Monad

import qualified System.Nix.Hash               as Store
import qualified System.Nix.StorePath          as Store
import qualified System.Nix.ContentAddress     as Store
import qualified "cryptonite" Crypto.Hash      as Hash
import qualified "crypton" Crypto.Hash         as CryptonHash
import           Data.Some.Newtype              ( Some(..) )
import           Data.Dependent.Sum             ( DSum(..) )
import qualified Data.Dependent.Sum            as DSum

-- Type for fixed-output derivation hash digest
-- Digest comes from crypton (Crypto.Hash.Digest)
type HashDigest = DSum Store.HashAlgo CryptonHash.Digest

-- Helper functions to work with HashDigest
hashDigestAlgoText :: HashDigest -> Text
hashDigestAlgoText (hashAlgo DSum.:=> _) = Store.algoToText hashAlgo

hashDigestText :: HashDigest -> Text
hashDigestText (_ DSum.:=> hashDigest) = Store.encodeDigestWith Store.NixBase32 hashDigest

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

parsePath :: (Framed e m) => Text -> m Store.StorePath
parsePath p = case Store.parsePath def (encodeUtf8 p) of
  Left err -> throwError $ ErrorCall $ "Cannot parse store path " <> show p <> ":\n" <> show err
  Right path -> pure path

writeDerivation :: (Framed e m, MonadStore m) => Derivation -> m Store.StorePath
writeDerivation drv@Derivation{inputs, name} = do
  let (inputSrcs, inputDrvs) = inputs
  referencePaths <- traverse parsePath (Set.toList $ inputSrcs <> Set.fromList (Map.keys inputDrvs))
  let references = S.fromList $ map (StorePath . fromString . decodeUtf8 . Store.storePathToRawFilePath def) referencePaths
  path <- addTextToStore (Text.append name ".drv") (unparseDrv drv) references False
  parsePath $ fromString $ coerce path

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
                -- Convert digest to base32 for Nix store paths (get raw bytes, not hex string)
                let hashBytes = convert digestValue :: ByteString
                let hash = Base32.encode hashBytes
                pure (hash, outs)
              )
              (\ hash -> pure (hash, outs))
              (M.lookup path cache)
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
    s <- M.mapKeys coerce <$> fromValue @(AttrSet (NValue t f m)) val
    (drv, ctx) <- runWithStringContextT' $ buildDerivationWithContext s
    drvName <- makeStorePathName $ name drv
    let
      inputs = toStorePaths ctx
      ifNotJsonModEnv f =
        bool f id (useJson drv)
          (env drv)

    -- Compute the output paths, and add them to the environment if needed.
    -- Also add the inputs, just computed from the strings contexts.
    drv' <- case mFixed drv of
      Just digest -> do
        -- Fixed-output derivation: output path is content-addressed
        outputPath <- makeFixedOutputPath drvName digest (hashMode drv)
        let outputs' = Map.singleton "out" outputPath
        pure $ drv
          { inputs
          , outputs = outputs'
          , env = ifNotJsonModEnv (outputs' <>)
          }

      Nothing -> do
        hash <- hashDerivationModulo $ drv
          { inputs
        --, outputs = Map.map (const "") (outputs drv)  -- not needed, this is already the case
          , env =
              ifNotJsonModEnv
                (\ baseEnv ->
                  foldl'
                    (\m k -> Map.insert k mempty m)
                    baseEnv
                    (Map.keys $ outputs drv)
                )
          }
        outputs' <- sequenceA $ Map.mapWithKey (\o _ -> makeOutputPath o hash drvName) $ outputs drv
        pure $ drv
          { inputs
          , outputs = outputs'
          , env = ifNotJsonModEnv (outputs' <>)
          }

    (coerce @Text @VarName -> drvPath) <- pathToText <$> writeDerivation drv'

    -- Memoize here, as it may be our last chance in case of readonly stores.
    digestValue <- hashDerivationModulo drv'
    -- Convert digest to base32 for Nix store paths (get raw bytes, not hex string)
    let drvHashBytes = convert digestValue :: ByteString
    let drvHash = Base32.encode drvHashBytes
    modify $ second $ MS.insert (coerce drvPath) drvHash

    let
      outputsWithContext =
        Map.mapWithKey
          (\out (coerce -> path) -> mkNixStringWithSingletonContext (StringContext (DerivationOutput out) drvPath) path)
          (outputs drv')
      drvPathWithContext = mkNixStringWithSingletonContext (StringContext AllOutputs drvPath) drvPath
      attrSet = NVStr <$> M.fromList (("drvPath", drvPathWithContext) : Map.toList outputsWithContext)
    -- TODO: Add location information for all the entries.
    --              here --v
    pure $ NVSet mempty $ M.mapKeys coerce attrSet

  where

    pathToText = decodeUtf8 . Store.storePathToRawFilePath def

    makeFixedOutputPath :: (Framed e m) => Store.StorePathName -> HashDigest -> HashMode -> m Text
    makeFixedOutputPath name digest mode = do
      -- For fixed-output derivations, the output path is computed from the content hash
      -- Format: "fixed:out:<hashMode>:<hashAlgo>:<hash>:/nix/store:<name>"
      let algoText = hashDigestAlgoText digest
      let hashText = hashDigestText digest
      let modePrefix = case mode of
            Recursive -> "r:"
            Flat -> mempty
      let toHash = "fixed:out:" <> modePrefix <> algoText <> ":" <> hashText <> ":/nix/store:" <> Store.unStorePathName name
      -- Use SHA256 to compute the store path hash
      let hashPart = Store.mkStorePathHashPart @CryptonHash.SHA256 (encodeUtf8 toHash)
      let hashBase32 = Base32.encode $ Store.unStorePathHashPart hashPart
      pure $ "/nix/store/" <> hashBase32 <> "-" <> Store.unStorePathName name

    makeOutputPath o h n = do
      name <- makeStorePathName $ Store.unStorePathName n <> if o == "out" then mempty else "-" <> o
      -- Compute the output path hash according to Nix's algorithm:
      -- For non-fixed outputs, hash = sha256("output:<outputName>:sha256:<drv_modulo_hex>:/nix/store:<outputPathName>")
      -- where outputPathName is the base name for "out", or base name + "-" + output name for other outputs
      -- Convert the hash digest to hex string (lowercase)
      let drvHashHex = decodeUtf8 (convertToBase Base16 h :: ByteString)
      -- The final name in the hash input must match the output-specific name (same as 'name')
      -- This is critical: Nix's makeOutputPath uses outputPathName(drvName, outputName) here
      let toHash = "output:" <> o <> ":sha256:" <> drvHashHex <> ":/nix/store:" <> Store.unStorePathName name
      -- Use hnix-store-core's mkStorePathHashPart which handles truncation and returns raw bytes
      -- Use crypton's SHA256 (compatible with hnix-store-core)
      let hashPart = Store.mkStorePathHashPart @CryptonHash.SHA256 (encodeUtf8 toHash)
      -- Extract raw bytes and base32-encode using Nix base32
      let hashText = Base32.encode $ Store.unStorePathHashPart hashPart
      -- Build the store path
      pure $ "/nix/store/" <> hashText <> "-" <> Store.unStorePathName name

    toStorePaths :: HashSet StringContext -> (Set Text, Map Text [Text])
    toStorePaths = foldl (flip addToInputs) mempty

    addToInputs :: Bifunctor p => StringContext -> p (Set Text) (Map Text [Text])  -> p (Set Text) (Map Text [Text])
    addToInputs (StringContext kind (coerce -> path)) =
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
    -- Parse name first, so we can add an informative frame
    drvName     <- getAttr   "name"                      $ assertDrvStoreName <=< extractNixString
    withFrame' Info (ErrorCall $ "While evaluating derivation " <> show drvName) $ do

      useJson     <- getAttrOr "__structuredAttrs" False     pure
      ignoreNulls <- getAttrOr "__ignoreNulls"     False     pure

      args        <- getAttrOr "args"              mempty  $ traverse (extractNixString <=< fromValue')
      builder     <- getAttr   "builder"                     extractNixString
      platform    <- getAttr   "system"                    $ assertNonNull <=< extractNoCtx
      mHash       <- getAttrOr "outputHash"        mempty  $ (pure . pure) <=< extractNoCtx
      hashMode    <- getAttrOr "outputHashMode"    Flat    $ parseHashMode <=< extractNoCtx
      outputs     <- getAttrOr "outputs"       (one "out") $ traverse (extractNoCtx <=< fromValue')

      mFixedOutput <-
        maybe
          (pure Nothing)
          (\ hash -> do
            when (outputs /= one "out") $ lift $ throwError $ ErrorCall "Multiple outputs are not supported for fixed-output derivations"
            hashType <- getAttr "outputHashAlgo" extractNoCtx
            -- mkNamedDigest returns Either String (DSum HashAlgo Digest)
            digest <- lift $ either (throwError . ErrorCall) pure $ Store.mkNamedDigest hashType hash
            pure $ Just digest)
          mHash

      -- filter out null values if needed.
      attrs <-
        lift $
          bool
            (pure drvAttrs)
            (M.mapMaybe id <$>
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
          jsonString :: NixString <- lift $ toJSONNixString $ NVSet mempty $ M.mapKeys coerce $
            deleteKeys [ "args", "__ignoreNulls", "__structuredAttrs" ] attrs
          rawString :: Text <- extractNixString jsonString
          pure $ one ("__json", rawString)
        else
          traverse (extractNixString <=< lift . coerceAnyToNixString callFunc CopyToStore) $
            Map.fromList $ M.toList $ deleteKeys [ "args", "__ignoreNulls" ] attrs

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
    getAttrOr' n d f = case M.lookup n drvAttrs of
      Nothing -> lift d
      Just v  -> withFrame' Info (ErrorCall $ "While evaluating attribute '" <> show n <> "'") $
                   f =<< fromValue' v

    getAttrOr n = getAttrOr' n . pure

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

    deleteKeys :: [Text] -> KeyMap a -> KeyMap a
    deleteKeys keys attrSet = foldl' (flip M.delete) attrSet keys

