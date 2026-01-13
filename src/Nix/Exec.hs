{-# language AllowAmbiguousTypes #-}
{-# language CPP #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language PartialTypeSignatures #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options_ghc -Wno-orphans #-}
{-# options_ghc -fno-warn-name-shadowing #-}


module Nix.Exec where

import           Nix.Prelude             hiding ( putStr
                                                , putStrLn
                                                , print
                                                )
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Control.Monad.Catch     hiding ( catchJust )
import           Control.Monad.Fix
import           Data.Fix
import qualified Data.HashMap.Strict           as HM
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as Text
import           Nix.Atoms
import           Nix.Cited
import           Nix.Config.Singleton
import           Nix.Convert
import           Nix.Effects
import           Nix.Eval                      as Eval
import           Nix.Expr.Desugar              ( desugarExprLoc )
import           Nix.Expr.Strings              ( runAntiquoted )
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.Options
import           Nix.Context                    ( askEvalStats, CtxCfg, HasEvalCfg )
import           Nix.EvalStats                  ( EvalStats(..), withExprTiming, withBuiltinTiming )
import           Nix.Pretty
import           Nix.Render
import           Nix.Scope
import           Nix.String
import           Nix.String.Coerce
import           Nix.Thunk
import           Nix.Value
import           Nix.Value.Equal
import           Data.Vector                    ( Vector )
import           Nix.Value.Monad
import           Prettyprinter
import qualified Text.Show.Pretty              as PS
import qualified GHC.Clock                     as Clock
import           Data.Data                     ( toConstr )

#ifdef MIN_VERSION_ghc_datasize 
import           GHC.DataSize
#endif

type MonadCited t f m =
  ( HasCitations m (NValue t f m) t
  , HasCitations1 m (NValue t f m) f
  , MonadDataContext f m
  )

mkNVConstantWithProvenance
  :: MonadCited t f m
  => Scopes m (NValue t f m)
  -> SrcSpan
  -> NAtom
  -> NValue t f m
mkNVConstantWithProvenance scopes span x =
  addProvenance (Provenance scopes . NConstantAnnF span $ x) $ NVConstant x

mkNVStrWithProvenance
  :: MonadCited t f m
  => Scopes m (NValue t f m)
  -> SrcSpan
  -> NixString
  -> NValue t f m
mkNVStrWithProvenance scopes span x =
  addProvenance (Provenance scopes . NStrAnnF span . DoubleQuoted . one . Plain . ignoreContext $ x) $ NVStr x

mkNVPathWithProvenance
  :: MonadCited t f m
  => Scopes m (NValue t f m)
  -> SrcSpan
  -> Path
  -> Path
  -> NValue t f m
mkNVPathWithProvenance scope span lit real =
  addProvenance (Provenance scope . NLiteralPathAnnF span $ lit) $ NVPath real

mkNVClosureWithProvenance
  :: MonadCited t f m
  => Scopes m (NValue t f m)
  -> SrcSpan
  -> Params ()
  -> (NValue t f m -> m (NValue t f m))
  -> NValue t f m
mkNVClosureWithProvenance scopes span x f =
  addProvenance (Provenance scopes $ NAbsAnnF span (Nothing <$ x) Nothing) $ NVClosure x f

mkNVUnaryOpWithProvenance
  :: MonadCited t f m
  => Scopes m (NValue t f m)
  -> SrcSpan
  -> NUnaryOp
  -> Maybe (NValue t f m)
  -> NValue t f m
  -> NValue t f m
mkNVUnaryOpWithProvenance scope span op val =
  addProvenance (Provenance scope $ NUnaryAnnF span op val)

mkNVAppOpWithProvenance
  :: MonadCited t f m
  => Scopes m (NValue t f m)
  -> SrcSpan
  -> Maybe (NValue t f m)
  -> Maybe (NValue t f m)
  -> NValue t f m
  -> NValue t f m
mkNVAppOpWithProvenance scope span lval rval =
  addProvenance (Provenance scope $ NAppAnnF span lval rval)

mkNVBinaryOpWithProvenance
  :: MonadCited t f m
  => Scopes m (NValue t f m)
  -> SrcSpan
  -> NBinaryOp
  -> Maybe (NValue t f m)
  -> Maybe (NValue t f m)
  -> NValue t f m
  -> NValue t f m
mkNVBinaryOpWithProvenance scope span op lval rval =
  addProvenance (Provenance scope $ NBinaryAnnF span op lval rval)

type MonadCitedThunks t f m =
  ( MonadThunk t m (NValue t f m)
  , MonadDataErrorContext t f m
  , HasCitations m (NValue t f m) t
  , HasCitations1 m (NValue t f m) f
  )

-- | Core constraint for Nix evaluation monad.
--
-- Type-level configuration for compile-time specialization (stats, tracing) is
-- provided separately via 'EvalConfig' from "Nix.Config.Singleton". Functions
-- like 'evalExprLocT' use both 'MonadNix' and 'HasStats'/'HasTracing' constraints
-- to achieve zero-cost conditional execution. We deliberately avoid adding type
-- parameters here, as doing so would cause cascading ambiguous type variable
-- errors throughout the codebase.
type MonadNix e t f m =
  ( Has e SrcSpan
  , Has e Options
  , Has e (Maybe EvalStats)
  , Scoped (NValue t f m) m
  , Framed e m
  , MonadFix m
  , MonadCatch m
  , MonadThrow m
  , Alternative m
  , MonadEffects t f m
  , MonadCitedThunks t f m
  , MonadValue (NValue t f m) m
  )

data ExecFrame t f m = Assertion SrcSpan (NValue t f m)
  deriving (Show, Typeable)

instance MonadDataErrorContext t f m => Exception (ExecFrame t f m)

nverr :: forall e t f s m a. (MonadNix e t f m, HasProvCfg (CtxCfg e), Exception s) => s -> m a
nverr = evalError @(NValue t f m)

askSpan :: forall e m . (MonadReader e m, Has e SrcSpan) => m SrcSpan
askSpan = askLocal

wrapExprLoc :: SrcSpan -> NExprLocF r -> NExprLoc
wrapExprLoc span x = Fix $ NSymAnn span "<?>" <$ x
{-# INLINABLE wrapExprLoc #-}

-- | Zero-cost provenance dispatch with scope and span.
--
-- Captures the common pattern where provenance-enabled code needs to fetch
-- scope and span, while provenance-disabled code can skip these lookups entirely.
--
-- @
-- evalConstant c = withProvCtx
--   (\\scope span -> pure $ mkNVConstantWithProvenance scope span c)
--   (pure $ NVConstant c)
-- @
--
-- When @CfgProv (CtxCfg e) ~ 'False@, the first argument is eliminated at
-- compile time, and @askScopes@ / @askSpan@ calls are never executed.
withProvCtx
  :: forall e t f m a. (MonadNix e t f m, HasProvCfg (CtxCfg e))
  => (Scopes m (NValue t f m) -> SrcSpan -> m a)  -- ^ With provenance
  -> m a                                           -- ^ Without provenance
  -> m a
withProvCtx withProv withoutProv = case singProv @(CtxCfg e) of
  STrue -> do
    scope <- askScopes
    span  <- askSpan
    withProv scope span
  SFalse -> withoutProv
{-# INLINE withProvCtx #-}

--  2021-01-07: NOTE: This instance belongs to be beside MonadEval type class.
-- Currently instance is stuck in orphanage between the requirements to be MonadEval, aka Eval stage, and emposed requirement to be MonadNix (Execution stage). MonadNix constraint tries to put the cart before horse and seems superflous, since Eval in Nix also needs and can throw exceptions. It is between `nverr` and `evalError`.
--
-- The HasProvCfg constraint enables compile-time provenance dispatch.
-- When cfg is known (via withEvalCfg), GHC eliminates unused branches.
instance (MonadNix e t f m, HasProvCfg (CtxCfg e)) => MonadEval (NValue t f m) m where
  freeVariable var =
    nverr @e @t @f $ ErrorCall $ toString @Text $ "Undefined variable '" <> varNameText var <> "'"

  synHole name =
    do
      span  <- askSpan
      scope <- askScopes
      evalError @(NValue t f m) $ SynHole $
        SynHoleInfo
          { _synHoleInfo_expr  = NSynHoleAnn span name
          , _synHoleInfo_scope = scope
          }


  attrMissing ks ms =
    evalError @(NValue t f m) $ ErrorCall $ toString $
      case ms of
        Nothing -> "Inheriting unknown attribute: " <> attr
        Just s -> "Could not look up attribute " <> attr <> " in " <> show (prettyNValue s)
       where
        attr = Text.intercalate "." $ NE.toList $ fmap varNameText ks

  -- Compile-time provenance dispatch using singProv @(CtxCfg e).
  -- When CfgProv (CtxCfg e) ~ 'False, GHC eliminates the STrue branches entirely.

  evalCurPos = do
    span@(SrcSpan delta _) <- askSpan
    case singProv @(CtxCfg e) of
      STrue -> do
        scope <- askScopes
        addProvenance @_ @_ @(NValue t f m)
          (Provenance scope . NSymAnnF span $ "__curPos") <$>
            toValue delta
      SFalse -> toValue delta

  evaledSym name val = withProvCtx
    (\scope span -> pure $ addProvenance @_ @_ @(NValue t f m)
      (Provenance scope $ NSymAnnF span name) val)
    (pure val)

  evalConstant c = withProvCtx
    (\scope span -> pure $ mkNVConstantWithProvenance scope span c)
    (pure $ NVConstant c)

  evalString str =
    do
      -- Use coerceAnyToNixString to properly handle __toString and outPath
      ns <- assembleStringWithCoercion (stringParts str)
      withProvCtx
        (\scope span -> pure $ mkNVStrWithProvenance scope span ns)
        (pure $ NVStr ns)
   where
    assembleStringWithCoercion :: [Antiquoted Text (m (NValue t f m))] -> m NixString
    assembleStringWithCoercion parts = fold <$> traverse coercePart parts

    coercePart :: Antiquoted Text (m (NValue t f m)) -> m NixString
    coercePart =
      runAntiquoted
        "\n"
        (pure . mkNixStringWithoutContext)
        coerceValue

    coerceValue :: m (NValue t f m) -> m NixString
    coerceValue mv = do
      v <- mv
      -- Use CopyToStore: when paths are interpolated in strings, they get added to the store
      coerceAnyToNixString callFunc CopyToStore v

  evalLiteralPath p = do
    realPath <- toAbsolutePath @t @f @m p
    withProvCtx
      (\scope span -> pure $ mkNVPathWithProvenance scope span p realPath)
      (pure $ NVPath realPath)

  evalPath str =
    do
      mns <- assembleString str
      case mns of
        Nothing -> nverr $ ErrorCall "Failed to assemble path"
        Just ns -> do
          let litText = ignoreContext ns
          let litPath = fromString (toString litText)
          real <- toAbsolutePath @t @f @m litPath
          withProvCtx
            (\scope span -> pure $ addProvenance
              (Provenance scope . NPathAnnF span $ DoubleQuoted $ one $ Plain litText)
              (NVPath real))
            (pure $ NVPath real)

  evalEnvPath p = do
    realPath <- findEnvPath @t @f @m (coerce p)
    withProvCtx
      (\scope span -> pure $ mkNVPathWithProvenance scope span p realPath)
      (pure $ NVPath realPath)

  evalUnary = execUnaryOp'

  evalBinary = execBinaryOp'

  evalWith c b = do
    result <- evalWithAttrSet c b
    withProvCtx
      (\scope span ->
        let f = join $ addProvenance . Provenance scope . NWithAnnF span Nothing . pure
        in pure $ f result)
      (pure result)

  evalIf c tVal fVal = do
    bl :: Bool <- fromValue c
    case singProv @(CtxCfg e) of
      STrue -> do
        scope <- askScopes
        span  <- askSpan
        let
          fun x y = addProvenance (Provenance scope $ NIfAnnF span (pure c) x y)
        if bl
          then do
            tv <- tVal
            pure $ fun (pure tv) Nothing tv
          else do
            fv <- fVal
            pure $ fun Nothing (pure fv) fv
      SFalse -> if bl then tVal else fVal

  evalAssert c body = do
    span <- askSpan
    b :: Bool <- fromValue c
    if b
      then withProvCtx
        (\scope _ -> join (addProvenance . Provenance scope . NAssertAnnF span (pure c) . pure) <$> body)
        body
      else nverr $ Assertion span c

  evalApp f x = do
    result <- callFunc f =<< defer x
    withProvCtx
      (\scope span -> pure $ mkNVAppOpWithProvenance scope span (pure f) Nothing result)
      (pure result)

  evalAbs
    :: Params (m (NValue t f m))
    -> ( forall a
      . m (NValue t f m)
      -> ( AttrSet (m (NValue t f m))
        -> m (NValue t f m)
        -> m (a, NValue t f m)
        )
      -> m (a, NValue t f m)
      )
    -> m (NValue t f m)
  evalAbs p k =
    let closureFunc = fmap snd . flip (k @()) (const (fmap (mempty ,))) . pure
    in withProvCtx
      (\scope span -> pure $ mkNVClosureWithProvenance scope span (void p) closureFunc)
      (pure $ NVClosure (void p) closureFunc)

  evalError = throwError

infixl 1 `callFunc`
callFunc
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
callFunc fun arg =
  do
    -- O(min(2001, depth)) depth check - faster than O(n) for shallow stacks
    -- See Nix.Frames for performance notes
    frames <- askFrames
    when (exceedsDepth 2000 frames) $ throwError $ ErrorCall "Function call stack exhausted"

    fun' <- demand fun
    case fun' of
      NVBuiltin name f    ->
        do
          span <- askSpan
          mstats <- askEvalStats
          withFrame Info ((Calling @m @(NValue t f m)) name span) $
            case mstats of
              Nothing -> f arg
              Just stats -> withBuiltinTiming stats (varNameText name) (f arg)
      NVClosure _params f -> f arg
      (NVSet _ m) | Just f <- HM.lookup "__functor" m ->
        (`callFunc` arg) =<< (`callFunc` fun') f
      _x -> throwError $ ErrorCall $ "Attempt to call non-function: " <> show _x

-- | Unified unary operator execution with zero-cost provenance dispatch.
--
-- When provenance is disabled, the @wrapResult@ helper compiles to @pure@.
-- When enabled, it adds provenance tracking to the result.
execUnaryOp'
  :: forall e t f m
   . (MonadNix e t f m, HasProvCfg (CtxCfg e))
  => NUnaryOp
  -> NValue t f m
  -> m (NValue t f m)
execUnaryOp' op arg =
  case arg of
    NVConstant c ->
      case (op, c) of
        (NNeg, NInt   i) -> wrapResult $ NVConstant $ NInt (negate i)
        (NNeg, NFloat f) -> wrapResult $ NVConstant $ NFloat (negate f)
        (NNot, NBool  b) -> wrapResult $ NVConstant $ NBool (not b)
        _seq ->
          nverr @e @t @f $ ErrorCall $ "unsupported argument type for unary operator " <> show _seq
    _x ->
      nverr @e @t @f $ ErrorCall $ "argument to unary operator must evaluate to an atomic type: " <> show _x
 where
  wrapResult :: NValue t f m -> m (NValue t f m)
  wrapResult v = withProvCtx
    (\scope span -> pure $ mkNVUnaryOpWithProvenance scope span op (pure arg) v)
    (pure v)
  {-# INLINE wrapResult #-}

-- | Unified binary operator execution with zero-cost provenance dispatch.
--
-- Handles short-circuit operators (NEq, NNEq, NOr, NAnd, NImpl) directly,
-- and delegates forced operations to 'execBinaryOpForced''.
execBinaryOp'
  :: forall e t f m
   . (MonadNix e t f m, MonadEval (NValue t f m) m, HasProvCfg (CtxCfg e))
  => NBinaryOp
  -> NValue t f m
  -> m (NValue t f m)
  -> m (NValue t f m)
execBinaryOp' op lval rarg =
  case op of
    NEq   -> helperEq id
    NNEq  -> helperEq not
    NOr   -> do
      bl <- fromValue lval
      if bl
        then wrapBool Nothing True
        else evalRight
    NAnd  -> do
      bl <- fromValue lval
      if bl
        then evalRight
        else wrapBool Nothing False
    NImpl -> do
      bl <- fromValue lval
      if bl
        then evalRight
        else wrapBool Nothing True
    _     ->
      do
        rval  <- rarg
        rval' <- demand rval
        lval' <- demand lval
        execBinaryOpForced' op lval' rval'

 where
  helperEq :: (Bool -> Bool) -> m (NValue t f m)
  helperEq flag = do
    rval <- rarg
    eq <- valueEqM lval rval
    wrapBool (pure rval) $ flag eq

  evalRight = do
    rval <- rarg
    x <- fromValue rval
    wrapBool (pure rval) x

  -- | Zero-cost bool wrapper: adds provenance when enabled, identity when disabled.
  wrapBool :: Maybe (NValue t f m) -> Bool -> m (NValue t f m)
  wrapBool rvalM b = withProvCtx
    (\scope span -> pure $ mkNVBinaryOpWithProvenance scope span op (pure lval) rvalM $ NVConstant $ NBool b)
    (pure $ NVConstant $ NBool b)
  {-# INLINE wrapBool #-}

-- | Unified forced binary operator execution with zero-cost provenance dispatch.
--
-- When provenance is disabled, all wrapper functions compile to identity.
-- When enabled, provenance tracking is added to the result.
execBinaryOpForced'
  :: forall e t f m
   . (MonadNix e t f m, MonadEval (NValue t f m) m, HasProvCfg (CtxCfg e))
  => NBinaryOp
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)
execBinaryOpForced' op lval rval =
  case op of
    NLt    -> mkCmpOp (<)
    NLte   -> mkCmpOp (<=)
    NGt    -> mkCmpOp (>)
    NGte   -> mkCmpOp (>=)
    NMinus -> mkBinNumOp (-)
    NMult  -> mkBinNumOp (*)
    NDiv   -> mkBinNumOp' div (/)
    NConcat ->
      case (lval, rval) of
        (NVList ls, NVList rs) -> wrapResult $ NVList $ ls <> rs
        _ -> unsupportedTypes

    NUpdate ->
      case (lval, rval) of
        (NVSet lp ls, NVSet rp rs) -> wrapResult $ NVSet (rp <> lp) (rs <> ls)
        (NVSet lp ls, NVConstant NNull) -> wrapResult $ NVSet lp ls
        (NVConstant NNull, NVSet rp rs) -> wrapResult $ NVSet rp rs
        _ -> unsupportedTypes

    NPlus ->
      case (lval, rval) of
        (NVConstant _, NVConstant _) -> mkBinNumOp (+)
        (NVStr ls, NVStr rs) -> wrapResult $ NVStr (ls <> rs)
        (NVStr ls, NVPath p) ->
          wrapResult . NVStr . (ls <>) =<<
            coercePathToNixString CopyToStore p
        (NVPath ls, NVStr rs) ->
          case getStringNoContext rs of
            Nothing ->
              throwError $ ErrorCall "A string that refers to a store path cannot be appended to a path." -- data/nix/src/libexpr/eval.cc:1412
            Just rs2 -> wrapResult . NVPath =<< toAbsolutePath @t @f (ls <> coerce (toString rs2))
        (NVPath ls, NVPath rs) -> wrapResult . NVPath =<< toAbsolutePath @t @f (ls <> rs)

        (ls@NVSet{}, NVStr rs) ->
          wrapResult . NVStr . (<> rs) =<<
            coerceAnyToNixString callFunc DontCopyToStore ls
        (NVStr ls, rs@NVSet{}) ->
          wrapResult . NVStr . (ls <>) =<<
            coerceAnyToNixString callFunc DontCopyToStore rs
        _ -> unsupportedTypes
    _other   -> shouldBeAlreadyHandled

 where
  -- | Zero-cost wrapper: adds provenance when enabled, identity when disabled.
  wrapResult :: NValue t f m -> m (NValue t f m)
  wrapResult v = withProvCtx
    (\scope span -> pure $ mkNVBinaryOpWithProvenance scope span op (pure lval) (pure rval) v)
    (pure v)
  {-# INLINE wrapResult #-}

  mkBoolP :: Bool -> m (NValue t f m)
  mkBoolP = wrapResult . NVConstant . NBool

  mkIntP :: Integer -> m (NValue t f m)
  mkIntP = wrapResult . NVConstant . NInt

  mkFloatP :: Float -> m (NValue t f m)
  mkFloatP = wrapResult . NVConstant . NFloat

  mkCmpOp :: (forall a. Ord a => a -> a -> Bool) -> m (NValue t f m)
  mkCmpOp cmpOp = case (lval, rval) of
    (NVConstant l, NVConstant r) -> mkBoolP $ l `cmpOp` r
    (NVStr l, NVStr r) -> mkBoolP $ l `cmpOp` r
    _ -> unsupportedTypes

  mkBinNumOp :: (forall a. Num a => a -> a -> a) -> m (NValue t f m)
  mkBinNumOp numOp = mkBinNumOp' numOp numOp

  mkBinNumOp'
    :: (Integer -> Integer -> Integer)
    -> (Float -> Float -> Float)
    -> m (NValue t f m)
  mkBinNumOp' intOp floatOp =
    case (lval, rval) of
      (NVConstant l, NVConstant r) ->
        case (l, r) of
          (NInt   li, NInt   ri) -> mkIntP $ li `intOp` ri
          (NInt   li, NFloat rf) -> mkFloatP $ fromInteger li `floatOp` rf
          (NFloat lf, NInt   ri) -> mkFloatP $ lf `floatOp` fromInteger ri
          (NFloat lf, NFloat rf) -> mkFloatP $ lf `floatOp` rf
          _ -> unsupportedTypes
      _ -> unsupportedTypes

  unsupportedTypes = throwError $ ErrorCall $ "Unsupported argument types for binary operator " <> show op <> ": " <> show lval <> ", " <> show rval

  shouldBeAlreadyHandled = throwError $ ErrorCall $ "This cannot happen: operator " <> show op <> " should have been handled in execBinaryOp."

-- This function is here, rather than in 'Nix.String', because of the need to
-- use 'throwError'.
fromStringNoContext
  :: Framed e m
  => NixString
  -> m Text
fromStringNoContext ns =
  case getStringNoContext ns of
    Nothing -> throwError $ ErrorCall $ "expected string with no context, but got " <> show ns
    Just v -> pure v

addTracing
  ::( MonadNix e t f m
    , Has e Options
    , Alternative n
    , MonadReader Int n
    , MonadFail n
    )
  => Alg NExprLocF (m a)
  -> Alg NExprLocF (n (m a))
addTracing k v = do
  depth <- ask
  guard $ depth < 2000
  local succ $ do
    v'@(AnnF span x) <- sequenceA v
    pure $ do
      opts <- askOptions
      let
        rendered =
          if getVerbosity opts >= Chatty
            then pretty $ PS.ppShow $ void x
            else prettyNix $ Fix $ Fix (NSym "?") <$ x
        msg x = pretty ("eval: " <> replicate depth ' ') <> x
      loc <- renderLocation span $ msg rendered <> " ...\n"
      putStr $ show loc
      res <- k v'
      print $ msg rendered <> " ...done"
      pure res

addTiming
  :: forall e t f m a
   . MonadNix e t f m
  => Int
  -> Alg NExprLocF (m a)
  -> Alg NExprLocF (m a)
addTiming thresholdMs k v@(AnnF span x) = do
  mstats <- askEvalStats
  -- Get IO time before evaluation (if stats available)
  ioTimeBefore <- case mstats of
    Just stats -> liftIO $ readIORef (statsIOTime stats)
    Nothing -> pure 0

  start <- liftIO Clock.getMonotonicTimeNSec
  res <- k v
  end <- liftIO Clock.getMonotonicTimeNSec

  -- Get IO time after evaluation and compute pure elapsed time
  ioTimeAfter <- case mstats of
    Just stats -> liftIO $ readIORef (statsIOTime stats)
    Nothing -> pure 0
  let ioTimeDuring = ioTimeAfter - ioTimeBefore
      totalNs = end - start
      -- Exclude IO time from the reported timing
      pureNs = if totalNs > ioTimeDuring then totalNs - ioTimeDuring else 0
      elapsedMs :: Int
      elapsedMs = fromIntegral (pureNs `div` 1000000)

  when (elapsedMs >= max 0 thresholdMs) $ do
    let headTag = Text.pack (show (toConstr (void x)))
    let msg =
          "timing "
            <> Text.pack (show elapsedMs)
            <> "ms "
            <> headTag
            <> "\n"
    loc <- renderLocation span (pretty msg)
    putStr $ show loc
  pure res

addStats
  :: forall e t f m a
   . MonadNix e t f m
  => EvalStats
  -> Alg NExprLocF (m a)
  -> Alg NExprLocF (m a)
addStats stats k v@(AnnF _ x) =
  let exprType = Text.pack (show (toConstr (void x)))
  in withExprTiming stats exprType (k v)
{-# INLINABLE addStats #-}

evalWithTracingAndMetaInfo
  :: forall e t f m
  . (MonadNix e t f m, HasProvCfg (CtxCfg e))
  => NExprLoc
  -> ReaderT Int m (m (NValue t f m))
evalWithTracingAndMetaInfo =
  adi
    addMetaInfo
    (addTracing Eval.evalContent)
  where
  addMetaInfo :: (NExprLoc -> ReaderT r m a) -> NExprLoc -> ReaderT r m a
  addMetaInfo = (ReaderT .) . flip . (Eval.addMetaInfo .) . flip . (runReaderT .)
{-# INLINABLE evalWithTracingAndMetaInfo #-}

evalWithTimingAndMetaInfo
  :: forall e t f m
  . (MonadNix e t f m, HasProvCfg (CtxCfg e))
  => Int
  -> NExprLoc
  -> m (NValue t f m)
evalWithTimingAndMetaInfo thresholdMs =
  adi
    Eval.addMetaInfo
    (addTiming thresholdMs Eval.evalContent)
{-# INLINABLE evalWithTimingAndMetaInfo #-}

evalWithTracingTimingAndMetaInfo
  :: forall e t f m
  . (MonadNix e t f m, HasProvCfg (CtxCfg e))
  => Int
  -> NExprLoc
  -> ReaderT Int m (m (NValue t f m))
evalWithTracingTimingAndMetaInfo thresholdMs =
  adi
    addMetaInfo
    (addTracing (addTiming thresholdMs Eval.evalContent))
  where
  addMetaInfo :: (NExprLoc -> ReaderT r m a) -> NExprLoc -> ReaderT r m a
  addMetaInfo = (ReaderT .) . flip . (Eval.addMetaInfo .) . flip . (runReaderT .)
{-# INLINABLE evalWithTracingTimingAndMetaInfo #-}

evalWithStatsAndMetaInfo
  :: forall e t f m
  . (MonadNix e t f m, HasProvCfg (CtxCfg e))
  => EvalStats
  -> NExprLoc
  -> m (NValue t f m)
evalWithStatsAndMetaInfo stats =
  adi
    Eval.addMetaInfo
    (addStats stats Eval.evalContent)
{-# INLINABLE evalWithStatsAndMetaInfo #-}

evalExprLoc :: forall e t f m. (MonadNix e t f m, HasProvCfg (CtxCfg e)) => NExprLoc -> m (NValue t f m)
evalExprLoc expr =
  do
    opts <- askOptions
    mstats <- askEvalStats
    let thresholdMs = getEvalTimingThresholdMs opts
    let
      -- Apply AST-level desugaring before evaluation
      desugared = desugarExprLoc expr
      traced = isTrace opts
      timed  = isEvalTiming opts
      pTracedAdi =
        case (traced, timed, mstats) of
          (True, True, _) ->
            join . (`runReaderT` (0 :: Int)) . evalWithTracingTimingAndMetaInfo thresholdMs
          (True, False, _) ->
            join . (`runReaderT` (0 :: Int)) . evalWithTracingAndMetaInfo
          (False, True, _) ->
            evalWithTimingAndMetaInfo thresholdMs
          (False, False, Just stats) ->
            evalWithStatsAndMetaInfo stats
          (False, False, Nothing) ->
            Eval.evalWithMetaInfo
    pTracedAdi desugared
{-# INLINABLE evalExprLoc #-}

-- | Evaluation with compile-time feature dispatch tied to environment config.
--
-- Unlike 'evalExprLoc' which does runtime dispatch, this function uses the
-- configuration from the environment type ('CtxCfg e') for compile-time
-- specialization. GHC eliminates branches for disabled features.
--
-- The 'HasEvalCfg e' constraint guarantees that stats, provenance, and tracing
-- all use the same configuration - there's no risk of mismatch.
--
-- Example usage:
--
-- @
-- withEvalCfg (isEvalStats opts) (isValues opts) (isTrace opts) $
--   \\(_ :: Proxy cfg) ->
--     runWithStoreEffectsIOT @cfg opts $
--       evalExprLocT expr  -- config inferred from environment
-- @
evalExprLocT
  :: forall e t f m
   . (MonadNix e t f m, HasEvalCfg e)
  => NExprLoc
  -> m (NValue t f m)
evalExprLocT expr =
  let
    -- Apply AST-level desugaring before evaluation
    desugared = desugarExprLoc expr
  in case singTrace @(CtxCfg e) of
    STrue ->
      -- Tracing enabled: use tracing evaluator
      join $ (`runReaderT` (0 :: Int)) $ evalWithTracingAndMetaInfo desugared
    SFalse -> case singStats @(CtxCfg e) of
      STrue -> do
        -- Stats enabled: use stats evaluator (still need runtime lookup for handle)
        mstats <- askEvalStats
        case mstats of
          Just s  -> evalWithStatsAndMetaInfo s desugared
          Nothing -> Eval.evalWithMetaInfo desugared  -- fallback
      SFalse ->
        -- Stats disabled: use plain evaluator (fastest path, zero overhead)
        Eval.evalWithMetaInfo desugared
{-# INLINABLE evalExprLocT #-}

exec :: (MonadNix e t f m, MonadInstantiate m, HasProvCfg (CtxCfg e)) => Vector Text -> m (NValue t f m)
exec args = do
  res <- exec' args
  case res of
    Left err -> throwError err
    Right expr -> evalExprLoc expr

-- | Type-parameterized version of 'exec' with compile-time feature dispatch.
-- Uses 'CtxCfg e' from the environment for all config dispatch, ensuring
-- stats, provenance, and tracing settings stay consistent.
execT
  :: forall e t f m
   . (MonadNix e t f m, MonadInstantiate m, HasEvalCfg e)
  => Vector Text
  -> m (NValue t f m)
execT args = do
  res <- exec' args
  case res of
    Left err -> throwError err
    Right expr -> evalExprLocT expr
{-# INLINABLE execT #-}

-- Please, delete `nix` from the name
nixInstantiateExpr
  :: (MonadNix e t f m, MonadInstantiate m, HasProvCfg (CtxCfg e)) => Text -> m (NValue t f m)
nixInstantiateExpr s = do
  res <- instantiateExpr s
  case res of
    Left err -> throwError err
    Right expr -> evalExprLoc expr

-- | Type-parameterized version of 'nixInstantiateExpr' with compile-time feature dispatch.
-- Uses 'CtxCfg e' from the environment for all config dispatch, ensuring
-- stats, provenance, and tracing settings stay consistent.
nixInstantiateExprT
  :: forall e t f m
   . (MonadNix e t f m, MonadInstantiate m, HasEvalCfg e)
  => Text
  -> m (NValue t f m)
nixInstantiateExprT s = do
  res <- instantiateExpr s
  case res of
    Left err -> throwError err
    Right expr -> evalExprLocT expr
{-# INLINABLE nixInstantiateExprT #-}
