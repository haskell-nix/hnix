{-# language AllowAmbiguousTypes #-}
{-# language CPP #-}
{-# language ConstraintKinds #-}
{-# language PartialTypeSignatures #-}
{-# language RankNTypes #-}
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
import qualified Data.HashMap.Lazy             as M
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as Text
import           Nix.Atoms
import           Nix.Cited
import           Nix.Convert
import           Nix.Effects
import           Nix.Eval                      as Eval
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.Options
import           Nix.Pretty
import           Nix.Render
import           Nix.Scope
import           Nix.String
import           Nix.String.Coerce
import           Nix.Thunk
import           Nix.Value
import           Nix.Value.Equal
import           Nix.Value.Monad
import           Prettyprinter
import qualified Text.Show.Pretty              as PS

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

type MonadNix e t f m =
  ( Has e SrcSpan
  , Has e Options
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

nverr :: forall e t f s m a . (MonadNix e t f m, Exception s) => s -> m a
nverr = evalError @(NValue t f m)

askSpan :: forall e m . (MonadReader e m, Has e SrcSpan) => m SrcSpan
askSpan = askLocal

wrapExprLoc :: SrcSpan -> NExprLocF r -> NExprLoc
wrapExprLoc span x = Fix $ NSymAnn span "<?>" <$ x
{-# inline wrapExprLoc #-}

--  2021-01-07: NOTE: This instance belongs to be beside MonadEval type class.
-- Currently instance is stuck in orphanage between the requirements to be MonadEval, aka Eval stage, and emposed requirement to be MonadNix (Execution stage). MonadNix constraint tries to put the cart before horse and seems superflous, since Eval in Nix also needs and can throw exceptions. It is between `nverr` and `evalError`.
instance MonadNix e t f m => MonadEval (NValue t f m) m where
  freeVariable var =
    nverr @e @t @f $ ErrorCall $ toString @Text $ "Undefined variable '" <> coerce var <> "'"

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
      maybe
        ("Inheriting unknown attribute: " <> attr)
        (\ s -> "Could not look up attribute " <> attr <> " in " <> show (prettyNValue s))
        ms
       where
        attr = Text.intercalate "." $ NE.toList $ coerce ks

  evalCurPos =
    do
      scope                  <- askScopes
      span@(SrcSpan delta _) <- askSpan
      addProvenance @_ @_ @(NValue t f m)
        (Provenance scope . NSymAnnF span $ coerce @Text "__curPos") <$>
          toValue delta

  evaledSym name val =
    do
      scope <- askScopes
      span  <- askSpan
      pure $
        addProvenance @_ @_ @(NValue t f m)
          (Provenance scope $ NSymAnnF span name)
          val

  evalConstant c =
    do
      scope <- askScopes
      span  <- askSpan
      pure $ mkNVConstantWithProvenance scope span c

  evalString =
    maybe
      (nverr $ ErrorCall "Failed to assemble string")
      (\ ns ->
        do
          scope <- askScopes
          span  <- askSpan
          pure $ mkNVStrWithProvenance scope span ns
      )
      <=< assembleString

  evalLiteralPath p =
    do
      scope <- askScopes
      span  <- askSpan
      mkNVPathWithProvenance scope span p <$> toAbsolutePath @t @f @m p

  evalEnvPath p =
    do
      scope <- askScopes
      span  <- askSpan
      mkNVPathWithProvenance scope span p <$> findEnvPath @t @f @m (coerce p)

  evalUnary op arg =
    do
      scope <- askScopes
      span  <- askSpan
      execUnaryOp scope span op arg

  evalBinary op larg rarg =
    do
      scope <- askScopes
      span  <- askSpan
      execBinaryOp scope span op larg rarg

  evalWith c b =
    do
      scope <- askScopes
      span  <- askSpan
      let f = join $ addProvenance . Provenance scope . NWithAnnF span Nothing . pure
      f <$> evalWithAttrSet c b

  evalIf c tVal fVal =
    do
      scope <- askScopes
      span  <- askSpan
      bl <- fromValue c

      let
        fun x y = addProvenance (Provenance scope $ NIfAnnF span (pure c) x y)
        falseVal = (fun Nothing =<< pure) <$> fVal
        trueVal = (flip fun Nothing =<< pure) <$> tVal

      bool
        falseVal
        trueVal
        bl

  evalAssert c body =
    do
      span <- askSpan
      b <- fromValue c
      bool
        (nverr $ Assertion span c)
        (do
          scope <- askScopes
          join (addProvenance . Provenance scope . NAssertAnnF span (pure c) . pure) <$> body
        )
        b

  evalApp f x =
    do
      scope <- askScopes
      span  <- askSpan
      mkNVAppOpWithProvenance scope span (pure f) Nothing <$> (callFunc f =<< defer x)

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
    do
      scope <- askScopes
      span  <- askSpan
      pure $ mkNVClosureWithProvenance scope span (void p) (fmap snd . flip (k @()) (const (fmap (mempty ,))) . pure)

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
    frames <- askFrames
    when (length frames > 2000) $ throwError $ ErrorCall "Function call stack exhausted"

    fun' <- demand fun
    case fun' of
      NVBuiltin name f    ->
        do
          span <- askSpan
          withFrame Info ((Calling @m @(NValue t f m)) name span) $ f arg -- Is this cool?
      NVClosure _params f -> f arg
      (NVSet _ m) | Just f <- M.lookup "__functor" m ->
        (`callFunc` arg) =<< (`callFunc` fun') f
      _x -> throwError $ ErrorCall $ "Attempt to call non-function: " <> show _x

execUnaryOp
  :: forall e t f m
   . (Framed e m, MonadCited t f m, Show t)
  => Scopes m (NValue t f m)
  -> SrcSpan
  -> NUnaryOp
  -> NValue t f m
  -> m (NValue t f m)
execUnaryOp scope span op arg =
  case arg of
    NVConstant c ->
      case (op, c) of
        (NNeg, NInt   i) -> mkUnaryOp NInt negate i
        (NNeg, NFloat f) -> mkUnaryOp NFloat negate f
        (NNot, NBool  b) -> mkUnaryOp NBool not b
        _seq ->
          throwError $ ErrorCall $ "unsupported argument type for unary operator " <> show _seq
    _x ->
      throwError $ ErrorCall $ "argument to unary operator must evaluate to an atomic type: " <> show _x
 where
  mkUnaryOp :: (a -> NAtom) -> (a -> a) -> a -> m (NValue t f m)
  mkUnaryOp c b a = pure . mkNVUnaryOpWithProvenance scope span op (pure arg) . NVConstant $ c (b a)

execBinaryOp
  :: forall e t f m
   . (MonadNix e t f m, MonadEval (NValue t f m) m)
  => Scopes m (NValue t f m)
  -> SrcSpan
  -> NBinaryOp
  -> NValue t f m
  -> m (NValue t f m)
  -> m (NValue t f m)
execBinaryOp scope span op lval rarg =
  case op of
    NEq   -> helperEq id
    NNEq  -> helperEq not
    NOr   -> helperLogic flip True
    NAnd  -> helperLogic id   False
    NImpl -> helperLogic id   True
    _     ->
      do
        rval  <- rarg
        rval' <- demand rval
        lval' <- demand lval

        execBinaryOpForced scope span op lval' rval'

 where

  helperEq :: (Bool -> Bool) -> m (NValue t f m)
  helperEq flag =
    do
      rval <- rarg
      eq <- valueEqM lval rval
      boolOp rval $ flag eq

  helperLogic flp flag =
    flp bool
      (bypass flag)
      (do
          rval <- rarg
          x <- fromValue rval
          boolOp rval x
      )
      =<< fromValue lval

  boolOp rval = toBoolOp $ pure rval

  bypass      = toBoolOp Nothing

  toBoolOp :: Maybe (NValue t f m) -> Bool -> m (NValue t f m)
  toBoolOp r b =
    pure $ mkNVBinaryOpWithProvenance scope span op (pure lval) r $ NVConstant $ NBool b

execBinaryOpForced
  :: forall e t f m
   . (MonadNix e t f m, MonadEval (NValue t f m) m)
  => Scopes m (NValue t f m)
  -> SrcSpan
  -> NBinaryOp
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)

execBinaryOpForced scope span op lval rval =
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
        (NVList ls, NVList rs) -> pure $ mkListP $ ls <> rs
        _ -> unsupportedTypes

    NUpdate ->
      case (lval, rval) of
        (NVSet lp ls, NVSet rp rs) -> pure $ mkSetP (rp <> lp) (rs <> ls)
        (NVSet lp ls, NVConstant NNull) -> pure $ mkSetP lp ls
        (NVConstant NNull, NVSet rp rs) -> pure $ mkSetP rp rs
        _ -> unsupportedTypes

    NPlus ->
      case (lval, rval) of
        (NVConstant _, NVConstant _) -> mkBinNumOp (+)
        (NVStr ls, NVStr rs) -> pure $ mkStrP (ls <> rs)
        (NVStr ls, NVPath p) ->
          mkStrP . (ls <>) <$>
            coercePathToNixString CopyToStore p
        (NVPath ls, NVStr rs) ->
          maybe
            (throwError $ ErrorCall "A string that refers to a store path cannot be appended to a path.") -- data/nix/src/libexpr/eval.cc:1412
            (\ rs2 -> mkPathP <$> toAbsolutePath @t @f (ls <> coerce (toString rs2)))
            (getStringNoContext rs)
        (NVPath ls, NVPath rs) -> mkPathP <$> toAbsolutePath @t @f (ls <> rs)

        (ls@NVSet{}, NVStr rs) ->
          mkStrP . (<> rs) <$>
            coerceAnyToNixString callFunc DontCopyToStore ls
        (NVStr ls, rs@NVSet{}) ->
          mkStrP . (ls <>) <$>
            coerceAnyToNixString callFunc DontCopyToStore rs
        _ -> unsupportedTypes
    _other   -> shouldBeAlreadyHandled

 where
  addProv :: NValue t f m -> NValue t f m
  addProv =
    mkNVBinaryOpWithProvenance scope span op (pure lval) (pure rval)

  mkBoolP :: Bool -> m (NValue t f m)
  mkBoolP = pure . addProv . NVConstant . NBool

  mkIntP :: Integer -> m (NValue t f m)
  mkIntP = pure . addProv . NVConstant . NInt

  mkFloatP :: Float -> m (NValue t f m)
  mkFloatP = pure . addProv . NVConstant . NFloat

  mkListP :: [NValue t f m] -> NValue t f m
  mkListP = addProv . NVList

  mkStrP :: NixString -> NValue t f m
  mkStrP = addProv . NVStr

  mkPathP :: Path -> NValue t f m
  mkPathP = addProv . NVPath

  mkSetP :: (PositionSet -> AttrSet (NValue t f m) -> NValue t f m)
  mkSetP x s = addProv $ NVSet x s

  mkCmpOp :: (forall a. Ord a => a -> a -> Bool) -> m (NValue t f m)
  mkCmpOp op = case (lval, rval) of
    (NVConstant l, NVConstant r) -> mkBoolP $ l `op` r
    (NVStr l, NVStr r) -> mkBoolP $ l `op` r
    _ -> unsupportedTypes

  mkBinNumOp :: (forall a. Num a => a -> a -> a) -> m (NValue t f m)
  mkBinNumOp op = mkBinNumOp' op op

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
  maybe
    (throwError $ ErrorCall $ "expected string with no context, but got " <> show ns)
    pure
    (getStringNoContext ns)

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
          bool
            (prettyNix $ Fix $ Fix (NSym "?") <$ x)
            (pretty $ PS.ppShow $ void x)
            (getVerbosity opts >= Chatty)
        msg x = pretty ("eval: " <> replicate depth ' ') <> x
      loc <- renderLocation span $ msg rendered <> " ...\n"
      putStr $ show loc
      res <- k v'
      print $ msg rendered <> " ...done"
      pure res

evalWithTracingAndMetaInfo
  :: forall e t f m
  . MonadNix e t f m
  => NExprLoc
  -> ReaderT Int m (m (NValue t f m))
evalWithTracingAndMetaInfo =
  adi
    addMetaInfo
    (addTracing Eval.evalContent)
  where
  addMetaInfo :: (NExprLoc -> ReaderT r m a) -> NExprLoc -> ReaderT r m a
  addMetaInfo = (ReaderT .) . flip . (Eval.addMetaInfo .) . flip . (runReaderT .)

evalExprLoc :: forall e t f m . MonadNix e t f m => NExprLoc -> m (NValue t f m)
evalExprLoc expr =
  do
    opts <- askOptions
    let
      pTracedAdi =
        bool
          Eval.evalWithMetaInfo
          (join . (`runReaderT` (0 :: Int)) . evalWithTracingAndMetaInfo)
          (isTrace opts)
    pTracedAdi expr

exec :: (MonadNix e t f m, MonadInstantiate m) => [Text] -> m (NValue t f m)
exec args = either throwError evalExprLoc =<< exec' args

-- Please, delete `nix` from the name
nixInstantiateExpr
  :: (MonadNix e t f m, MonadInstantiate m) => Text -> m (NValue t f m)
nixInstantiateExpr s = either throwError evalExprLoc =<< instantiateExpr s
