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

import           Prelude                 hiding ( putStr
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

nvConstantP
  :: MonadCited t f m
  => Provenance m (NValue t f m)
  -> NAtom
  -> NValue t f m
nvConstantP p x = addProvenance p $ nvConstant x

nvStrP
  :: MonadCited t f m
  => Provenance m (NValue t f m)
  -> NixString
  -> NValue t f m
nvStrP p ns = addProvenance p $ nvStr ns

nvPathP
  :: MonadCited t f m
  => Provenance m (NValue t f m)
  -> Path
  -> NValue t f m
nvPathP p x = addProvenance p $ nvPath x

nvListP
  :: MonadCited t f m
  => Provenance m (NValue t f m)
  -> [NValue t f m]
  -> NValue t f m
nvListP p l = addProvenance p $ nvList l

nvSetP
  :: MonadCited t f m
  => Provenance m (NValue t f m)
  -> PositionSet
  -> AttrSet (NValue t f m)
  -> NValue t f m
nvSetP p x s = addProvenance p $ nvSet x s

nvClosureP
  :: MonadCited t f m
  => Provenance m (NValue t f m)
  -> Params ()
  -> (NValue t f m -> m (NValue t f m))
  -> NValue t f m
nvClosureP p x f = addProvenance p $ nvClosure x f

nvBuiltinP
  :: MonadCited t f m
  => Provenance m (NValue t f m)
  -> VarName
  -> (NValue t f m -> m (NValue t f m))
  -> NValue t f m
nvBuiltinP p name f = addProvenance p $ nvBuiltin name f

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

currentPos :: forall e m . (MonadReader e m, Has e SrcSpan) => m SrcSpan
currentPos = asks $ view hasLens

wrapExprLoc :: SrcSpan -> NExprLocF r -> NExprLoc
wrapExprLoc span x = Fix $ NSymAnn span "<?>" <$ x
{-# inline wrapExprLoc #-}

--  2021-01-07: NOTE: This instance belongs to be beside MonadEval type class.
-- Currently instance is stuck in orphanage between the requirements to be MonadEval, aka Eval stage, and emposed requirement to be MonadNix (Execution stage). MonadNix constraint tries to put the cart before horse and seems superflous, since Eval in Nix also needs and can throw exceptions. It is between `nverr` and `evalError`.
instance MonadNix e t f m => MonadEval (NValue t f m) m where
  freeVariable var =
    nverr @e @t @f $ ErrorCall $ toString @Text $ "Undefined variable '" <> coerce var <> "'"

  synHole name = do
    span  <- currentPos
    scope <- currentScopes
    evalError @(NValue t f m) $ SynHole $
      SynHoleInfo
        { _synHoleInfo_expr  = NSynHoleAnn span name
        , _synHoleInfo_scope = scope
        }


  attrMissing ks ms =
    evalError @(NValue t f m) $ ErrorCall $ toString $
      maybe
        ("Inheriting unknown attribute: " <> attr)
        (\ s ->
          "Could not look up attribute " <> attr <> " in " <> show (prettyNValue s)
        )
        ms
       where
        attr = Text.intercalate "." $ NE.toList $ coerce ks

  evalCurPos = do
    scope                  <- currentScopes
    span@(SrcSpan delta _) <- currentPos
    addProvenance @_ @_ @(NValue t f m)
      (Provenance scope $ NSymAnnF span (coerce @Text "__curPos")) <$>
        toValue delta

  evaledSym name val = do
    scope <- currentScopes
    span  <- currentPos
    pure $
      addProvenance @_ @_ @(NValue t f m)
        (Provenance scope $ NSymAnnF span name)
        val

  evalConstant c = do
    scope <- currentScopes
    span  <- currentPos
    pure $ join (nvConstantP . Provenance scope . NConstantAnnF span) c

  evalString =
    maybe
      (nverr $ ErrorCall "Failed to assemble string")
      (\ ns ->
        do
          scope <- currentScopes
          span  <- currentPos
          pure $
            join
              (nvStrP
                . Provenance
                  scope
                  . NStrAnnF span . DoubleQuoted . one . Plain . stringIgnoreContext
              )
              ns
      )
      <=< assembleString

  evalLiteralPath p = do
    scope <- currentScopes
    span  <- currentPos
    let
      evalPath :: Path -> m (NValue t f m)
      evalPath p1 =
        fmap
          (g p1)
          (f p1)
       where
        g :: Path -> Path -> NValue t f m
        g = nvPathP . Provenance scope . NLiteralPathAnnF span
        f :: Path -> m Path
        f = toAbsolutePath @t @f @m
    evalPath p

  evalEnvPath p = do
    scope <- currentScopes
    span  <- currentPos
    nvPathP (Provenance scope $ NEnvPathAnnF span p) <$>
      findEnvPath @t @f @m (coerce p)

  evalUnary op arg = do
    scope <- currentScopes
    span  <- currentPos
    execUnaryOp scope span op arg

  evalBinary op larg rarg = do
    scope <- currentScopes
    span  <- currentPos
    execBinaryOp scope span op larg rarg

  evalWith c b = do
    scope <- currentScopes
    span  <- currentPos
    let f = join $ addProvenance . Provenance scope . NWithAnnF span Nothing . pure
    f <$> evalWithAttrSet c b

  evalIf c tVal fVal = do
    scope <- currentScopes
    span  <- currentPos
    bl <- fromValue c

    let
      fun x y = addProvenance (Provenance scope $ NIfAnnF span (pure c) x y)
      -- Note: join acts as \ f x -> f x x
      falseVal = join (fun Nothing . pure) <$> fVal
      trueVal = join (flip fun Nothing . pure) <$> tVal

    bool
      falseVal
      trueVal
      bl

  evalAssert c body =
    do
      span <- currentPos
      b <- fromValue c
      bool
        (nverr $ Assertion span c)
        (do
          scope <- currentScopes
          join (addProvenance . Provenance scope . NAssertAnnF span (pure c) . pure) <$> body
        )
        b

  evalApp f x = do
    scope <- currentScopes
    span  <- currentPos
    addProvenance (Provenance scope $ NBinaryAnnF span NApp (pure f) Nothing) <$>
      (callFunc f =<< defer x)

  evalAbs p k = do
    let
      fk = flip k
    scope <- currentScopes
    span  <- currentPos
    pure $
      nvClosureP
        (Provenance scope $ NAbsAnnF span (Nothing <$ p) Nothing)
        (void p)
        $ fmap snd . fk (const $ fmap ((), )) . pure

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
    frames :: Frames <- asks $ view hasLens
    when (length frames > 2000) $ throwError $ ErrorCall "Function call stack exhausted"

    fun' <- demand fun
    case fun' of
      NVClosure _params f -> f arg
      NVBuiltin name f    ->
        do
          span <- currentPos
          withFrame Info ((Calling @m @(NValue t f m)) (coerce name) span) $ f arg -- Is this cool?
      (NVSet _ m) | Just f <- M.lookup "__functor" m ->
        (`callFunc` arg) =<< (`callFunc` fun') =<< demand f
      _x -> throwError $ ErrorCall $ "Attempt to call non-function: " <> show _x

execUnaryOp
  :: (Framed e m, MonadCited t f m, Show t)
  => Scopes m (NValue t f m)
  -> SrcSpan
  -> NUnaryOp
  -> NValue t f m
  -> m (NValue t f m)
execUnaryOp scope span op arg =
  case arg of
    NVConstant c ->
      case (op, c) of
        (NNeg, NInt   i) -> unaryOp $ NInt   (  - i)
        (NNeg, NFloat f) -> unaryOp $ NFloat (  - f)
        (NNot, NBool  b) -> unaryOp $ NBool  (not b)
        _seq ->
          throwError $ ErrorCall $ "unsupported argument type for unary operator " <> show _seq
    _x ->
      throwError $ ErrorCall $ "argument to unary operator must evaluate to an atomic type: " <> show _x
 where
  unaryOp = pure . nvConstantP (Provenance scope $ NUnaryAnnF span op $ pure arg)

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
    pure $
      nvConstantP
        (Provenance scope $ NBinaryAnnF span op (pure lval) r)
        (NBool b)

execBinaryOpForced
  :: forall e t f m
   . (MonadNix e t f m, MonadEval (NValue t f m) m)
  => Scopes m (NValue t f m)
  -> SrcSpan
  -> NBinaryOp
  -> NValue t f m
  -> NValue t f m
  -> m (NValue t f m)

execBinaryOpForced scope span op lval rval = case op of
  NLt    -> compare (<)
  NLte   -> compare (<=)
  NGt    -> compare (>)
  NGte   -> compare (>=)
  NMinus -> numBinOp (-)
  NMult  -> numBinOp (*)
  NDiv   -> numBinOp' div (/)
  NConcat ->
    case (lval, rval) of
      (NVList ls, NVList rs) -> pure $ nvListP prov $ ls <> rs
      _ -> unsupportedTypes

  NUpdate ->
    case (lval, rval) of
      (NVSet lp ls, NVSet rp rs) -> pure $ nvSetP prov (rp <> lp) (rs <> ls)
      (NVSet lp ls, NVConstant NNull) -> pure $ nvSetP prov lp ls
      (NVConstant NNull, NVSet rp rs) -> pure $ nvSetP prov rp rs
      _ -> unsupportedTypes

  NPlus ->
    case (lval, rval) of
      (NVConstant _, NVConstant _) -> numBinOp (+)

      (NVStr ls, NVStr rs) -> pure $ nvStrP prov (ls <> rs)
      (NVStr ls, NVPath p) ->
        (\rs2 -> nvStrP prov (ls <> rs2)) <$>
          coercePathToNixString CopyToStore p
      (NVPath ls, NVStr rs) ->
        maybe
          (throwError $ ErrorCall "A string that refers to a store path cannot be appended to a path.") -- data/nix/src/libexpr/eval.cc:1412
          (\ rs2 ->
            nvPathP prov <$>
              toAbsolutePath @t @f (ls <> coerce (toString rs2))
          )
          (getStringNoContext rs)
      (NVPath ls, NVPath rs) -> nvPathP prov <$> toAbsolutePath @t @f (ls <> rs)

      (ls@NVSet{}, NVStr rs) ->
        (\ls2 -> nvStrP prov (ls2 <> rs)) <$>
          coerceAnyToNixString callFunc DontCopyToStore ls
      (NVStr ls, rs@NVSet{}) ->
        (\rs2 -> nvStrP prov (ls <> rs2)) <$>
          coerceAnyToNixString callFunc DontCopyToStore rs
      _ -> unsupportedTypes

  NEq   -> alreadyHandled
  NNEq  -> alreadyHandled
  NAnd  -> alreadyHandled
  NOr   -> alreadyHandled
  NImpl -> alreadyHandled
  NApp  -> throwError $ ErrorCall "NApp should be handled by evalApp"

 where
  prov :: Provenance m (NValue t f m)
  prov = Provenance scope $ NBinaryAnnF span op (pure lval) (pure rval)

  toBool = pure . nvConstantP prov . NBool
  compare :: (forall a. Ord a => a -> a -> Bool) -> m (NValue t f m)
  compare op = case (lval, rval) of
    (NVConstant l, NVConstant r) -> toBool $ l `op` r
    (NVStr l, NVStr r) -> toBool $ l `op` r
    _ -> unsupportedTypes

  nvInt = pure . nvConstantP prov . NInt
  nvFloat = pure . nvConstantP prov . NFloat

  numBinOp :: (forall a. Num a => a -> a -> a) -> m (NValue t f m)
  numBinOp op = numBinOp' op op

  numBinOp'
    :: (Integer -> Integer -> Integer)
    -> (Float -> Float -> Float)
    -> m (NValue t f m)
  numBinOp' intOp floatOp = case (lval, rval) of
    (NVConstant l, NVConstant r) -> case (l, r) of
      (NInt   li, NInt   ri) -> nvInt $ li `intOp` ri
      (NInt   li, NFloat rf) -> nvFloat $ fromInteger li `floatOp` rf
      (NFloat lf, NInt   ri) -> nvFloat $ lf `floatOp` fromInteger ri
      (NFloat lf, NFloat rf) -> nvFloat $ lf `floatOp` rf
      _ -> unsupportedTypes
    _ -> unsupportedTypes

  unsupportedTypes = throwError $ ErrorCall $ "Unsupported argument types for binary operator " <> show op <> ": " <> show lval <> ", " <> show rval

  alreadyHandled = throwError $ ErrorCall $ "This cannot happen: operator " <> show op <> " should have been handled in execBinaryOp."


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
      opts :: Options <- asks $ view hasLens
      let
        rendered =
          if verbose opts >= Chatty
            then
              pretty $
                PS.ppShow $ void x
            else prettyNix $ Fix $ Fix (NSym "?") <$ x
        msg x = pretty ("eval: " <> replicate depth ' ') <> x
      loc <- renderLocation span $ msg rendered <> " ...\n"
      putStr $ show loc
      res <- k v'
      print $ msg rendered <> " ...done"
      pure res

evalExprLoc :: forall e t f m . MonadNix e t f m => NExprLoc -> m (NValue t f m)
evalExprLoc expr =
  do
    opts :: Options <- asks $ view hasLens
    let
      pTracedAdi =
        bool
          Eval.framedEvalExprLoc
          (join . (`runReaderT` (0 :: Int)) .
            adi
              (raise Eval.addMetaInfo)
              (addTracing Eval.evalContent)
          )
          (tracing opts)
    pTracedAdi expr
 where
  raise k f x = ReaderT $ \e -> k (\t -> runReaderT (f t) e) x

exec :: (MonadNix e t f m, MonadInstantiate m) => [Text] -> m (NValue t f m)
exec args = either throwError evalExprLoc =<< exec' args

nixInstantiateExpr
  :: (MonadNix e t f m, MonadInstantiate m) => Text -> m (NValue t f m)
nixInstantiateExpr s = either throwError evalExprLoc =<< instantiateExpr s
