{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Exec where

import           Prelude                 hiding ( putStr
                                                , putStrLn
                                                , print
                                                )

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch     hiding ( catchJust )
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import           Control.Monad.Trans.State.Strict
                                                ( StateT(..) )
import           Data.Fix
import           Data.HashMap.Lazy              ( HashMap )
import qualified Data.HashMap.Lazy             as M
import           Data.List
import qualified Data.List.NonEmpty            as NE
import           Data.List.Split
import           Data.Maybe                     ( maybeToList )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Prettyprint.Doc
import           Data.Typeable
import           Nix.Atoms
import           Nix.Cited
import           Nix.Context
import           Nix.Convert
import           Nix.Effects
import           Nix.Eval                      as Eval
import           Nix.Expr
import           Nix.Frames
import           Nix.Normal
import           Nix.Options
import           Nix.Parser
import           Nix.Pretty
import           Nix.Render
import           Nix.Scope
import           Nix.String
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value
#ifdef MIN_VERSION_haskeline
import           System.Console.Haskeline.MonadException hiding(catch)
#endif
import           System.FilePath
#ifdef MIN_VERSION_pretty_show
import qualified Text.Show.Pretty as PS
#endif

#ifdef MIN_VERSION_ghc_datasize
#if MIN_VERSION_ghc_datasize(0,2,0) && __GLASGOW_HASKELL__ >= 804
import           GHC.DataSize
#endif
#endif

type MonadCited t f m
    = ( HasCitations1 t m (NValue t f m) f
      , MonadDataContext f m
      )

nvConstantP :: MonadCited t f m => Provenance t m (NValue t f m) -> NAtom -> NValue t f m
nvConstantP p x = addProvenance p (nvConstant x)

nvStrP :: MonadCited t f m => Provenance t m (NValue t f m) -> NixString -> NValue t f m
nvStrP p ns = addProvenance p (nvStr ns)

nvPathP :: MonadCited t f m => Provenance t m (NValue t f m) -> FilePath -> NValue t f m
nvPathP p x = addProvenance p (nvPath x)

nvListP :: MonadCited t f m => Provenance t m (NValue t f m) -> [t] -> NValue t f m
nvListP p l = addProvenance p (nvList l)

nvSetP
  :: MonadCited t f m
  => Provenance t m (NValue t f m)
  -> AttrSet t
  -> AttrSet SourcePos
  -> NValue t f m
nvSetP p s x = addProvenance p (nvSet s x)

nvClosureP
  :: MonadCited t f m
  => Provenance t m (NValue t f m)
  -> Params ()
  -> (m (NValue t f m) -> m t)
  -> NValue t f m
nvClosureP p x f = addProvenance p (nvClosure x f)

nvBuiltinP
  :: MonadCited t f m
  => Provenance t m (NValue t f m)
  -> String
  -> (m (NValue t f m) -> m t)
  -> NValue t f m
nvBuiltinP p name f = addProvenance p (nvBuiltin name f)

type MonadCitedThunks t f m
  = ( MonadThunk t m (NValue t f m)
  , MonadDataErrorContext t f m
  , HasCitations1 t m (NValue t f m) f
  )

type MonadNix e t f m
  = ( Has e SrcSpan
  , Has e Options
  , Scoped t m
  , Framed e m
  , MonadFix m
  , MonadCatch m
  , MonadThrow m
  , Alternative m
  , MonadEffects t f m
  , MonadCitedThunks t f m
  )

data ExecFrame t f m = Assertion SrcSpan (NValue t f m)
    deriving (Show, Typeable)

instance MonadDataErrorContext t f m => Exception (ExecFrame t f m)

nverr
  :: forall e t f s m a . (MonadNix e t f m, Exception s)
  => s
  -> m a
nverr = evalError @(NValue t f m)

currentPos :: forall e m . (MonadReader e m, Has e SrcSpan) => m SrcSpan
currentPos = asks (view hasLens)

wrapExprLoc :: SrcSpan -> NExprLocF r -> NExprLoc
wrapExprLoc span x = Fix (Fix (NSym_ span "<?>") <$ x)

instance MonadNix e t f m => MonadEval (NValue t f m) m where
  freeVariable var =
    nverr @e @t @f
      $  ErrorCall
      $  "Undefined variable '"
      ++ Text.unpack var
      ++ "'"

  synHole name = do
    span  <- currentPos
    scope <- currentScopes
    evalError @(NValue t f m) $ SynHole $ SynHoleInfo
      { _synHoleInfo_expr  = Fix $ NSynHole_ span name
      , _synHoleInfo_scope = scope
      }

  attrMissing ks Nothing =
    evalError @(NValue t f m)
      $  ErrorCall
      $  "Inheriting unknown attribute: "
      ++ intercalate "." (map Text.unpack (NE.toList ks))

  attrMissing ks (Just s) = do
    s' <- prettyNValue s
    evalError @(NValue t f m)
      $  ErrorCall
      $  "Could not look up attribute "
      ++ intercalate "." (map Text.unpack (NE.toList ks))
      ++ " in "
      ++ show s'

  evalCurPos = do
    scope                  <- currentScopes
    span@(SrcSpan delta _) <- currentPos
    addProvenance @_ @_ @(NValue t f m)
      (Provenance scope (NSym_ span "__curPos"))
      <$> toValue delta

  evaledSym name val = do
    scope <- currentScopes
    span  <- currentPos
    pure $ addProvenance @_ @_ @(NValue t f m)
      (Provenance scope (NSym_ span name)) val

  evalConstant c = do
    scope <- currentScopes
    span  <- currentPos
    pure $ nvConstantP (Provenance scope (NConstant_ span c)) c

  evalString = assembleString >=> \case
    Just ns -> do
      scope <- currentScopes
      span  <- currentPos
      pure $ nvStrP
        (Provenance
          scope
          (NStr_ span (DoubleQuoted [Plain (hackyStringIgnoreContext ns)]))
        )
        ns
    Nothing -> nverr $ ErrorCall "Failed to assemble string"

  evalLiteralPath p = do
    scope <- currentScopes
    span  <- currentPos
    nvPathP (Provenance scope (NLiteralPath_ span p))
      <$> makeAbsolutePath @t @f @m p

  evalEnvPath p = do
    scope <- currentScopes
    span  <- currentPos
    nvPathP (Provenance scope (NEnvPath_ span p)) <$> findEnvPath @t @f @m p

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
    (\b -> addProvenance (Provenance scope (NWith_ span Nothing (Just b))) b)
      <$> evalWithAttrSet c b

  evalIf c t f = do
    scope <- currentScopes
    span  <- currentPos
    fromValue c >>= \b -> if b
      then
        (\t -> addProvenance
            (Provenance scope (NIf_ span (Just c) (Just t) Nothing))
            t
          )
          <$> t
      else
        (\f -> addProvenance
            (Provenance scope (NIf_ span (Just c) Nothing (Just f)))
            f
          )
          <$> f

  evalAssert c body = fromValue c >>= \b -> do
    span <- currentPos
    if b
      then do
        scope <- currentScopes
        (\b ->
            addProvenance (Provenance scope (NAssert_ span (Just c) (Just b))) b
          )
          <$> body
      else nverr $ Assertion span c

  evalApp f x = do
    scope <- currentScopes
    span  <- currentPos
    addProvenance (Provenance scope (NBinary_ span NApp (Just f) Nothing))
      <$> callFunc f x

  evalAbs p k = do
    scope <- currentScopes
    span  <- currentPos
    pure $ nvClosureP
      (Provenance scope (NAbs_ span (Nothing <$ p) Nothing))
      (void p)
      (\arg -> wrapValue . snd <$> k arg (\_ b -> ((), ) <$> b))

  evalError = throwError

infixl 1 `callFunc`
callFunc
  :: forall e t f m
   . MonadNix e t f m
  => NValue t f m
  -> m (NValue t f m)
  -> m (NValue t f m)
callFunc fun arg = do
  frames :: Frames <- asks (view hasLens)
  when (length frames > 2000) $ throwError $ ErrorCall
    "Function call stack exhausted"
  case fun of
    NVClosure params f -> do
      traceM $ "callFunc:NVFunction taking " ++ show params
      force ?? pure =<< f arg
    NVBuiltin name f -> do
      span <- currentPos
      force ?? pure =<< withFrame Info (Calling @m @t name span) (f arg)
    s@(NVSet m _) | Just f <- M.lookup "__functor" m -> do
      traceM "callFunc:__functor"
      force f $ (`callFunc` pure s) >=> (`callFunc` arg)
    x -> throwError $ ErrorCall $ "Attempt to call non-function: " ++ show x

execUnaryOp
  :: (Framed e m, MonadCited t f m, Show t)
  => Scopes m t
  -> SrcSpan
  -> NUnaryOp
  -> NValue t f m
  -> m (NValue t f m)
execUnaryOp scope span op arg = do
  traceM "NUnary"
  case arg of
    NVConstant c -> case (op, c) of
      (NNeg, NInt i  ) -> unaryOp $ NInt (-i)
      (NNeg, NFloat f) -> unaryOp $ NFloat (-f)
      (NNot, NBool b ) -> unaryOp $ NBool (not b)
      _ ->
        throwError
          $  ErrorCall
          $  "unsupported argument type for unary operator "
          ++ show op
    x ->
      throwError
        $  ErrorCall
        $  "argument to unary operator"
        ++ " must evaluate to an atomic type: "
        ++ show x
 where
  unaryOp = pure . nvConstantP (Provenance scope (NUnary_ span op (Just arg)))

execBinaryOp
  :: forall e t f m
   . (MonadNix e t f m, MonadEval (NValue t f m) m)
  => Scopes m t
  -> SrcSpan
  -> NBinaryOp
  -> NValue t f m
  -> m (NValue t f m)
  -> m (NValue t f m)

execBinaryOp scope span NOr larg rarg = fromNix larg >>= \l -> if l
  then orOp Nothing True
  else rarg >>= \rval -> fromNix @Bool rval >>= orOp (Just rval)
 where
  orOp r b = pure $ nvConstantP
    (Provenance scope (NBinary_ span NOr (Just larg) r))
    (NBool b)

execBinaryOp scope span NAnd larg rarg = fromNix larg >>= \l -> if l
  then rarg >>= \rval -> fromNix @Bool rval >>= andOp (Just rval)
  else andOp Nothing False
 where
  andOp r b = pure $ nvConstantP
    (Provenance scope (NBinary_ span NAnd (Just larg) r))
    (NBool b)

execBinaryOp scope span op lval rarg = do
  rval <- rarg
  let bin :: (Provenance t m (NValue t f m) -> a) -> a
      bin f = f (Provenance scope (NBinary_ span op (Just lval) (Just rval)))
      toBool = pure . bin nvConstantP . NBool
  case (lval, rval) of
    (NVConstant lc, NVConstant rc) -> case (op, lc, rc) of
      (NEq , _, _) -> toBool =<< valueEqM lval rval
      (NNEq, _, _) -> toBool . not =<< valueEqM lval rval
      (NLt , l, r) -> toBool $ l < r
      (NLte, l, r) -> toBool $ l <= r
      (NGt , l, r) -> toBool $ l > r
      (NGte, l, r) -> toBool $ l >= r
      (NAnd, _, _) ->
        nverr $ ErrorCall "should be impossible: && is handled above"
      (NOr, _, _) ->
        nverr $ ErrorCall "should be impossible: || is handled above"
      (NPlus , l      , r      ) -> numBinOp bin (+) l r
      (NMinus, l      , r      ) -> numBinOp bin (-) l r
      (NMult , l      , r      ) -> numBinOp bin (*) l r
      (NDiv  , l      , r      ) -> numBinOp' bin div (/) l r
      (NImpl , NBool l, NBool r) -> toBool $ not l || r
      _ -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (NVStr ls, NVStr rs) -> case op of
      NPlus -> pure $ bin nvStrP (ls `principledStringMappend` rs)
      NEq   -> toBool =<< valueEqM lval rval
      NNEq  -> toBool . not =<< valueEqM lval rval
      NLt   -> toBool $ ls < rs
      NLte  -> toBool $ ls <= rs
      NGt   -> toBool $ ls > rs
      NGte  -> toBool $ ls >= rs
      _     -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (NVStr _, NVConstant NNull) -> case op of
      NEq  -> toBool False
      NNEq -> toBool True
      _    -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (NVConstant NNull, NVStr _) -> case op of
      NEq  -> toBool False
      NNEq -> toBool True
      _    -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (NVSet ls lp, NVSet rs rp) -> case op of
      NUpdate -> pure $ bin nvSetP (rs `M.union` ls) (rp `M.union` lp)
      NEq     -> toBool =<< valueEqM lval rval
      NNEq    -> toBool . not =<< valueEqM lval rval
      _       -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (NVSet ls lp, NVConstant NNull) -> case op of
      NUpdate -> pure $ bin nvSetP ls lp
      NEq     -> toBool =<< valueEqM lval (nvSet M.empty M.empty)
      NNEq    -> toBool . not =<< valueEqM lval (nvSet M.empty M.empty)
      _       -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (NVConstant NNull, NVSet rs rp) -> case op of
      NUpdate -> pure $ bin nvSetP rs rp
      NEq     -> toBool =<< valueEqM (nvSet M.empty M.empty) rval
      NNEq    -> toBool . not =<< valueEqM (nvSet M.empty M.empty) rval
      _       -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (ls@NVSet{}, NVStr rs) -> case op of
      NPlus ->
        (\ls2 -> bin nvStrP (ls2 `principledStringMappend` rs))
          <$> coerceToString DontCopyToStore CoerceStringy ls
      NEq  -> toBool =<< valueEqM lval rval
      NNEq -> toBool . not =<< valueEqM lval rval
      _    -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (NVStr ls, rs@NVSet{}) -> case op of
      NPlus ->
        (\rs2 -> bin nvStrP (ls `principledStringMappend` rs2))
          <$> coerceToString DontCopyToStore CoerceStringy rs
      NEq  -> toBool =<< valueEqM lval rval
      NNEq -> toBool . not =<< valueEqM lval rval
      _    -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (NVList ls, NVList rs) -> case op of
      NConcat -> pure $ bin nvListP $ ls ++ rs
      NEq     -> toBool =<< valueEqM lval rval
      NNEq    -> toBool . not =<< valueEqM lval rval
      _       -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (NVList ls, NVConstant NNull) -> case op of
      NConcat -> pure $ bin nvListP ls
      NEq     -> toBool =<< valueEqM lval (nvList [])
      NNEq    -> toBool . not =<< valueEqM lval (nvList [])
      _       -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (NVConstant NNull, NVList rs) -> case op of
      NConcat -> pure $ bin nvListP rs
      NEq     -> toBool =<< valueEqM (nvList []) rval
      NNEq    -> toBool . not =<< valueEqM (nvList []) rval
      _       -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (NVPath p, NVStr ns) -> case op of
      NEq   -> toBool False -- From eqValues in nix/src/libexpr/eval.cc
      NNEq  -> toBool True
      NPlus -> bin nvPathP <$> makeAbsolutePath @t @f
        (p `mappend` Text.unpack (hackyStringIgnoreContext ns))
      _ -> nverr $ ErrorCall $ unsupportedTypes lval rval

    (NVPath ls, NVPath rs) -> case op of
      NPlus -> bin nvPathP <$> makeAbsolutePath @t @f (ls ++ rs)
      _     -> nverr $ ErrorCall $ unsupportedTypes lval rval

    _ -> case op of
      NEq  -> toBool False
      NNEq -> toBool True
      _    -> nverr $ ErrorCall $ unsupportedTypes lval rval
 where
  unsupportedTypes :: Show a => a -> a -> String
  unsupportedTypes lval rval =
    "Unsupported argument types for binary operator "
      ++ show op
      ++ ": "
      ++ show lval
      ++ ", "
      ++ show rval

  numBinOp
    :: (forall r . (Provenance t m (NValue t f m) -> r) -> r)
    -> (forall a . Num a => a -> a -> a)
    -> NAtom
    -> NAtom
    -> m (NValue t f m)
  numBinOp bin f = numBinOp' bin f f

  numBinOp'
    :: (forall r . (Provenance t m (NValue t f m) -> r) -> r)
    -> (Integer -> Integer -> Integer)
    -> (Float -> Float -> Float)
    -> NAtom
    -> NAtom
    -> m (NValue t f m)
  numBinOp' bin intF floatF l r = case (l, r) of
    (NInt   li, NInt ri  ) -> toInt $ li `intF` ri
    (NInt   li, NFloat rf) -> toFloat $ fromInteger li `floatF` rf
    (NFloat lf, NInt ri  ) -> toFloat $ lf `floatF` fromInteger ri
    (NFloat lf, NFloat rf) -> toFloat $ lf `floatF` rf
    _                      -> nverr $ ErrorCall $ unsupportedTypes l r
   where
    toInt   = pure . bin nvConstantP . NInt
    toFloat = pure . bin nvConstantP . NFloat

-- | Data type to avoid boolean blindness on what used to be called coerceMore
data CoercionLevel
  = CoerceStringy
  -- ^ Coerce only stringlike types: strings, paths, and appropriate sets
  | CoerceAny
  -- ^ Coerce everything but functions
  deriving (Eq,Ord,Enum,Bounded)

-- | Data type to avoid boolean blindness on what used to be called copyToStore
data CopyToStoreMode
  = CopyToStore
  -- ^ Add paths to the store as they are encountered
  | DontCopyToStore
  -- ^ Add paths to the store as they are encountered
  deriving (Eq,Ord,Enum,Bounded)

coerceToString
  :: MonadNix e t f m
  => CopyToStoreMode
  -> CoercionLevel
  -> NValue t f m
  -> m NixString
coerceToString ctsm clevel = go
 where
  go = \case
    NVConstant (NBool b)
      |
        -- TODO Return a singleton for "" and "1"
        b && clevel == CoerceAny -> pure
      $  principledMakeNixStringWithoutContext "1"
      | clevel == CoerceAny -> pure $ principledMakeNixStringWithoutContext ""
    NVConstant (NInt n) | clevel == CoerceAny ->
      pure $ principledMakeNixStringWithoutContext $ Text.pack $ show n
    NVConstant (NFloat n) | clevel == CoerceAny ->
      pure $ principledMakeNixStringWithoutContext $ Text.pack $ show n
    NVConstant NNull | clevel == CoerceAny ->
      pure $ principledMakeNixStringWithoutContext ""
    NVStr ns -> pure ns
    NVPath p
      | ctsm == CopyToStore -> storePathToNixString <$> addPath p
      | otherwise -> pure $ principledMakeNixStringWithoutContext $ Text.pack p
    NVList l | clevel == CoerceAny ->
      nixStringUnwords <$> traverse (`force` go) l

    v@(NVSet s _) | Just p <- M.lookup "__toString" s ->
      force p $ (`callFunc` pure v) >=> go

    NVSet s _ | Just p <- M.lookup "outPath" s -> force p go

    v -> throwError $ ErrorCall $ "Expected a string, but saw: " ++ show v

  nixStringUnwords =
    principledIntercalateNixString (principledMakeNixStringWithoutContext " ")
  storePathToNixString :: StorePath -> NixString
  storePathToNixString sp = principledMakeNixStringWithSingletonContext
    t
    (StringContext t DirectPath)
    where t = Text.pack $ unStorePath sp

fromStringNoContext :: MonadNix e t f m => NixString -> m Text
fromStringNoContext ns = case principledGetStringNoContext ns of
  Just str -> return str
  Nothing  -> throwError $ ErrorCall "expected string with no context"

newtype Lazy t (f :: * -> *) m a = Lazy
    { runLazy :: ReaderT (Context (Lazy t f m) t)
                        (StateT (HashMap FilePath NExprLoc) m) a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadFix
        , MonadIO
        , MonadCatch
        , MonadThrow
        , MonadReader (Context (Lazy t f m) t)
        )

instance MonadTrans (Lazy t f) where
  lift = Lazy . lift . lift

instance MonadRef m => MonadRef (Lazy t f m) where
  type Ref (Lazy t f m) = Ref m
  newRef  = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (Lazy t f m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance (MonadFile m, Monad m) => MonadFile (Lazy t f m)

#ifdef MIN_VERSION_haskeline
instance MonadException m => MonadException (Lazy t f m) where
  controlIO f = Lazy $ controlIO $ \(RunIO run) ->
      let run' = RunIO (fmap Lazy . run . runLazy)
      in runLazy <$> f run'
#endif

instance MonadStore m => MonadStore (Lazy t f m) where
  addPath' = lift . addPath'
  toFile_' n = lift . toFile_' n

instance MonadPutStr m => MonadPutStr (Lazy t f m)
instance MonadHttp m => MonadHttp (Lazy t f m)
instance MonadEnv m => MonadEnv (Lazy t f m)
instance MonadInstantiate m => MonadInstantiate (Lazy t f m)
instance MonadExec m => MonadExec (Lazy t f m)
instance MonadIntrospect m => MonadIntrospect (Lazy t f m)

instance MonadThunkId m => MonadThunkId (Lazy t f m) where
  type ThunkId (Lazy t f m) = ThunkId m

instance ( MonadFix m
         , MonadCatch m
         , MonadFile m
         , MonadStore m
         , MonadPutStr m
         , MonadHttp m
         , MonadEnv m
         , MonadInstantiate m
         , MonadExec m
         , MonadIntrospect m
         , Alternative m
         , MonadPlus m
         , MonadCitedThunks t f (Lazy t f m)
        )
         => MonadEffects t f (Lazy t f m) where
  makeAbsolutePath origPath = do
    origPathExpanded <- expandHomePath origPath
    absPath          <- if isAbsolute origPathExpanded
      then pure origPathExpanded
      else do
        cwd <- do
          mres <- lookupVar "__cur_file"
          case mres of
            Nothing -> getCurrentDirectory
            Just v  -> force v $ \case
              NVPath s -> return $ takeDirectory s
              v ->
                throwError
                  $  ErrorCall
                  $  "when resolving relative path,"
                  ++ " __cur_file is in scope,"
                  ++ " but is not a path; it is: "
                  ++ show v
        pure $ cwd <///> origPathExpanded
    removeDotDotIndirections <$> canonicalizePath absPath

-- Given a path, determine the nix file to load
  pathToDefaultNix = pathToDefaultNixFile

  findEnvPath      = findEnvPathM
  findPath         = findPathM

  importPath path = do
    traceM $ "Importing file " ++ path
    withFrame Info (ErrorCall $ "While importing file " ++ show path) $ do
      imports <- Lazy $ ReaderT $ const get
      evalExprLoc =<< case M.lookup path imports of
        Just expr -> pure expr
        Nothing   -> do
          eres <- parseNixFileLoc path
          case eres of
            Failure err ->
              throwError
                $ ErrorCall
                . show
                $ fillSep
                $ ["Parse during import failed:", err]
            Success expr -> do
              Lazy $ ReaderT $ const $ modify (M.insert path expr)
              pure expr

  derivationStrict = fromValue @(AttrSet t) >=> \s -> do
    nn <- maybe (pure False) (force ?? fromNix) (M.lookup "__ignoreNulls" s)
    s' <- M.fromList <$> mapMaybeM (handleEntry nn) (M.toList s)
    v' <- normalForm =<< toValue @(AttrSet t) @_ @(NValue t f (Lazy t f m)) s'
    nixInstantiateExpr $ "derivationStrict " ++ show (prettyNValueNF v')
   where
    mapMaybeM :: (a -> Lazy t f m (Maybe b)) -> [a] -> Lazy t f m [b]
    mapMaybeM op = foldr f (return [])
      where f x xs = op x >>= (<$> xs) . (++) . maybeToList

    handleEntry :: Bool -> (Text, t) -> Lazy t f m (Maybe (Text, t))
    handleEntry ignoreNulls (k, v) = fmap (k, ) <$> case k of
        -- The `args' attribute is special: it supplies the command-line
        -- arguments to the builder.
        -- TODO This use of coerceToString is probably not right and may
        -- not have the right arguments.
      "args"          -> force v $ fmap Just . coerceNixList
      "__ignoreNulls" -> pure Nothing
      _               -> force v $ \case
        NVConstant NNull | ignoreNulls -> pure Nothing
        v'                             -> Just <$> coerceNix v'
     where
      coerceNix :: NValue t f (Lazy t f m) -> Lazy t f m t
      coerceNix =
        fmap wrapValue . toNix <=< coerceToString CopyToStore CoerceAny

      coerceNixList :: NValue t f (Lazy t f m) -> Lazy t f m t
      coerceNixList v = do
        xs :: [t] <- fromValue @[t] v
        ys :: [t] <- traverse (\x -> force x coerceNix) xs
        v' :: NValue t f (Lazy t f m) <- toValue @[t] ys
        return $ wrapValue v'

  traceEffect = putStrLn

getRecursiveSize :: (MonadIntrospect m, Applicative f) => a -> m (NValue t f m)
getRecursiveSize = fmap (nvConstant . NInt . fromIntegral) . recursiveSize

runLazyM :: Options -> MonadIO m => Lazy t f m a -> m a
runLazyM opts =
  (`evalStateT` M.empty) . (`runReaderT` newContext opts) . runLazy

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

expandHomePath :: MonadFile m => FilePath -> m FilePath
expandHomePath ('~' : xs) = flip (++) xs <$> getHomeDirectory
expandHomePath p          = return p

-- Given a path, determine the nix file to load
pathToDefaultNixFile :: MonadFile m => FilePath -> m FilePath
pathToDefaultNixFile p = do
  isDir <- doesDirectoryExist p
  pure $ if isDir then p </> "default.nix" else p

infixr 9 <///>
(<///>) :: FilePath -> FilePath -> FilePath
x <///> y | isAbsolute y || "." `isPrefixOf` y = x </> y
          | otherwise                          = joinByLargestOverlap x y
 where
  joinByLargestOverlap (splitDirectories -> xs) (splitDirectories -> ys) =
    joinPath $ head
      [ xs ++ drop (length tx) ys | tx <- tails xs, tx `elem` inits ys ]

findPathBy
  :: forall e t f m. MonadNix e t f m
  => (FilePath -> m (Maybe FilePath))
  -> [t]
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
        ++ " (add it using $NIX_PATH or -I)"
    Just path -> return path
 where
  go :: Maybe FilePath -> t -> m (Maybe FilePath)
  go p@(Just _) _ = pure p
  go Nothing    l = force l $ fromValue >=> \(s :: HashMap Text t) -> do
    p <- resolvePath s
    force p $ fromValue >=> \(Path path) -> case M.lookup "prefix" s of
      Nothing -> tryPath path Nothing
      Just pf -> force pf $ fromValueMay >=> \case
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
    Just t  -> return t
    Nothing -> case M.lookup "uri" s of
      Just ut -> thunk $ fetchTarball (force ut pure)
      Nothing ->
        throwError
          $  ErrorCall
          $  "__nixPath must be a list of attr sets"
          ++ " with 'path' elements, but saw: "
          ++ show s

findPathM
  :: forall e t f m. MonadNix e t f m
  => [t]
  -> FilePath
  -> m FilePath
findPathM l name = findPathBy path l name
 where
  path :: MonadEffects t f m => FilePath -> m (Maybe FilePath)
  path path = do
    path   <- makeAbsolutePath @t @f path
    exists <- doesPathExist path
    return $ if exists then Just path else Nothing

findEnvPathM
  :: forall e t f m. MonadNix e t f m
  => FilePath
  -> m FilePath
findEnvPathM name = do
  mres <- lookupVar "__nixPath"
  case mres of
    Nothing -> error "impossible"
    Just x ->
      force x $ fromValue >=> \(l :: [t]) -> findPathBy nixFilePath l name
 where
  nixFilePath :: MonadEffects t f m => FilePath -> m (Maybe FilePath)
  nixFilePath path = do
    path   <- makeAbsolutePath @t @f path
    exists <- doesDirectoryExist path
    path'  <- if exists
      then makeAbsolutePath @t @f $ path </> "default.nix"
      else return path
    exists <- doesFileExist path'
    return $ if exists then Just path' else Nothing

addTracing
  :: (MonadNix e t f m, Has e Options, MonadReader Int n, Alternative n)
  => Alg NExprLocF (m a)
  -> Alg NExprLocF (n (m a))
addTracing k v = do
  depth <- ask
  guard (depth < 2000)
  local succ $ do
    v'@(Compose (Ann span x)) <- sequence v
    return $ do
      opts :: Options <- asks (view hasLens)
      let rendered = if verbose opts >= Chatty
#ifdef MIN_VERSION_pretty_show
                     then pretty $ PS.ppShow (void x)
#else
            then pretty $ show (void x)
#endif
            else prettyNix (Fix (Fix (NSym "?") <$ x))
          msg x = pretty ("eval: " ++ replicate depth ' ') <> x
      loc <- renderLocation span (msg rendered <> " ...\n")
      putStr $ show loc
      res <- k v'
      print $ msg rendered <> " ...done"
      return res

evalExprLoc
  :: forall e t f m. MonadNix e t f m
  => NExprLoc
  -> m (NValue t f m)
evalExprLoc expr = do
  opts :: Options <- asks (view hasLens)
  if tracing opts
    then join . (`runReaderT` (0 :: Int)) $ adi
      (addTracing phi)
      (raise (addStackFrames @t . addSourcePositions))
      expr
    else adi phi (addStackFrames @t . addSourcePositions) expr
 where
  phi = Eval.eval . annotated . getCompose
  raise k f x = ReaderT $ \e -> k (\t -> runReaderT (f t) e) x

fetchTarball
  :: forall e t f m. MonadNix e t f m
  => m (NValue t f m)
  -> m (NValue t f m)
fetchTarball v = v >>= \case
  NVSet s _ -> case M.lookup "url" s of
    Nothing ->
      throwError $ ErrorCall "builtins.fetchTarball: Missing url attribute"
    Just url -> force url $ go (M.lookup "sha256" s)
  v@NVStr{} -> go Nothing v
  v ->
    throwError
      $  ErrorCall
      $  "builtins.fetchTarball: Expected URI or set, got "
      ++ show v
 where
  go :: Maybe t -> NValue t f m -> m (NValue t f m)
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

  fetch :: Text -> Maybe t -> m (NValue t f m)
  fetch uri Nothing =
    nixInstantiateExpr $ "builtins.fetchTarball \"" ++ Text.unpack uri ++ "\""
  fetch url (Just t) = force t $ fromValue >=> \nsSha ->
    let sha = hackyStringIgnoreContext nsSha
    in  nixInstantiateExpr
          $  "builtins.fetchTarball { "
          ++ "url    = \""
          ++ Text.unpack url
          ++ "\"; "
          ++ "sha256 = \""
          ++ Text.unpack sha
          ++ "\"; }"

exec
  :: (MonadNix e t f m, MonadInstantiate m)
  => [String]
  -> m (NValue t f m)
exec args = either throwError evalExprLoc =<< exec' args

nixInstantiateExpr
  :: (MonadNix e t f m, MonadInstantiate m)
  => String
  -> m (NValue t f m)
nixInstantiateExpr s = either throwError evalExprLoc =<< instantiateExpr s

instance Monad m => Scoped t (Lazy t f m) where
  currentScopes = currentScopesReader
  clearScopes   = clearScopesReader @(Lazy t f m) @t
  pushScopes    = pushScopesReader
  lookupVar     = lookupVarReader







