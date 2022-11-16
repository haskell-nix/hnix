{-# language ConstraintKinds #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-name-shadowing #-}
{-# options_ghc -Wno-missing-methods #-}

module Nix.Lint where

import           Nix.Prelude
import           Relude.Unsafe                 as Unsafe ( head )
import           Control.Exception              ( throw )
import           GHC.Exception                  ( ErrorCall(ErrorCall) )
import           Control.Monad                  ( foldM )
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.Ref
import           Control.Monad.ST
import qualified Data.HashMap.Lazy             as M
-- Plese, use NonEmpty
import           Data.List                      ( intersect )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as Text
import qualified Text.Show
import           Nix.Atoms
import           Nix.Context
import           Nix.Convert
import           Nix.Eval                       ( MonadEval(..) )
import qualified Nix.Eval                      as Eval
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.Fresh
import           Nix.String
import           Nix.Options
import           Nix.Scope
import           Nix.Thunk
import           Nix.Thunk.Basic
import           Nix.Value.Monad

data TAtom
  = TInt
  | TFloat
  | TBool
  | TNull
  deriving (Show, Eq, Ord)

data NTypeF (m :: Type -> Type) r
  = TConstant [TAtom]
  | TStr
  | TList r
  | TSet (Maybe (AttrSet r))
  | TClosure (Params ())
  | TPath
  | TBuiltin Text (Symbolic m -> m r)
  deriving Functor

compareTypes :: NTypeF m r -> NTypeF m r -> Ordering
compareTypes (TConstant _)  (TConstant _)  = EQ
compareTypes (TConstant _)  _              = LT
compareTypes _              (TConstant _)  = GT
compareTypes TStr           TStr           = EQ
compareTypes TStr           _              = LT
compareTypes _              TStr           = GT
compareTypes (TList _)      (TList _)      = EQ
compareTypes (TList _)      _              = LT
compareTypes _              (TList _)      = GT
compareTypes (TSet _)       (TSet  _)      = EQ
compareTypes (TSet _)       _              = LT
compareTypes _              (TSet _)       = GT
compareTypes TClosure{}     TClosure{}     = EQ
compareTypes TClosure{}     _              = LT
compareTypes _              TClosure{}     = GT
compareTypes TPath          TPath          = EQ
compareTypes TPath          _              = LT
compareTypes _              TPath          = GT
compareTypes (TBuiltin _ _) (TBuiltin _ _) = EQ

data NSymbolicF r
  = NAny
  | NMany [r]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type SThunk (m :: Type -> Type) = NThunkF m (Symbolic m)

type SValue (m :: Type -> Type) = Ref m (NSymbolicF (NTypeF m (Symbolic m)))

data Symbolic m = SV { getSV :: SValue m } | ST { getST :: SThunk m }

instance Show (Symbolic m) where
  show _ = "<symbolic>"

everyPossible
  :: MonadAtomicRef m
  => m (Symbolic m)
everyPossible = packSymbolic NAny

mkSymbolic
  :: MonadAtomicRef m
  => [NTypeF m (Symbolic m)]
  -> m (Symbolic m)
mkSymbolic = packSymbolic . NMany

mkSymbolic1
  :: MonadAtomicRef m
  => NTypeF m (Symbolic m)
  -> m (Symbolic m)
mkSymbolic1 = mkSymbolic . one

packSymbolic
  :: MonadAtomicRef m
  => NSymbolicF (NTypeF m (Symbolic m))
  -> m (Symbolic m)
packSymbolic = fmap SV . newRef

unpackSymbolic
  :: (MonadAtomicRef m, MonadThunkId m, MonadCatch m)
  => Symbolic m
  -> m (NSymbolicF (NTypeF m (Symbolic m)))
unpackSymbolic = readRef . getSV <=< demand

type MonadLint e m =
  ( Scoped (Symbolic m) m
  , Framed e m
  , MonadAtomicRef m
  , MonadCatch m
  , MonadThunkId m
  )

symerr :: forall e m a . MonadLint e m => Text -> m a
symerr = evalError @(Symbolic m) . ErrorCall . toString

renderSymbolic :: MonadLint e m => Symbolic m -> m Text
renderSymbolic =
  (\case
    NAny     -> pure "<any>"
    NMany xs ->
      Text.intercalate ", " <$>
        traverse
          (\case
            TConstant ys ->
              pure $
                Text.intercalate ", "
                  (fmap
                    (\case
                      TInt   -> "int"
                      TFloat -> "float"
                      TBool  -> "bool"
                      TNull  -> "null"
                    )
                    ys
                  )
            TStr    -> pure "string"
            TList r ->
              fmap brackets $ renderSymbolic =<< demand r
            TSet Nothing  -> pure "<any set>"
            TSet (Just s) ->
              braces . show <$> traverse (renderSymbolic <=< demand) s
            f@(TClosure p) ->
              do
                (args, sym) <-
                  do
                    f' <- mkSymbolic1 f
                    lintApp (NAbs p mempty) f' everyPossible
                args' <- traverse renderSymbolic args
                sym'  <- renderSymbolic sym
                pure $ parens $ show args' <> " -> " <> sym'
            TPath          -> pure "path"
            TBuiltin _n _f -> pure "<builtin function>"
          )
          xs
  ) <=< unpackSymbolic
 where
  between a b c = a <> b <> c
  parens   = between "(" ")"
  brackets = between "[" "]"
  braces   = between "{" "}"

-- This function is order and uniqueness preserving (of types).
merge
  :: forall e m
   . MonadLint e m
  => NExprF ()
  -> [NTypeF m (Symbolic m)]
  -> [NTypeF m (Symbolic m)]
  -> m [NTypeF m (Symbolic m)]
merge context = go
 where
  go
    :: [NTypeF m (Symbolic m)]
    -> [NTypeF m (Symbolic m)]
    -> m [NTypeF m (Symbolic m)]
  go []       _        = stub
  go _        []       = stub
  go xxs@(x : xs) yys@(y : ys) = case (x, y) of
    (TStr , TStr ) -> (one TStr <>) <$> rest
    (TPath, TPath) -> (one TPath <>) <$> rest
    (TConstant ls, TConstant rs) ->
      (one (TConstant (ls `intersect` rs)) <>) <$> rest
    (TList l, TList r) ->
      do
        l' <- demand l
        r' <- demand r
        m <- defer $ unify context l' r'
        (one (TList m) <>) <$> rest
    (TSet x       , TSet Nothing ) -> (one (TSet x) <>) <$> rest
    (TSet Nothing , TSet x       ) -> (one (TSet x) <>) <$> rest
    (TSet (Just l), TSet (Just r)) -> do
      hm <-
        sequenceA $
          M.intersectionWith
            (\ i j ->
              do
                i'' <- i
                j'' <- j
                defer $ unify context i'' j''
            )
            (fmap demand l)
            (fmap demand r)
      handlePresence
        id
        (const ((one (TSet $ pure hm) <>) <$>))
        hm
        rest

    (TClosure{}, TClosure{}) ->
      throwError $ ErrorCall "Cannot unify functions"
    (TBuiltin _ _, TBuiltin _ _) ->
      throwError $ ErrorCall "Cannot unify builtin functions"
    _ | compareTypes x y == LT -> go xs yys
      | compareTypes x y == GT -> go xxs ys
      | otherwise              -> error "impossible"
   where
    rest :: m [NTypeF m (Symbolic m)]
    rest = go xs ys

{-
    mergeFunctions pl nl fl pr fr xs ys = do
        m <- sequenceA $ M.intersectionWith
            (\i j -> i >>= \i' -> j >>= \j' -> case (i', j') of
                    (Nothing, Nothing) -> stub
                    (_, Nothing) -> stub
                    (Nothing, _) -> stub
                    (Just i'', Just j'') ->
                        pure . pure <$> unify context i'' j'')
            (pure <$> pl) (pure <$> pr)
        let Just m' = sequenceA $ M.filter isJust m
        if M.null m'
            then go xs ys
            else do
                g <- unify context fl fr
                (TClosure (ParamSet m' False nl) g :)
                    <$> go xs ys
-}

-- | Result @== NMany []@ -> @unify@ fails.
unify
  :: forall e m a
   . MonadLint e m
  => NExprF a
  -> Symbolic m
  -> Symbolic m
  -> m (Symbolic m)
unify (void -> context) (SV x) (SV y) = do
  x' <- readRef x
  y' <- readRef y
  case (x', y') of
    (NAny, _) ->
      do
        writeRef x y'
        pure $ SV y
    (_, NAny) ->
      do
        writeRef y x'
        pure $ SV x
    (NMany xs, NMany ys) ->
      handlePresence
        (
          -- x' <- renderSymbolic (Symbolic x)
          -- y' <- renderSymbolic (Symbolic y)
          throwError $ ErrorCall "Cannot unify "
                  -- <> show x' <> " with " <> show y'
                  --  <> " in context: " <> show context
        )
        (\ m ->
          do
            let
              nm = NMany m
            writeRef x   nm
            writeRef y   nm
            packSymbolic nm
        )
        =<< merge context xs ys
unify _ _ _ = error "The unexpected hath transpired!"

-- These aren't worth defining yet, because once we move to Hindley-Milner,
-- we're not going to be managing Symbolic values this way anymore.

instance ToValue Bool m (Symbolic m) where

instance ToValue [Symbolic m] m (Symbolic m) where

instance FromValue NixString m (Symbolic m) where

instance FromValue (AttrSet (Symbolic m), PositionSet) m (Symbolic m) where

instance ToValue (AttrSet (Symbolic m), PositionSet) m (Symbolic m) where

instance (MonadThunkId m, MonadAtomicRef m, MonadCatch m)
  => MonadValue (Symbolic m) m where

  defer :: m (Symbolic m) -> m (Symbolic m)
  defer = fmap ST . thunk

  demand :: Symbolic m -> m (Symbolic m)
  demand (ST v) = demand =<< force v
  demand (SV v) = pure (SV v)


instance (MonadThunkId m, MonadAtomicRef m, MonadCatch m)
  => MonadValueF (Symbolic m) m where

  demandF :: (Symbolic m -> m r) -> Symbolic m -> m r
  demandF f (ST v) = demandF f =<< force v
  demandF f (SV v) = f (SV v)


instance MonadLint e m => MonadEval (Symbolic m) m where
  freeVariable var = symerr $ "Undefined variable '" <> coerce var <> "'"

  attrMissing ks ms =
    evalError @(Symbolic m) . ErrorCall . toString $
      maybe
        ("Inheriting unknown attribute: " <> attr)
        (\ s ->  "Could not look up attribute " <> attr <> " in " <> show s)
        ms
   where
    attr = Text.intercalate "." $ NE.toList $ coerce ks

  evalCurPos =
    do
      f <- mkSymbolic1 TPath
      l <- mkSymbolic1 $ TConstant $ one TInt
      c <- mkSymbolic1 $ TConstant $ one TInt
      mkSymbolic1 $ TSet . pure $ M.fromList [("file", f), ("line", l), ("col", c)]

  evalConstant c = mkSymbolic1 $ fun c
   where
    fun =
      \case
        NURI   _ -> TStr
        NInt   _ -> TConstant $ one TInt
        NFloat _ -> TConstant $ one TFloat
        NBool  _ -> TConstant $ one TBool
        NNull    -> TConstant $ one TNull

  evalString      = const $ mkSymbolic1 TStr
  evalLiteralPath = const $ mkSymbolic1 TPath
  evalEnvPath     = const $ mkSymbolic1 TPath

  evalUnary op arg =
    unify (NUnary op arg) arg =<< mkSymbolic1 (TConstant [TInt, TBool])

  evalBinary = lintBinaryOp

  -- The scope is deliberately wrapped in a thunk here, since it is evaluated
  -- each time a name is looked up within the weak scope, and we want to be
  -- sure the action it evaluates is to force a thunk, so its value is only
  -- computed once.
  evalWith scope body =
    do
      s <- unpackSymbolic =<< demand =<< defer scope

      pushWeakScope
        (case s of
          NMany [TSet (Just (coerce -> scope))] -> pure scope
          NMany [TSet Nothing] -> error "NYI: with unknown"
          _ -> throwError $ ErrorCall "scope must be a set in with statement"
        )
        body

  evalIf cond t f =
    do
      t' <- t
      f' <- f
      let e = unify (NIf cond t' f')
      e t' f' <* (e cond =<< mkSymbolic1 (TConstant $ one TBool))

  evalAssert cond body =
    do
      body' <- body
      body' <$ (unify (NAssert cond body') cond =<< mkSymbolic1 (TConstant $ one TBool))

  evalApp = (fmap snd .) . lintApp (join NApp mempty)
  evalAbs params _ = mkSymbolic1 (TClosure $ void params)

  evalError = throwError

lintBinaryOp
  :: forall e m
   . (MonadLint e m, MonadEval (Symbolic m) m)
  => NBinaryOp
  -> Symbolic m
  -> m (Symbolic m)
  -> m (Symbolic m)
lintBinaryOp op lsym rarg =
  do
    rsym <- rarg
    y    <- defer everyPossible

    check lsym rsym $
      case op of
        NEq     -> [TConstant [TInt, TBool, TNull], TStr, TList y]
        NNEq    -> [TConstant [TInt, TBool, TNull], TStr, TList y]

        NLt     -> one $ TConstant [TInt, TBool, TNull]
        NLte    -> one $ TConstant [TInt, TBool, TNull]
        NGt     -> one $ TConstant [TInt, TBool, TNull]
        NGte    -> one $ TConstant [TInt, TBool, TNull]

        NAnd    -> one $ TConstant $ one TBool
        NOr     -> one $ TConstant $ one TBool
        NImpl   -> one $ TConstant $ one TBool

        -- jww (2018-04-01): NYI: Allow Path + Str
        NPlus   -> [TConstant $ one TInt, TStr, TPath]
        NMinus  -> one $ TConstant $ one TInt
        NMult   -> one $ TConstant $ one TInt
        NDiv    -> one $ TConstant $ one TInt

        NUpdate -> one $ TSet mempty

        NConcat -> one $ TList y
#if __GLASGOW_HASKELL__ < 810
        _ -> fail "Should not be possible"  -- symerr or this fun signature should be changed to work in type scope
#endif



 where
  check lsym rsym xs =
    do
      let
        contextUnify = unify $ NBinary op lsym rsym

      m <- mkSymbolic xs
      _ <- contextUnify lsym m
      _ <- contextUnify rsym m
      contextUnify lsym rsym

infixl 1 `lintApp`
lintApp
  :: forall e m
   . MonadLint e m
  => NExprF ()
  -> Symbolic m
  -> m (Symbolic m)
  -> m (HashMap VarName (Symbolic m), Symbolic m)
lintApp context fun arg =
  (\case
    NAny ->
      throwError $ ErrorCall "Cannot apply something not known to be a function"
    NMany xs ->
      do
        (args, ys) <-
          unzip <$>
            traverse
              (\case
                TClosure _params ->
                  (\case
                    NAny                  -> error "NYI"
                    NMany [TSet (Just _)] -> error "NYI"
                    NMany _               -> throwError $ ErrorCall "NYI: lintApp NMany not set"
                  ) =<< unpackSymbolic =<< arg
                TBuiltin _ _f -> throwError $ ErrorCall "NYI: lintApp builtin"
                TSet _m       -> throwError $ ErrorCall "NYI: lintApp Set"
                _x            -> throwError $ ErrorCall "Attempt to call non-function"
              )
              xs

        y <- everyPossible
        (Unsafe.head args, ) <$> foldM (unify context) y ys
  ) =<< unpackSymbolic fun

newtype Lint s a = Lint
  { runLint :: ReaderT (Context (Lint s) (Symbolic (Lint s))) (FreshIdT Int (ST s)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadReader (Context (Lint s) (Symbolic (Lint s)))
    , MonadThunkId
    , MonadRef
    , MonadAtomicRef
    )

instance MonadThrow (Lint s) where
  throwM :: forall e a . Exception e => e -> Lint s a
  throwM e = Lint $ ReaderT $ const (throw e)

instance MonadCatch (Lint s) where
  catch _m _h = Lint $ ReaderT $ const (error "Cannot catch in 'Lint s'")

runLintM :: Options -> Lint s a -> ST s a
runLintM opts action =
  runFreshIdT ((`runReaderT` newContext opts) $ runLint action) =<< newRef (1 :: Int)

symbolicBaseEnv
  :: Monad m
  => m (Scopes m (Symbolic m))
symbolicBaseEnv = stub

lint :: Options -> NExprLoc -> ST s (Symbolic (Lint s))
lint opts expr =
  runLintM opts $
    do
      basis <- symbolicBaseEnv

      pushScopes
        basis
        (adi
          Eval.addSourcePositions
          Eval.evalContent
          expr
        )

instance
  Scoped (Symbolic (Lint s)) (Lint s) where
  askScopes = askScopesReader
  clearScopes   = clearScopesReader @(Lint s) @(Symbolic (Lint s))
  pushScopes    = pushScopesReader
  lookupVar     = lookupVarReader
