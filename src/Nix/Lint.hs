{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Lint where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import           Data.Coerce
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.List
import           Data.STRef
import           Data.Text (Text)
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Context
import           Nix.Convert
import           Nix.Eval
import qualified Nix.Eval as Eval
import           Nix.Expr
import           Nix.Scope
import           Nix.Stack
import           Nix.Thunk
-- import           Nix.Type.Infer
import           Nix.Utils

data TAtom
  = TInt
  | TFloat
  | TBool
  | TNull
  | TUri
  deriving (Show, Eq, Ord)

data NTypeF (m :: * -> *) r
    = TConstant [TAtom]
    | TStr
    | TList r
    | TSet (Maybe (HashMap Text r))
    | TClosure (Params ()) (m (Symbolic m) -> m (Symbolic m))
    | TPath
    | TBuiltin String (SThunk m -> m (Symbolic m))
    deriving Functor

compareTypes :: NTypeF m r -> NTypeF m r -> Ordering
compareTypes (TConstant _)   (TConstant _)   = EQ
compareTypes (TConstant _)   _               = LT
compareTypes _               (TConstant _)   = GT
compareTypes TStr            TStr            = EQ
compareTypes TStr            _               = LT
compareTypes _               TStr            = GT
compareTypes (TList _)       (TList _)       = EQ
compareTypes (TList _)       _               = LT
compareTypes _               (TList _)       = GT
compareTypes (TSet _)        (TSet _)        = EQ
compareTypes (TSet _)        _               = LT
compareTypes _               (TSet _)        = GT
compareTypes TClosure {}     TClosure {}     = EQ
compareTypes TClosure {}     _               = LT
compareTypes _               TClosure {}     = GT
compareTypes TPath           TPath           = EQ
compareTypes TPath            _              = LT
compareTypes _               TPath           = GT
compareTypes (TBuiltin _ _)  (TBuiltin _ _)  = EQ

data NSymbolicF r
    = NAny
    | NMany [r]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

newtype SThunk m = SThunk { getSThunk :: Thunk m (Symbolic m) }

sthunk :: MonadVar m => m (Symbolic m) -> m (SThunk m)
sthunk = fmap coerce . buildThunk

sforce :: (Framed e m, MonadFile m, MonadVar m)
       => SThunk m -> (Symbolic m -> m r) -> m r
sforce = forceThunk . coerce

svalueThunk :: forall m. Symbolic m -> SThunk m
svalueThunk = coerce . valueRef @_ @m

newtype Symbolic m =
    Symbolic { getSymbolic :: Var m (NSymbolicF (NTypeF m (SThunk m))) }

instance Show (Symbolic m) where
    show _ = "<symbolic>"

everyPossible :: MonadVar m => m (Symbolic m)
everyPossible = packSymbolic NAny

mkSymbolic :: MonadVar m => [NTypeF m (SThunk m)] -> m (Symbolic m)
mkSymbolic xs = packSymbolic (NMany xs)

packSymbolic :: MonadVar m
             => NSymbolicF (NTypeF m (SThunk m)) -> m (Symbolic m)
packSymbolic = fmap coerce . newVar

unpackSymbolic :: MonadVar m
               => Symbolic m -> m (NSymbolicF (NTypeF m (SThunk m)))
unpackSymbolic = readVar . coerce

type MonadLint e m =
    (Scoped e (SThunk m) m, Framed e m, MonadVar m, MonadFile m)

symerr :: forall e m a. MonadLint e m => String -> m a
symerr = evalError @(Symbolic m)

renderSymbolic :: MonadLint e m => Symbolic m -> m String
renderSymbolic = unpackSymbolic >=> \case
    NAny -> return "<any>"
    NMany xs -> fmap (intercalate ", ") $ forM xs $ \case
        TConstant ys  -> fmap (intercalate ", ") $ forM ys $ \case
            TInt   -> return "int"
            TFloat -> return "float"
            TBool  -> return "bool"
            TNull  -> return "null"
            TUri   -> return "uri"
        TStr            -> return "string"
        TList r         -> do
            x <- sforce r renderSymbolic
            return $ "[" ++ x ++ "]"
        TSet Nothing    -> return "<any set>"
        TSet (Just s)   -> do
            x <- traverse (`sforce` renderSymbolic) s
            return $ "{" ++ show x ++ "}"
        f@(TClosure p _) -> do
            (args, sym) <- do
                f' <- mkSymbolic [f]
                lintApp (NAbs (void p) ()) f' everyPossible
            args' <- traverse renderSymbolic args
            sym'  <- renderSymbolic sym
            return $ "(" ++ show args' ++ " -> " ++ sym' ++ ")"
        TPath           -> return "path"
        TBuiltin _n _f    -> return "<builtin function>"

-- This function is order and uniqueness preserving (of types).
merge :: forall e m. MonadLint e m
      => NExprF () -> [NTypeF m (SThunk m)] -> [NTypeF m (SThunk m)]
      -> m [NTypeF m (SThunk m)]
merge context = go
  where
    go :: [NTypeF m (SThunk m)] -> [NTypeF m (SThunk m)]
       -> m [NTypeF m (SThunk m)]
    go [] _ = return []
    go _ [] = return []
    go (x:xs) (y:ys) = case (x, y) of
        (TStr,  TStr)  -> (TStr :)  <$> go xs ys
        (TPath, TPath) -> (TPath :) <$> go xs ys
        (TConstant ls, TConstant rs) ->
            (TConstant (ls `intersect` rs) :) <$> go xs ys
        (TList l, TList r) -> sforce l $ \l' -> sforce r $ \r' -> do
            m <- sthunk $ unify context l' r'
            (TList m :) <$> go xs ys
        (TSet x, TSet Nothing) -> (TSet x :) <$> go xs ys
        (TSet Nothing, TSet x) -> (TSet x :) <$> go xs ys
        (TSet (Just l), TSet (Just r)) -> do
            m <- sequenceA $ M.intersectionWith
                (\i j -> i >>= \i' -> j >>= \j' ->
                        sforce i' $ \i'' -> sforce j' $ \j'' ->
                            sthunk $ unify context i'' j'')
                (return <$> l) (return <$> r)
            if M.null m
                then go xs ys
                else (TSet (Just m) :) <$> go xs ys
        (TClosure {}, TClosure {}) ->
            throwError "Cannot unify functions"
        (TBuiltin _ _, TBuiltin _ _) ->
            throwError "Cannot unify builtin functions"
        _ | compareTypes x y == LT -> go xs (y:ys)
          | compareTypes x y == GT -> go (x:xs) ys
          | otherwise              -> error "impossible"

{-
    mergeFunctions pl nl fl pr fr xs ys = do
        m <- sequenceA $ M.intersectionWith
            (\i j -> i >>= \i' -> j >>= \j' -> case (i', j') of
                    (Nothing, Nothing) -> return $ Just Nothing
                    (_, Nothing) -> return Nothing
                    (Nothing, _) -> return Nothing
                    (Just i'', Just j'') ->
                        Just . Just <$> unify context i'' j'')
            (return <$> pl) (return <$> pr)
        let Just m' = sequenceA $ M.filter isJust m
        if M.null m'
            then go xs ys
            else do
                g <- unify context fl fr
                (TClosure (ParamSet m' False nl) g :)
                    <$> go xs ys
-}

-- | unify raises an error if the result is would be 'NMany []'.
unify :: MonadLint e m
      => NExprF () -> Symbolic m -> Symbolic m -> m (Symbolic m)
unify context (Symbolic x) (Symbolic y) = do
    x' <- readVar x
    y' <- readVar y
    case (x', y') of
        (NAny, _) -> do
            writeVar x y'
            return $ Symbolic y
        (_, NAny) -> do
            writeVar y x'
            return $ Symbolic x
        (NMany xs, NMany ys) -> do
            m <- merge context xs ys
            if null m
                then do
                    x' <- renderSymbolic (Symbolic x)
                    y' <- renderSymbolic (Symbolic y)
                    throwError $ "Cannot unify "
                        ++ show x' ++ " with " ++ show y'
                         ++ " in context: " ++ show context
                else do
                    writeVar x (NMany m)
                    writeVar y (NMany m)
                    packSymbolic (NMany m)

-- jww (2018-04-15): These aren't worth defining yet, because once we move to
-- Hindley-Milner, we're not going to be managing Symbolic values this way
-- anymore.

instance FromValue (AttrSet (SThunk m)) m (Symbolic m) where

instance FromValue (AttrSet (SThunk m), AttrSet SourcePos) m (Symbolic m) where

instance ToValue (AttrSet (SThunk m)) m (Symbolic m) where

instance ToValue (AttrSet (SThunk m), AttrSet SourcePos) m (Symbolic m) where

instance ToValue [SThunk m] m (Symbolic m) where

instance ToValue Bool m (Symbolic m) where

instance MonadLint e m => MonadThunk (Symbolic m) (SThunk m) m where
    thunk = fmap coerce . buildThunk
    force = forceThunk . coerce
    value = coerce . valueRef

instance MonadLint e m => MonadEval (Symbolic m) m where
    freeVariable var = symerr $
        "Undefined variable '" ++ Text.unpack var ++ "'"

    evalCurPos = do
        f <- value <$> mkSymbolic [TPath]
        l <- value <$> mkSymbolic [TConstant [TInt]]
        c <- value <$> mkSymbolic [TConstant [TInt]]
        mkSymbolic [TSet (Just (M.fromList (go f l c)))]
      where
        go f l c =
            [ ("file", f)
            , ("line", l)
            , ("col",  c) ]

    evalConstant c  = mkSymbolic [TConstant [go c]]
      where
        go = \case
          NInt _   -> TInt
          NFloat _ -> TFloat
          NBool _  -> TBool
          NNull    -> TNull
          NUri _   -> TUri

    evalString      = const $ mkSymbolic [TStr]
    evalLiteralPath = const $ mkSymbolic [TPath]
    evalEnvPath     = const $ mkSymbolic [TPath]

    evalUnary op arg =
        unify (void (NUnary op arg)) arg
            =<< mkSymbolic [TConstant [TInt, TBool]]

    evalBinary = lintBinaryOp

    evalWith scope body = do
        -- The scope is deliberately wrapped in a thunk here, since it is
        -- evaluated each time a name is looked up within the weak scope, and
        -- we want to be sure the action it evaluates is to force a thunk, so
        -- its value is only computed once.
        s <- sthunk scope
        pushWeakScope ?? body $ sforce s $ unpackSymbolic >=> \case
            NMany [TSet (Just s')] -> return s'
            NMany [TSet Nothing] -> error "NYI: with unknown"
            _ -> throwError "scope must be a set in with statement"

    evalIf cond t f = do
        t' <- t
        f' <- f
        let e = NIf cond t' f'
        _ <- unify (void e) cond =<< mkSymbolic [TConstant [TBool]]
        unify (void e) t' f'

    evalAssert cond body = do
        body' <- body
        let e = NAssert cond body'
        _ <- unify (void e) cond =<< mkSymbolic [TConstant [TBool]]
        pure body'

    evalApp = (fmap snd .) . lintApp (NBinary NApp () ())
    evalAbs params body = mkSymbolic [TClosure params body]

    evalError = throwError

    type MText (Symbolic m) = ()

    wrapMText   = const $ return ()
    unwrapMText = const $ return ""

    embedMText   = const $ mkSymbolic [TStr]
    projectMText = const $ return Nothing -- jww (2018-04-10): TODO

lintBinaryOp
    :: forall e m. (MonadLint e m, MonadEval (Symbolic m) m)
    => NBinaryOp -> Symbolic m -> m (Symbolic m) -> m (Symbolic m)
lintBinaryOp op lsym rarg = do
    rsym <- rarg
    y <- sthunk everyPossible
    case op of
        NApp   -> symerr "lintBinaryOp:NApp: should never get here"
        NEq    -> check lsym rsym [ TConstant [TInt, TBool, TNull, TUri]
                                 , TStr
                                 , TList y ]
        NNEq   -> check lsym rsym [ TConstant [TInt, TBool, TNull, TUri]
                                 , TStr
                                 , TList y ]

        NLt    -> check lsym rsym [ TConstant [TInt, TBool, TNull, TUri] ]
        NLte   -> check lsym rsym [ TConstant [TInt, TBool, TNull, TUri] ]
        NGt    -> check lsym rsym [ TConstant [TInt, TBool, TNull, TUri] ]
        NGte   -> check lsym rsym [ TConstant [TInt, TBool, TNull, TUri] ]

        NAnd   -> check lsym rsym [ TConstant [TBool] ]
        NOr    -> check lsym rsym [ TConstant [TBool] ]
        NImpl  -> check lsym rsym [ TConstant [TBool] ]

        -- jww (2018-04-01): NYI: Allow Path + Str
        NPlus  -> check lsym rsym [ TConstant [TInt], TStr, TPath ]
        NMinus -> check lsym rsym [ TConstant [TInt] ]
        NMult  -> check lsym rsym [ TConstant [TInt] ]
        NDiv   -> check lsym rsym [ TConstant [TInt] ]

        NUpdate -> check lsym rsym [ TSet Nothing ]

        NConcat -> check lsym rsym [ TList y ]
  where
    check lsym rsym xs = do
        let e = NBinary op lsym rsym
        m <- mkSymbolic xs
        _ <- unify (void e) lsym m
        _ <- unify (void e) rsym m
        unify (void e) lsym rsym

infixl 1 `lintApp`
lintApp :: forall e m. MonadLint e m
        => NExprF () -> Symbolic m -> m (Symbolic m)
        -> m (HashMap VarName (Symbolic m), Symbolic m)
lintApp context fun arg = unpackSymbolic fun >>= \case
    NAny -> throwError "Cannot apply something not known to be a function"
    NMany xs -> do
        (args:_, ys) <- fmap unzip $ forM xs $ \case
            TClosure _params _f -> arg >>= unpackSymbolic >>= \case
                NAny -> do
                    error "NYI"

                NMany [TSet (Just _)] -> do
                    error "NYI"

                NMany _ -> throwError "NYI: lintApp NMany not set"
            TBuiltin _ _f -> throwError "NYI: lintApp builtin"
            TSet _m -> throwError "NYI: lintApp Set"
            _x -> throwError "Attempt to call non-function"

        y <- everyPossible
        (args,) <$> foldM (unify context) y ys

newtype Lint s a = Lint
    { runLint :: ReaderT (Context (Lint s) (SThunk (Lint s))) (ST s) a }
    deriving (Functor, Applicative, Monad, MonadFix,
              MonadReader (Context (Lint s) (SThunk (Lint s))))

instance MonadVar (Lint s) where
    type Var (Lint s) = STRef s

    newVar x     = Lint $ ReaderT $ \_ -> newSTRef x
    readVar x    = Lint $ ReaderT $ \_ -> readSTRef x
    writeVar x y = Lint $ ReaderT $ \_ -> writeSTRef x y
    atomicModifyVar x f = Lint $ ReaderT $ \_ -> do
        res <- snd . f <$> readSTRef x
        _ <- modifySTRef x (fst . f)
        return res

instance MonadFile (Lint s) where
    readFile x = Lint $ ReaderT $ \_ -> unsafeIOToST $ BS.readFile x

instance MonadThrow (Lint s) where
    throwM e = Lint $ ReaderT $ \_ -> unsafeIOToST $ throw e

runLintM :: Lint s a -> ST s a
runLintM = flip runReaderT newContext . runLint

symbolicBaseEnv :: Monad m => m (Scopes m (SThunk m))
symbolicBaseEnv = return emptyScopes

lint :: NExprLoc -> ST s (Symbolic (Lint s))
lint expr = runLintM $
    symbolicBaseEnv >>= (`pushScopes` Eval.framedEvalExpr Eval.eval expr)
