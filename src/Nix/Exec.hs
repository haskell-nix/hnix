{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Exec where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Coerce
import           Data.Functor.Compose
import qualified Data.HashMap.Lazy as M
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Eval
import           Nix.Expr
import           Nix.Monad
import           Nix.Normal
import           Nix.Pretty
import           Nix.Stack
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

type MonadExec e m =
    (Framed e m, MonadVar m, MonadFile m, MonadEffects m)

nverr :: forall e m a. MonadExec e m => String -> m a
nverr = evalError @(NValue m)

instance MonadExec e m => ConvertValue (NValue m) Bool where
    ofVal = NVConstant . NBool
    wantVal = \case NVConstant (NBool b) -> Just b; _ -> Nothing

instance ConvertValue (NValue m) Int where
    ofVal = NVConstant . NInt . fromIntegral
    wantVal = \case NVConstant (NInt i) -> Just (fromIntegral i); _ -> Nothing

instance ConvertValue (NValue m) Integer where
    ofVal = NVConstant . NInt
    wantVal = \case NVConstant (NInt i) -> Just i; _ -> Nothing

instance ConvertValue (NValue m) Float where
    ofVal = NVConstant . NFloat
    wantVal = \case NVConstant (NFloat f) -> Just f; _ -> Nothing

instance ConvertValue (NValue m) Text where
    ofVal = flip NVStr mempty
    wantVal = \case NVStr t _ -> Just t; _ -> Nothing

instance ConvertValue (NValue m) (Maybe Text) where
    ofVal (Just s) = NVStr s mempty
    ofVal Nothing = NVConstant NNull
    wantVal (NVStr s _) = Just (Just s)
    wantVal (NVConstant NNull) = Just Nothing
    wantVal _ = Nothing

instance ConvertValue (NValue m) (Text, DList Text) where
    ofVal = uncurry NVStr
    wantVal = \case NVStr s p -> Just (s, p); _ -> Nothing

instance ConvertValue (NValue m) (Maybe (Text, DList Text)) where
    ofVal Nothing = NVConstant NNull
    ofVal (Just (s, p)) = NVStr s p
    wantVal = \case
        NVStr s p -> Just (Just (s, p))
        NVConstant NNull -> Just Nothing
        _ -> Nothing

instance ConvertValue (NValue m) [NThunk m] where
    ofVal = NVList
    wantVal = \case NVList l -> Just l; _ -> Nothing

instance ConvertValue (NValue m)
      (AttrSet (NThunk m), AttrSet Delta) where
    ofVal (s, p) = NVSet s p
    wantVal = \case NVSet s p -> Just (s, p); _ -> Nothing

instance ConvertValue (NValue m) (AttrSet (NThunk m)) where
    ofVal = flip NVSet M.empty
    wantVal = \case NVSet s _ -> Just s; _ -> Nothing

instance MonadExec e m => MonadThunk (NValue m) (NThunk m) m where
    thunk = fmap coerce . buildThunk
    force = forceThunk . coerce
    value = coerce . valueRef

instance MonadExec e m => MonadEval (NValue m) m where
    freeVariable var = evalError @(NValue m) $
        "Undefined variable '" ++ Text.unpack var ++ "'"

    evalCurPos = do
        Compose (Ann (SrcSpan delta _) _):_ <-
            asks (mapMaybe (either (const Nothing) Just)
                  . view @_ @Frames hasLens)
        return $ posFromDelta delta

    evalConstant    = pure . NVConstant
    evalString      = pure . uncurry NVStr
    evalLiteralPath = fmap NVPath . makeAbsolutePath
    evalEnvPath     = fmap NVPath . findEnvPath
    evalUnary       = execUnaryOp
    evalBinary      = execBinaryOp
    evalApp         = callFunc
    evalAbs         = (pure .) . NVClosure

    evalError = throwError

    type MText m = (Text, DList Text)

    wrapMText   = return . (, mempty)
    unwrapMText = return . fst

    embedMText   = return . uncurry NVStr
    projectMText = \case
        NVConstant NNull -> return $ Just Nothing
        v -> fmap (Just . Just) . valueText True =<< normalForm v

infixl 1 `callFunc`
callFunc :: MonadExec e m => NValue m -> m (NValue m) -> m (NValue m)
callFunc fun arg = case fun of
    NVClosure _ f -> do
        traceM "callFunc:NVFunction"
        f arg
    NVBuiltin name f -> do
        traceM $ "callFunc:NVBuiltin " ++ name
        f =<< thunk arg
    s@(NVSet m _) | Just f <- M.lookup "__functor" m -> do
        traceM "callFunc:__functor"
        force f $ \f' -> f' `callFunc` pure s >>= \g' -> g' `callFunc` arg
    x -> throwError $ "Attempt to call non-function: " ++ showValue x

execUnaryOp
    :: (Framed e m, MonadVar m, MonadFile m)
    => NUnaryOp -> NValue m -> m (NValue m)
execUnaryOp op arg = do
    traceM "NUnary"
    case arg of
        NVConstant c -> case (op, c) of
            (NNeg, NInt   i) -> return $ NVConstant $ NInt   (-i)
            (NNeg, NFloat f) -> return $ NVConstant $ NFloat (-f)
            (NNot, NBool  b) -> return $ NVConstant $ NBool  (not b)
            _ -> throwError $ "unsupported argument type for unary operator "
                     ++ show op
        x -> throwError $ "argument to unary operator"
                ++ " must evaluate to an atomic type: " ++ showValue x

execBinaryOp
    :: forall e m.
        (Framed e m, MonadVar m, MonadFile m,
         MonadEval (NValue m) m, MonadEffects m)
    => NBinaryOp -> NValue m -> m (NValue m) -> m (NValue m)

execBinaryOp NOr larg rarg = case larg of
    NVConstant (NBool l) -> if l
        then valueRefBool True
        else rarg >>= \case
            NVConstant (NBool r) -> valueRefBool r
            v -> throwError $ "operator `||`: left argument: boolean expected, got " ++ show (void v)
    v -> throwError $ "operator `||`: right argument: boolean expected, got " ++ show (void v)

execBinaryOp NAnd larg rarg = case larg of
    NVConstant (NBool l) -> if l
        then rarg >>= \case
            NVConstant (NBool r) -> valueRefBool r
            v -> throwError $ "operator `&&`: left argument: boolean expected, got " ++ show (void v)
        else valueRefBool False
    v -> throwError $ "operator `&&`: right argument: boolean expected, got " ++ show (void v)

-- jww (2018-04-08): Refactor so that eval (NBinary ..) *always* dispatches
-- based on operator first
execBinaryOp op larg rarg = do
    let lval = larg
    rval <- traceM "NBinary:right" >> rarg

    let unsupportedTypes =
            "unsupported argument types for binary operator "
                ++ showValue lval ++ " " ++ show op ++ " " ++ showValue rval
        numBinOp :: (forall a. Num a => a -> a -> a) -> NAtom -> NAtom
                 -> m (NValue m)
        numBinOp f = numBinOp' f f
        numBinOp'
            :: (Integer -> Integer -> Integer)
            -> (Float -> Float -> Float)
            -> NAtom -> NAtom -> m (NValue m)
        numBinOp' intF floatF l r = case (l, r) of
            (NInt   li, NInt   ri) ->
                pure . ofVal $             li `intF`               ri
            (NInt   li, NFloat rf) ->
                pure . ofVal $ fromInteger li `floatF`             rf
            (NFloat lf, NInt   ri) ->
                pure . ofVal $             lf `floatF` fromInteger ri
            (NFloat lf, NFloat rf) ->
                pure . ofVal $             lf `floatF`             rf
            _ -> evalError @(NValue m) unsupportedTypes

    case (lval, rval) of
        (NVConstant lc, NVConstant rc) -> case (op, lc, rc) of
            (NEq,  _, _)   -> ofVal <$> valueEq lval rval
            (NNEq, _, _)   -> ofVal . not <$> valueEq lval rval
            (NLt,  l, r)   -> pure . ofVal $ l <  r
            (NLte, l, r)   -> pure . ofVal $ l <= r
            (NGt,  l, r)   -> pure . ofVal $ l >  r
            (NGte, l, r)   -> pure . ofVal $ l >= r
            (NAnd,  _, _)  -> nverr "should be impossible: && is handled above"
            (NOr,   _, _)  -> nverr "should be impossible: || is handled above"
            (NPlus,  l, r) -> numBinOp (+) l r
            (NMinus, l, r) -> numBinOp (-) l r
            (NMult,  l, r) -> numBinOp (*) l r
            (NDiv,   l, r) -> numBinOp' div (/) l r
            (NImpl, NBool l, NBool r) -> pure . ofVal $ not l || r
            _ -> nverr unsupportedTypes

        (NVStr ls lc, NVStr rs rc) -> case op of
            NPlus -> pure $ NVStr (ls `mappend` rs) (lc `mappend` rc)
            NEq   -> ofVal <$> valueEq lval rval
            NNEq  -> ofVal . not <$> valueEq lval rval
            NLt   -> pure . ofVal $ ls <  rs
            NLte  -> pure . ofVal $ ls <= rs
            NGt   -> pure . ofVal $ ls >  rs
            NGte  -> pure . ofVal $ ls >= rs
            _ -> nverr unsupportedTypes

        (NVStr _ _, NVConstant NNull) -> case op of
            NEq   -> ofVal <$> valueEq lval (NVStr "" mempty)
            NNEq  -> ofVal . not <$> valueEq lval (NVStr "" mempty)
            _ -> nverr unsupportedTypes

        (NVConstant NNull, NVStr _ _) -> case op of
            NEq   -> ofVal <$> valueEq (NVStr "" mempty) rval
            NNEq  -> ofVal . not <$> valueEq (NVStr "" mempty) rval
            _ -> nverr unsupportedTypes

        (NVSet ls lp, NVSet rs rp) -> case op of
            NUpdate -> pure $ NVSet (rs `M.union` ls) (rp `M.union` lp)
            NEq     -> ofVal <$> valueEq lval rval
            NNEq    -> ofVal . not <$> valueEq lval rval
            _ -> nverr unsupportedTypes

        (NVList ls, NVList rs) -> case op of
            NConcat -> pure $ NVList $ ls ++ rs
            NEq     -> ofVal <$> valueEq lval rval
            NNEq    -> ofVal . not <$> valueEq lval rval
            _ -> nverr unsupportedTypes

        (NVList ls, NVConstant NNull) -> case op of
            NConcat -> pure $ NVList ls
            NEq     -> ofVal <$> valueEq lval (NVList [])
            NNEq    -> ofVal . not <$> valueEq lval (NVList [])
            _ -> nverr unsupportedTypes

        (NVConstant NNull, NVList rs) -> case op of
            NConcat -> pure $ NVList rs
            NEq     -> ofVal <$> valueEq (NVList []) rval
            NNEq    -> ofVal . not <$> valueEq (NVList []) rval
            _ -> nverr unsupportedTypes

        (NVPath ls, NVStr rs _) -> case op of
            NPlus -> NVPath <$> makeAbsolutePath (ls `mappend` Text.unpack rs)
            _ -> nverr unsupportedTypes

        (NVPath ls, NVPath rs) -> case op of
            NPlus -> NVPath <$> makeAbsolutePath (ls ++ rs)
            _ -> nverr unsupportedTypes

        _ -> nverr unsupportedTypes
