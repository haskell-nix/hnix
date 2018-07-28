{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nix.Normal where

import           Control.Monad
import           Data.Fix
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Effects
import           Nix.Frames
import           Nix.NixString
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

newtype NormalLoop m = NormalLoop (NValue m)
    deriving Show

instance Typeable m => Exception (NormalLoop m)

normalFormBy
    :: forall e m. (Framed e m, MonadVar m, Typeable m)
    => (forall r. NThunk m -> (NValue m -> m r) -> m r)
    -> Int
    -> NValue m
    -> m (NValueNF m)
normalFormBy k n v = do
    when (n > 2000) $ throwError $ NormalLoop v
    case v of
        NVConstant a     -> return $ Fix $ NVConstantF a
        NVStr ns         -> return $ Fix $ NVStrF $ ns
        NVList l         ->
            fmap (Fix . NVListF) $ forM (zip [0..] l) $ \(i :: Int, t) -> do
                traceM $ replicate n ' ' ++ "normalFormBy: List[" ++ show i ++ "]"
                t `k` normalFormBy k (succ n)
        NVSet s p        ->
            fmap (Fix . flip NVSetF p) $ sequence $ flip M.mapWithKey s $ \ky t -> do
                traceM $ replicate n ' ' ++ "normalFormBy: Set{" ++ show ky ++ "}"
                t `k` normalFormBy k (succ n)
        NVClosure p f    -> return $ Fix $ NVClosureF p f
        NVPath fp        -> return $ Fix $ NVPathF fp
        NVBuiltin name f -> return $ Fix $ NVBuiltinF name f
        _ -> error "Pattern synonyms mask complete matches"

normalForm :: (Framed e m, MonadVar m, Typeable m,
              MonadThunk (NValue m) (NThunk m) m)
           => NValue m -> m (NValueNF m)
normalForm = normalFormBy force 0

embed :: forall m. (MonadThunk (NValue m) (NThunk m) m)
      => NValueNF m -> m (NValue m)
embed (Fix x) = case x of
    NVConstantF a     -> return $ nvConstant a
    NVStrF ns  -> return $ nvStr ns
    NVListF l         -> nvList . fmap (value @_ @_ @m)
        <$> traverse embed l
    NVSetF s p        -> flip nvSet p . fmap (value @_ @_ @m)
        <$> traverse embed s
    NVClosureF p f    -> return $ nvClosure p f
    NVPathF fp        -> return $ nvPath fp
    NVBuiltinF name f -> return $ nvBuiltin name f

valueText :: forall e m. (Framed e m, MonadEffects m, Typeable m)
          => Bool -> NValueNF m -> m NixString
valueText addPathsToStore = cata phi
  where
    phi :: NValueF m NixString -> m NixString
    phi (NVConstantF a) = pure (makeNixStringWithoutContext (atomText a))
    phi (NVStrF ns) = pure ns
    phi v@(NVListF _)   = coercionFailed v
    phi v@(NVSetF s _)
      | Just asString <- M.lookup "__asString" s = asString
      | otherwise = coercionFailed v
    phi v@NVClosureF {} = coercionFailed v
    phi (NVPathF originalPath)
        | addPathsToStore = do
            storePath <- addPath originalPath
            pure (makeNixStringWithoutContext $ Text.pack $ unStorePath storePath)
        | otherwise = pure (makeNixStringWithoutContext (Text.pack originalPath))
    phi v@(NVBuiltinF _ _) = coercionFailed v

    coercionFailed v =
        throwError $ Coercion @m (valueType v) TString

valueTextNoContext :: (Framed e m, MonadEffects m, Typeable m)
                   => Bool -> NValueNF m -> m Text
valueTextNoContext addPathsToStore = fmap stringIntentionallyDropContext . valueText addPathsToStore
