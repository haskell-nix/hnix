{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Normal where

import           Control.Monad
import           Data.Fix
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Effects
import           Nix.Stack
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

normalFormBy :: (Framed e m, MonadVar m, MonadFile m)
             => (forall r. NThunk m -> (NValue m -> m r) -> m r)
             -> Int
             -> NValue m
             -> m (NValueNF m)
normalFormBy k n v = do
    traceM $ replicate n ' ' ++ "normalFormBy: " ++ show v
    when (n > 2000) $ throwError "<<loop during normalization>>"
    case v of
        NVConstant a     -> return $ Fix $ NVConstant a
        NVStr t s        -> return $ Fix $ NVStr t s
        NVList l         ->
            fmap (Fix . NVList) $ forM (zip [0..] l) $ \(i :: Int, t) -> do
                traceM $ replicate n ' ' ++ "normalFormBy: List[" ++ show i ++ "]"
                t `k` normalFormBy k (succ n)
        NVSet s p        ->
            fmap (Fix . flip NVSet p) $ sequence $ flip M.mapWithKey s $ \key t -> do
                traceM $ replicate n ' ' ++ "normalFormBy: Set{" ++ show key ++ "}"
                t `k` normalFormBy k (succ n)
        NVClosure p f    -> return $ Fix $ NVClosure p f
        NVPath fp        -> return $ Fix $ NVPath fp
        NVBuiltin name f -> return $ Fix $ NVBuiltin name f

normalForm :: (Framed e m, MonadVar m, MonadFile m,
               MonadThunk (NValue m) (NThunk m) m)
           => NValue m -> m (NValueNF m)
normalForm = normalFormBy force 0

embed :: forall m. (MonadThunk (NValue m) (NThunk m) m)
      => NValueNF m -> m (NValue m)
embed (Fix x) = case x of
    NVConstant a     -> return $ NVConstant a
    NVStr t s        -> return $ NVStr t s
    NVList l         -> NVList . fmap (value @_ @_ @m)
        <$> traverse embed l
    NVSet s p        -> flip NVSet p . fmap (value @_ @_ @m)
        <$> traverse embed s
    NVClosure p f    -> return $ NVClosure p f
    NVPath fp        -> return $ NVPath fp
    NVBuiltin name f -> return $ NVBuiltin name f

valueText :: forall e m. (Framed e m, MonadFile m, MonadEffects m)
          => Bool -> NValueNF m -> m (Text, DList Text)
valueText addPathsToStore = cata phi
  where
    phi :: NValueF m (m (Text, DList Text)) -> m (Text, DList Text)
    phi (NVConstant a)    = pure (atomText a, mempty)
    phi (NVStr t c)       = pure (t, c)
    phi (NVList _)        = throwError "Cannot coerce a list to a string"
    phi (NVSet s _)
      | Just asString <-
        -- TODO: Should this be run through valueText recursively?
        M.lookup "__asString" s = asString
      | otherwise = throwError "Cannot coerce a set to a string"
    phi NVClosure {} = throwError "Cannot coerce a function to a string"
    phi (NVPath originalPath)
        | addPathsToStore = do
            -- TODO: Capture and use the path of the file being processed as the
            -- base path
            storePath <- addPath originalPath
            pure (Text.pack $ unStorePath storePath, mempty)
        | otherwise = pure (Text.pack originalPath, mempty)
    phi (NVBuiltin _ _)    = throwError "Cannot coerce a function to a string"

valueTextNoContext :: (Framed e m, MonadFile m, MonadEffects m)
                   => Bool -> NValueNF m -> m Text
valueTextNoContext addPathsToStore = fmap fst . valueText addPathsToStore
