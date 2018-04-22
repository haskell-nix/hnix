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
        NVConstant a     -> return $ Fix $ NVConstantF a
        NVStr t s        -> return $ Fix $ NVStrF t s
        NVList l         ->
            fmap (Fix . NVListF) $ forM (zip [0..] l) $ \(i :: Int, t) -> do
                traceM $ replicate n ' ' ++ "normalFormBy: List[" ++ show i ++ "]"
                t `k` normalFormBy k (succ n)
        NVSet s p        ->
            fmap (Fix . flip NVSetF p) $ sequence $ flip M.mapWithKey s $ \key t -> do
                traceM $ replicate n ' ' ++ "normalFormBy: Set{" ++ show key ++ "}"
                t `k` normalFormBy k (succ n)
        NVClosure p f    -> return $ Fix $ NVClosureF p f
        NVPath fp        -> return $ Fix $ NVPathF fp
        NVBuiltin name f -> return $ Fix $ NVBuiltinF name f
        _ -> error "Pattern synonyms mask complete matches"

normalForm :: (Framed e m, MonadVar m, MonadFile m,
               MonadThunk (NValue m) (NThunk m) m)
           => NValue m -> m (NValueNF m)
normalForm = normalFormBy force 0

embed :: forall m. (MonadThunk (NValue m) (NThunk m) m)
      => NValueNF m -> m (NValue m)
embed (Fix x) = case x of
    NVConstantF a     -> return $ nvConstant a
    NVStrF t s        -> return $ nvStr t s
    NVListF l         -> nvList . fmap (value @_ @_ @m)
        <$> traverse embed l
    NVSetF s p        -> flip nvSet p . fmap (value @_ @_ @m)
        <$> traverse embed s
    NVClosureF p f    -> return $ nvClosure p f
    NVPathF fp        -> return $ nvPath fp
    NVBuiltinF name f -> return $ nvBuiltin name f

valueText :: forall e m. (Framed e m, MonadFile m, MonadEffects m)
          => Bool -> NValueNF m -> m (Text, DList Text)
valueText addPathsToStore = cata phi
  where
    phi :: NValueF m (m (Text, DList Text)) -> m (Text, DList Text)
    phi (NVConstantF a)    = pure (atomText a, mempty)
    phi (NVStrF t c)       = pure (t, c)
    phi (NVListF _)        = throwError "Cannot coerce a list to a string"
    phi (NVSetF s _)
      | Just asString <-
        -- TODO: Should this be run through valueText recursively?
        M.lookup "__asString" s = asString
      | otherwise = throwError "Cannot coerce a set to a string"
    phi NVClosureF {} = throwError "Cannot coerce a function to a string"
    phi (NVPathF originalPath)
        | addPathsToStore = do
            -- TODO: Capture and use the path of the file being processed as the
            -- base path
            storePath <- addPath originalPath
            pure (Text.pack $ unStorePath storePath, mempty)
        | otherwise = pure (Text.pack originalPath, mempty)
    phi (NVBuiltinF _ _)    = throwError "Cannot coerce a function to a string"

valueTextNoContext :: (Framed e m, MonadFile m, MonadEffects m)
                   => Bool -> NValueNF m -> m Text
valueTextNoContext addPathsToStore = fmap fst . valueText addPathsToStore
