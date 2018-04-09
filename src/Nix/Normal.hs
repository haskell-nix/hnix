{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Normal where

import           Data.Fix
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Monad
import           Nix.Stack
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

normalFormBy :: Monad m
             => (forall r. NThunk m -> (NValue m -> m r) -> m r) -> NValue m
             -> m (NValueNF m)
normalFormBy k = \case
    NVConstant a     -> return $ Fix $ NVConstant a
    NVStr t s        -> return $ Fix $ NVStr t s
    NVList l         -> Fix . NVList <$> traverse (`k` normalFormBy k) l
    NVSet s p        -> Fix . flip NVSet p <$> traverse (`k` normalFormBy k) s
    NVClosure p f    -> return $ Fix $ NVClosure p f
    NVPath fp        -> return $ Fix $ NVPath fp
    NVBuiltin name f -> return $ Fix $ NVBuiltin name f

normalForm :: (MonadThunk (NValue m) (NThunk m) m)
           => NValue m -> m (NValueNF m)
normalForm = normalFormBy force

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
