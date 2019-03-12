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
import           Control.Monad.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.HashMap.Lazy as M
import           Data.List (find)
import           Data.Maybe (isJust)
import           Nix.Frames
-- import           Nix.Pretty
import           Nix.Thunk
import           Nix.Thunk.Basic
import           Nix.Utils
import           Nix.Value
import           Nix.Var

newtype NormalLoop m = NormalLoop (NValue m)
    deriving Show

instance Typeable m => Exception (NormalLoop m)

normalFormBy
    :: forall e m. (Framed e m, MonadVar m, Typeable m)
    => (forall r. NThunk m -> (NValue m -> StateT [Var m Bool] m r)
         -> StateT [Var m Bool] m r)
    -> Int
    -> NValue m
    -> StateT [Var m Bool] m (NValueNF m)
normalFormBy k n v = case v of
    NVConstant a     -> return $ Free $ NVConstantF a
    NVStr ns         -> return $ Free $ NVStrF ns
    NVList l         ->
        fmap (Free . NVListF) $ forM (zip [0..] l) $ \(i :: Int, t) -> do
            traceM $ show n ++ ": normalFormBy: List[" ++ show i ++ "]"
            k t (next t)
    NVSet s p        ->
        fmap (Free . flip NVSetF p) $ sequence $ flip M.mapWithKey s $ \ky t -> do
            traceM $ show n ++ ": normalFormBy: Set{" ++ show ky ++ "}"
            k t (next t)
    NVClosure p f    -> return $ Free $ NVClosureF p f
    NVPath fp        -> return $ Free $ NVPathF fp
    NVBuiltin name f -> return $ Free $ NVBuiltinF name f
    _ -> error "Pattern synonyms mask complete matches"
  where
    next t val = do
        b <- seen t
        if b
            then return $ Pure val
            else normalFormBy k (succ n) val

    -- jww (2019-03-11): NYI
    -- seen (NThunk (NCited _ (Thunk _ b _))) = do
    --     res <- gets (isJust . find (eqVar @m b))
    --     unless res $
    --         modify (b:)
    --     return res
    seen _ = pure False

normalForm' :: forall e m. (Framed e m, MonadVar m, Typeable m,
               MonadThunk (NValue m) (NThunk m) m)
            => (forall r. NThunk m -> (NValue m -> m r) -> m r)
            -> NValue m -> m (NValueNF m)
normalForm' f = flip evalStateT mempty . normalFormBy go 0
  where
    go :: NThunk m
       -> (NValue m -> StateT [Var m Bool] m r)
       -> StateT [Var m Bool] m r
    go t k = do
        s <- get
        (res, s') <- lift $ f t $ \v -> runStateT (k v) s
        put s'
        return res

normalForm :: forall e m. (Framed e m, MonadVar m, Typeable m,
              MonadThunk (NValue m) (NThunk m) m)
           => NValue m -> m (NValueNF m)
normalForm = normalForm' force

normalForm_
    :: forall e m. (Framed e m, MonadVar m, Typeable m,
              MonadThunk (NValue m) (NThunk m) m)
    => NValue m -> m ()
normalForm_ = void . normalForm' (forceEffects . _cited . _nThunk)

embed :: forall m. (MonadThunk (NValue m) (NThunk m) m)
      => NValueNF m -> m (NValue m)
embed (Pure v) = return v
embed (Free x) = case x of
    NVConstantF a  -> return $ nvConstant a
    NVStrF ns      -> return $ nvStr ns
    NVListF l      ->
        nvList       . fmap (wrapValue @_ @_ @m) <$> traverse embed l
    NVSetF s p     ->
        flip nvSet p . fmap (wrapValue @_ @_ @m) <$> traverse embed s
    NVClosureF p f -> return $ nvClosure p f
    NVPathF fp     -> return $ nvPath fp
    NVBuiltinF n f -> return $ nvBuiltin n f
