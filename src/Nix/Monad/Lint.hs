{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Monad.Lint where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as M
import           Data.STRef
import           Data.Text (Text)
import qualified Nix.Eval as Eval
import qualified Nix.Lint as Lint
import           Nix.Lint
import           Nix.Monad.Context
import           Nix.Scope
import           Nix.Stack
import           Nix.Thunk

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

{-
instance Eval.MonadExpr (SThunk (Lint s))
             (STRef s (NSymbolicF (NTypeF (Lint s) (SThunk (Lint s)))))
             (Lint s) where
    embedSet s = mkSymbolic [TSet (Just s)]
    projectSet = unpackSymbolic >=> \case
        NMany [TSet s] -> return s
        _ -> return Nothing
    projectSetWithPos = unpackSymbolic >=> \case
        NMany [TSet s] -> return $ (, M.empty) <$> s
        _ -> return Nothing

    type MText (Lint s) = Text

    wrapText   = return
    unwrapText = return

    embedText   = const $ mkSymbolic [TStr]
    projectText = const $ return Nothing
-}

runLintM :: Lint s a -> ST s a
runLintM = flip runReaderT (Context emptyScopes []) . runLint
