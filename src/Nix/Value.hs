{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Nix.Value where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Align
import           Data.Fix
import qualified Data.HashMap.Lazy as M
import           Data.Monoid (appEndo)
import           Data.Text (Text)
import           Data.These
import           Data.Typeable (Typeable)
import           Data.Void
import           GHC.Generics
import           Nix.Atoms
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Scope
import           Nix.Thunk
import           Nix.Utils

-- | An 'NValue' is the most reduced form of an 'NExpr' after evaluation
-- is completed.
data NValueF m r
    = NVConstantF NAtom
     -- | A string has a value and a context, which can be used to record what a
     -- string has been build from
    | NVStrF Text (DList Text)
    | NVPathF FilePath
    | NVListF [r]
    | NVSetF (AttrSet r) (AttrSet SourcePos)
    | NVClosureF (Params Void) (m (NValue m) -> m (NValue m))
      -- ^ A function is a closed set of parameters representing the "call
      --   signature", used at application time to check the type of arguments
      --   passed to the function. Since it supports default values which may
      --   depend on other values within the final argument set, this
      --   dependency is represented as a set of pending evaluations. The
      --   arguments are finally normalized into a set which is passed to the
      --   function.
      --
      --   Note that 'm r' is being used here because effectively a function
      --   and its set of default arguments is "never fully evaluated". This
      --   enforces in the type that it must be re-evaluated for each call.
    | NVBuiltinF String (m (NValue m) -> m (NValue m))
      -- ^ A builtin function is itself already in normal form. Also, it may
      --   or may not choose to evaluate its argument in the production of a
      --   result.
    deriving (Generic, Typeable, Functor, Foldable, Traversable)

-- | An 'NValueNF' is a fully evaluated value in normal form. An 'NValue m' is
--   a value in head normal form, where only the "top layer" has been
--   evaluated. An action of type 'm (NValue m)' is a pending evualation that
--   has yet to be performed. An 'NThunk m' is either a pending evaluation, or
--   a value in head normal form. A 'NThunkSet' is a set of mappings from keys
--   to thunks.

type    NValueNF m = Fix (NValueF m)      -- normal form
newtype NThunk m   = NThunk (Thunk m (NValue m))
type    ValueSet m = AttrSet (NThunk m)

data Provenance m = Provenance
    { lexicalScope :: Scopes m (NThunk m)
    , originExpr   :: NExprLocF (Maybe (NValue m))
    , contextExpr  :: Maybe (NExprLocF (Maybe (NValue m)))
      -- ^ When calling the function x: x + 2 with argument x = 3, the
      --   'originExpr' for the resulting value will be 3 + 2, while the
      --   'contextExpr' will be @(x: x + 2) 3@, preserving not only the
      --   result of the call, but what was called and with what arguments.
    }

-- jww (2018-04-22): Tracking value provenance may need to be a compile-time
-- option.
data NValue m = NValue
    { provenance :: Maybe (Provenance m)
    , baseValue  :: NValueF m (NThunk m)
    }

changeProvenance :: Scopes m (NThunk m)
                 -> (NValue m -> NExprLocF (Maybe (NValue m)))
                 -> NValue m -> NValue m
changeProvenance s f l@(NValue _ v) =
    NValue (Just (Provenance s (f l) Nothing)) v

provenanceContext :: NExprLocF (Maybe (NValue m))
                 -> NValue m -> NValue m
provenanceContext c (NValue p v) =
    NValue (fmap (\x -> x { contextExpr = Just c }) p) v

pattern NVConstant x <- NValue _ (NVConstantF x)

nvConstant x = NValue Nothing (NVConstantF x)
nvConstantP p x = NValue (Just p) (NVConstantF x)

pattern NVStr s d <- NValue _ (NVStrF s d)

nvStr s d = NValue Nothing (NVStrF s d)
nvStrP p s d = NValue (Just p) (NVStrF s d)

pattern NVPath x <- NValue _ (NVPathF x)

nvPath x = NValue Nothing (NVPathF x)
nvPathP p x = NValue (Just p) (NVPathF x)

pattern NVList l <- NValue _ (NVListF l)

nvList l = NValue Nothing (NVListF l)
nvListP p l = NValue (Just p) (NVListF l)

pattern NVSet s x <- NValue _ (NVSetF s x)

nvSet s x = NValue Nothing (NVSetF s x)
nvSetP p s x = NValue (Just p) (NVSetF s x)

pattern NVClosure x f <- NValue _ (NVClosureF x f)

nvClosure x f = NValue Nothing (NVClosureF x f)
nvClosureP p x f = NValue (Just p) (NVClosureF x f)

pattern NVBuiltin name f <- NValue _ (NVBuiltinF name f)

nvBuiltin name f = NValue Nothing (NVBuiltinF name f)
nvBuiltinP p name f = NValue (Just p) (NVBuiltinF name f)

instance Show (NValueF m (Fix (NValueF m))) where
    showsPrec = flip go where
      go (NVConstantF atom)    = showsCon1 "NVConstant" atom
      go (NVStrF text context) = showsCon2 "NVStr"      text (appEndo context [])
      go (NVListF     list)    = showsCon1 "NVList"     list
      go (NVSetF attrs _)      = showsCon1 "NVSet"      attrs
      go (NVClosureF p _)      = showsCon1 "NVClosure"  p
      go (NVPathF p)           = showsCon1 "NVPath"     p
      go (NVBuiltinF name _)   = showsCon1 "NVBuiltin"  name

      showsCon1 :: Show a => String -> a -> Int -> String -> String
      showsCon1 con a d =
          showParen (d > 10) $ showString (con ++ " ") . showsPrec 11 a

      showsCon2 :: (Show a, Show b)
                => String -> a -> b -> Int -> String -> String
      showsCon2 con a b d =
          showParen (d > 10)
              $ showString (con ++ " ")
              . showsPrec 11 a
              . showString " "
              . showsPrec 11 b

builtin :: Monad m => String -> (m (NValue m) -> m (NValue m)) -> m (NValue m)
builtin name f = return $ nvBuiltin name f

builtin2 :: Monad m
         => String -> (m (NValue m) -> m (NValue m) -> m (NValue m)) -> m (NValue m)
builtin2 name f = builtin name (builtin name . f)

builtin3 :: Monad m
         => String -> (m (NValue m) -> m (NValue m) -> m (NValue m) -> m (NValue m))
         -> m (NValue m)
builtin3 name f =
    builtin name $ \a -> builtin name $ \b -> builtin name $ \c -> f a b c

isClosureNF :: Monad m => NValueNF m -> Bool
isClosureNF (Fix NVClosureF {}) = True
isClosureNF _ = False

thunkEq :: MonadThunk (NValue m) (NThunk m) m
        => NThunk m -> NThunk m -> m Bool
thunkEq lt rt = force lt $ \lv -> force rt $ \rv -> valueEq lv rv

-- | Checks whether two containers are equal, using the given item equality
--   predicate. If there are any item slots that don't match between the two
--   containers, the result will be False.
alignEqM
    :: (Align f, Traversable f, Monad m)
    => (a -> b -> m Bool)
    -> f a
    -> f b
    -> m Bool
alignEqM eq fa fb = fmap (either (const False) (const True)) $ runExceptT $ do
    pairs <- forM (align fa fb) $ \case
        These a b -> return (a, b)
        _ -> throwE ()
    forM_ pairs $ \(a, b) -> guard =<< lift (eq a b)

isDerivation :: MonadThunk (NValue m) (NThunk m) m
             => AttrSet (NThunk m) -> m Bool
isDerivation m = case M.lookup "type" m of
    Nothing -> pure False
    Just t -> force t $ valueEq (nvStr "derivation" mempty)

valueEq :: MonadThunk (NValue m) (NThunk m) m
        => NValue m -> NValue m -> m Bool
valueEq l r = case (l, r) of
    (NVStr ls _, NVConstant (NUri ru)) -> pure $ ls == ru
    (NVConstant (NUri lu), NVStr rs _) -> pure $ lu == rs
    (NVConstant lc, NVConstant rc) -> pure $ lc == rc
    (NVStr ls _, NVStr rs _) -> pure $ ls == rs
    (NVStr ls _, NVConstant NNull) -> pure $ ls == ""
    (NVConstant NNull, NVStr rs _) -> pure $ "" == rs
    (NVList ls, NVList rs) -> alignEqM thunkEq ls rs
    (NVSet lm _, NVSet rm _) -> do
        let compareAttrs = alignEqM thunkEq lm rm
        isDerivation lm >>= \case
            True -> isDerivation rm >>= \case
                True | Just lp <- M.lookup "outPath" lm
                     , Just rp <- M.lookup "outPath" rm
                       -> thunkEq lp rp
                _ -> compareAttrs
            _ -> compareAttrs
    (NVPath lp, NVPath rp) -> pure $ lp == rp
    _ -> pure False
