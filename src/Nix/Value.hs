{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Nix.Value where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import qualified Data.Aeson as A
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
import           Nix.Frames
import           Nix.Scope
import           Nix.Thunk
import           Nix.Utils

-- | An 'NValue' is the most reduced form of an 'NExpr' after evaluation is
--   completed. 's' is related to the type of errors that might occur during
--   construction or use of a value.
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

type NValueNF m = Fix (NValueF m)      -- normal form
type ValueSet m = AttrSet (NThunk m)

data Provenance m = Provenance
    { lexicalScope :: Scopes m (NThunk m)
    , originExpr   :: NExprLocF (Maybe (NValue m))
      -- ^ When calling the function x: x + 2 with argument x = 3, the
      --   'originExpr' for the resulting value will be 3 + 2, while the
      --   'contextExpr' will be @(x: x + 2) 3@, preserving not only the
      --   result of the call, but what was called and with what arguments.
    }

data NThunk m = NThunk
    { thunkProvenance :: [Provenance m]
    , baseThunk       :: Thunk m (NValue m)
    }

data NValue m = NValue
    { valueProvenance :: [Provenance m]
    , baseValue       :: NValueF m (NThunk m)
    }

addProvenance :: (NValue m -> Provenance m) -> NValue m -> NValue m
addProvenance f l@(NValue p v) = NValue (f l : p) v

pattern NVConstant x <- NValue _ (NVConstantF x)

nvConstant x = NValue [] (NVConstantF x)
nvConstantP p x = NValue [p] (NVConstantF x)

pattern NVStr s d <- NValue _ (NVStrF s d)

nvStr s d = NValue [] (NVStrF s d)
nvStrP p s d = NValue [p] (NVStrF s d)

pattern NVPath x <- NValue _ (NVPathF x)

nvPath x = NValue [] (NVPathF x)
nvPathP p x = NValue [p] (NVPathF x)

pattern NVList l <- NValue _ (NVListF l)

nvList l = NValue [] (NVListF l)
nvListP p l = NValue [p] (NVListF l)

pattern NVSet s x <- NValue _ (NVSetF s x)

nvSet s x = NValue [] (NVSetF s x)
nvSetP p s x = NValue [p] (NVSetF s x)

pattern NVClosure x f <- NValue _ (NVClosureF x f)

nvClosure x f = NValue [] (NVClosureF x f)
nvClosureP p x f = NValue [p] (NVClosureF x f)

pattern NVBuiltin name f <- NValue _ (NVBuiltinF name f)

nvBuiltin name f = NValue [] (NVBuiltinF name f)
nvBuiltinP p name f = NValue [p] (NVBuiltinF name f)

instance Show (NValueF m (Fix (NValueF m))) where
    showsPrec = flip go where
      go (NVConstantF atom)  = showsCon1 "NVConstant" atom
      go (NVStrF txt ctxt)   = showsCon2 "NVStr"      txt (appEndo ctxt [])
      go (NVListF     lst)   = showsCon1 "NVList"     lst
      go (NVSetF attrs _)    = showsCon1 "NVSet"      attrs
      go (NVClosureF p _)    = showsCon1 "NVClosure"  p
      go (NVPathF p)         = showsCon1 "NVPath"     p
      go (NVBuiltinF name _) = showsCon1 "NVBuiltin"  name

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

instance Eq (NValue m) where
    NVConstant (NFloat x) == NVConstant (NInt y)   = x == fromInteger y
    NVConstant (NInt x)   == NVConstant (NFloat y) = fromInteger x == y
    NVConstant (NInt x)   == NVConstant (NInt y)   = x == y
    NVConstant (NFloat x) == NVConstant (NFloat y) = x == y
    NVStr x _ == NVStr y _ = x < y
    NVPath x  == NVPath y  = x < y
    _         == _         = False

instance Ord (NValue m) where
    NVConstant (NFloat x) <= NVConstant (NInt y)   = x <= fromInteger y
    NVConstant (NInt x)   <= NVConstant (NFloat y) = fromInteger x <= y
    NVConstant (NInt x)   <= NVConstant (NInt y)   = x <= y
    NVConstant (NFloat x) <= NVConstant (NFloat y) = x <= y
    NVStr x _ <= NVStr y _ = x < y
    NVPath x  <= NVPath y  = x < y
    _         <= _         = False

checkComparable :: (Framed e m, MonadThrow m, Typeable m)
                => NValue m -> NValue m -> m ()
checkComparable x y = case (x, y) of
    (NVConstant (NFloat _), NVConstant (NInt _))   -> pure ()
    (NVConstant (NInt _),   NVConstant (NFloat _)) -> pure ()
    (NVConstant (NInt _),   NVConstant (NInt _))   -> pure ()
    (NVConstant (NFloat _), NVConstant (NFloat _)) -> pure ()
    (NVStr _ _, NVStr _ _) -> pure ()
    (NVPath _, NVPath _)   -> pure ()
    _ -> throwError $ Comparison x y

builtin :: Monad m
        => String -> (m (NValue m) -> m (NValue m)) -> m (NValue m)
builtin name f = return $ nvBuiltin name f

builtin2 :: Monad m
         => String -> (m (NValue m) -> m (NValue m) -> m (NValue m))
         -> m (NValue m)
builtin2 name f = builtin name (builtin name . f)

builtin3 :: Monad m
         => String
         -> (m (NValue m) -> m (NValue m) -> m (NValue m) -> m (NValue m))
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
    pairs <- forM (Data.Align.align fa fb) $ \case
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

data ValueType
    = TInt
    | TFloat
    | TBool
    | TUri
    | TNull
    | TString
    | TList
    | TSet
    | TClosure
    | TPath
    | TBuiltin
    deriving Show

valueType :: NValueF m r -> ValueType
valueType = \case
    NVConstantF a -> case a of
        NInt _    -> TInt
        NFloat _  -> TFloat
        NBool _   -> TBool
        NUri _    -> TUri
        NNull     -> TNull
    NVStrF {}     -> TString
    NVListF {}    -> TList
    NVSetF {}     -> TSet
    NVClosureF {} -> TClosure
    NVPathF {}    -> TPath
    NVBuiltinF {} -> TBuiltin

describeValue :: ValueType -> String
describeValue = \case
    TInt     -> "an integer"
    TFloat   -> "a float"
    TBool    -> "a boolean"
    TUri     -> "a URI"
    TNull    -> "a null"
    TString  -> "a string"
    TList    -> "a list"
    TSet     -> "an attr set"
    TClosure -> "a function"
    TPath    -> "a path"
    TBuiltin -> "a builtin function"

instance Show (NValueF m (NThunk m)) where
    show = show . describeValue . valueType

instance Show (NValue m) where
    show (NValue _ v)  = show v

instance Show (NThunk m) where
    show (NThunk _ (Value v)) = show v
    show (NThunk _ _) = "<thunk>"

data ValueFrame m
    = ForcingThunk
    | ConcerningValue (NValue m)
    | Comparison (NValue m) (NValue m)
    | Coercion ValueType ValueType
    | CoercionToJsonNF (NValueNF m)
    | CoercionFromJson A.Value
    | ExpectationNF ValueType (NValueNF m)
    | Expectation ValueType (NValue m)
    deriving (Show, Typeable)

instance Typeable m => Frame (ValueFrame m)
