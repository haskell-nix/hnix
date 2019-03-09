{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Nix.Value where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import qualified Data.Aeson as A
import           Data.Align
import           Data.Fix
import           Data.Functor.Classes
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.These
import           Data.Typeable (Typeable)
import           GHC.Generics
import           Lens.Family2
import           Lens.Family2.Stock
import           Lens.Family2.TH
import           Nix.Atoms
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.String
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
    | NVStrF NixString
    | NVPathF FilePath
    | NVListF [r]
    | NVSetF (AttrSet r) (AttrSet SourcePos)
    | NVClosureF (Params ()) (m (NValue m) -> m (NValue m))
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
--
--   The 'Free' structure is used here to represent the possibility that
--   cycles may appear during normalization.

type NValueNF m = Free (NValueF m) (NValue m)
type ValueSet m = AttrSet (NThunk m)

data Provenance m = Provenance
    { _lexicalScope :: Scopes m (NThunk m)
    , _originExpr   :: NExprLocF (Maybe (NValue m))
      -- ^ When calling the function x: x + 2 with argument x = 3, the
      --   'originExpr' for the resulting value will be 3 + 2, while the
      --   'contextExpr' will be @(x: x + 2) 3@, preserving not only the
      --   result of the call, but what was called and with what arguments.
    }

data NThunk m = NThunk
    { _thunkProvenance :: [Provenance m]
    , _baseThunk       :: Thunk m (NValue m)
    }

data NValue m = NValue
    { _valueProvenance :: [Provenance m]
    , _baseValue       :: NValueF m (NThunk m)
    }

addProvenance :: (NValue m -> Provenance m) -> NValue m -> NValue m
addProvenance f l@(NValue p v) = NValue (f l : p) v

pattern NVConstant x <- NValue _ (NVConstantF x)

nvConstant x = NValue [] (NVConstantF x)
nvConstantP p x = NValue [p] (NVConstantF x)

pattern NVStr ns <- NValue _ (NVStrF ns)

nvStr ns = NValue [] (NVStrF ns)
nvStrP p ns = NValue [p] (NVStrF ns)

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
      go (NVStrF ns)         = showsCon1 "NVStr"      (hackyStringIgnoreContext ns)
      go (NVListF     lst)   = showsCon1 "NVList"     lst
      go (NVSetF attrs _)    = showsCon1 "NVSet"      attrs
      go (NVClosureF p _)    = showsCon1 "NVClosure"  p
      go (NVPathF p)         = showsCon1 "NVPath"     p
      go (NVBuiltinF name _) = showsCon1 "NVBuiltin"  name

      showsCon1 :: Show a => String -> a -> Int -> String -> String
      showsCon1 con a d =
          showParen (d > 10) $ showString (con ++ " ") . showsPrec 11 a
{-
      showsCon2 :: (Show a, Show b)
                => String -> a -> b -> Int -> String -> String
      showsCon2 con a b d =
          showParen (d > 10)
              $ showString (con ++ " ")
              . showsPrec 11 a
              . showString " "
              . showsPrec 11 b
-}
instance Eq (NValue m) where
    NVConstant (NFloat x) == NVConstant (NInt y)   = x == fromInteger y
    NVConstant (NInt x)   == NVConstant (NFloat y) = fromInteger x == y
    NVConstant (NInt x)   == NVConstant (NInt y)   = x == y
    NVConstant (NFloat x) == NVConstant (NFloat y) = x == y
    NVStr x   == NVStr y   = hackyStringIgnoreContext x == hackyStringIgnoreContext y
    NVPath x  == NVPath y  = x == y
    _         == _         = False

instance Ord (NValue m) where
    NVConstant (NFloat x) <= NVConstant (NInt y)   = x <= fromInteger y
    NVConstant (NInt x)   <= NVConstant (NFloat y) = fromInteger x <= y
    NVConstant (NInt x)   <= NVConstant (NInt y)   = x <= y
    NVConstant (NFloat x) <= NVConstant (NFloat y) = x <= y
    NVStr x   <= NVStr y   = hackyStringIgnoreContext x <= hackyStringIgnoreContext y
    NVPath x  <= NVPath y  = x <= y
    _         <= _         = False

checkComparable :: (Framed e m, Typeable m) => NValue m -> NValue m -> m ()
checkComparable x y = case (x, y) of
    (NVConstant (NFloat _), NVConstant (NInt _))   -> pure ()
    (NVConstant (NInt _),   NVConstant (NFloat _)) -> pure ()
    (NVConstant (NInt _),   NVConstant (NInt _))   -> pure ()
    (NVConstant (NFloat _), NVConstant (NFloat _)) -> pure ()
    (NVStr _, NVStr _)     -> pure ()
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
isClosureNF (Free NVClosureF {}) = True
isClosureNF _ = False

thunkEq :: MonadThunk (NValue m) (NThunk m) m
        => NThunk m -> NThunk m -> m Bool
thunkEq (NThunk _ (Thunk lid _ _)) (NThunk _ (Thunk rid _ _)) | lid == rid = return True
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
    Just t -> force t $ \case
      -- We should probably really make sure the context is empty here but the
      -- C++ implementation ignores it.
      NVStr s -> pure $ principledStringIgnoreContext s == "derivation"
      _ -> pure False

valueEq :: MonadThunk (NValue m) (NThunk m) m
        => NValue m -> NValue m -> m Bool
valueEq = curry $ \case
    (NVConstant lc, NVConstant rc) -> pure $ lc == rc
    (NVStr ls, NVStr rs) -> pure $ principledStringIgnoreContext ls == principledStringIgnoreContext rs
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


data TStringContext = NoContext | HasContext
  deriving Show

data ValueType
    = TInt
    | TFloat
    | TBool
    | TNull
    | TString TStringContext
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
        NNull     -> TNull
    NVStrF ns | stringHasContext ns ->  TString HasContext
              | otherwise -> TString NoContext
    NVListF {}    -> TList
    NVSetF {}     -> TSet
    NVClosureF {} -> TClosure
    NVPathF {}    -> TPath
    NVBuiltinF {} -> TBuiltin

describeValue :: ValueType -> String
describeValue = \case
    TInt               -> "an integer"
    TFloat             -> "a float"
    TBool              -> "a boolean"
    TNull              -> "a null"
    TString NoContext  -> "a string"
    TString HasContext -> "a string with context"
    TList              -> "a list"
    TSet               -> "an attr set"
    TClosure           -> "a function"
    TPath              -> "a path"
    TBuiltin           -> "a builtin function"

instance Show (NValueF m (NThunk m)) where
    show = show . describeValue . valueType

instance Show (NValue m) where
    show (NValue _ v)  = show v

instance Show (NThunk m) where
    show (NThunk _ (Value v)) = show v
    show (NThunk _ _) = "<thunk>"

instance Eq1 (NValueF m) where
    liftEq _  (NVConstantF x)  (NVConstantF y)  = x == y
    liftEq _  (NVStrF x)     (NVStrF y)     = x == y
    liftEq eq (NVListF x)      (NVListF y)      = liftEq eq x y
    liftEq eq (NVSetF x _)     (NVSetF y _)     = liftEq eq x y
    liftEq _  (NVPathF x)      (NVPathF y)      = x == y
    liftEq _ _ _ = False

instance Show1 (NValueF m) where
    liftShowsPrec sp sl p = \case
        NVConstantF atom  -> showsUnaryWith showsPrec "NVConstantF" p atom
        NVStrF ns         -> showsUnaryWith showsPrec "NVStrF"      p (hackyStringIgnoreContext ns)
        NVListF     lst   -> showsUnaryWith (liftShowsPrec sp sl) "NVListF" p lst
        NVSetF attrs _    -> showsUnaryWith (liftShowsPrec sp sl) "NVSetF"  p attrs
        NVClosureF c _    -> showsUnaryWith showsPrec "NVClosureF"  p c
        NVPathF path      -> showsUnaryWith showsPrec "NVPathF"     p path
        NVBuiltinF name _ -> showsUnaryWith showsPrec "NVBuiltinF"  p name

data ValueFrame m
    = ForcingThunk
    | ConcerningValue (NValue m)
    | Comparison (NValue m) (NValue m)
    | Addition (NValue m) (NValue m)
    | Multiplication (NValue m) (NValue m)
    | Division (NValue m) (NValue m)
    | Coercion ValueType ValueType
    | CoercionToJson (NValue m)
    | CoercionFromJson A.Value
    | ExpectationNF ValueType (NValueNF m)
    | Expectation ValueType (NValue m)
    deriving (Show, Typeable)

instance Typeable m => Exception (ValueFrame m)

$(makeTraversals ''NValueF)
$(makeLenses ''Provenance)
$(makeLenses ''NThunk)
$(makeLenses ''NValue)

alterF :: (Eq k, Hashable k, Functor f)
       => (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
alterF f k m = f (M.lookup k m) <&> \case
    Nothing -> M.delete k m
    Just v  -> M.insert k v m

hashAt :: VarName -> Lens' (AttrSet v) (Maybe v)
hashAt = flip alterF

key :: Applicative f => VarName -> LensLike' f (NValue m) (Maybe (NThunk m))
key k = baseValue._NVSetF._1.hashAt k
