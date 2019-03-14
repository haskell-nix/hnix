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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Nix.Value where

import           Control.Comonad
import           Control.Exception
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import qualified Data.Aeson as A
import           Data.Align
import           Data.Functor.Classes
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Data.These
import           Data.Typeable (Typeable)
import           Data.Void
import           GHC.Generics
import           Lens.Family2
import           Lens.Family2.Stock
import           Lens.Family2.TH
import           Nix.Atoms
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.String
import           Nix.Thunk
import           Nix.Utils

-- | An 'NValue' is the most reduced form of an 'NExpr' after evaluation is
--   completed. 's' is related to the type of errors that might occur during
--   construction or use of a value.
data NValueF p m r
    = NVConstantF NAtom
     -- | A string has a value and a context, which can be used to record what a
     -- string has been build from
    | NVStrF NixString
    | NVPathF FilePath
    | NVListF [r]
    | NVSetF (AttrSet r) (AttrSet SourcePos)
    | NVClosureF (Params ()) (m p -> m r)
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
    | NVBuiltinF String (m p -> m r)
      -- ^ A builtin function is itself already in normal form. Also, it may
      --   or may not choose to evaluate its argument in the production of a
      --   result.
    deriving (Generic, Typeable, Functor)

-- | This 'Foldable' instance only folds what the value actually is known to
--   contain at time of fold.
instance Foldable (NValueF p m) where
    foldMap f = \case
        NVConstantF _  -> mempty
        NVStrF _       -> mempty
        NVPathF _      -> mempty
        NVListF l      -> foldMap f l
        NVSetF s _     -> foldMap f s
        NVClosureF _ _ -> mempty
        NVBuiltinF _ _ -> mempty

bindNValueF :: (Monad m, Monad n)
            => (forall x. n x -> m x) -> (a -> n b) -> NValueF p m a
            -> n (NValueF p m b)
bindNValueF transform f = \case
    NVConstantF a  -> pure $ NVConstantF a
    NVStrF s       -> pure $ NVStrF s
    NVPathF p      -> pure $ NVPathF p
    NVListF l      -> NVListF <$> traverse f l
    NVSetF s p     -> NVSetF <$> traverse f s <*> pure p
    NVClosureF p g -> pure $ NVClosureF p (transform . f <=< g)
    NVBuiltinF s g -> pure $ NVBuiltinF s (transform . f <=< g)

lmapNValueF :: Functor m => (b -> a) -> NValueF a m r -> NValueF b m r
lmapNValueF f = \case
    NVConstantF a  -> NVConstantF a
    NVStrF s       -> NVStrF s
    NVPathF p      -> NVPathF p
    NVListF l      -> NVListF l
    NVSetF s p     -> NVSetF s p
    NVClosureF p g -> NVClosureF p (g . fmap f)
    NVBuiltinF s g -> NVBuiltinF s (g . fmap f)

type MonadDataContext f (m :: * -> *) =
    (Show1 f, Comonad f, Applicative f, Traversable f, Monad m)

-- | At the time of constructor, the expected arguments to closures are values
--   that may contain thunks. The type of such thunks are fixed at that time.
newtype NValue' t f m a = NValue { _nValue :: f (NValueF (NValue t f m) m a) }
    deriving (Generic, Typeable, Functor, Foldable)

instance Show r => Show (NValueF p m r) where
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

instance (MonadDataContext f m, Show a) => Show (NValue' t f m a) where
    show (NValue (extract -> v)) = show v

type NValue t f m = NValue' t f m t

bindNValue :: (Traversable f, Monad m, Monad n)
           => (forall x. n x -> m x) -> (a -> n b) -> NValue' t f m a
           -> n (NValue' t f m b)
bindNValue transform f (NValue v) =
    NValue <$> traverse (bindNValueF transform f) v

-- | An 'NValueNF' is a fully evaluated value in normal form. An 'NValue f t m' is
--   a value in head normal form, where only the "top layer" has been
--   evaluated. An action of type 'm (NValue f t m)' is a pending evualation that
--   has yet to be performed. An 't' is either a pending evaluation, or
--   a value in head normal form. A 'NThunkSet' is a set of mappings from keys
--   to thunks.
--
--   The 'Free' structure is used here to represent the possibility that
--   cycles may appear during normalization.

type NValueNF t f m = Free (NValue' t f m) (NValue' t f m Void)

iterNValue
    :: forall t f m a r. MonadDataContext f m
    => (a -> (NValue' t f m a -> r) -> r)
    -> (NValue' t f m r -> r)
    -> NValue' t f m a -> r
iterNValue k f = f . fmap (\a -> k a (iterNValue k f))

iterNValueM
    :: (MonadDataContext f m, Monad n)
    => (forall x. n x -> m x)
    -> (a -> (NValue' t f m a -> n r) -> n r)
    -> (NValue' t f m r -> n r)
    -> NValue' t f m a -> n r
iterNValueM transform k f =
    f <=< bindNValue transform (\a -> k a (iterNValueM transform k f))

iterNValueNF
    :: MonadDataContext f m
    => (NValue' t f m Void -> r)
    -> (NValue' t f m r -> r)
    -> NValueNF t f m -> r
iterNValueNF k f = iter f . fmap k

iterNValueNFM
    :: forall f m n t r. (MonadDataContext f m, Monad n)
    => (NValue' t f m Void -> n r)
    -> (NValue' t f m (n r) -> n r)
    -> NValueNF t f m -> n r
iterNValueNFM k f v = join (iterM (pure . f . fmap join) (fmap k v))

nValueFromNF :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
             => NValueNF t f m -> NValue t f m
nValueFromNF = iterNValueNF (fmap absurd) (fmap wrapValue)

nValueToNF :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
           => (t -> (NValue t f m -> NValueNF t f m) -> NValueNF t f m)
           -> NValue t f m
           -> NValueNF t f m
nValueToNF k = iterNValue k Free

nValueToNFM
    :: (MonadDataContext f m, Monad n)
    => (forall x. n x -> m x)
    -> (t -> (NValue t f m -> n (NValueNF t f m)) -> n (NValueNF t f m))
    -> NValue t f m
    -> n (NValueNF t f m)
nValueToNFM transform k = iterNValueM transform k $ pure . Free

pattern NVConstant x <- NValue (extract -> NVConstantF x)
pattern NVConstantNF x <- Free (NValue (extract -> NVConstantF x))

nvConstant :: MonadDataContext f m => NAtom -> NValue t f m
nvConstant x = NValue (pure (NVConstantF x))
nvConstantNF :: MonadDataContext f m => NAtom -> NValueNF t f m
nvConstantNF x = Free (NValue (pure (NVConstantF x)))

pattern NVStr ns <- NValue (extract -> NVStrF ns)
pattern NVStrNF ns <- Free (NValue (extract -> NVStrF ns))

nvStr :: MonadDataContext f m => NixString -> NValue t f m
nvStr ns = NValue (pure (NVStrF ns))
nvStrNF :: MonadDataContext f m => NixString -> NValueNF t f m
nvStrNF ns = Free (NValue (pure (NVStrF ns)))

pattern NVPath x <- NValue (extract -> NVPathF x)
pattern NVPathNF x <- Free (NValue (extract -> NVPathF x))

nvPath :: MonadDataContext f m => FilePath -> NValue t f m
nvPath x = NValue (pure (NVPathF x))
nvPathNF :: MonadDataContext f m => FilePath -> NValueNF t f m
nvPathNF x = Free (NValue (pure (NVPathF x)))

pattern NVList l <- NValue (extract -> NVListF l)
pattern NVListNF l <- Free (NValue (extract -> NVListF l))

nvList :: MonadDataContext f m => [t] -> NValue t f m
nvList l = NValue (pure (NVListF l))
nvListNF :: MonadDataContext f m => [NValueNF t f m] -> NValueNF t f m
nvListNF l = Free (NValue (pure (NVListF l)))

pattern NVSet s x <- NValue (extract -> NVSetF s x)
pattern NVSetNF s x <- Free (NValue (extract -> NVSetF s x))

nvSet :: MonadDataContext f m
      => HashMap Text t -> HashMap Text SourcePos -> NValue t f m
nvSet s x = NValue (pure (NVSetF s x))
nvSetNF :: MonadDataContext f m
        => HashMap Text (NValueNF t f m) -> HashMap Text SourcePos -> NValueNF t f m
nvSetNF s x = Free (NValue (pure (NVSetF s x)))

pattern NVClosure x f <- NValue (extract -> NVClosureF x f)
pattern NVClosureNF x f <- Free (NValue (extract -> NVClosureF x f))

nvClosure :: MonadDataContext f m
          => Params () -> (m (NValue t f m) -> m t) -> NValue t f m
nvClosure x f = NValue (pure (NVClosureF x f))
nvClosureNF :: MonadDataContext f m
            => Params () -> (m (NValue t f m) -> m (NValueNF t f m)) -> NValueNF t f m
nvClosureNF x f = Free (NValue (pure (NVClosureF x f)))

pattern NVBuiltin name f <- NValue (extract -> NVBuiltinF name f)
pattern NVBuiltinNF name f <- Free (NValue (extract -> NVBuiltinF name f))

nvBuiltin :: MonadDataContext f m
          => String -> (m (NValue t f m) -> m t) -> NValue t f m
nvBuiltin name f = NValue (pure (NVBuiltinF name f))
nvBuiltinNF :: MonadDataContext f m
            => String -> (m (NValue t f m) -> m (NValueNF t f m)) -> NValueNF t f m
nvBuiltinNF name f = Free (NValue (pure (NVBuiltinF name f)))

instance MonadDataContext f m => Eq (NValue t f m) where
    NVConstant (NFloat x) == NVConstant (NInt y)   = x == fromInteger y
    NVConstant (NInt x)   == NVConstant (NFloat y) = fromInteger x == y
    NVConstant (NInt x)   == NVConstant (NInt y)   = x == y
    NVConstant (NFloat x) == NVConstant (NFloat y) = x == y
    NVStr x   == NVStr y   = hackyStringIgnoreContext x == hackyStringIgnoreContext y
    NVPath x  == NVPath y  = x == y
    _         == _         = False

instance MonadDataContext f m => Ord (NValue t f m) where
    NVConstant (NFloat x) <= NVConstant (NInt y)   = x <= fromInteger y
    NVConstant (NInt x)   <= NVConstant (NFloat y) = fromInteger x <= y
    NVConstant (NInt x)   <= NVConstant (NInt y)   = x <= y
    NVConstant (NFloat x) <= NVConstant (NFloat y) = x <= y
    NVStr x   <= NVStr y   = hackyStringIgnoreContext x <= hackyStringIgnoreContext y
    NVPath x  <= NVPath y  = x <= y
    _         <= _         = False

checkComparable :: (Framed e m, MonadDataErrorContext t f m)
                => NValue t f m -> NValue t f m -> m ()
checkComparable x y = case (x, y) of
    (NVConstant (NFloat _), NVConstant (NInt _))   -> pure ()
    (NVConstant (NInt _),   NVConstant (NFloat _)) -> pure ()
    (NVConstant (NInt _),   NVConstant (NInt _))   -> pure ()
    (NVConstant (NFloat _), NVConstant (NFloat _)) -> pure ()
    (NVStr _, NVStr _)     -> pure ()
    (NVPath _, NVPath _)   -> pure ()
    _ -> throwError $ Comparison x y

type IsThunk f m t = (MonadThunk t m (NValue t f m), MonadDataContext f m)

thunkEq :: IsThunk f m t => t -> t -> m Bool
thunkEq lt rt = force lt $ \lv -> force rt $ \rv ->
 let unsafePtrEq = case (lt, rt) of
         (thunkId -> lid, thunkId -> rid)
             | lid == rid -> return True
         _ -> valueEq lv rv
 in case (lv, rv) of
   (NVClosure _ _, NVClosure _ _) -> unsafePtrEq
   (NVList _, NVList _) -> unsafePtrEq
   (NVSet _ _, NVSet _ _) -> unsafePtrEq
   _ -> valueEq lv rv

builtin :: forall m f t. (MonadThunk t m (NValue t f m), MonadDataContext f m)
        => String -> (m (NValue t f m) -> m (NValue t f m)) -> m (NValue t f m)
builtin name f = return $ nvBuiltin name $ thunk . f

builtin2 :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
         => String -> (m (NValue t f m) -> m (NValue t f m) -> m (NValue t f m))
         -> m (NValue t f m)
builtin2 name f = builtin name (builtin name . f)

builtin3 :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
         => String
         -> (m (NValue t f m) -> m (NValue t f m) -> m (NValue t f m) -> m (NValue t f m))
         -> m (NValue t f m)
builtin3 name f =
    builtin name $ \a -> builtin name $ \b -> builtin name $ \c -> f a b c

isClosureNF :: MonadDataContext f m => NValueNF t f m -> Bool
isClosureNF NVClosureNF {} = True
isClosureNF _ = False

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

isDerivation :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
             => AttrSet t -> m Bool
isDerivation m = case M.lookup "type" m of
    Nothing -> pure False
    Just t -> force t $ \case
      -- We should probably really make sure the context is empty here but the
      -- C++ implementation ignores it.
      NVStr s -> pure $ principledStringIgnoreContext s == "derivation"
      _ -> pure False

valueEq :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
        => NValue t f m -> NValue t f m -> m Bool
valueEq = curry $ \case
    (NVConstant lc, NVConstant rc) -> pure $ lc == rc
    (NVStr ls, NVStr rs) ->
        pure $ principledStringIgnoreContext ls
            == principledStringIgnoreContext rs
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

valueType :: NValueF a m r -> ValueType
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

instance Eq1 (NValueF (NValue' t f m a) m) where
    liftEq _  (NVConstantF x) (NVConstantF y) = x == y
    liftEq _  (NVStrF x)      (NVStrF y)      = x == y
    liftEq eq (NVListF x)     (NVListF y)     = liftEq eq x y
    liftEq eq (NVSetF x _)    (NVSetF y _)    = liftEq eq x y
    liftEq _  (NVPathF x)     (NVPathF y)     = x == y
    liftEq _ _ _ = False

instance MonadDataContext f m => Show1 (NValue' t f m) where
    liftShowsPrec sp sl p = \case
        NVConstant atom  -> showsUnaryWith showsPrec "NVConstantF" p atom
        NVStr ns         -> showsUnaryWith showsPrec "NVStrF"      p
                                          (hackyStringIgnoreContext ns)
        NVList     lst   -> showsUnaryWith (liftShowsPrec sp sl) "NVListF" p lst
        NVSet attrs _    -> showsUnaryWith (liftShowsPrec sp sl) "NVSetF"  p attrs
        NVPath path      -> showsUnaryWith showsPrec "NVPathF"     p path
        NVClosure c _    -> showsUnaryWith showsPrec "NVClosureF"  p c
        NVBuiltin name _ -> showsUnaryWith showsPrec "NVBuiltinF"  p name
        _                -> error "Pattern synonyms mask coverage"

data ValueFrame t f m
    = ForcingThunk
    | ConcerningValue (NValue t f m)
    | Comparison (NValue t f m) (NValue t f m)
    | Addition (NValue t f m) (NValue t f m)
    | Multiplication (NValue t f m) (NValue t f m)
    | Division (NValue t f m) (NValue t f m)
    | Coercion ValueType ValueType
    | CoercionToJson (NValue t f m)
    | CoercionFromJson A.Value
    | ExpectationNF ValueType (NValueNF t f m)
    | Expectation ValueType (NValue t f m)
    deriving (Show, Typeable)

type MonadDataErrorContext t f m =
    (Show t, Typeable t, Typeable m, Typeable f, MonadDataContext f m)

instance MonadDataErrorContext t f m => Exception (ValueFrame t f m)

$(makeTraversals ''NValueF)
$(makeLenses ''NValue')

key :: (MonadDataContext f m, Applicative g)
    => VarName -> LensLike' g (NValue' t f m a) (Maybe a)
key k = nValue.traverse._NVSetF._1.hashAt k
