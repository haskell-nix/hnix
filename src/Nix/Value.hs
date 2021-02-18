{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}  -- @key@ in Aeson reports: @key@ often has better inference than @ix@ when used with OverloadedStrings.
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

-- | The core of the type system, Nix language values
module Nix.Value
where

import           Data.Bool                      ( bool )
import           Control.Comonad                ( Comonad, extract )
import           Control.Exception              ( Exception )
import           Control.Monad                  ( (<=<) )
import           Control.Monad.Free             ( Free(..)
                                                , hoistFree, iter, iterM )
import           Control.Monad.Trans.Class      ( MonadTrans, lift )
import qualified Data.Aeson                    as Aeson
import           Data.Functor.Classes           ( Show1
                                                , liftShowsPrec
                                                , showsUnaryWith
                                                , Eq1(liftEq) )
import           Data.HashMap.Lazy              ( HashMap )
import           Data.Text                      ( Text )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           Lens.Family2.Stock             ( _1 )
import           Lens.Family2.TH                ( makeTraversals
                                                , makeLenses )
import           Nix.Atoms
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.String
import           Nix.Thunk
import           Nix.Utils


-- * @__NValueF__@: Base functor

-- | 'NValue' is the most reduced form of a 'NExpr' after evaluation is
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
    | NVClosureF (Params ()) (p -> m r)
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
    | NVBuiltinF String (p -> m r)
      -- ^ A builtin function is itself already in normal form. Also, it may
      --   or may not choose to evaluate its argument in the production of a
      --   result.
  deriving (Generic, Typeable, Functor)


-- ** Eq1

instance Eq1 (NValueF p m) where
  liftEq _  (NVConstantF x) (NVConstantF y) = x == y
  liftEq _  (NVStrF      x) (NVStrF      y) = x == y
  liftEq eq (NVListF     x) (NVListF     y) = liftEq eq x y
  liftEq eq (NVSetF x _   ) (NVSetF y _   ) = liftEq eq x y
  liftEq _  (NVPathF x    ) (NVPathF y    ) = x == y
  liftEq _  _               _               = False


-- ** Show

instance Show r => Show (NValueF p m r) where
  showsPrec = flip go
   where
    go :: NValueF p m r -> Int -> String -> String
    go = \case
      (NVConstantF atom     ) -> showsCon1 "NVConstant" atom
      (NVStrF      ns       ) -> showsCon1 "NVStr" (stringIgnoreContext ns)
      (NVListF     lst      ) -> showsCon1 "NVList" lst
      (NVSetF      attrs  _ ) -> showsCon1 "NVSet" attrs
      (NVClosureF  params _ ) -> showsCon1 "NVClosure" params
      (NVPathF     path     ) -> showsCon1 "NVPath" path
      (NVBuiltinF  name   _ ) -> showsCon1 "NVBuiltin" name

    showsCon1 :: Show a => String -> a -> Int -> String -> String
    showsCon1 con a d =
      showParen (d > 10) $ showString (con <> " ") . showsPrec 11 a


-- ** Foldable

-- | Folds what the value is known to contain at time of fold.
instance Foldable (NValueF p m) where
  foldMap f = \case
    NVConstantF _  -> mempty
    NVStrF      _  -> mempty
    NVPathF     _  -> mempty
    NVListF     l  -> foldMap f l
    NVSetF     s _ -> foldMap f s
    NVClosureF _ _ -> mempty
    NVBuiltinF _ _ -> mempty


-- ** Traversable

-- | @traverse@
sequenceNValueF
  :: (Functor n, Monad m, Applicative n)
  => (forall x . n x -> m x)
  -> NValueF p m (n a)
  -> n (NValueF p m a)
sequenceNValueF transform = \case
  NVConstantF a  -> pure $ NVConstantF a
  NVStrF      s  -> pure $ NVStrF s
  NVPathF     p  -> pure $ NVPathF p
  NVListF     l  -> NVListF <$> sequenceA l
  NVSetF     s p -> NVSetF <$> sequenceA s <*> pure p
  NVClosureF p g -> pure $ NVClosureF p (transform <=< g)
  NVBuiltinF s g -> pure $ NVBuiltinF s (transform <=< g)


-- ** Monad

-- | @bind@
bindNValueF
  :: (Monad m, Monad n)
  => (forall x . n x -> m x)
  -> (a -> n b)
  -> NValueF p m a
  -> n (NValueF p m b)
bindNValueF transform f = \case
  NVConstantF a  -> pure $ NVConstantF a
  NVStrF      s  -> pure $ NVStrF s
  NVPathF     p  -> pure $ NVPathF p
  NVListF     l  -> NVListF <$> traverse f l
  NVSetF     s p -> NVSetF <$> traverse f s <*> pure p
  NVClosureF p g -> pure $ NVClosureF p (transform . f <=< g)
  NVBuiltinF s g -> pure $ NVBuiltinF s (transform . f <=< g)


-- *** MonadTrans

-- | @lift@
liftNValueF
  :: (MonadTrans u, Monad m)
  => NValueF p m a
  -> NValueF p (u m) a
liftNValueF = hoistNValueF lift

-- **** MonadTransUnlift

-- | @unlift@
unliftNValueF
  :: (MonadTrans u, Monad m)
  => (forall x . u m x -> m x)
  -> NValueF p (u m) a
  -> NValueF p m a
unliftNValueF = hoistNValueF

-- **** Utils

-- | Back & forth hoisting in the monad stack
hoistNValueF
  :: (forall x . m x -> n x)
  -> NValueF p m a
  -> NValueF p n a
hoistNValueF lft = \case
  NVConstantF a  -> NVConstantF a
  NVStrF      s  -> NVStrF s
  NVPathF     p  -> NVPathF p
  NVListF     l  -> NVListF l
  NVSetF     s p -> NVSetF s p
  NVClosureF p g -> NVClosureF p (lft . g)
  NVBuiltinF s g -> NVBuiltinF s (lft . g)


-- * @__NValue'__@: forming the (F(A))

-- | At the time of constructor, the expected arguments to closures are values
--   that may contain thunks. The type of such thunks are fixed at that time.
newtype NValue' t f m a =
  NValue
    {
    -- | Applying F-algebra carrier (@NValue@) to the F-algebra Base functor data type (@NValueF@), forming the \( F(A)-> A \)).
    _nValue :: f (NValueF (NValue t f m) m a)
    }
  deriving (Generic, Typeable, Functor, Foldable, Eq1)

instance (Comonad f, Show a) => Show (NValue' t f m a) where
  show (NValue (extract -> v)) = show v


-- ** Show1

instance Comonad f => Show1 (NValue' t f m) where
  liftShowsPrec sp sl p = \case
    NVConstant' atom  -> showsUnaryWith showsPrec "NVConstantF" p atom
    NVStr' ns ->
      showsUnaryWith showsPrec "NVStrF" p (stringIgnoreContext ns)
    NVList' lst       -> showsUnaryWith (liftShowsPrec sp sl) "NVListF" p lst
    NVSet' attrs _    -> showsUnaryWith (liftShowsPrec sp sl) "NVSetF" p attrs
    NVPath' path      -> showsUnaryWith showsPrec "NVPathF" p path
    NVClosure' c    _ -> showsUnaryWith showsPrec "NVClosureF" p c
    NVBuiltin' name _ -> showsUnaryWith showsPrec "NVBuiltinF" p name
    _                 -> error "Pattern synonyms mask coverage"


-- ** Traversable

-- | @traverse@
sequenceNValue'
  :: (Functor n, Traversable f, Monad m, Applicative n)
  => (forall x . n x -> m x)
  -> NValue' t f m (n a)
  -> n (NValue' t f m a)
sequenceNValue' transform (NValue v) =
  NValue <$> traverse (sequenceNValueF transform) v


-- ** Profunctor

-- | @lmap@
lmapNValueF :: Functor m => (b -> a) -> NValueF a m r -> NValueF b m r
lmapNValueF f = \case
  NVConstantF a  -> NVConstantF a
  NVStrF      s  -> NVStrF s
  NVPathF     p  -> NVPathF p
  NVListF     l  -> NVListF l
  NVSetF     s p -> NVSetF s p
  NVClosureF p g -> NVClosureF p (g . f)
  NVBuiltinF s g -> NVBuiltinF s (g . f)


-- ** Free

-- | @iter@
iterNValue'
  :: forall t f m a r
   . MonadDataContext f m
  => (a -> (NValue' t f m a -> r) -> r)
  -> (NValue' t f m r -> r)
  -> NValue' t f m a
  -> r
iterNValue' k f = f . fmap (\a -> k a (iterNValue' k f))


-- | @hoistFree@: Back & forth hoisting in the monad stack
hoistNValue'
  :: (Functor m, Functor n, Functor f)
  => (forall x . n x -> m x)
  -> (forall x . m x -> n x)
  -> NValue' t f m a
  -> NValue' t f n a
hoistNValue' run lft (NValue v) =
    NValue $ lmapNValueF (hoistNValue lft run) . hoistNValueF lft <$> v


-- ** Monad

-- |@bind@
bindNValue'
  :: (Traversable f, Monad m, Monad n)
  => (forall x . n x -> m x)
  -> (a -> n b)
  -> NValue' t f m a
  -> n (NValue' t f m b)
bindNValue' transform f (NValue v) =
  NValue <$> traverse (bindNValueF transform f) v

-- *** MonadTrans

-- | @lift@
liftNValue'
  :: (MonadTrans u, Monad m, Functor (u m), Functor f)
  => (forall x . u m x -> m x)
  -> NValue' t f m a
  -> NValue' t f (u m) a
liftNValue' run = hoistNValue' run lift

-- **** MonadTransUnlift

-- | @unlift@
unliftNValue'
  :: (MonadTrans u, Monad m, Functor (u m), Functor f)
  => (forall x . u m x -> m x) -- aka "run"
  -> NValue' t f (u m) a
  -> NValue' t f m a
unliftNValue' = hoistNValue' lift


-- ** Bijective Hask subcategory <-> @NValue'@
-- *** @F: Hask subcategory → NValue'@
--
-- #mantra#
-- $Methods @F: Hask → NValue'@
--
-- Since Haskell and Nix are both recursive purely functional lazy languages.
-- And since recursion-schemes.
-- It is possible to create a direct functor between the Hask and Nix categories.
-- Or make Nix a DLS language of Haskell, embed it into a Hask, if you would like.
-- Of course, we mean: pick Hask subcategory and form Nix Category from it.
-- Take subcategory of Hask, and by applying functor to it - have a Nix Category.
-- Wouldn't it be cool and fast?
--
-- In fact - it is what we do here.
--
-- Since it is a proper way of scientific implementation, we would eventually form a
-- lawful functor.
--
-- Facts of which are seen below:


-- | Haskell constant to the Nix constant,
nvConstant' :: Applicative f
  => NAtom
  -> NValue' t f m r
nvConstant' = NValue . pure . NVConstantF


-- | Haskell text & context to the Nix text & context,
nvStr' :: Applicative f
  => NixString
  -> NValue' t f m r
nvStr' = NValue . pure . NVStrF


-- | Haskell @FilePath@ to the Nix path,
nvPath' :: Applicative f
  => FilePath
  -> NValue' t f m r
nvPath' = NValue . pure . NVPathF


-- | Haskell @[]@ to the Nix @[]@,
nvList' :: Applicative f
  => [r]
  -> NValue' t f m r
nvList' = NValue . pure . NVListF


-- | Haskell key-value to the Nix key-value,
nvSet' :: Applicative f
  => HashMap Text r
  -> HashMap Text SourcePos
  -> NValue' t f m r
nvSet' s x = NValue $ pure $ NVSetF s x


-- | Haskell closure to the Nix closure,
nvClosure' :: (Applicative f, Functor m)
  => Params ()
  -> (NValue t f m
      -> m r
    )
  -> NValue' t f m r
nvClosure' x f = NValue $ pure $ NVClosureF x f


-- | Haskell functions to the Nix functions!
nvBuiltin' :: (Applicative f, Functor m)
  => String
  -> (NValue t f m -> m r)
  -> NValue' t f m r
nvBuiltin' name f = NValue $ pure $ NVBuiltinF name f


-- So above we have maps of Hask subcategory objects to Nix objects,
-- and Hask subcategory morphisms to Nix morphisms.

-- *** @F: NValue -> NValue'@

-- | Module pattens use @language PatternSynonyms@: unidirectional synonyms (@<-@),
-- and @ViewPatterns@: (@->@) at the same time.
-- @ViewPatterns Control.Comonad.extract@ extracts
-- from the @NValue (Free (NValueF a))@
-- the @NValueF a@. Which is @NValueF p m r@. Since it extracted from the
-- @NValue@, which is formed by \( (F a -> a) F a \) in the first place.
-- So @NValueF p m r@ which is extracted here, internally holds the next NValue.
pattern NVConstant' x <- NValue (extract -> NVConstantF x)
pattern NVStr' ns <- NValue (extract -> NVStrF ns)
pattern NVPath' x <- NValue (extract -> NVPathF x)
pattern NVList' l <- NValue (extract -> NVListF l)
pattern NVSet' s x <- NValue (extract -> NVSetF s x)
pattern NVClosure' x f <- NValue (extract -> NVClosureF x f)
pattern NVBuiltin' name f <- NValue (extract -> NVBuiltinF name f)


-- * @__NValue__@: Nix language values

-- | 'NValue t f m' is
--   a value in head normal form (it means only the tip of it has been
--   evaluated to the normal form, while the rest of it is in lazy
--   not evaluated form (thunk), this known as WHNF).
--
--   An action 'm (NValue t f m)' is a pending evaluation that
--   has yet to be performed.
--
--   An 't' is either:
--     * a pending evaluation.
--     * a value in head normal form.
--
--   The 'Free' structure is used here to represent the possibility that
--   Nix language allows cycles that may appear during normalization.

type NValue t f m = Free (NValue' t f m) t


-- ** Free

-- | @iter@
iterNValue
  :: forall t f m r
   . MonadDataContext f m
  => (t -> (NValue t f m -> r) -> r)
  -> (NValue' t f m r -> r)
  -> NValue t f m
  -> r
iterNValue k f = iter f . fmap (\t -> k t (iterNValue k f))


-- | @iter@ for monadic values
iterNValueM
  :: (MonadDataContext f m, Monad n)
  => (forall x . n x -> m x)
  -> (t -> (NValue t f m -> n r) -> n r)
  -> (NValue' t f m (n r) -> n r)
  -> NValue t f m
  -> n r
iterNValueM transform k f =
    iterM f <=< go . fmap (\t -> k t (iterNValueM transform k f))
  where
    go (Pure x) = Pure <$> x
    go (Free fa) = Free <$> bindNValue' transform go fa


-- | @hoistFree@, Back & forth hoisting in the monad stack
hoistNValue
  :: (Functor m, Functor n, Functor f)
  => (forall x . n x -> m x)
  -> (forall x . m x -> n x)
  -> NValue t f m
  -> NValue t f n
hoistNValue run lft = hoistFree (hoistNValue' run lft)


-- ** MonadTrans

-- | @lift@
liftNValue
  :: (MonadTrans u, Monad m, Functor (u m), Functor f)
  => (forall x . u m x -> m x)
  -> NValue t f m
  -> NValue t f (u m)
liftNValue run = hoistNValue run lift


-- *** MonadTransUnlift
-- | @unlift@
unliftNValue
  :: (MonadTrans u, Monad m, Functor (u m), Functor f)
  => (forall x . u m x -> m x)  -- aka "run"
  -> NValue t f (u m)
  -> NValue t f m
unliftNValue = hoistNValue lift


-- ** Methods @F: Hask → NValue@
--
-- $Methods @F: Hask → NValue@
--
-- The morphisms of the functor @Hask → NValue@.
-- Continuation of the mantra: "Nix.Value#mantra"


-- | Life of a Haskell thunk to the life of a Nix thunk,
nvThunk :: Applicative f
  => t
  -> NValue t f m
nvThunk = Pure


-- | Life of a Haskell constant to the life of a Nix constant,
nvConstant :: Applicative f
  => NAtom
  -> NValue t f m
nvConstant = Free . nvConstant'


-- | Life of a Haskell sting & context to the life of a Nix string & context,
nvStr :: Applicative f
  => NixString
  -> NValue t f m
nvStr = Free . nvStr'


-- | Life of a Haskell FilePath to the life of a Nix path
nvPath :: Applicative f
  => FilePath
  -> NValue t f m
nvPath = Free . nvPath'


nvList :: Applicative f
  => [NValue t f m]
  -> NValue t f m
nvList = Free . nvList'


nvSet :: Applicative f
  => HashMap Text (NValue t f m)
  -> HashMap Text SourcePos
  -> NValue t f m
nvSet s x = Free $ nvSet' s x


nvClosure :: (Applicative f, Functor m)
  => Params ()
  -> (NValue t f m
      -> m (NValue t f m)
    )
  -> NValue t f m
nvClosure x f = Free $ nvClosure' x f


nvBuiltin :: (Applicative f, Functor m)
  => String
  -> (NValue t f m
    -> m (NValue t f m)
    )
  -> NValue t f m
nvBuiltin name f = Free $ nvBuiltin' name f


builtin
  :: forall m f t
   . (MonadThunk t m (NValue t f m), MonadDataContext f m)
  => String
  -> (NValue t f m
      -> m (NValue t f m)
    )
  -> m (NValue t f m)
builtin name f = pure $ nvBuiltin name $ \a -> f a


builtin2
  :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
  => String
  -> (NValue t f m -> NValue t f m
      -> m (NValue t f m)
    )
  -> m (NValue t f m)
builtin2 name f = builtin name $ \a -> builtin name $ \b -> f a b


builtin3
  :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
  => String
  -> (  NValue t f m
     -> NValue t f m
     -> NValue t f m
     -> m (NValue t f m)
     )
  -> m (NValue t f m)
builtin3 name f =
  builtin name $ \a -> builtin name $ \b -> builtin name $ \c -> f a b c

-- *** @F: Evaluation -> NValue@

pattern NVThunk t <- Pure t
pattern NVConstant x <- Free (NVConstant' x)
pattern NVStr ns <- Free (NVStr' ns)
pattern NVPath x <- Free (NVPath' x)
pattern NVList l <- Free (NVList' l)
pattern NVSet s x <- Free (NVSet' s x)
pattern NVClosure x f <- Free (NVClosure' x f)
pattern NVBuiltin name f <- Free (NVBuiltin' name f)


-- * @TStringContext@

data TStringContext = NoContext | HasContext
  deriving Show

-- * @ValueType@

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


-- | Determine type of a value
valueType :: NValueF a m r -> ValueType
valueType = \case
  NVConstantF a -> case a of
    NURI   _ -> TString NoContext
    NInt   _ -> TInt
    NFloat _ -> TFloat
    NBool  _ -> TBool
    NNull    -> TNull
  NVStrF ns  ->
    TString $
      bool
        NoContext
        HasContext
        $ stringHasContext ns
  NVListF{}    -> TList
  NVSetF{}     -> TSet
  NVClosureF{} -> TClosure
  NVPathF{}    -> TPath
  NVBuiltinF{} -> TBuiltin


-- | Describe type value
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


showValueType :: (MonadThunk t m (NValue t f m), Comonad f)
  => NValue t f m
  -> m String
showValueType (Pure t) = force t showValueType
showValueType (Free (NValue (extract -> v))) =
  pure $ describeValue $ valueType v


-- * @ValueFrame@

data ValueFrame t f m
    = ForcingThunk t
    | ConcerningValue (NValue t f m)
    | Comparison (NValue t f m) (NValue t f m)
    | Addition (NValue t f m) (NValue t f m)
    | Multiplication (NValue t f m) (NValue t f m)
    | Division (NValue t f m) (NValue t f m)
    | Coercion ValueType ValueType
    | CoercionToJson (NValue t f m)
    | CoercionFromJson Aeson.Value
    | Expectation ValueType (NValue t f m)
    deriving Typeable

deriving instance (Comonad f, Show t) => Show (ValueFrame t f m)


-- * @MonadDataContext@

type MonadDataContext f (m :: * -> *)
  = (Comonad f, Applicative f, Traversable f, Monad m)


-- * @MonadDataErrorContext@

type MonadDataErrorContext t f m
  = (Show t, Typeable t, Typeable m, Typeable f, MonadDataContext f m)

instance MonadDataErrorContext t f m => Exception (ValueFrame t f m)


-- ** NValue' traversals, getter & setters


-- | Make traversals for Nix traversable structures.
$(makeTraversals ''NValueF)

-- | Make lenses for the Nix values
$(makeLenses ''NValue')

-- | Lens-generated getter-setter function for a traversable NValue' key-val structures.
--   Nix value analogue of the @Data-Aeson-Lens@:@key :: AsValue t => Text -> Traversal' t Value@.
key
  :: (Traversable f, Applicative g)
  => VarName
  -> LensLike' g (NValue' t f m a) (Maybe a)
key k = nValue . traverse . _NVSetF . _1 . hashAt k
