{-# language CPP #-}
{-# language DeriveAnyClass #-}
{-# language KindSignatures #-}
{-# language ConstraintKinds #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}

{-# options_ghc -Wno-missing-signatures #-}
{-# options_ghc -Wno-missing-pattern-synonym-signatures #-}

-- | The core of the type system, Nix language values
module Nix.Value
where

import           Nix.Prelude
import           Control.Comonad                ( Comonad
                                                , extract
                                                )
import           Control.Monad.Free             ( Free(..)
                                                , hoistFree
                                                , iter
                                                , iterM
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Functor.Classes           ( Show1
                                                , liftShowsPrec
                                                , showsUnaryWith
                                                , Eq1(liftEq) )
import           Data.Eq.Deriving
import qualified Text.Show
import           Text.Show                      ( showsPrec
                                                , showString
                                                , showParen
                                                )
import           Lens.Family2.Stock             ( _2 )
import           Lens.Family2.TH                ( makeTraversals
                                                , makeLenses
                                                )
import           Nix.Atoms
import           Nix.Expr.Types
import           Nix.String
import           Nix.Thunk


-- * @__NValueF__@: Base functor (F)

-- | An NValueF p m r represents all the possible types of Nix values.
--
--   Is is the base functor to form the Free monad of nix expressions.
--   The parameter `r` represents Nix values in their final form (NValue).
--   The parameter `p` represents exactly the same type, but is kept separate
--   or it would prevent NValueF from being a proper functor.
--   It is intended to be hard-coded to the same final type as r.
--   `m` is the monad in which evaluations will run.

-- | An NValue' t f m a is a magic layer between NValueF and the Free monad construction.
--
--   It fixes the `p` parameter of NValueF to the final NValue type, making the
--   definition of NValue' and NValue depend on each other in a recursive
--   fashion.
--
--   It also introduces a `f` parameter for a custom functor that can be used
--   to wrap each intermediate value in the reduced expression tree.
--   This is where expression evaluations can store annotations and other
--   useful information.
--
--   `t` is not really used here, but is needed to type the (NValue t f m)
--   used to tie the knot of the `p` parameter in the inner NValueF.
--
--   `a` is will be an `NValue t f m` when NValue' functor is turned into a
--   Free monad.

-- | 'NValue t f m' is the most reduced form of a 'NExpr' after evaluation is
--   completed. It is a layer cake of NValueF base values, wrapped in the f
--   functor and into the Free recursive construction.
--
--   Concretely, an NValue t f m can either be a thunk, representing a value
--   yet to be evaluated (Pure t), or a know value in WHNF
--   (Free (NValue' t f m (NValue t f m))) = (Free (f (NValueF NValue m NValue))
--   That is, a base value type, wrapped into the generic `f`
--   functor, and based on other NValue's, which can in turn be either thunks,
--   or more already WHNF evaluated values.
--
--   As an example, the value `[1]` will be represented as
--
--   Free (f (NVListF [
--      (Free (f (NVConstantF (NInt 1))))
--   ]))
--
--   Should this 1 be a laziy and yet unevaluated value, it would be represented as
--
--   Free (f (NVListF [ (Pure t) ]))
--
--   Where the t is evaluator dependant, and should contain anough information
--   to be evaluated to an NValue when needed. `demand` of `force` are used to
--   turn a potential thunk into a `m (NValue t f m)`.
--
--   Of course, trees can be much bigger.
--
--   The number of layers and type aliases for similar things is huge, so
--   this module provides ViewPatterns for each NValueF constructor.
--
--   For example, the pattern NVStr' ns matches a NValue' containing an NVStrF,
--   and bind that NVStrF to ns, ignoring the f functor inside.
--   Similarly, the pattern NVStr ns (without prime mark) will match the inner
--   NVstrF value inside an NValue. Of course, the patterns are declined for
--   all the NValueF constructors. The non primed version also has an NVThunk t
--   pattern to account for the possibility of an NValue to no be fully
--   evaluated yet, as opposed to an NValue'.

data NValueF p m r
    = NVConstantF NAtom
     -- | A string has a value and a context, which can be used to record what a
     -- string has been build from
    | NVStrF NixString
    | NVPathF Path
    | NVListF [r]
    | NVSetF PositionSet (AttrSet r)
      -- ^
      --   Quite frequently actions/processing happens with values
      --   (for example - forcing of values & recreation of the monad),
      --   but @SourcePos@ does not change then.
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
    | NVBuiltinF VarName (p -> m r)
      -- ^ A builtin function is itself already in normal form. Also, it may
      --   or may not choose to evaluate its argument in the production of a
      --   result.
  deriving (Generic, Typeable, Functor)


-- ** Eq1

instance Eq1 (NValueF p m) where
  liftEq _  (NVConstantF x) (NVConstantF y) = x == y
  liftEq _  (NVStrF      x) (NVStrF      y) = x == y
  liftEq _  (NVPathF     x) (NVPathF     y) = x == y
  liftEq eq (NVListF     x) (NVListF     y) = liftEq eq x y
  liftEq eq (NVSetF  _   x) (NVSetF _    y) = liftEq eq x y
  liftEq _  _               _               = False


-- ** Show

instance Show r => Show (NValueF p m r) where
  showsPrec d =
    \case
      (NVConstantF atom     ) -> showsCon1 "NVConstant" atom
      (NVStrF      ns       ) -> showsCon1 "NVStr"      $ ignoreContext ns
      (NVListF     lst      ) -> showsCon1 "NVList"     lst
      (NVSetF      _   attrs) -> showsCon1 "NVSet"      attrs
      (NVClosureF  params _ ) -> showsCon1 "NVClosure"  params
      (NVPathF     path     ) -> showsCon1 "NVPath"     path
      (NVBuiltinF  name   _ ) -> showsCon1 "NVBuiltin"  name
   where
    showsCon1 :: Show a => String -> a -> String -> String
    showsCon1 con a =
      showParen (d > 10) $ showString (con <> " ") . showsPrec 11 a


-- ** Foldable

-- | Folds what the value is known to contain at time of fold.
instance Foldable (NValueF p m) where
  foldMap f = \case
    NVConstantF _  -> mempty
    NVStrF      _  -> mempty
    NVPathF     _  -> mempty
    NVClosureF _ _ -> mempty
    NVBuiltinF _ _ -> mempty
    NVListF     l  -> foldMap f l
    NVSetF     _ s -> foldMap f s


-- ** Traversable

-- | @sequence@
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
  NVSetF     p s -> NVSetF p <$> sequenceA s
  NVClosureF p g -> pure $ NVClosureF p (transform <=< g)
  NVBuiltinF s g -> pure $ NVBuiltinF s (transform <=< g)


-- ** Monad

-- | @bind@
bindNValueF
  :: (Monad m, Monad n)
  => (forall x . n x -> m x) -- ^ Transform @n@ into @m@.
  -> (a -> n b) -- ^ A Kleisli arrow (see 'Control.Arrow.Kleisli' & Kleisli catagory).
  -> NValueF p m a -- ^ "Unfixed" (openly recursive) value of an embedded Nix language.
  -> n (NValueF p m b) -- ^ An implementation of @transform (f =<< x)@ for embedded Nix language values.
bindNValueF transform f = \case
  NVConstantF a  -> pure $ NVConstantF a
  NVStrF      s  -> pure $ NVStrF s
  NVPathF     p  -> pure $ NVPathF p
  NVListF     l  -> NVListF <$> traverse f l
  NVSetF     p s -> NVSetF p <$> traverse f s
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
hoistNValueF lft =
  \case
    -- Pass-through the:
    --   [ NVConstantF a
    --   , NVStrF s
    --   , NVPathF p
    --   , NVListF l
    --   , NVSetF p s
    --   ]
    NVConstantF a  -> NVConstantF a
    NVStrF      s  -> NVStrF s
    NVPathF     p  -> NVPathF p
    NVListF     l  -> NVListF l
    NVSetF     p s -> NVSetF p s
    NVBuiltinF s g -> NVBuiltinF s (lft . g)
    NVClosureF p g -> NVClosureF p (lft . g)
{-# inline hoistNValueF #-}

-- * @__NValue'__@: forming the (F(A))

-- | NVConstraint constraint the f layer in @NValue'@.
-- It makes bijection between sub category of Hask and Nix Value possible.
-- 'Comonad' enable Nix Value to Hask part.
-- 'Applicative' enable Hask to Nix Value part.
type NVConstraint f = (Comonad f, Applicative f)

-- | At the time of constructor, the expected arguments to closures are values
--   that may contain thunks. The type of such thunks are fixed at that time.
newtype NValue' t f m a =
  NValue'
    {
    -- | Applying F-algebra Base functor data type (@NValueF@) to the F-algebra carrier (@NValue@), forming the \( F(A)-> A \)).
    _nValue :: f (NValueF (NValue t f m) m a)
    }
  deriving (Generic, Typeable, Functor, Foldable)

instance (NVConstraint f, Show a) => Show (NValue' t f m a) where
  show (NValue' (extract -> v)) = show v


-- ** Show1

instance NVConstraint f  => Show1 (NValue' t f m) where
  liftShowsPrec sp sl p = \case
    NVConstant' atom  -> showsUnaryWith showsPrec             "NVConstantF" p atom
    NVStr' ns         -> showsUnaryWith showsPrec             "NVStrF"      p $ ignoreContext ns
    NVList' lst       -> showsUnaryWith (liftShowsPrec sp sl) "NVListF"     p lst
    NVSet'  _   attrs -> showsUnaryWith (liftShowsPrec sp sl) "NVSetF"      p attrs
    NVPath' path      -> showsUnaryWith showsPrec             "NVPathF"     p path
    NVClosure' c    _ -> showsUnaryWith showsPrec             "NVClosureF"  p c
    NVBuiltin' name _ -> showsUnaryWith showsPrec             "NVBuiltinF"  p name


-- ** Traversable

-- | @sequence@
sequenceNValue'
  :: (Functor n, Traversable f, Monad m, Applicative n)
  => (forall x . n x -> m x)
  -> NValue' t f m (n a)
  -> n (NValue' t f m a)
sequenceNValue' transform (NValue' v) =
  NValue' <$> traverse (sequenceNValueF transform) v


-- ** Profunctor

-- | @lmap@
lmapNValueF :: Functor m => (b -> a) -> NValueF a m r -> NValueF b m r
lmapNValueF f = \case
  NVConstantF a  -> NVConstantF a
  NVStrF      s  -> NVStrF s
  NVPathF     p  -> NVPathF p
  NVListF     l  -> NVListF l
  NVSetF     p s -> NVSetF p s
  NVClosureF p g -> NVClosureF p (g . f)
  NVBuiltinF s g -> NVBuiltinF s (g . f)


-- ** Free

-- | @iter@
iterNValue'
  :: forall t f m a r
   . MonadDataContext f m
  => ((NValue' t f m a -> r) -> a -> r)
  -> (NValue' t f m r -> r)
  -> NValue' t f m a
  -> r
iterNValue' k f = fix ((f .) . fmap . k)

-- *** Utils

-- | @hoistFree@: Back & forth hoisting in the monad stack
hoistNValue'
  :: (Functor m, Functor n, Functor f)
  => (forall x . n x -> m x)
  -> (forall x . m x -> n x)
  -> NValue' t f m a
  -> NValue' t f n a
hoistNValue' run lft (NValue' v) =
  NValue' $ lmapNValueF (hoistNValue lft run) . hoistNValueF lft <$> v
{-# inline hoistNValue' #-}

-- ** Monad

-- |@bind@
bindNValue'
  :: (Traversable f, Monad m, Monad n)
  => (forall x . n x -> m x)
  -> (a -> n b)
  -> NValue' t f m a
  -> n (NValue' t f m b)
bindNValue' transform f (NValue' v) =
  NValue' <$> traverse (bindNValueF transform f) v

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
-- *** @F: Hask subcategory <-> NValue'@
-- #mantra#
-- $Patterns @F: Hask <-> NValue'@
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
-- Module pattens use @language PatternSynonyms@: bidirectional synonyms (@<-@),
-- and @ViewPatterns@: (@->@) at the same time.
-- @ViewPatterns Control.Comonad.extract@ extracts
-- from the @NValue (Free (NValueF a))@
-- the @NValueF a@. Which is @NValueF p m r@. Since it extracted from the
-- @NValue@, which is formed by \( (F a -> a) F a \) in the first place.
-- So @NValueF p m r@ which is extracted here, internally holds the next NValue.
--
-- Facts of bijection between Hask subcategory objects and Nix objects,
-- and between Hask subcategory morphisms and Nix morphisms are seen blow:


-- | Using of Nulls is generally discouraged (in programming language design et al.), but, if you need it.
pattern NVNull' :: NVConstraint w => NValue' t w m a
pattern NVNull' = NVConstant' NNull

-- | Haskell constant to the Nix constant,
pattern NVConstant' :: NVConstraint w => NAtom -> NValue' t w m a
pattern NVConstant' x <- NValue' (extract -> NVConstantF x)
  where NVConstant' = NValue' . pure . NVConstantF

-- | Haskell text & context to the Nix text & context,
pattern NVStr' :: NVConstraint w => NixString -> NValue' t w m a
pattern NVStr' ns <- NValue' (extract -> NVStrF ns)
  where NVStr' = NValue' . pure . NVStrF

-- | Haskell @Path@ to the Nix path,
pattern NVPath' :: NVConstraint w => Path -> NValue' t w m a
pattern NVPath' x <- NValue' (extract -> NVPathF x)
  where NVPath' = NValue' . pure . NVPathF . coerce

-- | Haskell @[]@ to the Nix @[]@,
pattern NVList' :: NVConstraint w => [a] -> NValue' t w m a
pattern NVList' l <- NValue' (extract -> NVListF l)
  where NVList' = NValue' . pure . NVListF

-- | Haskell key-value to the Nix key-value,
pattern NVSet' :: NVConstraint w => PositionSet -> AttrSet a -> NValue' t w m a
pattern NVSet' p s <- NValue' (extract -> NVSetF p s)
  where NVSet' p s = NValue' $ pure $ NVSetF p s

-- | Haskell closure to the Nix closure,
pattern NVClosure' :: NVConstraint w => Params () -> (NValue t w m -> m a) -> NValue' t w m a
pattern NVClosure' x f <- NValue' (extract -> NVClosureF x f)
  where NVClosure' x f = NValue' $ pure $ NVClosureF x f

-- | Haskell functions to the Nix functions!
pattern NVBuiltin' :: NVConstraint w => VarName -> (NValue t w m -> m a) -> NValue' t w m a
pattern NVBuiltin' name f <- NValue' (extract -> NVBuiltinF name f)
  where NVBuiltin' name f = NValue' $ pure $ NVBuiltinF name f
{-# complete NVConstant', NVStr', NVPath', NVList', NVSet', NVClosure', NVBuiltin' #-}


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

-- | HOF of @iter@ from @Free@
iterNValue
  :: forall t f m r
   . MonadDataContext f m
  => ((NValue t f m -> r) -> t -> r)
  -> (NValue' t f m r -> r)
  -> NValue t f m
  -> r
iterNValue k f = fix ((iter f .) . fmap . k) -- already almost iterNValue'

iterNValueByDiscardWith
  :: MonadDataContext f m
  => r
  -> (NValue' t f m r -> r)
  -> NValue t f m
  -> r
iterNValueByDiscardWith = iterNValue . const . const


-- | HOF of @iterM@ from @Free@
iterNValueM
  :: (MonadDataContext f m, Monad n)
  => (forall x . n x -> m x)
  -> ((NValue t f m -> n r) -> t -> n r)
  -> (NValue' t f m (n r) -> n r)
  -> NValue t f m
  -> n r
iterNValueM transform k f = fix (((iterM f <=< go) .) . fmap . k)
  where
    go (Pure x) = Pure <$> x -- It should be a 'sequenceA' if to remote 'transform' form function.
    go (Free fa) = Free <$> bindNValue' transform go fa

-- *** Utils

-- | @hoistFree@, Back & forth hoisting in the monad stack
hoistNValue
  :: (Functor m, Functor n, Functor f)
  => (forall x . n x -> m x)
  -> (forall x . m x -> n x)
  -> NValue t f m
  -> NValue t f n
hoistNValue run lft = hoistFree $ hoistNValue' run lft
{-# inline hoistNValue #-}

-- ** MonadTrans

-- | @lift@
liftNValue
  :: (MonadTrans u, Monad m, Functor (u m), Functor f)
  => (forall x . u m x -> m x)
  -> NValue t f m
  -> NValue t f (u m)
liftNValue f = hoistNValue f lift


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

-- | Using of Nulls is generally discouraged (in programming language design et al.), but, if you need it.

mkNVStrWithoutContext :: NVConstraint f
  => Text
  -> NValue t f m
mkNVStrWithoutContext = NVStr . mkNixStringWithoutContext


builtin
  :: forall m f t
   . (MonadThunk t m (NValue t f m), MonadDataContext f m)
  => VarName -- ^ function name
  -> ( NValue t f m
    -> m (NValue t f m)
    ) -- ^ unary function
  -> m (NValue t f m)
builtin = (pure .) . NVBuiltin


builtin2
  :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
  => VarName -- ^ function name
  -> ( NValue t f m
    -> NValue t f m
    -> m (NValue t f m)
    ) -- ^ binary function
  -> m (NValue t f m)
builtin2 = ((.) <*> (.)) . builtin


builtin3
  :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
  => VarName -- ^ function name
  -> ( NValue t f m
    -> NValue t f m
    -> NValue t f m
    -> m (NValue t f m)
    ) -- ^ ternary function
  -> m (NValue t f m)
builtin3 =
  liftA2 (.) -- compose 2 together
    builtin
    ((.) . builtin2)

-- *** @F: Evaluation -> NValue@

pattern NVNull = Free NVNull'
pattern NVThunk t = Pure t
pattern NVValue v = Free v
{-# complete NVThunk, NVValue, NVNull #-}
pattern NVConstant x = Free (NVConstant' x)
pattern NVStr ns = Free (NVStr' ns)
pattern NVPath x = Free (NVPath' x)
pattern NVList l = Free (NVList' l)
pattern NVSet s x = Free (NVSet' s x)
pattern NVBuiltin name f = Free (NVBuiltin' name f)
pattern NVClosure x f = Free (NVClosure' x f)
{-# complete NVThunk, NVConstant, NVStr, NVPath, NVList, NVSet, NVClosure, NVBuiltin #-}




-- * @TStringContext@

data TStringContext = NoContext | HasContext
 deriving Show

instance Semigroup TStringContext where
  (<>) NoContext NoContext = NoContext
  (<>) _         _         = HasContext


instance Monoid TStringContext where
  mempty = NoContext

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
valueType =
  \case
    NVConstantF a ->
      case a of
        NURI   _ -> TString mempty
        NInt   _ -> TInt
        NFloat _ -> TFloat
        NBool  _ -> TBool
        NNull    -> TNull
    NVStrF ns  ->
      TString $
        HasContext `whenTrue` hasContext ns
    NVListF{}    -> TList
    NVSetF{}     -> TSet
    NVClosureF{} -> TClosure
    NVPathF{}    -> TPath
    NVBuiltinF{} -> TBuiltin


-- | Describe type value
describeValue :: ValueType -> Text
describeValue =
  \case
    TInt               -> "an integer"
    TFloat             -> "a float"
    TBool              -> "a boolean"
    TNull              -> "a null"
    TString NoContext  -> "a string with no context"
    TString HasContext -> "a string"
    TList              -> "a list"
    TSet               -> "an attr set"
    TClosure           -> "a function"
    TPath              -> "a path"
    TBuiltin           -> "a builtin function"


showValueType :: (MonadThunk t m (NValue t f m), Comonad f)
  => NValue t f m
  -> m Text
showValueType (Pure t) = showValueType =<< force t
showValueType (Free (NValue' (extract -> v))) =
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

deriving instance (NVConstraint f, Show t) => Show (ValueFrame t f m)


-- * @MonadDataContext@

type MonadDataContext f (m :: Type -> Type)
  = (Comonad f, Applicative f, Traversable f, Monad m)

-- * @MonadDataErrorContext@

type MonadDataErrorContext t f m
  = (Show t, Typeable t, Typeable m, Typeable f, MonadDataContext f m, MonadFail m)

instance MonadDataErrorContext t f m => Exception (ValueFrame t f m)

-- * @instance Eq1 NValue'@

-- TH derivable works only after MonadDataContext
$(deriveEq1 ''NValue')


-- * @NValueF@ traversals, getter & setters

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
key k = nValue . traverse . _NVSetF . _2 . hashAt k
