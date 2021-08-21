{-# language ConstraintKinds #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-unused-do-bind #-}


module Nix.Thunk.Basic
  ( NThunkF(..)
  , Deferred(..)
  , deferred
  , MonadBasicThunk
  ) where

import           Control.Monad.Ref              ( MonadRef(Ref, newRef, readRef, writeRef)
                                                , MonadAtomicRef(atomicModifyRef)
                                                )
import           Control.Monad.Catch            ( MonadCatch(..)
                                                , MonadThrow(throwM)
                                                )
import qualified Text.Show
import           Nix.Thunk


-- * Data type @Deferred@

-- | Data is computed OR in a lazy thunk state which
-- is still not evaluated.
data Deferred m v = Computed v | Deferred (m v)
  deriving (Functor, Foldable, Traversable)

-- ** Utils

-- | Apply second if @Deferred@, otherwise (@Computed@) - apply first.
-- Analog of @either@ for @Deferred = Computed|Deferred@.
deferred :: (v -> b) -> (m v -> b) -> Deferred m v -> b
deferred f1 f2 =
  \case
    Computed v -> f1 v
    Deferred action -> f2 action
{-# inline deferred #-}


-- * Thunk references & lock handling

-- | Thunk resource reference (@ref-tf: Ref m@), and as such also also hold
-- a @Bool@ lock flag.
type ThunkRef m = Ref m Bool

-- | Reference (@ref-tf: Ref m v@) to a value that the thunk holds.
type ThunkValueRef m v = Ref m (Deferred m v)

-- | @ref-tf@ lock instruction for @Ref m@ (@ThunkRef@).
lockVal :: Bool -> (Bool, Bool)
lockVal = (True, )

-- | @ref-tf@ unlock instruction for @Ref m@ (@ThunkRef@).
unlockVal :: Bool -> (Bool, Bool)
unlockVal = (False, )

-- | Takes @ref-tf: Ref m@ reference, returns Bool result of the operation.
lock
  :: ( MonadBasicThunk m
    , MonadCatch m
    )
  => ThunkRef m
  -> m Bool
lock r = atomicModifyRef r lockVal

-- | Takes @ref-tf: Ref m@ reference, returns Bool result of the operation.
unlock
  :: ( MonadBasicThunk m
    , MonadCatch m
    )
  => ThunkRef m
  -> m Bool
unlock r = atomicModifyRef r unlockVal


-- * Data type for thunks: @NThunkF@

-- | The type of very basic thunks
data NThunkF m v =
  Thunk (ThunkId m) (ThunkRef m) (ThunkValueRef m v)

instance (Eq v, Eq (ThunkId m)) => Eq (NThunkF m v) where
  Thunk x _ _ == Thunk y _ _ = x == y

instance Show (NThunkF m v) where
  show Thunk{} = toString thunkStubText

type MonadBasicThunk m = (MonadThunkId m, MonadAtomicRef m)


-- ** @instance MonadThunk NThunkF@

instance (MonadBasicThunk m, MonadCatch m)
  => MonadThunk (NThunkF m v) m v where

  thunkId :: NThunkF m v -> ThunkId m
  thunkId (Thunk n _ _) = n

  thunk :: m v -> m (NThunkF m v)
  thunk action =
    do
      freshThunkId <- freshId
      liftA2 (Thunk freshThunkId)
        (newRef   False          )
        (newRef $ Deferred action)

  query :: m v -> NThunkF m v -> m v
  query vStub (Thunk _ _ lTValRef) =
    do
      v <- readRef lTValRef
      deferred pure (const vStub) v

  force :: NThunkF m v -> m v
  force = forceMain

  forceEff :: NThunkF m v -> m v
  forceEff = forceMain

  further :: NThunkF m v -> m (NThunkF m v)
  further t@(Thunk _ _ ref) =
    do
      _ <-
        atomicModifyRef
          ref
          dup
      pure t


-- *** United body of `force*`

-- | Always returns computed @m v@.
--
-- Checks if resource is computed,
-- if not - with locking evaluates the resource.
forceMain
  :: ( MonadBasicThunk m
    , MonadCatch m
    )
  => NThunkF m v
  -> m v
forceMain (Thunk tIdV tRefV tValRefV) =
  do
    v <- readRef tValRefV
    deferred pure computeW v
 where
  computeW vDefferred =
    do
      locked <- lock tRefV
      bool
        lockFailedV
        (do
          v <- vDefferred `catch` bindFailedW
          writeRef tValRefV $ Computed v  -- Proclaim value computed
          unlockRef
          pure v
        )
        (not locked)

  lockFailedV = throwM $ ThunkLoop $ show tIdV

  bindFailedW (e :: SomeException) =
    do
      unlockRef
      throwM e

  unlockRef = unlock tRefV
{-# inline forceMain #-} -- it is big function, but internal, and look at its use.



-- ** Kleisli functor HOFs: @instance MonadThunkF NThunkF@

instance (MonadBasicThunk m, MonadCatch m)
  => MonadThunkF (NThunkF m v) m v where

  queryF
    :: (v -> m r)
    -> m r
    -> NThunkF m v
    -> m r
  queryF k n (Thunk _ thunkRef thunkValRef) =
    do
      locked <- lock thunkRef
      bool
        n
        go
        (not locked)
    where
      go =
        do
          eres <- readRef thunkValRef
          res  <-
            deferred
              k
              (const n)
              eres
          unlockRef
          pure res

      unlockRef = unlock thunkRef

  forceF
    :: (v -> m a)
    -> NThunkF m v
    -> m a
  forceF k = k <=< force

  forceEffF
    :: (v -> m r)
    -> NThunkF m v
    -> m r
  forceEffF k = k <=< forceEff

  furtherF
    :: (m v -> m v)
    -> NThunkF m v
    -> m (NThunkF m v)
  furtherF k t@(Thunk _ _ ref) =
    do
      _modifiedIt <- atomicModifyRef ref $
        \x ->
          deferred
            (const (x, x))
            (\ d -> (Deferred (k d), x))
            x
      pure t


