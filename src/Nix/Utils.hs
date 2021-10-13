{-# language NoImplicitPrelude #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}

-- | This is a module of custom "Prelude" code.
-- It is for import for projects other then @HNix@.
-- For @HNix@ - this module gets reexported by "Prelude", so for @HNix@ please fix-up pass-through there.
module Nix.Utils
  ( KeyMap
  , TransformF
  , Transform
  , Alg
  , Path(..)
  , takeFileName
  , takeDirectory
  , isAbsolute
  , splitDirectories
  , joinPath
  , (</>)
  , replaceExtension
  , takeExtension
  , takeExtensions
  , Has(..)
  , trace
  , traceM
  , stub
  , pass
  , whenTrue
  , whenFalse
  , list
  , whenText
  , free
  , whenJust
  , dup
  , mapPair
  , both
  , readFile
  , traverseM
  , lifted
  , loebM
  , adi
  , module X
  )
 where

import           Relude                  hiding ( pass
                                                , force
                                                , readFile
                                                , whenJust
                                                , whenNothing
                                                , trace
                                                , traceM
                                                )

import           Data.Binary                    ( Binary )
import           Data.Data                      ( Data )
import           Codec.Serialise                ( Serialise )
import           Control.Monad.Fix              ( MonadFix(..) )
import           Control.Monad.Free             ( Free(..) )
import           Control.Monad.Trans.Control    ( MonadTransControl(..) )
import qualified Data.Aeson                    as A
import           Data.Fix                       ( Fix(..) )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                 as Text
import           Lens.Family2                  as X
                                                ( view
                                                , over
                                                , LensLike'
                                                , Lens'
                                                )
import           Lens.Family2.Stock             ( _1
                                                , _2
                                                )
import qualified System.FilePath              as FilePath

#if ENABLE_TRACING
import qualified Relude.Debug                 as X
#else
-- Well, since it is currently CPP intermingled with Debug.Trace, required to use String here.
trace :: String -> a -> a
trace = const id
{-# inline trace #-}
traceM :: Monad m => String -> m ()
traceM = const stub
{-# inline traceM #-}
#endif

-- | To have explicit type boundary between FilePath & String.
newtype Path = Path FilePath
  deriving
    ( Eq, Ord, Generic
    , Typeable, Data, NFData, Serialise, Binary, A.ToJSON, A.FromJSON
    , Show, Read, Hashable
    , Semigroup, Monoid
    )

instance ToText Path where
  toText = toText @String . coerce

instance IsString Path where
  fromString = coerce

takeFileName :: FilePath -> FilePath
takeFileName = FilePath.takeFileName

takeExtension :: FilePath -> String
takeExtension = FilePath.takeExtensions

takeExtensions :: FilePath -> String
takeExtensions = FilePath.takeExtensions

isAbsolute :: FilePath -> Bool
isAbsolute = FilePath.isAbsolute

takeDirectory :: FilePath -> FilePath
takeDirectory = FilePath.takeDirectory

(</>) :: FilePath -> FilePath -> FilePath
(</>) = (FilePath.</>)
infixr 5 </>

splitDirectories :: FilePath -> [FilePath]
splitDirectories = FilePath.splitDirectories

joinPath :: [FilePath] -> FilePath
joinPath = FilePath.joinPath

replaceExtension :: FilePath -> String -> FilePath
replaceExtension = FilePath.replaceExtension

-- | > Hashmap Text -- type synonym
type KeyMap = HashMap Text

-- | F-algebra defines how to reduce the fixed-point of a functor to a value.
-- > type Alg f a = f a -> a
type Alg f a = f a -> a

-- | Do according transformation.
--
-- It is a transformation of a recursion scheme.
-- See @TransformF@.
type Transform f a = TransformF (Fix f) a
-- | Do according transformation.
--
-- It is a transformation between functors.
-- ...
-- You got me, it is a natural transformation.
type TransformF f a = (f -> a) -> f -> a

class Has a b where
  hasLens :: Lens' a b

instance Has a a where
  hasLens f = f

instance Has (a, b) a where
  hasLens = _1

instance Has (a, b) b where
  hasLens = _2

loebM :: (MonadFix m, Traversable t) => t (t a -> m a) -> m (t a)
-- Sectioning here insures optimization happening.
loebM f = mfix $ \a -> (`traverse` f) ($ a)
{-# inline loebM #-}

--  2021-08-21: NOTE: Someone needs to put in normal words, what this does.
-- This function is pretty spefic & used only once, in "Nix.Normal".
lifted
  :: (MonadTransControl u, Monad (u m), Monad m)
  => ((a -> m (StT u b)) -> m (StT u b))
  -> (a -> u m b)
  -> u m b
lifted f k =
  do
    lftd <- liftWith (\run -> f (run . k))
    restoreT $ pure lftd

-- | adi is Abstracting Definitional Interpreters:
--
--     https://arxiv.org/abs/1707.04755
--
--   All ADI does is interleaves every layer of evaluation by inserting intermitten layers between them, in that way the evaluation can be extended/embelished in any way wanted. Look at its use to see great examples.
--
--   Essentially, it does for evaluation what recursion schemes do for
--   representation: allows threading layers through existing structure, only
--   in this case through behavior.
adi
  :: Functor f
  => Transform f a
  -> Alg f a
  -> Fix f
  -> a
adi g f = g $ f . (adi g f <$>) . unFix


-- | Analog for @bool@ or @maybe@, for list-like cons structures.
list
  :: Foldable t
  => b -> (t a -> b) -> t a -> b
list e f l =
  bool
    (f l)
    e
    (null l)
{-# inline list #-}

whenText
  :: a -> (Text -> a) -> Text -> a
whenText e f t =
  bool
    (f t)
    e
    (Text.null t)

-- | Lambda analog of @maybe@ or @either@ for Free monad.
free :: (a -> b) -> (f (Free f a) -> b) -> Free f a -> b
free fP fF fr =
  case fr of
    Pure a -> fP a
    Free fa -> fF fa
{-# inline free #-}


whenTrue :: (Monoid a)
  => a -> Bool -> a
whenTrue =
  bool
    mempty
{-# inline whenTrue #-}

whenFalse :: (Monoid a)
  => a  -> Bool  -> a
whenFalse f =
  bool
    f
    mempty
{-# inline whenFalse #-}

whenJust
  :: Monoid b
  => (a -> b)
  -> Maybe a
  -> b
whenJust =
  maybe
    mempty
{-# inline whenJust #-}


-- | Apply a single function to both components of a pair.
--
-- > both succ (1,2) == (2,3)
--
-- Taken From package @extra@
both :: (a -> b) -> (a, a) -> (b, b)
both f (x,y) = (f x, f y)
{-# inline both #-}


-- | Duplicates object into a tuple.
dup :: a -> (a, a)
dup x = (x, x)
{-# inline dup #-}

-- | From @utility-ht@ for tuple laziness.
mapPair :: (a -> c, b -> d) -> (a,b) -> (c,d)
mapPair ~(f,g) ~(a,b) = (f a, g b)
{-# inline mapPair #-}

-- After migration from the @relude@ - @relude: pass -> stub@
-- | @pure mempty@: Short-curcuit, stub.
stub :: (Applicative f, Monoid a) => f a
stub = pure mempty
{-# inline stub #-}

-- | Alias for @stub@, since @Relude@ has more specialized @pure ()@.
pass :: (Applicative f) => f ()
pass = stub
{-# inline pass #-}

readFile :: Path -> IO Text
readFile = Text.readFile . coerce

traverseM
  :: ( Applicative m
     , Applicative f
     , Traversable t
     )
  => ( a
     -> m (f b)
     )
  -> t a
  -> m (f (t b))
traverseM f x = sequenceA <$> traverse f x
