{-# LANGUAGE TypeFamilies #-}

-- | Abstraction over sequence types for Nix list operations.
--
-- This module provides the 'NixList' typeclass which abstracts over
-- different sequential container implementations (Vector, Seq, []).
-- This allows the Nix evaluator to use the most efficient representation
-- while keeping the code generic.
--
-- The default implementation uses 'Data.Vector' for O(1) length and indexing.
module Nix.List
  ( NixList(..)
  ) where

import           Nix.Prelude
import           Data.List                      ( (!?) )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import qualified Data.Sequence                 as Seq

-- | Typeclass abstracting over sequential container types for Nix lists.
--
-- This allows swapping the underlying representation (Vector, Seq, [])
-- without changing the code that operates on Nix lists.
--
-- __Complexity annotations__ document the expected performance for each
-- operation. Implementations should aim to meet these bounds.
class NixList f where
  -- | Get the length of the sequence.
  --
  -- Complexity: O(1) for Vector\/Seq, O(n) for []
  nlLength :: f a -> Int

  -- | Safe indexing by position.
  --
  -- Complexity: O(1) for Vector, O(log(min(i,n-i))) for Seq, O(i) for []
  nlIndex :: f a -> Int -> Maybe a

  -- | Check if the sequence is empty.
  --
  -- Complexity: O(1) for all
  nlNull :: f a -> Bool

  -- | Decompose into head and tail.
  --
  -- Complexity: O(1) for Vector (tail is a slice), O(1) for Seq, O(1) for []
  nlUncons :: f a -> Maybe (a, f a)

  -- | Prepend an element.
  --
  -- Complexity: O(n) for Vector, O(1) for Seq, O(1) for []
  nlCons :: a -> f a -> f a

  -- | Append an element.
  --
  -- Complexity: O(n) for Vector (amortized), O(1) for Seq, O(n) for []
  nlSnoc :: f a -> a -> f a

  -- | Concatenate two sequences.
  --
  -- Complexity: O(n+m) for Vector, O(log(min(n,m))) for Seq, O(n) for []
  nlAppend :: f a -> f a -> f a

  -- | Convert from a Haskell list.
  --
  -- Complexity: O(n) for all
  nlFromList :: [a] -> f a

  -- | Convert to a Haskell list.
  --
  -- Complexity: O(n) for all
  nlToList :: f a -> [a]

  -- | Map a function over elements.
  --
  -- Complexity: O(n) for all
  nlMap :: (a -> b) -> f a -> f b

  -- | Filter elements by a predicate.
  --
  -- Complexity: O(n) for all
  nlFilter :: (a -> Bool) -> f a -> f a

  -- | Traverse with an applicative effect.
  --
  -- Complexity: O(n) for all
  nlTraverse :: Applicative m => (a -> m b) -> f a -> m (f b)

  -- | Reverse the sequence.
  --
  -- Complexity: O(n) for all
  nlReverse :: f a -> f a

  -- | The empty sequence.
  nlEmpty :: f a

  -- | Fold from the left, strict in the accumulator.
  --
  -- Complexity: O(n) for all
  nlFoldl' :: (b -> a -> b) -> b -> f a -> b

  -- | Get the head, if non-empty.
  --
  -- Complexity: O(1) for all
  nlHead :: f a -> Maybe a
  nlHead = fmap fst . nlUncons
  {-# INLINE nlHead #-}

  -- | Get the tail, if non-empty.
  --
  -- Complexity: O(1) for Vector (slice), O(1) for Seq, O(1) for []
  nlTail :: f a -> Maybe (f a)
  nlTail = fmap snd . nlUncons
  {-# INLINE nlTail #-}

-- | Vector instance - optimal for O(1) length and random access.
instance NixList Vector where
  nlLength = V.length
  {-# INLINE nlLength #-}

  nlIndex = (V.!?)  -- Vector's safe indexing
  {-# INLINE nlIndex #-}

  nlNull = V.null
  {-# INLINE nlNull #-}

  nlUncons = V.uncons  -- Vector provides this directly
  {-# INLINE nlUncons #-}

  nlCons = V.cons
  {-# INLINE nlCons #-}

  nlSnoc = V.snoc
  {-# INLINE nlSnoc #-}

  nlAppend = (V.++)
  {-# INLINE nlAppend #-}

  nlFromList = V.fromList
  {-# INLINE nlFromList #-}

  nlToList = V.toList
  {-# INLINE nlToList #-}

  nlMap = V.map
  {-# INLINE nlMap #-}

  nlFilter = V.filter
  {-# INLINE nlFilter #-}

  nlTraverse = traverse
  {-# INLINE nlTraverse #-}

  nlReverse = V.reverse
  {-# INLINE nlReverse #-}

  nlEmpty = V.empty
  {-# INLINE nlEmpty #-}

  nlFoldl' = V.foldl'
  {-# INLINE nlFoldl' #-}

  -- nlHead and nlTail use default implementations via nlUncons

-- | Seq instance - optimal for O(1) cons/snoc and O(log n) indexing.
instance NixList Seq where
  nlLength = Seq.length
  {-# INLINE nlLength #-}

  nlIndex s i = Seq.lookup i s
  {-# INLINE nlIndex #-}

  nlNull = Seq.null
  {-# INLINE nlNull #-}

  nlUncons s = case Seq.viewl s of
    Seq.EmptyL  -> Nothing
    x Seq.:< xs -> Just (x, xs)
  {-# INLINE nlUncons #-}

  nlCons = (Seq.<|)
  {-# INLINE nlCons #-}

  nlSnoc = (Seq.|>)
  {-# INLINE nlSnoc #-}

  nlAppend = (Seq.><)
  {-# INLINE nlAppend #-}

  nlFromList = Seq.fromList
  {-# INLINE nlFromList #-}

  nlToList = toList
  {-# INLINE nlToList #-}

  nlMap = fmap
  {-# INLINE nlMap #-}

  nlFilter = Seq.filter
  {-# INLINE nlFilter #-}

  nlTraverse = traverse
  {-# INLINE nlTraverse #-}

  nlReverse = Seq.reverse
  {-# INLINE nlReverse #-}

  nlEmpty = Seq.empty
  {-# INLINE nlEmpty #-}

  nlFoldl' = foldl'
  {-# INLINE nlFoldl' #-}

-- | List instance - baseline, useful for comparison and compatibility.
instance NixList [] where
  nlLength = length
  {-# INLINE nlLength #-}

  nlIndex = (!?)  -- Safe indexing from Data.List
  {-# INLINE nlIndex #-}

  nlNull = null
  {-# INLINE nlNull #-}

  nlUncons = uncons  -- From Data.List
  {-# INLINE nlUncons #-}

  nlCons = (:)
  {-# INLINE nlCons #-}

  nlSnoc xs x = xs <> [x]
  {-# INLINE nlSnoc #-}

  nlAppend = (<>)
  {-# INLINE nlAppend #-}

  nlFromList = id
  {-# INLINE nlFromList #-}

  nlToList = id
  {-# INLINE nlToList #-}

  nlMap = fmap
  {-# INLINE nlMap #-}

  nlFilter = filter
  {-# INLINE nlFilter #-}

  nlTraverse = traverse
  {-# INLINE nlTraverse #-}

  nlReverse = reverse
  {-# INLINE nlReverse #-}

  nlEmpty = []
  {-# INLINE nlEmpty #-}

  nlFoldl' = foldl'
  {-# INLINE nlFoldl' #-}
