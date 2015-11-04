{- |
Module      :  Data.RangeSet.List
Description :  A trivial implementation of range sets
Copyright   :  (c) Oleg Grenrus 2014
License     :  MIT

Maintainer  :  oleg.grenrus@iki.fi
Stability   :  experimental
Portability :  non-portable (tested with GHC only)

A trivial implementation of range sets.

This module is intended to be imported qualified, to avoid name
clashes with Prelude functions, e.g.

>  import Data.RangeSet.List (RSet)
>  import qualified Data.RangeSet.List as RSet

The implementation of 'RSet' is based on /list/.

Compared to 'Data.Set', this module imposes also 'Enum' restriction for many functions.
We must be able to identify consecutive elements to be able to /glue/ and /split/ ranges properly.

The implementation assumes that

> x < succ x
> pred x < x

and there aren't elements in between (not true for 'Float' and 'Double').
Also 'succ' and 'pred' are never called for largest or smallest value respectively.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Safe #-}
module Data.RangeSet.List (
  -- * Range set type
  RSet

  -- * Operators
  , (\\)

  -- * Query
  , null
  , isFull
  , size
  , member
  , notMember

  -- * Construction
  , empty
  , full
  , singleton
  , singletonRange
  , insert
  , insertRange
  , delete
  , deleteRange

  -- * Combine
  , union
  , difference
  , intersection

  -- * Min/Max
  , findMin
  , findMax

  -- * Complement
  , complement

  -- * Conversion
  , elems
  , toList
  , fromList
  , toRangeList
  , fromRangeList

  ) where

import Prelude hiding (filter,foldl,foldr,null,map)
import qualified Prelude

import Control.DeepSeq (NFData(..))
import Data.Foldable (foldMap)
import Data.Typeable (Typeable)
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..), getSum)
import Data.Hashable (Hashable(..))

import Data.RangeSet.Internal

-- | Internally set is represented as sorted list of distinct inclusive ranges.
newtype RSet a = RSet [(a, a)]
  deriving (Eq, Ord, Typeable)

instance Show a => Show (RSet a) where
  show (RSet xs) = "fromRangeList " ++ show xs

instance (Ord a, Enum a) => Semigroup (RSet a) where
    (<>) = union

instance (Ord a, Enum a) => Monoid (RSet a) where
    mempty  = empty
    mappend = union

instance Hashable a => Hashable (RSet a) where
    hashWithSalt salt (RSet xs) = hashWithSalt salt xs

instance NFData a => NFData (RSet a) where
    rnf (RSet xs) = rnf xs

{- Operators -}
infixl 9 \\ --

-- | /O(n+m)/. See 'difference'.
(\\) :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
m1 \\ m2 = difference m1 m2

{- Query -}

-- | /O(1)/. Is this the empty set?
null :: RSet a -> Bool
null = Prelude.null . toRangeList

-- | /O(1)/. Is this the full set?
isFull :: (Eq a, Bounded a) => RSet a -> Bool
isFull = (==) full

-- | /O(n)/. The number of the elements in the set.
size :: Enum a => RSet a -> Int
size (RSet xs) = getSum $ foldMap (uncurry rangeSize) xs

-- | /O(n)/. Is the element in the set?
member :: Ord a => a -> RSet a -> Bool
member x (RSet xs) = f xs where
  f ((a,b):s)
    | x < a = False
    | x <= b = True
    | otherwise = f s
  f [] = False

-- | /O(n)/. Is the element not in the set?
notMember :: Ord a => a -> RSet a -> Bool
notMember a r = not $ member a r

{- Construction -}

-- | /O(1)/. The empty set.
empty :: RSet a
empty = RSet []

-- | /O(1)/. The full set.
full :: Bounded a => RSet a
full = singletonRange' minBound maxBound

singletonRange' :: a -> a -> RSet a
singletonRange' x y = RSet [(x, y)]

-- | /O(1)/. Create a singleton set.
singleton :: a -> RSet a
singleton x = singletonRange' x x

-- | /O(1)/. Create a continuos range set.
singletonRange :: Ord a => (a, a) -> RSet a
singletonRange (x, y) | x > y     = empty
                      | otherwise = singletonRange' x y

{- Construction -}

-- | /O(n)/. Insert an element in a set.
insert :: (Ord a, Enum a) => a -> RSet a -> RSet a
insert x (RSet xs) = RSet $ insertRangeList x x xs

-- | /O(n)/. Insert a continuos range in a set.
insertRange :: (Ord a, Enum a) => (a, a) -> RSet a -> RSet a
insertRange (x, y) set@(RSet xs)
  | x > y      = set
  | otherwise  = RSet $ insertRangeList x y xs

-- | /O(n). Delete an element from a set.
delete :: (Ord a, Enum a) => a -> RSet a -> RSet a
delete x (RSet xs) = RSet $ deleteRangeList x x xs

-- | /O(n). Delete a continuos range from a set.
deleteRange :: (Ord a, Enum a) => (a, a) -> RSet a -> RSet a
deleteRange (x, y) set@(RSet xs)
  | x > y      = set
  | otherwise  = RSet $ deleteRangeList x y xs

{- Combination -}

-- | /O(n+m)/. The union of two sets.
union :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
union (RSet xs) (RSet ys) = RSet $ unionRangeList xs ys

-- | /O(n+m)/. Difference of two sets.
difference :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
difference (RSet xs) (RSet ys) = RSet $ differenceRangeList xs ys

-- | /O(n+m)/. The intersection of two sets.
intersection :: (Ord a) => RSet a -> RSet a -> RSet a
intersection (RSet xs) (RSet ys) = RSet $ intersectRangeList xs ys

{- Complement -}

-- | /O(n)/. Complement of the set.
complement :: (Ord a, Enum a, Bounded a) => RSet a -> RSet a
complement (RSet xs) = RSet $ complementRangeList xs

{- Min/Max -}

-- | /O(1)/. The minimal element of a set.
findMin :: RSet a -> a
findMin (RSet ((x, _) : _))  = x
findMin _                    = error "RangeSet.List.findMin: empty set"

-- | /O(n)/. The maximal element of a set.
findMax :: RSet a -> a
findMax (RSet rs) = findMax' rs
  where findMax' [(_, x)]  = x
        findMax' (_:xs)    = findMax' xs
        findMax' _         = error "RangeSet.List.findMax: empty set"

{- Conversion -}

-- | /O(n*r)/. Convert the set to a list of elements. /r/ is the size of longest range.
elems :: Enum a => RSet a -> [a]
elems = toList

-- | /O(n*r)/. Convert the set to a list of elements. /r/ is the size of longest range.
toList :: Enum a => RSet a -> [a]
toList (RSet xs) = concatMap (uncurry enumFromTo) xs

-- | /O(n*log n)/. Create a set from a list of elements.
fromList :: (Ord a, Enum a) => [a] -> RSet a
fromList = RSet . fromElemList

-- | /O(1)/. Convert the set to a list of range pairs.
toRangeList :: RSet a -> [(a, a)]
toRangeList (RSet xs) = xs

-- | /O(n*log n)/. Create a set from a list of range pairs.
fromRangeList :: (Ord a, Enum a) => [(a, a)] -> RSet a
fromRangeList = RSet . normalizeRangeList
