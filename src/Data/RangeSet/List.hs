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

module Data.RangeSet.List (
  -- * Range set type
  RSet

  -- * Operators
  , (\\)

  -- * Query
  , null
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

import Data.Monoid (Monoid(..))

-- | Internally set is represented as sorted list of distinct inclusive ranges.
newtype RSet a = RSet [(a, a)]
  deriving (Eq, Ord)

instance Show a => Show (RSet a) where
  show (RSet xs) = "fromRangeList " ++ show xs

instance (Ord a, Enum a) => Monoid (RSet a) where
    mempty  = empty
    mappend = union

{- Operators -}
infixl 9 \\ --

-- | /O(n+m)/. See 'difference'.
(\\) :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
m1 \\ m2 = difference m1 m2

{- Query -}

-- | /O(1)/. Is this the empty set?
null :: RSet a -> Bool
null = Prelude.null . toRangeList

-- | /O(n)/. The number of the elements in the set.
size :: Enum a => RSet a -> Int
size (RSet xs) = sum (Prelude.map f xs)
  where f (a, b) = fromEnum b - fromEnum a + 1

-- | /O(n)/. Is the element in the set?
member :: (Ord a, Enum a) => a -> RSet a -> Bool
member x (RSet xs) = any f $ takeWhile g xs
  where f (a, b) = a <= x && x <= b
        g (a,_) = a <= x

-- | /O(n)/. Is the element not in the set?
notMember :: (Ord a, Enum a) => a -> RSet a -> Bool
notMember a r = not $ member a r

{- Construction -}

-- | /O(1)/. The empty set.
empty :: RSet a
empty = RSet []

-- | /O(1)/. The full set.
full :: Bounded a => RSet a
full = RSet [(minBound, maxBound)]

-- | /O(1)/. Create a singleton set.
singleton :: a -> RSet a
singleton x = RSet [(x, x)]

-- | /O(1)/. Create a continuos range set.
singletonRange :: Ord a => (a, a) -> RSet a
singletonRange (x, y) | x > y     = empty
                      | otherwise = RSet [(x, y)]

{- Construction -}

-- | /O(n)/. Insert an element in a set.
insert :: (Ord a, Enum a) => a -> RSet a -> RSet a
insert x = insertRange (x, x)

-- | /O(n)/. Insert a continuos range in a set.
insertRange :: (Ord a, Enum a) => (a, a) -> RSet a -> RSet a
insertRange r@(x, y) set@(RSet xs)
  | x > y      = set
  | otherwise  = RSet $ insertRange' r xs

-- There are three possibilities we consider, when inserting into non-empty set:
-- * discretely less
-- * discretely more
-- * other
insertRange' :: (Ord a, Enum a) => (a, a) -> [(a, a)] -> [(a, a)]
insertRange' r        []  = [r]
insertRange' r@(x, y) set@(s@(u, v) : xs)
  | y < u && succ y /= u  = r : set
  | v < x && succ v /= x  = s : insertRange' r xs
  | otherwise             = insertRange' (min x u, max y v) xs

-- | /O(n). Delete an element from a set.
delete :: (Ord a, Enum a) => a -> RSet a -> RSet a
delete x = deleteRange (x, x)

-- | /O(n). Delete a continuos range from a set.
deleteRange :: (Ord a, Enum a) => (a, a) -> RSet a -> RSet a
deleteRange r@(x, y) set@(RSet xs)
  | x > y      = set
  | otherwise  = RSet $ deleteRange' r xs

-- There are 6 possibilities we consider, when deleting from non-empty set:
-- * less
-- * more
-- * strictly inside (splits)
-- * overlapping less-edge
-- * overlapping more-edge
-- * stricly larger
--
-- TODO: is there simpler rules, with less cases
deleteRange' :: (Ord a, Enum a) => (a, a) -> [(a, a)] -> [(a, a)]
deleteRange' _        []  = []
deleteRange' r@(x, y) set@(s@(u, v) : xs)
  | y < u                 = set
  | v < x                 = s : deleteRange' r xs
  | u < x && y < v        = (u, pred x) : (succ y, v) : xs
  | y < v                 = (succ y, v) : xs
  | u < x                 = (u, pred x) : deleteRange' r xs
  | otherwise             = deleteRange' r xs

{- Combination -}

-- | /O(n*m)/. The union of two sets.
union :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
union set (RSet xs) = Prelude.foldr insertRange set xs

-- | /O(n*m)/. Difference of two sets.
difference :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
difference set (RSet xs) = Prelude.foldr deleteRange set xs

-- | /O(n*m)/. The intersection of two sets.
intersection :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
intersection a b = a \\ (a \\ b)

{- Complement -}

-- | /O(n)/. Complement of the set.
complement :: (Ord a, Enum a, Bounded a) => RSet a -> RSet a
complement a = full `difference` a

{- Conversion -}

-- | /O(n*r)/. Convert the set to a list of elements. /r/ is the size of longest range.
elems :: Enum a => RSet a -> [a]
elems = toList

-- | /O(n*r)/. Convert the set to a list of elements. /r/ is the size of longest range.
toList :: Enum a => RSet a -> [a]
toList (RSet xs) = concatMap (uncurry enumFromTo) xs

-- | /O(n^2)/. Create a set from a list of elements.
fromList :: (Ord a, Enum a) => [a] -> RSet a
fromList = fromRangeList . Prelude.map f
  where f a = (a, a)

-- | /O(1)/. Convert the set to a list of range pairs.
toRangeList :: RSet a -> [(a, a)]
toRangeList (RSet xs) = xs

-- | /O(n^2)/. Create a set from a list of range pairs.
fromRangeList :: (Ord a, Enum a) => [(a, a)] -> RSet a
fromRangeList = Prelude.foldr insertRange empty
