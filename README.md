# range-set-list

[![Build Status](https://travis-ci.org/phadej/range-set-list.svg?branch=travis-expr)](https://travis-ci.org/phadej/range-set-list)
[![Hackage](https://img.shields.io/hackage/v/range-set-list.svg)](http://hackage.haskell.org/package/range-set-list)
[![Stackage LTS 2](http://stackage.org/package/range-set-list/badge/lts-2)](http://stackage.org/lts-2/package/range-set-list)
[![Stackage LTS 3](http://stackage.org/package/range-set-list/badge/lts-3)](http://stackage.org/lts-3/package/range-set-list)
[![Stackage Nightly](http://stackage.org/package/range-set-list/badge/nightly)](http://stackage.org/nightly/package/range-set-list)

A trivial implementation of range sets.

You can find the package (and it's documentation) on [hackage](http://hackage.haskell.org/package/range-set-list).

This module is intended to be imported qualified, to avoid name
clashes with Prelude functions, e.g.

```haskell
import Data.RangeSet.List (RSet)
import qualified Data.RangeSet.List as RSet
```

The implementation of `RSet` is based on _list_.

Compared to [`Data.Set`](http://hackage.haskell.org/package/containers-0.5.4.0/docs/Data-Set.html),
this module imposes also [`Enum`](http://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html#t:Enum)
restriction for many functions.
We must be able to identify consecutive elements to be able to _glue_ and _split_ ranges properly.

The implementation assumes that

```haskell
x < succ x
pred x < x
```

and there aren't elements in between (not true for `Float` and `Double`).
Also `succ` and `pred` are never called for largest or smallest value respectively.
