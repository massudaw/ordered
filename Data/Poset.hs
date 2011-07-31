{-
 - Copyright (C) 2009 Nick Bowler.
 -
 - License BSD2:  2-clause BSD license.  See LICENSE for full terms.
 - This is free software: you are free to change and redistribute it.
 - There is NO WARRANTY, to the extent permitted by law.
 -}

-- | Partially ordered data types.  The standard 'Prelude.Ord' class is for
-- total orders and therefore not suitable for floating point.  However, we can
-- still define meaningful 'max' and 'sort' functions for these types.
--
-- We define a 'PosetOrd' class which extends 'Prelude.Ord' by adding
-- a 'NComp' constructor representing that two elements are
-- incomparable.
module Data.Poset (
    Poset(..), 
    PosetOrd(..),
    module Data.Poset
) where

import Data.Poset.Instances
import Data.Poset.Internal

import Data.Function
import Data.Monoid

instance Poset a => Poset (Maybe a) where
    Just x  `leq` Just y = x `leq` y
    Nothing `leq` _      = True
    _       `leq` _      = False

instance Poset a => Poset [a] where
    posetCmp = (mconcat .) . zipWith posetCmp

-- | Apply a function to values before comparing.
comparing :: Poset b => (a -> b) -> a -> a -> PosetOrd
comparing = on posetCmp
