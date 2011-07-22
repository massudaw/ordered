{-
 - Copyright (C) 2009-2010 Nick Bowler.
 -
 - License BSD2:  2-clause BSD license.  See LICENSE for full terms.
 - This is free software: you are free to change and redistribute it.
 - There is NO WARRANTY, to the extent permitted by law.
 -}

module Data.Poset.Internal where

import Data.Monoid

-- | Are two elements of the underlying comparabale or not; if they
-- are, then Ordering tell the relation between them.
data PosetOrd = Comp Ordering | NComp
    deriving (Eq, Show, Read)

instance Bounded PosetOrd where
    minBound = Comp $ minBound 
    maxBound = NComp

instance Enum PosetOrd where
    toEnum n | n >= 0 && n < 4 = Comp $ toEnum n
             | n == 4 = NComp
             | otherwise = error "Data.Poset.toEnum: bad argument"

    fromEnum (Comp c) = fromEnum c
    fromEnum NComp = 4

-- Lexicographic ordering.

instance Monoid PosetOrd where
    mempty = Comp EQ
    mappend (Comp EQ) x = x
    mappend NComp _ = NComp
    mappend (Comp LT) _ = Comp LT
    mappend (Comp GT) _ = Comp GT


-- | Internal-use function to convert our Ordering to the ordinary one.
unsafeTotalOrder :: PosetOrd -> Ordering
unsafeTotalOrder (Comp cmp) = cmp
unsafeTotalOrder NComp = error "Uncomparable elements in total order."

-- | Internal-use function to convert the ordinary Ordering to ours.
partialOrder :: Ordering -> PosetOrd
partialOrder = Comp

-- | Class for partially ordered data types.  Instances should satisfy the
-- following laws for all values a, b and c:
--
-- * @a `leq` a@.
--
-- * @a `leq` b@ and @b `leq` a@ implies @a == b@.
--
-- * @a `leq` b@ and @b `leq` c@ implies @a `leq` c@.
--
-- But note that the floating point instances don't satisfy the first rule.
--
-- Minimal definition: posetCmp or leq.
class Eq a => Poset a where
    posetCmp :: a -> a -> PosetOrd
    -- | Is comparable to.
    (<=?=>)  :: a -> a -> Bool
    -- | Is not comparable to.
    (<=/?=>)  :: a -> a -> Bool
    -- | Less than or equal.
    leq :: a -> a -> Bool
    -- | Greater than or equal.
    geq :: a -> a -> Bool
    -- | Strict less than.
    lt :: a -> a -> Bool
    -- | Strict greater than.
    gt :: a -> a -> Bool

    a `posetCmp` b
        | a == b = Comp EQ
        | a `leq` b = Comp LT
        | b `leq` a = Comp GT
        | otherwise = NComp

    a <=?=> b = a `posetCmp` b /= NComp
    a <=/?=> b = a `posetCmp` b == NComp

    a `lt` b = a `posetCmp` b == Comp LT
    a `gt` b = a `posetCmp` b == Comp GT

    a `leq` b | a <=?=> b = a `posetCmp` b /= Comp GT
              | otherwise = False
    a `geq` b | a <=?=> b = a `posetCmp` b /= Comp LT
              | otherwise = False

infixl 4 <=?=>,<=/?=>
