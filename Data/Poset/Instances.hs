{-
 - Copyright (C) 2009-2010 Nick Bowler.
 -
 - License BSD2:  2-clause BSD license.  See LICENSE for full terms.
 - This is free software: you are free to change and redistribute it.
 - There is NO WARRANTY, to the extent permitted by law.
 -}

-- | 'Poset' and 'Sortable' instances for instances of 'Prelude.Ord'
{-# LANGUAGE CPP #-}
module Data.Poset.Instances where

import qualified Data.Poset.Internal as Poset
import Data.Poset.Internal (Poset, partialOrder, unsafeTotalOrder)

import Data.Ratio
import Data.List
import Data.Word
import Data.Int

#define POSET_ORD_INSTANCE(ctx, v) instance ctx Poset (v) where { \
    posetCmp = (partialOrder .) . compare; \
    (<=?=>)  = const $ const True; \
    (<=/?=>)  = const $ const False }

POSET_ORD_INSTANCE(, Bool)
POSET_ORD_INSTANCE(, Char)
POSET_ORD_INSTANCE(, Int)
POSET_ORD_INSTANCE(, Int8)
POSET_ORD_INSTANCE(, Int16)
POSET_ORD_INSTANCE(, Int32)
POSET_ORD_INSTANCE(, Int64)
POSET_ORD_INSTANCE(, Word)
POSET_ORD_INSTANCE(, Word8)
POSET_ORD_INSTANCE(, Word16)
POSET_ORD_INSTANCE(, Word32)
POSET_ORD_INSTANCE(, Word64)
POSET_ORD_INSTANCE(, Integer)

POSET_ORD_INSTANCE(Integral a =>, Ratio a)
