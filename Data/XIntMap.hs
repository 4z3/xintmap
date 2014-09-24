-- This file was part of Quipper. Copyright (C) 2011-2014. Please see the
-- file COPYRIGHT.quipper for a list of authors, copyright holders, licensing,
-- and other details. All rights reserved.
--
-- ======================================================================

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.XIntMap (
  XIntMap,
  delete,
  deletes,
  insert,
  inserts,
  lookup,
  member,
  empty,
  freshkey,
  freshkeys,
  toIntmap,
  size,
  dirty,
  reserves,
  unreserves,
  makeclean,
  ) where

-- import other stuff
import Prelude hiding (lookup)

import Data.List (foldl')

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- ----------------------------------------------------------------------
-- * Set related operations

-- | Insert the elements of a list in an 'IntSet' (cf. 'IntSet.insert').
intset_inserts :: [Int] -> IntSet -> IntSet
intset_inserts list set =
  foldl' (\t x -> IntSet.insert x t) set list


-- ----------------------------------------------------------------------
-- * XIntMaps.

-- | A 'XIntMap' is just like an 'IntMap', except that it supports
-- some additional efficient operations: to find the smallest unused
-- key, to find the set of all keys ever used in the past, and to
-- reserve a set of keys so that they will not be allocated. Moreover,
-- it keeps track of the highest key ever used (whether or not it is
-- still used in the current map).

-- This is implemented as a tuple (/m/, /n/, /free/, /h/), where /m/ is an
-- 'IntMap', /n/ is an integer such that dom /m/ ⊆ [0../n/-1], /free/
-- ⊆ [0../n/-1] \\ dom /m/ is a set of integers not currently reserved
-- or used, and /h/ is the set of all integers used in the past (the
-- set of /touched/ wires).

data XIntMap a = XIntMap !(IntMap a) !Int !IntSet !IntSet

instance (Show a) => Show (XIntMap a) where
  show = show . toIntmap

-- | Delete a key from the 'XIntMap'.
delete :: Int -> XIntMap a -> XIntMap a
delete k (XIntMap m n free h) = (XIntMap m' n free' h) where
  m' = IntMap.delete k m
  free' = IntSet.insert k free

-- | Delete a list of keys from a 'XIntMap'.
deletes :: [Int] -> XIntMap a -> XIntMap a
deletes list map =
  foldl' (\map k -> delete k map) map list

-- | Insert a new key-value pair in the 'XIntMap'.
insert :: Int -> a -> XIntMap a -> XIntMap a
insert k v (XIntMap m n free h) = (XIntMap m' n' free' h') where
  m' = IntMap.insert k v m
  h' = IntSet.insert k h
  n' = max n (k+1)
  free' = IntSet.delete k (intset_inserts [n..n'-1] free)

-- | Insert a list of key-value pairs in the 'XIntMap'.
inserts :: [(Int,a)] -> XIntMap a -> XIntMap a
inserts list map =
  foldl' (\map (k,v) -> insert k v map) map list

-- | Look up the value at a key in the 'XIntMap'. Return 'Nothing' if
-- not found.
lookup :: Int -> XIntMap a -> Maybe a
lookup k (XIntMap m n free h) =
  IntMap.lookup k m

-- | Check whether the given key is in the 'XIntMap'.
member :: Int -> XIntMap a -> Bool
member k (XIntMap m n free h) =
    IntMap.member k m

-- | The empty 'XIntMap'.
empty :: XIntMap a
empty = (XIntMap m n free h) where
  m = IntMap.empty
  n = 0
  free = IntSet.empty
  h = IntSet.empty

-- | Return the first free key in the 'XIntMap', but without actually
-- using it yet.
freshkey :: XIntMap a -> Int
freshkey (XIntMap m n free h) =
  if IntSet.null free then n else IntSet.findMin free

-- | Return the next /k/ unused keys in the 'XIntMap', but without
-- actually using them yet.
freshkeys :: Int -> XIntMap a -> [Int]
freshkeys k (XIntMap m n free h) = ks1 ++ ks2 where
  ks1 = take k (IntSet.elems free)
  delta = k - (length ks1)
  ks2 = [n .. n+delta-1]

-- | Convert a 'XIntMap' to an 'IntMap'.
toIntmap :: XIntMap a -> IntMap a
toIntmap (XIntMap m n free h) = m

-- | Return the smallest key never used in the 'XIntMap'.
size :: XIntMap a -> Int
size (XIntMap m n free k) = n

-- | Return the set of all keys ever used in the 'XIntMap'.
touched :: XIntMap a -> IntSet
touched (XIntMap m n free h) = h

-- | A wire is /dirty/ if it is touched but currently free.
dirty :: XIntMap a -> IntSet
dirty (XIntMap m n free h) = h `IntSet.intersection` free

-- | Reserve a key in the 'XIntMap'. If the key is not free, do
-- nothing. The key must have been used before; for example, this is
-- the case if it was returned by 'dirty'.
reserve :: Int -> XIntMap a -> XIntMap a
reserve k (XIntMap m n free h) = (XIntMap m n free' h) where
  free' = IntSet.delete k free

-- | Reserve a set of keys in the 'XIntMap'. For any keys that are not
-- free, do nothing. All keys must have been used before; for example,
-- this is the case if they were returned by 'dirty'.
reserves :: IntSet -> XIntMap a -> XIntMap a
reserves ks (XIntMap m n free h) = (XIntMap m n free' h) where
  free' = free `IntSet.difference` ks

-- | Unreserve a key in the 'XIntMap'. If the key is currently used,
-- do nothing. The key must have been reserved before, and (therefore)
-- must have been used before.
unreserve :: Int -> XIntMap a -> XIntMap a
unreserve k (XIntMap m n free h)
  | IntMap.member k m = (XIntMap m n free h)
  | otherwise = (XIntMap m n free' h)
    where
      free' = IntSet.insert k free

-- | Unreserve a list of keys in the 'XIntMap'. If any key is
-- currently used, do nothing. All keys must have been reserved
-- before, and (therefore) must have been used before.
unreserves :: IntSet -> XIntMap a -> XIntMap a
unreserves ks map =
  IntSet.fold (\k map -> unreserve k map) map ks

-- | Make an exact copy of the 'XIntMap', except that the set of
-- touched wires is initially set to the set of used wires. In other
-- words, we mark all free and reserved wires as untouched.
makeclean :: XIntMap a -> XIntMap a
makeclean (XIntMap m n free h) = (XIntMap m n free h') where
  h' = IntMap.keysSet m

