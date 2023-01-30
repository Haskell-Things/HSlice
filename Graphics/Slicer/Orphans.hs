{-
 - Copyright 2016 Noah Halford and Catherine Moresco
 - Copyright 2019 Julia Longtin
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU Affero General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU Affero General Public License for more details.

 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

-- This file is a container for orphan instances. these should go in their appropriate upstreams.

-- So that none of the orphans here generate warnings.
{-# OPTIONS_GHC -Wno-orphans #-}

-- So we can use ℝ in instance declarations.
{-# LANGUAGE FlexibleInstances #-}

-- So we can use TowardInf'
{-# LANGUAGE DataKinds #-}

-- So we can add generic to ℝ
{-# LANGUAGE StandaloneDeriving #-}

-- So we can add generic to ℝ
{-# LANGUAGE DeriveGeneric #-}

-- So we can add MemoTrie to ℝ
{-# LANGUAGE TypeFamilies #-}

-- So we can add MemoTrie to ℝ
{-# LANGUAGE TypeOperators #-}

module Graphics.Slicer.Orphans () where

import Prelude (Double, Integer, Int, Monoid(mempty), Ord, Semigroup((<>)), (+), (.), ($), decodeFloat, error, encodeFloat, seq, uncurry)

import Control.DeepSeq (NFData (rnf))

import Data.MemoTrie (HasTrie(enumerate, trie, untrie), Reg, (:->:), enumerateGeneric, trieGeneric, untrieGeneric)

import Data.Set (Set, elems, fromList)

import GHC.Generics (Generic)

import Graphics.Slicer.Definitions (Fastℕ(Fastℕ), ℝ)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf))

import Slist.Size (Size (Infinity, Size))

import Slist.Type (Slist (Slist))

instance NFData a => NFData (Slist a) where
  rnf (Slist vals n) = rnf vals `seq` rnf n

instance NFData Size where
  rnf Infinity = ()
  rnf (Size n) = seq n ()

-- | FIXME: move this to the proper place in ImplicitCAD.
instance NFData Fastℕ where
  rnf a = seq a ()

instance Semigroup ℝ where
  (<>) a b = a + b

instance Monoid ℝ where
  mempty = 0

deriving instance Generic ℝ

deriving instance Generic Fastℕ

deriving instance Generic Size

deriving instance Generic (Slist a)

-- FIXME: test decodeFloat
mangle :: Double -> (Integer, Int)
mangle = decodeFloat

-- FIXME: test encodeFloat
unMangle :: (Integer, Int) -> Double
unMangle = uncurry encodeFloat

instance HasTrie Double where
  data Double :->: a = DoubleTrie ((Integer, Int) :->: a)
  trie f = DoubleTrie $ trie $ f . unMangle
  untrie (DoubleTrie t) = untrie t . mangle
  enumerate = error "cannot enumerate doubles."

instance (HasTrie a, Ord a) => HasTrie (Set a) where
  data (Set a) :->: b = SetTrie ([a] :->: b)
  trie s = SetTrie $ trie $ s . fromList
  untrie (SetTrie t) = untrie t . elems
  enumerate = error "cannot enumerate sets."

instance HasTrie Size where
  newtype (Size :->: b) = SizeTrie { unSizeTrie :: Reg Size :->: b }
  trie = trieGeneric SizeTrie
  untrie = untrieGeneric unSizeTrie
  enumerate = enumerateGeneric unSizeTrie

instance HasTrie Fastℕ where
  newtype (Fastℕ :->: b) = FastℕTrie { unFastℕTrie :: Reg Fastℕ :->: b }
  trie = trieGeneric FastℕTrie
  untrie = untrieGeneric unFastℕTrie
  enumerate = enumerateGeneric unFastℕTrie

instance (HasTrie a) => HasTrie (Slist a) where
  newtype ((Slist a) :->: b) = SlistTrie { unSlistTrie :: Reg (Slist a) :->: b }
  trie = trieGeneric SlistTrie
  untrie = untrieGeneric unSlistTrie
  enumerate = enumerateGeneric unSlistTrie

instance HasTrie (Rounded 'TowardInf ℝ) where
  newtype ((Rounded 'TowardInf ℝ) :->: b) = RTℝTrie { unRTℝTrie :: Reg (Rounded 'TowardInf ℝ) :->: b }
  trie = trieGeneric RTℝTrie
  untrie = untrieGeneric unRTℝTrie
  enumerate = enumerateGeneric unRTℝTrie
