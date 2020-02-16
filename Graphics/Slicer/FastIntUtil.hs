-- Slicer. Copyright 2016 Noah Halford and Catherine Moresco
-- ImplicitCAD Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014-2019, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Use existing instances for the wrapped types rather than manually making them
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Use ℝ somewhat like it's a real type, instead of referring to Double.
{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.Slicer.FastIntUtil (Fastℕ(Fastℕ), toFastℕ, fromFastℕ, maybeToFastℕ) where

import Prelude (Integral, Num, Eq, Ord, Enum, Real, Show, Read, Int, Maybe(Just, Nothing), fromInteger, fromIntegral, id, floor, (==), ($), (.), error)

import Graphics.Slicer.RationalUtil (ℝ)

class FastN n where
  fromFastℕ :: Fastℕ -> n
  toFastℕ :: n -> Fastℕ
  maybeToFastℕ :: n -> Maybe Fastℕ

instance FastN Int where
  fromFastℕ (Fastℕ a) = a
  {-# INLINABLE fromFastℕ #-}
  toFastℕ = Fastℕ
  {-# INLINABLE toFastℕ #-}
  maybeToFastℕ = Just . Fastℕ
  {-# INLINABLE maybeToFastℕ #-}

instance FastN Fastℕ where
  fromFastℕ = id
  {-# INLINABLE fromFastℕ #-}
  toFastℕ = id
  {-# INLINABLE toFastℕ #-}
  maybeToFastℕ = Just
  {-# INLINABLE maybeToFastℕ #-}

instance FastN ℝ where
  fromFastℕ (Fastℕ a) = fromIntegral a
  {-# INLINABLE fromFastℕ #-}
  toFastℕ = error "there can be no toFastℕ of an ℝ"
  {-# INLINABLE toFastℕ #-}
  maybeToFastℕ n = if (fromInteger $ floor n) == (n::ℝ) then Just . Fastℕ $ floor n else Nothing
  {-# INLINABLE maybeToFastℕ #-}

-- System integers, meant to go fast, and have no chance of wrapping 2^31.
newtype Fastℕ = Fastℕ Int
  deriving (Show, Read, Eq, Ord, Num, Enum, Integral, Real)
