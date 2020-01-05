
-- Slicer.
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

-- The purpose of this file is to hold datatype definitions that are useful in all of the code.

module Graphics.Slicer.Definitions (ℝ, toℝ, ℝ2, ℝ3, ℕ, Fastℕ, fromFastℕ, toFastℕ) where

import Prelude (Double, Rational, fromRational)

import Graphics.Slicer.FastIntUtil (Fastℕ, fromFastℕ, toFastℕ)

import Graphics.Slicer.IntegralUtil (ℕ)

-- Let's make things a bit nicer.
-- Following the math notation ℝ, ℝ², ℝ³...
type ℝ = Double
type ℝ2 = (ℝ,ℝ)
type ℝ3 = (ℝ,ℝ,ℝ)

toℝ :: Rational -> ℝ
toℝ val = fromRational val

