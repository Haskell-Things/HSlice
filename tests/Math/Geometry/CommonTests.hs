{- ORMOLU_DISABLE -}
{- HSlice.
 - Copyright 2020-2022 Julia Longtin
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

{- property tests that are reusable with many types of geometric figgures. -}

module Math.Geometry.CommonTests (
  prop_NoMotorcycles
  ) where

import Prelude ()

-- Hspec, for writing specs.
import Test.Hspec (Expectation)

-- Basic definitions, used in multiple places in the math library.
import Graphics.Slicer.Math.Definitions (Contour)

-- The portion of our library that reasons about motorcycles, emiting from the concave nodes of our contour.
import Graphics.Slicer.Math.Skeleton.Motorcycles (convexMotorcycles)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->))

prop_NoMotorcycles :: Contour -> Expectation
prop_NoMotorcycles contour = convexMotorcycles contour --> []

