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
  prop_HasAStraightSkeleton,
  prop_NodeTreeHasFewerThanFourGenerations,
  prop_NoDivides,
  prop_NoMotorcycles,
  prop_StraightSkeletonHasOneNodeTree
  ) where

import Prelude (Bool, ($), (<), error, show)

-- The Maybe library.
import Data.Maybe (Maybe(Nothing), fromMaybe)

-- Hspec, for writing specs.
import Test.Hspec (Expectation)

-- Basic definitions, used in multiple places in the math library.
import Graphics.Slicer.Math.Definitions (Contour)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (generationsOf, nodeTreesOf, oneNodeTreeOf)

-- Our logic for dividing a contour into cells, which each get nodetrees for them, which are combined into a straight skeleton.
import Graphics.Slicer.Math.Skeleton.Cells (findDivisions)

-- The portion of our library that reasons about motorcycles, emiting from the concave nodes of our contour.
import Graphics.Slicer.Math.Skeleton.Motorcycles (crashMotorcycles, convexMotorcycles)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->), (-/>))

-- | Ensure we can actually draw a straight skeleton for the given contour.
prop_HasAStraightSkeleton :: Contour -> Expectation
prop_HasAStraightSkeleton contour = findStraightSkeleton contour [] -/> Nothing

prop_NodeTreeHasFewerThanFourGenerations :: Contour -> Bool
prop_NodeTreeHasFewerThanFourGenerations contour = generationsOf (oneNodeTreeOf $ fromMaybe (error "no straight skeleton?") $ findStraightSkeleton contour []) < 4

-- | Ensure the given contour has no divides in it.
prop_NoDivides :: Contour -> Expectation
prop_NoDivides contour = findDivisions contour (fromMaybe (error $ show contour) $ crashMotorcycles contour []) --> []

-- | Ensure no motorcycles are found in the given contour.
prop_NoMotorcycles :: Contour -> Expectation
prop_NoMotorcycles contour = convexMotorcycles contour --> []

-- | Ensure that for a given contour, only one nodetree is constructed.
prop_StraightSkeletonHasOneNodeTree :: Contour -> Expectation
prop_StraightSkeletonHasOneNodeTree contour = nodeTreesOf (findStraightSkeleton contour []) --> 1
