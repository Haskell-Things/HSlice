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

{- tests for the properties of a DualRightQuad, or a four sided figgure with two right angles. -}

module Math.Geometry.ConcaveChevronQuad (
  concaveChevronQuadBrokenSpec,
  concaveChevronQuadSpec
  ) where

import Prelude (Bool, Show(show), ($), (==), error, length)

-- The Maybe library.
import Data.Maybe (fromMaybe)

-- Slists, a form of list with a stated size in the structure.
import Slist (slist, len)

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Our basic math library.
import Graphics.Slicer.Math.Definitions (Contour)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian(Radian), generationsOf, oneNodeTreeOf, randomConcaveChevronQuad)

-- Our logic for dividing a contour into cells, which each get nodetrees for them, which are combined into a straight skeleton.
import Graphics.Slicer.Math.Skeleton.Cells (findDivisions)

-- The portion of our library that reasons about motorcycles, emiting from the concave nodes of our contour.
import Graphics.Slicer.Math.Skeleton.Motorcycles (convexMotorcycles, crashMotorcycles)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Shared tests, between different geometry.
import Math.Geometry.CommonTests (prop_CanPlaceFaces, prop_ENodeArcsIntersectAtSamePoint, prop_FacesAllWoundLeft, prop_FacesHaveThreeSides, prop_FacesInOrder, prop_HasFourFaces, prop_HasAStraightSkeleton, prop_NodeTreeHasFewerThanThreeGenerations, prop_NoDivides, prop_NoMotorcycles, prop_StraightSkeletonHasOneNodeTree)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->))

prop_OneMotorcycle :: Contour -> Bool
prop_OneMotorcycle contour = length (convexMotorcycles contour) == 1

prop_OneDivide :: Contour -> Expectation
prop_OneDivide contour = len (slist $ findDivisions contour (fromMaybe (error $ show contour) $ crashMotorcycles contour [])) --> 1

prop_NodeTreeHasTwoGenerations :: Contour -> Expectation
prop_NodeTreeHasTwoGenerations contour = generationsOf (oneNodeTreeOf $ fromMaybe (error "no straight skeleton?") $ findStraightSkeleton contour []) --> 2

-- | insane result of filterAllIntersections
{-
also happens with:
         0.0
         0.0
         Radian {getRadian = 1.0}
         Radian {getRadian = 0.1}
         Positive {getPositive = 1.0e-2}
         Positive {getPositive = 29.0}
-}
{-
also happens with:
         0.0
         0.0
         Radian {getRadian = 1.0}
         Radian {getRadian = 1.0}
         Positive {getPositive = 16.0}
         Positive {getPositive = 3.0e-3}
-}
unit_ConcaveChevronQuadNodeTreeHasTwoGenerations :: Expectation
unit_ConcaveChevronQuadNodeTreeHasTwoGenerations = doTest $ randomConcaveChevronQuad x y tilt1 tilt2 distance1 distance2
  where
    doTest concaveChevronQuad = generationsOf (oneNodeTreeOf $ fromMaybe (error "no straight skeleton?") $ findStraightSkeleton concaveChevronQuad []) --> 2
    x,y :: ℝ
    x = 0
    y = 0
    tilt1 = Radian 2.0
    tilt2 = Radian 1.0
    distance1,distance2 :: Positive ℝ
    distance1 = 1.0e-4
    distance2 = 1.0

concaveChevronQuadBrokenSpec :: Spec
concaveChevronQuadBrokenSpec = do
  describe "Geometry (Concave Chevron Quads)" $
    it "generates two generations of INodes(unit)" $
      unit_ConcaveChevronQuadNodeTreeHasTwoGenerations

concaveChevronQuadSpec :: Spec
concaveChevronQuadSpec = do
  describe "Geometry (Concave Chevron Quads)" $ do
    it "finds one motorcycle" $
      property (boolFromConcaveChevronQuad prop_OneMotorcycle)
    it "finds one divide" $
      property (expectationFromConcaveChevronQuad prop_OneDivide)
    it "finds a straight skeleton" $
      property (expectationFromConcaveChevronQuad prop_HasAStraightSkeleton)
    it "only finds one nodetree in the straight skeleton" $
      property (expectationFromConcaveChevronQuad prop_StraightSkeletonHasOneNodeTree)
    it "generates two generations of INodes" $
      property (expectationFromConcaveChevronQuad prop_NodeTreeHasTwoGenerations)
    it "can place faces on the straight skeleton" $
      property (expectationFromConcaveChevronQuad prop_CanPlaceFaces)
    it "only places four faces" $
      property (expectationFromConcaveChevronQuad prop_HasFourFaces)
    it "places faces on a concave chevron quad in the order the line segments were given" $
      property (expectationFromConcaveChevronQuad prop_FacesInOrder)
  where
    boolFromConcaveChevronQuad :: (Contour -> Bool) -> ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Bool
    boolFromConcaveChevronQuad f x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = f concaveChevronQuad
      where
        concaveChevronQuad = randomConcaveChevronQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    expectationFromConcaveChevronQuad :: (Contour -> Expectation) -> ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
    expectationFromConcaveChevronQuad f x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = f concaveChevronQuad
      where
        concaveChevronQuad = randomConcaveChevronQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
