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

module Math.Geometry.ConvexQuad (
  convexQuadSpec
  ) where

import Prelude (Bool, Show(show), ($), error)

-- The Maybe library.
import Data.Maybe (fromMaybe)

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Our Contour library.
import Graphics.Slicer.Math.Contour (lineSegsOfContour)

-- Basic definitions, used in multiple places in the math library.
import Graphics.Slicer.Math.Definitions (Contour)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian, edgesOf, onlyOneOf, randomConvexQuad)

-- The part of our library that puts faces onto a contour. faces have one exterior side, and a number of internal sides (defined by Arcs).
import Graphics.Slicer.Math.Skeleton.Face (orderedFacesOf)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Shared tests, between different geometry.
import Math.Geometry.CommonTests (prop_CanPlaceFaces, prop_HasFourFaces, prop_HasAStraightSkeleton, prop_NodeTreeHasFewerThanFourGenerations, prop_NoDivides, prop_NoMotorcycles, prop_StraightSkeletonHasOneNodeTree)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->))

prop_ConvexQuadFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadFacesInOrder x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show convexQuad) $ findStraightSkeleton convexQuad []) --> convexQuadAsSegs
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner
    convexQuadAsSegs = lineSegsOfContour convexQuad
    firstSeg = onlyOneOf convexQuadAsSegs

convexQuadSpec :: Spec
convexQuadSpec = do
  describe "Geometry (Convex Quads)" $ do
    it "finds no convex motorcycles" $
      property (expectationFromConvexQuad prop_NoMotorcycles)
    it "finds no divides" $
      property (expectationFromConvexQuad prop_NoDivides)
    it "finds a straight skeleton" $
      property (expectationFromConvexQuad prop_HasAStraightSkeleton)
    it "only finds one nodetree in the straight skeleton" $
      property (expectationFromConvexQuad prop_StraightSkeletonHasOneNodeTree)
    it "generates one, two, or three generations of INodes" $
      property (boolFromConvexQuad prop_NodeTreeHasFewerThanFourGenerations)
    it "can place faces on the straight skeleton" $
      property (expectationFromConvexQuad prop_CanPlaceFaces)
    it "only places four faces" $
      property (expectationFromConvexQuad prop_HasFourFaces)
    it "places faces on a convex quad in the order the line segments were given" $
      property prop_ConvexQuadFacesInOrder
  where
    boolFromConvexQuad :: (Contour -> Bool) -> ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Bool
    boolFromConvexQuad f x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = f convexQuad
      where
        convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner
    expectationFromConvexQuad :: (Contour -> Expectation) -> ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ-> Expectation
    expectationFromConvexQuad f x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner= f convexQuad
      where
        convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner
