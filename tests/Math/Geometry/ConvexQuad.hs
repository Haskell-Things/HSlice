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

import Prelude (Show(show), ($), error, length)

-- The Maybe library.
import Data.Maybe (fromMaybe, Maybe(Nothing))

-- Slists, a form of list with a stated size in the structure.
import Slist (slist)

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Our Contour library.
import Graphics.Slicer.Math.Contour (lineSegsOfContour)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian, edgesOf, generationsOf, randomConvexQuad, onlyOneOf)

-- Our logic for dividing a contour into cells, which each get nodetrees for them, which are combined into a straight skeleton.
import Graphics.Slicer.Math.Skeleton.Cells (findDivisions)

-- The part of our library that puts faces onto a contour. faces have one exterior side, and a number of internal sides (defined by Arcs).
import Graphics.Slicer.Math.Skeleton.Face (facesOf, orderedFacesOf)

-- The portion of our library that reasons about motorcycles, emiting from the concave nodes of our contour.
import Graphics.Slicer.Math.Skeleton.Motorcycles (crashMotorcycles)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->), (-/>))

prop_ConvexQuadNoDivides :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadNoDivides x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = findDivisions convexQuad (fromMaybe (error $ show convexQuad) $ crashMotorcycles convexQuad []) --> []
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner

prop_ConvexQuadHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadHasStraightSkeleton x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = findStraightSkeleton convexQuad [] -/> Nothing
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner

prop_ConvexQuadStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadStraightSkeletonHasRightGenerationCount x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = generationsOf (findStraightSkeleton convexQuad []) --> 1
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner

prop_ConvexQuadCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadCanPlaceFaces x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = facesOf (fromMaybe (error $ show convexQuad) $ findStraightSkeleton convexQuad []) -/> slist []
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner

prop_ConvexQuadHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadHasRightFaceCount x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = length (facesOf $ fromMaybe (error $ show convexQuad) $ findStraightSkeleton convexQuad []) --> 4
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner

prop_ConvexQuadFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadFacesInOrder x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show convexQuad) $ findStraightSkeleton convexQuad []) --> convexQuadAsSegs
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner
    convexQuadAsSegs = lineSegsOfContour convexQuad
    firstSeg = onlyOneOf convexQuadAsSegs

convexQuadSpec :: Spec
convexQuadSpec = do
  describe "Geometry (Convex Quads)" $ do
    it "finds no divides in a convex quad" $
      property prop_ConvexQuadNoDivides
    it "finds the straight skeleton of a convex quad (property)" $
      property prop_ConvexQuadHasStraightSkeleton
    it "only generates one generation for a convex quad" $
      property prop_ConvexQuadStraightSkeletonHasRightGenerationCount
    it "places faces on the straight skeleton of a convex quad" $
      property prop_ConvexQuadCanPlaceFaces
    it "finds only four faces for any convex quad" $
      property prop_ConvexQuadHasRightFaceCount
    it "places faces on a convex quad in the order the line segments were given" $
      property prop_ConvexQuadFacesInOrder
