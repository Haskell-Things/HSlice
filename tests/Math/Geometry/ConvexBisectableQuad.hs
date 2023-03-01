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

{- tests for the properties of a quadralaterial that is convex, and has at least one right angle. -}

module Math.Geometry.ConvexBisectableQuad (
  convexBisectableQuadSpec
  ) where

import Prelude (Bool, Show(show), ($), (<), error, length)

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
import Graphics.Slicer.Math.RandomGeometry (Radian, edgesOf, generationsOf, nodeTreesOf, oneNodeTreeOf, onlyOneOf, randomConvexBisectableQuad)

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

prop_ConvexBisectableQuadNoDivides :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadNoDivides x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = findDivisions convexBisectableQuad (fromMaybe (error $ show convexBisectableQuad) $ crashMotorcycles convexBisectableQuad []) --> []
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadHasStraightSkeleton x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = findStraightSkeleton convexBisectableQuad [] -/> Nothing
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadStraightSkeletonHasOneNodeTree :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadStraightSkeletonHasOneNodeTree x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = nodeTreesOf (findStraightSkeleton convexBisectableQuad []) --> 1
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadNodeTreeHasLessThanThreeGenerations :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_ConvexBisectableQuadNodeTreeHasLessThanThreeGenerations x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = generationsOf (oneNodeTreeOf $ fromMaybe (error "no skeleton?") $ findStraightSkeleton convexBisectableQuad []) < 3
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadCanPlaceFaces x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = facesOf (fromMaybe (error $ show convexBisectableQuad) $ findStraightSkeleton convexBisectableQuad []) -/> slist []
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadHasRightFaceCount x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = length (facesOf $ fromMaybe (error $ show convexBisectableQuad) $ findStraightSkeleton convexBisectableQuad []) --> 4
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadFacesInOrder x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show convexBisectableQuad) $ findStraightSkeleton convexBisectableQuad []) --> convexBisectableQuadAsSegs
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    convexBisectableQuadAsSegs = lineSegsOfContour convexBisectableQuad
    firstSeg = onlyOneOf convexBisectableQuadAsSegs

convexBisectableQuadSpec :: Spec
convexBisectableQuadSpec = do
  describe "Geometry (Convex Bisectable Quads)" $ do
    it "finds no divides" $
      property prop_ConvexBisectableQuadNoDivides
    it "finds a straight skeleton" $
      property prop_ConvexBisectableQuadHasStraightSkeleton
    it "only finds one nodetree in the straight skeleton" $
      property prop_ConvexBisectableQuadStraightSkeletonHasOneNodeTree
    it "finds fewer than three generations in the found nodeTree" $
      property prop_ConvexBisectableQuadNodeTreeHasLessThanThreeGenerations
    it "places faces on the straight skeleton of a convex bisectable quad" $
      property prop_ConvexBisectableQuadCanPlaceFaces
    it "finds only four faces for any convex bisectable quad" $
      property prop_ConvexBisectableQuadHasRightFaceCount
    it "places faces on a convex bisectable quad in the order the line segments were given" $
      property prop_ConvexBisectableQuadFacesInOrder
