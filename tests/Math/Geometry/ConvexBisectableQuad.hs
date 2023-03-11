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
  convexBisectableQuadBrokenSpec,
  convexBisectableQuadSpec
  ) where

import Prelude (Bool, Show(show), ($), (<$>), error)

-- The Maybe library.
import Data.Maybe (fromMaybe)

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Assorted basic math functions.
import Graphics.Slicer.Math.Definitions (Contour)

-- Basic intersection logic.
import Graphics.Slicer.Math.Intersections (intersectionsAtSamePoint)

-- Our 2D Projective Geometric Algebra library.
import Graphics.Slicer.Math.PGA (outAndErrOf)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian(Radian), randomConvexBisectableQuad)

-- Our logic for dividing a contour into cells, which each get nodetrees for them, which are combined into a straight skeleton.
import Graphics.Slicer.Math.Skeleton.Cells (findDivisions)

-- The logic for creating straight skeletons from concave contours.
import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour)

-- The portion of our library that reasons about motorcycles, emiting from the concave nodes of our contour.
import Graphics.Slicer.Math.Skeleton.Motorcycles (crashMotorcycles)

-- Shared tests, between different geometry.
import Math.Geometry.CommonTests (prop_CanPlaceFaces, prop_ENodeArcsIntersectAtSamePoint, prop_FacesAllWoundLeft, prop_FacesHaveThreeSides, prop_FacesInOrder, prop_HasFourFaces, prop_HasAStraightSkeleton, prop_NodeTreeHasFewerThanThreeGenerations, prop_NoDivides, prop_NoMotorcycles, prop_StraightSkeletonHasOneNodeTree)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->))

-- | can fail to flip the contour.
unit_ConvexBisectableQuadNoDivides :: Expectation
unit_ConvexBisectableQuadNoDivides = findDivisions convexBisectableQuad (fromMaybe (error $ show convexBisectableQuad) $ crashMotorcycles convexBisectableQuad []) --> []
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    x,y :: ℝ
    x = -37.7
    y = 5.67
    rawFirstTilt = Radian 0.852
    rawSecondTilt = Radian 2.218
    rawFirstDistanceToCorner,rawSecondDistanceToCorner :: Positive ℝ
    rawFirstDistanceToCorner = 25.08
    rawSecondDistanceToCorner = 4.595

-- looks like another rectangle.
unit_ConvexBisectableQuadENodeArcsIntersectAtSamePoint :: Bool
unit_ConvexBisectableQuadENodeArcsIntersectAtSamePoint = retVal
  where
    retVal = intersectionsAtSamePoint nodeOutsAndErrs
    nodeOutsAndErrs = outAndErrOf <$> eNodes
    eNodes = eNodesOfOutsideContour convexBisectableQuad
    convexBisectableQuad = randomConvexBisectableQuad centerX centerY rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    centerX,centerY :: ℝ
    centerX = -1.0
    centerY = 0.0
    rawFirstTilt = Radian 1.0
    rawSecondTilt = Radian 4.1
    rawFirstDistanceToCorner, rawSecondDistanceToCorner :: Positive ℝ
    rawFirstDistanceToCorner = 3.0
    rawSecondDistanceToCorner = 1.0e-3

-- | Tests that are expected to fail.
convexBisectableQuadBrokenSpec :: Spec
convexBisectableQuadBrokenSpec =
  describe "Convex Bisectable Quads" $ do
    it "does not find all ENodes intersect at a central point (rectangle-like quad?)" $
      unit_ConvexBisectableQuadENodeArcsIntersectAtSamePoint

-- | Tests that are expected to succeed.
convexBisectableQuadSpec :: Spec
convexBisectableQuadSpec = do
  describe "Convex Bisectable Quads" $ do
    it "finds no convex motorcycles" $
      property (expectationFromConvexBisectableQuad prop_NoMotorcycles)
    it "finds no divides" $
      property (expectationFromConvexBisectableQuad prop_NoDivides)
    it "finds no divides (unit)" $
      unit_ConvexBisectableQuadNoDivides
    it "finds a straight skeleton" $
      property (expectationFromConvexBisectableQuad prop_HasAStraightSkeleton)
    it "only finds one nodetree in the straight skeleton" $
      property (expectationFromConvexBisectableQuad prop_StraightSkeletonHasOneNodeTree)
    it "generates fewer than three generations of INodes" $
      property (boolFromConvexBisectableQuad prop_NodeTreeHasFewerThanThreeGenerations)
    it "finds that all of the outArcs of the ENodes intersect at the same point" $
      property (boolFromConvexBisectableQuad prop_ENodeArcsIntersectAtSamePoint)
    it "can place faces on the straight skeleton" $
      property (expectationFromConvexBisectableQuad prop_CanPlaceFaces)
    it "only places four faces" $
      property (expectationFromConvexBisectableQuad prop_HasFourFaces)
    it "faces have less than four sides" $
      property (boolFromConvexBisectableQuad prop_FacesHaveThreeSides)
    it "places faces in the order the line segments were given" $
      property (expectationFromConvexBisectableQuad prop_FacesInOrder)
    it "each face is wound to the left" $
      property (boolFromConvexBisectableQuad prop_FacesAllWoundLeft)
  where
    boolFromConvexBisectableQuad :: (Contour -> Bool) -> ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Bool
    boolFromConvexBisectableQuad f x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = f convexBisectableQuad
      where
        convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    expectationFromConvexBisectableQuad :: (Contour -> Expectation) -> ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
    expectationFromConvexBisectableQuad f x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = f convexBisectableQuad
      where
        convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
