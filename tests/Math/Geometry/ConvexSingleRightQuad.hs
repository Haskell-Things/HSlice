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

module Math.Geometry.ConvexSingleRightQuad (
  convexSingleRightQuadBrokenSpec,
  convexSingleRightQuadSpec
  ) where

import Prelude (Bool(False, True), Show(show), ($), (<), (&&), (==), error, length, otherwise)

-- The Maybe library.
import Data.Maybe (Maybe(Nothing), fromMaybe, isJust)

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
import Graphics.Slicer.Math.RandomGeometry (Radian, edgesOf, generationsOf, nodeTreesOf, oneNodeTreeOf, onlyOneOf, randomConvexSingleRightQuad)

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

prop_SingleRightQuadConvexNoDivides :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_SingleRightQuadConvexNoDivides x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = findDivisions convexSingleRightQuad (fromMaybe (error $ show convexSingleRightQuad) $ crashMotorcycles convexSingleRightQuad []) --> []
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_SingleRightQuadConvexHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_SingleRightQuadConvexHasStraightSkeleton x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = findStraightSkeleton convexSingleRightQuad [] -/> Nothing
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

unit_SingleRightQuadConvexHasNoStraightSkeleton :: Bool
unit_SingleRightQuadConvexHasNoStraightSkeleton
  | isJust skeleton = True
  | otherwise = False
  where
    skeleton = findStraightSkeleton convexSingleRightQuad []
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    x,y :: ℝ
    x = 0
    y = 0
    rawFirstTilt, rawSecondTilt, rawThirdTilt :: Radian ℝ
    rawFirstTilt = 2.0
    rawSecondTilt = 2.0
    rawThirdTilt = 3.2
    rawFirstDistanceToCorner, rawSecondDistanceToCorner :: Positive ℝ
    rawFirstDistanceToCorner = 2.0
    rawSecondDistanceToCorner = 4.0

unit_SingleRightQuadConvexStraightSkeletonBreaks :: Bool
unit_SingleRightQuadConvexStraightSkeletonBreaks
  | isJust skeleton && length (facesOf $ fromMaybe (error "whoops!") $ skeleton) == 4  = True
  | otherwise = False
  where
    skeleton = findStraightSkeleton convexSingleRightQuad []
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    x,y :: ℝ
    x = 0
    y = 0
    rawFirstTilt, rawSecondTilt, rawThirdTilt :: Radian ℝ
    rawFirstTilt = 3.0
    rawSecondTilt = 3.0
    rawThirdTilt = 3.0
    rawFirstDistanceToCorner, rawSecondDistanceToCorner :: Positive ℝ
    rawFirstDistanceToCorner = 1.0
    rawSecondDistanceToCorner = 1.0

prop_SingleRightQuadConvexStraightSkeletonHasOneNodeTree :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_SingleRightQuadConvexStraightSkeletonHasOneNodeTree x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = nodeTreesOf (findStraightSkeleton convexSingleRightQuad []) --> 1
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_SingleRightQuadConvexNodeTreeHasLessThanFourGenerations :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_SingleRightQuadConvexNodeTreeHasLessThanFourGenerations x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = generationsOf (oneNodeTreeOf $ fromMaybe (error "no straight skeleton?") $ findStraightSkeleton convexSingleRightQuad []) < 4
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_SingleRightQuadConvexCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_SingleRightQuadConvexCanPlaceFaces x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = facesOf (fromMaybe (error $ show convexSingleRightQuad) $ findStraightSkeleton convexSingleRightQuad []) -/> slist []
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_SingleRightQuadConvexHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_SingleRightQuadConvexHasRightFaceCount x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = length (facesOf $ fromMaybe (error $ show convexSingleRightQuad) $ findStraightSkeleton convexSingleRightQuad []) --> 4
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_SingleRightQuadConvexFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_SingleRightQuadConvexFacesInOrder x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show convexSingleRightQuad) $ findStraightSkeleton convexSingleRightQuad []) --> convexSingleRightQuadAsSegs
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    convexSingleRightQuadAsSegs = lineSegsOfContour convexSingleRightQuad
    firstSeg = onlyOneOf convexSingleRightQuadAsSegs

convexSingleRightQuadBrokenSpec :: Spec
convexSingleRightQuadBrokenSpec = do
  describe "geometry (Convex Single Right Quads)" $ do
    it "finds a straight skeleton(unit)" $
      unit_SingleRightQuadConvexHasNoStraightSkeleton
    it "finds a straight skeleton(unit 2)" $
      unit_SingleRightQuadConvexStraightSkeletonBreaks

convexSingleRightQuadSpec :: Spec
convexSingleRightQuadSpec = do
  describe "Geometry (Single Right Quads, Convex)" $ do
    it "finds no divides" $
      property prop_SingleRightQuadConvexNoDivides
    it "finds a straight skeleton" $
      property prop_SingleRightQuadConvexHasStraightSkeleton
    it "only finds one nodeTree in the straight skeleton" $
      property prop_SingleRightQuadConvexStraightSkeletonHasOneNodeTree
    it "generates less than four generations" $
      property prop_SingleRightQuadConvexNodeTreeHasLessThanFourGenerations
    it "places faces on the straight skeleton of a convex single right quad" $
      property prop_SingleRightQuadConvexCanPlaceFaces
    it "finds only four faces for any convex single right quad" $
      property prop_SingleRightQuadConvexHasRightFaceCount
    it "places faces on a convex single right quad in the order the line segments were given" $
      property prop_SingleRightQuadConvexFacesInOrder
