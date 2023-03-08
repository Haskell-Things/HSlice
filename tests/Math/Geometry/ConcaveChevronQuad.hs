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

import Prelude (Show(show), ($), error, length)

-- The Maybe library.
import Data.Maybe (fromMaybe, Maybe(Nothing))

-- Slists, a form of list with a stated size in the structure.
import Slist (slist, len)

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Our basic math library.
import Graphics.Slicer.Math.Definitions (lineSegsOfContour)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian(Radian), edgesOf, generationsOf, nodeTreesOf, oneNodeTreeOf, onlyOneOf, randomConcaveChevronQuad)

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

prop_ConcaveChevronQuadOneDivide :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadOneDivide x y tilt1 tilt2 distance1 distance2 = doTest $ randomConcaveChevronQuad x y tilt1 tilt2 distance1 distance2
  where
    doTest concaveChevronQuad = len (slist $ findDivisions concaveChevronQuad (fromMaybe (error $ show concaveChevronQuad) $ crashMotorcycles concaveChevronQuad [])) --> 1

prop_ConcaveChevronQuadHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadHasStraightSkeleton x y tilt1 tilt2 distance1 distance2 = doTest $ randomConcaveChevronQuad x y tilt1 tilt2 distance1 distance2
  where
    doTest concaveChevronQuad = findStraightSkeleton concaveChevronQuad [] -/> Nothing

prop_ConcaveChevronQuadStraightSkeletonHasOneNodeTree :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadStraightSkeletonHasOneNodeTree x y tilt1 tilt2 distance1 distance2 = doTest $ randomConcaveChevronQuad x y tilt1 tilt2 distance1 distance2
  where
    doTest concaveChevronQuad = nodeTreesOf (findStraightSkeleton concaveChevronQuad []) --> 1

prop_ConcaveChevronQuadNodeTreeHasTwoGenerations :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadNodeTreeHasTwoGenerations x y tilt1 tilt2 distance1 distance2 = doTest $ randomConcaveChevronQuad x y tilt1 tilt2 distance1 distance2
  where
    doTest concaveChevronQuad = generationsOf (oneNodeTreeOf $ fromMaybe (error "no straight skeleton?") $ findStraightSkeleton concaveChevronQuad []) --> 2

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

prop_ConcaveChevronQuadCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadCanPlaceFaces x y tilt1 tilt2 distance1 distance2 = doTest $ randomConcaveChevronQuad x y tilt1 tilt2 distance1 distance2
  where
    doTest concaveChevronQuad = facesOf (fromMaybe (error $ show concaveChevronQuad) $ findStraightSkeleton concaveChevronQuad []) -/> slist []

prop_ConcaveChevronQuadHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadHasRightFaceCount x y tilt1 tilt2 distance1 distance2 = doTest $ randomConcaveChevronQuad x y tilt1 tilt2 distance1 distance2
  where
    doTest concaveChevronQuad = length (facesOf $ fromMaybe (error $ show concaveChevronQuad) $ findStraightSkeleton concaveChevronQuad []) --> 4

prop_ConcaveChevronQuadFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadFacesInOrder x y tilt1 tilt2 distance1 distance2 = doTest $ randomConcaveChevronQuad x y tilt1 tilt2 distance1 distance2
  where
    doTest concaveChevronQuad = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show concaveChevronQuad) $ findStraightSkeleton concaveChevronQuad []) --> concaveChevronQuadAsSegs
      where
        concaveChevronQuadAsSegs = lineSegsOfContour concaveChevronQuad
        firstSeg = onlyOneOf concaveChevronQuadAsSegs

concaveChevronQuadBrokenSpec :: Spec
concaveChevronQuadBrokenSpec = do
  describe "Geometry (Concave Chevron Quads)" $
    it "generates two generations of INodes(unit)" $
      unit_ConcaveChevronQuadNodeTreeHasTwoGenerations

concaveChevronQuadSpec :: Spec
concaveChevronQuadSpec = do
  describe "Geometry (Concave Chevron Quads)" $ do
    it "finds a straight skeleton" $
      property prop_ConcaveChevronQuadHasStraightSkeleton
    it "finds only one nodetree in the straight skeleton" $
      property prop_ConcaveChevronQuadStraightSkeletonHasOneNodeTree
    it "generates two generations of INodes" $
      property prop_ConcaveChevronQuadNodeTreeHasTwoGenerations
    it "places faces on the straight skeleton" $
      property prop_ConcaveChevronQuadCanPlaceFaces
    it "finds only four faces for any concave chevron quad" $
      property prop_ConcaveChevronQuadHasRightFaceCount
    it "places faces on a concave chevron quad in the order the line segments were given" $
      property prop_ConcaveChevronQuadFacesInOrder
    it "finds one divide in a concave chevron quad" $
      property prop_ConcaveChevronQuadOneDivide
