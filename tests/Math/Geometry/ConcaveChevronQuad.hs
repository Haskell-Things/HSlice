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

-- Our Contour library.
import Graphics.Slicer.Math.Contour (lineSegsOfContour)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian, edgesOf, generationsOf, onlyOneOf, randomConcaveChevronQuad)

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
prop_ConcaveChevronQuadOneDivide a b c d e f = doTest $ randomConcaveChevronQuad a b c d e f
  where
    doTest concaveChevronQuad = len (slist $ findDivisions concaveChevronQuad (fromMaybe (error $ show concaveChevronQuad) $ crashMotorcycles concaveChevronQuad [])) --> 1

prop_ConcaveChevronQuadHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadHasStraightSkeleton a b c d e f = doTest $ randomConcaveChevronQuad a b c d e f
  where
    doTest concaveChevronQuad = findStraightSkeleton concaveChevronQuad [] -/> Nothing

prop_ConcaveChevronQuadStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadStraightSkeletonHasRightGenerationCount a b c d e f = doTest $ randomConcaveChevronQuad a b c d e f
  where
    doTest concaveChevronQuad = generationsOf (findStraightSkeleton concaveChevronQuad []) --> 1

prop_ConcaveChevronQuadCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadCanPlaceFaces a b c d e f = doTest $ randomConcaveChevronQuad a b c d e f
  where
    doTest concaveChevronQuad = facesOf (fromMaybe (error $ show concaveChevronQuad) $ findStraightSkeleton concaveChevronQuad []) -/> slist []

prop_ConcaveChevronQuadHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadHasRightFaceCount a b c d e f = doTest $ randomConcaveChevronQuad a b c d e f
  where
    doTest concaveChevronQuad = length (facesOf $ fromMaybe (error $ show concaveChevronQuad) $ findStraightSkeleton concaveChevronQuad []) --> 4

prop_ConcaveChevronQuadFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadFacesInOrder a b c d e f = doTest $ randomConcaveChevronQuad a b c d e f
  where
    doTest concaveChevronQuad = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show concaveChevronQuad) $ findStraightSkeleton concaveChevronQuad []) --> concaveChevronQuadAsSegs
      where
        concaveChevronQuadAsSegs = lineSegsOfContour concaveChevronQuad
        firstSeg = onlyOneOf concaveChevronQuadAsSegs


concaveChevronQuadSpec :: Spec
concaveChevronQuadSpec = do
  describe "Geometry (Concave Chevron Quads)" $ do
    it "places faces on the straight skeleton of a concave chevron quad" $
      property prop_ConcaveChevronQuadCanPlaceFaces
    it "finds only four faces for any concave chevron quad" $
      property prop_ConcaveChevronQuadHasRightFaceCount
    it "places faces on a concave chevron quad in the order the line segments were given" $
      property prop_ConcaveChevronQuadFacesInOrder
    it "finds one divide in a concave chevron quad" $
      property prop_ConcaveChevronQuadOneDivide
    it "finds the straight skeleton of a concave chevron quad (property)" $
      property prop_ConcaveChevronQuadHasStraightSkeleton
    it "only generates one generation for a concave chevron quad" $
      property prop_ConcaveChevronQuadStraightSkeletonHasRightGenerationCount
