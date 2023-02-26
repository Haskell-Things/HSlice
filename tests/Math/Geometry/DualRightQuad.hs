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

module Math.Geometry.DualRightQuad (
  dualRightQuadSpec
  ) where

import Prelude (Show(show), ($), (<>), error, length)

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

-- Our debugging library, for making the below simpler to read, and drop into command lines.
import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian, edgesOf, generationsOf, randomDualRightQuad, onlyOneOf)

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


prop_DualRightQuadNoDivides :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_DualRightQuadNoDivides x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner = findDivisions dualRightQuad (fromMaybe (errorReport) $ crashMotorcycles dualRightQuad []) --> []
  where
    dualRightQuad = randomDualRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner
    errorReport = error $ "failed to generate a motorcycle crash report.\n"
                        <> dumpGanjas [toGanja dualRightQuad] <> "\n"

prop_DualRightQuadHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_DualRightQuadHasStraightSkeleton x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner = findStraightSkeleton dualRightQuad [] -/> Nothing
  where
    dualRightQuad = randomDualRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner

prop_DualRightQuadStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_DualRightQuadStraightSkeletonHasRightGenerationCount x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner = generationsOf (findStraightSkeleton dualRightQuad []) --> 1
  where
    dualRightQuad = randomDualRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner

prop_DualRightQuadCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_DualRightQuadCanPlaceFaces x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner = facesOf (fromMaybe (error $ show dualRightQuad) $ findStraightSkeleton dualRightQuad []) -/> slist []
  where
    dualRightQuad = randomDualRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner

prop_DualRightQuadHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_DualRightQuadHasRightFaceCount x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner = length (facesOf $ fromMaybe (error $ show dualRightQuad) $ findStraightSkeleton dualRightQuad []) --> 4
  where
    dualRightQuad = randomDualRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner

prop_DualRightQuadFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_DualRightQuadFacesInOrder x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show dualRightQuad) $ findStraightSkeleton dualRightQuad []) --> dualRightQuadAsSegs
  where
    dualRightQuad = randomDualRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner
    dualRightQuadAsSegs = lineSegsOfContour dualRightQuad
    firstSeg = onlyOneOf dualRightQuadAsSegs

dualRightQuadSpec :: Spec
dualRightQuadSpec = do
  describe "Geometry (Dual Right Quads)" $ do
    it "finds no divides in a  dual right quad" $
      property prop_DualRightQuadNoDivides
    it "finds the straight skeleton of a  dual right quad (property)" $
      property prop_DualRightQuadHasStraightSkeleton
    it "only generates one generation for a  dual right quad" $
      property prop_DualRightQuadStraightSkeletonHasRightGenerationCount
    it "places faces on the straight skeleton of a  dual right quad" $
      property prop_DualRightQuadCanPlaceFaces
    it "finds only four faces for any  dual right quad" $
      property prop_DualRightQuadHasRightFaceCount
    it "places faces on a dual right quad in the order the line segments were given" $
      property prop_DualRightQuadFacesInOrder
