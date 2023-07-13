{- ORMOLU_DISABLE -}
{- HSlice.
 - Copyright 2020-2023 Julia Longtin
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
    dualRightQuadFlakeySpec
  , dualRightQuadSpec
  ) where

import Prelude (Bool, ($), pure)

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Our common math functions.
import Graphics.Slicer.Math.Definitions (Contour)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian, randomDualRightQuad)

-- The portion of our library that reasons about motorcycles, emiting from the concave nodes of our contour.
import Graphics.Slicer.Math.Skeleton.Motorcycles (convexMotorcycles)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->))

-- Shared tests, between different geometry.
import Math.Geometry.CommonTests (prop_CanPlaceFaces, prop_FacesHaveThreeToFiveSides, prop_FacesInOrder, prop_HasFourFaces, prop_HasAStraightSkeleton, prop_NodeTreeHasFewerThanFourGenerations, prop_NoDivides, prop_NoMotorcycles, prop_StraightSkeletonHasOneNodeTree)

unit_DualRightQuadNoMotorcycles :: Expectation
unit_DualRightQuadNoMotorcycles = convexMotorcycles dualRightQuad --> []
  where
    dualRightQuad = randomDualRightQuad x y rawFirstTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    x,y :: ℝ
    x = 0
    y = 0
    rawFirstTilt :: Radian ℝ
    rawFirstTilt = 2.0
    rawFirstDistanceToCorner, rawSecondDistanceToCorner :: Positive ℝ
    rawFirstDistanceToCorner = 1.0
    rawSecondDistanceToCorner = 0.5

dualRightQuadFlakeySpec :: Spec
dualRightQuadFlakeySpec = pure ()

dualRightQuadSpec :: Spec
dualRightQuadSpec = do
  describe "Geometry (Dual Right Quads)" $ do
    it "finds no convex motorcycles (unit)"
      unit_DualRightQuadNoMotorcycles
    it "finds no convex motorcycles" $
      property (expectationFromDualRightQuad prop_NoMotorcycles)
    it "finds no divides" $
      property (expectationFromDualRightQuad prop_NoDivides)
    it "finds a straight skeleton" $
      property (expectationFromDualRightQuad prop_HasAStraightSkeleton)
    it "only finds one nodeTree in the straight skeleton" $
      property (expectationFromDualRightQuad prop_StraightSkeletonHasOneNodeTree)
    it "finds fewer than four generations in the nodeTree" $
      property (boolFromDualRightQuad prop_NodeTreeHasFewerThanFourGenerations)
    it "can place faces on the straight skeleton" $
      property (expectationFromDualRightQuad prop_CanPlaceFaces)
    it "finds only four faces" $
      property (expectationFromDualRightQuad prop_HasFourFaces)
    it "faces have between three and five sides" $
      property (boolFromDualRightQuad prop_FacesHaveThreeToFiveSides)
    it "places faces in the order the line segments were given" $
      property (expectationFromDualRightQuad prop_FacesInOrder)
  where
    boolFromDualRightQuad :: (Contour -> Bool) -> ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Bool
    boolFromDualRightQuad f x y rawFirstTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = f dualRightQuad
      where
        dualRightQuad = randomDualRightQuad x y rawFirstTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    expectationFromDualRightQuad :: (Contour -> Expectation) -> ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
    expectationFromDualRightQuad f x y rawFirstTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = f dualRightQuad
      where
        dualRightQuad = randomDualRightQuad x y rawFirstTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
