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

{- tests for the properties of a rectangle. -}

module Math.Geometry.Rectangle (
  rectangleBrokenSpec,
  rectangleSpec
  ) where

import Prelude (Bool(False, True), Show(show), ($), (*), (<), (/), (-), (+), (&&), (<>), (==), (||),  (<$>), cos, error, length, min, otherwise, pi, sin)

-- The List library.
import Data.List (concat, intersperse, sort)

-- The Maybe library.
import Data.Maybe (fromMaybe)

-- Slists, a form of list with a stated size in the structure.
import Slist (slist)
import Slist.Type (Slist(Slist))

-- Hspec, for writing specs.
import Test.Hspec (Expectation, Spec, describe, it)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive(Positive))
import Data.Coerce (coerce)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Basic math functions.
import Graphics.Slicer.Math.Definitions (Contour, lineSegsOfContour)

-- Basic intersection logic.
import Graphics.Slicer.Math.Intersections (intersectionsAtSamePoint)

-- The Projective Geometry library.
import Graphics.Slicer.Math.PGA (outAndErrOf)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian(Radian), generationsOf, oneNodeTreeOf, randomRectangle)

-- The logic for creating straight skeletons from concave contours.
import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour, skeletonOfNodes)

-- The part of our library that puts faces onto a contour. faces have one exterior side, and a number of internal sides (defined by Arcs).
import Graphics.Slicer.Math.Skeleton.Face (facesOf)

-- functions for placing lines segments onto faces.
import Graphics.Slicer.Math.Skeleton.Line (insetBy)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->))

-- Shared tests, between different geometry.
import Math.Geometry.CommonTests (prop_CanPlaceFaces, prop_FacesHaveThreeToFiveSides, prop_FacesAllWoundLeft, prop_FacesInOrder, prop_HasFourFaces, prop_HasAStraightSkeleton, prop_InsetIsSmaller, prop_NodeTreeHasFewerThanThreeGenerations, prop_NoDivides, prop_NoMotorcycles, prop_StraightSkeletonHasOneNodeTree)

-- | Ensure that we only place four races on the given contour.
unit_HasFourFaces :: Bool
unit_HasFourFaces
 | length res == 4 = True
 | otherwise = error $ "wrong number of faces: " <> show (length res) <> "\n"
                    <> concat (intersperse "\n" ((\(Slist a _) -> a) $ show <$> res)) <> "\n"
                    <> show straightSkeleton <> "\n"
  where
    res = facesOf straightSkeleton
    straightSkeleton = fromMaybe (error $ show contour) $ findStraightSkeleton contour []
    contour = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner
    x,y :: ℝ
    x = 0
    y = 0
    rawFirstTilt = Radian 0.5
    rawSecondTilt = Radian 3.4
    rawDistanceToCorner :: Positive ℝ
    rawDistanceToCorner = 5.67

unit_RectangleNodeTreeHasLessThanThreeGenerations :: Bool
unit_RectangleNodeTreeHasLessThanThreeGenerations
  | res < 3 = True
  | otherwise = error $ "fail!\n"
                     <> "generations: " <> show res <> "\n"
                     <> "skeleton: " <> show (findStraightSkeleton rectangle []) <> "\n"
                     <> "raw Skeleton: " <> show (skeletonOfNodes True lineSegs lineSegs []) <> "\n"
                     <> "faces: " <> show (facesOf $ fromMaybe (error "no") $ findStraightSkeleton rectangle []) <> "\n"
  where
    res = generationsOf (oneNodeTreeOf $ fromMaybe (error "No straight skeleton?") $ findStraightSkeleton rectangle [])
    lineSegs = slist [lineSegsOfContour rectangle]
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner
    x,y :: ℝ
    x = 0
    y = -1.0
    rawFirstTilt = Radian 4.802
    rawSecondTilt = Radian 1.65993
    rawDistanceToCorner :: Positive ℝ
    rawDistanceToCorner = 28.0

prop_RectangleMotorcyclesDoNotIntersectAtPoint :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleMotorcyclesDoNotIntersectAtPoint x y rawFirstTilt rawSecondTilt distanceToCorner = intersectionsAtSamePoint nodeOutsAndErrs --> False
  where
    nodeOutsAndErrs = outAndErrOf <$> eNodes
    eNodes = eNodesOfOutsideContour rectangle
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt distanceToCorner

prop_RectangleFacesInsetWithRemainder :: Contour -> Positive ℝ -> Bool
prop_RectangleFacesInsetWithRemainder contour maxInsetDistance = length faces == 4
  where
    faces = facesOf $ fromMaybe (error $ show insetContour) $ findStraightSkeleton insetContour []
    ([insetContour],_) = insetBy (coerce $ maxInsetDistance/2) $ facesOf $ fromMaybe (error $ show contour) $ findStraightSkeleton contour []

prop_RectangleFacesInsetSmallerThanRectangle :: Contour -> Positive ℝ -> Bool
prop_RectangleFacesInsetSmallerThanRectangle contour maxInsetDistance = prop_InsetIsSmaller (coerce $ maxInsetDistance/2) contour

prop_RectangleFacesInsetWithoutRemainder :: Contour -> Positive ℝ -> Bool
prop_RectangleFacesInsetWithoutRemainder contour distanceToCorner
  | (length insetContours == 0 && length remainingFaces == 0) ||
    (length insetContours == 0 && length remainingFaces == 4) = True
    -- FIXME: these inset contours are so small, they can cause contour containing contour functions to fail.
  | (length insetContours == 1 && length remainingFaces == 0) ||
    (length insetContours == 1 && length remainingFaces == 4) = True -- prop_InsetIsSmaller (coerce distanceToCorner) contour
  | otherwise = error $ "fail!\n" <> "numInsetContours: " <> show (length insetContours) <> "\nnumRemainingFaces: " <> show (length remainingFaces) <> "\n" <> show insetContours <> "\n" <> show remainingFaces <> "\n"
  where
    (insetContours, remainingFaces) = insetBy (coerce distanceToCorner) (facesOf $ fromMaybe (error $ show contour) $ findStraightSkeleton contour [])

rectangleBrokenSpec :: Spec
rectangleBrokenSpec = do
  describe "Rectangles" $ do
    it "only generates one, or two generations of INodes" $
      unit_RectangleNodeTreeHasLessThanThreeGenerations

rectangleSpec :: Spec
rectangleSpec = do
  describe "Rectangles" $ do
    it "finds no convex motorcycles" $
      property (expectationFromRectangle prop_NoMotorcycles)
    it "finds no divides" $
      property (expectationFromRectangle prop_NoDivides)
    it "finds a straight skeleton" $
      property (expectationFromRectangle prop_HasAStraightSkeleton)
    it "only has one Nodetree in the straight skeleton" $
      property (expectationFromRectangle prop_StraightSkeletonHasOneNodeTree)
    it "only generates one, or two generations of INodes" $
      property (boolFromRectangle prop_NodeTreeHasFewerThanThreeGenerations)
    it "does not consider a rectangle to be square (outArcs do not intersect)" $
      property prop_RectangleMotorcyclesDoNotIntersectAtPoint
    it "can place faces on the straight skeleton" $
      property (expectationFromRectangle prop_CanPlaceFaces)
    it "only places four faces" $
      property (expectationFromRectangle prop_HasFourFaces)
    it "only places four faces (unit)" $
      property unit_HasFourFaces
    it "faces have between three and five sides" $
      property (boolFromRectangle prop_FacesHaveThreeToFiveSides)
    it "places faces in the same order the line segments were given in" $
      property (expectationFromRectangle prop_FacesInOrder)
    it "each face is wound to the left" $
      property (boolFromRectangle prop_FacesAllWoundLeft)
    it "insets halfway, finding 4 remaining faces" $
      property (boolFromRectangleWithDistance prop_RectangleFacesInsetWithRemainder)
    it "insets completely, does not crash" $
      property (boolFromRectangleWithDistance prop_RectangleFacesInsetWithoutRemainder)
    it "sees an inset of a rectangle as being smaller than the source rectangle" $
      property (boolFromRectangleWithDistance prop_RectangleFacesInsetSmallerThanRectangle)
    where
      boolFromRectangle :: (Contour -> Bool) -> ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Bool
      boolFromRectangle f x y rawFirstTilt rawSecondTilt distanceToCorner = f rectangle
        where
          rectangle = randomRectangle x y rawFirstTilt rawSecondTilt distanceToCorner
      boolFromRectangleWithDistance :: (Contour -> Positive ℝ -> Bool) -> ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Bool
      boolFromRectangleWithDistance f x y rawFirstTilt rawSecondTilt distanceToCorner = f rectangle maxInsetDistance
        where
          maxInsetDistance = Positive $ min a b
          a, b, c, theta :: ℝ
          a = c * sin theta
          b = c * cos theta
          theta = coerce $ (tilt2 - tilt1) / 2
          -- find the two closest together lines from the center point to the corners.
          (tilt1, tilt2)
            | r2 - r1 < r3 - r2 = (r1, r2)
            | otherwise = (r2, r3)
          [r1, r2, r3, _] = sort
            [
              firstTilt
            , rawSecondTilt
            , flipRadian firstTilt
            , flipRadian rawSecondTilt
            ]
          firstTilt
            | rawFirstTilt == rawSecondTilt = rawFirstTilt + rawSecondTilt
            | otherwise = rawFirstTilt
          flipRadian :: Radian ℝ -> Radian ℝ
          flipRadian v
            | v < Radian pi = v + Radian pi
            | otherwise     = v - Radian pi
          c = coerce distanceToCorner
          rectangle = randomRectangle x y rawFirstTilt rawSecondTilt distanceToCorner
          
      expectationFromRectangle :: (Contour -> Expectation) -> ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
      expectationFromRectangle f x y rawFirstTilt rawSecondTilt distanceToCorner = f rectangle
        where
          rectangle = randomRectangle x y rawFirstTilt rawSecondTilt distanceToCorner
