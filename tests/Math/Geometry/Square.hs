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

{- tests for the properties of a square. -}

module Math.Geometry.Square (
  squareBrokenSpec,
  squareSpec
  ) where

import Prelude (Bool(True), Show(show), (<), ($), (/), (<>), (<$>), error, length, otherwise)

-- The List library.
import Data.List (head)

-- The Maybe library.
import Data.Maybe (fromMaybe)

-- Slists, a form of list with a stated size in the structure.
import Slist.Type (Slist(Slist))

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive(Positive))
import Data.Coerce (coerce)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Assorted basic math functions
import Graphics.Slicer.Math.Definitions (Contour, lineSegsOfContour)

-- Our debugging library, for making the below simpler to read, and drop into command lines.
import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

-- Basic intersection logic.
import Graphics.Slicer.Math.Intersections (intersectionsAtSamePoint)

-- Our 2D Projective Geometric Algebra library.
import Graphics.Slicer.Math.PGA (outAndErrOf)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian(Radian), generationsOf, oneNodeTreeOf, randomSquare)

-- The logic for creating straight skeletons from concave contours.
import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour)

-- The part of our library that puts faces onto a contour. faces have one exterior side, and a number of internal sides (defined by Arcs).
import Graphics.Slicer.Math.Skeleton.Face (facesOf)

-- functions for placing lines segments onto faces.
import Graphics.Slicer.Math.Skeleton.Line (insetBy)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Shared tests, between different geometry.
import Math.Geometry.CommonTests (prop_CanPlaceFaces, prop_ENodeArcsIntersectAtSamePoint, prop_FacesAllWoundLeft, prop_FacesHaveThreeSides, prop_FacesInOrder, prop_HasFourFaces, prop_HasAStraightSkeleton, prop_InsetIsSmaller, prop_InsetOfInsetIsSmaller, prop_NodeTreeHasFewerThanThreeGenerations, prop_NoDivides, prop_NoMotorcycles, prop_StraightSkeletonHasOneNodeTree)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->))

-- | Fails to see a square as having a center point.
unit_SquareENodeArcsIntersectAtSamePoint :: Bool
unit_SquareENodeArcsIntersectAtSamePoint = retVal
  where
    retVal = intersectionsAtSamePoint nodeOutsAndErrs
    nodeOutsAndErrs = outAndErrOf <$> eNodes
    eNodes = eNodesOfOutsideContour square
    square = randomSquare centerX centerY tilt distanceToCorner
    centerX,centerY :: ℝ
    centerX = -2.0
    centerY = 13.0
    tilt = Radian 0.1
    distanceToCorner :: Positive ℝ
    distanceToCorner = 3.0e-2

prop_SquareFacesInsetWithRemainder :: Contour -> Positive ℝ -> Bool
prop_SquareFacesInsetWithRemainder contour distanceToCorner = prop_InsetIsSmaller (coerce insetDistance) contour
  where
    insetDistance = distanceToCorner / 2

unit_SquareFacesInsetWithRemainder :: Bool
unit_SquareFacesInsetWithRemainder
  | prop_InsetIsSmaller (coerce $ distanceToCorner / 2) square = True
  | otherwise = error $ "whoops!\n"
                     <> "insetContours: " <> show (length insetContours) <> "\n"
                     <> "contour segments: " <> show (length $ lineSegsOfContour insetContour) <> "\n"
                     <> "faces returned: " <> show (length remainingFaces) <> "\n"
                     <> "original contour: " <> show square <> "\n"
                     <> "returned contour: " <> show insetContour <> "\n"
                     <> "returned faces: " <> show remainingFaces <> "\n"
  where
    insetContour = head insetContours
    (insetContours, remainingFaces) = insetBy (coerce distanceToCorner/2) (facesOf $ fromMaybe (error $ show square) $ findStraightSkeleton square [])
    square = randomSquare x y tilt distanceToCorner
    x,y :: ℝ
    x = -1.0
    y = 0.6
    distanceToCorner :: Positive ℝ
    distanceToCorner = 1.0e-4
    tilt = Radian 1.0

prop_SquareFacesInsetOfInsetWithRemainder :: Contour -> Positive ℝ -> Bool
prop_SquareFacesInsetOfInsetWithRemainder contour distanceToCorner = prop_InsetOfInsetIsSmaller (coerce insetDistance1) (coerce insetDistance2) contour
  where
    -- FIXME: this breaks with /2.
    insetDistance1 = distanceToCorner / 3
    insetDistance2 = distanceToCorner / 4

-- | A unit test for a square that is interpreted as a really square looking rectangle.
unit_squareFromRandomSquare :: Bool
unit_squareFromRandomSquare
  | prop_InsetIsSmaller (coerce $ distanceToCorner / 2) contour = True
  | otherwise = error $ "malformed result:\n"
                     <> dumpGanjas ([toGanja contour]
                                    <> (toGanja <$> (\(Slist a _) -> a) foundFaces)
                                    <> (toGanja <$> remainingFaces))
                     <> "insetContours: " <> show (length insetContours) <> "\n"
                     <> "contour segments: " <> show (length $ lineSegsOfContour insetContour) <> "\n"
                     <> "faces returned: " <> show (length remainingFaces) <> "\n"
                     <> "original contour: " <> show contour <> "\n"
                     <> "returned contour: " <> show insetContour <> "\n"
                     <> "returned faces: " <> show remainingFaces <> "\n"
  where
    insetContour = head insetContours
    (insetContours, remainingFaces) = insetBy (coerce distanceToCorner/2) foundFaces
    foundFaces = facesOf straightSkeleton
    straightSkeleton = fromMaybe (error $ show contour) $ findStraightSkeleton contour []
    contour = randomSquare x y tilt distanceToCorner
    x,y :: ℝ
    x = 0
    y = 0
    distanceToCorner :: Positive ℝ
    distanceToCorner = 35
    tilt = Radian 2.0

prop_SquareFacesInsetWithoutRemainder :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareFacesInsetWithoutRemainder x y tilt distanceToCorner = (length insetContours, length remainingFaces) --> (0, 0)
  where
    (insetContours, remainingFaces) = insetBy (coerce distanceToCorner) (facesOf $ fromMaybe (error $ show square) $ findStraightSkeleton square [])
    square = randomSquare x y tilt distanceToCorner

unit_SquareFacesInsetWithoutRemainder :: Expectation
unit_SquareFacesInsetWithoutRemainder = (length insetContours, length remainingFaces) --> (0, 0)
  where
    (insetContours, remainingFaces) = insetBy (coerce distanceToCorner) (facesOf $ fromMaybe (error $ show square) $ findStraightSkeleton square [])
    square = randomSquare x y tilt distanceToCorner
    x,y :: ℝ
    x = -0.3
    y = -1.0
    tilt = Radian 0.5
    distanceToCorner :: Positive ℝ
    distanceToCorner = Positive 2.0e-3

-- | Failed to find a reference when sorting PLines. Fixed.
unit_SquareFacesFewerThanThreeGenerations :: Bool
unit_SquareFacesFewerThanThreeGenerations = generationsOf (oneNodeTreeOf $ fromMaybe (error "no straight skeleton?") $ findStraightSkeleton square []) < 3
  where
    square = randomSquare x y tilt distanceToCorner
    x,y :: ℝ
    x = 0.2
    y = 0.0
    tilt = Radian 1.5
    distanceToCorner :: Positive ℝ
    distanceToCorner = Positive 1.0

squareBrokenSpec :: Spec
squareBrokenSpec = do
  describe "Squares" $
    it "finds that all of the outArcs of the ENodes intersect at the same point"
      unit_SquareENodeArcsIntersectAtSamePoint

squareSpec :: Spec
squareSpec = do
  describe "Squares" $ do
    it "finds no convex motorcycles" $
      property (expectationFromSquare prop_NoMotorcycles)
    it "finds no divides" $
      property (expectationFromSquare prop_NoDivides)
    it "finds a straight skeleton" $
      property (expectationFromSquare prop_HasAStraightSkeleton)
    it "finds one Nodetree in the straight skeleton" $
      property (expectationFromSquare prop_StraightSkeletonHasOneNodeTree)
    it "has fewer than three generations of INodes in the NodeTree" $
      property (boolFromSquare prop_NodeTreeHasFewerThanThreeGenerations)
    it "has fewer than three generations of INodes in the NodeTree (unit)" $
      unit_SquareFacesFewerThanThreeGenerations
    it "finds that all of the outArcs of the ENodes intersect at the same point" $
      property (boolFromSquare prop_ENodeArcsIntersectAtSamePoint)
    it "can place faces on the straight skeleton" $
      property (expectationFromSquare prop_CanPlaceFaces)
    it "only places four faces" $
      property (expectationFromSquare prop_HasFourFaces)
    it "faces have three sides" $
      property (boolFromSquare prop_FacesHaveThreeSides)
    it "places faces in the same order of the input line segments" $
      property (expectationFromSquare prop_FacesInOrder)
    it "each face is wound to the left" $
      property (boolFromSquare prop_FacesAllWoundLeft)
    it "insets halfway, finding a smaller inset" $
      property (boolFromSquareWithDistance prop_SquareFacesInsetWithRemainder)
    it "insets halfway, finding a smaller inset(unit)"
      unit_SquareFacesInsetWithRemainder
    it "insets a square that is detected by the code as a rectangle(unit)"
      unit_squareFromRandomSquare
    it "insets halfway then a quarter way again, finding a smaller inset" $
      property (boolFromSquareWithDistance prop_SquareFacesInsetOfInsetWithRemainder)
    it "insets completely, finding 0 remaining faces" $
      property prop_SquareFacesInsetWithoutRemainder
    it "insets a square that is detected by the code as a rectangle completely, finding 0 remaining faces(unit)"
      unit_SquareFacesInsetWithoutRemainder
  where
    boolFromSquare :: (Contour -> Bool) -> ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Bool
    boolFromSquare f x y tilt distanceToCorner = f square
      where
        square = randomSquare x y tilt distanceToCorner
    boolFromSquareWithDistance :: (Contour -> Positive ℝ -> Bool) -> ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Bool
    boolFromSquareWithDistance f x y tilt distanceToCorner = f square distanceToCorner
      where
        square = randomSquare x y tilt distanceToCorner
    expectationFromSquare :: (Contour -> Expectation) -> ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
    expectationFromSquare f x y tilt distanceToCorner = f square
      where
        square = randomSquare x y tilt distanceToCorner

