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
  squareSpec
  ) where

import Prelude (Bool(True), Show(show), ($), (<), (.), (/), (+), (&&), (<>), (==), (<$>), all, error, length, otherwise)

-- The List library.
import Data.List (concat, head)

-- The Maybe library.
import Data.Maybe (fromMaybe, Maybe(Nothing))

-- Slists, a form of list with a stated size in the structure.
import Slist (len, slist)
import Slist.Type (Slist(Slist))

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive(Positive))
import Data.Coerce (coerce)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Our Contour library.
import Graphics.Slicer.Math.Contour (lineSegsOfContour)

-- Our debugging library, for making the below simpler to read, and drop into command lines.
import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian(Radian), edgesOf, generationsOf, randomSquare, onlyOneOf)

-- Our logic for dividing a contour into cells, which each get nodetrees for them, which are combined into a straight skeleton.
import Graphics.Slicer.Math.Skeleton.Cells (findDivisions)

-- The part of our library that puts faces onto a contour. faces have one exterior side, and a number of internal sides (defined by Arcs).
import Graphics.Slicer.Math.Skeleton.Face (Face(Face), facesOf, orderedFacesOf)

-- functions for placing lines segments onto faces.
import Graphics.Slicer.Math.Skeleton.Line (insetBy)

-- The portion of our library that reasons about motorcycles, emiting from the concave nodes of our contour.
import Graphics.Slicer.Math.Skeleton.Motorcycles (convexMotorcycles, crashMotorcycles)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->), (-/>))

prop_SquareNoConvexMotorcycles :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareNoConvexMotorcycles centerX centerY rawRadians rawDists = convexMotorcycles square --> []
  where
    square = randomSquare centerX centerY rawRadians rawDists

prop_SquareNoDivides :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareNoDivides x y tilt distanceToCorner = findDivisions square (fromMaybe (error $ show square) $ crashMotorcycles square []) --> []
  where
    square = randomSquare x y tilt distanceToCorner

prop_SquareHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareHasStraightSkeleton x y tilt distanceToCorner = findStraightSkeleton square [] -/> Nothing
  where
    square = randomSquare x y tilt distanceToCorner

prop_SquareStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareStraightSkeletonHasRightGenerationCount x y tilt distanceToCorner = generationsOf (findStraightSkeleton square []) --> 1
  where
    square = randomSquare x y tilt distanceToCorner

prop_SquareMotorcyclesIntersectAtPoint :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareMotorcyclesIntersectAtPoint x y tilt distanceToCorner = generationsOf (findStraightSkeleton square []) --> 1
  where
    square = randomSquare x y tilt distanceToCorner

prop_SquareCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareCanPlaceFaces x y tilt distanceToCorner = facesOf (fromMaybe (error $ show square) $ findStraightSkeleton square []) -/> slist []
  where
    square = randomSquare x y tilt distanceToCorner

prop_SquareHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareHasRightFaceCount x y tilt distanceToCorner = length (facesOf $ fromMaybe (error $ show square) $ findStraightSkeleton square []) --> 4
  where
    square = randomSquare x y tilt distanceToCorner

prop_SquareFacesRightArcCount :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Bool
prop_SquareFacesRightArcCount x y rawFirstTilt rawDistanceToCorner
  | res == True = True
  | otherwise = error $ "Too many arcs found:\n"
                     <> (concat $ show . arcCount <$> faces) <> "\n"
                     <> show skeleton <> "\n"
                     <> show faces <> "\n"
  where
    res = all (\a -> arcCount a < 4) faces
    faces = facesOf skeleton
    skeleton = fromMaybe (error $ show square) $ findStraightSkeleton square []
    arcCount (Face _ _ midArcs _) = 2 + len midArcs
    square = randomSquare x y rawFirstTilt rawDistanceToCorner

prop_SquareFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareFacesInOrder x y tilt distanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show square) $ findStraightSkeleton square []) --> squareAsSegs
  where
    square = randomSquare x y tilt distanceToCorner
    squareAsSegs = lineSegsOfContour square
    firstSeg = onlyOneOf squareAsSegs

prop_SquareFacesInsetWithRemainder :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Bool
prop_SquareFacesInsetWithRemainder x y tilt distanceToCorner
  | length insetContours == 1 && length (lineSegsOfContour insetContour) == 4 && length remainingFaces == 4 = True
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

-- | A unit test for a square that is interpreted as a really square looking rectangle.
unit_squareFromRandomSquare :: Bool
unit_squareFromRandomSquare
  | length insetContours == 1 && length (lineSegsOfContour insetContour) == 4 && length remainingFaces == 4 = True
  | otherwise = error $ "malformed result:\n"
                     <> dumpGanjas ([toGanja foundContour]
                                    <> (toGanja <$> (\(Slist a _) -> a) foundFaces)
                                    <> (toGanja <$> remainingFaces))
                     <> "insetContours: " <> show (length insetContours) <> "\n"
                     <> "contour segments: " <> show (length $ lineSegsOfContour insetContour) <> "\n"
                     <> "faces returned: " <> show (length remainingFaces) <> "\n"
                     <> "original contour: " <> show foundContour <> "\n"
                     <> "returned contour: " <> show insetContour <> "\n"
                     <> "returned faces: " <> show remainingFaces <> "\n"
  where
    insetContour = head insetContours
    (insetContours, remainingFaces) = insetBy (coerce distanceToCorner/2) foundFaces
    foundFaces = facesOf straightSkeleton
    straightSkeleton = fromMaybe (error $ show foundContour) $ findStraightSkeleton foundContour []
    foundContour = randomSquare x y tilt distanceToCorner
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

squareSpec :: Spec
squareSpec = do
  describe "Geometry (Squares)" $ do
    it "finds no convex motorcycles" $
      property prop_SquareNoConvexMotorcycles
    it "finds no divides" $
      property prop_SquareNoDivides
    it "finds a straight skeleton" $
      property prop_SquareHasStraightSkeleton
    it "only generates one generation of INodes" $
      property prop_SquareStraightSkeletonHasRightGenerationCount
    it "sees all of the faces intersecting in a point" $
      property prop_SquareMotorcyclesIntersectAtPoint
    it "can place faces on the straight skeleton" $
      property prop_SquareCanPlaceFaces
    it "only finds four faces" $
      property prop_SquareHasRightFaceCount
    it "faces have less than four sides" $
      property prop_SquareFacesRightArcCount
    it "places faces in the same order of the input line segments" $
      property prop_SquareFacesInOrder
    it "insets halfway, finding 4 remaining faces" $
      property prop_SquareFacesInsetWithRemainder
    it "insets a square that is detected by the code as a rectangle(unit)" $
      unit_squareFromRandomSquare
    it "insets completely, finding 0 remaining faces" $
      property prop_SquareFacesInsetWithoutRemainder
    it "insets a square that is detected by the code as a rectangle completely, finding 0 remaining faces(unit)" $
      unit_SquareFacesInsetWithoutRemainder
