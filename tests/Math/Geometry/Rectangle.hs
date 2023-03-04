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

import Prelude (Bool(False, True), Show(show), ($), (*), (<), (>), (.), (/), (-), (+), (&&), (<>), (<=), (==), (<$>), all, cos, error, length, min, otherwise, pi, sin)

-- The List library.
import Data.List (concat, head, sort)

-- The Maybe library.
import Data.Maybe (fromMaybe, Maybe(Just, Nothing))

-- Slists, a form of list with a stated size in the structure.
import Slist (len, slist)
import Slist.Type (Slist(Slist))

-- Hspec, for writing specs.
import Test.Hspec (Expectation, Spec, describe, it)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive(Positive))
import Data.Coerce (coerce)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

--
import Graphics.Slicer.Math.Definitions(Point2(Point2), mapWithFollower, minMaxPoints)

-- Our Contour library.
import Graphics.Slicer.Math.Contour (contourContainsContour, lineSegsOfContour)

-- Our debugging library, for making the below simpler to read, and drop into command lines.
import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

-- Basic intersection logic.
import Graphics.Slicer.Math.Intersections (intersectionsAtSamePoint)

-- The Projective Geometry library.
import Graphics.Slicer.Math.PGA (eToPL, outAndErrOf, pLineIsLeft)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian(Radian), edgesOf, generationsOf, nodeTreesOf, onlyOneOf, oneNodeTreeOf, randomRectangle)

-- Our logic for dividing a contour into cells, which each get nodetrees for them, which are combined into a straight skeleton.
import Graphics.Slicer.Math.Skeleton.Cells (findDivisions)

-- The logic for creating straight skeletons from concave contours.
import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour, skeletonOfNodes)

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

prop_RectangleNoConvexMotorcycles :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleNoConvexMotorcycles centerX centerY rawFirstTilt rawSecondTilt rawDistanceToCorner = convexMotorcycles rectangle --> []
  where
    rectangle = randomRectangle centerX centerY rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleNoDivides :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleNoDivides x y rawFirstTilt rawSecondTilt rawDistanceToCorner = findDivisions rectangle (fromMaybe (error $ show rectangle) $ crashMotorcycles rectangle []) --> []
  where
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleHasStraightSkeleton x y rawFirstTilt rawSecondTilt rawDistanceToCorner = findStraightSkeleton rectangle [] -/> Nothing
  where
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleStraightSkeletonHasOneNodeTree :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Bool
prop_RectangleStraightSkeletonHasOneNodeTree x y rawFirstTilt rawSecondTilt rawDistanceToCorner
  | res == 1 = True
  | otherwise = error $ "fail!\n"
                     <> "generations: " <> show res <> "\n"
                     <> "skeleton: " <> show (findStraightSkeleton rectangle []) <> "\n"
                     <> "raw Skeleton: " <> show (skeletonOfNodes True lineSegs lineSegs []) <> "\n"
                     <> "faces: " <> show (facesOf $ fromMaybe (error "no") $ findStraightSkeleton rectangle []) <> "\n"
  where
    res = nodeTreesOf (findStraightSkeleton rectangle [])
    lineSegs = slist [lineSegsOfContour rectangle]
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleNodeTreeHasLessThanThreeGenerations :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Bool
prop_RectangleNodeTreeHasLessThanThreeGenerations x y rawFirstTilt rawSecondTilt rawDistanceToCorner
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

prop_RectangleCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleCanPlaceFaces x y rawFirstTilt rawSecondTilt rawDistanceToCorner = facesOf (fromMaybe (error $ show rectangle) $ findStraightSkeleton rectangle []) -/> slist []
  where
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleHasRightFaceCount x y rawFirstTilt rawSecondTilt rawDistanceToCorner = length (facesOf $ fromMaybe (error $ show rectangle) $ findStraightSkeleton rectangle []) --> 4
  where
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleFacesInOrder x y rawFirstTilt rawSecondTilt rawDistanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show rectangle) $ findStraightSkeleton rectangle []) --> rectangleAsSegs
  where
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner
    rectangleAsSegs = lineSegsOfContour rectangle
    firstSeg = onlyOneOf rectangleAsSegs

prop_RectangleFacesRightArcCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Bool
prop_RectangleFacesRightArcCount x y rawFirstTilt rawSecondTilt rawDistanceToCorner
  | res == True = True
  | otherwise = error $ "Too many arcs found:\n"
                     <> (concat $ show . arcCount <$> faces) <> "\n"
                     <> show skeleton <> "\n"
                     <> show faces <> "\n"
  where
    res = all (\a -> arcCount a < 4) faces
    faces = facesOf skeleton
    skeleton = fromMaybe (error $ show rectangle) $ findStraightSkeleton rectangle []
    arcCount (Face _ _ midArcs _) = 2 + len midArcs
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleFacesAllWoundLeft  :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Bool
prop_RectangleFacesAllWoundLeft x y rawFirstTilt rawSecondTilt rawDistanceToCorner
  | allIsLeft = True
  | otherwise = error $ "miswound face found:\n"
                     <> (concat $ show . faceLefts <$> faces) <> "\n"
                     <> show skeleton <> "\n"
                     <> show faces <> "\n"
  where
    allIsLeft = all faceAllIsLeft faces
    faceAllIsLeft face = all (== Just True) $ faceLefts face
    faceLefts (Face edge firstArc (Slist midArcs _) lastArc) = mapWithFollower (\(pl1, _) (pl2, _) -> pLineIsLeft pl1 pl2)  $ (eToPL edge) : firstArc : midArcs <> [lastArc]
    faces = facesOf skeleton
    skeleton = fromMaybe (error $ show rectangle) $ findStraightSkeleton rectangle []
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleFacesInsetWithRemainder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Bool
prop_RectangleFacesInsetWithRemainder x y rawFirstTilt rawSecondTilt distanceToCorner
  | length insetContours == 1 &&
    length remainingFaces <= 4 &&
    length faces == 4 = True
  | otherwise = error $ "inset contours (1): " <> show (length insetContours) <> "\n"
                     <> "remainingFaces (4): " <> show (length remainingFaces) <> "\n"
                     <> "faces (4): " <> show (len faces) <> "\n"
                     <> "inset distance: " <> show insetDistance <> "\n"
                     <> show insetContours <> "\n"
                     <> show remainingFaces <> "\n"
                     <> dumpGanjas (toGanja rectangle : (toGanja <$> remainingFaces) <> (toGanja <$> rawFaces))
  where
    (insetContours, remainingFaces) = insetBy insetDistance faces
    insetDistance = (min a b) / 2
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
    faces@(Slist rawFaces _) = facesOf $ fromMaybe (error $ show rectangle) $ findStraightSkeleton rectangle []
    c = coerce distanceToCorner
    rectangle = randomRectangle x y firstTilt rawSecondTilt distanceToCorner

prop_RectangleFacesInsetSmallerThanRectangle :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Bool
prop_RectangleFacesInsetSmallerThanRectangle  x y rawFirstTilt rawSecondTilt distanceToCorner
  | length insetContours == 1 &&
    contourContainsContour rectangle insetContour &&
    insetIsSmaller = True
  | otherwise = error "inset failed to shrink"
  where
    insetIsSmaller = minIX > minRX && minIY > minRY && maxRX > maxIX && maxRY > maxIY
      where
        ((Point2 (minRX, minRY)) , (Point2 (maxRX, maxRY))) = minMaxPoints rectangle
        ((Point2 (minIX, minIY)) , (Point2 (maxIX, maxIY))) = minMaxPoints insetContour
    insetContour = head insetContours
    (insetContours, _) = insetBy insetDistance faces
    insetDistance = (min a b) / 2
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
    faces = facesOf $ fromMaybe (error $ show rectangle) $ findStraightSkeleton rectangle []
    c = coerce distanceToCorner
    rectangle = randomRectangle x y firstTilt rawSecondTilt distanceToCorner

prop_RectangleFacesInsetWithoutRemainder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleFacesInsetWithoutRemainder x y rawFirstTilt rawSecondTilt distanceToCorner = (length insetContours, length remainingFaces) --> (0, 0)
  where
    (insetContours, remainingFaces) = insetBy (coerce distanceToCorner) (facesOf $ fromMaybe (error $ show rectangle) $ findStraightSkeleton rectangle [])
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt distanceToCorner

rectangleBrokenSpec :: Spec
rectangleBrokenSpec = do
  describe "Rectangles" $ do
    it "only generates one, or two generations of INodes" $
      unit_RectangleNodeTreeHasLessThanThreeGenerations

rectangleSpec :: Spec
rectangleSpec = do
  describe "Rectangles" $ do
    it "finds no convex motorcycles" $
      property prop_RectangleNoConvexMotorcycles
    it "finds no divides" $
      property prop_RectangleNoDivides
    it "finds a straight skeleton" $
      property prop_RectangleHasStraightSkeleton
    it "only has one Nodetree in the found straight skeleton" $
      property prop_RectangleStraightSkeletonHasOneNodeTree
    it "only generates one, or two generations of INodes" $
      property prop_RectangleNodeTreeHasLessThanThreeGenerations
    it "places faces on the straight skeleton of a rectangle" $
      property prop_RectangleCanPlaceFaces
    it "finds only four faces for any rectangle" $
      property prop_RectangleHasRightFaceCount
    it "places faces on a rectangle in the order the line segments were given" $
      property prop_RectangleFacesInOrder
    it "places faces on a rectangle such that each face is wound to the left" $
      property prop_RectangleFacesAllWoundLeft
    it "does not consider a rectangle to be a square" $
      property prop_RectangleMotorcyclesDoNotIntersectAtPoint
    it "generates faces with less than four arcs" $
      property prop_RectangleFacesRightArcCount
    it "insets a rectangle halfway, finding 4 remaining faces" $
      property prop_RectangleFacesInsetWithRemainder
    it "insets a rectangle completely, finding 0 remaining faces" $
      property prop_RectangleFacesInsetWithoutRemainder
    it "sees an inset of a rectangle as being smaller than the source rectangle" $
      property prop_RectangleFacesInsetSmallerThanRectangle
