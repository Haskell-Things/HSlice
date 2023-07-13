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

{- property tests that are reusable with many types of geometric figgures. -}

module Math.Geometry.CommonTests (
  prop_CanPlaceFaces,
  prop_ENodeArcsIntersectAtSamePoint,
  prop_FacesAllWoundLeft,
  prop_FacesHaveThreeSides,
  prop_FacesHaveThreeToFiveSides,
  prop_FacesInOrder,
  prop_HasFourFaces,
  prop_HasAStraightSkeleton,
  prop_InsetIsSmaller,
  prop_InsetOfInsetIsSmaller,
  prop_NodeTreeHasFewerThanFourGenerations,
  prop_NodeTreeHasFewerThanThreeGenerations,
  prop_NoDivides,
  prop_NoMotorcycles,
  prop_StraightSkeletonHasOneNodeTree
  ) where

import Prelude (Bool(True), ($), (<), (>), (.), (+), (&&), (<>), (==), (||), (<$>), all, error, fst, head, length, not, null, otherwise, show, snd)

-- The List library.
import Data.List (concatMap)

-- The Maybe library.
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

-- Slists, a form of list with a stated size in the structure.
import Slist (len, slist)
import Slist.Type(Slist(Slist))

-- Hspec, for writing specs.
import Test.Hspec (Expectation)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Basic contour handling. the Point and LineSeg we use to determine how to flip a contour.
import Graphics.Slicer.Math.Contour (contourContainsContour, firstPointPairOfContour, innerContourPoint, insideIsLeft, mostPerpPointAndLineSeg)

-- Basic definitions, used in multiple places in the math library.
import Graphics.Slicer.Math.Definitions (Contour, Point2(Point2), endPoint, lineSegsOfContour, mapWithFollower, minMaxPoints, pointBetweenPoints, startPoint)

-- Basic intersection logic.
import Graphics.Slicer.Math.Intersections (intersectionsAtSamePoint)

-- Our 2D Projective Geometric Algebra library.
import Graphics.Slicer.Math.PGA (eToPL, eToPP, join2EP, join2PP, normalizeL, outAndErrOf, pLineIsLeft, plinesIntersectIn)

-- Our debugging library, for making the below simpler to read, and drop into command lines.
import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (edgesOf, generationsOf, nodeTreesOf, oneNodeTreeOf, onlyOneOf)

-- Our logic for dividing a contour into cells, which each get nodetrees for them, which are combined into a straight skeleton.
import Graphics.Slicer.Math.Skeleton.Cells (findDivisions)

-- The logic for creating straight skeletons from concave contours.
import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour)

-- The part of our library that puts faces onto a contour. faces have one exterior side, and a number of internal sides (defined by Arcs).
import Graphics.Slicer.Math.Skeleton.Face (Face(Face), facesOf, orderedFacesOf)

-- functions for placing lines segments onto faces.
import Graphics.Slicer.Math.Skeleton.Line (insetBy)

-- The portion of our library that reasons about motorcycles, emiting from the concave nodes of our contour.
import Graphics.Slicer.Math.Skeleton.Motorcycles (crashMotorcycles, convexMotorcycles)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->), (-/>))

-- | Ensure that faces can be placed on the given contour.
prop_CanPlaceFaces :: Contour -> Expectation
prop_CanPlaceFaces contour = facesOf (fromMaybe (error $ show contour) $ findStraightSkeleton contour []) -/> slist []

prop_ENodeArcsIntersectAtSamePoint :: Contour -> Bool
prop_ENodeArcsIntersectAtSamePoint contour = intersectionsAtSamePoint nodeOutsAndErrs
  where
    nodeOutsAndErrs = outAndErrOf <$> eNodes
    eNodes = eNodesOfOutsideContour contour

prop_FacesAllWoundLeft  :: Contour -> Bool
prop_FacesAllWoundLeft contour
  | allIsLeft = True
  | otherwise = error $ "miswound face found:\n"
                     <> concatMap (show . faceLefts) faces <> "\n"
                     <> show skeleton <> "\n"
                     <> show faces <> "\n"
  where
    allIsLeft = all faceAllIsLeft faces
    faceAllIsLeft face = all (== Just True) $ faceLefts face
    faceLefts (Face edge firstArc (Slist midArcs _) lastArc) = mapWithFollower (\(pl1, _) (pl2, _) -> pLineIsLeft pl1 pl2)  $ eToPL edge : firstArc : midArcs <> [lastArc]
    faces = facesOf skeleton
    skeleton = fromMaybe (error $ show contour) $ findStraightSkeleton contour []

prop_FacesHaveThreeSides :: Contour -> Bool
prop_FacesHaveThreeSides contour
  | res = True
  | otherwise = error $ "Too many arcs found:\n"
                     <> concatMap (show . arcCount) faces <> "\n"
                     <> show skeleton <> "\n"
                     <> show faces <> "\n"
  where
    res = all (\a -> arcCount a == 2) faces
    faces = facesOf skeleton
    skeleton = fromMaybe (error $ show contour) $ findStraightSkeleton contour []
    arcCount (Face _ _ midArcs _) = 2 + len midArcs

-- | Ensure all of the faces placed on a contour have between three and five sides.
prop_FacesHaveThreeToFiveSides :: Contour -> Bool
prop_FacesHaveThreeToFiveSides contour
  | res = True
  | otherwise = error $ "Too many arcs found:\n"
                     <> concatMap (show . arcCount) faces <> "\n"
                     <> show skeleton <> "\n"
                     <> show faces <> "\n"
  where
    res = all (\a -> arcCount a == 2 || arcCount a == 3 || arcCount a == 4) faces
    faces = facesOf skeleton
    skeleton = fromMaybe (error $ show contour) $ findStraightSkeleton contour []
    arcCount (Face _ _ midArcs _) = 2 + len midArcs

prop_FacesInOrder :: Contour -> Expectation
prop_FacesInOrder contour = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show contour) $ findStraightSkeleton contour []) --> contourAsSegs
  where
    firstSeg = onlyOneOf contourAsSegs
    contourAsSegs = lineSegsOfContour contour

-- | Ensure that we only place four races on the given contour.
prop_HasFourFaces :: Contour -> Expectation
prop_HasFourFaces contour = length (facesOf $ fromMaybe (error $ show contour) $ findStraightSkeleton contour []) --> 4

-- | Ensure we can actually draw a straight skeleton for the given contour.
prop_HasAStraightSkeleton :: Contour -> Expectation
prop_HasAStraightSkeleton contour = findStraightSkeleton contour [] -/> Nothing

-- | ensure that when shrinking a contour, the result is smaller.
prop_InsetIsSmaller :: ℝ -> Contour -> Bool
prop_InsetIsSmaller distance contour
  | length foundContours == 1 && contourContainsContour contour insetContour = insetIsSmaller
  | otherwise = error $ "wrong number of contours found when shrinking contour: " <> show contour <> "\n"
    where
      insetIsSmaller = minIX > minRX && minIY > minRY && maxRX > maxIX && maxRY > maxIY
        where
          (Point2 (minRX, minRY), Point2 (maxRX, maxRY)) = minMaxPoints contour
          (Point2 (minIX, minIY), Point2 (maxIX, maxIY)) = minMaxPoints insetContour
      insetContour = head foundContours
      (foundContours, _) = insetBy distance faces
      faces = facesOf $ fromMaybe (error $ "could not get a straight skeleton for: " <> show contour) $ findStraightSkeleton contour []

-- | ensure that when shrinking a contour twice, the result is smaller.
prop_InsetOfInsetIsSmaller :: ℝ -> ℝ -> Contour -> Bool
prop_InsetOfInsetIsSmaller distance1 distance2 contour
  | length foundContours == 1 && contourContainsContour contour foundContour = insetIsSmaller
  | null foundContours && contourContainsContour contour foundContour = error $ "no contours found when shrinking contour: " <> errorMessage
  | length foundContours == 1 = error $ "smaller contour was not inside of provided contour.\n" <> errorMessage
  | otherwise = error $ "wtf\n" <> errorMessage
    where
      errorMessage = "Provided contour: " <> show contour <> "\n"
                  <> "first inset generation:\n" <> show insetContours <> "\n"
                  <> "number first inset faces: " <> show (length insetFaces) <> "\n"
                  <> "first inset faces:\n" <> show insetFaces <> "\n"
                  <> "final inset generation:\n" <> show foundContours <> "\n"
                  <> "final inset faces:\n" <> show foundFaces <> "\n"
                  <> dumpGanjas ([toGanja contour] <> if not (null insetContours) then [toGanja insetContour] else [])
      insetIsSmaller = minIX > minRX && minIY > minRY && maxRX > maxIX && maxRY > maxIY
        where
          (Point2 (minRX, minRY), Point2 (maxRX, maxRY)) = minMaxPoints contour
          (Point2 (minIX, minIY), Point2 (maxIX, maxIY)) = minMaxPoints foundContour
      foundContour = head foundContours
      (foundContours, foundFaces) = insetBy distance2 (slist insetFaces)
      insetContour = head insetContours
      (insetContours , insetFaces) = insetBy distance1 faces
      faces = facesOf $ fromMaybe (error $ "could not get a straight skeleton for: " <> show contour) $ findStraightSkeleton contour []

-- | ensure that we only find less than four generations of INodes.
prop_NodeTreeHasFewerThanFourGenerations :: Contour -> Bool
prop_NodeTreeHasFewerThanFourGenerations contour = generationsOf (oneNodeTreeOf $ fromMaybe (error "no straight skeleton?") $ findStraightSkeleton contour []) < 4

prop_NodeTreeHasFewerThanThreeGenerations :: Contour -> Bool
prop_NodeTreeHasFewerThanThreeGenerations contour = generationsOf (oneNodeTreeOf $ fromMaybe (error "no straight skeleton?") $ findStraightSkeleton contour []) < 3

-- | Ensure the given contour has no divides in it.
prop_NoDivides :: Contour -> Expectation
prop_NoDivides contour = findDivisions contour (fromMaybe dumpError $ crashMotorcycles contour []) --> []
  where
    dumpError = error $ "no crash tree?\n" <> errorString
    errorString =  dumpGanjas ([toGanja contour, toGanja pLineFromInside, toGanja pLineFromMid] <> (toGanja . fst . eToPL <$> lineSegsOfContour contour)) <> "\n"
                <> show lineSeg <> "\n"
                <> show firstPoints <> "\n"
                <> show (insideIsLeft contour) <> "\n"
                <> show (plinesIntersectIn pLine pLineFromInside) <> "\n"
    -- we normalize this for Ganja.js.
    pLineFromInside = normalizeL $ fst $ join2PP innerPoint $ eToPP outsidePoint
    pLineFromMid    = fst $ normalizeL $ fst $ join2EP midPoint outsidePoint
    firstPoints     = firstPointPairOfContour contour
    innerPoint      = fromMaybe (error "cannot find inner point.") maybeInnerPoint
    maybeInnerPoint = innerContourPoint contour
    midPoint        = pointBetweenPoints (startPoint lineSeg) (endPoint lineSeg)
    pLine           = eToPL lineSeg
    outsidePoint    = fst $ mostPerpPointAndLineSeg contour
    lineSeg         = snd $ mostPerpPointAndLineSeg contour

-- | Ensure no motorcycles are found in the given contour.
prop_NoMotorcycles :: Contour -> Expectation
prop_NoMotorcycles contour = convexMotorcycles contour --> []

-- | Ensure that for a given contour, only one nodetree is constructed.
prop_StraightSkeletonHasOneNodeTree :: Contour -> Expectation
prop_StraightSkeletonHasOneNodeTree contour = nodeTreesOf (findStraightSkeleton contour []) --> 1
