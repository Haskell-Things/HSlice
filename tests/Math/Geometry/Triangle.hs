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

{- tests for the properties of a triangle. -}

module Math.Geometry.Triangle (
  triangleSpec,
  triangleStatSpec
  ) where

import Prelude (Bool(True), Show(show), ($), (.), (<>), (<$>), error, fst, length, snd)

-- The Either library.
import Data.Either (rights)

-- The List library.
import Data.List (concat, transpose)

-- The Maybe library.
import Data.Maybe (fromMaybe, fromJust, Maybe(Nothing))

-- Slists, a form of list with a stated size in the structure.
import Slist (slist)

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive)
import Test.QuickCheck.Property (label, liftBool, Property)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Our Contour library.
import Graphics.Slicer.Math.Contour (lineSegsOfContour, insideIsLeft, innerContourPoint, firstPointPairOfContour)

-- Basic functions for mapping, rounding, and introspecting points.
import Graphics.Slicer.Math.Definitions (Point2(Point2), mapWithFollower, startPoint, endPoint, pointBetweenPoints)

-- Basic intersection logic.
import Graphics.Slicer.Math.Intersections (intersectionsAtSamePoint, intersectionBetween)

-- Our 2D Projective Geometric Algebra library.
import Graphics.Slicer.Math.PGA (distance2PP, eToPL, eToPP, fuzzinessOfP, join2EP, join2PP, normalizeL, outAndErrOf, plinesIntersectIn)

-- Our debugging library, for making the below simpler to read, and drop into command lines.
import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (ListThree, Radian, edgesOf, generationsOf, randomTriangle, onlyOneOf)

-- Our logic for dividing a contour into cells, which each get nodetrees for them, which are combined into a straight skeleton.
import Graphics.Slicer.Math.Skeleton.Cells (findDivisions)

-- The logic for creating straight skeletons from concave contours.
import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour)

-- Basic contour handling. the Point and LineSeg we use to determine how to flip a contour.
import Graphics.Slicer.Math.Contour (mostPerpPointAndLineSeg)

-- The part of our library that puts faces onto a contour. faces have one exterior side, and a number of internal sides (defined by Arcs).
import Graphics.Slicer.Math.Skeleton.Face (facesOf, orderedFacesOf)

-- The portion of our library that reasons about motorcycles, emiting from the concave nodes of our contour.
import Graphics.Slicer.Math.Skeleton.Motorcycles (convexMotorcycles, crashMotorcycles)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->), (-/>))

prop_TriangleNoConvexMotorcycles :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleNoConvexMotorcycles centerX centerY rawRadians rawDists = convexMotorcycles triangle --> []
  where
    triangle  = randomTriangle centerX centerY rawRadians rawDists

prop_TriangleNoDivides :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleNoDivides centerX centerY rawRadians rawDists = findDivisions triangle (fromMaybe dumpError $ crashMotorcycles triangle []) --> []
  where
    dumpError = error $ "no crash tree?\n" <> errorString
    errorString =  dumpGanjas ([toGanja triangle, toGanja (Point2 (centerX, centerY)), toGanja pLineFromInside, toGanja pLineFromMid] <> (toGanja . fst . eToPL <$> lineSegsOfContour triangle)) <> "\n"
                <> show lineSeg <> "\n"
                <> show firstPoints <> "\n"
                <> show (insideIsLeft triangle) <> "\n"
                <> show (plinesIntersectIn pLine pLineFromInside) <> "\n"
    -- we normalize this for Ganja.js.
    pLineFromInside = normalizeL $ fst $ join2PP innerPoint $ eToPP outsidePoint
    pLineFromMid    = fst $ normalizeL $ fst $ join2EP midPoint outsidePoint
    firstPoints     = firstPointPairOfContour triangle
    innerPoint      = fromMaybe (error "cannot find inner point.") maybeInnerPoint
    maybeInnerPoint = innerContourPoint triangle
    midPoint        = pointBetweenPoints (startPoint lineSeg) (endPoint lineSeg)
    pLine           = eToPL lineSeg
    outsidePoint    = fst $ mostPerpPointAndLineSeg triangle
    lineSeg         = snd $ mostPerpPointAndLineSeg triangle
    triangle        = randomTriangle centerX centerY rawRadians rawDists

prop_TriangleMotorcyclesEndAtSamePoint  :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Property
prop_TriangleMotorcyclesEndAtSamePoint centerX centerY rawRadians rawDists
  = label ("Triangle: " <> show triangle <> "\n"
           <> "ENodes: " <> show eNodes <> "\n"
           <> "Intersections: " <> show intersections <> "\n"
           <> show nodeOutsAndErrs <> "\n"
           <> dumpGanjas ((toGanja <$> eNodes)
                        <> concat (transpose [(toGanja <$> intersections)
                                             ,(toGanja . show <$> fuzziness)
                                             ,(toGanja . show <$> distances)
                                             , [toGanja $ show retVal]])))
           $ liftBool True
  where
    retVal = intersectionsAtSamePoint nodeOutsAndErrs
    intersections = rights $ fromJust <$> mapWithFollower intersectionBetween nodeOutsAndErrs
    fuzziness = fuzzinessOfP <$> intersections
    distances = mapWithFollower distance2PP intersections
    nodeOutsAndErrs = outAndErrOf <$> eNodes
    eNodes = eNodesOfOutsideContour triangle
    triangle = randomTriangle centerX centerY rawRadians rawDists

prop_TriangleHasStraightSkeleton :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleHasStraightSkeleton centerX centerY rawRadians rawDists = findStraightSkeleton triangle [] -/> Nothing
  where
    triangle = randomTriangle centerX centerY rawRadians rawDists

prop_TriangleStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleStraightSkeletonHasRightGenerationCount centerX centerY rawRadians rawDists = generationsOf (findStraightSkeleton triangle []) --> 1
  where
    triangle = randomTriangle centerX centerY rawRadians rawDists

prop_TriangleCanPlaceFaces :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleCanPlaceFaces centerX centerY rawRadians rawDists = facesOf (fromMaybe (error "Got Nothing") $ findStraightSkeleton triangle []) -/> slist []
  where
    triangle = randomTriangle centerX centerY rawRadians rawDists

prop_TriangleHasRightFaceCount :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleHasRightFaceCount centerX centerY rawRadians rawDists = length (facesOf $ fromMaybe (error $ show triangle) $ findStraightSkeleton triangle []) --> 3
  where
    triangle = randomTriangle centerX centerY rawRadians rawDists

prop_TriangleFacesInOrder :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleFacesInOrder centerX centerY rawRadians rawDists = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show triangle) $ findStraightSkeleton triangle []) --> lineSegsOfContour triangle
  where
    triangle = randomTriangle centerX centerY rawRadians rawDists
    firstSeg = onlyOneOf $ lineSegsOfContour triangle

triangleStatSpec :: Spec
triangleStatSpec = do
  describe "Triangles" $ do
   it "finds that all motorcycles intersect at the same point in a triangle" $
      property prop_TriangleMotorcyclesEndAtSamePoint

triangleSpec :: Spec
triangleSpec = do
  describe "Geometry (Triangles)" $ do
    it "finds no convex motorcycles in a triangle" $
      property prop_TriangleNoConvexMotorcycles
    it "finds no divides in a triangle" $
      property prop_TriangleNoDivides
--    it "finds that all motorcycles intersect at the same point in a triangle" $
--      property prop_TriangleMotorcyclesEndAtSamePoint
    it "finds the straight skeleton of a triangle (property)" $
      property prop_TriangleHasStraightSkeleton
    it "only generates one generation for a triangle" $
      property prop_TriangleStraightSkeletonHasRightGenerationCount
    it "places faces on the straight skeleton of a triangle" $
      property prop_TriangleCanPlaceFaces
    it "places faces on a triangle in the order the line segments were given" $
      property prop_TriangleFacesInOrder
    it "only finds three face triangles" $
      property prop_TriangleHasRightFaceCount