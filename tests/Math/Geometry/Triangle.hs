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
  triangleBrokenSpec,
  triangleSpec,
  triangleStatSpec
  ) where

import Prelude (Bool(True), Show(show), ($), (.), (+), (<>), (==), (<$>), all, error, length, otherwise, pure, sqrt)

-- The Either library.
import Data.Either (rights)

-- The List library.
import Data.List (concat, concatMap, transpose)

-- The Maybe library.
import Data.Maybe (fromMaybe, fromJust)

-- Slists, a form of list with a stated size in the structure.
import Slist (len)

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive(Positive))
import Test.QuickCheck.Property (label, liftBool, Property)

-- for boiling types down to their basics.
import Data.Coerce (coerce)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

import Graphics.Slicer.Math.Contour (makePointContour)

-- Basic functions for mapping, rounding, and introspecting points.
import Graphics.Slicer.Math.Definitions (Contour, Point2(Point2), mapWithFollower)

-- Basic intersection logic.
import Graphics.Slicer.Math.Intersections (intersectionsAtSamePoint, intersectionBetween)

-- Our 2D Projective Geometric Algebra library.
import Graphics.Slicer.Math.PGA (distance2PP, fuzzinessOfP, outAndErrOf)

-- Our debugging library, for making the below simpler to read, and drop into command lines.
import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (ListThree(ListThree), Radian(Radian), generationsOf, oneNodeTreeOf, randomTriangle)

-- The logic for creating straight skeletons from concave contours.
import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour)

-- The part of our library that puts faces onto a contour. faces have one exterior side, and a number of internal sides (defined by Arcs).
import Graphics.Slicer.Math.Skeleton.Face (Face(Face), facesOf)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Shared tests, between different geometry.
import Math.Geometry.CommonTests (prop_CanPlaceFaces, prop_ENodeArcsIntersectAtSamePoint, prop_FacesAllWoundLeft, prop_FacesInOrder, prop_HasAStraightSkeleton, prop_InsetIsSmaller, prop_InsetOfInsetIsSmaller, prop_NoDivides, prop_NoMotorcycles, prop_StraightSkeletonHasOneNodeTree)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->))

prop_StraightSkeletonHasOneGeneration :: Contour -> Expectation
prop_StraightSkeletonHasOneGeneration contour = generationsOf (oneNodeTreeOf $ fromMaybe (error "no straight skeleton?") $ findStraightSkeleton contour []) --> 1

stat_TriangleENodeArcsIntersectAtSamePoint :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Property
stat_TriangleENodeArcsIntersectAtSamePoint centerX centerY rawRadians rawDists
  = label ("Triangle: " <> show triangle <> "\n"
           <> "ENodes: " <> show eNodes <> "\n"
           <> "Intersections: " <> show intersections <> "\n"
           <> show nodeOutsAndErrs <> "\n"
           <> dumpGanjas ((toGanja <$> eNodes)
                        <> concat (transpose [toGanja <$> intersections
                                             ,toGanja . show <$> fuzziness
                                             ,toGanja . show <$> distances
                                             ,[toGanja $ show retVal]])))
           $ liftBool True
  where
    retVal = intersectionsAtSamePoint nodeOutsAndErrs
    intersections = rights $ fromJust <$> mapWithFollower intersectionBetween nodeOutsAndErrs
    fuzziness = fuzzinessOfP <$> intersections
    distances = mapWithFollower distance2PP intersections
    nodeOutsAndErrs = outAndErrOf <$> eNodes
    eNodes = eNodesOfOutsideContour triangle
    triangle = randomTriangle centerX centerY rawRadians rawDists

-- | failed, until the fuzziness factor in Intersections.HS was lifted to 512.
unit_TriangleENodeArcsIntersectAtSamePoint :: Bool
unit_TriangleENodeArcsIntersectAtSamePoint = retVal
  where
    retVal = intersectionsAtSamePoint nodeOutsAndErrs
    nodeOutsAndErrs = outAndErrOf <$> eNodes
    eNodes = eNodesOfOutsideContour triangle
    triangle = randomTriangle centerX centerY rawRadians rawDists
    centerX,centerY :: ℝ
    centerX = 0
    centerY = 0
    rawRadians = ListThree [Radian 2.985457801469671, Radian 2.626880074405778, Radian 5.132144721027657]
    rawDists :: ListThree (Positive ℝ)
    rawDists = ListThree [15.806453706102848, 50.6285286757685, 16.68828123028247]

-- use our property test to test this, rather than just a hard coded unit test.
unit_TriangleInset :: Contour -> Positive ℝ -> Bool
unit_TriangleInset contour insetDistance = prop_InsetIsSmaller (coerce insetDistance) contour

unit_InsetsInset :: Contour -> Positive ℝ -> Positive ℝ -> Bool
unit_InsetsInset contour insetDistance1 insetDistance2 = prop_InsetOfInsetIsSmaller (coerce insetDistance1) (coerce insetDistance2) contour

-- | Ensure we find three faces for the given contour (a triangle).
prop_HasThreeFaces :: Contour -> Expectation
prop_HasThreeFaces contour = length (facesOf $ fromMaybe (error $ show contour) $ findStraightSkeleton contour []) --> 3

prop_FacesHaveThreeSides :: Contour -> Bool
prop_FacesHaveThreeSides contour
  | res = True
  | otherwise = error $ "Too many arcs found:\n"
                     <> concatMap (show . arcCount) faces <> "\n"
                     <> show skeleton <> "\n"
                     <> show faces <> "\n"
  where
    res = all (\a -> arcCount a == 2) faces
    arcCount (Face _ _ midArcs _) = 2 + len midArcs
    faces = facesOf skeleton
    skeleton = fromMaybe (error $ show contour) $ findStraightSkeleton contour []

-- FIXME: add inset tests here.

triangleBrokenSpec :: Spec
triangleBrokenSpec = pure ()

triangleStatSpec :: Spec
triangleStatSpec = do
  describe "Triangles" $ do
   it "finds that all of the outArcs of the ENodes intersect at the same point" $
      property stat_TriangleENodeArcsIntersectAtSamePoint

triangleSpec :: Spec
triangleSpec = do
  describe "Triangles" $ do
    it "finds no convex motorcycles" $
      property (expectationFromTriangle prop_NoMotorcycles)
    it "finds no divides" $
      property (expectationFromTriangle prop_NoDivides)
    it "finds a straight skeleton" $
      property (expectationFromTriangle prop_HasAStraightSkeleton)
    it "finds only one NodeTree in the straight skeleton" $
      property (expectationFromTriangle prop_StraightSkeletonHasOneNodeTree)
    it "only generates one generation of INodes" $
      property (expectationFromTriangle prop_StraightSkeletonHasOneGeneration)
    it "finds that all of the outArcs of the ENodes intersect at the same point" $
      property (boolFromTriangle prop_ENodeArcsIntersectAtSamePoint)
    it "finds that all of the outArcs of the ENodes intersect at the same point (unit)"
      unit_TriangleENodeArcsIntersectAtSamePoint
    it "can place faces on the straight skeleton" $
      property (expectationFromTriangle prop_CanPlaceFaces)
    it "only places three faces" $
      property (expectationFromTriangle prop_HasThreeFaces)
    it "faces only have three sides" $
      property (boolFromTriangle prop_FacesHaveThreeSides)
    it "each face is wound to the left" $
      property (boolFromTriangle prop_FacesAllWoundLeft)
    it "places faces in the same order as the input line segments" $
      property (expectationFromTriangle prop_FacesInOrder)
    it "insets part way (unit)" $
      unit_TriangleInset unitTriangle 0.25
    it "insets part way, twice (unit)" $
      unit_InsetsInset unitTriangle 0.25 0.2
{-    it "insets halfway, finding 3 remaining faces" $
      property prop_TriangleFacesInsetWithRemainder
    it "insets completely, finding 0 remaining faces" $
      property prop_TriangleFacesInsetWithoutRemainder
-}
  where
    boolFromTriangle :: (Contour -> Bool) -> ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Bool
    boolFromTriangle f x y rawRadians rawDists = f triangle
      where
        triangle = randomTriangle x y rawRadians rawDists
    expectationFromTriangle :: (Contour -> Expectation) -> ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
    expectationFromTriangle f x y rawRadians rawDists = f triangle
      where
        triangle = randomTriangle x y rawRadians rawDists
    unitTriangle = makePointContour [Point2 (2,0), Point2 (1.0,sqrt 3), Point2 (0,0)]
