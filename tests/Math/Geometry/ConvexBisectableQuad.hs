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

{- tests for the properties of a quadralaterial that is convex, and has at least one right angle. -}

module Math.Geometry.ConvexBisectableQuad (
  convexBisectableQuadBrokenSpec,
  convexBisectableQuadSpec
  ) where

import Prelude (Bool(True), Show(show), ($), (<), (.), (+), (<>), (==), (<$>), all, concat, error, length, otherwise, pure)

-- The Maybe library.
import Data.Maybe (fromMaybe, Maybe(Just, Nothing))

-- Slists, a form of list with a stated size in the structure.
import Slist (len, slist)
import Slist.Type (Slist(Slist))

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Our Contour library.
import Graphics.Slicer.Math.Contour (lineSegsOfContour)

-- Assorted basic math functions
import Graphics.Slicer.Math.Definitions (mapWithFollower)

-- Basic intersection logic.
import Graphics.Slicer.Math.Intersections (intersectionsAtSamePoint)

-- Our 2D Projective Geometric Algebra library.
import Graphics.Slicer.Math.PGA (eToPL, outAndErrOf, pLineIsLeft)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian(Radian), edgesOf, generationsOf, nodeTreesOf, oneNodeTreeOf, onlyOneOf, randomConvexBisectableQuad)

-- Our logic for dividing a contour into cells, which each get nodetrees for them, which are combined into a straight skeleton.
import Graphics.Slicer.Math.Skeleton.Cells (findDivisions)

-- The logic for creating straight skeletons from concave contours.
import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour)

-- The part of our library that puts faces onto a contour. faces have one exterior side, and a number of internal sides (defined by Arcs).
import Graphics.Slicer.Math.Skeleton.Face (Face(Face), facesOf, orderedFacesOf)

-- The portion of our library that reasons about motorcycles, emiting from the concave nodes of our contour.
import Graphics.Slicer.Math.Skeleton.Motorcycles (convexMotorcycles, crashMotorcycles)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->), (-/>))

prop_ConvexBisectableQuadNoConvexMotorcycles :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadNoConvexMotorcycles centerX centerY rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = convexMotorcycles convexBisectableQuad --> []
  where
    convexBisectableQuad = randomConvexBisectableQuad centerX centerY rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadNoDivides :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadNoDivides x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = findDivisions convexBisectableQuad (fromMaybe (error $ show convexBisectableQuad) $ crashMotorcycles convexBisectableQuad []) --> []
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

-- | can fail to flip the contour.
unit_ConvexBisectableQuadNoDivides :: Expectation
unit_ConvexBisectableQuadNoDivides = findDivisions convexBisectableQuad (fromMaybe (error $ show convexBisectableQuad) $ crashMotorcycles convexBisectableQuad []) --> []
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    x,y :: ℝ
    x = -37.7
    y = 5.67
    rawFirstTilt = Radian 0.852
    rawSecondTilt = Radian 2.218
    rawFirstDistanceToCorner,rawSecondDistanceToCorner :: Positive ℝ
    rawFirstDistanceToCorner = 25.08
    rawSecondDistanceToCorner = 4.595

prop_ConvexBisectableQuadHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadHasStraightSkeleton x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = findStraightSkeleton convexBisectableQuad [] -/> Nothing
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadStraightSkeletonHasOneNodeTree :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadStraightSkeletonHasOneNodeTree x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = nodeTreesOf (findStraightSkeleton convexBisectableQuad []) --> 1
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadNodeTreeHasLessThanThreeGenerations :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_ConvexBisectableQuadNodeTreeHasLessThanThreeGenerations x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = generationsOf (oneNodeTreeOf $ fromMaybe (error "no skeleton?") $ findStraightSkeleton convexBisectableQuad []) < 3
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadENodeArcsIntersectAtSamePoint :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_ConvexBisectableQuadENodeArcsIntersectAtSamePoint centerX centerY rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = retVal
  where
    retVal = intersectionsAtSamePoint nodeOutsAndErrs
    nodeOutsAndErrs = outAndErrOf <$> eNodes
    eNodes = eNodesOfOutsideContour convexBisectableQuad
    convexBisectableQuad = randomConvexBisectableQuad centerX centerY rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadCanPlaceFaces x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = facesOf (fromMaybe (error $ show convexBisectableQuad) $ findStraightSkeleton convexBisectableQuad []) -/> slist []
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadHasRightFaceCount x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = length (facesOf $ fromMaybe (error $ show convexBisectableQuad) $ findStraightSkeleton convexBisectableQuad []) --> 4
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadFacesRightArcCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_ConvexBisectableQuadFacesRightArcCount x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
  | res == True = True
  | otherwise = error $ "Too many arcs found:\n"
                     <> (concat $ show . arcCount <$> faces) <> "\n"
                     <> show skeleton <> "\n"
                     <> show faces <> "\n"
  where
    res = all (\a -> arcCount a == 2) faces
    faces = facesOf skeleton
    skeleton = fromMaybe (error $ show convexBisectableQuad) $ findStraightSkeleton convexBisectableQuad []
    arcCount (Face _ _ midArcs _) = 2 + len midArcs
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadFacesInOrder x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show convexBisectableQuad) $ findStraightSkeleton convexBisectableQuad []) --> convexBisectableQuadAsSegs
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    convexBisectableQuadAsSegs = lineSegsOfContour convexBisectableQuad
    firstSeg = onlyOneOf convexBisectableQuadAsSegs

prop_ConvexBisectableQuadFacesAllWoundLeft  :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_ConvexBisectableQuadFacesAllWoundLeft x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
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
    skeleton = fromMaybe (error $ show convexBisectableQuad) $ findStraightSkeleton convexBisectableQuad []
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

convexBisectableQuadBrokenSpec :: Spec
convexBisectableQuadBrokenSpec = pure ()

convexBisectableQuadSpec :: Spec
convexBisectableQuadSpec = do
  describe "Convex Bisectable Quads" $ do
    it "finds no convex motorcycles" $
      property prop_ConvexBisectableQuadNoConvexMotorcycles
    it "finds no divides" $
      property prop_ConvexBisectableQuadNoDivides
    it "finds no divides (unit)" $
      unit_ConvexBisectableQuadNoDivides
    it "finds a straight skeleton" $
      property prop_ConvexBisectableQuadHasStraightSkeleton
    it "finds only one nodetree in the straight skeleton" $
      property prop_ConvexBisectableQuadStraightSkeletonHasOneNodeTree
    it "generates fewer than three generations of INodes" $
      property prop_ConvexBisectableQuadNodeTreeHasLessThanThreeGenerations
    it "finds that all of the outArcs of the ENodes intersect at the same point" $
      property prop_ConvexBisectableQuadENodeArcsIntersectAtSamePoint
    it "can place faces on the straight skeleton" $
      property prop_ConvexBisectableQuadCanPlaceFaces
    it "only places four faces" $
      property prop_ConvexBisectableQuadHasRightFaceCount
    it "faces have less than four sides" $
      property prop_ConvexBisectableQuadFacesRightArcCount
    it "places faces in the order the line segments were given" $
      property prop_ConvexBisectableQuadFacesInOrder
    it "each face is wound to the left" $
      property prop_ConvexBisectableQuadFacesAllWoundLeft
