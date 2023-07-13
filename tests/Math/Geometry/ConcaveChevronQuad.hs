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

module Math.Geometry.ConcaveChevronQuad (
  concaveChevronQuadBrokenSpec,
  concaveChevronQuadSpec
  ) where

import Prelude (Bool(True), Show(show), ($), (<>), (==), error, head, length, otherwise)

-- The Maybe library.
import Data.Maybe (fromJust, fromMaybe, isJust)

-- Slists, a form of list with a stated size in the structure.
import Slist (slist, len)

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Our basic math library.
import Graphics.Slicer.Math.Definitions (Contour)

-- Our serialization library, for debugging,
import Graphics.Slicer.Math.Ganja (dumpGanja, dumpGanjas, toGanja)

import Graphics.Slicer.Math.PGA (outAndErrOf, plinesIntersectIn)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian(Radian), cellFrom, generationsOf, oneNodeTreeOf, onlyOne, randomConcaveChevronQuad, remainderFrom)

-- Our logic for dividing a contour into cells, which each get nodetrees for them, which are combined into a straight skeleton.
import Graphics.Slicer.Math.Skeleton.Cells (findDivisions, findFirstCellOfContour, findNextCell, getRawNodeTreeOfCell, landingPointOf)

import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour)

import Graphics.Slicer.Math.Skeleton.Definitions (MotorcycleIntersection(WithENode))

-- The part of our library that puts faces onto a contour. faces have one exterior side, and a number of internal sides (defined by Arcs).
import Graphics.Slicer.Math.Skeleton.Face (facesOf)

-- The portion of our library that reasons about motorcycles, emiting from the concave nodes of our contour.
import Graphics.Slicer.Math.Skeleton.Motorcycles (convexMotorcycles, crashMotorcycles, motorcycleIntersectsAt)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Shared tests, between different geometry.
import Math.Geometry.CommonTests (prop_CanPlaceFaces, prop_FacesInOrder, prop_HasFourFaces, prop_HasAStraightSkeleton, prop_StraightSkeletonHasOneNodeTree, prop_NodeTreeHasFewerThanThreeGenerations)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->))

prop_OneMotorcycle :: Contour -> Bool
prop_OneMotorcycle contour = length (convexMotorcycles contour) == 1

prop_OneDivide :: Contour -> Expectation
prop_OneDivide contour = len (slist $ findDivisions contour (fromMaybe (error $ show contour) $ crashMotorcycles contour [])) --> 1

{-
prop_ConcaveChevronQuadSample :: Contour -> Expectation
prop_ConcaveChevronQuadSample contour = error $ "dumping: \n"
                                             <> dumpGanjas [toGanja contour, toGanja (head $ convexMotorcycles contour)] <> "\n"
-}

unit_ConcaveChevronQuadHasAStraightSkeleton :: Bool
unit_ConcaveChevronQuadHasAStraightSkeleton
  | isJust (findStraightSkeleton contour []) = True
  | otherwise = error $ "dumping: \n"
                <> dumpGanjas [toGanja contour, toGanja $ head $ convexMotorcycles contour] <> "\n"
                <> show (landingPointOf contour motorcycle) <> "\n"
                <> show (plinesIntersectIn (outAndErrOf motorcycle) (outAndErrOf eNode)) <> "\n"
                <> show divides <> "\n"
  where
    eNode = (\(WithENode a) -> a) $ landingPointOf contour motorcycle
    divides = findDivisions contour (fromMaybe (error "no") $ crashMotorcycles contour [])
    motorcycle = head $ convexMotorcycles contour
    contour = randomConcaveChevronQuad x y tilt1 distance1 distance2
    x,y :: ℝ
    x = 0
    y = 0
    tilt1 = Radian 1.0
    distance1,distance2 :: Positive ℝ
    distance1 = 1.0
    distance2 = 1.0

unit_ConcaveChevronQuadNodeTreeHasOneGeneration :: Bool
unit_ConcaveChevronQuadNodeTreeHasOneGeneration
   | res == 1 = True
   | otherwise = error $ "returned " <> show res <> ".\n"
                      <> show nodeTreeOfFirstCell <> "\n"
                      <> show nodeTreeOfSecondCell <> "\n"
                      <> show finalNodeTree <> "\n"
                      <> dumpGanja nodeTreeOfFirstCell <> "\n"
                      <> dumpGanja nodeTreeOfSecondCell <> "\n"
                      <> dumpGanja (fromJust $ findStraightSkeleton contour []) <> "\n"
  where
    res = generationsOf finalNodeTree
    finalNodeTree = oneNodeTreeOf $ fromMaybe (error "no straight skeleton?") $ findStraightSkeleton contour []
    nodeTreeOfFirstCell = getRawNodeTreeOfCell $ cellFrom $ findFirstCellOfContour contour $ findDivisions contour $ fromMaybe (error "failed to crash") $ crashMotorcycles contour []
    nodeTreeOfSecondCell = getRawNodeTreeOfCell $ cellFrom $ findNextCell $ onlyOne $ fromJust $ remainderFrom $ findFirstCellOfContour contour $ findDivisions contour $ fromMaybe (error "failed to crash") $ crashMotorcycles contour []
    contour = randomConcaveChevronQuad x y tilt1 distance1 distance2
    x,y :: ℝ
    x = 0
    y = 0
    tilt1 = Radian 1.0
    distance1,distance2 :: Positive ℝ
    distance1 = 1.0
    distance2 = 1.0

-- Failed, until we changed filterAllIntersections to always look at the middle object.
unit_ConcaveChevronQuadNodeTreeHasOneGeneration_2 :: Bool
unit_ConcaveChevronQuadNodeTreeHasOneGeneration_2
  | res == 1 = True
  | otherwise = error $ "returned:\n"
                     <> show res <> ".\n"
                      <> show firstCell <> "\n"
                      <> show divisions <> "\n"
                      <> dumpGanjas [toGanja motorcycle, toGanja contour] <> "\n"
  where
    res = generationsOf finalNodeTree
    finalNodeTree = oneNodeTreeOf $ fromMaybe (error "no straight skeleton?") $ findStraightSkeleton contour []
    firstCell = findFirstCellOfContour contour divisions
    divisions = findDivisions contour $ fromMaybe (error "failed to crash") $ crashMotorcycles contour []
    motorcycle = head $ convexMotorcycles contour
    contour = randomConcaveChevronQuad x y tilt1 distance1 distance2
    x,y :: ℝ
    x = 0
    y = 0
    tilt1 = Radian 0.1
    distance1,distance2 :: Positive ℝ
    distance1 = 6.0
    distance2 = 1.0e-3

unit_ConcaveChevronQuadSample :: Bool
unit_ConcaveChevronQuadSample = error $ "dumping: \n"
                                             <> dumpGanjas [toGanja contour, toGanja (head $ convexMotorcycles contour)] <> "\n"
  where
    contour = randomConcaveChevronQuad x y tilt1 distance1 distance2
    x,y :: ℝ
    x = 0
    y = 0
    tilt1 = Radian 0.1
    distance1,distance2 :: Positive ℝ
    distance1 = 2.0
    distance2 = 1.0e-4

{-
       uncaught exception: ErrorCall
-}
unit_ConcaveChevronQuadNodeTreeHasOneGeneration_3 :: Bool
unit_ConcaveChevronQuadNodeTreeHasOneGeneration_3
  | res == 1 = True
  | otherwise = error $ "returned:\n"
                      <> show firstCell <> "\n"
                      <> dumpGanjas [toGanja finalNodeTree] <> "\n"
  where
    res = generationsOf finalNodeTree
    finalNodeTree = oneNodeTreeOf $ fromMaybe (error "no straight skeleton?") $ findStraightSkeleton contour []
    firstCell = findFirstCellOfContour contour divisions
    divisions = findDivisions contour $ fromMaybe (error "failed to crash") $ crashMotorcycles contour []
    contour = randomConcaveChevronQuad x y tilt1 distance1 distance2
    x,y :: ℝ
    x = 0
    y = 0
    tilt1 = Radian 0.1
    distance1,distance2 :: Positive ℝ
    distance1 = 2.0
    distance2 = 1.0e-4

-- | was broken, until we stopped treating a motorcycle<->ENode intersection like a side.
unit_ConcaveChevronQuadCanPlaceFaces :: Bool
unit_ConcaveChevronQuadCanPlaceFaces
  | length faces == 4 = True
  | otherwise = error $ "dumping: \n"
                <> dumpGanjas [toGanja contour, toGanja $ head $ convexMotorcycles contour] <> "\n"
                <> show (landingPointOf contour motorcycle) <> "\n"
                <> show (plinesIntersectIn (outAndErrOf motorcycle) (outAndErrOf eNode)) <> "\n"
                <> show divides <> "\n"
  where
    faces = facesOf skeleton
    skeleton = fromJust $ findStraightSkeleton contour []
    eNode = (\(WithENode a) -> a) $ landingPointOf contour motorcycle
    divides = findDivisions contour (fromMaybe (error "no") $ crashMotorcycles contour [])
    motorcycle = head $ convexMotorcycles contour
    contour = randomConcaveChevronQuad x y tilt1 distance1 distance2
    x,y :: ℝ
    x = 0
    y = 0
    tilt1 = Radian 1.0
    distance1,distance2 :: Positive ℝ
    distance1 = 1.0
    distance2 = 1.0

-- | was broken, until we did a logical check for if a motorcycle intersects a line segment that is part of an anticollinear enode.
unit_ConcaveChevronQuadCanPlaceFaces_2 :: Bool
unit_ConcaveChevronQuadCanPlaceFaces_2
  | length faces == 4 = True
  | otherwise = error $ "dumping: \n"
                <> show motorcycle <> "\n"
                <> show targetENode <> "\n"
                <> show (plinesIntersectIn (outAndErrOf motorcycle) (outAndErrOf targetENode)) <> "\n"
                <> dumpGanjas [toGanja motorcycle, toGanja targetENode] <> "\n"
                <> show (landingPointOf contour motorcycle) <> "\n"
                <> show (motorcycleIntersectsAt contour motorcycle) <> "\n"
                <> show divides <> "\n"
  where
    faces = facesOf skeleton
    skeleton = fromJust $ findStraightSkeleton contour []
    divides = findDivisions contour (fromMaybe (error "no") $ crashMotorcycles contour [])
    [_, targetENode, _] = eNodes
    eNodes = eNodesOfOutsideContour contour
    motorcycle = head $ convexMotorcycles contour
    contour = randomConcaveChevronQuad x y tilt1 distance1 distance2
    x,y :: ℝ
    x = 0
    y = 0
    tilt1 = Radian 0.2
    distance1,distance2 :: Positive ℝ
    distance1 = 3.0
    distance2 = 0.5

concaveChevronQuadBrokenSpec :: Spec
concaveChevronQuadBrokenSpec =
  describe "Concave Chevron Quads" $
    it "always fails, dumping a sample"
      unit_ConcaveChevronQuadSample

concaveChevronQuadSpec :: Spec
concaveChevronQuadSpec =
  describe "Concave Chevron Quads" $ do
    it "finds one motorcycle" $
      property (boolFromConcaveChevronQuad prop_OneMotorcycle)
    it "finds one divide" $
      property (expectationFromConcaveChevronQuad prop_OneDivide)
    it "finds a straight skeleton (unit)"
      unit_ConcaveChevronQuadHasAStraightSkeleton
    it "finds a straight skeleton" $
      property (expectationFromConcaveChevronQuad prop_HasAStraightSkeleton)
    it "only finds one nodetree in the straight skeleton" $
      property (expectationFromConcaveChevronQuad prop_StraightSkeletonHasOneNodeTree)
    it "generates one generation of INodes (unit)"
      unit_ConcaveChevronQuadNodeTreeHasOneGeneration
    it "generates one generation of INodes (unit 2)"
      unit_ConcaveChevronQuadNodeTreeHasOneGeneration_2
    it "generates one generation of INodes (unit 3)"
      unit_ConcaveChevronQuadNodeTreeHasOneGeneration_3
    it "generates one or two generations of INodes" $
      property (boolFromConcaveChevronQuad prop_NodeTreeHasFewerThanThreeGenerations)
    it "can place faces on the straight skeleton (unit)"
      unit_ConcaveChevronQuadCanPlaceFaces
    it "can place faces on the straight skeleton (unit 2)"
      unit_ConcaveChevronQuadCanPlaceFaces_2
    it "can place faces on the straight skeleton" $
      property (expectationFromConcaveChevronQuad prop_CanPlaceFaces)
    it "only places four faces" $
      property (expectationFromConcaveChevronQuad prop_HasFourFaces)
    it "places faces in the order the line segments were given" $
      property (expectationFromConcaveChevronQuad prop_FacesInOrder)
    -- NOTE: faces on a concave figgure can end up not all wound left.
  where
    boolFromConcaveChevronQuad :: (Contour -> Bool) -> ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Bool
    boolFromConcaveChevronQuad f x y rawFirstTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = f concaveChevronQuad
      where
        concaveChevronQuad = randomConcaveChevronQuad x y rawFirstTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    expectationFromConcaveChevronQuad :: (Contour -> Expectation) -> ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
    expectationFromConcaveChevronQuad f x y rawFirstTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = f concaveChevronQuad
      where
        concaveChevronQuad = randomConcaveChevronQuad x y rawFirstTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
