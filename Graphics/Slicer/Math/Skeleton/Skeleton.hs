{-
 - Copyright 2021 Julia Longtin
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

{- Purpose of this file: to hold the logic and routines required for building
   a Straight Skeleton of a contour, with a set of sub-contours cut out of it.
   This file will require calling out to other files for applying algorithms.
-}

-- inherit instances when deriving.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

-- So we can section tuples
{-# LANGUAGE TupleSections #-}

module Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton) where

import Prelude (Bool(True), otherwise, ($), (<$>), (==), error, length, (&&), head, null, filter, zip)
  
import Graphics.Slicer.Math.Definitions (Contour, mapWithFollower)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode(ENode), Motorcycle(Motorcycle), Arcable(outOf), linesOfContour, convexMotorcycles, linePairs)

import Graphics.Slicer.Math.PGA (lineIsLeft, PLine2(PLine2), plinesIntersectIn, PIntersection(PCollinear), eToPLine2, flipPLine2)

import Data.Maybe( Maybe(Just,Nothing), catMaybes)

import Graphics.Slicer.Math.Line (LineSeg)

import Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Tscherne (tscherneMerge, leftRegion, rightRegion)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPair)


--------------------------------------------------------------------
--------------- Straight Skeleton Calculation ----------------------
--------------------------------------------------------------------

-- | Find the StraightSkeleton of a given contour, with a given set of holes cut out of it.
-- FIXME: Does not know how to calculate a straight skeleton for contours with holes, or more than one motorcycle. 
-- FIXME: abusing Maybe until we can cover all cases.
findStraightSkeleton :: Contour -> [Contour] -> Maybe StraightSkeleton
findStraightSkeleton contour holes
  | null holes && null outsideContourMotorcycles        = Just $ StraightSkeleton [[skeletonOfConcaveRegion (linesOfContour contour) True]] []
  -- Use the algorithm from Christopher Tscherne's master's thesis.
  | null holes && length outsideContourMotorcycles == 1 = Just $ tscherneMerge dividingMotorcycle maybeOpposingNode leftSide rightSide
  | otherwise = Nothing
  where
    outsideContourMotorcycles = convexMotorcycles contour
    -- | not yet used.
    -- motorcyclesOfHoles = concaveMotorcycles <$> holes

    ---------------------------------------------------------
    -- routines used when a single motorcycle has been found.
    ---------------------------------------------------------
    dividingMotorcycle = head outsideContourMotorcycles
    leftSide  = leftRegion contour dividingMotorcycle
    rightSide = rightRegion contour dividingMotorcycle
    -- | find nodes where the arc coresponding to them is collinear with the dividing Motorcycle.
    -- FIXME: this is implemented wrong. it needs to find only the one node opposing the dividing motorcycle, not every line segment that could be an opposing node.
    -- FIXME: we should construct a line segment from the point of the node to the point of the motorcycle, and keep the one that intersects the contour an even amount of times?
    maybeOpposingNode
      | length outsideContourMotorcycles == 1 && null opposingNodes        = Nothing
      | length outsideContourMotorcycles == 1 && length opposingNodes == 1 = Just $ head opposingNodes
      | otherwise                                                          = error "more than one opposing node. impossible situation."
      where
        opposingNodes = filter (\eNode -> plinesIntersectIn (outOf eNode) (pathOf dividingMotorcycle) == PCollinear) $ concaveENodes contour
        pathOf (Motorcycle _ path) = path

-- | Find the non-reflex virtexes of a contour, and create ENodes from them.
--   This function is meant to be used on the exterior contour.
concaveENodes :: Contour -> [ENode]
concaveENodes contour = catMaybes $ onlyNodes <$> zip (linePairs contour) (mapWithFollower concavePLines $ linesOfContour contour)
  where
    onlyNodes :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe ENode
    onlyNodes ((seg1, seg2), Just pLine) = Just $ ENode (seg1,seg2) pLine 
    onlyNodes ((_, _), Nothing) = Nothing 

-- | Examine two line segments that are part of a Contour, and determine if they are concave toward the interior of the Contour. if they are, construct a PLine2 bisecting them, pointing toward the interior of the Contour.
concavePLines :: LineSeg -> LineSeg -> Maybe PLine2
concavePLines seg1 seg2
  | Just True == lineIsLeft seg1 seg2  = Just $ PLine2 $ addVecPair pv1 pv2
  | otherwise                          = Nothing
  where
    (PLine2 pv1) = eToPLine2 seg1
    (PLine2 pv2) = flipPLine2 $ eToPLine2 seg2
