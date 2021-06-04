{- ORMOLU_DISABLE -}
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
{-# LANGUAGE DerivingStrategies #-}


module Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton) where

import Prelude (Bool(True), otherwise, ($), (<$>), (==), error, length, (&&), head, null, filter, zip, Either(Right), last, (>), even)
  
import Graphics.Slicer.Math.Definitions (Contour, mapWithFollower)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode(ENode), CellDivide(CellDivide), concavePLines, linePairs, outOf, pPointOf)

import Graphics.Slicer.Math.PGA (PLine2, PIntersection(PCollinear), plinesIntersectIn)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, isJust, fromJust, isNothing)

import Graphics.Slicer.Math.Line (LineSeg)

import Graphics.Slicer.Math.Contour (contourIntersections, linesOfContour)

import Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Motorcycles (CrashTree(CrashTree), Collision(HeadOn), Crash, crashMotorcycles, collisionResult)

import Graphics.Slicer.Math.Skeleton.Tscherne (applyTscherne)

----------------------------------------------------------------------------------
------------------- Straight Skeleton Calculation (Entry Point) ------------------
----------------------------------------------------------------------------------

-- | Find the StraightSkeleton of a given contour, with a given set of holes cut out of it.
--   Really, this is a dispatcher, to a series of algorithms for doing the actual work.
-- FIXME: Does not know how to calculate a straight skeleton for contours with holes, or more than one motorcycle.. or two motorcycles that are collinear.
-- FIXME: abusing Maybe until we can cover all cases.
findStraightSkeleton :: Contour -> [Contour] -> Maybe StraightSkeleton
findStraightSkeleton contour holes
  | isNothing foundCrashTree                                                                 = Nothing
  | null holes && null (motorcyclesIn foundCrashTree)                                        = Just $ StraightSkeleton [[skeletonOfConcaveRegion (linesOfContour contour) True]] []
  -- Use the algorithm from Christopher Tscherne's master's thesis.
  | null holes && length (motorcyclesIn foundCrashTree) == 1                                 = applyTscherne contour [CellDivide (motorcyclesIn foundCrashTree) maybeOpposingENode]
  | null holes && length (motorcyclesIn foundCrashTree) == 2 && lastCrashType == Just HeadOn = applyTscherne contour [CellDivide (motorcyclesIn foundCrashTree) maybeOpposingENode]
  | otherwise = Nothing
  where
    foundCrashTree = crashMotorcycles contour holes
    motorcyclesIn (Just (CrashTree motorcycles _ _)) = motorcycles
    motorcyclesIn Nothing = []

    -- | find nodes or motorcycles where the arc coresponding to them is collinear with the dividing Motorcycle.
    maybeOpposingENode
      | null opposingNodes           = Nothing
      | length opposingNodes == 1    = Just $ head opposingNodes
      | otherwise                    = error "more than one opposing node. impossible situation."
      where
        opposingNodes :: [ENode]
        opposingNodes = filter (\eNode -> enoughIntersections $ length (contourIntersections contour (Right $ pPointOf eNode) (Right $ pPointOf dividingMotorcycle)))
                        $ filter (\eNode -> plinesIntersectIn (outOf eNode) (outOf dividingMotorcycle) == PCollinear) $ concaveENodes contour
          where
            enoughIntersections n = n > 0 && even n

    ------------------------------------------------------
    -- routines used when two motorcycles have been found.
    ------------------------------------------------------

    -- Determine the type of the last crash that occured. only useful when we're dealing with two motorcycles, and want to find out if we can treat them like one motorcycle.
    lastCrashType
      | isJust (lastCrash foundCrashTree) && collisionResult (fromJust $ lastCrash foundCrashTree) == HeadOn = Just HeadOn
      | otherwise = Nothing
        where
          lastCrash :: Maybe CrashTree -> Maybe Crash
          lastCrash (Just (CrashTree _ _ crashes)) = Just $ last $ last crashes
          lastCrash _ = Nothing

    ---------------------------------------------------------
    -- routines used when a single motorcycle has been found.
    ---------------------------------------------------------
    -- when we have just a single dividing motorcycle, we can use tscherneCheat.
    dividingMotorcycle = head (motorcyclesIn foundCrashTree)

-- | Find the non-reflex virtexes of a contour, and create ENodes from them.
--   This function is meant to be used on the exterior contour.
-- FIXME: merge this with the same logic in Concave.
concaveENodes :: Contour -> [ENode]
concaveENodes contour = catMaybes $ onlyNodes <$> zip (linePairs contour) (mapWithFollower concavePLines $ linesOfContour contour)
  where
    onlyNodes :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe ENode
    onlyNodes ((seg1, seg2), Just pLine) = Just $ ENode (seg1,seg2) pLine 
    onlyNodes ((_, _), Nothing) = Nothing 

