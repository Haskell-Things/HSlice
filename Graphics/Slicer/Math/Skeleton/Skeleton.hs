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

module Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton) where

import Prelude (Bool(True), ($), (<$>), (==), error, length, (&&), null, filter, zip, Either(Right), (>), even)

import Data.Maybe( Maybe(Just,Nothing), catMaybes)

import Slist (safeLast)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Definitions (Contour, mapWithFollower)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode(ENode), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), concavePLines, linePairs, outOf, pPointOf)

import Graphics.Slicer.Math.PGA (PLine2, PIntersection(PCollinear), plinesIntersectIn)

import Graphics.Slicer.Math.Line (LineSeg)

import Graphics.Slicer.Math.Contour (contourIntersections, lineSegsOfContour)

import Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Motorcycles (CrashTree(CrashTree), CollisionType(HeadOn), Collision, crashMotorcycles, collisionResult)

import Graphics.Slicer.Math.Skeleton.Tscherne (applyTscherne)

----------------------------------------------------------------------------------
------------------- Straight Skeleton Calculation (Entry Point) ------------------
----------------------------------------------------------------------------------

-- | Find the StraightSkeleton of a given contour, with a given set of holes cut out of it.
--   Really, this is a dispatcher, to a series of algorithms for doing the actual work.
-- FIXME: Does not know how to calculate a straight skeleton for contours with holes, or more than one motorcycle.. or two motorcycles that are collinear.
-- FIXME: abusing Maybe until we can cover all cases.
findStraightSkeleton :: Contour -> [Contour] -> Maybe StraightSkeleton
findStraightSkeleton contour holes = case foundCrashTree of
  Nothing -> Nothing
  (Just crashTree) -> if null holes
                      then case motorcyclesIn crashTree of
                             (Slist _ 0) -> Just $ StraightSkeleton [[skeletonOfConcaveRegion (lineSegsOfContour contour) True]] []
                             -- Use the algorithm from Christopher Tscherne's master's thesis.
                             (Slist [inMC] 1) -> applyTscherne contour [CellDivide (DividingMotorcycles inMC (Slist [] 0)) maybeOpposingENode]
                             (Slist [firstMC,secondMC] 2) -> if lastCrashType == Just HeadOn
                                                then applyTscherne contour [CellDivide (DividingMotorcycles firstMC (Slist [secondMC] 1)) maybeOpposingENode]
                                                else Nothing
                             (Slist _ _) -> Nothing
                      else Nothing
  where
    foundCrashTree = crashMotorcycles contour holes
    motorcyclesIn (CrashTree motorcycles _ _) = motorcycles

    -- | find nodes or motorcycles where the arc coresponding to them is collinear with the dividing Motorcycle.
    maybeOpposingENode = case opposingNodes of
                         [] -> Nothing
                         [oneNode] -> Just oneNode
                         (_:_) -> error "more than one opposing exterior node. impossible situation."
      where
        opposingNodes :: [ENode]
        opposingNodes = filter (\eNode -> enoughIntersections $ length (contourIntersections contour (Right (pPointOf eNode, pPointOf dividingMotorcycle))))
                        $ filter (\eNode -> plinesIntersectIn (outOf eNode) (outOf dividingMotorcycle) == PCollinear) $ concaveENodes contour
          where
            enoughIntersections n = n > 0 && even n

    ------------------------------------------------------
    -- routines used when two motorcycles have been found.
    ------------------------------------------------------

    -- Determine the type of the last crash that occured. only useful when we're dealing with two motorcycles, and want to find out if we can treat them like one motorcycle.
    lastCrashType = case lastCrash foundCrashTree of
                      (Just crash) -> if collisionResult crash == HeadOn
                                      then Just HeadOn
                                      else Nothing
                      Nothing -> Nothing
        where
          lastCrash :: Maybe CrashTree -> Maybe Collision
          lastCrash (Just (CrashTree _ _ crashes)) = case safeLast crashes of
                                                       Nothing -> Nothing
                                                       (Just crash) -> Just crash
          lastCrash _ = Nothing

    ---------------------------------------------------------
    -- routines used when a single motorcycle has been found.
    ---------------------------------------------------------
    -- when we have just a single dividing motorcycle, we can use tscherneCheat.
    dividingMotorcycle = case foundCrashTree of
                           Nothing -> error "no crash tree?"
                           (Just crashTree) -> case motorcyclesIn crashTree of
                                                 (Slist [inMC] 1) -> inMC
                                                 (Slist _ _) -> error "cannot handle anything but one motorcycle."

-- | Find the non-reflex virtexes of a contour, and create ENodes from them.
--   This function is meant to be used on the exterior contour.
-- FIXME: merge this with the same logic in Concave.
concaveENodes :: Contour -> [ENode]
concaveENodes contour = catMaybes $ onlyNodes <$> zip (linePairs contour) (mapWithFollower concavePLines $ lineSegsOfContour contour)
  where
    onlyNodes :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe ENode
    onlyNodes ((seg1, seg2), Just pLine) = Just $ ENode (seg1,seg2) pLine
    onlyNodes ((_, _), Nothing) = Nothing

