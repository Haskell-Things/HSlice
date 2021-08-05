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

-- |  This file contains the entry point for the logic and routines required for building
--    a Straight Skeleton of a contour, with a set of sub-contours cut out of it.
module Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton) where

import Prelude (($), (<$>), (==), error, (&&), null, filter, zip, Either(Right), (>), even, not)

import Data.Maybe( Maybe(Just,Nothing), catMaybes)

import Slist (safeLast, slist)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Definitions (Contour, mapWithFollower)

import Graphics.Slicer.Math.PGA (PLine2, PIntersection(PAntiCollinear), plinesIntersectIn)

import Graphics.Slicer.Math.Line (LineSeg)

import Graphics.Slicer.Math.Contour (contourIntersectionCount, lineSegsOfContour)

import Graphics.Slicer.Math.Skeleton.Cells (cellBefore, cellAfter, nodeTreesDoNotOverlap, addMirrorNodeTrees, simpleNodeTreeOfCell, contourToCell)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode(ENode), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), concavePLines, linePairs, outOf, pPointOf)

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
findStraightSkeleton contour holes =
  let
    foundCrashTree = crashMotorcycles contour holes
--    cells = cellsOf contour holes foundCrashTree
  in
    case foundCrashTree of
      Nothing -> Nothing
      (Just crashTree) -> if not $ null holes
                          then Nothing
                          else case motorcyclesIn crashTree of
                                 -- Simple case. convert the whole contour to a cell, and use the simple solver on it.
                                 (Slist _ 0) -> Just $ StraightSkeleton [[simpleNodeTreeOfCell $ contourToCell contour]] (slist [])
                                 (Slist [inMC] 1) -> if nodeTreesDoNotOverlap (cellAfter contour division) (cellBefore contour division) division
                                                     then -- Use simple NodeTrees.
                                                       Just $ addMirrorNodeTrees (cellAfter contour division) (cellBefore contour division) division
                                                     else -- Use the algorithm from Christopher Tscherne's master's thesis.
                                                       applyTscherne contour [division]
                                   where
                                     division = oneMCDivision crashTree inMC
                                 -- Divide into cells, and walk the tree.
                                 (Slist [firstMC,secondMC] 2) -> if lastCrashType crashTree == Just HeadOn && nodeTreesDoNotOverlap (cellAfter contour division) (cellBefore contour division) division
                                                                 then -- Use simple NodeTrees.
                                                                   Just $ addMirrorNodeTrees (cellAfter contour division) (cellBefore contour division) division
                                                                 else -- Use the algorithm from Christopher Tscherne's master's thesis.
                                                                   applyTscherne contour [division]
                                   where
                                     division = twoMCDivision crashTree firstMC secondMC
                                 (Slist _ _) -> Nothing
  where
    motorcyclesIn (CrashTree motorcycles _ _) = motorcycles
    oneMCDivision crashTree inMC = CellDivide (DividingMotorcycles inMC (Slist [] 0)) (maybeOpposingENodeOf crashTree)
    twoMCDivision crashTree firstMC secondMC = if lastCrashType crashTree == Just HeadOn
                                               then CellDivide (DividingMotorcycles firstMC (Slist [secondMC] 1)) Nothing
                                               else error "given two motorcycles that do not crash head on."

    -- | find nodes or motorcycles where the arc coresponding to them is collinear with the dividing Motorcycle.
    maybeOpposingENodeOf crashTree = case opposingNodes crashTree of
                                       [] -> Nothing
                                       [oneNode] -> Just oneNode
                                       (_:_) -> error "more than one opposing exterior node. cannot yet handle this situation."
      where
        opposingNodes :: CrashTree -> [ENode]
        opposingNodes myCrashTree = filter (\eNode -> enoughIntersections $ contourIntersectionCount contour (Right (pPointOf eNode, pPointOf $ dividingMotorcycle myCrashTree)))
                                           $ filter (\eNode -> plinesIntersectIn (outOf eNode) (outOf $ dividingMotorcycle myCrashTree) == PAntiCollinear) $ concaveENodes contour
          where
            enoughIntersections n = n > 0 && even n

    ------------------------------------------------------
    -- routines used when two motorcycles have been found.
    ------------------------------------------------------

    -- Determine the type of the last crash that occured. only useful when we're dealing with two motorcycles, and want to find out if we can treat them like one motorcycle.
    lastCrashType :: CrashTree -> Maybe CollisionType
    lastCrashType crashTree = case lastCrash crashTree of
                                (Just crash) -> if collisionResult crash == HeadOn
                                                then Just HeadOn
                                                else Nothing
                                Nothing -> Nothing
      where
          lastCrash :: CrashTree -> Maybe Collision
          lastCrash (CrashTree _ _ crashes) = case safeLast crashes of
                                                Nothing -> Nothing
                                                (Just crash) -> Just crash

    ---------------------------------------------------------
    -- routines used when a single motorcycle has been found.
    ---------------------------------------------------------
    -- when we have just a single dividing motorcycle, we can use tscherneCheat.
    dividingMotorcycle crashTree = case motorcyclesIn crashTree of
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

