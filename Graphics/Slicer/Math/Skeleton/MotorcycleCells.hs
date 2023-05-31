{- ORMOLU_DISABLE -}
{-
 - Copyright 2023 Julia Longtin
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

-- | This file contains the logic for operating on MotorcycleClusters.
module Graphics.Slicer.Math.Skeleton.MotorcycleCells (
  allMotorcycleCells,
  findClusters,
  motorcyclesAlongEdgeOf,
  simplifyCluster
  ) where

import Prelude (Bool(False, True), ($), (&&), (||), (==), (<=), (<$>), all, error, mempty, not, null, otherwise)

import Data.Either (Either(Left, Right))

import Data.List (concatMap, filter)

import qualified Data.List as DL (head)

import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, isJust)

import qualified Slist as SL (head, last)

import Slist.Type (Slist(Slist), len, slist)

import Graphics.Slicer.Math.Definitions (Contour, lineSegsOfContour, makeLineSeg, mapWithFollower)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.Intersections (intersectionBetweenArcsOf, isAntiCollinear)

import Graphics.Slicer.Math.Lossy (eToPLine2, pToEPoint2)

import Graphics.Slicer.Math.PGA (Pointable(ePointOf), angleBetween2PL, outOf, outAndErrOf)

import Graphics.Slicer.Math.Skeleton.Cells (addNodeTreesAlongDivide, findDivisions, findFirstCellOfContour, findNextCell, getRawNodeTreeOfCell, landingPointOf, nodeTreesDoNotOverlap)

import Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (ENodeSet(ENodeSet), MotorcycleCluster(MotorcycleCluster), NodeTree(NodeTree), RemainingContour(RemainingContour), StraightSkeleton(StraightSkeleton), Motorcycle, Cell, CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), MotorcycleCell(MotorcycleCell), MotorcycleIntersection(WithMotorcycle))

import Graphics.Slicer.Math.Skeleton.Motorcycles (CollisionType(HeadOn), CrashTree(CrashTree), lastCrashType)

import Graphics.Slicer.Math.Skeleton.Tscherne (tscherneMerge)

-- | find the groups of motorcycles dividing up the contour, along with the motorcycleCells that border each motorcycle group.
-- Note: it is possible for a motorcycleCell to belong to multiple motorcycle clusters
findClusters :: Contour -> CrashTree -> Either [MotorcycleCluster] StraightSkeleton
findClusters contour crashTree = case motorcyclesIn crashTree of
                                    (Slist [] _) -> Right $ StraightSkeleton (slist [[ skeletonOfConcaveRegion $ slist [lineSegsOfContour contour]]]) mempty
                                    (Slist [inMC] _) -> Left [MotorcycleCluster (slist $ allMotorcycleCells contour crashTree) (CellDivide (DividingMotorcycles inMC mempty) $ landingPointOf contour inMC)]
                                    (Slist [firstMC,secondMC] _) -> if lastCrashType crashTree == Just HeadOn
                                                                    then
                                                                      Left [MotorcycleCluster (slist $ allMotorcycleCells contour crashTree) (CellDivide (DividingMotorcycles firstMC mempty) $ WithMotorcycle secondMC)]
                                                                    else if intersectionIsBehind firstMC || intersectionIsBehind secondMC
                                                                         then -- These motorcycles cannot intersect.
                                                                           Left [MotorcycleCluster (sListFromPair $ motorcycleCellsAlongMotorcycle contour crashTree firstMC) (CellDivide (DividingMotorcycles firstMC mempty) $ landingPointOf contour firstMC),
                                                                                 MotorcycleCluster (sListFromPair $ motorcycleCellsAlongMotorcycle contour crashTree secondMC) (CellDivide (DividingMotorcycles secondMC mempty) $ landingPointOf contour secondMC)]
                                                                         else
                                                                           -- FIXME: We should be able to see if these intersect inside of the contour.
                                                                           -- LOWHANGINGFRUIT: what about two motorcycles that are anticolinear?
                                                                           error "don't know what to do with these motorcycles."
                                      where
                                        sListFromPair (a,b) = slist [a,b]
                                        intersectionIsBehind m = angleFound <= ulpVal angleErr
                                          where
                                            (angleFound, (_,_, angleErr)) = angleBetween2PL (outOf m) (eToPLine2 $ lineSegToIntersection m)
                                        lineSegToIntersection m = makeLineSeg (ePointOf m) (pToEPoint2 intersectionPPoint)
                                        (intersectionPPoint, _) = fromMaybe (error "has arcs, but no intersection?") $ intersectionBetweenArcsOf firstMC secondMC
                                    (Slist (_:_) _) -> error "too many motorcycles."
  where
    motorcyclesIn (CrashTree motorcycles _ _) = motorcycles

simplifyCluster :: MotorcycleCluster -> Either MotorcycleCluster StraightSkeleton
simplifyCluster motorcycleCluster@(MotorcycleCluster cells cellDivide)
  | cellDivideIsSimple cellDivide && len cells == 2 && allCellsCanSimplify = simplifyCellPair (SL.head cells) (SL.last cells)
  | otherwise = Left motorcycleCluster
    where
      cellDivideIsSimple (CellDivide (DividingMotorcycles _ (Slist _ 0)) _) = True
      cellDivideIsSimple _ = False
      allCellsCanSimplify = all (== True) $ hasOneMotorcycle <$> cells
      hasOneMotorcycle (MotorcycleCell _ _ cellMotorcycles _) = len cellMotorcycles == 1
      simplifyCellPair :: MotorcycleCell -> MotorcycleCell -> Either MotorcycleCluster StraightSkeleton
      simplifyCellPair cell1@(MotorcycleCell _ _ _ nodeTreeA) cell2@(MotorcycleCell _ _ _ nodeTreeB)
       | nodeTreesDoNotOverlap nodeTreeA nodeTreeB cellDivide = Right $ StraightSkeleton (slist [[ addNodeTreesAlongDivide nodeTreeA nodeTreeB cellDivide ]]) mempty
       | otherwise = Right $ tscherneMerge cell1 cell2 cellDivide

-- | divide the contour into motorcycle cells.
allMotorcycleCells :: Contour -> CrashTree -> [MotorcycleCell]
allMotorcycleCells contour crashTree = firstMotorcycleCell : allMotorcycleCells' maybeFirstRemainder
  where
    ((firstCell, maybeCellDivide), maybeFirstRemainder) = findFirstCellOfContour contour $ findDivisions contour crashTree
    firstMotorcycleCell = MotorcycleCell sidesOfFirstCell mempty firstCellMotorcycles rawNodeTreeOfFirstCell
      where
        firstCellMotorcycles
          | isJust maybeCellDivide = motorcyclesAlongEdgeOf firstCell $ fromJust maybeCellDivide
          | otherwise = mempty
        rawNodeTreeOfFirstCell@(NodeTree (ENodeSet sidesOfFirstCell) _) = getRawNodeTreeOfCell firstCell
    allMotorcycleCells' :: Maybe [RemainingContour] -> [MotorcycleCell]
    allMotorcycleCells' maybeRemainders
     | maybeRemainders == Just [] = []
     | otherwise = case maybeRemainders of
                     Nothing -> []
                     (Just []) -> error "was given an empty list of remainders."
                     (Just [oneRemainder]) -> [oneMotorcycleCellFrom oneRemainder]
                     (Just (oneRemainder:others)) -> (oneMotorcycleCellFrom oneRemainder) :  concatMap (\a -> allMotorcycleCells' $ Just [a]) others
      where
        oneMotorcycleCellFrom remainder@(RemainingContour _ inDivide _) = MotorcycleCell sidesOfCell mempty motorcyclesOfCell rawNodeTreeOfCell
          where
            ((cell, _), _) = findNextCell remainder
            motorcyclesOfCell
              | isJust inDivide = motorcyclesAlongEdgeOf cell $ fromJust inDivide
              | otherwise = error "no divide?"
            rawNodeTreeOfCell@(NodeTree (ENodeSet sidesOfCell) _) = getRawNodeTreeOfCell cell

-- | Find the motorcycles that are part of a cellDivide, and are at the edge of this cell.
motorcyclesAlongEdgeOf :: Cell -> CellDivide -> Slist Motorcycle
motorcyclesAlongEdgeOf _ cellDivide = case cellDivide of
                                             (CellDivide (DividingMotorcycles m1 (Slist _ 0)) _) -> slist [m1]
                                             (CellDivide (DividingMotorcycles m1 mx@(Slist mxRaw _)) _) -> if len mx == 1 && isAntiCollinear (outAndErrOf m1) (outAndErrOf $ SL.head mx)
                                                                                                           then slist $ m1 : mxRaw
                                                                                                           else error "multiple dividing motorcycles, and not collinear."

-- | Find the pair of motorcycle cells that 
motorcycleCellsAlongMotorcycle :: Contour -> CrashTree -> Motorcycle -> (MotorcycleCell, MotorcycleCell)
motorcycleCellsAlongMotorcycle contour crashTree motorcycle = (firstCell, secondCell)
  where
    (firstCell, secondCell) = DL.head $ filter (\(a,b) -> testCells a b) $ mapWithFollower (\a b -> (a,b)) $ allMotorcycleCells contour crashTree
    testCells a b = a `hasMotorcycle` motorcycle && b `hasMotorcycle` motorcycle
      where
        hasMotorcycle :: MotorcycleCell -> Motorcycle -> Bool
        hasMotorcycle (MotorcycleCell _ _ (Slist motorcycles _) _) target = not $ null $ filter (== target) motorcycles
{-
-- | find groups of motorcycles in a given contour without holes.
findClumps :: Contour -> CrashTree -> Slist [Motorcycle]
findClumps contour crashTree = case motorcyclesIn crashTree of
                                    (Slist [] _) -> []
                                    (Slist [inMC] _) -> [CellDivide (DividingMotorcycles inMC (Slist [] 0)) $ landingPointOf contour inMC]
                                    (Slist [firstMC,secondMC] _) -> case findDivides crashTree of
                                                                      [] -> error "no divides, but two motorcycles?"
                                                                      [_] -> [CellDivide (DividingMotorcycles firstMC (Slist [] 0)) $ WithMotorcycle secondMC]
                                                                      [_,_] ->
                                                                           [CellDivide (DividingMotorcycles firstMC (Slist [] 0)) $ landingPointOf contour firstMC,
                                                                            CellDivide (DividingMotorcycles secondMC (Slist [] 0)) $ landingPointOf contour secondMC]
                                                                      (_:_) -> error "don't know what to do with these line segments."
                                    (Slist (_:_) _) -> error "too many motorcycles."
  where
    -- | find the divides
    findDivides :: CrashTree -> [[Motorcycle]]
    findDivides myCrashTree = case motorcyclesIn myCrashTree of
                                             (Slist [] _) -> []
                                             (Slist [inMC] _) -> [[inMC]]
                                             (Slist [firstMC, secondMC] _) -> if lastCrashType myCrashTree == Just HeadOn
                                                                              then [[firstMC, secondMC]]
                                                                              else if intersectionIsBehind firstMC || intersectionIsBehind secondMC
                                                                                   then -- These motorcycles cannot intersect.
                                                                                     [[firstMC],[secondMC]]
                                                                                   else -- FIXME: We should be able to see if these intersect inside of the contour.
                                                                                     -- LOWHANGINGFRUIT: what about two motorcycles that are anticolinear?
                                                                                     error "don't know what to do with these motorcycles."
                                               where
                                                 intersectionIsBehind m = angleFound <= ulpVal angleErr
                                                   where
                                                     (angleFound, (_,_, angleErr)) = angleBetween2PL (outOf m) (eToPLine2 $ lineSegToIntersection m)
                                                 lineSegToIntersection m = makeLineSeg (ePointOf m) (pToEPoint2 intersectionPPoint)
                                                 (intersectionPPoint, _) = fromMaybe (error "has arcs, but no intersection?") $ intersectionBetweenArcsOf firstMC secondMC
                                             (Slist (_:_) _) -> error "too many motorcycles."
    motorcyclesIn (CrashTree motorcycles _ _) = motorcycles
    -- | find where the last motorcycle of a divide lands
    landingPointOf :: Contour -> Motorcycle -> MotorcycleIntersection
    landingPointOf myContour myMotorcycle =
      case eNodesInPath of
        [] -> WithLineSeg $ fst $ motorcycleIntersectsAt myContour myMotorcycle
        [oneNode] -> if motorcycleENodeDistance <= motorcycleLineSegDistance
                     then WithENode oneNode
                     else WithLineSeg $ fst $ motorcycleIntersectsAt myContour myMotorcycle
          where
            cMotorcyclePoint = cPPointAndErrOf myMotorcycle
            cNodePoint = cPPointAndErrOf oneNode
            motorcycleENodeDistance = distanceBetweenPPointsWithErr cMotorcyclePoint cNodePoint
            motorcycleLineSegDistance = distanceBetweenPPointsWithErr cMotorcyclePoint $ fromMaybe (error "no outArc?") $ outputIntersectsPLineAt myMotorcycle (eToPL $ fst $ motorcycleIntersectsAt myContour myMotorcycle)
        (_:_) -> error "more than one opposing exterior node. cannot yet handle this situation."
      where
        eNodesInPath = opposingNodes myContour myMotorcycle
          where
            opposingNodes :: Contour -> Motorcycle -> [ENode]
            opposingNodes c m = filter (\eNode -> isAntiCollinear (outAndErrOf eNode) (outAndErrOf m)) $ eNodesOfOutsideContour c

-}
