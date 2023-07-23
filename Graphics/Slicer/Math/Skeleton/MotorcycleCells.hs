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
  mergeClusterPair,
  simplifyCluster
  ) where

import Prelude (Bool(False), Int, (.), ($), (+), (<>), (&&), (||), (/=), (==), (<=), (<$>), all, error, fst, mempty, null, otherwise, show, snd, uncurry)

import Data.Either (Either(Left, Right))

import Data.List (concatMap, elem, filter, length)

import qualified Data.List as DL (head)

import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, isJust)

import qualified Slist as SL (head, last, filter)

import Slist.Type (Slist(Slist), len, slist)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg, lineSegsOfContour, makeLineSeg, mapWithFollower)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.Intersections (intersectionBetweenArcsOf, isAntiCollinear)

import Graphics.Slicer.Math.Lossy (eToPLine2, pToEPoint2)

import Graphics.Slicer.Math.PGA (Pointable(ePointOf), angleBetween2PL, outOf, outAndErrOf)

import Graphics.Slicer.Math.Skeleton.Cells (INodeDirection (TowardIn, TowardOut), addNodeTreesAlongDivide, crossoverLinesOfDivision, findDivisions, findFirstCellOfContour, findNextCell, getRawNodeTreeOfCell, landingPointOf, matchDirectionOfSegments, nodeTreeFromDivision, nodeTreesDoNotOverlap, redirectLastOut)

import Graphics.Slicer.Math.Skeleton.Concave (errorIfLeft, skeletonOfNodes, skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode, ENodeSet(ENodeSet), MotorcycleCluster(MotorcycleCluster), NodeTree(NodeTree), RemainingContour(RemainingContour), StraightSkeleton(StraightSkeleton), Motorcycle, CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), MotorcycleCell(MotorcycleCell), MotorcycleIntersection(WithMotorcycle), finalINodeOf, getFirstLineSeg, getLastLineSeg, eNodesOfSide)

import Graphics.Slicer.Math.Skeleton.Motorcycles (CollisionType(HeadOn), CrashTree(CrashTree), lastCrashType)

import Graphics.Slicer.Math.Skeleton.NodeTrees (MaybeMatch(FirstLast, LastFirst, NoMatch), firstSegOf, lastSegOf, makeNodeTree, mergeNodeTrees)

import Graphics.Slicer.Math.Skeleton.Tscherne (tscherneMerge)

-- | find the groups of motorcycles dividing up the contour, along with the motorcycleCells that border each motorcycle group.
-- Note: it is possible for a motorcycleCell to belong to multiple motorcycle clusters
findClusters :: Contour -> CrashTree -> Maybe (Either [MotorcycleCluster] StraightSkeleton)
findClusters contour crashTree = case motorcyclesIn crashTree of
                                    (Slist [] _) -> Just $ Right $ StraightSkeleton (slist [[ skeletonOfConcaveRegion (slist [lineSegsOfContour contour]) [] ]]) mempty
                                    (Slist [inMC] _) -> Just $ Left [MotorcycleCluster (slist $ allMotorcycleCells contour crashTree) (CellDivide (DividingMotorcycles inMC mempty) $ landingPointOf contour inMC)]
                                    (Slist [firstMC,secondMC] _) -> if lastCrashType crashTree == Just HeadOn
                                                                    then
                                                                      Just $ Left [MotorcycleCluster (slist $ allMotorcycleCells contour crashTree) (CellDivide (DividingMotorcycles firstMC mempty) $ WithMotorcycle secondMC)]
                                                                    else if intersectionIsBehind firstMC || intersectionIsBehind secondMC
                                                                         then -- These motorcycles cannot intersect.
                                                                           Just $ Left [MotorcycleCluster (sListFromPair $ motorcycleCellsAlongMotorcycle contour crashTree firstMC) (CellDivide (DividingMotorcycles firstMC mempty) $ landingPointOf contour firstMC),
                                                                                        MotorcycleCluster (sListFromPair $ motorcycleCellsAlongMotorcycle contour crashTree secondMC) (CellDivide (DividingMotorcycles secondMC mempty) $ landingPointOf contour secondMC)]
                                                                         else
                                                                           Nothing

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

-- Try to eliminate the motorcycle from the cluster.
simplifyCluster :: MotorcycleCluster -> Either MotorcycleCluster StraightSkeleton
simplifyCluster cluster@(MotorcycleCluster cells cellDivide)
  | isSimple (divideOfCluster cluster) && cellsInCluster cluster == 2 && all cellHasOneMotorcycle cells = Right $ simplifyCellPair (SL.head cells) (SL.last cells)
  | otherwise = Left cluster
    where
      simplifyCellPair :: MotorcycleCell -> MotorcycleCell -> StraightSkeleton
      simplifyCellPair cell1@(MotorcycleCell _ _ _ nodeTreeA) cell2@(MotorcycleCell _ _ _ nodeTreeB)
       | nodeTreesDoNotOverlap nodeTreeA nodeTreeB cellDivide = StraightSkeleton (slist [[ addNodeTreesAlongDivide nodeTreeA nodeTreeB cellDivide ]]) mempty
       | otherwise = tscherneMerge cell1 cell2 cellDivide

-- Try to eliminate A motorcycle from one of the two clusters.
mergeClusterPair :: MotorcycleCluster -> MotorcycleCluster -> Either (MotorcycleCluster, MotorcycleCluster) MotorcycleCluster
mergeClusterPair cluster1 cluster2
  | ((isSimple (divideOfCluster cluster1) && cellsInCluster cluster1 == 2) ||
     (isSimple (divideOfCluster cluster2) && cellsInCluster cluster2 == 2)) && hasOneJoiningCell = Right res
  | otherwise = Left (cluster1, cluster2)
  where
    res
      | isSimple (divideOfCluster cluster1) && cellsInCluster cluster1 == 2 = addMotorcycleCellToCluster (joinCellPairWithDivide (DL.head $ cluster1 `cellsWithoutJoiner` joiningCell) (divideOfCluster cluster1) joiningCell)
                                                                                                         (cluster2 `withoutJoiner` joiningCell)
      | isSimple (divideOfCluster cluster2) && cellsInCluster cluster2 == 2 = addMotorcycleCellToCluster (joinCellPairWithDivide (DL.head $ cluster2 `cellsWithoutJoiner` joiningCell) (divideOfCluster cluster2) joiningCell)
                                                                                                         (cluster1 `withoutJoiner` joiningCell)
      | otherwise = error $ "cannot merge clusters:\n " <> show cluster1 <> "\n" <> show cluster2 <> "\n" <> show joiningCell <> "\n"
      where
        cellsWithoutJoiner :: MotorcycleCluster -> MotorcycleCell -> [MotorcycleCell]
        cellsWithoutJoiner cluster joiner = filter (/= joiner) $ cellsOfCluster cluster
        withoutJoiner :: MotorcycleCluster -> MotorcycleCell -> MotorcycleCluster
        withoutJoiner cluster@(MotorcycleCluster _ divide) joiner = MotorcycleCluster (slist $ cellsWithoutJoiner cluster joiner) divide
    addMotorcycleCellToCluster :: MotorcycleCell -> MotorcycleCluster -> MotorcycleCluster
    addMotorcycleCellToCluster newCell (MotorcycleCluster (Slist rawCells _) divide)  = MotorcycleCluster (slist $ newCell : rawCells) divide 
    joiningCell = case joiningCells of
                    [] -> error "failed to find any joining cells."
                    [a] -> a
                    (_:_) -> error "found too many joining cells?"
    cellsOfCluster (MotorcycleCluster (Slist rawCells _) _) = rawCells
    hasOneJoiningCell = length joiningCells == 1
    joiningCells = filter (cellHasMotorcycleOfCluster cluster2) $ cellsOfCluster cluster1
    cellHasMotorcycleOfCluster cluster@(MotorcycleCluster _ (CellDivide (DividingMotorcycles motorcycle _) _)) cell
      | isSimple (divideOfCluster cluster) = cellHasMotorcycle cell motorcycle
      | otherwise = error "trying to check for the motorcycle of a cluster that has a complex (not 1D) divide."

-- Eliminate a cell divide, given one one-sided motorcycle cell, and one two-sided motorcycle cell. always returns a one-sided motorcycle cell.
joinCellPairWithDivide :: MotorcycleCell -> CellDivide -> MotorcycleCell -> MotorcycleCell
joinCellPairWithDivide completeCell@(MotorcycleCell _ _ completeMotorcycles completeNodeTree) divide partialCell@(MotorcycleCell partialSides@(Slist rawPartialSides _) partialFloatingSegments partialMotorcycles _)
  | foundSides completeCell /= 1 = error "first argument must be a complete side."
  | foundSides partialCell == 2 && len partialSides == 1 && len partialFloatingSegments == 1 = MotorcycleCell ((\(NodeTree (ENodeSet a) _) -> a) newNodeTree)  mempty newMotorcycles newNodeTree
  | foundSides partialCell == 2 = error $ "merging complete cell:\n" <> show completeCell <> "\nwith partial cell:\n" <> show partialCell <> "\nalong divide:\n" <> show divide <> "\n"
  | otherwise = error $ "cannot handle partial cell with " <> show (foundSides partialCell) <> " sides."
  where
    newMotorcycles = SL.filter (/= SL.head completeMotorcycles) partialMotorcycles
    newNodeTree = mergeNodeTrees [newNodeTreeSeed, adjustedNewPartialNodeTree]
    newNodeTreeSeed = mergeNodeTrees [adjustedCompleteNodeTree, divisionNodeTree]
    (adjustedCompleteNodeTree, adjustedNewPartialNodeTree)
      | matchDirection == FirstLast = (redirectLastOut completeNodeTree crossoverLine1,
                                       redirectLastOut newPartialNodeTree crossoverLine2)
      | otherwise                   = (redirectLastOut completeNodeTree crossoverLine2,
                                       redirectLastOut newPartialNodeTree crossoverLine1)
    -- Now that we have a closing divide, re-render the Cell's NodeTree with it.
    newPartialNodeTree = makeNodeTree rawPartialSides $ errorIfLeft $ skeletonOfNodes False inSegSets inSegSets [finalINodeOf $ fromJust iNodeSetOfDivide]
    inSegSets = slist [segmentsOfENodes $ concatMap eNodesOfSide rawPartialSides]
    segmentsOfENodes :: [ENode] -> [LineSeg]
    segmentsOfENodes [] = []
    segmentsOfENodes [a] = getFirstLineSeg a : [getLastLineSeg a]
    segmentsOfENodes (x:xs) = getFirstLineSeg x : (getLastLineSeg <$> x:xs)

    -- when we create an INode for the divide, we must point toward the open end.
    divisionNodeTree@(NodeTree _ iNodeSetOfDivide) = nodeTreeFromDivision divide crossoverIn crossoverOut iNodeOutDirection matchDirection
    (crossoverIn, crossoverOut)
      | matchDirection == FirstLast = (crossoverLine1, crossoverLine2)
      | otherwise                   = (crossoverLine2, crossoverLine1)
    iNodeOutDirection
      | matchDirection == FirstLast = TowardOut
      | otherwise = TowardIn
    matchDirection = case matchDirectionOfSegments (firstSegOf completeNodeTree) (lastSegOf completeNodeTree) divide of
                       FirstLast -> FirstLast
                       LastFirst -> LastFirst
                       NoMatch -> error "could not match with complete side."
    (crossoverLine1, crossoverLine2) = crossoverLinesOfDivision divide
    foundSides (MotorcycleCell sides floatingSegments _ _)= len sides + len floatingSegments

cellsInCluster :: MotorcycleCluster -> Int
cellsInCluster (MotorcycleCluster cells _) = len cells

divideOfCluster :: MotorcycleCluster -> CellDivide
divideOfCluster (MotorcycleCluster _ divide) = divide

cellHasOneMotorcycle :: MotorcycleCell -> Bool
cellHasOneMotorcycle (MotorcycleCell _ _ cellMotorcycles _) = len cellMotorcycles == 1

-- Not quite just a "does this divide have just one motorcycle" check. note that it's possible to intersect a motorcycle head on, and still be a 'simple' divide.
isSimple :: CellDivide -> Bool
isSimple (CellDivide (DividingMotorcycles _ moreMotorcycles) _) = len moreMotorcycles == 0

cellHasMotorcycle :: MotorcycleCell -> Motorcycle -> Bool
cellHasMotorcycle (MotorcycleCell _ _ motorcycles _) motorcycle = elem motorcycle motorcycles

-- | divide the contour into motorcycle cells.
allMotorcycleCells :: Contour -> CrashTree -> [MotorcycleCell]
allMotorcycleCells contour crashTree = firstMotorcycleCell : allMotorcycleCells' firstRemainders
  where
    ((firstCell, maybeCellDivide), firstRemainders) = findFirstCellOfContour contour $ findDivisions contour crashTree
    firstMotorcycleCell = MotorcycleCell sidesOfFirstCell extraSegs (slist firstCellMotorcycles) rawNodeTreeOfFirstCell
      where
        firstCellMotorcycles
          | isJust maybeCellDivide = motorcyclesAlongEdgeOf $ fromJust maybeCellDivide
          | otherwise = mempty
        (rawNodeTreeOfFirstCell@(NodeTree (ENodeSet sidesOfFirstCell) _), extraSegs) = getRawNodeTreeOfCell firstCell
    allMotorcycleCells' :: [RemainingContour] -> [MotorcycleCell]
    allMotorcycleCells' remainders
     | null remainders = mempty
     | otherwise = case remainders of
                     [] -> mempty
                     [oneRemainder] -> oneMotorcycleCellFrom oneRemainder : allMotorcycleCells' (snd $ findNextCell oneRemainder)
                     (oneRemainder:others) -> oneMotorcycleCellFrom oneRemainder : concatMap (\a -> allMotorcycleCells' [a]) others
      where
        oneMotorcycleCellFrom remainder@(RemainingContour _ inDivide _) = MotorcycleCell sidesOfCell extraSegs (slist $ motorcyclesFromInput <> motorcyclesFromOutputs) rawNodeTreeOfCell
          where
            ((cell, _), nextRemainders) = findNextCell remainder
            motorcyclesFromInput
              | isJust inDivide = motorcyclesAlongEdgeOf $ fromJust inDivide
              | otherwise = error "no divide?"
            motorcyclesFromOutputs
              | null nextRemainders = mempty
              | otherwise = concatMap (motorcyclesAlongEdgeOf . fromJust . snd . fst . findNextCell) nextRemainders
            (rawNodeTreeOfCell@(NodeTree (ENodeSet sidesOfCell) _), extraSegs) = getRawNodeTreeOfCell cell

-- | Find the motorcycles that are part of a cellDivide, and are at the edge of this cell.
motorcyclesAlongEdgeOf :: CellDivide -> [Motorcycle]
motorcyclesAlongEdgeOf cellDivide = case cellDivide of
                                      (CellDivide (DividingMotorcycles m1 (Slist _ 0)) _) -> [m1]
                                      (CellDivide (DividingMotorcycles m1 mx@(Slist mxRaw _)) _) -> if len mx == 1 && isAntiCollinear (outAndErrOf m1) (outAndErrOf $ SL.head mx)
                                                                                                    then m1 : mxRaw
                                                                                                    else error "multiple dividing motorcycles, and not anti-collinear."

-- | Find the pair of motorcycle cells that border a motorcycle.
motorcycleCellsAlongMotorcycle :: Contour -> CrashTree -> Motorcycle -> (MotorcycleCell, MotorcycleCell)
motorcycleCellsAlongMotorcycle contour crashTree motorcycle = (firstCell, secondCell)
  where
    (firstCell, secondCell) = case filter (uncurry testCells) $ mapWithFollower (,) $ allMotorcycleCells contour crashTree of
                                [] -> error $ "no results?\n"
                                           <> show motorcycle <> "\n"
                                           <> show (allMotorcycleCells contour crashTree) <> "\n"
                                (a:_) -> a
    testCells a b = a `hasMotorcycle` motorcycle && b `hasMotorcycle` motorcycle
      where
        hasMotorcycle :: MotorcycleCell -> Motorcycle -> Bool
        hasMotorcycle (MotorcycleCell _ _ (Slist motorcycles _) _) target = target `elem` motorcycles
