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

-- |  This file contains the entry point for the logic and routines required for dividing
--    a contour into cells.
module Graphics.Slicer.Math.Skeleton.Cells (cellAfter, cellBefore, contourToCell, simpleNodeTreeOfCell, nodeTreesDoNotOverlap, addMirrorNodeTrees) where

import Prelude (Bool(False), Eq, elem, filter, null, otherwise, ($), (<$>), (==), (++), error, fst, uncurry, (+), Int, drop, take, (-), error, (<>), show, (&&))

import Data.List (elemIndex)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, fromMaybe)

import Slist (slist)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), StraightSkeleton(StraightSkeleton), Motorcycle(Motorcycle), Cell(Cell), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), StraightSkeleton, ENode, finalPLine, outOf)

import Graphics.Slicer.Math.Skeleton.Motorcycles (motorcycleIntersectsAt, motorcyclesInDivision, intersectionSameSide, motorcyclesAreAntiCollinear, motorcycleToENode)

import Graphics.Slicer.Math.Skeleton.NodeTrees (firstSegOf, lastSegOf, makeNodeTree, sortNodeTrees)

import Graphics.Slicer.Math.Contour (lineSegsOfContour)

import Graphics.Slicer.Math.Definitions (Contour)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg), endpoint)

import Graphics.Slicer.Math.PGA (eToPPoint2, plinesIntersectIn)

-- | When there are no motorcycles, and there are no holes, we can just treat the whole contour as a single cell. This does the conversion.
contourToCell :: Contour -> Cell
contourToCell contour = Cell (slist [(lineSegsOfContour contour, Nothing)])

-- | get a naieve node tree for a given cell. can give incorrect results for a cell with a cell wall, in some cases.
simpleNodeTreeOfCell :: Cell -> NodeTree
simpleNodeTreeOfCell (Cell (Slist [(extSegs, _)] _)) = skeletonOfConcaveRegion extSegs
simpleNodeTreeOfCell _ = error "unsupported."

-- A flag for which side of a dividing motorcycle to cut a cell from, the side after or before the start of the motorcycle.
data Side = SideAfter
          | SideBefore
  deriving (Eq)

-- | Calculate a partial straight skeleton for the motorcycle cell that is on the left side of the point that a motorcycle\'s path starts at, ending where the motorcycle intersects the contour.
cellAfter :: Contour -> CellDivide -> NodeTree
cellAfter contour cellDivide = simpleNodeTreeOfCell $ createCellFromStraightWall contour cellDivide SideAfter

-- | Calculate a partial straight skeleton for the motorcycle cell that is on the right side of the point that a motorcycle\'s path starts at, ending where the motorcycle intersects the contour.
cellBefore :: Contour -> CellDivide -> NodeTree
cellBefore contour cellDivide = simpleNodeTreeOfCell $ createCellFromStraightWall contour cellDivide SideBefore

-- | use a single motorcycle to cut a section of a contour out, converting it to a cell.
-- | FIXME: what about the cell wall?
createCellFromStraightWall :: Contour -> CellDivide -> Side -> Cell
createCellFromStraightWall contour cellDivide@(CellDivide (DividingMotorcycles motorcycle@(Motorcycle (_,outSeg) _) _) _) side = Cell (slist [(gatherLineSegs side, Just cellDivide)])
  where
    contourSegs = lineSegsOfContour contour
    startSegmentIndex = segIndex outSeg contourSegs
    motorcycleIntersection = motorcycleIntersectsAt contour motorcycle
    -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the first of the two segments (from the beginning of the contour).
    motorcycleInSegment = fst motorcycleIntersection
    -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the last of the two segments (from the beginning of the contour).
    motorcycleOutSegment = uncurry fromMaybe motorcycleIntersection
    afterOpenSide   = drop afterStopSegmentIndex contourSegs ++ take startSegmentIndex contourSegs
    afterClosedSide = take (startSegmentIndex - afterStopSegmentIndex) $ drop afterStopSegmentIndex contourSegs
    afterStopSegmentIndex = segIndex motorcycleOutSegment contourSegs
    beforeOpenSide   = drop startSegmentIndex contourSegs ++ take beforeStopSegmentIndex contourSegs
    beforeClosedSide = take (beforeStopSegmentIndex - startSegmentIndex) $ drop startSegmentIndex contourSegs
    beforeStopSegmentIndex = 1 + segIndex motorcycleInSegment contourSegs

    -- |  Return the line segments we're responsible for straight skeletoning.
    gatherLineSegs :: Side -> [LineSeg]
    gatherLineSegs s =
      case s of
        SideAfter -> if findSegFromStart contour outSeg motorcycleInSegment == outSeg
                     -- test whether we can gather our segments from the stop segment to the end ++ first one until the segment the motorcycle hits...
                     then afterOpenSide
                     -- .. or by starting at the stop segment, and stopping after the segment the motorcycle hits
                     else afterClosedSide
        SideBefore -> if findSegFromStart contour outSeg motorcycleInSegment == motorcycleInSegment
                      -- test whether we can gather our segments from the stop segment to the end ++ first one until the segment the motorcycle hits...
                      then beforeOpenSide
                      -- .. or by starting at the stop segment, and stopping after the segment the motorcycle hits
                      else beforeClosedSide

    -- | Get the index of a specific segment, in a list of segments.
    segIndex :: LineSeg -> [LineSeg] -> Int
    segIndex seg segs = fromMaybe (error "cannot find item") $ elemIndex seg segs

    -- | Search a contour starting at the beginning, and return the first of the two line segments given
    findSegFromStart :: Contour -> LineSeg -> LineSeg -> LineSeg
    findSegFromStart c seg1 seg2 = case catMaybes (foundSeg seg1 seg2 <$> lineSegsOfContour c) of
                                     [] -> error "could not find requested segment."
                                     [a] -> a
                                     (a:_) -> a
      where
        foundSeg s1 s2 sn
          | sn == s1  = Just s1
          | sn == s2  = Just s2
          | otherwise = Nothing

-- | Add a pair of NodeTrees together, to create a straight skeleton. The straight skeleton should have it's NodeTrees in order.
addMirrorNodeTrees :: NodeTree -> NodeTree -> CellDivide -> StraightSkeleton
addMirrorNodeTrees nodeTree1 nodeTree2 division = StraightSkeleton [sortNodeTrees $ nodeTree1 : nodeTree2 : nodetreesFromDivision division] (slist [])
  where
    nodetreesFromDivision :: CellDivide -> [NodeTree]
    nodetreesFromDivision cellDivision@(CellDivide motorcycles maybeENode) = case motorcycles of
                                                                               (DividingMotorcycles _ (Slist [] 0)) -> res
                                                                               (DividingMotorcycles firstMotorcycle (Slist [secondMotorcycle] 1)) -> if motorcyclesAreAntiCollinear firstMotorcycle secondMotorcycle
                                                                                                                                                     then res
                                                                                                                                                     else errorOut
                                                                               (DividingMotorcycles _ (Slist _ _)) -> errorOut
      where
        res = case maybeENode of
                (Just eNode) -> [makeNodeTree (motorcycleToENode <$> motorcyclesInDivision cellDivision) (INodeSet $ slist []), makeNodeTree [eNode] (INodeSet $ slist [])]
                Nothing -> [makeNodeTree (motorcycleToENode <$> motorcyclesInDivision cellDivision) (INodeSet $ slist [])]
        errorOut = error "tried to add two NodeTrees with a non-bilateral cellDivide"

-- | Check whether the NodeTrees of two cells have an effect on each other.
nodeTreesDoNotOverlap :: NodeTree -> NodeTree -> CellDivide -> Bool
nodeTreesDoNotOverlap nodeTree1 nodeTree2 cellDivide@(CellDivide motorcycles1 _) = case motorcycles1 of
                                                                                     (DividingMotorcycles _ (Slist [] 0)) -> res
                                                                                     (DividingMotorcycles firstMotorcycle (Slist [secondMotorcycle] 1)) -> motorcyclesAreAntiCollinear firstMotorcycle secondMotorcycle && res
                                                                                     (DividingMotorcycles _ (Slist _ _)) -> False
  where
    res = null (crossoverENodes nodeTree1 cellDivide) &&
          null (crossoverENodes nodeTree2 cellDivide) &&
          lastOutsIntersect nodeTree1 nodeTree2 cellDivide

-- | Check that the outputs of the NodeTrees collide at the same point at the division between the two cells the NodeTrees correspond to.
lastOutsIntersect :: NodeTree -> NodeTree -> CellDivide -> Bool
lastOutsIntersect nodeTree1 nodeTree2 (CellDivide motorcycles _) = case motorcycles of
                                                                     (DividingMotorcycles m (Slist _ 0)) -> plinesIntersectIn (finalPLine nodeTree1) (outOf m) == plinesIntersectIn (finalPLine nodeTree2) (outOf m)
                                                                     (DividingMotorcycles _ (Slist _ _)) -> error "cannot yet check outpoint intersections of more than one motorcycle."

-- | Given a nodeTree and it's closing division, return all of the ENodes where the point of the node is on the opposite side of the division.
crossoverENodes :: NodeTree -> CellDivide -> [ENode]
crossoverENodes nodeTree@(NodeTree (ENodeSet firstENode (Slist moreRawNodes _)) _) cellDivision = filter (\a -> Just False `elem` (intersectionSameSide pointOnSide a <$> motorcyclesInDivision cellDivision)) (firstENode:moreRawNodes)
  where
    pointOnSide = eToPPoint2 $ pointInCell nodeTree cellDivision
    pointInCell cell (CellDivide (DividingMotorcycles m _) _)
      | firstSegOf cell == lastCSegOf m = endpoint $ firstSegOf cell
      | lastSegOf cell == firstCSegOf m = startPoint $ lastSegOf cell
      | otherwise = error $ "unhandled case: " <> show cell <> "\n" <> show m <> "\n" <> show (lastSegOf cell) <> "\n" <> show (firstSegOf cell) <> "\n"
      where
        startPoint (LineSeg a _) = a
        firstCSegOf (Motorcycle (seg1,_) _) = seg1
        lastCSegOf (Motorcycle (_, seg2) _) = seg2
