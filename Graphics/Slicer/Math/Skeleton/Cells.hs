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

-- for deriving stock.
{-# LANGUAGE DerivingStrategies #-}

-- |  This file contains the entry point for the logic and routines required for dividing
--    a contour into cells.
module Graphics.Slicer.Math.Skeleton.Cells (RemainingContour, cellAfter, cellBefore, findOneCellOfContour, simpleNodeTreeOfCell, nodeTreesDoNotOverlap, addMirrorNodeTrees, findDivisions) where

import Prelude (Bool(False), Eq, Show, Ordering(LT, GT, EQ), elem, filter, null, otherwise, ($), (<$>), (==), (++), error, (+), Int, drop, take, (-), error, (<>), show, (&&), compare, (/=), (||), (<), fst, snd)

import Data.Either(Either(Left, Right))

import Data.List (elemIndex, sortBy, dropWhile)

import Data.Maybe (Maybe(Just, Nothing), catMaybes, fromMaybe, isNothing)

import Slist (slist, len, head)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), StraightSkeleton(StraightSkeleton), Motorcycle(Motorcycle), Cell(Cell), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), StraightSkeleton, ENode, ePointOf, finalPLine, intersectionOf, outOf)

import Graphics.Slicer.Math.Skeleton.Motorcycles (CollisionType(HeadOn), CrashTree(CrashTree), motorcycleIntersectsAt, motorcyclesInDivision, intersectionSameSide, lastCrashType, motorcyclesAreAntiCollinear, motorcycleToENode)

import Graphics.Slicer.Math.Skeleton.NodeTrees (firstSegOf, lastSegOf, makeNodeTree, sortNodeTrees)

import Graphics.Slicer.Math.Contour (lineSegsOfContour, makeLineSegContour)

import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg))

import Graphics.Slicer.Math.Line (endpoint, handleLineSegError, lineSegFromEndpoints)

import Graphics.Slicer.Math.PGA (PPoint2, PIntersection(PAntiCollinear), angleBetween, eToPLine2, eToPPoint2, pToEPoint2, plinesIntersectIn)

-- | get a naieve node tree for a given cell.
-- Warning: in the cases where the cell has nodes outside of the cell wall, you must use tscherne's algorithm to merge two cells.
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

-- The part of a contour that remains once we trim a concave section from it.
data RemainingContour = RemainingContour (Slist (Contour, Maybe CellDivide))
  deriving Eq
  deriving stock Show

-- | find the divisions of a given contour. Divisions are points where motorcycles cross the contour.
findDivisions :: Contour -> CrashTree -> [CellDivide]
findDivisions contour crashTree = case motorcyclesIn crashTree of
                                    (Slist [] _) -> []
                                    (Slist [inMC] _) -> [CellDivide (DividingMotorcycles inMC (Slist [] 0)) (maybeOpposingENodeOf contour crashTree)]
                                    (Slist [firstMC,secondMC] _) -> if lastCrashType crashTree == Just HeadOn
                                                              then [CellDivide (DividingMotorcycles firstMC (Slist [secondMC] 1)) Nothing]
                                                              else if intersectionIsBehind firstMC || intersectionIsBehind secondMC
                                                                   then -- These sections cannot intersect.
                                                                     [CellDivide (DividingMotorcycles firstMC (Slist [] 0)) Nothing,
                                                                      CellDivide (DividingMotorcycles secondMC (Slist [] 0)) Nothing]
                                                                   else error "don't know what to do with these line segments."
                                      where
                                        intersectionPPoint = intersectionOf (outOf firstMC) (outOf secondMC)
                                        intersectionIsBehind m = angleBetween (outOf m) (eToPLine2 $ lineSegToIntersection m) < 0
                                        lineSegToIntersection m = handleLineSegError $ lineSegFromEndpoints (ePointOf m) (pToEPoint2 intersectionPPoint)
  where
    motorcyclesIn (CrashTree motorcycles _ _) = motorcycles

-- | find enodes where the arc coresponding to them is collinear with the dividing Motorcycle.
maybeOpposingENodeOf :: Contour -> CrashTree -> Maybe ENode
maybeOpposingENodeOf contour crashTree = case motorcyclesIn crashTree of
                                           (Slist [inMC] 1) -> case opposingNodes contour inMC of
                                                                 [] -> Nothing
                                                                 [oneNode] -> Just oneNode
                                                                 (_:_) -> error "more than one opposing exterior node. cannot yet handle this situation."
                                           (Slist _ 2) -> if lastCrashType crashTree == Just HeadOn
                                                          then Nothing
                                                          else error "cannot handle anything but headon pairs of motorcycles yet."
                                           (Slist _ _) -> error "cannot handle anything but one motorcycle yet."
  where
    motorcyclesIn (CrashTree motorcycles _ _) = motorcycles
    opposingNodes :: Contour -> Motorcycle -> [ENode]
    opposingNodes myContour myMotorcycle = filter (\eNode -> plinesIntersectIn (outOf eNode) (outOf myMotorcycle) == PAntiCollinear) $ eNodesOfOutsideContour myContour

-- | Find the non-reflex virtexes of a contour and draw motorcycles from them. Useful for contours that are a 'hole' in a bigger contour.
-- | Find a single Cell of the given contour. always finds the cell on the 'open end' of the contour.
findOneCellOfContour :: Contour -> [CellDivide] -> (Cell, Maybe CellDivide, Maybe [RemainingContour], [CellDivide])
findOneCellOfContour contour divides = case divides of
                                         [] -> -- When there are no motorcycles, and there are no holes, we can just treat the whole contour as a single cell. This does the conversion.
                                           (contourFromCell, Nothing, Nothing, [])
                                         [oneDivide] -> (createCellFromStraightWall contour oneDivide (openSide oneDivide),
                                                         Just oneDivide,
                                                         Just [findContourRemainder contour oneDivide],
                                                         [])
                                         _ -> (createCellFromStraightWall contour closestDivide (openSide closestDivide),
                                               Just closestDivide,
                                               Just [findContourRemainder contour closestDivide],
                                               [])
                                           where
                                             closestDivide = if snd (head divideClosestSorted) == snd (head divideFurthestSorted)
                                                             then snd $ head divideClosestSorted
                                                             else error $ "ugh\n" <> show divideClosestSorted <> "\n" <> show divideFurthestSorted <> "\n"
  where
    openSide divide = case elemIndex (startSegOfDivide contour divide) contourSegs `compare` elemIndex (endSegOfDivide contour divide) contourSegs of
                        LT -> SideBefore
                        GT -> SideAfter
                        EQ -> error "FIXME"
    -- | convert a single contour to a single cell.
    contourFromCell = Cell (slist [(contourSegs, Nothing)])
    contourSegs = lineSegsOfContour contour
    divideClosestSorted = slist $ sortBy (\div1 div2 -> case elemIndex (fst div1) contourSegs `compare` elemIndex (fst div2) contourSegs of
                                                         LT -> LT
                                                         GT -> GT
--                                               EQ ->
                                        ) $ closestSegOfDivide contour <$> divides
    divideFurthestSorted = slist $ sortBy (\div1 div2 -> case elemIndex (fst div1) contourSegs `compare` elemIndex (fst div2) contourSegs of
                                                         LT -> LT
                                                         GT -> GT
--                                               EQ ->
                                        ) $ furthestSegOfDivide contour <$> divides
    closestSegOfDivide myContour divide = if elemIndex (startSegOfDivide myContour divide) contourSegs < elemIndex (endSegOfDivide myContour divide) contourSegs
                                          then (startSegOfDivide myContour divide, divide)
                                          else (endSegOfDivide myContour divide, divide)
    furthestSegOfDivide myContour divide = if elemIndex (startSegOfDivide myContour divide) contourSegs < elemIndex (endSegOfDivide myContour divide) contourSegs
                                           then (endSegOfDivide myContour divide, divide)
                                           else (startSegOfDivide myContour divide, divide)

-- | use a single straight divinion to cut the portion of a contour remaining after a cell has been cut out.
findContourRemainder :: Contour -> CellDivide -> RemainingContour
findContourRemainder contour divide
  | startBeforeEnd = RemainingContour $ slist $ [(makeLineSegContour $ takeWhileInclusive (\seg -> seg /= endSegOfDivide contour divide) $ dropWhile (\seg -> seg /= startSegOfDivide contour divide) contourSegs, Nothing)]
  | otherwise = RemainingContour $ slist $ [(makeLineSegContour $ takeWhileInclusive (\seg -> seg /= startSegOfDivide contour divide) contourSegs ++ dropWhile (\seg -> seg /= endSegOfDivide contour divide) contourSegs, Nothing)]
  where
    startBeforeEnd = elemIndex (startSegOfDivide contour divide) contourSegs `compare` elemIndex (endSegOfDivide contour divide) contourSegs == LT
    takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
    takeWhileInclusive _ [] = []
    takeWhileInclusive p (x:xs) = x : if p x
                                      then takeWhileInclusive p xs
                                      else []
    contourSegs = lineSegsOfContour contour

-- Get the segment the divide intersects that is closest to the beginning of the list of a contour's line segments.
startSegOfDivide :: Contour -> CellDivide -> LineSeg
startSegOfDivide _ (CellDivide (DividingMotorcycles (Motorcycle (inSeg, _) _) _) _) = inSeg

-- Get the segment the divide intersects that is closest to the end of the list of a contour's line segments.
endSegOfDivide :: Contour -> CellDivide -> LineSeg
endSegOfDivide contour divide = justSeg
      where
        (justSeg,_) = endOfDivide contour divide

-- Get the segment and location on the segment the given divide intersects that is closest to the end or beginning of the list of a contour's line segments.
endOfDivide :: Contour -> CellDivide -> (LineSeg, Either LineSeg PPoint2)
endOfDivide contour (CellDivide (DividingMotorcycles m ms) maybeENode)
  | len ms == 0 && isNothing maybeENode = motorcycleIntersectsAt contour m
  | otherwise = error "also no."
  -- FIXME: yes, this is woefully incomplete.

-- | use a single straight division to cut a section of a contour out, converting it to a cell.
-- | FIXME: what about the cell wall?
createCellFromStraightWall :: Contour -> CellDivide -> Side -> Cell
createCellFromStraightWall contour cellDivide@(CellDivide (DividingMotorcycles motorcycle@(Motorcycle (_,outSeg) _) _) _) side = Cell (slist [(gatherLineSegs side, Just cellDivide)])
  where
    contourSegs = lineSegsOfContour contour
    startSegmentIndex = segIndex outSeg contourSegs
    -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the first of the two segments (from the beginning of the contour).
    (motorcycleInSegment, eitherMotorcycleOutSegment) = motorcycleIntersectsAt contour motorcycle
    -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the last of the two segments (from the beginning of the contour).
    motorcycleOutSegment = case eitherMotorcycleOutSegment of
                             (Left motorcycleOutSeg) -> motorcycleOutSeg
                             (Right _) -> motorcycleInSegment
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
    lastOutsIntersect nt1 nt2 (CellDivide motorcycles _) = case motorcycles of
                                                             (DividingMotorcycles m (Slist _ 0)) -> plinesIntersectIn (finalPLine nt1) (outOf m) == plinesIntersectIn (finalPLine nt2) (outOf m)
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
