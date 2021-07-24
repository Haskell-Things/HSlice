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
   This will contain logic for handling all contours with no holes, using
   the algorithm in Christopher Tscherne's masters thesis.
-}

-- | Christopher Tscherne\'s algorithm from his master\'s thesis.
module Graphics.Slicer.Math.Skeleton.Tscherne (applyTscherne, cellAfter, cellBefore) where

import Prelude (Eq, Bool(False), elem, otherwise, ($), (<$>), (==), (++), error, (&&), fst, (<>), show, uncurry, null, filter, (+), Int, drop, take, (-))

import Data.List (elemIndex)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, fromMaybe)

import Slist (slist)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode, INodeSet (INodeSet), NodeTree(NodeTree), Motorcycle(Motorcycle), CellDivide(CellDivide), DividingMotorcycles (DividingMotorcycles), ENodeSet(ENodeSet), finalPLine, outOf)

import Graphics.Slicer.Math.Skeleton.NodeTrees (lastSegOf, firstSegOf, sortNodeTrees, makeNodeTree)

import Graphics.Slicer.Math.Skeleton.Motorcycles (motorcycleToENode, motorcycleIntersectsAt, intersectionSameSide)

import Graphics.Slicer.Math.Definitions (Contour)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg), endpoint)

import Graphics.Slicer.Math.Contour (lineSegsOfContour)

import Graphics.Slicer.Math.PGA (PIntersection(PCollinear), plinesIntersectIn, eToPPoint2)

-- | Use observations from christopher tscherne\'s masters thesis to cover the corner cases that do not require the whole algorithm.
-- If the two sides do not have an influence on one another, and the last line out of the two sides intersects the motorcycle at the same point
applyTscherne :: Contour -> [CellDivide] -> Maybe StraightSkeleton
applyTscherne contour cellDivisions =
  case cellDivisions of
    [] -> Nothing
    [oneDivision] -> if cellsDoNotOverlap (leftSide, oneDivision) (rightSide, oneDivision)
                     then Just $ addMirrorCells leftSide rightSide oneDivision
                     else errorIncomplete
    (_:_) -> Nothing
  where
    -- FIXME: ok, can't cheat. apply the full algorithm.
    errorIncomplete = error $ "failing to apply Tscherne's method.\n" <>
                      show (finalPLine leftSide) <> "\n" <>
                      show (finalPLine rightSide) <> "\n" <>
                      show leftSide <> "\n" <>
                      show rightSide <> "\n" <>
                      show contour  <> "\n" <>
                      show cellDivisions  <> "\n" <>
                      show dividingMotorcycle <> "\n"
    -- Check whether the NodeTrees of two cells have an effect on each other.
    cellsDoNotOverlap :: (NodeTree, CellDivide) -> (NodeTree, CellDivide) -> Bool
    cellsDoNotOverlap (cell1,cellDivision1@(CellDivide motorcycles1 _)) (cell2,cellDivision2)
      -- Only works when the CellDivide is simple enough that it is symetrical (a line).
      | cellDivision1 == cellDivision2 = case motorcycles1 of
                                           (DividingMotorcycles _ (Slist [] 0)) -> res
                                           (DividingMotorcycles firstMotorcycle (Slist [secondMotorcycle] 1)) -> motorcyclesAreCollinear firstMotorcycle secondMotorcycle && res
                                           (DividingMotorcycles _ (Slist _ _)) -> False
      | otherwise = False
      where
        res = null (crossoverENodes cell1 cellDivision1) &&
              null (crossoverENodes cell2 cellDivision2) &&
              cellOutsIntersect cell1 cell2 cellDivision1

    -- Check that the outputs of the cells collide at the same point at the division between the two cells.
    cellOutsIntersect cell1 cell2 (CellDivide motorcycles _) = case motorcycles of
                                                                 (DividingMotorcycles m (Slist _ 0)) -> plinesIntersectIn (finalPLine cell1) (outOf m) == plinesIntersectIn (finalPLine cell2) (outOf m)
                                                                 (DividingMotorcycles _ (Slist _ _)) -> error "cannot yet check outpoint intersections of more than one motorcycle."

    -- given a nodeTree and it's closing division, return all of the ENodes where the point of the node is on the opposite side of the division.
    crossoverENodes :: NodeTree -> CellDivide -> [ENode]
    crossoverENodes nodeTree@(NodeTree (ENodeSet firstENode (Slist moreRawNodes _)) _) cellDivision = filter (\a -> Just False `elem` (intersectionSameSide pointOnSide a <$> motorcyclesInDivision cellDivision)) (firstENode:moreRawNodes)
      where
        pointOnSide = eToPPoint2 $ pointInCell nodeTree cellDivision
        pointInCell cell (CellDivide (DividingMotorcycles m _) _)
          | firstSegOf cell == lastCSegOf m = endpoint $ firstSegOf cell
          | lastSegOf cell == firstCSegOf m = startPoint $ lastSegOf cell
          | otherwise = error $ "unhandled case: " <> show cell <> "\n" <> show m <> "\n" <> show contour <> "\n" <> show cellDivisions <> "\n" <> show (lastSegOf cell) <> "\n" <> show (firstSegOf cell) <> "\n"
          where
            startPoint (LineSeg a _) = a
            firstCSegOf (Motorcycle (seg1,_) _) = seg1
            lastCSegOf (Motorcycle (_, seg2) _) = seg2

    -- Add a set of cells together, to create a straight skeleton. The straight skeleton should have it's NodeTrees in order.
    addMirrorCells :: NodeTree -> NodeTree -> CellDivide -> StraightSkeleton
    addMirrorCells cell1 cell2 division = StraightSkeleton [sortNodeTrees $ cell1 : cell2 : nodetreesFromDivision division] (slist [])
      where
        nodetreesFromDivision :: CellDivide -> [NodeTree]
        nodetreesFromDivision cellDivision@(CellDivide motorcycles maybeENode) = case motorcycles of
                                                                                   (DividingMotorcycles _ (Slist [] 0)) -> res
                                                                                   (DividingMotorcycles firstMotorcycle (Slist [secondMotorcycle] 1)) -> if motorcyclesAreCollinear firstMotorcycle secondMotorcycle
                                                                                                                                                         then res
                                                                                                                                                         else errorOut
                                                                                   (DividingMotorcycles _ (Slist _ _)) -> errorOut
            where
              res = case maybeENode of
                      (Just eNode) -> [makeNodeTree (motorcycleToENode <$> motorcyclesInDivision cellDivision) (INodeSet $ slist []), makeNodeTree [eNode] (INodeSet $ slist [])]
                      Nothing -> [makeNodeTree (motorcycleToENode <$> motorcyclesInDivision cellDivision) (INodeSet $ slist [])]
              errorOut = error "tried to add two cells with a non-bilateral cellDivide"

    -- check if the output of two motorcycles are collinear with each other.
    motorcyclesAreCollinear motorcycle1 motorcycle2 = plinesIntersectIn (outOf motorcycle1) (outOf motorcycle2) == PCollinear

    motorcyclesFromDivision (CellDivide m _) = m

    motorcyclesInDivision (CellDivide (DividingMotorcycles a (Slist b _)) _) = a : b

    -------------------------------------------------------------------------------------
    -- Functions used when we have two cells, and one dividing motorcycle between them --
    -------------------------------------------------------------------------------------

    leftSide  = cellAfter contour dividingMotorcycle
    rightSide = cellBefore contour dividingMotorcycle
    dividingMotorcycle = case motorcyclesFromDivision cellDivision of
                           (DividingMotorcycles a (Slist _ 0)) -> a
                           (DividingMotorcycles _ (Slist _ _)) -> error "cannot yet handle more than one dividing motorcycle."
      where
        cellDivision = case cellDivisions of
                         [] -> error "no cellDivision to work with."
                         [a] -> a
                         (_:_) -> error "cannot yet handle more that one cell division point."

-- Which side of a division we're getting.
data Side = SideAfter
          | SideBefore
  deriving (Eq)

-- | Calculate a partial straight skeleton for the motorcycle cell that is on the left side of the point that a motorcycle\'s path starts at, ending where the motorcycle intersects the contour.
cellAfter :: Contour -> Motorcycle -> NodeTree
cellAfter contour motorcycle = nodeTreeOfCell contour motorcycle SideAfter

-- | Calculate a partial straight skeleton for the motorcycle cell that is on the right side of the point that a motorcycle\'s path starts at, ending where the motorcycle intersects the contour.
cellBefore :: Contour -> Motorcycle -> NodeTree
cellBefore contour motorcycle = nodeTreeOfCell contour motorcycle SideBefore

nodeTreeOfCell :: Contour -> Motorcycle -> Side -> NodeTree
nodeTreeOfCell contour motorcycle@(Motorcycle (_,outSeg) _) side = skeletonOfConcaveRegion (gatherLineSegs side) False
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
    -- Return the line segments we're responsible for straight skeletoning.
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
