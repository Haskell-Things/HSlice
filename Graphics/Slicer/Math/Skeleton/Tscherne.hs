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

module Graphics.Slicer.Math.Skeleton.Tscherne (applyTscherne, cellAfter, cellBefore) where

import Prelude (Bool(False), concat, elem, otherwise, tail, ($), (<$>), (==), (++), error, (&&), head, fst, (<>), show, uncurry, null, filter, (+), Int, drop, take, (-), (||), length)

import Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode, NodeTree(NodeTree), Motorcycle(Motorcycle), CellDivide(CellDivide), finalPLine, outOf)

import Graphics.Slicer.Math.Skeleton.NodeTrees (lastSegOf, firstSegOf, sortNodeTrees)

import Graphics.Slicer.Math.Skeleton.Motorcycles (motorcycleToENode, motorcycleIntersectsAt, intersectionSameSide)

import Graphics.Slicer.Math.Definitions (Contour)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg), endpoint)

import Data.List (elemIndex)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, isJust, fromJust, fromMaybe)

import Graphics.Slicer.Math.Contour (linesOfContour)

import Graphics.Slicer.Math.PGA (PIntersection(PCollinear), plinesIntersectIn, eToPPoint2)

applyTscherne :: Contour -> [CellDivide] -> Maybe StraightSkeleton
applyTscherne contour cellDivisions
  -- | use observations from christopher tscherne's masters thesis to cover the corner cases that do not require the whole algorithm.
  -- If the two sides do not have an influence on one another, and the last line out of the two sides intersects the motorcycle at the same point
  | length cellDivisions == 1 && cellsDoNotOverlap (leftSide, head cellDivisions) (rightSide, head cellDivisions) = Just $ addCells [leftSide,rightSide] cellDivisions
    -- FIXME: ok, can't cheat. apply the full algorithm.
  | otherwise = error $ "failing to apply Tscherne's method.\n" <>
                        show (crossoverENodes leftSide (head cellDivisions))  <> "\n" <>
                        show (crossoverENodes rightSide (head cellDivisions))  <> "\n" <>
                        show (finalPLine leftSide) <> "\n" <>
                        show (finalPLine rightSide) <> "\n" <>
                        show leftSide <> "\n" <>
                        show rightSide <> "\n" <>
                        show dividingMotorcycle <> "\n"
  | otherwise = Nothing
  where
    -- Check whether the NodeTrees of two cells have an effect on each other.
    cellsDoNotOverlap :: (NodeTree, CellDivide) -> (NodeTree, CellDivide) -> Bool
    cellsDoNotOverlap (cell1,cellDivision1@(CellDivide motorcycles1 _)) (cell2,cellDivision2)
      -- Only works when the CellDivide is simple enough that it is symetrical (a line).
      | cellDivision1 == cellDivision2 &&
        (length motorcycles1 == 1 ||
         (length motorcycles1 == 2 && motorcyclesAreCollinear (head motorcycles1) (head $ tail motorcycles1)))
      = null (crossoverENodes cell1 cellDivision1) &&
        null (crossoverENodes cell2 cellDivision2) &&
        cellOutsIntersect cell1 cell2 cellDivision1
      | otherwise = False

    -- Check that the outputs of the cells collide at the same point at the division between the two cells.
    cellOutsIntersect cell1 cell2 (CellDivide motorcycles _)
      | length motorcycles == 1 = plinesIntersectIn (finalPLine cell1) (outOf $ head motorcycles) ==
                                  plinesIntersectIn (finalPLine cell2) (outOf $ head motorcycles)
      | otherwise = error "cannot yet check outpoint intersections of more than one motorcycle."

    -- | given a nodeTree and it's closing division, return all of the ENodes where the point of the node is on the opposite side of the division.
    crossoverENodes :: NodeTree -> CellDivide -> [ENode]
    crossoverENodes nodeTree@(NodeTree eNodes _) cellDivision = filter (\a -> elem (Just False) (intersectionSameSide pointOnSide a <$> motorcyclesFromDivision cellDivision)) eNodes
      where
        pointOnSide = eToPPoint2 $ pointInCell nodeTree cellDivision
        pointInCell cell (CellDivide motorcycles _)
          | firstSegOf cell == lastCSegOf (head motorcycles) = endpoint $ firstSegOf cell
          | lastSegOf cell == firstCSegOf (head motorcycles) = startPoint $ lastSegOf cell
          | otherwise = error $ "unhandled case: " <> show cell <> "\n" <> show motorcycles <> "\n" <> show contour <> "\n" <> show cellDivisions <> "\n" <> show (lastSegOf cell) <> "\n" <> show (firstSegOf cell) <> "\n"
          where
            startPoint (LineSeg a _) = a
            firstCSegOf (Motorcycle (seg1,_) _) = seg1
            lastCSegOf (Motorcycle (_, seg2) _) = seg2

    -- Add a set of cells together, to create a straight skeleton. The straight skeleton should have it's NodeTrees in order.
    addCells :: [NodeTree] -> [CellDivide] -> StraightSkeleton
    addCells cells divisions
      | length cells == 2 && length divisions == 1 = StraightSkeleton [sortNodeTrees $ cells ++ concat (nodetreesFromDivision <$> divisions)] []
      where
        nodetreesFromDivision :: CellDivide -> [NodeTree]
        nodetreesFromDivision (CellDivide motorcycles maybeENode)
          | length motorcycles == 1 ||
            (length motorcycles == 2 && motorcyclesAreCollinear (head motorcycles) (head $ tail motorcycles))
          = if isJust maybeENode
            then [NodeTree (motorcycleToENode <$> motorcycles) [], NodeTree [fromJust maybeENode] []]
            else [NodeTree (motorcycleToENode <$> motorcycles) []]

    -- check if the output of two motorcycles are collinear with each other.
    motorcyclesAreCollinear motorcycle1 motorcycle2 = plinesIntersectIn (outOf motorcycle1) (outOf motorcycle2) == PCollinear

    motorcyclesFromDivision (CellDivide m _) = m

    -------------------------------------------------------------------------------------
    -- Functions used when we have two cells, and one dividing motorcycle between them --
    -------------------------------------------------------------------------------------

    leftSide  = cellAfter contour dividingMotorcycle
    rightSide = cellBefore contour dividingMotorcycle
    dividingMotorcycle = if length (motorcyclesFromDivision $ head cellDivisions) == 1
                         then head (motorcyclesFromDivision $ head cellDivisions)
                         else error "cannot yet handle more than one dividing motorcycle."

-- | Calculate a partial straight skeleton for the motorcycle cell that is on the left side of the point that a motorcycle's path starts at, ending where the motorcycle intersects the contour.
cellAfter :: Contour -> Motorcycle -> NodeTree
cellAfter contour motorcycle = skeletonOfConcaveRegion (gatherLineSegs contour motorcycle) False
  where
    -- Return the line segments we're responsible for straight skeletoning.
    gatherLineSegs :: Contour -> Motorcycle -> [LineSeg]
    gatherLineSegs c m@(Motorcycle (_,outSeg) _) =
      if findSegFromStart c outSeg motorcycleInSegment == outSeg
      -- test whether we can gather our segments from the stop segment to the end ++ first one until the segment the motorcycle hits...
      then openSide
      -- .. or by starting at the stop segment, and stopping after the segment the motorcycle hits
      else closedSide
        where
          openSide   = drop stopSegmentIndex (linesOfContour c) ++ take startSegmentIndex (linesOfContour c)
          closedSide = take (startSegmentIndex - stopSegmentIndex) $ drop stopSegmentIndex $ linesOfContour c
          startSegmentIndex = segIndex outSeg (linesOfContour c)
          motorcycleIntersection = motorcycleIntersectsAt c m
          -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the first of the two segments (from the beginning of the contour).
          motorcycleInSegment  = fst motorcycleIntersection
          stopSegmentIndex = segIndex motorcycleOutSegment (linesOfContour c)
          -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the last of the two segments (from the beginning of the contour).
          motorcycleOutSegment = uncurry fromMaybe motorcycleIntersection

-- | Calculate a partial straight skeleton for the motorcycle cell that is on the right side of the point that a motorcycle's path starts at, ending where the motorcycle intersects the contour.
cellBefore :: Contour -> Motorcycle -> NodeTree
cellBefore contour motorcycle = skeletonOfConcaveRegion (gatherLineSegs contour motorcycle) False
  where
    -- Return the line segments we're responsible for straight skeletoning.
    gatherLineSegs :: Contour -> Motorcycle -> [LineSeg]
    gatherLineSegs c m@(Motorcycle (_,outSeg) _) =
      if findSegFromStart c outSeg motorcycleInSegment == motorcycleInSegment
      -- test whether we can gather our segments from the stop segment to the end ++ first one until the segment the motorcycle hits...
      then openSide
      -- .. or by starting at the stop segment, and stopping after the segment the motorcycle hits
      else closedSide
        where
          openSide   = drop startSegmentIndex (linesOfContour c) ++ take stopSegmentIndex (linesOfContour c)
          closedSide = take (stopSegmentIndex - startSegmentIndex) $ drop startSegmentIndex $ linesOfContour c
          startSegmentIndex = segIndex outSeg (linesOfContour c)
          motorcycleIntersection = motorcycleIntersectsAt c m
          -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the first of the two segments (from the beginning of the contour).
          motorcycleInSegment  = fst motorcycleIntersection
          stopSegmentIndex = 1 + segIndex motorcycleInSegment (linesOfContour c)

-- | Get the index of a specific segment, in a list of segments.
segIndex :: LineSeg -> [LineSeg] -> Int
segIndex seg segs = fromMaybe (error "cannot find item") $ elemIndex seg segs

-- | Search a contour starting at the beginning, and return the first of the two line segments given
findSegFromStart :: Contour -> LineSeg -> LineSeg -> LineSeg
findSegFromStart c seg1 seg2 = head $ catMaybes $ foundSeg seg1 seg2 <$> linesOfContour c
  where
    foundSeg s1 s2 sn
      | sn == s1  = Just s1
      | sn == s2  = Just s2
      | otherwise = Nothing

-- | Apply Christopher Tscherne's algorithm from his master's thesis.
