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
module Graphics.Slicer.Math.Skeleton.Cells (cellAfter, cellBefore) where

import Prelude (Eq, Bool(False), otherwise, ($), (<$>), (==), (++), error, fst, uncurry, (+), Int, drop, take, (-))

import Data.List (elemIndex)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, fromMaybe)

import Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (NodeTree, Motorcycle(Motorcycle))

import Graphics.Slicer.Math.Skeleton.Motorcycles (motorcycleIntersectsAt)

import Graphics.Slicer.Math.Definitions (Contour)

import Graphics.Slicer.Math.Line (LineSeg)

import Graphics.Slicer.Math.Contour (lineSegsOfContour)

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
