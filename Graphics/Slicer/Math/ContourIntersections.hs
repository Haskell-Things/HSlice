{- ORMOLU_DISABLE -}
{-
 - Copyright 2022 Julia Longtin
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

{- Purpose of this file: to hold the logic and routines responsible for checking for intersections with contours, or portions of contours. -}

module Graphics.Slicer.Math.ContourIntersections (
  contourIntersectionCount
  ) where

import Prelude (Either(Left), Int, (<$>), ($), zip)

import Data.Maybe (Maybe(Just), catMaybes)

import Slist.Type (Slist)

import Slist (len, slist)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg, Point2, lineSegsOfContour, makeLineSeg, mapWithNeighbors)

import Graphics.Slicer.Math.Intersections (filterIntersections)

import Graphics.Slicer.Math.PGA (CPPoint2, NPLine2, PLine2Err, intersectsWithErr)

-- | return the number of intersections with a given contour when traveling in a straight line from the beginning of the given line segment to the end of the line segment.
-- Not for use when line segments can overlap or are collinear with one of the line segments that are a part of the contour.
contourIntersectionCount :: Contour -> (Point2, Point2) -> Int
contourIntersectionCount contour (start, end) = len $ getIntersections contour (start, end)
  where
    getIntersections :: Contour -> (Point2, Point2) -> Slist (LineSeg, Either Point2 CPPoint2)
    getIntersections c (pt1, pt2) = slist $ catMaybes $ mapWithNeighbors filterIntersections $ openCircuit $ zip (lineSegsOfContour contour) $ intersectsWithErr targetSeg <$> segs
      where
        segs :: [Either LineSeg (NPLine2, PLine2Err)]
        segs =  Left <$> lineSegsOfContour c
        targetSeg :: Either LineSeg (NPLine2, PLine2Err)
        targetSeg = Left $ makeLineSeg pt1 pt2
        openCircuit v = Just <$> v


  
