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
  contourIntersectionCount,
  getLineContourIntersections,
  getMotorcycleContourIntersections,
  getMotorcycleSegSetIntersections
  ) where

import Prelude (Either(Left, Right), Int, Show(show), (<$>), (<>), ($), (/=), (&&), error, fst, length, odd, otherwise, zip)

import Data.List (filter)

import Data.Maybe (Maybe(Just, Nothing), catMaybes)

import Slist.Type (Slist)

import Slist (len, slist)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg, Point2, lineSegsOfContour, makeLineSeg, mapWithNeighbors)

import Graphics.Slicer.Math.Intersections (filterIntersections)

import Graphics.Slicer.Math.PGA (CPPoint2, Intersection, NPLine2, PIntersection, PLine2Err, ProjectiveLine2, intersectsWithErr, normalizeL, outputIntersectsLineSeg, pToEP)

import Graphics.Slicer.Math.Skeleton.Definitions (Motorcycle(Motorcycle))

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

-- | Get the intersections between a Line and a contour as a series of points. always returns an even number of intersections.
-- FIXME: accept error on this first pLine!
getLineContourIntersections :: (ProjectiveLine2 a) => (a, PLine2Err) -> Contour -> [Point2]
getLineContourIntersections (line, lineErr) c
  | odd $ length res = error $ "odd number of transitions: " <> show (length res) <> "\n" <> show c <> "\n" <> show line <> "\n" <> show res <> "\n"
  | otherwise = res
  where
    res = getPoints $ catMaybes $ mapWithNeighbors filterIntersections $ openCircuit $ zip (lineSegsOfContour c) $ intersectsWithErr targetLine <$> segs
      where
        segs :: [Either LineSeg (NPLine2, PLine2Err)]
        segs =  Left <$> lineSegsOfContour c
        targetLine :: Either LineSeg (NPLine2, PLine2Err)
        targetLine = Right (nLine, lineErr <> nLineErr)
        (nLine, nLineErr) = normalizeL line
    openCircuit v = Just <$> v
    getPoints :: [(LineSeg, Either Point2 CPPoint2)] -> [Point2]
    getPoints vs = getPoint <$> vs
      where
        getPoint (_, Left v) = v
        getPoint (_, Right v) = fst $ pToEP v

-- | Get all possible intersections between the motorcycle and the contour.
-- Filters out the input and output segment of the motorcycle.
getMotorcycleContourIntersections :: Motorcycle -> Contour -> [(LineSeg, Either Point2 CPPoint2)]
getMotorcycleContourIntersections m@(Motorcycle (inSeg, outSeg) _ _) c = stripInSegOutSeg $ catMaybes $ mapWithNeighbors filterIntersections $ openCircuit $ zip contourLines $ outputIntersectsLineSeg m <$> contourLines
  where
    openCircuit v = Just <$> v
    contourLines = lineSegsOfContour c
    stripInSegOutSeg :: [(LineSeg, Either Point2 CPPoint2)] -> [(LineSeg, Either Point2 CPPoint2)]
    stripInSegOutSeg = filter fun
      where
        -- filter out inSeg and outSeg outSeg
        fun (seg,_) = seg /= inSeg && seg /= outSeg

-- | Get all possible intersections between the motorcycle and the given list of segments.
-- Filters out the input and output segment of the motorcycle.
getMotorcycleSegSetIntersections :: Motorcycle -> [LineSeg] -> [(LineSeg, Either Point2 CPPoint2)]
getMotorcycleSegSetIntersections m@(Motorcycle (inSeg, outSeg) _ _) segs = stripInSegOutSeg $ catMaybes $ mapWithNeighbors filterIntersections $ shortCircuit $ zip bufferedLineSegs $ mightIntersect <$> bufferedLineSegs
  where
    -- since this is a list of segments, we terminate the list with Nothings, so that the saneIntersections pattern matching logic can deal with "there is no neighbor, but i hit a start/end point"
    bufferedLineSegs :: [Maybe LineSeg]
    bufferedLineSegs = Nothing : (Just <$> segs) <> [Nothing]
    mightIntersect :: Maybe LineSeg -> Maybe (Either Intersection PIntersection)
    mightIntersect maybeSeg = case maybeSeg of
                                Nothing -> Nothing
                                (Just seg) -> Just $ outputIntersectsLineSeg m seg
    shortCircuit :: [(Maybe LineSeg, Maybe (Either Intersection PIntersection))] -> [Maybe (LineSeg, Either Intersection PIntersection)]
    shortCircuit items = shortCircuitItem <$> items
      where
        shortCircuitItem (Nothing, Nothing) = Nothing
        shortCircuitItem (Just seg, Just intersection) = Just (seg, intersection)
        shortCircuitItem item = error $ "cannot short circuit item: " <> show item <> "\n"
    stripInSegOutSeg :: [(LineSeg, Either Point2 CPPoint2)] -> [(LineSeg, Either Point2 CPPoint2)]
    stripInSegOutSeg = filter fun
      where
        -- make sure neither of these segments are inSeg or outSeg
        fun (seg,_) = seg /= inSeg && seg /= outSeg

