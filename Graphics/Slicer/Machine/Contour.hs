---------------------------------------------------------------
-------------------- Contour Optimizer ------------------------
---------------------------------------------------------------

{-
 - Copyright 2016 Noah Halford and Catherine Moresco
 - Copyright 2019 Julia Longtin
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
 -
 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

module Graphics.Slicer.Machine.Contour (cleanContour, shrinkContour, addInsideContour, expandContour, addOutsideContour) where

import Prelude (length, (>), ($), otherwise, (<$>), Int, Eq, (<>), show, error, (==), negate, (.), head, (<*>), sqrt, (*), (+), take, drop, cycle, (-))

import Data.List (nub, null, zipWith3)

import Data.Maybe (Maybe(Just, Nothing), fromMaybe, catMaybes)

import Control.Parallel.Strategies (withStrategy, parList, rpar)

import Control.Parallel (par, pseq)

import Graphics.Slicer.Math.Definitions (Point(Point), Contour(Contour), distance, addPoints, scalePoint)

import Graphics.Slicer.Math.Line (Line(Line), Slope, Intersection(IntersectsAt, NoIntersection, Parallel), makeLines, makeLinesLooped, lineIntersection, pointsFromLines, combineConsecutiveLines, lineSlope, flipLine, pointSlopeLength, combineLines, lineFromEndpoints, endpoint)

import Graphics.Slicer.Math.Contour (outerPerimeterPoint, innerPerimeterPoint)

import Graphics.Slicer.Concepts.Definitions (BuildArea(RectArea))

import Graphics.Slicer.Formats.GCode.Definitions (roundPoint)

import Graphics.Slicer.Definitions(ℝ)

-- Contour optimizer. Merges small line fragments into larger ones.
cleanContour :: Contour -> Maybe Contour
cleanContour (Contour points)
  | length (cleanPoints points) > 2 = Just $ Contour $ cleanPoints points
  | otherwise = Nothing -- error $ "asked to clean a contour with " <> show (length points) <> "points: " <> show points <> "\n"
  where
    cleanPoints :: [Point] -> [Point]
    cleanPoints pts
      | null pts = []
      | length lines > 2 = pointsFromLines $ combineConsecutiveLines $ lines
      | otherwise = [] 
        where
          lines = makeLinesLooped pointsRemaining
          pointsRemaining = nub $ roundPoint <$> pts

-- | Given a point and slope (on an xy plane), make a line segment, where the far end is at the edge of the print bed.
-- FIXME: assumes the origin is at the corner.
-- FIXME: other bed types?
lineToEdge :: BuildArea -> Slope -> Point -> Line
lineToEdge (RectArea (bedX,bedY,_)) m p@(Point (_,_,c)) = head . makeLines . nub $ (roundPoint <$> points)
    where
      edges = lineFromEndpoints <$> [Point (0,0,c), Point (bedX,bedY,c)]
                                <*> [Point (0,bedY,c), Point (bedX,0,c)]
      longestLength = sqrt $ bedX*bedX + bedY*bedY
      halfLine@(Line p' s) = pointSlopeLength p m longestLength -- should have p' == p
      line = lineFromEndpoints (endpoint halfLine) (addPoints p' (scalePoint (-1) s))
      points = catMaybes $ saneIntersection . lineIntersection line <$> edges
      saneIntersection :: Intersection -> Maybe Point
      saneIntersection (IntersectsAt _ p2) = Just p2
      saneIntersection NoIntersection = Nothing
      saneIntersection Parallel = Nothing 
      saneIntersection res = error $ "insane result drawing a line to the edge: " <> show res <> "\n"    

-- like map, only with previous, current, and next item, and wrapping around so the first entry gets the last entry as previous, and vica versa.
mapWithNeighbors :: (a -> a -> a -> b) -> [a] -> [b]
mapWithNeighbors  f l =
    let
      rotateList :: Int -> [a] -> [a]
      rotateList n list = take (length list + 1) . drop n $ cycle list
      x = rotateList (length l - 1) l
      z = rotateList 1 l
    in
      withStrategy (parList rpar) $ x `par` z `pseq` zipWith3 f x l z

data Direction =
    Inward
  | Outward
  deriving (Eq)

-- reduce a contour by a given amount.
shrinkContour, addInsideContour :: BuildArea -> ℝ -> [Contour] -> Contour -> Contour
shrinkContour surface amount allContours contour = modifyContour surface amount allContours contour Inward
addInsideContour surface amount allContours contour = modifyContour surface amount allContours contour Inward

-- increase a contour by a given amount.
expandContour, addOutsideContour :: BuildArea -> ℝ -> [Contour] -> Contour -> Contour
expandContour surface amount allContours contour = modifyContour surface amount allContours contour Outward
addOutsideContour surface amount allContours contour@(Contour contourPoints) = modifyContour surface amount allContours contour Outward

-- FIXME: implement this.
-- FIXME: if the currently drawn line hits the current or previous contour on a line other than the line before or after the parent, you have a pinch. shorten the current line.
-- FIXME: draw a line before, and after the intersection. the line after is the first line of the new contour, the line before is still this contour.
-- Optimization: only check non-neighbor lines when the angles add up to a certain amount? looking for curling back. anything over ~180 degrees, relative to the slope of this line.
findExtraContours :: Contour -> Contour -> [Contour]
findExtraContours _ _ = []

-- Add one contour inside or outside of a given contour, in a naieve fashion.
modifyContour :: BuildArea -> ℝ -> [Contour] -> Contour -> Direction -> Contour
modifyContour surface pathWidth allContours contour@(Contour contourPoints) direction = Contour $ pointsFromLines foundContour
  where
    -- FIXME: implement me. we need this so we can handle further interior contours, and only check against the contour they are inside of.
    foundContour
      | (length contourPoints) > 2 = mapWithNeighbors (findLine allContours) $ (makeLinesLooped contourPoints)
      | otherwise = error $ "tried to modify a contour with too few points: "
      where
        -- FIXME: if the currently drawn line hits the current or previous contour on a line other than the line before or after the parent, you have a pinch. shorten the current line.
        -- Optimization: only check non-neighbor lines when the angles add up to a certain amount? looking for curling back. anything over ~180 degrees, relative to the slope of this line.
        findLine :: [Contour] -> Line -> Line -> Line -> Line
        findLine contours previousln ln@(Line _ m) nextln = flipLine midToStart `combineLines` midToEnd
          where
            midToEnd, midToStart :: Line
            midToEnd   = pointSlopeLength (perimeterPoint pathWidth contours ln) (lineSlope m) (lengthToIntersection ln nextln)
            midToStart = pointSlopeLength (perimeterPoint pathWidth contours ln) (lineSlope m) (negate $ lengthToIntersection ln previousln)
        perimeterPoint :: ℝ -> [Contour] -> Line -> Point
        perimeterPoint pathWidth contours ln
          | direction == Inward  = innerPerimeterPoint pathWidth contour ln
          | direction == Outward = outerPerimeterPoint pathWidth contour ln
        -- get the length to where these lines intersect, assuming they are pathWidth away from the lines themselves.
        lengthToIntersection :: Line -> Line -> ℝ
        lengthToIntersection l1 l2
          | lineSlope (roundPoint $ slopeOf newL1) == lineSlope (roundPoint $ slopeOf newL2) = distance (linePoint newL1) (linePoint newL2)
          | otherwise = distance (perimeterPoint pathWidth allContours l1) $ fromMaybe (noIntersectionError l1 newL1 l2 newL2) $ saneIntersection $ lineIntersection newL1 newL2
          where
            saneIntersection :: Intersection -> Maybe Point
            saneIntersection (IntersectsAt _ p2) = Just p2
            saneIntersection NoIntersection = Nothing
            saneIntersection res = error $ "insane result: " <> show res <> "\n"    
            linePoint (Line p _) = p
            slopeOf (Line _ m) = m
            newL1 = rawMidToEdge allContours l1
            newL2 = flipLine $ rawMidToEdge allContours l2
            -- line segments for a hypothetical line, without being shortened yet.
            rawMidToEdge contours ln@(Line _ m) = lineToEdge surface (lineSlope m) (perimeterPoint pathWidth contours ln)
            noIntersectionError :: Line -> Line -> Line -> Line -> Point
            noIntersectionError l1 newL1 l2 newL2 = error $ "no intersection on contour: \n" <> (show contour) <> "\n" <> show l1 <> " -> " <> show newL1 <> "\n" <> show l2 <> " -> " <> show newL2 <> "\n"

