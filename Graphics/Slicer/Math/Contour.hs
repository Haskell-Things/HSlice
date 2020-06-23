-- Slicer.
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

 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{- The purpose of this file is to hold information about contoured surfaces. -}

module Graphics.Slicer.Math.Contour (getContours, makeContourTree, innerPerimeterPoint, outerPerimeterPoint, lineToOutsideContour, ContourTree(ContourTree), lineEntersContour) where

import Prelude ((==), otherwise, (++), (||), (.), null, (<$>), ($), (>), length, Show, filter, (/=), odd, snd, error, (<>), show, fst, (*), Bool, (-), (<), pi, (&&), sqrt, (+), (<*>))
import Safe (maximumNote, minimumNote, headNote, lastNote)

import Data.List(find, delete, tail, last, head, init, zipWith, nub)

import Data.Maybe(fromJust, isNothing, Maybe(Just,Nothing), catMaybes, mapMaybe)

import Graphics.Slicer.Math.Definitions (Contour(Contour), Point(Point), scalePoint, addPoints)

import Graphics.Slicer.Formats.GCode.Definitions (roundPoint)

import Graphics.Slicer.Math.Line (Line(Line), lineFromEndpoints, lineIntersection, makeLinesLooped, makeLines, point, endpoint, pointSlopeLength, midpoint, lineSlope, perpendicularBisector, Intersection(NoIntersection, IntersectsAt, Parallel, HitEndpointL1, HitEndpointL2), angleOf, flipLine, lineBetween, SearchDirection (Clockwise, CounterClockwise), Slope)

import Graphics.Implicit.Definitions (ℝ)

-- A contour is a closed loop of lines on a layer.

-- Extract a single contour from a list of points
findContour :: ([Point], [[Point]]) -> ([Point], [[Point]])
findContour (contour, pairs)
  | isNothing p = (contour, pairs)
  | otherwise = findContour (contour ++ delete (last contour) p', delete p' pairs)
  where match p0 = head p0 == last contour || last p0 == last contour
        p = find match pairs
        p' = fromJust p

-- From a list of contours we have already found and a list of pairs of points
-- (each corresponding to a segment), get all contours described by the points
makeContours :: ([[Point]], [[Point]]) -> [[Point]]
makeContours (contours, pairs)
  | null pairs = contours
  | otherwise = makeContours (contours ++ [next], ps)
  where (next, ps) = findContour (headNote "makeContours" pairs, tail pairs)

-- FIXME: square is double loop?
-- NOTE: drop contours with less than 3 points.
-- Turn pairs of points into lists of connected points
getContours :: [[Point]] -> [Contour]
getContours pointPairs = Contour <$> (filteredContourSets foundContourSets)
  where
    filteredContourSets :: [[Point]] -> [[Point]]
    filteredContourSets pointset = catMaybes $ contourLongEnough <$> pointset
      where
        contourLongEnough :: [Point] -> Maybe [Point]
        contourLongEnough pts
          | length pts > 3 = Just pts
          | otherwise = Nothing
    foundContourSets :: [[Point]]
    foundContourSets = makeContours . (,) [] $ pointPairs

-- | Given a line, generate a pair of lines from points on both sides of the given line's midpoint to the origin, on the same z plane as the given line.
perimeterLinesToCheck :: ℝ -> Line -> (Line, Line)
perimeterLinesToCheck pathWidth l@(Line p _) = (headNote "perimeterLinesToCheck" linePair, lastNote "perimeterLinesToCheck" linePair)
  where
    linePair = (`lineFromEndpoints` Point (0,0,zOf p)) . endpoint . pointSlopeLength (midpoint l) (lineSlope m) . (*pathWidth) <$> [-1,1]
    Line _ m = perpendicularBisector l
    zOf :: Point -> ℝ
    zOf (Point (_,_,z)) = z

-- | Find a point on the interior of the given contour, on the perpendicular bisector of the given line, pathWidth from the line.
innerPerimeterPoint :: ℝ -> Contour -> Line -> Point
innerPerimeterPoint pathWidth contour l
    | length oddIntersections > 0 = snd $ head oddIntersections
    | length nonzeroIntersections > 0 = snd $ head nonzeroIntersections
    | length intersections > 0 = snd $ head intersections
    | otherwise = error $ "no intersections for line " <> show l <> "\n"
    where
      linesToCheck = perimeterLinesToCheck pathWidth l
      bothLinesToCheck = fst linesToCheck : [snd linesToCheck]
      intersections = (\a -> (numIntersections a, point a)) <$> bothLinesToCheck
      contourLines = makeLinesLooped $ (\(Contour points) -> points) contour
      numIntersections l' = length $ saneIntersections l' contourLines
      -- a filter for results that make sense.
      saneIntersections :: Line -> [Line] -> [Point]
      saneIntersections l1 ls = catMaybes $ saneIntersection . lineIntersection l1 <$> ls
        where
          saneIntersection :: Intersection -> Maybe Point
          saneIntersection (IntersectsAt _ p2) = Just p2
          saneIntersection NoIntersection = Nothing
          saneIntersection Parallel = Nothing
          saneIntersection (HitEndpointL1 p2) = Just p2
          saneIntersection (HitEndpointL2 p2) = Just p2
          saneIntersection res = error $ "insane result of intersecting a line (" <> show l1 <> ") with it's bisector: " <> show ls <> "\n" <> show res <> "\n"
      oddIntersections = filter (odd . fst) intersections
      nonzeroIntersections = filter ((/=0) . fst) intersections

-- | Find an exterior point on the perpendicular bisector of the given line, pathWidth from the line.
outerPerimeterPoint :: ℝ -> Contour -> Line -> Point
outerPerimeterPoint pathWidth contour l
      | (snd $ headNote "outerPerimeterPoint" intersections) == innerPoint = snd $ lastNote "outerPerimeterPoint" intersections
      | otherwise = snd $ headNote "outerPerimeterPoint #2" intersections
    where
      linesToCheck = perimeterLinesToCheck pathWidth l
      bothLinesToCheck = fst linesToCheck : [snd linesToCheck]
      intersections = (\a -> (numIntersections a, point a)) <$> bothLinesToCheck
      contourLines = makeLinesLooped $ (\(Contour points) -> points) contour
      numIntersections l' = length $ saneIntersections l' contourLines
      -- a filter for results that make sense.
      saneIntersections :: Line -> [Line] -> [Point]
      saneIntersections l1 ls = catMaybes $ saneIntersection . lineIntersection l1 <$> ls
        where
          saneIntersection :: Intersection -> Maybe Point
          saneIntersection (IntersectsAt _ p2) = Just p2
          saneIntersection NoIntersection = Nothing
          saneIntersection res = error $ "insane result of intersecting a line with it's bisector: " <> show res <> "\n"
      innerPoint = innerPerimeterPoint pathWidth contour l

-- | Given a point and slope (on an xy plane), make a line segment, where the far end is guaranteed to be outside the contour.
lineToOutsideContour :: Contour -> ℝ -> Slope -> Point -> Line
lineToOutsideContour (Contour contourPoints) outsideDistance m p@(Point (_,_,z)) = headNote "lineToOutsideContour" . makeLines . nub $ (roundPoint <$> points)
    where
      longestLength = sqrt $ dx*dx + dy*dy
      halfLine@(Line p' s) = pointSlopeLength p m longestLength -- should have p' == p
      line = lineFromEndpoints (endpoint halfLine) (addPoints p' (scalePoint (-1) s))
      points = catMaybes $ saneIntersection . lineIntersection line <$> edges
      saneIntersection :: Intersection -> Maybe Point
      saneIntersection (IntersectsAt _ p2) = Just p2
      saneIntersection NoIntersection = Nothing
      saneIntersection Parallel = Nothing
      saneIntersection res = error $ "insane result drawing a line to the edge: " <> show res <> "\n"
      edges = lineFromEndpoints <$> [Point (xMin,yMin,z), Point (xMax,yMax,z)]
                                <*> [Point (xMin,yMax,z), Point (xMax,yMin,z)]
      xMinRaw = minimumNote "lineToOutsideContour x" $ xOf <$> contourPoints
      yMinRaw = minimumNote "lineToOutsideContour y" $ yOf <$> contourPoints
      xMaxRaw = maximumNote "lineToOutsideContour x" $ xOf <$> contourPoints
      yMaxRaw = maximumNote "lineToOutsideContour y" $ yOf <$> contourPoints
      (dx,dy) = (xMax-xMin, yMax-yMin)
      xMin = xMinRaw - outsideDistance
      yMin = yMinRaw - outsideDistance
      xMax = xMaxRaw + outsideDistance
      yMax = yMaxRaw + outsideDistance
      xOf, yOf :: Point -> ℝ
      xOf (Point (x,_,_)) = x
      yOf (Point (_,y,_)) = y

newtype ContourTree = ContourTree (Contour, [ContourTree])
  deriving (Show)

makeContourTree :: [Contour] -> [ContourTree]
makeContourTree []        = []
makeContourTree [contour] = [ContourTree (contour, [])]
makeContourTree contours  = [ContourTree (foundContour, makeContourTree $ contoursWithAncestor contours foundContour) | foundContour <- contoursWithoutParents contours]
  where
    contoursWithAncestor cs c = mapMaybe (contourContainsContour c) $ filter (/=c) cs
    contoursWithoutParents cs = catMaybes $ [if (null $ mapMaybe (contourContainedByContour contourToCheck) (filter (/=contourToCheck) cs)) then Just contourToCheck else Nothing | contourToCheck <- cs ]

-- determine whether a contour is inside of another contour.
contourContainsContour :: Contour -> Contour -> Maybe Contour
contourContainsContour parent child = if odd noIntersections then Just child else Nothing
  where
    noIntersections = length $ getContourLineIntersections parent $ lineFromEndpoints (innerPointOf child) (Point (0,0,0))
    getContourLineIntersections :: Contour -> Line -> [Point]
    getContourLineIntersections (Contour contourPoints) line
      | length contourPoints == 0 = []
      | otherwise = catMaybes $ saneIntersection . lineIntersection line <$> makeLinesLooped contourPoints
    saneIntersection :: Intersection -> Maybe Point
    saneIntersection (IntersectsAt _ p2) = Just p2
    saneIntersection (HitEndpointL2 p2) = Just p2
    saneIntersection Parallel = Nothing 
    saneIntersection NoIntersection = Nothing
    saneIntersection res = error $ "insane result determining whether a contour contains a contour: " <> show res <> "\n"
    innerPointOf contour = innerPerimeterPoint 0.0001 contour $ oneLineOf contour
      where
        oneLineOf (Contour contourPoints) = headNote "contourContainsContour" $ makeLines contourPoints

-- determine whether a contour is inside of another contour.
contourContainedByContour :: Contour -> Contour -> Maybe Contour
contourContainedByContour child parent = if odd noIntersections then Just child else Nothing
  where
    noIntersections = length $ getContourLineIntersections parent $ lineFromEndpoints (innerPointOf child) (Point (0,0,0)) 
    getContourLineIntersections :: Contour -> Line -> [Point]
    getContourLineIntersections (Contour contourPoints) line = catMaybes $ saneIntersection . lineIntersection line <$> makeLinesLooped contourPoints
    saneIntersection :: Intersection -> Maybe Point
    saneIntersection (IntersectsAt _ p2) = Just p2
    saneIntersection (HitEndpointL2 p2) = Just p2
    saneIntersection Parallel = Nothing 
    saneIntersection NoIntersection = Nothing
    saneIntersection res = error $ "insane result determining whether a contour is contained by a contour: " <> show res <> "\n"    
    innerPointOf contour = innerPerimeterPoint 0.0001 contour $ oneLineOf contour
      where
        oneLineOf (Contour contourPoints) = headNote "contourContainedByContour" $ makeLines contourPoints

-- Does a given line, in the direction it is given, enter from outside of a contour to inside of a contour, through a given point?
-- Used to check the corner case of corner cases.
lineEntersContour :: Line -> Intersection -> Contour -> Bool
lineEntersContour (Line _ m) intersection contour@(Contour contourPoints) = lineBetween lineFrom searchDirection continuation lineToInverted
  where
    lineToInverted = flipLine lineTo
    continuation = Line (intersectionPoint intersection) m
    searchDirection
      | (angleOf lineToInteriorPoint) - (angleOf lineFrom) < (pi*0.5) &&
        (angleOf lineFrom) - (angleOf lineToInteriorPoint) < (pi*0.5)    = if angleOf lineFrom > angleOf lineToInteriorPoint then Clockwise else CounterClockwise
      | (angleOf lineToInteriorPoint) - (angleOf lineFrom) > (pi*0.5)    = Clockwise
      | (angleOf lineFrom) - (angleOf lineToInteriorPoint) > (pi*0.5)    = CounterClockwise
      | otherwise                                                        = error "impossible!"
    lineToInteriorPoint = lineFromEndpoints (intersectionPoint intersection) $ innerPerimeterPoint 0.00001 contour lineFrom
    intersectionPoint (HitEndpointL2 pt) = pt
    intersectionPoint other = error $ "trying to find where a line enters a contour on something not a point of a contour where two lines intersect: " <> show other <> "\n" 
    -- lineTo has an endpoint of the intersection, lineFrom has a starting point of the intersection.
    (lineTo, lineFrom) = findLinesInContour intersection
    contourLines = makeLinesLooped contourPoints
    findLinesInContour (HitEndpointL2 pt) = headNote "lineEntersContour" $ catMaybes $ zipWith (\l1@(Line _ _) l2@(Line p2 _) -> if p2 == pt then Just (l1,l2) else Nothing) (init contourLines) (tail contourLines)
    findLinesInContour other = error $ "trying to find where a line enters a contour on something not a point of a contour where two lines intersect: " <> show other <> "\n" 
