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

import Prelude ((==), otherwise, (++), (||), (.), null, (<$>), ($), (>), length, Show, filter, (/=), odd, snd, error, (<>), show, fst, (*), Bool(False), (-), (<), pi, (&&), sqrt, (+), (<*>), minimum, maximum, concat, Eq, Show, init, not)

import Data.List(find, delete, tail, last, head, init, zipWith, nub, partition, reverse)

import Data.Maybe(fromJust, isNothing, Maybe(Just,Nothing), catMaybes, mapMaybe, fromMaybe)

import Graphics.Slicer.Math.Definitions (Contour(PointSequence), Point3(Point3), Point2(Point2), scalePoint, addPoints, xOf, yOf, zOf)

--import Graphics.Slicer.Formats.GCode.Definitions (roundPoint2)

import Graphics.Slicer.Math.Line (Line(Line), lineFromEndpoints, lineIntersection, makeLinesLooped, makeLines, point, endpoint, pointSlopeLength, midpoint, lineSlope, perpendicularBisector, Intersection(NoIntersection, IntersectsAt, Parallel, HitEndpointL1, HitEndpointL2), angleOf, flipLine, lineBetween, SearchDirection (Clockwise, CounterClockwise), Slope)

import Graphics.Implicit.Definitions (ℝ)

-- A contour is a closed loop of lines on a layer.


-- Unapologetically ripped from ImplicitCAD.
-- FIXME: merge this back into ImplicitCAD.
getLoops :: (Show a, Eq a) => [[a]] -> [[[a]]]
getLoops a = getLoops' a []

-- We will be actually doing the loop extraction with
-- getLoops'

-- getLoops' has a first argument of the segments as before,
-- but a *second argument* which is the loop presently being
-- built.

-- so we begin with the "building loop" being empty.
getLoops' :: (Show a, Eq a) => [[a]] -> [[a]] -> [[[a]]]

-- | If there aren't any segments, and the "building loop" is empty, produce no loops.
getLoops' [] [] = []

-- | If the building loop is empty, stick the first segment we have onto it to give us something to build on.
getLoops' (x:xs) [] = getLoops' xs [x]

-- | A loop is finished if its start and end are the same.
-- Return it and start searching for another loop.
getLoops' segs workingLoop
  | head (head workingLoop) == last (last workingLoop) = workingLoop : getLoops' segs []

-- Finally, we search for pieces that can continue the working loop,
-- and stick one on if we find it.
-- Otherwise... something is really screwed up.
getLoops' segs workingLoop =
  let
    presEnd :: [[a]] -> a
    presEnd = last . last
    connectsBackwards (_:x:[]) = x == presEnd workingLoop
    connectsBackwards [] = False
    connects (x:_) = x == presEnd workingLoop
    -- Handle the empty case.
    connects [] = False
    -- divide our set into sequences that connect, and sequences that don't.
    (possibleConts, nonConts) = partition connects segs
    (possibleBackConts, nonBackConts) = partition connectsBackwards segs
    (next, unused) = if not $ null possibleConts
                     then (head possibleConts, tail possibleConts <> nonConts)
                     else if not $ null possibleBackConts
                          then (reverse $ head possibleBackConts, tail possibleBackConts <> nonBackConts)
                          else error $ "unclosed loop in paths given: \nWorking: " <> show workingLoop <> "\nRemainder:" <> show nonConts <> "\n"
  in
    if null next
    then workingLoop : getLoops' segs []
    else getLoops' unused (workingLoop <> [next])

-- Extract a single contour from a list of points
findContour :: ([Point2], [[Point2]]) -> ([Point2], [[Point2]])
findContour (contour, pairs)
  | isNothing p = (contour, pairs)
  | otherwise = findContour (contour ++ delete (last contour) p', delete p' pairs)
  where match p0 = head p0 == last contour || last p0 == last contour
        p = find match pairs
        p' = fromJust p

-- From a list of contours we have already found and a list of pairs of points
-- (each corresponding to a segment), get all contours described by the points
makeContours :: ([[Point2]], [[Point2]]) -> [[Point2]]
makeContours (contours, pairs)
  | null pairs = contours
  | otherwise = makeContours (contours ++ [next], ps)
  where (next, ps) = findContour (head pairs, tail pairs)

-- FIXME: square is double loop?
-- NOTE: drop contours with less than 3 points.
-- Turn pairs of points into lists of connected points
getContours :: [(Point2,Point2)] -> [Contour]
getContours pointPairs = PointSequence . contourAsPoints . contourAsPointPairs <$> foundContours
  where
    contourAsPoints contour = (fst <$> contour)
    contourAsPointPairs pointPairs = (\(a:b:[]) -> (a,b)) <$> pointPairs
    foundContours = catMaybes $ contourLongEnough <$> foundContourSets
    contourLongEnough :: [[Point2]] -> Maybe [[Point2]]
    contourLongEnough pts
      | length pts > 2 = Just pts
      | otherwise = Nothing
    foundContourSets :: [[[Point2]]]
--    foundContourSets = makeContours . (,) [] $ nub $ concat $ (\(a,b) -> a:b:[]) <$> pointPairs
    foundContourSets = getLoops $ (\(a,b) -> a:b:[]) <$> pointPairs

-- | Given a line, generate a pair of lines from points on both sides of the given line's midpoint to the origin, on the same z plane as the given line.
perimeterLinesToCheck :: ℝ -> Line -> (Line, Line)
perimeterLinesToCheck pathWidth l@(Line p _) = (head linePair, last linePair)
  where
    linePair = (`lineFromEndpoints` farPoint p) . endpoint . pointSlopeLength (midpoint l) (lineSlope m) . (*pathWidth) <$> [-1,1]
    Line _ m = perpendicularBisector l
    farPoint :: Point2 -> Point2
    farPoint (Point2 _) = Point2 (0,0)

-- | Find a point on the interior of the given contour, on the perpendicular bisector of the given line, pathWidth from the line.
innerPerimeterPoint :: ℝ -> Contour -> Line -> Point2
innerPerimeterPoint pathWidth contour l
    | length oddIntersections > 0 = snd $ head oddIntersections
    | length nonzeroIntersections > 0 = snd $ head nonzeroIntersections
    | length intersections > 0 = snd $ head intersections
    | otherwise = error $ "no intersections for line " <> show l <> "\n"
    where
      linesToCheck = perimeterLinesToCheck pathWidth l
      bothLinesToCheck = fst linesToCheck : [snd linesToCheck]
      intersections = (\a -> (numIntersections a, point a)) <$> bothLinesToCheck
      contourLines (PointSequence c) = makeLinesLooped c
      numIntersections l' = length $ saneIntersections l' $ contourLines contour
      -- a filter for results that make sense.
      saneIntersections :: Line -> [Line] -> [Point2]
      saneIntersections l1 ls = catMaybes $ saneIntersection . lineIntersection l1 <$> ls
        where
          saneIntersection :: Intersection -> Maybe Point2
          saneIntersection (IntersectsAt _ p2) = Just p2
          saneIntersection NoIntersection = Nothing
          saneIntersection Parallel = Nothing 
          saneIntersection (HitEndpointL1 p2) = Just p2
          saneIntersection (HitEndpointL2 p2) = Just p2
          saneIntersection res = error $ "insane result of intersecting a line (" <> show l1 <> ") with it's bisector: " <> show ls <> "\n" <> show res <> "\n"
      oddIntersections = filter (odd . fst) intersections
      nonzeroIntersections = filter ((/=0) . fst) intersections

-- | Find an exterior point on the perpendicular bisector of the given line, pathWidth from the line.
outerPerimeterPoint :: ℝ -> Contour -> Line -> Point2
outerPerimeterPoint pathWidth contour l
      | (snd $ head intersections) == innerPoint = snd $ last intersections
      | otherwise = snd $ head intersections
    where
      linesToCheck = perimeterLinesToCheck pathWidth l
      bothLinesToCheck = fst linesToCheck : [snd linesToCheck]
      intersections = (\a -> (numIntersections a, point a)) <$> bothLinesToCheck
      contourLines (PointSequence c) = makeLinesLooped c
      numIntersections l' = length $ saneIntersections l' $ contourLines contour
      -- a filter for results that make sense.
      saneIntersections :: Line -> [Line] -> [Point2]
      saneIntersections l1 ls = catMaybes $ saneIntersection . lineIntersection l1 <$> ls
        where
          saneIntersection :: Intersection -> Maybe Point2
          saneIntersection (IntersectsAt _ p2) = Just p2
          saneIntersection NoIntersection = Nothing
          saneIntersection Parallel = Nothing 
          saneIntersection res = error $ "insane result of intersecting a line with it's bisector: " <> show res <> "\n"
      innerPoint = innerPerimeterPoint pathWidth contour l

-- | Given a point and slope (on an xy plane), make a line segment, where the far end is guaranteed to be outside the contour.
lineToOutsideContour :: Contour -> ℝ -> Slope -> Point2 -> Line
lineToOutsideContour (PointSequence contourPoints) outsideDistance m p = head $ makeLines $ points
    where
      longestLength = sqrt $ dx*dx + dy*dy
      halfLine@(Line p' s) = pointSlopeLength p m longestLength -- should have p' == p
      line = lineFromEndpoints (endpoint halfLine) (addPoints p' (scalePoint (-1) s))
      points = catMaybes $ saneIntersection . lineIntersection line <$> (edges p)
      saneIntersection :: Intersection -> Maybe Point2
      saneIntersection (IntersectsAt _ p2) = Just p2
      saneIntersection NoIntersection = Nothing
      saneIntersection Parallel = Nothing
      saneIntersection res = error $ "insane result drawing a line to the edge: " <> show res <> "\n"
      edges (Point2 _) = lineFromEndpoints <$> [Point2 (xMin,yMin), Point2 (xMax,yMax)]
                                           <*> [Point2 (xMin,yMax), Point2 (xMax,yMin)]
      xMinRaw = minimum $ xOf <$> contourPoints
      yMinRaw = minimum $ yOf <$> contourPoints
      xMaxRaw = maximum $ xOf <$> contourPoints
      yMaxRaw = maximum $ yOf <$> contourPoints
      (dx,dy) = (xMax-xMin, yMax-yMin)
      xMin = xMinRaw - outsideDistance
      yMin = yMinRaw - outsideDistance
      xMax = xMaxRaw + outsideDistance
      yMax = yMaxRaw + outsideDistance

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
    noIntersections = length $ getContourLineIntersections parent $ lineToEdge $ innerPointOf child
    lineToEdge p = lineFromEndpoints p (Point2 (0,0))
    getContourLineIntersections :: Contour -> Line -> [Point2]
    getContourLineIntersections (PointSequence contourPoints) line
      | length contourPoints == 0 = []
      | otherwise = catMaybes $ saneIntersection . lineIntersection line <$> makeLinesLooped contourPoints
    saneIntersection :: Intersection -> Maybe Point2
    saneIntersection (IntersectsAt _ p2) = Just p2
    saneIntersection (HitEndpointL2 p2) = Just p2
    saneIntersection Parallel = Nothing 
    saneIntersection NoIntersection = Nothing
    saneIntersection res = error $ "insane result determining whether a contour contains a contour: " <> show res <> "\n"
    innerPointOf contour = innerPerimeterPoint 0.0001 contour $ oneLineOf contour
      where
        oneLineOf (PointSequence contourPoints) = head $ makeLines contourPoints

-- determine whether a contour is inside of another contour.
contourContainedByContour :: Contour -> Contour -> Maybe Contour
contourContainedByContour child parent = if odd noIntersections then Just child else Nothing
  where
    noIntersections = length $ getContourLineIntersections parent $ lineToEdge $ innerPointOf child
    lineToEdge p = lineFromEndpoints p (Point2 (0,0))
    getContourLineIntersections :: Contour -> Line -> [Point2]
    getContourLineIntersections (PointSequence contourPoints) line = catMaybes $ saneIntersection . lineIntersection line <$> makeLinesLooped contourPoints
    saneIntersection :: Intersection -> Maybe Point2
    saneIntersection (IntersectsAt _ p2) = Just p2
    saneIntersection (HitEndpointL2 p2) = Just p2
    saneIntersection Parallel = Nothing 
    saneIntersection NoIntersection = Nothing
    saneIntersection res = error $ "insane result determining whether a contour is contained by a contour: " <> show res <> "\n"
    innerPointOf contour = innerPerimeterPoint 0.0001 contour $ oneLineOf contour
      where
        oneLineOf (PointSequence contourPoints) = head $ makeLines contourPoints

-- Does a given line, in the direction it is given, enter from outside of a contour to inside of a contour, through a given point?
-- Used to check the corner case of corner cases.
lineEntersContour :: Line -> Intersection -> Contour -> Bool
lineEntersContour (Line _ m) intersection contour@(PointSequence contourPoints) = lineBetween lineFrom searchDirection continuation lineToInverted
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
    findLinesInContour (HitEndpointL2 pt) = head $ catMaybes $ zipWith (\l1@(Line _ _) l2@(Line p2 _) -> if p2 == pt then Just (l1,l2) else Nothing) (init contourLines) (tail contourLines)
    findLinesInContour other = error $ "trying to find where a line enters a contour on something not a point of a contour where two lines intersect: " <> show other <> "\n" 
