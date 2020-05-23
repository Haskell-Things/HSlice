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

module Graphics.Slicer.Math.Contour (getContours, makeContourTree, innerPerimeterPoint, outerPerimeterPoint, ContourTree(ContourTree)) where

import Prelude ((==), otherwise, (++), (||), (.), null, fmap, (<$>), ($), (>), (>=), length, Show, filter, (/=), odd, snd, error, (<>), show, fst, foldMap, (*), even)

import Data.List(find, delete, tail, last, head)

import Data.Maybe(fromJust, isNothing, Maybe(Just,Nothing), catMaybes, mapMaybe)

import Graphics.Slicer.Math.Definitions (Contour(Contour), Point(Point))

import Graphics.Slicer.Math.Line (Line(Line), lineFromEndpoints, lineIntersection, makeLinesLooped, makeLines, point, endpoint, pointSlopeLength, midpoint, lineSlope, perpendicularBisector)

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
  where (next, ps) = findContour (head pairs, tail pairs)

-- FIXME: square is double loop?
-- NOTE: drop contours with less than 3 points.
-- Turn pairs of points into lists of connected points
getContours :: [[Point]] -> [Contour]
getContours pointPairs = Contour <$> (filteredContourSets foundContourSets)
  where
    filteredContourSets :: [[Point]] -> [[Point]]
    filteredContourSets points = catMaybes $ contourLongEnough <$> points
      where
        contourLongEnough :: [Point] -> Maybe [Point]
        contourLongEnough points
          | length points > 3 = Just points
          | otherwise = Nothing
    foundContourSets :: [[Point]]
    foundContourSets = makeContours . (,) [] $ pointPairs

-- Given a line, generate a pair of lines from points on both sides of the given line's midpoint to the origin, on the same z plane as the given line.
perimeterLinesToCheck :: ℝ -> Line -> (Line, Line)
perimeterLinesToCheck pathWidth l@(Line p _) = (head linePair, last linePair)
  where
    linePair = (`lineFromEndpoints` Point (0,0,zOf p)) . endpoint . pointSlopeLength (midpoint l) (lineSlope m) . (*pathWidth) <$> [-1,1]
    Line _ m = perpendicularBisector l
    zOf :: Point -> ℝ
    zOf (Point (_,_,z)) = z

-- Find an interior point on the perpendicular bisector of the given line, pathWidth from the line.
innerPerimeterPoint :: ℝ -> [Contour] -> Line -> Point
innerPerimeterPoint pathWidth contours l
    | length oddIntersections > 0 = snd $ head oddIntersections
    | length nonzeroIntersections > 0 = snd $ head nonzeroIntersections
    | length intersections > 0 = snd $ head intersections
    | otherwise = error $ "no intersections for line " <> show l <> "\n"
    where
      linesToCheck = perimeterLinesToCheck pathWidth l
      bothLinesToCheck = fst linesToCheck : [snd linesToCheck]
      intersections = (\a -> (numIntersections a, point a)) <$> bothLinesToCheck
      contourLines = foldMap makeLinesLooped $ (\(Contour points) -> points) <$> contours
      numIntersections l' = length $ mapMaybe (lineIntersection l') contourLines
      oddIntersections = filter (odd . fst) intersections
      nonzeroIntersections = filter ((/=0) . fst) intersections

-- Find an exterior point on the perpendicular bisector of the given line, pathWidth from the line.
outerPerimeterPoint :: ℝ -> [Contour] -> Line -> Point
outerPerimeterPoint pathWidth contours l
      | (snd $ head intersections) == innerPoint = snd $ last intersections
      | otherwise = snd $ head intersections
    where
      linesToCheck = perimeterLinesToCheck pathWidth l
      bothLinesToCheck = fst linesToCheck : [snd linesToCheck]
      intersections = (\a -> (numIntersections a, point a)) <$> bothLinesToCheck
      contourLines = foldMap makeLinesLooped $ (\(Contour points) -> points) <$> contours
      numIntersections l' = length $ mapMaybe (lineIntersection l') contourLines
      innerPoint = innerPerimeterPoint pathWidth contours l


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
      | otherwise = mapMaybe (lineIntersection line) $ makeLinesLooped contourPoints
    innerPointOf contour = innerPerimeterPoint 0.0001 (parent:child:[]) $ oneLineOf contour
      where
        oneLineOf (Contour contourPoints) = head $ makeLines contourPoints

-- determine whether a contour is inside of another contour.
contourContainedByContour :: Contour -> Contour -> Maybe Contour
contourContainedByContour child parent = if odd noIntersections then Just child else Nothing
  where
    noIntersections = length $ getContourLineIntersections parent $ lineFromEndpoints (innerPointOf child) (Point (0,0,0)) 
    getContourLineIntersections :: Contour -> Line -> [Point]
    getContourLineIntersections (Contour contourPoints) line = mapMaybe (lineIntersection line) $ makeLinesLooped contourPoints
    innerPointOf contour = innerPerimeterPoint 0.0001 (parent:child:[]) $ oneLineOf contour
      where
        oneLineOf (Contour contourPoints) = head $ makeLines contourPoints

