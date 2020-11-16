-----------------------------------------------------------------------
---------------------- Infill Generation ------------------------------
-----------------------------------------------------------------------

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

module Graphics.Slicer.Machine.Infill (makeInfill, InfillType(Diag1, Diag2, Vert, Horiz), infillLineSegInside, coveringLineSegsVertical) where

import Prelude ((+), (<$>), ($), maximum, minimum, filter, (>), head, (.), flip, (*), sqrt, (-), (<>), show, error, otherwise, (==), length, concat, not, null, (!!), odd)

import Data.List.Ordered (sort)

import Data.Maybe (Maybe(Just, Nothing), catMaybes, mapMaybe)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), Contour(PointSequence), distance, xOf, yOf, roundToFifth)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg), makeLineSegs, makeLineSegsLooped)

import Graphics.Slicer.Math.PGA (Intersection(HitStartPointL2, HitEndPointL2, IntersectsAt, NoIntersection, Parallel, AntiParallel, Collinear, LColinear), lineIntersection, lineIsLeft)

import Graphics.Slicer.Math.Contour (preceedingLineSeg)

-- | what direction to put down infill lines.
data InfillType = Diag1 | Diag2 | Vert | Horiz

-- Generate infill for a layer.
-- Basically, cover the build plane in lines, then remove the portions of those lines that are not inside of the target contour.
-- The target contour should be the innermost parameter, and the target inside contours should also be the innermost parameters.
makeInfill :: Contour -> [Contour] -> ℝ -> InfillType -> [[LineSeg]]
makeInfill contour insideContours ls layerType = catMaybes $ infillLineSegInside contour insideContours <$> infillCover layerType
    where
      infillCover Vert = coveringLineSegsVertical contour ls
      infillCover Horiz = coveringLineSegsHorizontal contour ls
      infillCover Diag1 = coveringLineSegsNegative contour ls
      infillCover Diag2 = coveringLineSegsPositive contour ls

-- Get the segments of an infill line that are inside of a contour, skipping space occluded by any of the child contours.
infillLineSegInside :: Contour -> [Contour] -> LineSeg -> Maybe [LineSeg]
infillLineSegInside contour childContours line
--  | contour == contour  = error $ "dumping infill inputs:" <> show contour <> "\n" <> show childContours <> "\n" <> show line <> "\n"
  | not (null allLines) = Just $ (allLines !!) <$> [0,2..length allLines - 1]
  | otherwise = Nothing
    where
      allLines :: [LineSeg]
      allLines
        | null allPoints         = []
        | odd $ length allPoints = error $ "found odd number of points:\n" <> show allPoints <> "\noverlaying line:\n" <> show line <> "\nonto contour:\n" <> show contour <> "\n" <> show childContours <> "\n"
        | otherwise              = makeLineSegs allPoints
        where
          allPoints = filterTooShort . sort . concat $ getLineSegIntersections line <$> contour:childContours
          filterTooShort :: [Point2] -> [Point2]
          filterTooShort [] = []
          filterTooShort [a] = [a]
          filterTooShort (a:b:xs) = if roundToFifth (distance a b) == 0 then filterTooShort xs else a:filterTooShort (b:xs)
          getLineSegIntersections :: LineSeg -> Contour -> [Point2]
          getLineSegIntersections myline c = saneIntersections $ lineIntersection myline <$> linesOfContour c
            where
              linesOfContour (PointSequence contourPoints) = makeLineSegsLooped $ (\(Point2 (x,y)) -> Point2 (roundToFifth x, roundToFifth y)) <$> contourPoints
              saneIntersections :: [Intersection] -> [Point2]
              saneIntersections xs = concat $ mapMaybe saneIntersection xs
                where
                  saneIntersection :: Intersection -> Maybe [Point2]
                  -- FIXME: we should 'stitch out' colinear segments, not just ignore them.
                  saneIntersection (LColinear _ _) = Nothing
--                  saneIntersection (LColinear l1 l2@(Line p2 _)) = if lineIsLeft l1 lineTo == lineIsLeft l1 lineFrom then Just [p2, endpoint l2] else Nothing
--                    where
--                      lineTo   = preceedingLine (linesOfContour c) l2
--                      lineFrom = followingLine  (linesOfContour c) l2
                  saneIntersection (HitStartPointL2 l1 l2 p2) = if lineIsLeft l1 lineTo == lineIsLeft l1 l2 then Just [p2] else Nothing
                    where
                      lineTo   = preceedingLineSeg (linesOfContour c) l2
                  saneIntersection (HitEndPointL2 {}) = Nothing
--                  saneIntersection (HitEndPointL2 l1 l2 p2) = if lineIsLeft l1 l2 == lineIsLeft l1 lineFrom then Just [p2] else Nothing
--                    where
--                      lineFrom = followingLine  (linesOfContour c) l2
                  saneIntersection (IntersectsAt p2) = Just [p2]
                  saneIntersection NoIntersection = Nothing
                  saneIntersection Parallel = Nothing
                  saneIntersection AntiParallel = Nothing
                  saneIntersection Collinear = error "not possible?"

-- Generate lines over entire print area, where each one is aligned with a -1 slope.
-- FIXME: other ways to only generate covering lines over the outer contour?
-- FIXME: assumes we're in positive space.
coveringLineSegsNegative :: Contour -> ℝ -> [LineSeg]
coveringLineSegsNegative (PointSequence contourPoints) ls = flip LineSeg s . f <$> [-xMin,-xMin+lsX..xMax]
    where s = Point2 (xMaxOutside,yMaxOutside)
          f v = Point2 (v,0)
          xMinRaw = minimum $ xOf <$> contourPoints
          xMin = head $ filter (> xMinRaw) [-ls, 0..]
          xMax = maximum $ xOf <$> contourPoints
          yMax = maximum $ yOf <$> contourPoints
          xMaxOutside = xMax + ls
          yMaxOutside = yMax + ls
          lsX = sqrt $ ls*ls+ls*ls

-- Generate lines over entire print area, where each one is aligned with a +1 slope.
-- FIXME: other ways to only generate covering lines over the outer contour?
-- FIXME: assumes we're in positive space.
coveringLineSegsPositive :: Contour -> ℝ -> [LineSeg]
coveringLineSegsPositive (PointSequence contourPoints) ls = flip LineSeg s . f <$> [0,lsY..yMax + xMax]
    where s =  Point2 (xMaxOutside + yMaxOutside,- xMaxOutside - yMaxOutside)
          f v = Point2 (0,v)
          xMax = maximum $ xOf <$> contourPoints
          yMax = maximum $ yOf <$> contourPoints
          xMaxOutside = xMax + ls
          yMaxOutside = yMax + ls
          lsY = sqrt $ ls*ls+ls*ls

-- Generate lines covering the entire contour, where each line is aligned with the Y axis.
-- FIXME: assumes we're in positive space.
coveringLineSegsVertical :: Contour -> ℝ -> [LineSeg]
coveringLineSegsVertical (PointSequence contourPoints) ls = flip LineSeg s . f <$> [xMin-ls,xMin..xMax]
    where s =  Point2 (0,yMax+ls)
          f v = Point2 (v,-ls)
          xMinRaw = minimum $ xOf <$> contourPoints
          xMin = head $ filter (> xMinRaw) [-ls, 0..]
          xMax = maximum $ xOf <$> contourPoints
          yMax = maximum $ yOf <$> contourPoints

-- Generate lines covering the entire contour, where each line is aligned with the X axis.
-- FIXME: assumes we're in positive space.
coveringLineSegsHorizontal :: Contour -> ℝ -> [LineSeg]
coveringLineSegsHorizontal (PointSequence contourPoints) ls = flip LineSeg s . f <$> [yMin-ls,yMin..yMax]
    where s =  Point2 (xMax+ls,0)
          f v = Point2 (-ls,v)
          yMinRaw = minimum $ yOf <$> contourPoints
          yMin = head $ filter (> yMinRaw) [-ls, 0..]
          xMax = maximum $ xOf <$> contourPoints
          yMax = maximum $ yOf <$> contourPoints


