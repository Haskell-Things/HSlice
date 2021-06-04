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

-----------------------------------------------------------------------
---------------------- Infill Generation ------------------------------
-----------------------------------------------------------------------

module Graphics.Slicer.Machine.Infill (makeInfill, InfillType(Diag1, Diag2, Vert, Horiz), infillLineSegInside, coveringLineSegsVertical) where

import Prelude ((+), (<$>), ($), maximum, minimum, filter, (>), head, (.), flip, (*), sqrt, (-), (<>), show, error, otherwise, (==), length, concat, not, null, (!!), odd, Either (Left, Right), zip)

import Data.List.Ordered (sort)

import Data.Maybe (Maybe(Just, Nothing), catMaybes)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Contour (pointsOfContour, linesOfContour)

import Graphics.Slicer.Math.Definitions (Point2(Point2), Contour, distance, xOf, yOf, roundToFifth, mapWithNeighbors)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg), makeLineSegs)

import Graphics.Slicer.Math.PGA (Intersection(HitStartPoint, HitEndPoint, NoIntersection), PIntersection(PParallel, PAntiParallel, PCollinear, IntersectsIn), PLine2, pToEPoint2, intersectsWith, eToPLine2)

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
      infillCover Diag1 = coveringLineSegsPositive contour ls
      infillCover Diag2 = coveringLineSegsNegative contour ls

-- Get the segments of an infill line that are inside of a contour, skipping space occluded by any of the child contours.
infillLineSegInside :: Contour -> [Contour] -> PLine2 -> Maybe [LineSeg]
infillLineSegInside contour childContours line
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
          getLineSegIntersections :: PLine2 -> Contour -> [Point2]
          getLineSegIntersections myline c = saneIntersections $ zip (linesOfContour c) $ intersectsWith (Right myline) . Left <$> linesOfContour c
            where
              -- FIXME: why were we snapping to grid here?
              saneIntersections :: [(LineSeg, Either Intersection PIntersection)] -> [Point2]
              saneIntersections xs = catMaybes $ mapWithNeighbors saneIntersection xs
                where
                  saneIntersection :: (LineSeg, Either Intersection PIntersection) -> (LineSeg, Either Intersection PIntersection) -> (LineSeg, Either Intersection PIntersection) -> Maybe Point2
                  saneIntersection _ (_, Right (IntersectsIn ppoint)) _ = Just $ pToEPoint2 ppoint
                  saneIntersection _ (_, Left NoIntersection)         _ = Nothing
                  saneIntersection _ (_, Right PParallel)             _ = Nothing
                  saneIntersection _ (_, Right PAntiParallel)         _ = Nothing
                  -- Since every stop point of one line segment in a contour should be the same as the next start point...
                  -- only count the first start point, when going in one direction..
                  saneIntersection _                               (_, Left (HitStartPoint _ point)) (_, Left (HitEndPoint   _ _)) = Just point
                  saneIntersection (_, Left (HitStartPoint _ _)) (_, Left (HitEndPoint   _ _    )) _                               = Nothing
                  -- and only count the first start point, when going in the other direction.
                  saneIntersection _                               (_, Left (HitEndPoint   _ _    )) (_, Left (HitStartPoint _ _)) = Nothing
                  saneIntersection (_, Left (HitEndPoint   _ _)) (_, Left (HitStartPoint _ point)) _                               = Just point
                  -- Ignore the end and start point that comes before / after a collinear section.
                  saneIntersection (_, Right PCollinear)          (_, Left (HitStartPoint _ _    )) _                               = Nothing
                  saneIntersection _                               (_, Left (HitEndPoint   _ _    )) (_, Right PCollinear)          = Nothing
                  saneIntersection (_, Right PCollinear)          (_, Left (HitEndPoint   _ _    )) _                               = Nothing
                  saneIntersection _                               (_, Left (HitStartPoint _ _    )) (_, Right PCollinear)          = Nothing
                  -- FIXME: we should 'stitch out' collinear segments, not just ignore them.
                  saneIntersection _                               (_, Right PCollinear)              _                             = Nothing
                  -- saneIntersection (Left NoIntersection)      (Left (HitEndPoint   _ point)) (Left NoIntersection)      = Just point
                  saneIntersection r1 r2 r3 = error $ "insane result of intersecting a line (" <> show myline <> ") with a contour " <> show c <> "\n" <> show r1 <> "\n" <> show r2 <> "\n" <> show r3 <> "\n"

-- Generate lines covering the entire contour, where each one is aligned with a +1 slope, which is to say, lines parallel to a line where x = y.
-- FIXME: assumes we're in positive space.
coveringLineSegsPositive :: Contour -> ℝ -> [PLine2]
coveringLineSegsPositive contour ls = eToPLine2 . flip LineSeg slope . f <$> [0,lss..(xMax-xMinRaw)+(yMax-yMin)+lss]
    where
      contourPoints = pointsOfContour contour
      slope = Point2 (1,1)
      f v = Point2 (v-xDiff,0)
      xDiff = -(xMin - yMax)
      yMin = minimum $ yOf <$> contourPoints
      yMax = maximum $ yOf <$> contourPoints
      xMinRaw = minimum $ xOf <$> contourPoints
      xMin = head $ filter (> xMinRaw) [0, lss..]
      xMax = maximum $ xOf <$> contourPoints
      -- line spacing, taking into account the slope.
      lss = sqrt $ ls*ls+ls*ls

-- Generate lines covering the entire contour, where each one is aligned with a -1 slope, which is to say, lines parallel to a line where x = -y.
-- FIXME: assumes we're in positive space.
coveringLineSegsNegative :: Contour -> ℝ -> [PLine2]
coveringLineSegsNegative contour ls = eToPLine2 . flip LineSeg slope . f <$> [0,lss..(xMax-xMin)+(yMax-yMin)+lss]
    where
      contourPoints = pointsOfContour contour
      slope =  Point2 (1,-1)
      f v = Point2 (v+yDiff,0)
      yDiff = xMin + yMin
      yMinRaw = minimum $ yOf <$> contourPoints
      yMin = head $ filter (> yMinRaw) [0, lss..]
      yMax = maximum $ yOf <$> contourPoints
      xMin = minimum $ xOf <$> contourPoints
      xMax = maximum $ xOf <$> contourPoints
      -- line spacing, taking into account the slope.
      lss = sqrt $ ls*ls+ls*ls

-- Generate lines covering the entire contour, where each line is aligned with the Y axis, which is to say, parallel to the Y basis vector.
-- FIXME: assumes we're in positive space.
coveringLineSegsVertical :: Contour -> ℝ -> [PLine2]
coveringLineSegsVertical contour ls = eToPLine2 . flip LineSeg slope . f <$> [xMin,xMin+ls..xMax]
    where
      contourPoints = pointsOfContour contour
      slope = Point2 (0,1)
      f v = Point2 (v,0)
      xMinRaw = minimum $ xOf <$> contourPoints
      xMin = head $ filter (> xMinRaw) [0, ls..]
      xMax = maximum $ xOf <$> contourPoints

-- Generate lines covering the entire contour, where each line is aligned with the X axis, which is to say, parallel to the X basis vector.
-- FIXME: assumes we're in positive space.
coveringLineSegsHorizontal :: Contour -> ℝ -> [PLine2]
coveringLineSegsHorizontal contour ls = eToPLine2 . flip LineSeg slope . f <$> [yMin,yMin+ls..yMax]
    where
      contourPoints = pointsOfContour contour
      slope = Point2 (1,0)
      f v = Point2 (0,v)
      yMinRaw = minimum $ yOf <$> contourPoints
      yMin = head $ filter (> yMinRaw) [0, ls..]
      yMax = maximum $ yOf <$> contourPoints


