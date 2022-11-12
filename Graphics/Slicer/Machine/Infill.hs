{- ORMOLU_DISABLE -}
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

module Graphics.Slicer.Machine.Infill (makeInfill, InfillType(Diag1, Diag2, Vert, Horiz), infillLineSegInside, coveringPLinesVertical) where

import Prelude ((+), (<$>), ($), (.), (*), sqrt, (-), Ordering(EQ, GT, LT), otherwise, (==), length, not, null, (!!), fromIntegral, ceiling, (/), floor, Integer, compare)

import Data.List (concatMap)

import Data.List.Ordered (sort)

import Data.Maybe (Maybe(Just, Nothing), mapMaybe)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), Contour, LineSeg, addPoints, distance, makeLineSeg, minMaxPoints, xOf, yOf, roundToFifth)

import Graphics.Slicer.Math.ContourIntersections (getPLine2Intersections)

import Graphics.Slicer.Math.Line (makeLineSegs)

import Graphics.Slicer.Math.PGA (ProjectiveLine, PLine2Err, eToPL)

-- | what direction to put down infill lines.
data InfillType = Diag1 | Diag2 | Vert | Horiz

-- Generate infill for a layer.
-- Basically, cover the build plane in lines, then remove the portions of those lines that are not inside of the target contour.
-- The target contour should be pre-shrunk to the innermost parameter, and the target inside contours should also be the outermost parameters.
makeInfill :: Contour -> [Contour] -> ℝ -> InfillType -> [[LineSeg]]
makeInfill contour insideContours ls layerType = mapMaybe (infillLineSegInside contour insideContours) $ infillCover layerType
    where
      infillCover Vert = coveringPLinesVertical contour ls
      infillCover Horiz = coveringPLinesHorizontal contour ls
      infillCover Diag1 = coveringPLinesPositive contour ls
      infillCover Diag2 = coveringPLinesNegative contour ls

-- Get the segments of an infill line that are inside of a contour, skipping space occluded by any of the child contours.
infillLineSegInside :: Contour -> [Contour] -> (ProjectiveLine,PLine2Err) -> Maybe [LineSeg]
infillLineSegInside contour childContours line
  | not (null allLines) = Just $ (allLines !!) <$> [0,2..length allLines - 1]
  | otherwise = Nothing
    where
      allLines :: [LineSeg]
      allLines = makeLineSegs allPoints
        where
          allPoints = (filterTooShort . sort) $ concatMap (getPLine2Intersections line) (contour:childContours)
          filterTooShort :: [Point2] -> [Point2]
          filterTooShort [] = []
          filterTooShort [a] = [a]
          filterTooShort (a:b:xs) = if roundToFifth (distance a b) == 0 then filterTooShort xs else a:filterTooShort (b:xs)

-- Generate lines covering the entire contour, where each one is aligned with a +1 slope, which is to say, lines parallel to a line where x = y.
coveringPLinesPositive :: Contour -> ℝ -> [(ProjectiveLine, PLine2Err)]
coveringPLinesPositive contour ls = eToPL . makeSeg <$> [0,lss..(xMax-xMinRaw)+(yMax-yMin)+lss]
    where
      makeSeg a = makeLineSeg (f a) $ addPoints (f a) slope
      (minPoint, maxPoint) = minMaxPoints contour
      slope = Point2 (1,1)
      f v = Point2 (v-xDiff,0)
      xDiff = -(xMin - yMax)
      yMin = yOf minPoint
      yMax = yOf maxPoint
      xMinRaw = xOf minPoint
      xMin = case xMinRaw `compare` 0 of
        GT -> lss * fromIntegral (ceiling (xMinRaw / lss) :: Integer)
        LT -> lss * fromIntegral (floor (xMinRaw / lss) :: Integer)
        EQ -> 0
      xMax = xOf maxPoint
      -- line spacing, taking into account the slope.
      lss = sqrt $ ls*ls+ls*ls

-- Generate lines covering the entire contour, where each one is aligned with a -1 slope, which is to say, lines parallel to a line where x = -y.
coveringPLinesNegative :: Contour -> ℝ -> [(ProjectiveLine, PLine2Err)]
coveringPLinesNegative contour ls = eToPL . makeSeg <$> [0,lss..(xMax-xMin)+(yMax-yMin)+lss]
    where
      makeSeg a = makeLineSeg (f a) $ addPoints (f a) slope
      (minPoint, maxPoint) = minMaxPoints contour
      slope =  Point2 (1,-1)
      f v = Point2 (v+yDiff,0)
      yDiff = xMin + yMin
      yMinRaw = yOf minPoint
      yMin = case yMinRaw `compare` 0 of
        GT -> lss * fromIntegral (ceiling (yMinRaw / lss) :: Integer)
        LT -> lss * fromIntegral (floor (yMinRaw / lss) :: Integer)
        EQ -> 0
      yMax = yOf maxPoint
      xMin = xOf minPoint
      xMax = xOf maxPoint
      -- line spacing, taking into account the slope.
      lss = sqrt $ ls*ls+ls*ls

-- Generate lines covering the entire contour, where each line is aligned with the Y axis, which is to say, parallel to the Y basis vector.
coveringPLinesVertical :: Contour -> ℝ -> [(ProjectiveLine, PLine2Err)]
coveringPLinesVertical contour ls = eToPL . makeSeg <$> [xMin,xMin+ls..xMax]
    where
      makeSeg a = makeLineSeg (f a) $ addPoints (f a) slope
      (minPoint, maxPoint) = minMaxPoints contour
      slope = Point2 (0,1)
      f v = Point2 (v,0)
      xMinRaw = xOf minPoint
      xMin = case xMinRaw `compare` 0 of
        GT -> ls * fromIntegral (ceiling (xMinRaw / ls) :: Integer)
        LT -> ls * fromIntegral (floor (xMinRaw / ls) :: Integer)
        EQ -> 0
      xMax = xOf maxPoint

-- Generate lines covering the entire contour, where each line is aligned with the X axis, which is to say, parallel to the X basis vector.
coveringPLinesHorizontal :: Contour -> ℝ -> [(ProjectiveLine, PLine2Err)]
coveringPLinesHorizontal contour ls = eToPL . makeSeg <$> [yMin,yMin+ls..yMax]
    where
      makeSeg a = makeLineSeg (f a) $ addPoints (f a) slope
      (minPoint, maxPoint) = minMaxPoints contour
      slope = Point2 (1,0)
      f v = Point2 (0,v)
      yMinRaw = yOf minPoint
      yMin = case yMinRaw `compare` 0 of
        GT -> ls * fromIntegral (ceiling (yMinRaw / ls) :: Integer)
        LT -> ls * fromIntegral (floor (yMinRaw / ls) :: Integer)
        EQ -> 0
      yMax = yOf maxPoint

