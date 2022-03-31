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

module Graphics.Slicer.Machine.Infill (makeInfill, InfillType(Diag1, Diag2, Vert, Horiz), infillLineSegInside, coveringLineSegsVertical) where

import Prelude ((+), (<$>), ($), (.), flip, (*), sqrt, (-), (<>), Ordering(EQ, GT, LT), show, error, otherwise, (==), length, concat, not, null, (!!), odd, fromIntegral, ceiling, (/), floor, Integer, compare)

import Data.List.Ordered (sort)

import Data.Maybe (Maybe(Just, Nothing), catMaybes)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), Contour, LineSeg(LineSeg), distance, minMaxPoints, xOf, yOf, roundToFifth)

import Graphics.Slicer.Math.Intersections (getLineSegIntersections)

import Graphics.Slicer.Math.Line (makeLineSegs)

import Graphics.Slicer.Math.PGA (PLine2, eToPLine2)

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

-- Generate lines covering the entire contour, where each one is aligned with a +1 slope, which is to say, lines parallel to a line where x = y.
coveringLineSegsPositive :: Contour -> ℝ -> [PLine2]
coveringLineSegsPositive contour ls = eToPLine2 . flip LineSeg slope . f <$> [0,lss..(xMax-xMinRaw)+(yMax-yMin)+lss]
    where
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
coveringLineSegsNegative :: Contour -> ℝ -> [PLine2]
coveringLineSegsNegative contour ls = eToPLine2 . flip LineSeg slope . f <$> [0,lss..(xMax-xMin)+(yMax-yMin)+lss]
    where
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
coveringLineSegsVertical :: Contour -> ℝ -> [PLine2]
coveringLineSegsVertical contour ls = eToPLine2 . flip LineSeg slope . f <$> [xMin,xMin+ls..xMax]
    where
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
coveringLineSegsHorizontal :: Contour -> ℝ -> [PLine2]
coveringLineSegsHorizontal contour ls = eToPLine2 . flip LineSeg slope . f <$> [yMin,yMin+ls..yMax]
    where
      (minPoint, maxPoint) = minMaxPoints contour
      slope = Point2 (1,0)
      f v = Point2 (0,v)
      yMinRaw = yOf minPoint
      yMin = case yMinRaw `compare` 0 of
        GT -> ls * fromIntegral (ceiling (yMinRaw / ls) :: Integer)
        LT -> ls * fromIntegral (floor (yMinRaw / ls) :: Integer)
        EQ -> 0
      yMax = yOf maxPoint

