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

module Graphics.Slicer.Machine.Infill (
  InfillFamily(Concentric, Lines),
  InfillType(ConcentricContours, Diag1, Diag2, Vert, Horiz),
  coveringPLinesVertical,
  infillLineSegInside,
  makeInfill
  ) where

import Prelude (Eq, Show(show), (+), (<>), (<$>), ($), (.), (*), sqrt, (-), Ordering(EQ, GT, LT), otherwise, (==), length, not, null, (!!), fromIntegral, ceiling, (/), floor, Integer, compare, error)

import Data.List (concatMap)

import Data.List.Ordered (sort)

import Data.Maybe (Maybe(Just, Nothing), mapMaybe, fromMaybe)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), Contour, LineSeg, addPoints, distance, minMaxPoints, xOf, yOf, roundToFifth)

import Graphics.Slicer.Math.ContourIntersections (getLineContourIntersections)

import Graphics.Slicer.Math.Line (makeLineSegs)

import Graphics.Slicer.Math.PGA (ProjectiveLine, ProjectiveLine2, PLine2Err, join2EP)

import Graphics.Slicer.Math.Skeleton.Face (facesOf)

import Graphics.Slicer.Math.Skeleton.Line (infiniteInset)

import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- | Which family of infill should we use for a given layer? each InfillType belongs to one Infill family.
data InfillFamily = Lines | Concentric
  deriving (Eq)

-- | What type of infill in specific to generate.
data InfillType = Diag1 | Diag2 | Vert | Horiz | ConcentricContours
  deriving (Eq)

-- | Generate infill for a layer.
-- Basically, cover the build plane in lines, then remove the portions of those lines that are not inside of the target contour.
-- The target contour should be pre-shrunk to the innermost parameter, and the target inside contours should also be expanded to their outermost parameters.
makeInfill :: Contour -> [Contour] -> ℝ -> InfillType -> [[LineSeg]]
makeInfill contour insideContours ls layerType =
  case layerType of
    Vert -> intersectWithContours $ coveringPLinesVertical contour ls
    Horiz -> intersectWithContours $ coveringPLinesHorizontal contour ls
    Diag1 -> intersectWithContours $ coveringPLinesPositive contour ls
    Diag2 -> intersectWithContours $ coveringPLinesNegative contour ls
    ConcentricContours -> infillConcentricInside contour insideContours ls
  where
    -- | Accept a list of line segments, colide them against our contours, and return a list of lists for where infill should be placed.
    intersectWithContours :: (ProjectiveLine2 a) => [(a, PLine2Err)] -> [[LineSeg]]
    intersectWithContours = mapMaybe (infillLineSegInside contour insideContours)

infillConcentricInside :: Contour -> [Contour] -> ℝ -> [[LineSeg]]
infillConcentricInside contour insideContours lineSpacing
  | null insideContours = case skeleton of
                            (Just s) -> infiniteInset lineSpacing $ facesOf s
                            Nothing -> error $ "failed to find a skeleton when infilling contour:\n" <> show contour
  | otherwise = error "cannot have Concentric infill with holes (yet).."
  where
    skeleton = findStraightSkeleton contour insideContours

-- | Get the segments of an infill line that are inside of a contour, skipping space occluded by any of the child contours.
infillLineSegInside :: (ProjectiveLine2 a) => Contour -> [Contour] -> (a, PLine2Err) -> Maybe [LineSeg]
infillLineSegInside contour childContours line
  | not (null allLines) = Just $ (allLines !!) <$> [0,2..length allLines - 1]
  | otherwise = Nothing
    where
      allLines :: [LineSeg]
      allLines = makeLineSegs allPoints
        where
          allPoints = (filterTooShort . sort) $ concatMap (\a -> fromMaybe (dumpError a) $ getLineContourIntersections line a) (contour:childContours)
            where
              dumpError failContour = error $ "getLineContourIntersections failed when placing line across contour:\n"
                                           <> show line <> "\n"
                                           <> show failContour <> "\n"
          filterTooShort :: [Point2] -> [Point2]
          filterTooShort [] = []
          filterTooShort [a] = [a]
          filterTooShort (a:b:xs) = if roundToFifth (distance a b) == 0 then filterTooShort xs else a:filterTooShort (b:xs)

-- | Generate lines covering the entire contour, where each one is aligned with a +1 slope, which is to say, lines parallel to a line where x = y.
coveringPLinesPositive :: Contour -> ℝ -> [(ProjectiveLine, PLine2Err)]
coveringPLinesPositive contour ls = makeLine <$> [0,lss..(xMax-xMinRaw)+(yMax-yMin)+lss]
    where
      makeLine a = join2EP (f a) $ addPoints (f a) slope
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

-- | Generate lines covering the entire contour, where each one is aligned with a -1 slope, which is to say, lines parallel to a line where x = -y.
coveringPLinesNegative :: Contour -> ℝ -> [(ProjectiveLine, PLine2Err)]
coveringPLinesNegative contour ls = makeLine <$> [0,lss..(xMax-xMin)+(yMax-yMin)+lss]
    where
      makeLine a = join2EP (f a) $ addPoints (f a) slope
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

-- | Generate lines covering the entire contour, where each line is aligned with the Y axis, which is to say, parallel to the Y basis vector.
coveringPLinesVertical :: Contour -> ℝ -> [(ProjectiveLine, PLine2Err)]
coveringPLinesVertical contour ls = makeLine <$> [xMin,xMin+ls..xMax]
    where
      makeLine a = join2EP (f a) $ addPoints (f a) slope
      (minPoint, maxPoint) = minMaxPoints contour
      slope = Point2 (0,1)
      f v = Point2 (v,0)
      xMinRaw = xOf minPoint
      xMin = case xMinRaw `compare` 0 of
        GT -> ls * fromIntegral (ceiling (xMinRaw / ls) :: Integer)
        LT -> ls * fromIntegral (floor (xMinRaw / ls) :: Integer)
        EQ -> 0
      xMax = xOf maxPoint

-- | Generate lines covering the entire contour, where each line is aligned with the X axis, which is to say, parallel to the X basis vector.
coveringPLinesHorizontal :: Contour -> ℝ -> [(ProjectiveLine, PLine2Err)]
coveringPLinesHorizontal contour ls = makeLine <$> [yMin,yMin+ls..yMax]
    where
      makeLine a = join2EP (f a) $ addPoints (f a) slope
      (minPoint, maxPoint) = minMaxPoints contour
      slope = Point2 (1,0)
      f v = Point2 (0,v)
      yMinRaw = yOf minPoint
      yMin = case yMinRaw `compare` 0 of
        GT -> ls * fromIntegral (ceiling (yMinRaw / ls) :: Integer)
        LT -> ls * fromIntegral (floor (yMinRaw / ls) :: Integer)
        EQ -> 0
      yMax = yOf maxPoint

