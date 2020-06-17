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

module Graphics.Slicer.Machine.Infill (makeInfill, makeSupport) where

import Prelude ((+), (<$>), ($), maximum, minimum, filter, (>), head, (.), flip, (*), sqrt, (-), (<>), show, error, otherwise, (&&), (==), length, (<), concat, not, null, (!!), fmap, (||))

import Data.Maybe (Maybe(Just, Nothing), catMaybes, mapMaybe, fromMaybe)

import Data.Bool (Bool(True, False))

import Data.List (sortBy)

import Graphics.Slicer.Definitions (ℝ,ℝ2)

import Graphics.Slicer.Math.Definitions (Point(Point), Contour(Contour), LayerType(BaseEven, BaseOdd), distance)

import Graphics.Slicer.Math.Point (orderPoints)

import Graphics.Slicer.Math.Line (Line(Line), Intersection(HitEndpointL2, IntersectsAt, NoIntersection, Parallel), makeLines, makeLinesLooped, lineIntersection, shortenLineBy)

import Graphics.Slicer.Math.Contour (lineEntersContour)

-- Generate infill for a layer.
-- Basically, cover the build plane in lines, then remove the portions of those lines that are not inside of the target contour.
-- The target contour should be the innermost parameter, and the target inside contours should also be the innermost parameters.
makeInfill :: Contour -> [Contour] -> ℝ -> ℝ -> LayerType -> [[Line]]
makeInfill contour insideContours ls zHeight layerType = catMaybes $ infillLineInside contour insideContours <$> infillCover layerType
    where infillCover BaseEven = coveringLinesNegative contour ls zHeight
          infillCover BaseOdd = coveringLinesPositive contour ls zHeight

-- Get the segments of an infill line that are inside of a contour, skipping space occluded by any of the child contours.
-- May return multiple lines, or empty set.
infillLineInside :: Contour -> [Contour] -> Line -> Maybe [Line]
infillLineInside contour childContours line
--  | not (null allLines) && length allLines > 1 = error $ show contour <> "\n" <> show childContours <> "\n" <> show line <> concat (show <$> [(allLines !!) <$> [0,2..length allLines - 1]] )
--  | not (null allLines) = [head allLine]
  | not (null allLines) = Just $ (allLines !!) <$> [0,2..length allLines - 1]
  | otherwise = Nothing -- error $ "did not find an intersection with a contour for: \n" <> show line <> "\n"
    where
      allLines :: [Line]
      allLines = if null allPoints then [] else makeLines $ allPoints
      allPoints = filterTooShort $ sortBy orderPoints $ concat $ getLineIntersections line <$> contour:childContours
      filterTooShort :: [Point] -> [Point]
      filterTooShort [] = []
      filterTooShort [a] = [a]
      filterTooShort (a:b:xs) = if distance a b < 0.01 then filterTooShort xs else a:(filterTooShort (b:xs))
      getLineIntersections :: Line -> Contour -> [Point]
      getLineIntersections myline c = catMaybes $ saneIntersections $ cookIntersections $ lineIntersection myline <$> linesOfContour c
        where
          cookIntersections :: [Intersection] -> [Intersection]
          cookIntersections res
            | length res == 1 && hitsEndpoint (head res) = []
            | otherwise = res
            where
              hitsEndpoint (HitEndpointL2 _) = True
              hitsEndpoint _ = False
          saneIntersections :: [Intersection] -> [Maybe Point]
          saneIntersections xs = saneIntersection <$> xs
          saneIntersection :: Intersection -> Maybe Point
          saneIntersection (IntersectsAt _ p2) = Just p2
          saneIntersection i@(HitEndpointL2 p2) = if lineEntersContour myline i c then Just p2 else Nothing
          saneIntersection NoIntersection = Nothing
          saneIntersection Parallel = Nothing
          saneIntersection res = error $ "insane result when infilling contour:\n" <> show res <> "\n" <> show c <> "\n"
          linesOfContour myC = (\(Contour contourPoints) -> makeLinesLooped contourPoints) myC

-- Generate lines over entire print area, where each one is aligned with a -1 slope.
-- FIXME: other ways to only generate covering lines over the outer contour?
coveringLinesNegative :: Contour -> ℝ -> ℝ -> [Line]
coveringLinesNegative (Contour contourPoints) ls zHeight = flip Line s . f <$> [-xMin,-xMin+lsX..xMax]
    where s = Point (xMaxOutside,yMaxOutside,0)
          f v = Point (v,0,zHeight)
          xMinRaw = minimum $ xOf <$> contourPoints
          xMin = head $ filter (> xMinRaw) [0,ls..]
          xMax = maximum $ xOf <$> contourPoints
          yMax = maximum $ yOf <$> contourPoints
          xMaxOutside = xMax + ls
          yMaxOutside = yMax + ls
          lsX = sqrt $ ls*ls+ls*ls
          xOf, yOf :: Point -> ℝ
          xOf (Point (x,_,_)) = x
          yOf (Point (_,y,_)) = y

-- Generate lines over entire print area, where each one is aligned with a +1 slope.
-- FIXME: other ways to only generate covering lines over the outer contour?
coveringLinesPositive :: Contour -> ℝ -> ℝ -> [Line]
coveringLinesPositive (Contour contourPoints) ls zHeight = flip Line s . f <$> [0,lsY..yMax + xMax]
    where s =  Point (xMaxOutside + yMaxOutside,- xMaxOutside - yMaxOutside,0)
          f v = Point (0,v,zHeight)
          yMinRaw = minimum $ yOf <$> contourPoints
          yMin = head $ filter (> yMinRaw) [0,ls..]
          yMax = maximum $ yOf <$> contourPoints
          xMax = maximum $ xOf <$> contourPoints
          xMaxOutside = xMax + ls
          yMaxOutside = yMax + ls
          lsY = sqrt $ ls*ls+ls*ls
          xOf, yOf :: Point -> ℝ
          xOf (Point (x,_,_)) = x
          yOf (Point (_,y,_)) = y

-- Generate lines over entire contour, where each one is aligned with the Y axis.
-- FIXME: assumes we're in positive space.
coveringLinesVertical :: Contour -> ℝ -> ℝ -> [Line]
coveringLinesVertical (Contour contourPoints) ls zHeight = flip Line s . f <$> [xMin,xMin+ls..xMax]
    where s =  Point (0,yMaxOutside,0)
          f v = Point (v,0,zHeight)
          xMinRaw = minimum $ xOf <$> contourPoints
          xMin = head $ filter (> xMinRaw) [0,ls..]
          xMax = maximum $ xOf <$> contourPoints
          yMax = maximum $ yOf <$> contourPoints
          yMaxOutside = yMax + ls
          xOf, yOf :: Point -> ℝ
          xOf (Point (x,_,_)) = x
          yOf (Point (_,y,_)) = y

-- Generate lines over entire contour, where each one is aligned with the X axis.
-- FIXME: assumes we're in positive space.
coveringLinesHorizontal :: Contour -> ℝ -> ℝ -> [Line]
coveringLinesHorizontal (Contour contourPoints) ls zHeight = flip Line s . f <$> [yMin,yMin+ls..yMax]
    where s =  Point (xMaxOutside,0,0)
          f v = Point (0,v,zHeight)
          yMinRaw = minimum $ yOf <$> contourPoints
          yMin = head $ filter (> yMinRaw) [0,ls..]
          yMax = maximum $ yOf <$> contourPoints
          xMax = maximum $ xOf <$> contourPoints
          xMaxOutside = xMax + ls
          xOf, yOf :: Point -> ℝ
          xOf (Point (x,_,_)) = x
          yOf (Point (_,y,_)) = y


-----------------------------------------------------------------------
----------------------------- SUPPORT ---------------------------------
-----------------------------------------------------------------------

-- Generate support
-- FIXME: hard coded infill amount.
-- FIXME: should be one string.
makeSupport :: Contour
            -> [Contour]
            -> ℝ
            -> ℝ
            -> ℝ
            -> [Line]
makeSupport contour childContours lh ls zHeight = fmap (shortenLineBy $ 2 * lh)
                                                  $ concat $ catMaybes $ infillLineInside contour (addBBox childContours zHeight)
                                                  <$> coveringLinesVertical contour ls zHeight

-- A bounding box. a box around a contour.
data BBox = BBox ℝ2 ℝ2

-- Check if a bounding box is empty.
isEmptyBBox :: BBox -> Bool
isEmptyBBox (BBox (x1,y1) (x2,y2)) = x1 == x2 || y1 == y2

-- Get a bounding box of all contours.
boundingBoxAll :: [Contour] -> Maybe BBox
boundingBoxAll contours = if isEmptyBBox box then Nothing else Just box
    where
      box  = BBox (minX, minY) (maxX, maxY)
      minX = minimum $ (\(BBox (x1,_) _) -> x1) <$> bBoxes
      minY = minimum $ (\(BBox (_,y1) _) -> y1) <$> bBoxes
      maxX = maximum $ (\(BBox _ (x2,_)) -> x2) <$> bBoxes
      maxY = maximum $ (\(BBox _ (_,y2)) -> y2) <$> bBoxes
      bBoxes = mapMaybe boundingBox contours

-- Get a 2D bounding box of a 2D contour.
boundingBox :: Contour -> Maybe BBox
boundingBox (Contour []) = Nothing
boundingBox (Contour contourPoints) = if isEmptyBBox box then Nothing else Just box
  where
    box  = BBox (minX, minY) (maxX, maxY)
    minX = minimum $ xOf <$> contourPoints
    minY = minimum $ yOf <$> contourPoints
    maxX = maximum $ xOf <$> contourPoints
    maxY = maximum $ yOf <$> contourPoints
    xOf,yOf :: Point -> ℝ
    xOf (Point (x,_,_)) = x
    yOf (Point (_,y,_)) = y

-- add a 2D bounding box to a list of contours, as the first contour in the list.
-- FIXME: assumes 2D contour.
addBBox :: [Contour] -> ℝ -> [Contour]
addBBox contours z0 = Contour [Point (x1,y1,z0), Point (x2,y1,z0), Point (x2,y2,z0), Point (x1,y2,z0), Point (x1,y1,z0)] : contours
    where
      bbox = fromMaybe (BBox (1,1) (-1,-1)) $ boundingBoxAll contours
      (BBox (x1, y1) (x2, y2)) = incBBox bbox 1
      -- Put a fixed amount around the 2d bounding box.
      incBBox (BBox (nx1,ny1) (nx2,ny2)) amount = BBox (nx1+amount, ny1+amount) (nx2-amount, ny2-amount)


