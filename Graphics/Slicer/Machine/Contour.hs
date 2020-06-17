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

module Graphics.Slicer.Machine.Contour (cleanContour) where

import Prelude (length, (>), ($), otherwise, (<$>))

import Data.List (nub, null)

import Data.Maybe (Maybe(Just, Nothing))

import Graphics.Slicer.Math.Definitions (Point(Point), Contour(Contour), distance)

import Graphics.Slicer.Math.Line (Line(Line), Intersection(HitEndpointL2, IntersectsAt, NoIntersection, Parallel), makeLines, makeLinesLooped, lineIntersection, pointsFromLines, combineConsecutiveLines)

import Graphics.Slicer.Formats.GCode.Definitions (roundPoint)

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

