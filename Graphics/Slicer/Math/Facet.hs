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

{- The purpose of this file is to hold facet based arithmatic. -}

module Graphics.Slicer.Math.Facet (Facet(Facet), sides, shiftFacet, facetIntersects, trimIntersections) where

import Prelude (Eq, map, (.), ($), length, (<=), otherwise)

import Data.List(nub)

import Data.Maybe(catMaybes)

import Graphics.Slicer.Definitions(ℝ)

import Graphics.Slicer.Math.Point (Point, addPoints)

import Graphics.Slicer.Math.Line (Line, point, pointAtZValue)

newtype Facet = Facet { sides :: [Line] } deriving Eq


-- Shift a facet by the vector p
shiftFacet :: Point -> Facet -> Facet
shiftFacet p = Facet . map (\l -> l { point = addPoints p (point l) }) . sides

-- determine where a facet intersects a plane at a given z value
facetIntersects :: ℝ -> Facet -> [Point]
facetIntersects v f = trimIntersections $ catMaybes intersections
  where intersections = map (`pointAtZValue` v) (sides f)

-- Get rid of the case where a facet intersects the plane at one point
trimIntersections :: [Point] -> [Point]
trimIntersections l
  | length l' <= 1 = []
  | otherwise = l'
  where l' = nub l
