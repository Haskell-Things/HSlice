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

import Prelude (Eq, Num, RealFrac, map, (.), ($), (/=), filter, flip, length, (<=), otherwise)

import Data.List(nub)

import Data.Maybe(fromJust, Maybe(Nothing))

import Graphics.Slicer.Math.Point (Point, addPoints)

import Graphics.Slicer.Math.Line (Line, point, pointAtZValue)

data Facet a = Facet { sides :: [Line a] } deriving Eq


-- Shift a facet by the vector p
shiftFacet :: Num a => Point a -> Facet a -> Facet a
shiftFacet p = Facet . map (\l -> l { point = addPoints p (point l) }) . sides

-- Determine if a facet intersects a plane at a given z value
facetIntersects :: RealFrac a => a -> Facet a -> [Point a]
facetIntersects v f = trimIntersections $ map fromJust $ filter (/= Nothing) intersections
  where intersections = map (flip pointAtZValue v) (sides f)

-- Get rid of the case where a facet intersects the plane at one point
trimIntersections :: Eq a => [Point a] -> [Point a]
trimIntersections l
  | length l' <= 1 = []
  | otherwise = l'
  where l' = nub l
                            
