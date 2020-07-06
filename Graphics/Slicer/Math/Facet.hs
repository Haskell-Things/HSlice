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

-- for adding Generic and NFData to Facet.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Graphics.Slicer.Math.Facet (Facet(Facet), sides, shiftFacet, facetIntersects, facetFromPoints) where

import Prelude (Eq, (<$>), (.), ($), fmap, error, show, (<>), (==), length, head, (&&))

import Data.List(nub)

import Data.Maybe(catMaybes, Maybe(Just, Nothing), fromMaybe)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Graphics.Slicer.Definitions(ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), Point3(Point3), addPoints, flatten, zOf)

import Graphics.Slicer.Math.Line (Line, point, pointAtZValue, rawLineFromEndpoints)

newtype Facet = Facet { sides :: [(Point3, Point3)] }
  deriving (Eq, Generic, NFData)

-- Shift a facet by the vector p
shiftFacet :: Point3 -> Facet -> Facet
shiftFacet p = Facet . fmap (\(start, stop) -> (addPoints p start, addPoints p stop)) . sides

facetFromPoints :: [Point3] -> Facet
facetFromPoints (p1:p2:p3:[]) = Facet $ (p1,p2):(p2,p3):(p3,p1):[]
facetFromPoints _ = error "tried to make a facet from something other than 3 points."

-- determine where a facet intersects a plane at a given z value
facetIntersects :: ℝ -> Facet -> Maybe (Point2,Point2)
facetIntersects v f = if length matchingEdge == 1
                      then Just $ head matchingEdge
                      else trimIntersections $ nub $ catMaybes intersections
  where
    matchingEdge = catMaybes $ edgeOnPlane <$> sides f
    edgeOnPlane (start,stop) = if zOf start == zOf stop && zOf start == v
                               then Just (flatten start, flatten stop)
                               else Nothing
    intersections = (`pointAtZValue` v) <$> sides f
    -- Get rid of the case where a facet intersects the plane at one point
    trimIntersections :: [Point2] -> Maybe (Point2,Point2)
    trimIntersections []      = Nothing
    trimIntersections [_]     = Nothing
    trimIntersections (p1:p2:[]) = Just (p1,p2)
    -- ignore triangles that are exactly aligned.
    trimIntersections (p1:p2:p3:[]) = Nothing

