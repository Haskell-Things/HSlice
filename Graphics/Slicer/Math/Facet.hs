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

 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

-- for adding Generic and NFData to Facet.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | The purpose of this file is to hold facet based arithmatic. really for if we need a better 'admesh' while debugging ImplicitCAD.
module Graphics.Slicer.Math.Facet (Facet(Facet), sidesOf, shiftFacet, facetIntersects) where

import Prelude (Eq, ($), error, (==), (&&), Show)

import Data.List.Extra(nubOrd)

import Data.Maybe(mapMaybe, Maybe(Just, Nothing))

import Data.Bifunctor (bimap)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Graphics.Slicer.Definitions(ℝ)

import Graphics.Slicer.Math.Definitions (Point2, Point3, addPoints, flatten, zOf)

import Graphics.Slicer.Math.Line (pointAtZValue)

data Facet = Facet {_sides :: !((Point3, Point3),(Point3, Point3),(Point3, Point3)), _normal :: !Point3}
  deriving (Eq, Generic, NFData, Show)

-- | Shift a facet by the vector p
shiftFacet :: Point3 -> Facet -> Facet
shiftFacet p (Facet (s1,s2,s3) n1) = Facet (bimap (addPoints p) (addPoints p) s1,
                                            bimap (addPoints p) (addPoints p) s2,
                                            bimap (addPoints p) (addPoints p) s3
                                           ) n1

-- | allow us to use mapping functions against the tuple of sides.
sidesOf :: Facet -> [(Point3, Point3)]
sidesOf (Facet (a,b,c) _) = [a,b,c]

-- | determine where a facet intersects a plane at a given z value
facetIntersects :: ℝ -> Facet -> Maybe (Point2, Point2)
facetIntersects v f = case matchingEdge of
                        [] -> Nothing
                        [oneEdge] ->Just oneEdge
                        (_:_) -> trimIntersections $ nubOrd $ mapMaybe (`pointAtZValue` v) $ sidesOf f
  where
    matchingEdge = mapMaybe edgeOnPlane $ sidesOf f
    edgeOnPlane :: (Point3, Point3) -> Maybe (Point2, Point2)
    edgeOnPlane (start,stop) = if zOf start == zOf stop && zOf start == v
                               then Just (flatten start, flatten stop)
                               else Nothing
    -- Get rid of the case where a facet intersects the plane at one point
    trimIntersections :: [Point2] -> Maybe (Point2, Point2)
    trimIntersections []      = Nothing
    trimIntersections [_]     = Nothing
    trimIntersections [p1,p2] = Just (p1,p2)
    -- ignore triangles that are exactly aligned.
    trimIntersections [_,_,_] = Nothing
    trimIntersections _ = error "unpossible!"
