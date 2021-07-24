{- ORMOLU_DISABLE -}
{-
 - Copyright 2020 Julia Longtin
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

{- part of an attempt to implement admesh capabilities. woefully incomplete. -}

module Graphics.Slicer.Math.CheckFacets (FacetWithNeighbors(FacetWithNeighbors), findNeighbors) where

import Prelude (Eq, (==), (/=), ($), filter, error, show, (||), (<>))

import Graphics.Slicer.Math.Definitions (Point3)

import Graphics.Slicer.Math.Facet (Facet(Facet))

data FacetWithNeighbors = FacetWithNeighbors { _self :: !Facet, _neighbor1 :: !Facet, _neighbor2 :: !Facet, _neighbor3 :: !Facet}
  deriving (Eq)

findNeighbors :: Facet -> [Facet] -> FacetWithNeighbors
findNeighbors facet@(Facet (s1,s2,s3) _) facets = FacetWithNeighbors facet (findOtherFacetWithSide s1 facet facets) (findOtherFacetWithSide s2 facet facets) (findOtherFacetWithSide s3 facet facets)
  where
    findOtherFacetWithSide :: (Point3,Point3) -> Facet -> [Facet] -> Facet
    findOtherFacetWithSide side target fs = case res of
                                              [] -> error "no res? impossible?"
                                              [a] -> error $ "only found self when looking for facet matching: " <> show a <> "\n"
                                              [_a,_b] -> case filter (/= target) res of
                                                         [] -> error "impossible"
                                                         [a] -> a
                                                         (a:bs) -> error $ "too many members: " <> show a <> "\n" <> show bs <> "\n"
                                              (a:bs) -> error $ "more edges found than possible: " <> show a <> "\n" <> show bs <> "\n"
      where
        res = filter (\(Facet (d1,d2,d3) _) -> d1 == side || d2 == side || d3 == side) fs
