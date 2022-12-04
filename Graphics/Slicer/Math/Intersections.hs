{- ORMOLU_DISABLE -}
{-
 - Copyright 2022 Julia Longtin
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

{- Purpose of this file: to hold the logic and routines responsible for checking for intersections with contours, or portions of contours. -}

module Graphics.Slicer.Math.Intersections (
  intersectionBetween,
  intersectionOf,
  isAntiCollinear,
  isAntiParallel,
  isCollinear,
  isParallel,
  noIntersection
  ) where

import Prelude (Bool, Either(Left, Right), (<>), ($), (<), (||), (==), error, show, mempty, realToFrac) 

import Data.Maybe (Maybe(Just, Nothing))

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.PGA (CPPoint2, PIntersection(IntersectsIn, PParallel, PAntiParallel, PCollinear, PAntiCollinear), PLine2, PLine2Err, PPoint2Err, ProjectiveLine2, distance2PL, plinesIntersectIn)

-- | check if two lines cannot intersect.
noIntersection :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
noIntersection pline1@(pl1,_) pline2@(pl2,_) = isCollinear pline1 pline2 || isParallel pline1 pline2 || isAntiCollinear pline1 pline2 || isAntiParallel pl1 pl2

-- | check if two lines are really the same line.
isCollinear :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isCollinear pline1 pline2 = plinesIntersectIn pline1 pline2 == PCollinear

-- | check if two lines are really the same line.
isAntiCollinear :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isAntiCollinear pline1 pline2 = plinesIntersectIn pline1 pline2 == PAntiCollinear

-- | check if two lines are parallel.
isParallel :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isParallel pline1 pline2 = plinesIntersectIn pline1 pline2 == PParallel

-- | check if two lines are anti-parallel.
isAntiParallel :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
isAntiParallel pline1 pline2 = plinesIntersectIn (pline1, mempty) (pline2, mempty) == PAntiParallel

-- | Get the intersection point of two lines we know have an intersection point.
intersectionOf :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (CPPoint2, PPoint2Err)
intersectionOf pl1 pl2 = saneIntersection $ plinesIntersectIn pl1 pl2
  where
    saneIntersection PAntiCollinear     = error $ "cannot get the intersection of anti-collinear lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PCollinear         = error $ "cannot get the intersection of collinear lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PParallel          = error $ "cannot get the intersection of parallel lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PAntiParallel      = error $ "cannot get the intersection of antiparallel lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection (IntersectsIn p (_,_, pErr)) = (p,pErr)

-- | Get the intersection point of two lines.
intersectionBetween :: PLine2 -> PLine2 -> Maybe (Either PLine2 (CPPoint2, PPoint2Err))
intersectionBetween pl1 pl2 = saneIntersection $ plinesIntersectIn (pl1, mempty) (pl2, mempty)
  where
    (foundDistance, (_,_, foundErr)) = distance2PL pl1 pl2
    saneIntersection PAntiCollinear     = Just $ Left pl1
    saneIntersection PCollinear         = Just $ Left pl1
    saneIntersection PParallel          = if foundDistance < realToFrac (ulpVal foundErr)
                                          then Just $ Left pl1
                                          else Nothing
    saneIntersection PAntiParallel      = if foundDistance < realToFrac (ulpVal foundErr)
                                          then Just $ Left pl1
                                          else Nothing
    saneIntersection (IntersectsIn p (_,_, pErr)) = Just $ Right (p, pErr)

