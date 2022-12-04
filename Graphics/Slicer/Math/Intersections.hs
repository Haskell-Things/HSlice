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

{-
  Purpose of this file: This file contains code for retrieving and reasoning about the intersection of two projective lines.
-}

module Graphics.Slicer.Math.Intersections (
  intersectionBetween,
  intersectionOf,
  isAntiCollinear,
  isAntiParallel,
  isCollinear,
  isParallel,
  noIntersection
  ) where

import Prelude (Bool, Either(Left, Right), (<>), ($), (<), (||), (==), error, show, realToFrac)

import Data.Maybe (Maybe(Just, Nothing))

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.PGA (CPPoint2, PIntersection(IntersectsIn, PParallel, PAntiParallel, PCollinear, PAntiCollinear), PLine2Err, PPoint2Err, ProjectiveLine2, distance2PL, plinesIntersectIn)

-- | Check if two lines cannot intersect.
noIntersection :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
noIntersection line1 line2 = isCollinear line1 line2 || isParallel line1 line2 || isAntiCollinear line1 line2 || isAntiParallel line1 line2

-- | Check if two lines are really the same line.
isCollinear :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isCollinear line1 line2 = plinesIntersectIn line1 line2 == PCollinear

-- | Check if two lines are really the same line, reversed.
isAntiCollinear :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isAntiCollinear line1 line2 = plinesIntersectIn line1 line2 == PAntiCollinear

-- | Check if two lines are parallel.
isParallel :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isParallel line1 line2 = plinesIntersectIn line1 line2 == PParallel

-- | Check if two lines are anti-parallel.
isAntiParallel :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isAntiParallel line1 line2 = plinesIntersectIn line1 line2 == PAntiParallel

-- | Get the intersection point of two lines we know have an intersection point.
intersectionOf :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (CPPoint2, PPoint2Err)
intersectionOf line1 line2 = saneIntersection $ plinesIntersectIn line1 line2
  where
    saneIntersection PAntiCollinear     = error $ "cannot get the intersection of anti-collinear lines.\nline1: " <> show line1 <> "\nline2: " <> show line2 <> "\n"
    saneIntersection PCollinear         = error $ "cannot get the intersection of collinear lines.\nline1: " <> show line1 <> "\nline2: " <> show line2 <> "\n"
    saneIntersection PParallel          = error $ "cannot get the intersection of parallel lines.\nline1: " <> show line1 <> "\nline2: " <> show line2 <> "\n"
    saneIntersection PAntiParallel      = error $ "cannot get the intersection of antiparallel lines.\nline1: " <> show line1 <> "\nline2: " <> show line2 <> "\n"
    saneIntersection (IntersectsIn p (_,_, pErr)) = (p, pErr)

-- | Get the intersection point of two lines.
intersectionBetween :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Maybe (Either (a, PLine2Err) (CPPoint2, PPoint2Err))
intersectionBetween line1@(l1, _) line2@(l2, _) = saneIntersection $ plinesIntersectIn line1 line2
  where
    (foundDistance, (_,_, foundErr)) = distance2PL l1 l2
    saneIntersection PAntiCollinear     = Just $ Left line1
    saneIntersection PCollinear         = Just $ Left line1
    saneIntersection PParallel          = if foundDistance < realToFrac (ulpVal foundErr)
                                          then Just $ Left line1
                                          else Nothing
    saneIntersection PAntiParallel      = if foundDistance < realToFrac (ulpVal foundErr)
                                          then Just $ Left line1
                                          else Nothing
    saneIntersection (IntersectsIn p (_,_, pErr)) = Just $ Right (p, pErr)

