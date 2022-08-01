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
   This file contains code for examining and retrieving the intersection of two projective lines.
-}

module Graphics.Slicer.Math.Intersections (noIntersection, intersectionOf, intersectionBetween, isCollinear, isAntiCollinear, isParallel, isAntiParallel, outputIntersectsPLine, outputsIntersect) where

import Prelude (Bool, Show, ($), (<), (<>), (==), (||), (&&), Maybe(Just, Nothing), Either(Right, Left), error, mempty, otherwise, realToFrac, show)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.PGA (Arcable(hasArc,errOfOut,outOf), PIntersection(IntersectsIn, PParallel, PAntiParallel, PCollinear, PAntiCollinear), PLine2Err(PLine2Err), ProjectiveLine, ProjectivePoint, distanceBetweenPLinesWithErr, plinesIntersectIn)

-- | check if two lines cannot intersect.
noIntersection :: (ProjectiveLine,PLine2Err) -> (ProjectiveLine,PLine2Err) -> Bool
noIntersection pline1 pline2 = isCollinear pline1 pline2 || isParallel pline1 pline2 || isAntiCollinear pline1 pline2 || isAntiParallel pline1 pline2

-- | check if two lines are really the same line.
isCollinear :: (ProjectiveLine,PLine2Err) -> (ProjectiveLine,PLine2Err) -> Bool
isCollinear pline1 pline2 = plinesIntersectIn pline1 pline2 == PCollinear

-- | check if two lines are really the same line.
isAntiCollinear :: (ProjectiveLine,PLine2Err) -> (ProjectiveLine,PLine2Err) -> Bool
isAntiCollinear pline1 pline2 = plinesIntersectIn pline1 pline2 == PAntiCollinear

-- | check if two lines are parallel.
isParallel :: (ProjectiveLine,PLine2Err) -> (ProjectiveLine,PLine2Err) -> Bool
isParallel pline1 pline2 = plinesIntersectIn pline1 pline2 == PParallel

-- | check if two lines are anti-parallel.
isAntiParallel :: (ProjectiveLine,PLine2Err) -> (ProjectiveLine,PLine2Err) -> Bool
isAntiParallel pline1 pline2 = plinesIntersectIn pline1 pline2 == PAntiParallel

-- | Get the intersection point of two lines we know have an intersection point.
-- FIXME: kill this off, intersectionBetween is better..
intersectionOf :: ProjectiveLine -> ProjectiveLine -> ProjectivePoint
intersectionOf pl1 pl2 = saneIntersection $ plinesIntersectIn (pl1, mempty) (pl2, mempty)
  where
    saneIntersection PAntiCollinear     = error $ "cannot get the intersection of anti-collinear lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PCollinear         = error $ "cannot get the intersection of collinear lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PParallel          = error $ "cannot get the intersection of parallel lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PAntiParallel      = error $ "cannot get the intersection of antiparallel lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection (IntersectsIn p _) = p

-- | Get the intersection point of two lines. if they are collinear, returns a line, and if they are parallel, returns Nothing.
-- FIXME: adding two different types of error.
intersectionBetween :: ProjectiveLine -> ProjectiveLine -> Maybe (Either ProjectiveLine ProjectivePoint)
intersectionBetween pl1 pl2 = saneIntersection $ plinesIntersectIn (pl1, mempty) (pl2, mempty)
  where
    (foundDistance, foundDistanceErr)   = distanceBetweenPLinesWithErr pl1 pl2
    foundErr :: ℝ
    -- FIXME: combines multiple types of error.
    foundErr                            = (\(PLine2Err _ n1 _ _ _, PLine2Err _ n2 _ _ _, a, b) -> realToFrac $ ulpVal $ a <> b <> n1 <> n2) foundDistanceErr
    saneIntersection PCollinear         = Just $ Left pl1
    saneIntersection PAntiCollinear     = Just $ Left pl1
    saneIntersection PParallel          = if foundDistance < foundErr
                                          then Just $ Left pl1
                                          else Nothing
    saneIntersection PAntiParallel      = if foundDistance < foundErr
                                          then Just $ Left pl1
                                          else Nothing
    saneIntersection (IntersectsIn p _) = Just $ Right p

-- | find out where the output of an Arcable intersects a given PLine2. errors if no intersection.
outputIntersectsPLine :: (Arcable a, Show a) => a -> (ProjectiveLine,PLine2Err) -> ProjectivePoint
outputIntersectsPLine n pline2WithErr
  | hasArc n = case res of
                 (IntersectsIn p _) -> p
                 v -> error $ "intersection failure." <> show v <> "\n"
  | otherwise = error $ "Tried to check if the output of node intersects PLine on a node with no output:\n" <> show n <> "\n" <> show pline2WithErr <> "\n"
  where
    res = plinesIntersectIn (outOf n, errOfOut n) pline2WithErr

-- | find out where the output of an Arcable intersects a given PLine2. errors if no intersection.
outputsIntersect :: (Arcable a, Arcable b, Show a, Show b) => a -> b -> ProjectivePoint
outputsIntersect node1 node2
  | hasArc node1 && hasArc node2 = case res of
                                     (IntersectsIn p _) -> p
                                     v -> error $ "intersection failure." <> show v <> "\n"
  | otherwise = error $ "Tried to check if the outputs of two nodes intersect, but a node with no output:\n" <> show node1 <> "\n" <> show node2 <> "\n"
  where
    res = plinesIntersectIn (outOf node1, errOfOut node1) (outOf node2, errOfOut node2)

