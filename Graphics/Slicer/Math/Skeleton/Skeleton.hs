{- ORMOLU_DISABLE -}
{-
 - Copyright 2021 Julia Longtin
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

-- |  This file contains the entry point for the logic and routines required for building
--    a Straight Skeleton of a contour, with a set of sub-contours cut out of it.
module Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton) where

import Prelude (($), (<>), (<=), (||), (==), error, show)

import Data.Either (Either (Left, Right))

import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Definitions (Contour, makeLineSeg)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.Intersections (intersectionBetweenArcsOf)

import Graphics.Slicer.Math.Lossy (eToPLine2, pToEPoint2)

import Graphics.Slicer.Math.PGA (Pointable(ePointOf), angleBetween2PL, outOf)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton)

-- Divide a contour into groups of motorcycle cells, based on the motorcycle tree...
import Graphics.Slicer.Math.Skeleton.MotorcycleCells (findClusters, simplifyCluster)

import Graphics.Slicer.Math.Skeleton.Motorcycles (CollisionType(HeadOn), CrashTree(CrashTree), crashMotorcycles, lastCrashType)

-- import Graphics.Slicer.Math.Skeleton.Tscherne (applyTscherne)

----------------------------------------------------------------------------------
------------------- Straight Skeleton Calculation (Entry Point) ------------------
----------------------------------------------------------------------------------

-- | Find the StraightSkeleton of a given contour, with a given set of holes cut out of it.
--   Really, this is a dispatcher, to a series of algorithms for doing the actual work.
-- FIXME: Does not know how to calculate a straight skeleton for :
--        * contours with holes,
--        * contours with more than one motorcycle,
--        * contours with a motorcycle that is not a straight divide,
--        * contours with two motorcycles that are collinear,
--        * ...
findStraightSkeleton :: Contour -> [Contour] -> Maybe StraightSkeleton
findStraightSkeleton contour holes =
  case crashMotorcycles contour holes of
    Nothing -> Nothing
    (Just (CrashTree (Slist [] _) _ _)) -> Just motorcycleCellRes
    (Just (CrashTree (Slist [_] _) _ _)) -> Just motorcycleCellRes
    (Just crashTree@(CrashTree (Slist [mcA, mcB] _) _ _)) -> if lastCrashType crashTree == Just HeadOn || intersectionIsBehind mcA || intersectionIsBehind mcB
                                                             then Just motorcycleCellRes
                                                             else Nothing
      where
        intersectionIsBehind m = angleFound <= ulpVal angleErr
          where
            (angleFound, (_,_, angleErr)) = angleBetween2PL (outOf m) (eToPLine2 $ lineSegToIntersection)
            lineSegToIntersection = makeLineSeg (ePointOf m) (pToEPoint2 intersectionPPoint)
            (intersectionPPoint, _) = fromMaybe (error "has arcs, but no intersection?") $ intersectionBetweenArcsOf mcA mcB
    (Just _) -> Nothing
  where
    ------------------------------------
    ----- New Motorcycle Cell Code -----
    ------------------------------------
    motorcycleCellRes = case cluster of
                          (Right skeleton) -> skeleton
                          (Left [_]) -> case simplifiedCluster of
                                          (Right skeleton) -> skeleton
                                          (Left simplerCluster) -> error $ "got a simpler cluster after simplification: " <> show simplerCluster <> "\n"
                          _ -> error "more than one cluster?"
    simplifiedCluster = simplifyCluster $ (\(Left [rawCluster]) -> rawCluster) cluster
    cluster = findClusters contour $ fromJust $ crashMotorcycles contour holes
