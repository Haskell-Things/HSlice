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

import Prelude ((<), ($), (<>), error, show)

import Data.Either (Either (Left, Right))

import Data.Maybe (Maybe(Just, Nothing), fromJust, isJust)

import Slist (len)

import Graphics.Slicer.Math.Definitions (Contour)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton)

-- Divide a contour into groups of motorcycle cells, based on the motorcycle tree...
import Graphics.Slicer.Math.Skeleton.MotorcycleCells (findClusters, mergeClusterPair, simplifyCluster)

import Graphics.Slicer.Math.Skeleton.Motorcycles (CrashTree(CrashTree), crashMotorcycles)

-- import Graphics.Slicer.Math.Skeleton.Tscherne (applyTscherne)

----------------------------------------------------------------------------------
------------------- Straight Skeleton Calculation (Entry Point) ------------------
----------------------------------------------------------------------------------

-- | Find the StraightSkeleton of a given contour, with a given set of holes cut out of it.
--   Really, this is a dispatcher, to a series of algorithms for doing the actual work.
-- FIXME: Does not know how to calculate a straight skeleton for :
--        * contours with holes,
--        * contours with more than two motorcycles,
--        * contours with a motorcycle that is not a straight divide,
--        * contours with two motorcycles that are collinear..
findStraightSkeleton :: Contour -> [Contour] -> Maybe StraightSkeleton
findStraightSkeleton contour holes =
  case crashMotorcycles contour holes of
    Nothing -> Nothing
    (Just (CrashTree motorcycles _ _)) -> if len motorcycles < 3
                                          then if isJust clusters
                                               then Just res
                                               else Nothing
                                          else
                                            error "whoops!"
  where
    ------------------------------------
    ----- New Motorcycle Cell Code -----
    ------------------------------------
    -- FIXME: this should probably be a recursive algorithm to start with.
    res = case fromJust clusters of
            -- must have been a single concave object.
            (Right skeleton) -> skeleton
            -- not concave, but all of the concave cells shaare a motorcycle.
            (Left [c]) -> case simplifyCluster c of
                            (Right skeleton) -> skeleton
                            (Left complexCluster) -> error $ "got a complex cluster after simplification: " <> show complexCluster <> "\n"
            -- not concave, and all of the cells do not share the same groupings of motorcycles..
            (Left (c1:c2:[])) -> case mergeClusterPair c1 c2 of
                                   (Right c) -> case simplifyCluster c of
                                                  (Right skeleton) -> skeleton
                                                  (Left complexCluster) -> error $ "got a complex cluster after simplification: " <> show complexCluster <> "\n"
                                   (Left _) -> error $ "failed to merge cluster pair:\n" <> show c1 <> "\n" <> show c2 <> "\n"
            other -> error $ "too many clusters?\n"
                          <> show other <> "\n"
    clusters = findClusters contour $ fromJust $ crashMotorcycles contour holes
