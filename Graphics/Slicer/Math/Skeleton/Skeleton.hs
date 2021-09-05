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

import Prelude (($), (<>), (<$>), error, null, not, show, head)

import Data.Either (Either(Left, Right), lefts, rights)

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

import Slist (slist)

import Graphics.Slicer.Math.Definitions (Contour)

import Graphics.Slicer.Math.Skeleton.Cells (addNodeTreesOnDivide, getNodeTreeOfCell, findFirstCellOfContour, findNextCell, findDivisions)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton))

import Graphics.Slicer.Math.Skeleton.Motorcycles (crashMotorcycles)

import Graphics.Slicer.Math.Skeleton.Tscherne (applyTscherne)

----------------------------------------------------------------------------------
------------------- Straight Skeleton Calculation (Entry Point) ------------------
----------------------------------------------------------------------------------

-- | Find the StraightSkeleton of a given contour, with a given set of holes cut out of it.
--   Really, this is a dispatcher, to a series of algorithms for doing the actual work.
-- FIXME: Does not know how to calculate a straight skeleton for contours with holes, or more than one motorcycle.. or two motorcycles that are collinear.
-- FIXME: abusing Maybe until we can cover all cases.
findStraightSkeleton :: Contour -> [Contour] -> Maybe StraightSkeleton
findStraightSkeleton contour holes =
  let
    foundCrashTree = crashMotorcycles contour holes
  in
    case foundCrashTree of
      Nothing -> Nothing
      (Just crashTree) -> if not $ null holes
                          then Nothing
                          else case divisions of
                                 -- Simple case. convert the whole contour to a cell, and use the simple solver on it.
                                 [] -> Just $ StraightSkeleton [[res]] (slist [])
                                   where
                                     res = case getNodeTreeOfCell singleCell of
                                             (Right nodetree) -> nodetree
                                             (Left _) -> error "unpossible."
                                     (singleCell,_,_) = fromMaybe (error "this should never fail?") $ findFirstCellOfContour contour []
                                 [division] -> if null (lefts $ getNodeTreeOfCell <$> cells)
                                               then Just $ addNodeTreesOnDivide firstNodeTree secondNodeTree division
                                               else applyTscherne contour divisions
                                   where
                                     [firstNodeTree, secondNodeTree] = rights $ getNodeTreeOfCell <$> cells
                                     cells = [firstCell, secondCell]
                                     (Just (secondCell,_,_)) = findNextCell (head remainder)
                                     remainder = fromMaybe (error $ "no remainder?\n" <> show firstCell <> "\n") maybeRemainder
                                     (Just (firstCell,_,maybeRemainder)) = findFirstCellOfContour contour [division]
                                 (_:_) -> Nothing
        where
          divisions = findDivisions contour crashTree

