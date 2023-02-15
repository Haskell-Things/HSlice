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

import Prelude (($), (<>), (<$>), error, null, not, show, otherwise)

import Data.Either (lefts, rights)

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

import Slist (slist)

import Graphics.Slicer.Math.Definitions (Contour)

--import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

import Graphics.Slicer.Math.Skeleton.Cells (addNodeTreesAlongDivide, getNodeTreeOfCell, findFirstCellOfContour, findNextCell, findDivisions)

import Graphics.Slicer.Math.Skeleton.Definitions (Cell, RemainingContour, StraightSkeleton(StraightSkeleton), NodeTree, CellDivide)

import Graphics.Slicer.Math.Skeleton.Motorcycles (crashMotorcycles)

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
  let
    foundCrashTree = crashMotorcycles contour holes
  in
    case foundCrashTree of
      Nothing -> Nothing
      (Just crashTree) -> if not $ null holes
                          then Nothing
                          else Just $ StraightSkeleton (slist [[sumNodeTrees $ intersperseDivides allNodeTrees divisions]]) (slist [])
        where
          -- Pair up divides with the nodetree of the cell that has the corresponding opening.
          intersperseDivides :: [NodeTree] -> [CellDivide] -> [(NodeTree, Maybe CellDivide)]
          intersperseDivides nodeTrees divides = case (nodeTrees, divides) of
                                                   ([], _) -> error "impossible"
                                                   ([a], []) -> [(a,Nothing)]
                                                   (a:as, b:bs) -> [(a, Just b)] <> intersperseDivides as bs
                                                   (_:_, _) -> error "what?"
          -- recursively merge our nodeTrees until we have just one.
          sumNodeTrees :: [(NodeTree, Maybe CellDivide)] -> NodeTree
          sumNodeTrees ins
            | null $ lefts rawNodeTrees = case ins of
                                            [] -> error "empty nodeTree"
                                            [(a, Nothing)] -> a
                                            ((_, Nothing):_) -> error "impossible?"
                                            [(_, Just _)] -> error $ show rawNodeTrees <> "\n" <> show divisions <> "\n"
                                            [(a, Just div1) ,(b, Nothing)] -> addNodeTreesAlongDivide a b div1
                                            ((a, Just div1):(b, div2):xs) -> addNodeTreesAlongDivide a (sumNodeTrees ((b, div2):xs)) div1
            | otherwise = error $ "inode crossed divide:\n" <> show rawNodeTrees <> "\n" <> show divisions <> "\n"
          allNodeTrees = rights rawNodeTrees
          rawNodeTrees = getNodeTreeOfCell <$> allCellsOfContour
          -- recursively find a list of all of the cells in a given contour.
          allCellsOfContour = remainingCells (firstCell, maybeFirstRemainder)
            where
              (Just (firstCell,maybeFirstRemainder)) = findFirstCellOfContour contour divisions
              remainingCells :: (Cell, Maybe [RemainingContour]) -> [Cell]
              remainingCells (priorCell, priorRemainder) =
                case priorRemainder of
                  Nothing -> [priorCell]
                  (Just []) -> error "impossible?"
                  (Just [oneRemainder]) -> [priorCell] <> remainingCells (fromMaybe (error "could not find next cell?") $ findNextCell oneRemainder)
                  (Just (_:_)) -> error "incomplete!"
          divisions = findDivisions contour crashTree
