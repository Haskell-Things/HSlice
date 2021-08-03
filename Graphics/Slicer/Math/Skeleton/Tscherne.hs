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

{- Purpose of this file: to hold the logic and routines required for building
   a Straight Skeleton of a contour, with a set of sub-contours cut out of it.
   This will contain logic for handling all contours with no holes, using
   the algorithm in Christopher Tscherne's masters thesis.
-}

-- | Christopher Tscherne\'s algorithm from his master\'s thesis.
module Graphics.Slicer.Math.Skeleton.Tscherne (applyTscherne, cellAfter, cellBefore) where

import Prelude (($), error, (<>), show)

import Data.Maybe( Maybe(Just,Nothing))

import Graphics.Slicer.Math.Skeleton.Cells (cellBefore, cellAfter, nodeTreesDoNotOverlap, addMirrorNodeTrees)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton, CellDivide)

import Graphics.Slicer.Math.Definitions (Contour)

-- | Implement the algorithm from christopher Tscherne's masters thesis.
applyTscherne :: Contour -> [CellDivide] -> Maybe StraightSkeleton
applyTscherne contour cellDivisions =
  case cellDivisions of
    [] -> error "you do not need to applyTscherne if there is only one cell."
    [oneDivision] -> error "you do not need to applyTscherne if there is only one division.. right?."
    (_:_) -> Nothing
  where
    errorIncomplete = error $ "failing to apply Tscherne's method.\n" <>
                      show contour  <> "\n" <>
                      show cellDivisions <> "\n"

