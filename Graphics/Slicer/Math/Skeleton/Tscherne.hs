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
module Graphics.Slicer.Math.Skeleton.Tscherne (tscherneMerge) where

import Prelude (Bool(True), ($), (&&), (<>), (||), (==), (<$>), all, elem, error, otherwise, show)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Skeleton.Definitions (CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), Motorcycle, MotorcycleCell(MotorcycleCell), StraightSkeleton)

-- | Implement the algorithm from christopher Tscherne's masters thesis.
tscherneMerge :: MotorcycleCell -> MotorcycleCell -> CellDivide -> StraightSkeleton
tscherneMerge cell1 cell2 cellDivide
  | cell1 `cellOnDivide` cellDivide && cell2 `cellOnDivide` cellDivide = error "incomplete."
  | otherwise = error $ "tried to merge two cells with a divide that one of them does not have."
                     <> "Cell1: " <> show cell1 <> "\n"
                     <> "Cell2: " <> show cell2 <> "\n"
                     <> "CellDivide: " <> show cellDivide <> "\n"
  where
    cellOnDivide :: MotorcycleCell -> CellDivide -> Bool
    cellOnDivide (MotorcycleCell _ _ motorcycles _) divide = all (== True) $ (divide `divideHasMotorcycle`) <$> motorcycles
      where
        divideHasMotorcycle :: CellDivide -> Motorcycle -> Bool
        divideHasMotorcycle (CellDivide (DividingMotorcycles m (Slist mx _)) _) motorcycle = motorcycle == m || motorcycle `elem` mx
