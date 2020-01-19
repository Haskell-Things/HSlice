{-
 - Copyright 2016 Noah Halford and Catherine Moresco
 - Copyright 2019 Julia Longtin
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

{- The purpose of this file is to hold the definitions of the data
   structures used when performing slicing related math. -}

-- FIXME: remove x, y, and z from this export list. 
module Graphics.Slicer.Math.Definitions(Point(Point), LayerType(BaseOdd,BaseEven,Middle), Contour) where

import Prelude (Eq, Show, show, ($), zipWith, (++), map, unwords)

import Graphics.Slicer.Definitions (ℝ)

-- A single Point in 3d space.
data Point = Point ℝ ℝ ℝ
  deriving Eq

-- Display a Point in the format expected by G-code
instance Show Point where
  show p = unwords $ zipWith (++) ["X","Y","Z"] (map show [xOf p, yOf p, zOf p])
    where
      xOf, yOf, zOf :: Point ->  ℝ
      xOf (Point x _ _) = x
      yOf (Point _ y _) = y
      zOf (Point _ _ z) = z

data LayerType = BaseOdd | BaseEven | Middle

type Contour = [Point]


      
