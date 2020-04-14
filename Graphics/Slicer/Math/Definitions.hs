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

-- for adding Generic and NFData to Point.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Graphics.Slicer.Math.Definitions(Point(Point), LayerType(BaseOdd,BaseEven,Middle), Contour(Contour)) where

import Prelude (Eq, (++), Monoid(mempty, mappend), Semigroup((<>)))

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Graphics.Slicer.Definitions (ℝ3)

-- A single Point in 3D space.
newtype Point = Point ℝ3
  deriving (Eq, Generic, NFData)

data LayerType = BaseOdd | BaseEven | Middle

-- a list of points around a shape.
newtype Contour = Contour [Point]
  deriving Eq

instance Semigroup Contour where
  (<>) (Contour a) (Contour b) = Contour (a ++ b)

instance Monoid Contour where
  mempty = Contour []
  mappend = (<>)
