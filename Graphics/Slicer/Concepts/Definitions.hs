{- ORMOLU_DISABLE -}
-- Slicer.
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

-- The purpose of this file is to define important non-math concepts of a machine.

module Graphics.Slicer.Concepts.Definitions (BuildArea(RectArea, CylinderArea)) where

import Prelude (Eq)

import Graphics.Slicer.Definitions(ℝ2, ℝ3)

import Graphics.Slicer.Math.Definitions(Contour)

-- The shape of the 3d printer's build area.
data BuildArea =
    RectArea ℝ3 -- Width, Depth, and Height of the build area.
  | CylinderArea ℝ2 -- Radius and Height of the build area.
  | ContourArea Contour -- Something strange.
  deriving Eq

