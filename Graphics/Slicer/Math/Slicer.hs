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

-- A general dumping ground, as we rip things out of one file, and convert to a library.

module Graphics.Slicer.Math.Slicer (accumulateValues) where

import Prelude ((+))

import Graphics.Slicer.Definitions (ℝ)

-- Take absolute values, turn into a vector of rising values
accumulateValues :: [ℝ] -> [ℝ]
accumulateValues [] = []
accumulateValues [a] = [a]
accumulateValues (a:b:cs) = a : accumulateValues (a + b : cs)
