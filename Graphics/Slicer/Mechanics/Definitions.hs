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

-- The purpose of this file is to define mechanical characteristics of a machine.

module Graphics.Slicer.Mechanics.Definitions (Printer(Printer), Extruder(Extruder), Bed(RectBed), GCodeFlavor(GCFlavorMarlin), filamentWidth, nozzleDiameter, getExtruder, getBuildArea, gCodeFlavor) where

import Prelude (Eq)

import Graphics.Slicer.Definitions(ℝ, ℝ2)

import Graphics.Slicer.Concepts.Definitions (BuildArea)

-- The properties of the printer's extruder.
data Extruder =
  Extruder { filamentWidth :: !ℝ, nozzleDiameter :: !ℝ}

-- The shape of the 3d printer's bed.
-- FIXME: replace these with Zones.
newtype Bed =
  RectBed ℝ2 -- Width and Depth of the build plate, with the front left surface being 0,0,0.

-- Characteristics of the printer we are using.
data Printer = Printer
  {
    _printBed    :: !Bed
  , getBuildArea :: !BuildArea
  , getExtruder  :: !Extruder
  , gCodeFlavor :: !GCodeFlavor
  }

-- | The flavor of GCode being generated
data GCodeFlavor = GCFlavorMarlin
  deriving Eq
