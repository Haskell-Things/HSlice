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

{- The purpose of this file is to pass on the functionality we want
   to be accessible to an end user who is compiling objects using
   this haskell library. -}

module Graphics.Slicer (
  module MD,
  module MP,
  module MC,
  module MS,
  module MeD,
  module D,
  module CD,
  module MaS
  ) where

import Prelude ()

import Graphics.Slicer.Math.Definitions as MD (Point2(Point2),Point3(Point3), Contour, distance, addPoints, scalePoint, roundPoint2)

import Graphics.Slicer.Math.Point as MP (crossProduct, twoDCrossProduct)

import Graphics.Slicer.Math.Contour as MC (getContours, makeContourTreeSet, ContourTree(ContourTree))

import Graphics.Slicer.Math.Slicer as MS (accumulateValues)

import Graphics.Slicer.Mechanics.Definitions as MeD (Extruder(Extruder), Bed(RectBed), filamentWidth, nozzleDiameter)

import Graphics.Slicer.Definitions as D (ℝ, ℝ2, ℕ, Fastℕ, fromFastℕ, toFastℕ);

import Graphics.Slicer.Concepts.Definitions as CD (BuildArea(RectArea, CylinderArea));

import Graphics.Slicer.Machine.StateM as MaS (EPos(EPos), FRate(FRate), StateM, MachineState(MachineState));
