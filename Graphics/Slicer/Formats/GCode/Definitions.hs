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
   structures used when generating GCode. -}
module Graphics.Slicer.Formats.GCode.Definitions (roundToFifth, roundPoint) where

import Prelude(($), RealFrac, Rational, round, fromRational, (*))

import Data.Ratio((%))

import Graphics.Slicer.Math.Point (Point(Point))

-- The GCode spec (https://ws680.nist.gov/publication/get_pdf.cfm?pub_id=823374) specifies only 5 digits of precision.

-- round a value
roundToFifth :: (RealFrac a) => a -> Rational
roundToFifth a = (round (100000 * a)) % 100000

-- round a point
roundPoint :: RealFrac a => Point a -> Point a
roundPoint (Point x1 y1 z1) = Point (fromRational $ roundToFifth x1) (fromRational $ roundToFifth y1) (fromRational $ roundToFifth z1)
