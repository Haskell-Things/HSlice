{- ORMOLU_DISABLE -}
{-
 - Copyright 2022 Julia Longtin
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

-- The purpose of this file is to hold the entry point for our tests.
-- Shamelessly stolen from ImplicitCAD.

-- be explicit about what we import.
import Prelude (($), IO)

-- our testing engine.
import Test.Hspec(hspec, parallel, describe)

-- the execution test for properties we want to examine statistically.
import Math.Geometry.Triangle (triangleStatSpec)

main :: IO ()
main = hspec $ parallel $ do
  -- run tests of the facet engine.
  describe "Statistics" triangleStatSpec
