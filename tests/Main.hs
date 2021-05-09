{-
 - Copyright 2020 Julia Longtin
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
import Test.Hspec(hspec, describe)


-- the execution test for warnings.
import Math.PGA(contourSpec, lineSpec, linearAlgSpec, geomAlgSpec, pgaSpec, proj2DGeomAlgSpec, facetSpec)

main :: IO ()
main = hspec $ do
  -- run tests against the mixed algebra engine.
  describe "linear algebra calculations" linearAlgSpec
  describe "contour handling" contourSpec
  describe "more contour handling" lineSpec
  -- run tests against the Geometric Algebra engine.
  describe "geometric algebra calculations" geomAlgSpec
  -- run tests against the 2D Projective Geometric Algebra engine.
  describe "2D PGA primitives" proj2DGeomAlgSpec
  describe "2D PGA operations" pgaSpec
  -- run tests of the facet engine.
  describe "Contour facetization algorithms" facetSpec
