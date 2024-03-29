{- ORMOLU_DISABLE -}
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
import Test.Hspec(hspec, describe, parallel)

import GoldenSpec.Spec (goldenSpec)

-- the execution test for warnings.
import Math.PGA(contourSpec, lineSpec, linearAlgSpec, geomAlgSpec, pgaSpec, proj2DGeomAlgSpec, facetSpec)

import Math.Geometry.ConcaveChevronQuad (concaveChevronQuadSpec)

import Math.Geometry.ConvexBisectableQuad (convexBisectableQuadSpec)

import Math.Geometry.ConvexQuad (convexQuadSpec)

import Math.Geometry.ConvexSingleRightQuad (convexSingleRightQuadSpec)

import Math.Geometry.DualRightQuad (dualRightQuadSpec)

import Math.Geometry.Rectangle (rectangleSpec)

import Math.Geometry.Square (squareSpec)

import Math.Geometry.Triangle (triangleSpec)

main :: IO ()
main = hspec $ parallel $ do
  -- run tests against the mixed algebra engine.
  describe "linear algebra calculations" linearAlgSpec
  -- run tests against the Geometric Algebra engine.
  describe "geometric algebra calculations" geomAlgSpec
  -- run tests against the 2D Projective Geometric Algebra engine.
  describe "2D PGA primitives" proj2DGeomAlgSpec
  describe "2D PGA operations" pgaSpec
  describe "Geometry/Triangles" triangleSpec
  describe "Geometry/Squares" squareSpec
  describe "Geometry/Rectangles" rectangleSpec
  describe "Geometry/DualRightQuads" dualRightQuadSpec
  describe "Geometry/ConvexSingleRightQuads" convexSingleRightQuadSpec
  describe "Geometry/ConvexBisectableQuads" convexBisectableQuadSpec
  describe "Geometry/ConvexQuads" convexQuadSpec
  describe "Geometry/ConcaveChevronQuadSpec" concaveChevronQuadSpec
  describe "contour handling" contourSpec
  describe "more contour handling" lineSpec
  -- run tests of the facet engine.
  describe "Contour facetization algorithms" facetSpec
  -- run the golden tests to ensure we haven't broken the serialized forms of our unit tests.
  describe "golden tests" goldenSpec
