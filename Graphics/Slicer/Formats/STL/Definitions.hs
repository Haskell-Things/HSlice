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

-- The top module of our STL handling routines.

module Graphics.Slicer.Formats.STL.Definitions (facetLinesFromSTL) where

import Prelude (String, map, dropWhile, ($), break, filter, not, map, tail, (==), (.), unwords, head, words, read, error, length, (<), otherwise, last, (<$>), fmap)

import Data.Char (isSpace, toLower)

import Graphics.Slicer.Math.Definitions (Point(Point))

import Graphics.Slicer.Math.Facet (Facet(Facet))

import Graphics.Slicer.Math.Line (makeLines)

import Graphics.Slicer.Definitions (ℝ3, ℝ)

import Data.List (take)

----------------------------------------------------------
----------- Functions to deal with STL parsing -----------
----------------------------------------------------------

-- From STL file (as a list of Strings, each String corresponding to one line),
-- produce a list of lists of Lines, where each list of Lines corresponds to a
-- facet in the original STL
facetLinesFromSTL :: [String] -> [Facet]
facetLinesFromSTL = fmap (readFacet . cleanupFacet) . facetsFromSTL

-- Separate lines of STL file into facets
facetsFromSTL :: [String] -> [[String]]
facetsFromSTL [] = []
facetsFromSTL [_] = []
facetsFromSTL l = map (map (dropWhile isSpace)) $ f : facetsFromSTL (tail r)
    where (f, r) = break (\s -> filter (not . isSpace) (map toLower s) == "endfacet") l

-- Read a point when it's given a string of the form "x y z"
readPoint :: String -> Point
readPoint s = do
  let
    xval, yval, zval :: ℝ
    (xval, yval, zval) = readThree $ take 3 $ words s
  Point (xval,yval,zval)
    where
      readThree :: [String] -> ℝ3
      readThree [xv,yv,zv] = (read xv,read yv,read zv)
      readThree _ = error "unexpected value when reading point."

-- Read a list of three coordinates (as strings separated by spaces) and generate a facet.
readFacet :: [String] -> Facet
readFacet f
    | length f < 3 = error "Invalid facet"
    | otherwise = Facet . makeLines $ readPoint <$> f'
    where f' = last f : f -- So that we're cyclic

-- Clean up a list of strings from STL file (corresponding to a facet) into just
-- the vertices
cleanupFacet :: [String] -> [String]
cleanupFacet = map (unwords . tail) . filter ((== "vertex") . head) . map words

