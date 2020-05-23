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

-- Allow us to use string literals for ByteStrings
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Slicer.Formats.STL.Definitions (facetLinesFromSTL) where

import Prelude (($), (==), read, error, length, otherwise, (<$>), (<>), show)

import Data.Maybe (Maybe(Just, Nothing), catMaybes)

import Data.ByteString(ByteString)

import Data.ByteString.Char8(lines, words, unpack, breakSubstring, break, null, drop)

import Control.Parallel.Strategies (using, rdeepseq, parBuffer)

import Graphics.Slicer.Math.Definitions (Point(Point))

import Graphics.Slicer.Math.Facet (Facet, facetFromPoints)

import Graphics.Slicer.Math.Line (makeLinesLooped)

import Graphics.Slicer.Definitions (Fastℕ, fromFastℕ)

----------------------------------------------------------------
----------- Functions to deal with ASCII STL parsing -----------
----------------------------------------------------------------

-- produce a list of Facets from the input STL file.
facetLinesFromSTL :: Fastℕ -> ByteString -> [Facet]
facetLinesFromSTL threads stl = [readFacet singleFacet | singleFacet <- facetsFromSTL strippedStl] `using` parBuffer (fromFastℕ threads) rdeepseq
  where
    -- strip the first line header off of the stl file.
    (_ , headStrippedStl) = break (== '\n') stl
    -- and the last line terminator off of the stl file.
    (strippedStl, _) = breakSubstring "endsolid" headStrippedStl

-- Separate STL file into facets
facetsFromSTL :: ByteString -> [ByteString]
facetsFromSTL l = if null l then [] else f : facetsFromSTL (drop 1 remainder)
    where (f, r) = breakSubstring "endfacet" l
          (_ , remainder) = break (=='\n') r

-- Read a point when it's given a string of the form "vertex x y z"
readPoint :: ByteString -> Maybe Point
readPoint s = readVertex $ words s
  where
    readVertex :: [ByteString] -> Maybe Point
    readVertex [vertex,xv,yv,zv]
      | vertex == "vertex" = Just (Point (read $ unpack xv,read $ unpack yv,read $ unpack zv))
      | otherwise = Nothing
    readVertex _ = Nothing

-- Read a list of three coordinates (as strings separated by spaces) and generate a facet.
readFacet :: ByteString -> Facet
readFacet f = do
        let
          foundPoints = catMaybes $ readPoint <$> lines f
        if length foundPoints == 3 then facetFromPoints foundPoints else error $ "wrong number of points found: " <> show (length foundPoints) <> "\n" <> show f <> "\n" <> show foundPoints <> "\n"
