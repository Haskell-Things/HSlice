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

-- The top module of our STL handling routines.

-- Allow us to use string literals for ByteStrings
{-# LANGUAGE OverloadedStrings #-}

{- https://www.utgjiu.ro/rev_mec/mecanica/pdf/2010-01/13_Catalin%20Iancu.pdf -}

module Graphics.Slicer.Formats.STL.Definitions (trianglesFromSTL) where

import Prelude (($), (==), read, error, (<$>), (<>), show)

import Control.Parallel.Strategies (using, rdeepseq, parBuffer)

import Data.Maybe (Maybe(Just, Nothing), catMaybes)

import Data.ByteString(ByteString)

import Data.ByteString.Char8(lines, words, unpack, breakSubstring, break, null, drop)

import Graphics.Slicer.Math.Definitions (Point3(Point3))

import Graphics.Slicer.Math.Tri (Tri(Tri))

import Graphics.Slicer.Definitions (Fastℕ, fromFastℕ)

----------------------------------------------------------------
----------- Functions to deal with ASCII STL reading -----------
----------------------------------------------------------------

-- FIXME: ensure we account for https://github.com/openscad/openscad/issues/2651
-- FIXME: handle upper case files.

-- FIXME: support binary STL reading / writing.
{- from the OpenSCAD folks:
15:49 < InPhase> juri_: The binary standard I followed came from this official standards documenting body:  https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL  ;)
15:52 < InPhase> juri_: For example, I followed the "assumed to be little-endian, although this is not stated in documentation", enforcing that with a conversion in the event OpenSCAD is used on a big-endian system.
15:53 < InPhase> Such a constraint seems essential or else it cannot function as a document exchange format.
-}

-- produce a list of Tris from the input STL file.
trianglesFromSTL :: Fastℕ -> ByteString -> [Tri]
trianglesFromSTL threads stl = [readTri f | f <- rawTrisFromSTL strippedStl] `using` parBuffer (fromFastℕ threads) rdeepseq
  where
    -- strip the first line header off of the stl file.
    (_, headStrippedStl) = break (== '\n') stl
    -- and the last line terminator off of the stl file.
    (strippedStl, _) = breakSubstring "endsolid" headStrippedStl

-- Separate STL file into triangles
rawTrisFromSTL :: ByteString -> [ByteString]
rawTrisFromSTL l = if null l then [] else f : rawTrisFromSTL (drop 1 remainder)
    where (f, r) = breakSubstring "endfacet" l
          (_ , remainder) = break (=='\n') r

-- | Read a point when it's given as a string of the form "vertex x y z".
--   Skip any other line.
readVertex :: ByteString -> Maybe Point3
readVertex s = readVertex' $ words s
  where
    readVertex' :: [ByteString] -> Maybe Point3
    readVertex' [vertex,xv,yv,zv]
      | vertex == "vertex" = Just $ Point3 (read $ unpack xv,read $ unpack yv,read $ unpack zv)
    readVertex' _ = Nothing

-- Read a list of three vertexes and generate a triangle from them.
readTri :: ByteString -> Tri
readTri f = do
        let
          points = readVertex <$> lines f
          foundPoints = catMaybes points
          triFromPoints :: [Point3] -> Tri
          triFromPoints [p1,p2,p3] = Tri ((p1,p2),(p2,p3),(p3,p1))
          triFromPoints _ = error "tried to make a tri from something other than 3 points."
        case foundPoints of
          threePoints@[_,_,_] -> triFromPoints threePoints
          _ -> error $ "wrong number of points found." <> show f <> "\n" <> show foundPoints <> "\n"

