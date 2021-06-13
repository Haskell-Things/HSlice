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

-- For our implementation of i2d.
{-# LANGUAGE MagicHash #-}

module Graphics.Slicer.Formats.STL.Facets (facetsFromSTL, buildAsciiSTL) where

import Prelude (($), (==), error, otherwise, (<$>), (<>), show, isNaN, isInfinite, (<), (||), isNegativeZero, (-), mconcat, concat)

import Control.Parallel.Strategies (using, rdeepseq, parBuffer)

import Data.ByteString(ByteString)

import Data.ByteString.Builder(Builder, stringUtf8, charUtf8, intDec, byteString)

import Data.ByteString.Char8(lines, words, unpack, breakSubstring, break, null, drop)

import Data.Either (Either(Left, Right), rights, lefts)

import Data.Maybe (Maybe(Just, Nothing), catMaybes)

import Numeric (floatToDigits)

import Text.Read (readMaybe)

import GHC.Base (Int(I#), Char(C#), chr#, ord#, (+#))

import Graphics.Slicer.Math.Definitions (Point3(Point3))

import Graphics.Slicer.Math.Facet (Facet(Facet))

import Graphics.Slicer.Definitions (ℝ, Fastℕ, fromFastℕ, fromℝtoFloat)

----------------------------------------------------------------
----------- Functions to deal with ASCII STL reading -----------
----------------------------------------------------------------

-- FIXME: ensure we account for https://github.com/openscad/openscad/issues/2651
-- FIXME: handle upper case files.

-- produce a list of Facets from the input STL file.
facetsFromSTL :: Fastℕ -> ByteString -> (ByteString, [Facet], ByteString)
facetsFromSTL threads stl = (header, [readFacet f | f <- rawFacetsFromSTL strippedStl] `using` parBuffer (fromFastℕ threads) rdeepseq, footer)
  where
    -- strip the first line header off of the stl file.
    (header, headStrippedStl) = break (== '\n') stl
    -- and the last line terminator off of the stl file.
    (strippedStl, footer) = breakSubstring "endsolid" headStrippedStl

-- Separate STL file into facets
rawFacetsFromSTL :: ByteString -> [ByteString]
rawFacetsFromSTL l = if null l then [] else f : rawFacetsFromSTL (drop 1 remainder)
    where (f, r) = breakSubstring "endfacet" l
          (_ , remainder) = break (=='\n') r

      -- Read a list of three coordinates (as strings separated by spaces) and generate a facet.
readFacet :: ByteString -> Facet
readFacet f = do
        let
          pointsAndNormals = readVertexOrNormal <$> lines f
          foundPoints =  lefts $ catMaybes pointsAndNormals
          foundNormals = rights $ catMaybes pointsAndNormals
          facetFromPointsAndNormal :: [Point3] -> Point3 -> Facet
          facetFromPointsAndNormal [p1,p2,p3] n = Facet ((p1,p2),(p2,p3),(p3,p1)) n
          facetFromPointsAndNormal _ _ = error "tried to make a facet from something other than 3 points."
        case foundPoints of
          [] -> error "no points found."
          [_] -> error "only one point found."
          [_,_] -> error "only two points found."
          [_,_,_] -> case foundNormals of
                       [] -> error "found no normal."
                       [foundNormal] -> facetFromPointsAndNormal foundPoints foundNormal
                       (_:_) -> error "too many normals."
          (_:_) -> error "found too many points."

-- | Read a point when it's given as a string of the form "vertex x y z"
--   or a normal when it's given as a string of the form "facet normal x y z".
--   Skip "outer loop" and "endloop".
readVertexOrNormal :: ByteString -> Maybe (Either Point3 Point3)
readVertexOrNormal s = readVertexOrNormal' $ words s
  where
    readVertexOrNormal' :: [ByteString] -> Maybe (Either Point3 Point3)
    readVertexOrNormal' inLine = case inLine of
                                   [] -> Nothing
                                   ["endloop"] -> Nothing
                                   ["outer", "loop"] -> Nothing
                                   ["facet", "normal", xs, ys, zs] -> case (readMaybe $ unpack xs, readMaybe $ unpack ys, readMaybe $ unpack zs) of
                                                                        (Just xv, Just yv, Just zv) -> Just $ Right $ Point3 (xv,yv,zv)
                                                                        (_maybex, _maybey, _maybez) -> error "could not read normal point."
                                   ["vertex" ,xs,ys,zs] -> case (readMaybe $ unpack xs,readMaybe $ unpack ys,readMaybe $ unpack zs) of
                                                             (Just xv, Just yv, Just zv) -> Just $ Left $ Point3 (xv,yv,zv)
                                                             (_maybex, _maybey, _maybez) -> error "error reading vertex point."
                                   (x:xs) -> error $ "unexpected input in STL file: " <> show x <> " " <> concat (show <$> xs) <> "\n"

----------------------------------------------------------------
----------- Functions to deal with ASCII STL writing -----------
----------------------------------------------------------------

-- | Generate an ASCII STL, as a Builder.
buildAsciiSTL :: (ByteString, [Facet], ByteString) -> Builder
buildAsciiSTL (header, facets, footer) = byteString header <> "\n" <> mconcat (writeFacet <$> facets) <> byteString footer

-- | Generate a single facet.
writeFacet :: Facet -> Builder
writeFacet (Facet ((p1,_),(p2,_),(p3,_)) n1) = stringUtf8 "facet " <> writeNormal n1 <> "outer loop\n" <> writeVertex p1 <> writeVertex p2 <> writeVertex p3 <> "endloop\nendfacet\n"

-- | Generate the normal for a facet. Note that the caller is assumed to have already placed "facet " on the line of text being generated.
writeNormal :: Point3 -> Builder
writeNormal (Point3 (x,y,z)) = "normal " <> formatFloat x <> " " <> formatFloat y <> " " <> formatFloat z <> "\n"

-- | Generate a vertex of a facet.
writeVertex :: Point3 -> Builder
writeVertex (Point3 (x,y,z)) = "vertex " <> formatFloat x <> " " <> formatFloat y <> " " <> formatFloat z <> "\n"

-- | Generate a formatted float for placement in an STL file.
-- Inspired from code in cassava, and the scientific library.
formatFloat :: ℝ -> Builder
formatFloat rawVal
   | isNaN val                     = error $ "NaN" <> "\n"
   | isInfinite val                = error $ if val < 0 then "-Infinity" else "Infinity" <> "\n"
   | val < 0 || isNegativeZero val = charUtf8 '-' <> format (floatToDigits 10 (-val))
   | otherwise                     = format (floatToDigits 10 val)
  where
    val = fromℝtoFloat rawVal
    format (digits, exponent) =
      let
        digitString = i2d <$> digits
        eVal = intDec (exponent-1)
        i2d (I# i#) = C# (chr# (ord# '0'# +# i# ))
      in
        case digitString of
          ['0']        -> stringUtf8 "0.0e0"
          [whole]      -> charUtf8 whole <> stringUtf8 ".0e" <> eVal
          (whole:frac) -> charUtf8 whole <> charUtf8 '.' <> mconcat (charUtf8 <$> frac) <> charUtf8 'e' <> eVal
          []           -> error "empty digit string?"

