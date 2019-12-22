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

{- The purpose of this file is to hold information about contoured surfaces. -}

module Graphics.Slicer.Math.Contour (getContours, simplifyContour) where

import Prelude (Eq, RealFrac, Floating, (==), otherwise, (++), (||), (.), ($), null)

import Data.List(find, delete, tail, last, head)

import Data.Maybe(Maybe(Nothing), fromJust, isNothing)

import Graphics.Slicer.Math.Point (Point)

import Graphics.Slicer.Math.Line (Line, combineLines, canCombineLines)

-- A contour is a closed loop of lines on a layer.

-- Extract a single contour from a list of points
findContour :: ([Point], [[Point]]) -> ([Point], [[Point]])
findContour (contour, pairs)
  | isNothing p = (contour, pairs)
  | otherwise = findContour (contour ++ delete (last contour) p', delete p' pairs)
  where match p0 = head p0 == last contour || last p0 == last contour
        p = find match pairs
        p' = fromJust p

-- From a list of contours we have already found and a list of pairs of points
-- (each corresponding to a segment), get all contours described by the points
makeContours :: ([[Point]], [[Point]]) -> [[Point]]
makeContours (contours, pairs)
  | null pairs = contours
  | otherwise = makeContours (contours ++ [next], ps)
  where (next, ps) = findContour (head pairs, tail pairs)
                  

-- Turn pairs of points into lists of connected points
getContours :: [[Point]] -> [[Point]]
getContours = makeContours . (,) []

-- Attempt to combine lines on a contour..
simplifyContour :: [Line] -> [Line]
simplifyContour [] = []
simplifyContour [a] = [a]
simplifyContour (a:b:cs)
  | canCombineLines a b = simplifyContour $ combineLines a b : cs
  | otherwise = a : simplifyContour (b : cs)
        
