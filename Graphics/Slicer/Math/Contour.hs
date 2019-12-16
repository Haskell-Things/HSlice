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

import Prelude (Eq, RealFrac, Floating, (==), otherwise, (++), (||), (.), ($))

import Data.List(find, delete, tail, last, head)

import Data.Maybe(Maybe(Nothing), fromJust)

import Graphics.Slicer.Math.Point (Point)

import Graphics.Slicer.Math.Line (Line, combineLines, canCombineLines)

-- A contour is a closed loop of lines on a layer.

-- Extract a single contour from a list of points
findContour :: (Eq a) => ([Point a], [[Point a]]) -> ([Point a], [[Point a]])
findContour (contour, pairs)
  | p == Nothing = (contour, pairs)
  | otherwise = findContour (contour ++ (delete (last contour) p'), delete p' pairs)
  where match p0 = head p0 == last contour || last p0 == last contour
        p = find match pairs
        p' = fromJust p

-- From a list of contours we have already found and a list of pairs of points
-- (each corresponding to a segment), get all contours described by the points
makeContours :: (Eq a) => ([[Point a]], [[Point a]]) -> [[Point a]]
makeContours (contours, pairs)
  | pairs == [] = contours
  | otherwise = makeContours (contours ++ [next], ps)
  where (next, ps) = findContour (head pairs, tail pairs)
                  

-- Turn pairs of points into lists of connected points
getContours :: (Eq a) => [[Point a]] -> [[Point a]]
getContours = makeContours . (,) []

-- Attempt to combine lines on a contour..
simplifyContour :: (RealFrac a, Floating a) => [Line a] -> [Line a]
simplifyContour [] = []
simplifyContour [a] = [a]
simplifyContour (a:b:cs)
  | canCombineLines a b = simplifyContour $ (combineLines a b) : cs
  | otherwise = a : simplifyContour (b : cs)
        
