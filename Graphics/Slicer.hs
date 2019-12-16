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

{- The purpose of this file is to pass on the functionality we want
   to be accessible to an end user who is compiling objects using
   this haskell library. -}

module Graphics.Slicer (Point(Point), x,y,z, crossProduct, twoDCrossProduct, lineIntersection, Line(Line), point, slope, scalePoint, addPoints, magnitude, distance, lineFromEndpoints, midpoint, endpoint, flipLine, Contour, Facet(Facet), sides, shiftFacet, LayerType(BaseOdd, BaseEven, Middle), pointSlopeLength, combineLines, perpendicularBisector, pointAtZValue, orderPoints, roundToFifth, roundPoint, shortenLineBy, accumulateValues, facetsFromSTL, cleanupFacet, makeLines, facetIntersects, trimIntersections, getContours, simplifyContour,Bed(Rect3), bedWidth, bedDepth, bedHeight, Extruder(Extruder), filamentWidth, nozzleDiameter) where

import Prelude ()

import Graphics.Slicer.Math.Definitions (Point(Point), x, y, z, LayerType(BaseOdd, BaseEven, Middle), Contour)

import Graphics.Slicer.Math.Point (crossProduct, twoDCrossProduct, scalePoint, addPoints, magnitude, distance, orderPoints)

import Graphics.Slicer.Math.Line (Line(Line), point, slope, lineIntersection, lineFromEndpoints, midpoint, endpoint, flipLine, pointSlopeLength, combineLines, perpendicularBisector, pointAtZValue, shortenLineBy, makeLines)

import Graphics.Slicer.Math.Facet (Facet(Facet), sides, shiftFacet, facetIntersects, trimIntersections)

import Graphics.Slicer.Math.Contour (getContours, simplifyContour)

import Graphics.Slicer.Math.Slicer (accumulateValues)

import Graphics.Slicer.Formats.GCode.Definitions (roundToFifth, roundPoint)

import Graphics.Slicer.Formats.STL.Definitions (facetsFromSTL, cleanupFacet)

import Graphics.Slicer.Mechanics.Definitions (Bed(Rect3), bedWidth, bedDepth, bedHeight, Extruder(Extruder), filamentWidth, nozzleDiameter)
