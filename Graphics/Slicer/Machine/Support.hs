-----------------------------------------------------------------------
----------------------------- SUPPORT ---------------------------------
-----------------------------------------------------------------------

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
 -
 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{- The purpose of this file is to contain our support logic, until when we have to revive it. -}

module Graphics.Slicer.Machine.Support (makeSupport) where

import Prelude (fmap, (||), Bool, (-), (+), ($), (<$>), maximum, minimum, error, (==), (/), (*), sqrt)

import Data.List (concat)

import Data.Maybe (Maybe(Just, Nothing), fromMaybe, mapMaybe, catMaybes)

import Graphics.Slicer.Definitions (ℝ,ℝ2)

import Graphics.Slicer.Math.Definitions (Contour(PointSequence), Point2(Point2), xOf, yOf, addPoints, scalePoint)

import Graphics.Slicer.Machine.Infill (infillLineSegInside, coveringLineSegsVertical)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg))

-- | shorten a line segment by a given amount in millimeters on each end
shortenLineBy :: ℝ -> LineSeg -> LineSeg
shortenLineBy amt (LineSeg p m) = LineSeg newStart newSlope
  where pct = amt / magnitude m
        newStart = addPoints p $ scalePoint pct m
        newSlope = scalePoint (1 - 2 * pct) m
        magnitude (Point2 (x1,y1)) = sqrt (x1 * x1 + y1 * y1)

-- Generate support
-- FIXME: hard coded infill amount.
-- FIXME: should be one string of plastic in most cases.
-- FIXME: support needs a complete rewrite.
makeSupport :: Contour
            -> [Contour]
            -> ℝ
            -> ℝ
            -> [LineSeg]
makeSupport contour childContours lh ls = fmap (shortenLineBy $ 2 * lh)
                                          $ concat $ catMaybes $ infillLineSegInside contour (addBBox childContours)
                                          <$> coveringLineSegsVertical contour ls

-- A bounding box. a box around a contour.
data BBox = BBox ℝ2 ℝ2

-- | Check if a bounding box is empty.
isEmptyBBox :: BBox -> Bool
isEmptyBBox (BBox (x1,y1) (x2,y2)) = x1 == x2 || y1 == y2

-- Get a bounding box of all contours.
boundingBoxAll :: [Contour] -> Maybe BBox
boundingBoxAll contours = if isEmptyBBox box then Nothing else Just box
    where
      box  = BBox (minX, minY) (maxX, maxY)
      minX = minimum $ (\(BBox (x1,_) _) -> x1) <$> bBoxes
      minY = minimum $ (\(BBox (_,y1) _) -> y1) <$> bBoxes
      maxX = maximum $ (\(BBox _ (x2,_)) -> x2) <$> bBoxes
      maxY = maximum $ (\(BBox _ (_,y2)) -> y2) <$> bBoxes
      bBoxes = mapMaybe boundingBox contours

-- Get a bounding box of a contour.
boundingBox :: Contour -> Maybe BBox
boundingBox (PointSequence []) = error "boundingBox given an empty contour?"
boundingBox (PointSequence contourPoints) = if isEmptyBBox box then Nothing else Just box
  where
    box  = BBox (minX, minY) (maxX, maxY)
    minX = minimum $ xOf <$> contourPoints
    minY = minimum $ yOf <$> contourPoints
    maxX = maximum $ xOf <$> contourPoints
    maxY = maximum $ yOf <$> contourPoints

-- add a bounding box to a list of contours, as the first contour in the list.
-- FIXME: what is this for?
addBBox :: [Contour] -> [Contour]
addBBox contours = PointSequence [Point2 (x1,y1), Point2 (x2,y1), Point2 (x2,y2), Point2 (x1,y2), Point2 (x1,y1)] : contours
    where
      bbox = fromMaybe (BBox (1,1) (-1,-1)) $ boundingBoxAll contours
      (BBox (x1, y1) (x2, y2)) = incBBox bbox 1
      -- Put a fixed amount around the 2d bounding box.
      incBBox (BBox (nx1,ny1) (nx2,ny2)) amount = BBox (nx1+amount, ny1+amount) (nx2-amount, ny2-amount)
