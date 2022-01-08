{- ORMOLU_DISABLE -}
{-
 - Copyright 2020 Julia Longtin, Sandy McGuire
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
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-type-defaults        #-}

module GoldenSpec.Spec (spec) where

import Prelude (($), error)

import Data.Maybe(fromMaybe)

import GoldenSpec.Util (golden)

import Test.Hspec (describe, Spec)

import Graphics.Slicer.Math.Ganja(toGanja)

import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Contour library.
import Graphics.Slicer.Math.Contour (contourContainsContour, getContours, pointsOfContour, numPointsOfContour, justOneContourFrom, lineSegsOfContour, makeLineSegContour, makePointContour)

-- A euclidian point.
import Graphics.Slicer.Math.Definitions(Point2(Point2), Contour(LineSegContour), LineSeg(LineSeg), roundPoint2, startPoint, distance)

import Graphics.Slicer.Math.Skeleton.Face (Face(Face), facesOf, orderedFacesOf)

spec :: Spec
spec = describe "golden tests" $ do
  golden "C0-Straight_Skeleton" $ fromMaybe (error "no skeleton?") (findStraightSkeleton c0 [])
  golden "C1-Straight_Skeleton" $ fromMaybe (error "no skeleton?") (findStraightSkeleton c1 [])
  golden "C2-Straight_Skeleton" $ fromMaybe (error "no skeleton?") (findStraightSkeleton c2 [])
  golden "C3-Straight_Skeleton" $ fromMaybe (error "no skeleton?") (findStraightSkeleton c3 [])
  golden "C4-Straight_Skeleton" $ fromMaybe (error "no skeleton?") (findStraightSkeleton c4 [])
  golden "C5-Straight_Skeleton" $ fromMaybe (error "no skeleton?") (findStraightSkeleton c5 [])
  golden "C6-Straight_Skeleton" $ fromMaybe (error "no skeleton?") (findStraightSkeleton c6 [])
  --golden "C0-Faces-Default" $ facesOf (fromMaybe (error "got Nothing") $ findStraightSkeleton c0 [])
  --golden "C0-Faces-Ordered" $ orderedFacesOf c0l0 (fromMaybe (error "got Nothing") $ findStraightSkeleton c0 [])
    where
      c0 = makePointContour [Point2 (0,0), Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c0l0 = LineSeg (Point2 (0,0)) (Point2 (-1,-1))
      c1 = makePointContour [Point2 (-1,-1), Point2 (0,0), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c2 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (0,0), Point2 (1,1), Point2 (-1,1)]
      c3 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (0,0), Point2 (-1,1)]
      c4 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1), Point2 (0,0)]
      c5 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (2,0), Point2 (1,1), Point2 (-1,1), Point2 (0,0)]
      c6 = makePointContour [Point2 (-1,-1), Point2 (-0.5,-1), Point2 (0,0), Point2 (0.5,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
--      c7 = makePointContour [Point2 (0,-1), Point2 (1,-1), Point2 (1,1), Point2 (0.5,1), Point2 (0.5,0), Point2 (0,1), Point2 (-1,1), Point2 (-1,0), Point2 (0,0)]
