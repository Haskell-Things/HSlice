-- Slicer.
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

module Graphics.Slicer.Math.Face (Face, makeFaces, addLineSegs) where

import Prelude ((==), otherwise, (<$>), ($), length, Show, (/=), error, (<>), show, Eq, Show, (<>), (<), (/), floor, fromIntegral, Either(Left, Right), (+), (*), (-))

import Data.Maybe(Maybe(Just,Nothing))

import Graphics.Slicer.Math.Definitions (Contour(PointSequence), Point2)

import Graphics.Slicer.Math.Line (LineSeg, lineSegFromEndpoints, LineSegError(LineSegFromPoint))

import Graphics.Slicer.Math.PGA (lineIsLeft, pointOnPerp, distancePPointToPLine, pToEPoint2, PLine2, plinesIntersectIn, PIntersection(PColinear,IntersectsIn,PParallel,PAntiParallel), eToPLine2, translatePerp)

import Graphics.Implicit.Definitions (ℝ, Fastℕ)

-- | A Face.
--   A portion of a contour, with a real side, and arcs (line segments between nodes) dividing it from other faces.
--   Faces have no holes, and their arcs and nodes (lines and points) are from a straight skeleton of a contour.
data Face = Face { _realSide :: LineSeg, _arcs :: [PLine2] }
  deriving (Show, Eq)

-- | construct a set of faces, using a straight skeleton to divide up the area of a contour, and the holes in the contour.
makeFaces :: Contour -> [Contour] -> [Face]
makeFaces contour holes = error "undefined!"
  where
    motorcycles :: Contour -> [(Point2, PLine2)]
    motorcycles (PointSequence c) = error "undefined!"

-- | Place line segments on a face. Might return remainders, in the form of one or multiple un-filled faces.
addLineSegs :: Face -> Fastℕ -> ℝ -> ([LineSeg], Maybe [Face])
addLineSegs face@(Face _ arcs) count lineWidth
  | length arcs == 2 = threeFace face count lineWidth
  | otherwise        = error "undefined!"
    where
      fourFace :: Face -> Fastℕ -> ℝ -> ([LineSeg], Maybe [Face])
      fourFace = error "undefined!"
      -- | handle faces that are triangular wedges. easiest case.
      threeFace :: Face -> Fastℕ -> ℝ -> ([LineSeg], Maybe [Face])
      threeFace f@(Face rs (a1:a2:[])) n lw = (foundLineSegs, remainder)
        where
          foundLineSegs          = errorIfLeft <$> maybeFoundLineSegs
          maybeFoundLineSegs     = [ lineSegFromEndpoints (pToEPoint2 $ intersectionOf newSide a1) (pToEPoint2 $ intersectionOf newSide a2) | newSide <- newSides ]
          newSides               = [ translatePerp (eToPLine2 rs) ((lw/2)+(lw*(fromIntegral (segmentNum-1)))) | segmentNum <- [1..linesToRender]]
          remainder              = if (distancePPointToPLine (intersectionOf a1 a2) (eToPLine2 rs)) /= (fromIntegral (linesToRender-1))*lw
                                   then Just [(Face finalSide (a1:a2:[]))]
                                   else Nothing
          finalLine              = translatePerp (eToPLine2 rs) ((fromIntegral linesToRender)*lw)
          finalSide              = errorIfLeft $ lineSegFromEndpoints (pToEPoint2 $ intersectionOf finalLine a1) (pToEPoint2 $ intersectionOf finalLine a2)
          linesToRender          = if linesUntilEnd < n then linesUntilEnd else n
          linesUntilEnd          = floor $ (distancePPointToPLine (intersectionOf a1 a2) (eToPLine2 rs)) / lw
          errorIfLeft lnSeg      = case lnSeg of
            Left (LineSegFromPoint point) -> error $ "tried to construct a line segment from two identical points: " <> show point <> "\n"
            Right                 lineSeg -> lineSeg
          intersectionOf pl1 pl2 = saneIntersection $ plinesIntersectIn pl1 pl2
            where
              saneIntersection PColinear            = error "impossible!"
              saneIntersection PParallel            = error "impossible!"
              saneIntersection PAntiParallel        = error "impossible!"
              saneIntersection (IntersectsIn point) = point
