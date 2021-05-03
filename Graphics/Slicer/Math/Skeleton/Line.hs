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

-- inherit instances when deriving.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

{-
 - This file contains two things that should probably be in separate files:
 - code for applying inset line segments to a series of faces, and
 - code to and infill to faces.
 -}
module Graphics.Slicer.Math.Skeleton.Line (addLineSegsToFace, addInfill) where

import Prelude ((==), otherwise, (<$>), ($), length, (/=), error, (<>), show, (<>), (/), floor, fromIntegral, Either(Left, Right), (+), (*), (-), (++), (>), min, Bool(True), head, (&&), fst, init, null, tail, last, maybe)

import Data.List (sortOn, dropWhile, takeWhile)

import Data.Maybe( Maybe(Just,Nothing), isJust, fromJust)

import Graphics.Slicer.Math.Definitions (mapWithFollower)

import Graphics.Slicer.Math.Line (LineSeg, lineSegFromEndpoints, LineSegError(LineSegFromPoint))

import Graphics.Slicer.Math.Skeleton.Definitions (intersectionOf)

import Graphics.Slicer.Math.Skeleton.Face (Face(Face))

import Graphics.Slicer.Math.PGA (distancePPointToPLine, pToEPoint2, PLine2, eToPLine2, translatePerp, pLineIsLeft)

import Graphics.Slicer.Machine.Infill (makeInfill, InfillType)

import Graphics.Implicit.Definitions (ℝ, Fastℕ)

------------------------------------------------------------------
------------------ Line Segment Placement ------------------------
------------------------------------------------------------------

-- | Place line segments on a face. Might return remainders, in the form of one or multiple un-filled faces.
addLineSegsToFace :: ℝ -> Maybe Fastℕ -> Face -> ([LineSeg], Maybe [Face])
addLineSegsToFace lw n face@(Face edge firstArc midArcs lastArc)
  | null midArcs        = (                    foundLineSegs, twoSideRemainder)
  | length midArcs == 1 = (        subSides ++ foundLineSegs, threeSideRemainder)
  | otherwise           = (sides1 ++ sides2 ++ foundLineSegs, nSideRemainder)

  where
    -----------------------------------------------------------------------------------------
    -- functions that are the same, regardless of number of sides of the ngon we are filling.
    -----------------------------------------------------------------------------------------

    -- | The direction we need to translate our edge in order for it to be going inward.
    translateDir v         = if Just True == pLineIsLeft (eToPLine2 edge) firstArc then (-v) else v

    -- | How many lines we are going to place in this Face.
    linesToRender          = maybe linesUntilEnd (min linesUntilEnd) n

    -- | The line segments we are placing.
    foundLineSegs          = [ errorIfLeft $ lineSegFromEndpoints (pToEPoint2 $ intersectionOf newSide firstArc) (pToEPoint2 $ intersectionOf newSide lastArc) | newSide <- newSides ]
      where
        newSides = [ translatePerp (eToPLine2 edge) $ translateDir ((lw/2)+(lw * fromIntegral segmentNum)) | segmentNum <- [0..linesToRender-1] ]

    -- | The line where we are no longer able to fill this face. from the firstArc to the lastArc, along the point that the lines we place stop.
    finalSide              = errorIfLeft $ lineSegFromEndpoints (pToEPoint2 $ intersectionOf finalLine firstArc) (pToEPoint2 $ intersectionOf finalLine lastArc)
      where
        finalLine = translatePerp (eToPLine2 edge) $ translateDir (lw * fromIntegral linesToRender)
    -- | how many lines can be fit in this Face.
    linesUntilEnd :: Fastℕ
    linesUntilEnd          = floor (distanceUntilEnd / lw)

    -- | what is the distance from the edge to the place we can no longer place lines.
    distanceUntilEnd
      | length midArcs >1  = closestArcDistance
      | length midArcs==1  = if firstArcLonger
                             then distancePPointToPLine (intersectionOf firstArc midArc) (eToPLine2 edge)
                             else distancePPointToPLine (intersectionOf midArc lastArc) (eToPLine2 edge)
      | otherwise          = distancePPointToPLine (intersectionOf firstArc lastArc) (eToPLine2 edge)

    -- | Generate an error if a line segment fails to construct.
    errorIfLeft :: Either LineSegError LineSeg -> LineSeg
    errorIfLeft lnSeg      = case lnSeg of
      Left (LineSegFromPoint point) -> error $ "tried to construct a line segment from two identical points: " <> show point <> "\n"
      Right                 lineSeg -> lineSeg
      _                             -> error "unknown error"

    -----------------------------------------------------------
    -- functions only used by n-gons with more than four sides.
    -----------------------------------------------------------
    nSideRemainder
      | isJust remains1 && isJust remains2 = Just $ fromJust remains1 ++ fromJust remains2
      | isJust remains1                    = remains1
      | isJust remains2                    = remains2
      | otherwise                          = error "impossible!"
    -- | Find the closest point where two of our arcs intersect, relative to our side.
    arcIntersections = init $ mapWithFollower (\a b -> (distancePPointToPLine (intersectionOf a b) (eToPLine2 edge), (a, b))) $ [firstArc] ++ midArcs ++ [lastArc]
    findClosestArc :: (ℝ, (PLine2, PLine2))
    findClosestArc         = head $ sortOn fst arcIntersections
    closestArcDistance     = fst findClosestArc
    closestArc             = (\(_,(b,_)) -> b) findClosestArc
    closestArcFollower     = (\(_,(_,c)) -> c) findClosestArc
    -- Return all of the arcs before and including the closest arc.
    untilArc               = if closestArc == firstArc
                             then [firstArc]
                             else takeWhile (/= closestArcFollower) $ midArcs ++ [lastArc]
    afterArc               = dropWhile (/= closestArcFollower) $ midArcs ++ [lastArc]
    (sides1, remains1)     = if closestArc == firstArc
                             then ([],Nothing)
                             else addLineSegsToFace lw n (Face finalSide firstArc (tail $ init untilArc) closestArc)
    (sides2, remains2)     = if closestArc == last midArcs
                             then ([],Nothing)
                             else addLineSegsToFace lw n (Face finalSide (head afterArc) (init $ tail afterArc) lastArc)
    ---------------------------------------------
    -- functions only used by a four-sided n-gon.
    ---------------------------------------------
    midArc
      | length midArcs==1  = head midArcs
      | otherwise          = error $ "evaluated midArc with the wrong number of items\nlw: " <> show lw <> "\nn: " <> show n <> "\nFace: " <> show face <> "\n"
    threeSideRemainder     = if distancePPointToPLine (intersectionOf firstArc midArc) (eToPLine2 edge) /= distancePPointToPLine (intersectionOf midArc lastArc) (eToPLine2 edge)
                             then subRemains
                             else Nothing
    (subSides, subRemains) = if firstArcLonger
                             then addLineSegsToFace lw n (Face finalSide firstArc [] midArc)
                             else addLineSegsToFace lw n (Face finalSide midArc   [] lastArc)
    firstArcLonger         = distancePPointToPLine (intersectionOf firstArc midArc) (eToPLine2 edge) > distancePPointToPLine (intersectionOf midArc lastArc) (eToPLine2 edge)
    ----------------------------------------------
    -- functions only used by a three-sided n-gon.
    ----------------------------------------------
    twoSideRemainder       = if lw * fromIntegral linesUntilEnd /= distanceUntilEnd
                             then Just [Face finalSide firstArc [] lastArc]
                             else Nothing

-- | Add infill to the area of a set of faces that was not covered in lines.
-- FIXME: unimplemented. basically, take the contour formed by the remainders of the faces, and squeeze in a line segment, if possible.
addInfill :: [Face] -> [[Face]] -> ℝ -> InfillType -> [[LineSeg]]
addInfill outsideFaces insideFaceSets = makeInfill (facesToContour outsideFaces) (facesToContour <$> insideFaceSets)
  where
    facesToContour faces = error "fixme!"

