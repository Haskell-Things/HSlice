{- ORMOLU_DISABLE -}
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
{-# LANGUAGE DerivingStrategies #-}

-- | Functions for for applying inset line segments to a series of faces, and for adding infill to a face.
module Graphics.Slicer.Math.Skeleton.Line (insetBy) where

import Prelude ((==), concat, otherwise, (<$>), ($), (/=), error, (<>), show, (<>), (/), floor, fromIntegral, (+), (*), (-), (<>), (>), min, Bool(True, False), fst, maybe, mempty, null, snd)

import Data.List (sortOn, dropWhile, takeWhile, transpose, uncons)

import Data.List.Extra (unsnoc)

import Data.Maybe (Maybe(Just,Nothing), fromMaybe, mapMaybe)

import Slist (head, isEmpty, len, slist)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Contour (makePointContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), (~=), mapWithFollower, mapWithPredecessor, scalePoint, addPoints, endPoint, makeLineSeg, startPoint)

import Graphics.Slicer.Math.Intersections (intersectionOf, noIntersection)

import Graphics.Slicer.Math.Skeleton.Face (Face(Face))

import Graphics.Slicer.Math.Lossy (distancePPointToPLineWithErr, eToPLine2, pToEPoint2)

import Graphics.Slicer.Math.PGA (ProjectiveLine, ProjectiveLine2, eToPL, plinesIntersectIn, pLineIsLeft, translateL)

import Graphics.Slicer.Machine.Contour (cleanContour)

import Graphics.Implicit.Definitions (ℝ, Fastℕ)

------------------------------------------------------------------
------------------ Line Segment Placement ------------------------
------------------------------------------------------------------

-- | inset the given set of faces, returning a new outside contour, and a new set of faces.
-- Requires the faces are a closed set, AKA, a set of faces created from a contour.
-- FIXME: handle inset requests that result in multiple contours.
insetBy :: ℝ -> Slist Face -> ([Contour], [Face])
insetBy distance faceSet
  | null (concat lineSegSets) = ([], [])
  | otherwise = ([reconstructedContour], remainingFaces)
  where
    reconstructedContour = case cleanContour $ makePointContour fuzzyContourPoints of
                             (Just v) -> v
                             Nothing -> error "failed to reconstruct single contour."
    fuzzyContourPoints = mapWithPredecessor recovery (concat $ transpose lineSegSets)
      where
        recovery l1@(LineSeg s1 _) l2@(LineSeg s2 _)
          | endPoint l1 == startPoint l2 = endPoint l1
          | endPoint l2 == startPoint l1 = endPoint l2
          -- error recovery. since we started with a single contour, we know the end of one line should be same as the beginning of the next.
          | endPoint l1 ~= s2 = averagePoints (endPoint l1) s1
          | endPoint l2 ~= s1 = averagePoints (endPoint l2) s1
          | otherwise = error $ "out of order lineSegs generated from faces: " <> show faceSet <> "\n" <> show lineSegSets <> "\n"
    averagePoints p1 p2 = scalePoint 0.5 $ addPoints p1 p2
    lineSegSets = fst <$> res
    remainingFaces = concat $ mapMaybe snd res
    res = addLineSegsToFace distance (Just 1) <$> (\(Slist a _) -> a) faceSet

-- FUTUREWORK: Add a function that takes the contour formed by the remainders of the faces, and squeezes in a line segment, if possible.

-- | Place line segments on a face. Might return remainders, in the form of un-filled faces.
addLineSegsToFace :: ℝ -> Maybe Fastℕ -> Face -> ([LineSeg], Maybe [Face])
addLineSegsToFace distance insets face@(Face edge firstArc midArcs@(Slist rawMidArcs _) lastArc)
  | len midArcs == 0 = (foundLineSegs, twoSideRemainder)
  | len midArcs == 1 = (subSides <> foundLineSegs, threeSideRemainder)
  | otherwise = (sides1 <> sides2 <> foundLineSegs, nSideRemainder)
  where
    -----------------------------------------------------------------------------------------
    -- functions that are the same, regardless of number of sides of the ngon we are filling.
    -----------------------------------------------------------------------------------------
    -- | The direction we need to translate our edge in order for it to be going inward.
    translateDir v         = case eToPLine2 edge `pLineIsLeft` firstArc of
                               (Just True) -> (-v)
                               (Just False) -> v
                               Nothing -> error "cannot happen: edge and firstArc are the same line?"

    -- | How many lines we are going to place in this Face.
    linesToRender          = maybe availableLines (min availableLines) insets
      where
        availableLines = linesUntilEnd distance face

    -- | The line segments we are placing.
    foundLineSegs          = [ makeLineSeg (pToEPoint2 $ fst $ intersectionOf newSide (firstArc, mempty)) (pToEPoint2 $ fst $ intersectionOf newSide (lastArc, mempty)) | newSide <- newSides ]
      where
        newSides = [ translateL (eToPLine2 edge) $ translateDir (-(distance+(distance * fromIntegral segmentNum))) | segmentNum <- [0..linesToRender-1] ]

    -- | The line where we are no longer able to fill this face. from the firstArc to the lastArc, along the point that the lines we place stop.
    finalSide              = makeLineSeg (pToEPoint2 $ fst $ intersectionOf finalLine (firstArc, mempty)) (pToEPoint2 $ fst $ intersectionOf finalLine (lastArc, mempty))
      where
        finalLine = translateL (eToPLine2 edge) $ translateDir (distance * fromIntegral linesToRender)

    -----------------------------------------------------------
    -- functions only used by n-gons with more than four sides.
    -----------------------------------------------------------
    nSideRemainder = case fromMaybe [] remains1 <> fromMaybe [] remains2 of
                       res@(_:_) -> Just res
                       [] -> error "no remains for an nSideRemainder?"

    closestArc             = (\(_,(b,_)) -> b) $ findClosestArc edge firstArc rawMidArcs lastArc
    closestArcFollower     = (\(_,(_,c)) -> c) $ findClosestArc edge firstArc rawMidArcs lastArc

    -- | Return all of the arcs before and including the closest arc.
    untilArc               = if closestArc == firstArc
                             then [firstArc]
                             else takeWhile (/= closestArcFollower) $ rawMidArcs <> [lastArc]

    -- | Return all of the arcs after the closest arc.
    afterArc               = dropWhile (/= closestArcFollower) $ rawMidArcs <> [lastArc]
    (sides1, remains1)     = if closestArc == firstArc
                             then noResult
                             else result firstArc untilArc
    (sides2, remains2)     = case unsnoc rawMidArcs of
                               Nothing -> noResult
                               Just (_,a) -> if closestArc == a
                                             then noResult
                                             else result closestArcFollower afterArc
    noResult = ([],Nothing)
    result begin arcs = case uncons arcs of
                          Nothing -> error "unpossible!"
                          Just (_,[]) -> addLineSegsToFace distance insets (Face finalSide begin (slist []) lastArc)
                          Just (_,manyArcs) -> addLineSegsToFace distance insets (Face finalSide begin remainingArcs lastArc)
                            where
                              remainingArcs = case unsnoc manyArcs of
                                                Nothing -> error "unpossible!"
                                                Just (as,_) -> slist as
    ---------------------------------------------
    -- functions only used by a four-sided n-gon.
    ---------------------------------------------
    midArc = case midArcs of
               (Slist [oneArc] 1) -> oneArc
               (Slist _ _) -> error $ "evaluated midArc with the wrong insets of items\nd: " <> show distance <> "\nn: " <> show insets <> "\nFace: " <> show face <> "\n"
    threeSideRemainder     = if distancePPointToPLineWithErr (intersectionOf (firstArc, mempty) (midArc, mempty)) (eToPL edge) /= distancePPointToPLineWithErr (intersectionOf (midArc, mempty) (lastArc, mempty)) (eToPL edge)
                             then subRemains
                             else Nothing
    (subSides, subRemains) = if firstArcEndsFarthest edge firstArc (head midArcs) lastArc
                             then addLineSegsToFace distance insets (Face finalSide firstArc (slist []) midArc)
                             else addLineSegsToFace distance insets (Face finalSide midArc   (slist []) lastArc)

    ----------------------------------------------
    -- functions only used by a three-sided n-gon.
    ----------------------------------------------
    twoSideRemainder       = if distance * fromIntegral linesToRender /= distanceUntilEnd face
                             then Just [Face finalSide firstArc (slist []) lastArc]
                             else Nothing

-- | How many lines can be drawn onto a given Face, parallel to the face.
linesUntilEnd :: ℝ -> Face -> Fastℕ
linesUntilEnd distance face = floor (distanceUntilEnd face / distance)

-- | What is the distance from the edge of a face to the place where we can no longer place lines.
distanceUntilEnd :: Face -> ℝ
distanceUntilEnd (Face edge firstArc midArcs@(Slist rawMidArcs _) lastArc)
  | isEmpty midArcs  =  distancePPointToPLineWithErr (intersectionOf (firstArc, mempty) (lastArc, mempty)) edgeLine
  | len midArcs == 1 = if firstArcEndsFarthest edge firstArc oneArc lastArc
                       then distancePPointToPLineWithErr firstIntersection edgeLine
                       else distancePPointToPLineWithErr lastIntersection edgeLine
  | otherwise        = fst $ findClosestArc edge firstArc rawMidArcs lastArc
  where
    firstIntersection = intersectionOf (firstArc, mempty) (oneArc, mempty)
    lastIntersection  = intersectionOf (oneArc, mempty) (lastArc, mempty)
    edgeLine = eToPL edge
    oneArc = head midArcs

-- | for a face with four sides, see which arc attached to the face ends the furthest away from the line of the face.
firstArcEndsFarthest :: (ProjectiveLine2 a) => LineSeg -> a -> a -> a -> Bool
firstArcEndsFarthest edge firstArc midArc lastArc = distancePPointToPLineWithErr firstIntersection edgeLine > distancePPointToPLineWithErr lastIntersection edgeLine
  where
    firstIntersection = intersectionOf (firstArc, mempty) (midArc, mempty)
    lastIntersection  = intersectionOf (midArc, mempty) (lastArc, mempty)
    edgeLine = eToPL edge

findClosestArc :: LineSeg -> ProjectiveLine -> [ProjectiveLine] -> ProjectiveLine -> (ℝ, (ProjectiveLine, ProjectiveLine))
findClosestArc edge firstArc rawMidArcs lastArc = case sortOn fst arcIntersections of
                   [] -> error "empty arcIntersections?"
                   [pair] -> pair
                   (pair:_) -> pair
  where
    -- | Find the closest point where two of our arcs intersect, relative to our side.
    arcIntersections = case unsnoc $ mapWithFollower (\a b -> (distancePPointToPLineWithErr (safeIntersectionOf (a,mempty) (b,mempty)) (eToPL edge), (a, b))) $ firstArc : rawMidArcs <> [lastArc] of
                         Nothing     -> []
                         Just (xs,_) -> xs
    safeIntersectionOf a b
      | noIntersection a b = error $ "given a non-intersecting pair of lines."
                                <> show a <> "\n"
                                <> show b <> "\n"
                                <> show (plinesIntersectIn a b) <> "\n"
                                <> showInputs
      | otherwise = intersectionOf a b
    showInputs = "edge: " <> show edge <> "\n"
              <> "firstArc:\n" <> show firstArc <> "\n"
              <> "face:\n" <> show rawMidArcs <> "\n"
              <> "lastArc:\n" <> show lastArc <> "\n"

