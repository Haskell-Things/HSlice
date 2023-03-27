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
module Graphics.Slicer.Math.Skeleton.Line (insetBy, infiniteInset) where

import Prelude ((==), all, concat, otherwise, (<$>), (<=), (&&), ($), (/=), error, (<>), show, (<>), (/), floor, fromIntegral, (+), (*), (-), (.), (<>), (>), (<), min, Bool(True, False), filter, fst, maybe, mempty, null, snd)

import Data.Either (isRight)

import Data.List (sortOn, dropWhile, last, length, takeWhile, transpose, uncons)

import Data.List.Extra (unsnoc)

import Data.Maybe (Maybe(Just,Nothing), catMaybes, fromMaybe, fromJust, isJust, isNothing, mapMaybe)

import Slist (head, isEmpty, len, slist)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Contour (makePointContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg, Point2, mapWithFollower, endPoint, lineSegsOfContour, makeLineSeg, startPoint)

import Graphics.Slicer.Math.Ganja (dumpGanja)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.Intersections (intersectionBetween, intersectionOf, noIntersection)

import Graphics.Slicer.Math.Skeleton.Face (Face(Face))

import Graphics.Slicer.Math.Lossy (distancePPointToPLineWithErr, eToPLine2, pToEPoint2)

import Graphics.Slicer.Math.PGA (PIntersection (IntersectsIn, PCollinear, PParallel), PLine2Err, ProjectiveLine, ProjectiveLine2, distance2PP, eToPL, eToPP, fuzzinessOfL, normalizeL, pLineErrAtPPoint, plinesIntersectIn, pLineIsLeft, pToEP, translateL)

import Graphics.Slicer.Machine.Contour (cleanContour)

import Graphics.Implicit.Definitions (ℝ, Fastℕ)

------------------------------------------------------------------
------------------ Line Segment Placement ------------------------
------------------------------------------------------------------

-- | Inset the given set of faces, returning new outside contours, and a new set of faces.
-- Requires the faces are a closed set, AKA, a set of faces created from a contour.
-- FIXME: handle inset requests that result in multiple contours.
insetBy :: ℝ -> Slist Face -> ([Contour], [Face])
insetBy distance faces
  | null (concat lineSegSets) = ([], [])
  | length (concat lineSegSets) < 3 = error "less than three, but not zero?"
  | otherwise = (contours, remainingFaces)
  where
    contours = reclaimContours lineSegSets
    lineSegSets = fst <$> res
    remainingFaces = concat $ mapMaybe snd res
    res = addLineSegsToFace distance (Just 1) <$> (\(Slist a _) -> a) faces

-- FUTUREWORK: Add a function that takes the contour formed by the remainders of the faces, and squeezes in a line segment, if possible.

-- | Cover a contour with lines, aligned to the faces of the contour.
-- FIXME: this should be returning a ContourTree.
infiniteInset :: ℝ -> Slist Face -> [[LineSeg]]
infiniteInset distance faces
  | null (concat lineSegSets) = []
  | length (concat lineSegSets) < 3 = error "less than three, but not zero?"
  | otherwise = lineSegsOfContour <$> contours
  where
    contours = reclaimContours lineSegSets
    lineSegSets = fst <$> res
    res = addLineSegsToFace distance Nothing <$> (\(Slist a _) -> a) faces

-- | Place line segments on a face, parallel to the edge. Might return remainders, in the form of un-filled faces.
-- FIXME: return a ((ProjectivePoint, PPoint2Err), (ProjectivePoint, PPoint2Err)) pair, so we can operate on it during contour reclamation without precision loss.
addLineSegsToFace :: ℝ -> Maybe Fastℕ -> Face -> ([LineSeg], Maybe [Face])
addLineSegsToFace distance insets face
  -- we were called, but instructed to do nothing.
  | isJust insets && fromJust insets < 1 = ([], Just [face])
  | len midArcs == 0 = (foundLineSegs, twoSideRemainder)
  | len midArcs == 1 = (foundLineSegs <> twoSideSubLineSegs, threeSideRemainder)
  | otherwise        = (foundLineSegs <> sides1 <> sides2, nSideRemainder)
  where
    -- | Run checks on our input face.
    checkedFace@(Face edge firstArc midArcs@(Slist rawMidArcs _) lastArc) = checkFace face
      where
        checkFace inFace@(Face myEdge myFirstArc (Slist myMidArcs _) myLastArc)
          | all (isRight . fromMaybe (error "wheee!")) intersections = inFace
          | otherwise = error $ "given a degenerate face: \n"
                             <> show face <> "\n"
                             <> show intersections <> "\n"
                             <> show insets <> "\n"
          where
            intersections = mapWithFollower intersectionBetween $ eToPL myEdge : myFirstArc : myMidArcs <> [myLastArc]

    -- | Subtract the line segments we place in this round from the input inset count.
    -- Used to determine if we should recurse.
    -- Just 0 or less == terminate, do not recurse.
    -- Nothing = recurse until we run out of space in the Face.
    subInsets = if isJust insets
                then Just $ fromJust insets - linesToRender
                else Nothing

    -----------------------------------------------------------------------------------------
    -- functions that are the same, regardless of number of sides of the ngon we are filling.
    -----------------------------------------------------------------------------------------
    -- | The direction we need to translate our edge in order for it to be going inward.
    translateDir v         = case eToPLine2 edge `pLineIsLeft` (fst firstArc) of
                               (Just True) -> (-v)
                               (Just False) -> v
                               Nothing -> error $ "cannot happen: edge and firstArc do not intersect?\n"
                                               <> show distance <> "\n"
                                               <> show insets <> "\n"
                                               <> show face <> "\n"
                                               <> show (normalizeL $ fst $ eToPL edge) <> "\n"
                                               <> show (normalizeL $ fst firstArc) <> "\n"
                                               <> show (plinesIntersectIn firstArc $ eToPL edge) <> "\n"
                                               <> dumpGanja face <> "\n"

    -- | How many lines we are going to place in this recursion. If inset is Nothing, cover the face entirely.
    linesToRender          = maybe availableLines (min availableLines) insets
      where
        availableLines = linesUntilEnd distance checkedFace

    -- | The line segments we are placing.
    foundLineSegs          = [ makeLineSeg (pToEPoint2 $ fst $ safeIntersectionOf newSide firstArc) (pToEPoint2 $ fst $ safeIntersectionOf newSide lastArc) | newSide <- newSides ]
      where
        newSides = [ translateL (eToPLine2 edge) $ translateDir (-(distance+(distance * fromIntegral segmentNum))) | segmentNum <- [0..linesToRender-1] ]

    -- | The line where we are no longer able to fill this face. from the firstArc to the lastArc, along the point that the lines we place stop.
    finalSide              = makeLineSeg (pToEPoint2 $ fst firstIntersection) (pToEPoint2 $ fst lastIntersection)
      where
        firstIntersection = safeIntersectionOf finalLine firstArc
        lastIntersection = safeIntersectionOf finalLine lastArc
        finalLine = translateL (eToPLine2 edge) $ translateDir (distance * fromIntegral linesToRender)

    -- | A wrapper, for generating smart errors.
    safeIntersectionOf a b
      | noIntersection a b = error $ "given a non-intersecting pair of lines."
                                <> show a <> "\n"
                                <> show b <> "\n"
                                <> show (plinesIntersectIn a b) <> "\n"
                                <> showInputs
      | otherwise = intersectionOf a b

    -- | dump our inputs, in case of failure.
    showInputs = "edge: " <> show edge <> "\n"
              <> "edgeLine: \n" <> show (normalizeL $ fst $ eToPL edge) <> "\n"
              <> "firstArc: \n" <> show firstArc <> "\n"
              <> "midArcs: \n" <> show midArcs <> "\n"
              <> "lastArc: \n" <> show lastArc <> "\n"
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
                          Just (_,[]) -> addLineSegsToFace distance subInsets (makeFace finalSide begin (slist []) lastArc)
                          Just (_,manyArcs) -> addLineSegsToFace distance subInsets (makeFace finalSide begin remainingArcs lastArc)
                            where
                              remainingArcs = case unsnoc manyArcs of
                                                Nothing -> error "unpossible!"
                                                Just (as,_) -> slist as
    ---------------------------------------------
    -- functions only used by a four-sided n-gon.
    ---------------------------------------------
    threeSideRemainder
      | null foundLineSegs = Nothing
      | otherwise          = case plinesIntersectIn edgeLine lastPlacedLine of
                               PCollinear -> Nothing
                               PParallel -> Just [makeFaceNoCheck finalSide firstArc (slist [midArc]) lastArc]
                               _ -> twoSideSubRemainder
    -- Recurse, so we get the remainder and line segments of the three sided n-gon left over.
    (twoSideSubLineSegs,
     twoSideSubRemainder)
      | null foundLineSegs = ([],Nothing)
      | otherwise          = case plinesIntersectIn edgeLine lastPlacedLine of
                               PCollinear -> ([], Nothing)
                               _ -> if firstArcEndsFarthest edge firstArc (head midArcs) lastArc
                                    then addLineSegsToFace distance subInsets (makeFace finalSide firstArc (slist []) midArc)
                                    else addLineSegsToFace distance subInsets (makeFace finalSide midArc   (slist []) lastArc)
    edgeLine = eToPL edge
    lastPlacedLine = eToPL $ last foundLineSegs
    midArc = case midArcs of
               (Slist [oneArc] 1) -> oneArc
               (Slist _ _) -> error $ "evaluated midArc with the wrong insets of items.\n"
                                   <> "d: " <> show distance <> "\n"
                                   <> "n: " <> show insets <> "\n"
                                   <> "Face: " <> show face <> "\n"
    ----------------------------------------------
    -- functions only used by a three-sided n-gon.
    ----------------------------------------------
    twoSideRemainder     = if distance * fromIntegral linesToRender /= distanceUntilEnd checkedFace
                           then Just [makeFaceNoCheck finalSide firstArc (slist []) lastArc]
                           else Nothing

-- | How many lines can be drawn onto a given Face, parallel to the face.
linesUntilEnd :: ℝ -> Face -> Fastℕ
linesUntilEnd distance face = floor (distanceUntilEnd face / distance)

-- | What is the distance from the edge of a face to the place where we can no longer place lines.
distanceUntilEnd :: Face -> ℝ
distanceUntilEnd (Face edge firstArc midArcs@(Slist rawMidArcs _) lastArc)
  | isEmpty midArcs  = distancePPointToPLineWithErr crossIntersection edgeLine
  | len midArcs == 1 = if firstArcEndsFarthest edge firstArc midArc lastArc
                       then distancePPointToPLineWithErr firstIntersection edgeLine
                       else distancePPointToPLineWithErr lastIntersection edgeLine
  | otherwise        = fst $ findClosestArc edge firstArc rawMidArcs lastArc
  where
    firstIntersection = safeIntersectionOf firstArc midArc
    lastIntersection  = safeIntersectionOf midArc lastArc
    crossIntersection = safeIntersectionOf firstArc lastArc
    safeIntersectionOf a b
      | noIntersection a b = error $ "given a non-intersecting pair of lines."
                                <> show a <> "\n"
                                <> show b <> "\n"
                                <> show (plinesIntersectIn a b) <> "\n"
                                <> showInputs
      | otherwise = intersectionOf a b
    showInputs = "edge: " <> show edge <> "\n"
              <> "firstArc: \n" <> show firstArc <> "\n"
              <> "midArcs: \n" <> show midArcs <> "\n"
              <> "lastArc: \n" <> show lastArc <> "\n"
    edgeLine = eToPL edge
    midArc = head midArcs

-- | for a face with four sides, see which arc attached to the face ends the furthest away from the line of the face.
firstArcEndsFarthest :: (ProjectiveLine2 a) => LineSeg -> (a, PLine2Err) -> (a, PLine2Err) -> (a, PLine2Err) -> Bool
firstArcEndsFarthest edge firstArc midArc lastArc = distancePPointToPLineWithErr firstIntersection edgeLine > distancePPointToPLineWithErr lastIntersection edgeLine
  where
    firstIntersection = safeIntersectionOf firstArc midArc
    lastIntersection  = safeIntersectionOf midArc lastArc
    safeIntersectionOf a b
      | noIntersection a b = error $ "given a non-intersecting pair of lines."
                                <> show a <> "\n"
                                <> show b <> "\n"
                                <> show (plinesIntersectIn a b) <> "\n"
                                <> showInputs
      | otherwise = intersectionOf a b
    showInputs = "edge: " <> show edge <> "\n"
              <> "firstArc: \n" <> show firstArc <> "\n"
              <> "midArc: \n" <> show midArc <> "\n"
              <> "lastArc: \n" <> show lastArc <> "\n"
    edgeLine = eToPL edge

findClosestArc :: LineSeg -> (ProjectiveLine, PLine2Err) -> [(ProjectiveLine, PLine2Err)] -> (ProjectiveLine, PLine2Err) -> (ℝ, ((ProjectiveLine, PLine2Err), (ProjectiveLine, PLine2Err)))
findClosestArc edge firstArc rawMidArcs lastArc = case sortOn fst arcIntersections of
                   [] -> error "empty arcIntersections?"
                   [pair] -> pair
                   (pair:_) -> pair
  where
    -- | Find the closest point where two of our arcs intersect, relative to our side.
    arcIntersections = case unsnoc $ mapWithFollower (\a b -> (distancePPointToPLineWithErr (safeIntersectionOf a b) (eToPL edge), (a, b))) $ firstArc : rawMidArcs <> [lastArc] of
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
              <> "firstArc: \n" <> show firstArc <> "\n"
              <> "midArcs: \n" <> show rawMidArcs <> "\n"
              <> "lastArc: \n" <> show lastArc <> "\n"

-- | Take the output of many calls to addLineSegsToFace, and construct contours from them.
reclaimContours :: [[LineSeg]] -> [Contour]
reclaimContours lineSegSets = if all isJust reclaimedRings && all isJust cleanedContours
                                 then catMaybes cleanedContours
                                 else error $ "failed to clean a contour in rings: " <> show rings <> "\n"
  where
    cleanedContours = cleanContour <$> concat (fromJust <$> reclaimedRings)
    reclaimedRings = reclaimRing <$> rings
    -- The input set of line segments has all of the line segments that cover a face in the same list.
    -- by transposing them, we get lists of rings around the object, rather than covered petals.
    rings = transpose lineSegSets

-- | tage a ring around N contours, and generate the contours.
-- FIXME: not handling breaks yet.
reclaimRing :: [LineSeg] -> Maybe [Contour]
reclaimRing ring = case filter (\(a,_) -> isNothing a) reclaimedContour of
                     [] -> Just [makePointContour $ fromJust . fst <$> reclaimedContour]
                     [a] -> error $ "found one break in ring: " <> show ring <> " at " <> show a <> "\n"
                     _ -> Nothing
  where
    reclaimedContour = mapWithFollower recovery ring
      where
        -- detect if two line segments SHOULD end at the same point, and if they do, return the point.
        recovery :: LineSeg -> LineSeg -> (Maybe Point2, (LineSeg, LineSeg))
        recovery l1 l2
          | endPoint l2 == startPoint l1 = (Just $ endPoint l2, (l1, l2))
          -- FIXME: we should use intersection for line segments close to 90 degrees, and average for segments closest to parallel?
          | l1l2Distance <= l1l2DistanceErr = (Just $ fst $ pToEP $ fst $ intersectionOf (eToPL l2) (eToPL l1), (l1,l2))
          | otherwise = (Nothing, (l1,l2))
          where
            --- FIXME: magic number: 512
            l1l2DistanceErr = 512 * ulpVal (l1l2DistanceErrRaw
                                            <> pLineErrAtPPoint (eToPL l1) (eToPP $ startPoint l1)
                                            <> fuzzinessOfL (eToPL l1)
                                            <> pLineErrAtPPoint (eToPL l2) (eToPP $ endPoint l2)
                                            <> fuzzinessOfL (eToPL l2))
            (l1l2Distance, (_, _, l1l2DistanceErrRaw)) = distance2PP (eToPP $ endPoint l2, mempty) (eToPP $ startPoint l1, mempty)

-- | A face constructor that checks that a face is valid during construction.
makeFace :: LineSeg -> (ProjectiveLine, PLine2Err) -> Slist (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> Face
makeFace edge firstArc arcs lastArc = res
  where
    res = checkFace $ Face edge firstArc arcs lastArc
    checkFace inFace@(Face myEdge myFirstArc (Slist myMidArcs _) myLastArc)
      | all isIntersection intersections = inFace
      | otherwise = error $ "Tried to generate a degenerate face: "
                         <> show inFace <> "\n"
                         <> show intersections <> "\n"
      where
        isIntersection intersection = case intersection of
                                        (IntersectsIn _ _) -> True
                                        _ -> False
        intersections = mapWithFollower plinesIntersectIn $ eToPL myEdge : myFirstArc : myMidArcs <> [myLastArc]

-- | a Face constructor with no checking.
makeFaceNoCheck :: LineSeg -> (ProjectiveLine, PLine2Err) -> Slist (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> Face
makeFaceNoCheck edge firstArc arcs lastArc = res
  where
    res = Face edge firstArc arcs lastArc
