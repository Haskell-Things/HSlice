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

import Prelude (Integer, (==), all, concat, max, otherwise, (<$>), (<=), (&&), (||), ($), (/=), error, (<>), show, (<>), (/), floor, fromIntegral, (*), (-), (.), (<>), (>), (<), min, Bool(True, False), filter, fst, maybe, mempty, null, snd)

import Data.Either (isRight)

import Data.List (concatMap, dropWhile, intercalate, last, length, nub, sortOn, takeWhile, transpose, uncons)

import Data.List.Extra (unsnoc)

import Data.Maybe (Maybe(Just,Nothing), catMaybes, fromMaybe, fromJust, isJust, isNothing, mapMaybe)

import Slist (head, isEmpty, len, slist)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Contour (makePointContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg, Point2, distance, mapWithFollower, endPoint, lineSegsOfContour, makeLineSeg, startPoint)

import Graphics.Slicer.Math.Ganja (dumpGanja)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.Intersections (intersectionBetween, intersectionOf, isAntiParallel, noIntersection)

import Graphics.Slicer.Math.Skeleton.Face (Face(Face))

import Graphics.Slicer.Math.Lossy (distancePPointToPLineWithErr, eToPLine2, pToEPoint2)

import Graphics.Slicer.Math.PGA (PIntersection (IntersectsIn, PAntiCollinear, PAntiParallel, PCollinear, PParallel), PLine2Err, ProjectiveLine, ProjectiveLine2, distance2PP, eToPL, eToPP, fuzzinessOfL, normalizeL, pLineErrAtPPoint, plinesIntersectIn, pLineIsLeft, pToEP, translateL)

import Graphics.Slicer.Machine.Contour (cleanContour)

import Graphics.Implicit.Definitions (ℝ, Fastℕ)

------------------------------------------------------------------
------------------ Line Segment Placement ------------------------
------------------------------------------------------------------

-- | Inset the given set of faces, returning new outside contours, and a new set of faces.
-- Requires the faces are a closed set, AKA, a set of faces created from a contour.
-- FIXME: handle inset requests that result in multiple contours.
insetBy :: ℝ -> Slist Face -> ([Contour], [Face])
insetBy distanceBetweenSegs faces
  -- no result? no resulting faces.
  | all null lineSegSets = mempty
  -- there cannot be such a thing as 2 faces remaining.
  -- assume we passed the endpoints of all faces.
  | length remainingFaces < 3 = (contours, [])
  | otherwise = (contours, remainingFaces)
  where
    contours = reclaimContours lineSegSets
    lineSegSets = fst <$> res
    remainingFaces = concat $ mapMaybe snd res
    res = addLineSegsToFace distanceBetweenSegs (Just 1) <$> (\(Slist a _) -> a) faces

-- FUTUREWORK: Add a function that takes the contour formed by the remainders of the faces, and squeezes in some line segments, if possible.

-- | Cover a contour with lines, aligned to the faces of the contour.
-- FIXME: this should be returning a ContourTree.
infiniteInset :: ℝ -> Slist Face -> [[LineSeg]]
infiniteInset distanceBetweenSegs faces
  | all null lineSegSets = mempty
  | length (concat lineSegSets) < 3 = error $ "less than three, but not zero?\n" <> show lineSegSets <> "\n"
  | otherwise = lineSegsOfContour <$> contours
  where
    contours = reclaimContours lineSegSets
    lineSegSets = fst <$> res
    res = addLineSegsToFace distanceBetweenSegs Nothing <$> (\(Slist a _) -> a) faces

-- | Place line segments on a face, parallel to the edge. Might return remainders, in the form of un-filled faces.
-- FIXME: return a ((ProjectivePoint, PPoint2Err), (ProjectivePoint, PPoint2Err)) pair, so we can operate on it during contour reclamation without precision loss.
addLineSegsToFace :: ℝ -> Maybe Fastℕ -> Face -> ([LineSeg], Maybe [Face])
addLineSegsToFace distanceBetweenSegs insets face
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
                then Just $ fromJust insets - linesToPlace
                else Nothing

    -----------------------------------------------------------------------------------------
    -- functions that are the same, regardless of number of sides of the ngon we are filling.
    -----------------------------------------------------------------------------------------
    -- | The direction we need to translate our edge in order for it to be going inward.
    translateDir v         = case eToPLine2 edge `pLineIsLeft` fst firstArc of
                               (Just True) -> (-v)
                               (Just False) -> v
                               Nothing -> error $ "cannot happen: edge and firstArc do not intersect?\n"
                                               <> show distanceBetweenSegs <> "\n"
                                               <> show insets <> "\n"
                                               <> show face <> "\n"
                                               <> show (normalizeL $ fst $ eToPL edge) <> "\n"
                                               <> show (normalizeL $ fst firstArc) <> "\n"
                                               <> show (plinesIntersectIn firstArc $ eToPL edge) <> "\n"
                                               <> dumpGanja face <> "\n"

    -- | How many lines we are going to place in this recursion. If inset is Nothing, cover the face entirely.
    linesToPlace           = maybe availableLines (min availableLines) insets
      where
        availableLines = linesUntilEnd distanceBetweenSegs checkedFace

    -- | The line segments we are placing in this round.
    foundLineSegs          = catMaybes [ maybeMakeLineSeg (pToEPoint2 $ fst $ safeIntersectionOf newSide lastArc) (pToEPoint2 $ fst $ safeIntersectionOf newSide firstArc) | newSide <- newSides ]
      where
        newSides = [ translateL (eToPLine2 edge) $ translateDir (-distanceBetweenSegs * fromIntegral segmentNum) | segmentNum <- [1..linesToPlace] ]
        -- Filter out the case where we try to construct an empty segment, EG: we have inset to the point we have only a point, not a line segment.

    -- | Maybe make a line segment. Maybe not.
    maybeMakeLineSeg a b
      | a == b = Nothing
      | otherwise = Just $ makeLineSeg a b

    -- | The line where we are no longer able to fill this face. from the firstArc to the lastArc, along the point that the lines we place stop.
    finalSide              = fromMaybe (error "tried to get the final side and it's a point!") maybeFinalSide
    maybeFinalSide         = maybeMakeLineSeg (pToEPoint2 $ fst lastIntersection) (pToEPoint2 $ fst firstIntersection)
      where
        firstIntersection = safeIntersectionOf finalLine firstArc
        lastIntersection = safeIntersectionOf finalLine lastArc
        finalLine = translateL (eToPLine2 edge) $ translateDir (-distanceBetweenSegs * fromIntegral linesToPlace)

    -- | A wrapper, for generating smart errors.
    safeIntersectionOf a b
      | noIntersection a b = error $ "given a non-intersecting pair of lines."
                                <> show a <> "\n"
                                <> show b <> "\n"
                                <> show (plinesIntersectIn a b) <> "\n"
                                <> showInputs
      | otherwise = intersectionOf a b

    -- | what to return when no result is necessary (we have run into the end of the face).
    noResult = ([],Nothing)

    -- | dump our inputs, in case of failure.
    showInputs = "edge: " <> show edge <> "\n"
              <> "firstArc: \n" <> show firstArc <> "\n"
              <> "midArcs: \n" <> show midArcs <> "\n"
              <> "lastArc: \n" <> show lastArc <> "\n"
    -----------------------------------------------------------
    -- functions only used by n-gons with more than four sides.
    -----------------------------------------------------------
    nSideRemainder
      | null foundLineSegs = Nothing
      | otherwise = case fromMaybe [] remains1 <> fromMaybe [] remains2 of
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
                             else nSideSubResult firstArc untilArc
    (sides2, remains2)     = case unsnoc rawMidArcs of
                               Nothing -> noResult
                               Just (_,a) -> if closestArc == a
                                             then noResult
                                             else nSideSubResult closestArcFollower afterArc

    -- | recurse, so we get the remainder and line segments of the remaining parts of the Face.
    nSideSubResult begin arcs
      | isNothing maybeFinalSide = noResult
      | otherwise = case uncons arcs of
                      Nothing -> error "unpossible!"
                      Just (_,[]) -> addLineSegsToFace distanceBetweenSegs subInsets (makeFace 1 finalSide begin (slist []) lastArc)
                      Just (_,manyArcs) -> addLineSegsToFace distanceBetweenSegs subInsets (makeFace 2 finalSide begin remainingArcs lastArc)
                        where
                          remainingArcs = case unsnoc manyArcs of
                                            Nothing -> error "unpossible!"
                                            Just (as,_) -> slist as
    ---------------------------------------------
    -- functions only used by a four-sided n-gon.
    ---------------------------------------------
    -- Determine if this face has a remainder, if that remainder has three sides, or if the remainder has two sides.
    threeSideRemainder :: Maybe [Face]
    threeSideRemainder
      -- If we weren't anle to place a line segment, we're done.
      | null foundLineSegs = Nothing
      | otherwise = case plinesIntersectIn midArc lastPlacedLine of
                      -- always an error. our line segments are placed in the opposite direction as our midarc.
                      PCollinear -> error "a constructed line segment cannot be colinear with the midArc"
                      PParallel -> error "a constructed line segment cannot be parallel with the midArc"
                      -- FIXME: this should happen only when we have inset completely, and the edge and midArc are anti-parallel.
                      PAntiCollinear -> if isAntiParallel (eToPL edge) midArc || edgeDistanceToLastPlacedLineSeg < distanceUntilEnd checkedFace
                                        then Just [makeFaceNoCheck finalSide firstArc midArcs lastArc]
                                        else Nothing
{-
                                          error $ "anticollinear should not have happened.\n"<> "edge: " <> show edge <> "\n"
                                                     <> "distanceBetweenSegs: " <> show distanceBetweenSegs <> "\n"
                                                     <> "foundLineSegs: " <> show foundLineSegs <> "\n"
                                                     <> "midArc: " <> show midArc <> "\n"
                                                     <> "intersection of midArc and lastPlacedLine: " <> show (plinesIntersectIn midArc lastPlacedLine) <> "\n"
                                                     <> "maybeFinalLine: " <> show maybeFinalSide <> "\n"
                                                     <> showInputs
-}
                      -- these are natural, when edge and midArc are parallel..
                      PAntiParallel -> Just [makeFaceNoCheck finalSide firstArc midArcs lastArc]
                      _ -> Just [makeFaceNoCheck finalSide firstArc midArcs lastArc]

    -- Recurse, so we get the remainder and line segments of the three sided n-gon left over.
    (twoSideSubLineSegs, _)
      | null foundLineSegs = noResult
      | isNothing maybeFinalSide = noResult
      | otherwise          = case plinesIntersectIn midArc lastPlacedLine of
                               PCollinear -> noResult
                               PAntiCollinear -> noResult
                               _ -> if firstArcEndsFarthest edge firstArc (head midArcs) lastArc
                                    then if noIntersection midArc (eToPL finalSide)
                                         -- our triangle is so small, two sides are considered colinear. abort.
                                         then noResult
                                         else addLineSegsToFace distanceBetweenSegs subInsets (makeFace 3 finalSide firstArc (slist []) midArc)
                                    else if noIntersection midArc (eToPL finalSide)
                                         -- our triangle is so small, two sides are considered colinear. abort.
                                         then noResult
                                         else addLineSegsToFace distanceBetweenSegs subInsets (makeFace 4 finalSide midArc (slist []) lastArc)
    lastPlacedLineSeg = last foundLineSegs
    lastPlacedLine = eToPL lastPlacedLineSeg
    edgeDistanceToLastPlacedLineSeg = max (distance (startPoint edge) (startPoint lastPlacedLineSeg))
                                          (distance (endPoint edge) (endPoint lastPlacedLineSeg))
    midArc = case midArcs of
               (Slist [oneArc] 1) -> oneArc
               (Slist _ _) -> error $ "evaluated midArc with the wrong insets of items.\n"
                                   <> "d: " <> show distanceBetweenSegs <> "\n"
                                   <> "n: " <> show insets <> "\n"
                                   <> "Face: " <> show face <> "\n"
    ----------------------------------------------
    -- functions only used by a three-sided n-gon.
    ----------------------------------------------
    twoSideRemainder     = if isJust maybeFinalSide && distanceBetweenSegs * fromIntegral linesToPlace /= distanceUntilEnd checkedFace
                           then Just [makeFaceNoCheck finalSide firstArc (slist []) lastArc]
                           else Nothing

-- | How many lines can be drawn onto a given Face, parallel to the face.
linesUntilEnd :: ℝ -> Face -> Fastℕ
linesUntilEnd distanceBetweenSegs face = floor (distanceUntilEnd face / distanceBetweenSegs)

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
reclaimContours lineSegSets
  -- every ring was reclaimed, and cleaned. return success.
  | all isJust reclaimedRings && all isJust cleanedContours = catMaybes cleanedContours
  | all isJust reclaimedRings = error $ "failed to clean a contour in rings: " <> show rings <> "\n" <> "input linsSegSets:\n" <> show lineSegSets <> "\n"
  | otherwise = error $ "failed to reclaim a ring in rings: " <> show rings <> "\n" <> "input linsSegSets:\n" <> show lineSegSets <> "\n"
  where
    cleanedContours = cleanContour <$> concatMap fromJust reclaimedRings
    reclaimedRings = reclaimRing <$> rings
    -- The input set of line segments has all of the line segments that cover a face in the same list.
    -- by transposing them, we get lists of rings around the object, rather than individually covered faces.
    -- by filtering lineSegSets we filter out any face that had no segments placed.
    -- by filtering for the length, we handle merge events.
    rings = filter (\a -> length a > 2) $ transpose $ filter (/= []) lineSegSets

-- | take a ring around N contours, and generate the contours.
-- FIXME: not handling split events yet.
reclaimRing :: [LineSeg] -> Maybe [Contour]
reclaimRing ring
  -- don't try to reclaim something that can not qualify as a 2d contour.
  | length ring < 3 = Nothing
  | otherwise = case filter (\(a,_) -> isNothing a) reclaimedContour of
                  [] -> if null reconstructedContours
                        then Nothing
                        else Just reconstructedContours
                  -- An odd number of breaks is either a floating point induced error, or an attempt to completely inset a contour.
                  [a] -> if length ring < 6
                         then Nothing
                         else error $ "found one break in ring: " <> show ring <> " at " <> show a <> "\n"
                  -- FIXME: this should actually be a split operation, or a segment removal operation.
                  _ -> Nothing
  where
    reconstructedContours = catMaybes [maybeMakePointContour $ nub $ fromJust . fst <$> reclaimedContour]
      where
        maybeMakePointContour points
          | length points < 3 = Nothing
          | otherwise = Just $ makePointContour points
    reclaimedContour = mapWithFollower recovery ring
      where
        -- detect if two line segments SHOULD end at the same point, and if they do, return the point.
        recovery :: LineSeg -> LineSeg -> (Maybe Point2, (LineSeg, LineSeg))
        recovery l1 l2
          | endPoint l1 == startPoint l2 = (Just $ endPoint l1, (l1, l2))
          | noIntersection (eToPL l1) (eToPL l2) = (Nothing, (l1, l2))
          -- FIXME: we should use intersection for line segments close to 90 degrees, and average for segments closest to parallel?
          | l1l2Distance <= l1l2DistanceErr = (Just $ fst $ pToEP $ fst $ intersectionOf (eToPL l1) (eToPL l2), (l1,l2))
          | otherwise = (Nothing, (l1,l2))
          where
            --- FIXME: magic number: 512
            l1l2DistanceErr = 512 * ulpVal (l1l2DistanceErrRaw
                                            <> pLineErrAtPPoint (eToPL l1) (eToPP $ endPoint l1)
                                            <> fuzzinessOfL (eToPL l1)
                                            <> pLineErrAtPPoint (eToPL l2) (eToPP $ startPoint l2)
                                            <> fuzzinessOfL (eToPL l2))
            (l1l2Distance, (_, _, l1l2DistanceErrRaw)) = distance2PP (eToPP $ endPoint l1, mempty) (eToPP $ startPoint l2, mempty)

-- | A face constructor that checks that a face is valid during construction.
makeFace :: Integer -> LineSeg -> (ProjectiveLine, PLine2Err) -> Slist (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> Face
makeFace breadCrumb edge firstArc arcs lastArc = res
  where
    res = checkFace $ Face edge firstArc arcs lastArc
    checkFace inFace@(Face myEdge myFirstArc (Slist myMidArcs _) myLastArc)
      | all isIntersection intersections = inFace
      | otherwise = error $ "Tried to generate a degenerate face: "
                         <> show breadCrumb <> "\n"
                         <> show inFace <> "\n"
                         <> intercalate "\n" (show <$> intersections) <> "\n"
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
