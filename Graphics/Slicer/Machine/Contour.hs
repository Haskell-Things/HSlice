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

module Graphics.Slicer.Machine.Contour (cleanContour, shrinkContour, expandContour) where

import Prelude (length, (>), ($), otherwise, Eq, (<>), show, error, (==), (&&), fst, Bool(True, False), last, init, (++), (<), Show)

import Data.List (null, foldl)

import Data.Maybe (Maybe(Just, Nothing), catMaybes, maybeToList)

import Data.Either (fromRight)

import Graphics.Slicer.Math.Definitions (Point2, Contour(PointSequence), addPoints, mapWithNeighbors)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg), makeLineSegsLooped, pointsFromLineSegs, lineSegFromEndpoints, endpoint)

import Graphics.Slicer.Math.PGA (combineConsecutiveLineSegs, PIntersection(IntersectsIn, PColinear, PParallel), plinesIntersectIn, translatePerp, eToPLine2, pToEPoint2, angleBetween)

import Graphics.Slicer.Definitions(ℝ)

---------------------------------------------------------------
-------------------- Contour Optimizer ------------------------
---------------------------------------------------------------

-- | Contour optimizer. Merges line segments that are colinear.
cleanContour :: Contour -> Maybe Contour
cleanContour (PointSequence points)
  | length (cleanPoints points) > 2 = Just $ PointSequence $ cleanPoints points
  | otherwise = error $ "asked to clean a contour with " <> show (length points) <> "points: " <> show points <> "\n"
  where
    cleanPoints :: [Point2] -> [Point2]
    cleanPoints pts
      | null pts = []
      | length pts > 2 = fromRight (error "no lines left") $ pointsFromLineSegs $ combineConsecutiveLineSegs $ makeLineSegsLooped pts
      | otherwise = [] 

---------------------------------------------------------------
-------------------- Contour Modifiers ------------------------
---------------------------------------------------------------

data Direction =
    Inward
  | Outward
  deriving (Eq, Show)

-- | Generate a new contour that is a given amount smaller than the given contour.
-- FIXME: what about other contours inside of this contour, or walling off of a section, creating two contours?
shrinkContour :: ℝ -> [Contour] -> Contour -> Maybe Contour
shrinkContour amount _ contour = fst $ modifyContour amount contour Inward

-- | Generate a new contour that is a given amount larger than the given contour.
-- FIXME: what about other contours outside of this contour, or walling off of a section, creating two contours?
expandContour :: ℝ -> [Contour] -> Contour -> Maybe Contour
expandContour amount _ contour = fst $ modifyContour amount contour Outward

-- | Generate a new contour that is a given amount larger/smaller than the given contour.
modifyContour :: ℝ -> Contour -> Direction -> (Maybe Contour,[Contour])
modifyContour pathWidth (PointSequence contourPoints) direction
  | null contourPoints = error "tried to modify an empty contour."
  | null foundContour  = (Nothing, [])
  | otherwise          = (Just $ PointSequence $ fromRight (error "found contour is empty") $ pointsFromLineSegs foundContour,[])
  where
    -- FIXME: implement me. we need this to handle further interior contours, and only check against the contour they are inside of.
    foundContour
      | length contourPoints > 2 = catMaybes maybeLineSegs
      | otherwise = error $ "tried to modify a contour with too few points: " <> show (length contourPoints) <> "\n"
      where
        -- FIXME: if the currently drawn line hits the current or previous contour on a line other than the line before or after the parent, you have a pinch. shorten the current line.
        -- FIXME: draw a line before, and after the intersection. return two lines?
        maybeLineSegs = mapWithNeighbors findLineSeg $ removeDegenerates $ makeLineSegsLooped contourPoints
        -- Remove sequential parallel lines, colinear sequential lines, and lines that are too close to parallel.
        removeDegenerates :: [LineSeg] -> [LineSeg]
        removeDegenerates lns
          | length res == length lns = res
          | otherwise                = removeDegenerates res
          where
            res = removeDegenerateEnds $ foldl concatDegenerates [] lns
            concatDegenerates xs x
              | null xs = [x]
              | isDegenerate (inwardAdjust (last xs)) (inwardAdjust x) = init xs ++ maybeToList (combineLineSegs (last xs) x)
              | otherwise = xs ++ [x]
            removeDegenerateEnds :: [LineSeg] -> [LineSeg]
            removeDegenerateEnds  []      = []
            removeDegenerateEnds  [l1]    = [l1]
            removeDegenerateEnds  (l1:ls)
              | length ls > 1 = if isDegenerate (inwardAdjust (last ls)) (inwardAdjust l1) then init ls ++ maybeToList (combineLineSegs (last ls) l1) else l1:ls
              | otherwise = l1:ls
            -- Combine lines (p1 -- p2) (p3 -- p4) to (p1 -- p4). We really only want to call this
            -- if p2 == p3 and the lines are really close to parallel
            combineLineSegs :: LineSeg -> LineSeg -> Maybe LineSeg
            combineLineSegs l1@(LineSeg p _) l2@(LineSeg p1 s1) = if endpoint l2 == p -- If line 2 ends where line 1 begins:
                                                         then Nothing -- handle a contour that loops back on itsself.
                                                         else Just $ fromRight (error $ "cannot combine lines: " <> show l1 <> "\n" <> show l2 <> "\n") $ lineSegFromEndpoints p (addPoints p1 s1)
            isDegenerate pl1 pl2
              | angleBetween pl1 pl2 < (-0.999) = True
              | angleBetween pl1 pl2 >   0.999  = True
              | otherwise = case plinesIntersectIn pl1 pl2 of
                              PParallel -> True
                              PColinear -> True
                              _         -> False
        inwardAdjust l1 = translatePerp (eToPLine2 l1) (if direction == Inward then pathWidth else (-pathWidth))
        findLineSeg :: LineSeg -> LineSeg -> LineSeg -> Maybe LineSeg
        findLineSeg previousln ln nextln
          -- The ideal case.
          | isIntersection previousln ln &&
            isIntersection ln nextln        = Just $ fromRight ( error "failed to construct intersection") $ lineSegFromEndpoints (intersectionPoint (inwardAdjust previousln) (inwardAdjust ln)) (intersectionPoint (inwardAdjust ln) (inwardAdjust nextln))
          | otherwise = error $ "no intersection?\n" <> show (isIntersection previousln ln) <> "\n" <> show (isIntersection ln nextln) <> "\n" <> show previousln <> "\n" <> show ln <> "\n" <> show nextln <> "\n"
          where
            isIntersection l1 l2 = case plinesIntersectIn (inwardAdjust l1) (inwardAdjust l2) of
                                     IntersectsIn _ -> True
                                     _              -> False
            intersectionPoint pl1 pl2 = case plinesIntersectIn pl1 pl2 of
                                          IntersectsIn p2 -> {- let (Point2 (x,y)) = pToEPoint2 p2
                                                             in
                                                               if x<0 || y<0
                                                               then
                                                                 error $ "outside field!\nresult: " <> show p2 <> "\npline 1: " <> show pl1
                                                                 <> "\npline 2: " <> show pl2
                                                                 <> "\nEvaluating line intersections between:\nFirst: " <> show previousln
                                                                 <> "\nSecond: " <> show ln
                                                                 <> "\nThird: " <> show nextln
                                                                 <> "\n" <> show (eToPLine2 previousln)
                                                                 <> "\n" <> show (inwardAdjust previousln)
                                                                 <> "\n" <> show (angleBetween (eToPLine2 previousln) (eToPLine2 ln))
                                                                 <> "\n" <> show (angleBetween (inwardAdjust previousln) (inwardAdjust ln))
                                                                 <> "\n" <> show (eToPLine2 ln)
                                                                 <> "\n" <> show (inwardAdjust ln)
                                                                 <> "\n" <> show (angleBetween (eToPLine2 ln) (eToPLine2 nextln))
                                                                 <> "\n" <> show (angleBetween (inwardAdjust ln) (inwardAdjust nextln))
                                                                 <> "\n" <> show (eToPLine2 nextln)
                                                                 <> "\n" <> show (inwardAdjust nextln)
                                                                 <> "\n" <> show direction
                                                                 <> "\n" <> show contourPoints
                                                                 <> "\n"
                                                               else  -} pToEPoint2 p2
                                          a               -> error $ "impossible result!\nresult: " <> show a <> "\npline 1: " <> show pl1
                                                             <> "\npline 2: " <> show pl2
                                                             <> "\nEvaluating line intersections between:\nFirst: " <> show previousln
                                                             <> "\nSecond: " <> show ln
                                                             <> "\nThird: " <> show nextln <> "\n"<> show (inwardAdjust previousln)
                                                             <> "\n" <> show (eToPLine2 previousln)
                                                             <> "\n" <> show (inwardAdjust ln)
                                                             <> "\n" <> show (eToPLine2 ln)
                                                             <> "\n" <> show (inwardAdjust nextln)
                                                             <> "\n" <> show (eToPLine2 nextln)
                                                             <> "\n" <> show direction
                                                             <> "\n" <> show contourPoints
                                                             <> "\n"
