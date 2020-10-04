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

import Prelude (length, (>), ($), otherwise, Int, Eq, (<>), show, error, (==), (.), (+), take, drop, cycle, (-), (&&), fst, Bool(True, False), last, init, (++), (<), Show)

import Data.List (null, zipWith3, foldl)

import Data.Maybe (Maybe(Just, Nothing), catMaybes)

import Control.Parallel.Strategies (withStrategy, parList, rpar)

import Control.Parallel (par, pseq)

import Graphics.Slicer.Math.Definitions (Point2, Contour(PointSequence), addPoints)

import Graphics.Slicer.Math.Line (Line(Line), makeLinesLooped, pointsFromLines, lineFromEndpoints)

import Graphics.Slicer.Math.PGA (PLine2(PLine2), combineConsecutiveLines, Intersection(IntersectsAt, Collinear, Parallel), plinesIntersectAt, translatePerp, eToPLine2)

import Graphics.Slicer.Math.GeometricAlgebra((⋅), scalarIze)

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
      | length pts > 2 = pointsFromLines $ combineConsecutiveLines $ makeLinesLooped pts
      | otherwise = [] 

---------------------------------------------------------------
-------------------- Contour Modifiers ------------------------
---------------------------------------------------------------

-- | like map, only with previous, current, and next item, and wrapping around so the first entry gets the last entry as previous, and vica versa.
mapWithNeighbors :: (a -> a -> a -> b) -> [a] -> [b]
mapWithNeighbors  f l =
    let
      rotateList :: Int -> [a] -> [a]
      rotateList n list = take (length list + 1) . drop n $ cycle list
      x = rotateList (length l - 1) l
      z = rotateList 1 l
    in
      withStrategy (parList rpar) $ x `par` z `pseq` zipWith3 f x l z

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
modifyContour pathWidth (PointSequence contourPoints) direction = if null foundContour then (Nothing, []) else (Just $ PointSequence $ pointsFromLines foundContour,[])
  where
    -- FIXME: implement me. we need this to handle further interior contours, and only check against the contour they are inside of.
    foundContour
      | length contourPoints > 2 = catMaybes maybeLines
      | otherwise = error $ "tried to modify a contour with too few points: " <> show (length contourPoints) <> "\n"
      where
        -- FIXME: if the currently drawn line hits the current or previous contour on a line other than the line before or after the parent, you have a pinch. shorten the current line.
        -- FIXME: draw a line before, and after the intersection. return two lines?
        maybeLines = mapWithNeighbors findLine $ removeDegenerates $ makeLinesLooped contourPoints
        -- Remove sequential parallel lines, colinear sequential lines, and lines that are too close to parallel.
        removeDegenerates :: [Line] -> [Line]
        removeDegenerates lns
          | length res == length lns = res
          | otherwise                = removeDegenerates res
          where
            res = removeDegenerateEnds $ foldl concatDegenerates [] lns
            concatDegenerates xs x
              | null xs = [x]
              | isDegenerate (inwardAdjust (last xs)) (inwardAdjust (x)) = (init xs) ++ [combineLines (last xs) x]
              | otherwise = xs ++ [x]
            removeDegenerateEnds :: [Line] -> [Line]
            removeDegenerateEnds  []      = []
            removeDegenerateEnds  [l1]    = [l1]
            removeDegenerateEnds  (l1:ls)
              | length ls > 1 = if isDegenerate (inwardAdjust (last ls)) (inwardAdjust l1) then init ls ++ [combineLines (last ls) l1] else l1:ls
              | otherwise = l1:ls
            -- Combine lines (p1 -- p2) (p3 -- p4) to (p1 -- p4). We really only want to call this
            -- if p2 == p3 and the lines are really close to parallel
            combineLines :: Line -> Line -> Line
            combineLines (Line p _) (Line p1 s1) = lineFromEndpoints p (addPoints p1 s1)
            isDegenerate pl1@(PLine2 gvec1) pl2@(PLine2 gvec2)
              | (fst . scalarIze $ gvec1 ⋅ gvec2) < (-0.9999) = True
              | (fst . scalarIze $ gvec1 ⋅ gvec2) > (0.9999) = True
              | otherwise = case plinesIntersectAt pl1 pl2 of
                              Parallel  -> True
                              Collinear -> True
                              _         -> False
        inwardAdjust l1 = translatePerp (eToPLine2 l1) (if direction == Inward then pathWidth else (-pathWidth))
        findLine :: Line -> Line -> Line -> Maybe Line
        findLine previousln ln nextln
          -- The ideal case.
          | isIntersection previousln ln &&
            isIntersection ln nextln        = Just $ lineFromEndpoints (intersectionPoint (inwardAdjust previousln) (inwardAdjust ln)) (intersectionPoint (inwardAdjust ln) (inwardAdjust nextln))
          | otherwise = error $ "no intersection?\n" <> (show $ isIntersection previousln ln) <> "\n" <> (show $ isIntersection ln nextln) <> "\n" <> show previousln <> "\n" <> show ln <> "\n" <> show nextln <> "\n"
          where
            isIntersection l1 l2 = case plinesIntersectAt (inwardAdjust l1) (inwardAdjust l2) of
                                     IntersectsAt _ -> True
                                     _              -> False
            intersectionPoint pl1 pl2 = case plinesIntersectAt pl1 pl2 of
                                          IntersectsAt p2 -> p2
                                          a               -> error $ "impossible result!\nresult: " <> show a <> "\npline 1: " <> show pl1
                                                             <> "\npline 2: " <> show pl2
                                                             <> "\nEvaluating line intersections between:\nFirst: " <> show previousln
                                                             <> "\nSecond: " <> show ln
                                                             <> "\nThird: " <> show nextln <> "\n"<> show (inwardAdjust previousln)
                                                             <> "\n" <> show (eToPLine2 previousln)
                                                             <> "\n"<> show (inwardAdjust ln)
                                                             <> "\n" <> show (eToPLine2 ln)
                                                             <> "\n"<> show (inwardAdjust nextln)
                                                             <> "\n" <> show (eToPLine2 nextln)
                                                             <> "\n" <> show direction
                                                             <> "\n" <> show contourPoints
                                                             <> "\n"
