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

import Prelude (length, (>), ($), otherwise, (<$>), Int, Eq, (<>), show, error, (==), negate, (.), (*), (+), take, drop, cycle, (-), (&&), fst, (<), (/), Bool)

import Data.List (nub, null, zipWith3)

import Data.Maybe (Maybe(Just, Nothing), catMaybes, isJust, fromJust)

import Control.Parallel.Strategies (withStrategy, parList, rpar)

import Control.Parallel (par, pseq)

import Graphics.Slicer.Math.Definitions (Point2, Contour(PointSequence), distance, roundPoint2)

import Graphics.Slicer.Math.Line (Line(Line), Intersection(IntersectsAt, HitEndpointL1, NoIntersection, Collinear, Parallel), makeLinesLooped, lineIntersection, pointsFromLines, combineConsecutiveLines, lineSlope, flipLine, pointSlopeLength, combineLines, endpoint, lineFromEndpoints)

import Graphics.Slicer.Math.Contour (outerPerimeterPoint, innerPerimeterPoint, lineToOutsideContour)

import Graphics.Slicer.Definitions(ℝ)

---------------------------------------------------------------
-------------------- Contour Optimizer ------------------------
---------------------------------------------------------------

-- Contour optimizer. Merges small line fragments into larger ones.
cleanContour :: Contour -> Maybe Contour
cleanContour (PointSequence points)
  | length (cleanPoints points) > 2 = Just $ PointSequence $ cleanPoints points
  | otherwise = Nothing -- error $ "asked to clean a contour with " <> show (length points) <> "points: " <> show points <> "\n"
  where
    cleanPoints :: [Point2] -> [Point2]
    cleanPoints pts
      | null pts = []
      | length pointsRemaining > 2 = pointsFromLines $ combineConsecutiveLines $ lines
      | otherwise = [] 
        where
          lines = makeLinesLooped pointsRemaining
          pointsRemaining = nub $ roundPoint2 <$> pts

---------------------------------------------------------------
-------------------- Contour Modifiers ------------------------
---------------------------------------------------------------

-- like map, only with previous, current, and next item, and wrapping around so the first entry gets the last entry as previous, and vica versa.
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
  deriving (Eq)

-- reduce a contour by a given amount.
shrinkContour :: ℝ -> [Contour] -> Contour -> Maybe Contour
shrinkContour amount allContours contour = fst $ modifyContour amount allContours contour Inward

-- increase a contour by a given amount.
expandContour :: ℝ -> [Contour] -> Contour -> Maybe Contour
expandContour amount allContours contour = fst $ modifyContour amount allContours contour Outward

-- FIXME: implement this.
-- FIXME: if the currently drawn line hits the current or previous contour on a line other than the line before or after the parent, you have a pinch. shorten the current line.
-- FIXME: draw a line before, and after the intersection. the line after is the first line of the new contour, the line before is still this contour.
-- Optimization: only check non-neighbor lines when the angles add up to a certain amount? looking for curling back. anything over ~180 degrees, relative to the slope of this line.
findExtraContours :: Contour -> Contour -> [Contour]
findExtraContours _ _ = []

-- Add one contour inside or outside of a given contour, in a naieve fashion.
modifyContour :: ℝ -> [Contour] -> Contour -> Direction -> (Maybe Contour,[Contour])
modifyContour pathWidth allContours contour@(PointSequence contourPoints) direction = if null foundContour then (Nothing, []) else (Just $ PointSequence $ pointsFromLines foundContour,[])
  where
    -- FIXME: implement me. we need this so we can handle further interior contours, and only check against the contour they are inside of.
    foundContour
      | (length contourPoints) > 2 = catMaybes $ mapWithNeighbors (findLine allContours) $ (makeLinesLooped contourPoints)
      | otherwise = error $ "tried to modify a contour with too few points: " <> show (length contourPoints) <> "\n"
      where
        -- FIXME: if the currently drawn line hits the current or previous contour on a line other than the line before or after the parent, you have a pinch. shorten the current line.
        -- Optimization: only check non-neighbor lines when the angles add up to a certain amount? looking for curling back. anything over ~180 degrees, relative to the slope of this line.
        findLine :: [Contour] -> Line -> Line -> Line -> Maybe Line
        findLine contours previousln ln@(Line _ m) nextln
          -- The ideal case.
          | isJustPositive (lengthToIntersection ln previousln)
            && isJustPositive (lengthToIntersection ln nextln)   = Just $ flipLine midToStart `combineLines` midToEnd
          | isJustZero (lengthToIntersection ln previousln)
            && isJustPositive (lengthToIntersection ln nextln)   = Just $ midToEnd
          | isJustPositive (lengthToIntersection ln previousln)
            && isJustZero (lengthToIntersection ln nextln)       = Just $ flipLine midToStart
          | isJustZero (lengthToIntersection ln previousln)
            && isJustZero (lengthToIntersection ln nextln)       = Nothing
          | isJust (lengthToIntersection ln previousln)          =
            case lineIntersection (rayToEnd ln) (rayToStart nextln) of
              NoIntersection -> case lineIntersection (rayToStart ln) (rayToStart nextln) of
                           IntersectsAt _ p2 -> if distance (perimeterPoint ln) p2 < lineLength midToStart
                                                then Just $ lineFromEndpoints p2 (endpoint midToStart)
                                                else Nothing
                           _                 -> Nothing
              _              -> Nothing
          | isJust (lengthToIntersection ln nextln)              =
            case lineIntersection (rayToStart ln) (rayToEnd previousln) of
              NoIntersection -> case lineIntersection (rayToEnd ln) (rayToEnd previousln) of
                           IntersectsAt _ p2 -> if distance (perimeterPoint ln) p2 < lineLength midToEnd
                                                then Just $ lineFromEndpoints p2 (endpoint midToEnd)
                                                else Nothing
                           _                 -> Nothing
              _              -> Nothing
          | otherwise = Nothing
          where
            midToEnd, midToStart :: Line
            midToEnd   = pointSlopeLength (perimeterPoint ln) (lineSlope m) (fromJust $ lengthToIntersection ln nextln)
            midToStart = pointSlopeLength (perimeterPoint ln) (lineSlope m) (negate $ fromJust $ lengthToIntersection ln previousln)
            isJustZero, isJustPositive :: Maybe ℝ -> Bool
            isJustZero a = isJust a && fromJust a == 0
            isJustPositive a = isJust a && fromJust a > 0
        lineLength ln@(Line p _) = distance p $ endpoint ln
        perimeterPoint :: Line -> Point2
        perimeterPoint ln
          | direction == Inward = innerPerimeterPoint pathWidth contour ln
          | otherwise           = outerPerimeterPoint pathWidth contour ln
        -- line segments for a hypothetical line, without being shortened yet.
        rayToEnd ln = rawMidToEdge contour ln
        rayToStart ln = flipLine $ rawMidToEdge contour ln
        rawMidToEdge c ln@(Line _ m) = lineToOutsideContour c (pathWidth * 2) (lineSlope m) (perimeterPoint ln)
        -- get the length to where these lines intersect, assuming they are pathWidth away from the lines themselves.
        lengthToIntersection :: Line -> Line -> Maybe ℝ
        lengthToIntersection l1 l2
          | otherwise = case lineIntersection (rayToEnd l1) (rayToStart l2) of
                          IntersectsAt _ p2 -> foundDistance p2
                          HitEndpointL1 _   -> Just 0
                          NoIntersection    -> Nothing
                          Parallel          -> Nothing
                          Collinear         -> Just $ (lineLength l1 / 2)
                          a                 -> error $ "insane result: " <> show a <>"\nno intersection on contour:\n" <> (show contour) <> "\n" <> show l1 <> " -> " <> show (rayToEnd l1) <> "\n" <> show l2 <> " -> " <> show (rayToStart l2) <> "\n"
          where
            foundDistance p2 = if rawDistance p2 > 0 then Just (rawDistance p2) else Nothing
            rawDistance p2 = distance (perimeterPoint l1) p2
