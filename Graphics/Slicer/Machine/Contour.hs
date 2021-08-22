{- ORMOLU_DISABLE -}
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

import Prelude ((>), ($), otherwise, Eq, (<>), show, error, (==), (&&), Bool(True, False), (++), (<), Show)

import Data.Either (fromRight)

import Data.List (null, foldl')

import Data.List.Extra (unsnoc)

import Data.Maybe (Maybe(Just, Nothing), catMaybes, maybeToList)

import Graphics.Slicer.Math.Contour (lineSegsOfContour, makeLineSegContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg, mapWithNeighbors)

import Graphics.Slicer.Math.Line (lineSegFromEndpoints, combineLineSegs)

import Graphics.Slicer.Math.PGA (combineConsecutiveLineSegs, PIntersection(IntersectsIn, PCollinear, PParallel, PAntiParallel), plinesIntersectIn, translatePerp, eToPLine2, pToEPoint2, angleBetween)

import Graphics.Slicer.Definitions(ℝ)

---------------------------------------------------------------
-------------------- Contour Optimizer ------------------------
---------------------------------------------------------------

-- | Contour optimizer. Merges line segments that are collinear.
cleanContour :: Contour -> Maybe Contour
cleanContour contour = Just $ makeLineSegContour $ combineConsecutiveLineSegs $ lineSegsOfContour contour

---------------------------------------------------------------
-------------------- Contour Modifiers ------------------------
---------------------------------------------------------------

data Direction =
    Inward
  | Outward
  deriving (Eq, Show)

-- | Generate a new contour that is a given amount smaller than the given contour.
-- WARNING: uses unsafe modifyContour.
shrinkContour :: ℝ -> [Contour] -> Contour -> Maybe Contour
shrinkContour amount _ contour = modifyContour amount contour Inward

-- | Generate a new contour that is a given amount larger than the given contour.
-- WARNING: uses unsafe modifyContour.
expandContour :: ℝ -> [Contour] -> Contour -> Maybe Contour
expandContour amount _ contour = modifyContour amount contour Outward

-- | Generate a new contour that is a given amount larger/smaller than the given contour.
-- WARNING: unsafe, generating results that may collide into other contours inside of this contour, or may wall off of a section, creating what should be two contours.
modifyContour :: ℝ -> Contour -> Direction -> Maybe Contour
modifyContour pathWidth contour direction
  | null foundContour  = Nothing
  | otherwise          = Just $ makeLineSegContour foundContour
  where
    -- FIXME: implement me. we need this to handle further interior contours, and only check against the contour they are inside of.
    foundContour = catMaybes maybeLineSegs
      where
        -- FIXME: if the currently drawn line hits the current or previous contour on a line other than the line before or after the parent, you have a pinch. shorten the current line.
        -- FIXME: draw a line before, and after the intersection. return two lines?
        maybeLineSegs = mapWithNeighbors findLineSeg $ removeDegenerates $ lineSegsOfContour contour
        -- Remove sequential parallel lines, collinear sequential lines, and lines that are too close to parallel.
        removeDegenerates :: [LineSeg] -> [LineSeg]
        removeDegenerates lns = removeDegenerateEnds $ foldl' concatDegenerates [] lns
          where
            removeDegenerateEnds :: [LineSeg] -> [LineSeg]
            removeDegenerateEnds inSegs = case inSegs of
                                            [] -> []
                                            [l1] -> [l1]
                                            [l1,l2] -> [l1,l2]
                                            (firstSeg:moreSegs) -> case unsnoc moreSegs of
                                                                     Nothing -> error "impossible."
                                                                     (Just (middleSegs,lastSeg)) -> if isDegenerate (inwardAdjust lastSeg) (inwardAdjust firstSeg)
                                                                                                    then middleSegs ++ maybeToList (combineLineSegs lastSeg firstSeg)
                                                                                                    else inSegs
            concatDegenerates :: [LineSeg] -> LineSeg -> [LineSeg]
            concatDegenerates inSegs oneSeg = case unsnoc inSegs of
                                       Nothing -> [oneSeg]
                                       (Just (middleSegs,lastSeg)) -> middleSegs ++ if isDegenerate (inwardAdjust lastSeg) (inwardAdjust oneSeg)
                                                                                    then maybeToList (combineLineSegs lastSeg oneSeg)
                                                                                    else [lastSeg,oneSeg]
            isDegenerate pl1 pl2
              | angleBetween pl1 pl2 < (-0.999999) = True
              | angleBetween pl1 pl2 >   0.999999  = True
              | plinesIntersectIn pl1 pl2  == PParallel = True
              | plinesIntersectIn pl1 pl2  == PAntiParallel = True
              | plinesIntersectIn pl1 pl2  == PCollinear = True
              | otherwise = False
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
                                     _other         -> False
            intersectionPoint pl1 pl2 = case plinesIntersectIn pl1 pl2 of
                                          IntersectsIn p2 -> pToEPoint2 p2
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
                                                             <> "\n" <> show contour
                                                             <> "\n"
