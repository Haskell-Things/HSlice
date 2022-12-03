{- ORMOLU_DISABLE -}
{-
 - Copyright 2022 Julia Longtin
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

{- Purpose of this file: to hold the logic and routines responsible for checking for intersections with contours, or portions of contours. -}

module Graphics.Slicer.Math.ContourIntersections (getMotorcycleSegSetIntersections, getMotorcycleContourIntersections, contourIntersectionCount, getLineContourIntersections) where

import Prelude (Either(Left,Right), error, otherwise, show, (&&), (<>), ($), (<$>), (/=), zip, Int, (<), (*), length, odd)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, isJust, fromJust)

import Data.List as L (filter)

import Slist.Type (Slist)

import Slist (len, slist)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg, Point2, mapWithNeighbors, startPoint, distance, lineSegsOfContour, endPoint, fudgeFactor, makeLineSeg)

import Graphics.Slicer.Math.Lossy (pToEPoint2)

import Graphics.Slicer.Math.PGA (PIntersection(IntersectsIn, PParallel, PAntiParallel, PCollinear, PAntiCollinear), ProjectivePoint, Intersection(HitEndPoint, HitStartPoint, NoIntersection), ProjectiveLine, ProjectiveLine2, PLine2Err, intersectsWithErr, normalizeL, outputIntersectsLineSeg)

import Graphics.Slicer.Math.Skeleton.Definitions (Motorcycle(Motorcycle))

-- | Get all possible intersections between the motorcycle and the given list of segments.
-- Filters out the input and output segment of the motorcycle.
getMotorcycleSegSetIntersections :: Motorcycle -> [LineSeg] -> [(LineSeg, Either Point2 ProjectivePoint)]
getMotorcycleSegSetIntersections m@(Motorcycle (inSeg, outSeg) _ _) segs = stripInSegOutSeg $ catMaybes $ mapWithNeighbors filterIntersections $ shortCircuit $ zip bufferedLineSegs $ mightIntersect <$> bufferedLineSegs
  where
    -- since this is a list of segments, we terminate the list with Nothings, so that the saneIntersections pattern matching logic can deal with "there is no neighbor, but i hit a start/end point"
    bufferedLineSegs :: [Maybe LineSeg]
    bufferedLineSegs = Nothing : (Just <$> segs) <> [Nothing]
    mightIntersect :: Maybe LineSeg -> Maybe (Either Intersection PIntersection)
    mightIntersect maybeSeg = case maybeSeg of
                                Nothing -> Nothing
                                (Just seg) -> Just $ outputIntersectsLineSeg m seg
    shortCircuit :: [(Maybe LineSeg, Maybe (Either Intersection PIntersection))] -> [Maybe (LineSeg, Either Intersection PIntersection)]
    shortCircuit items = shortCircuitItem <$> items
      where
        shortCircuitItem (Nothing, Nothing) = Nothing
        shortCircuitItem (Just seg, Just intersection) = Just (seg, intersection)
        shortCircuitItem item = error $ "cannot short circuit item: " <> show item <> "\n"
    stripInSegOutSeg :: [(LineSeg, Either Point2 ProjectivePoint)] -> [(LineSeg, Either Point2 ProjectivePoint)]
    stripInSegOutSeg = L.filter fun
      where
        -- make sure neither of these segments are inSeg or outSeg
        fun (seg,_) = seg /= inSeg && seg /= outSeg

-- | Get all possible intersections between the motorcycle and the contour.
-- Filters out the input and output segment of the motorcycle.
getMotorcycleContourIntersections :: Motorcycle -> Contour -> [(LineSeg, Either Point2 ProjectivePoint)]
getMotorcycleContourIntersections m@(Motorcycle (inSeg, outSeg) _ _) c = stripInSegOutSeg $ catMaybes $ mapWithNeighbors filterIntersections $ openCircuit $ zip contourLines $ outputIntersectsLineSeg m <$> contourLines
  where
    openCircuit v = Just <$> v
    contourLines = lineSegsOfContour c
    stripInSegOutSeg :: [(LineSeg, Either Point2 ProjectivePoint)] -> [(LineSeg, Either Point2 ProjectivePoint)]
    stripInSegOutSeg = L.filter fun
      where
        -- filter out inSeg and outSeg outSeg
        fun (seg,_) = seg /= inSeg && seg /= outSeg

-- | return the number of intersections with a given contour when traveling in a straight line from the beginning of the given line segment to the end of the line segment.
-- Not for use when line segments can overlap or are collinear with one of the line segments that are a part of the contour.
contourIntersectionCount :: Contour -> (Point2, Point2) -> Int
contourIntersectionCount contour (start, end) = len $ getIntersections contour (start, end)
  where
    getIntersections :: Contour -> (Point2, Point2) -> Slist (LineSeg, Either Point2 ProjectivePoint)
    getIntersections c (pt1, pt2) = slist $ catMaybes $ mapWithNeighbors filterIntersections $ openCircuit $ zip (lineSegsOfContour contour) $ intersectsWithErr targetSeg <$> segs
      where
        segs :: [Either LineSeg (ProjectiveLine, PLine2Err)]
        segs =  Left <$> lineSegsOfContour c
        targetSeg :: Either LineSeg (ProjectiveLine, PLine2Err)
        targetSeg = Left $ makeLineSeg pt1 pt2
        openCircuit v = Just <$> v

-- | Get the intersections between a Line and a contour as a series of points. always returns an even number of intersections.
getLineContourIntersections :: (ProjectiveLine2 a) => (a, PLine2Err) -> Contour -> [Point2]
getLineContourIntersections (line, lineErr) c
  | odd $ length res = error $ "odd number of transitions: " <> show (length res) <> "\n" <> show c <> "\n" <> show line <> "\n" <> show res <> "\n"
  | otherwise = res
  where
    res = getPoints $ catMaybes $ mapWithNeighbors filterIntersections $ openCircuit $ zip (lineSegsOfContour c) $ intersectsWithErr targetLine <$> segs
    openCircuit v = Just <$> v
    segs :: [Either LineSeg (ProjectiveLine, PLine2Err)]
    segs =  Left <$> lineSegsOfContour c
    -- FIXME: why do we have to use a concrete type here?
    targetLine :: Either LineSeg (ProjectiveLine, PLine2Err)
    targetLine = Right (nLine, lineErr <> nLineErr)
    (nLine, nLineErr) = normalizeL line
    getPoints :: [(LineSeg, Either Point2 ProjectivePoint)] -> [Point2]
    getPoints vs = getPoint <$> vs
      where
        getPoint (_, Left v) = v
        getPoint (_, Right v) = pToEPoint2 v

-- | filter the intersections given.
-- The purpose of this function is to ensure we only count the crossing of a line (segment) across a contour's edge more than once. so if it hits a sttartpoint, make sure we don't count the endpoint.. etc.
-- FIXME: does not take into account (anti)collinear line segments correctly.
filterIntersections :: Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Point2 ProjectivePoint)
filterIntersections  _ (Just (seg, Right (IntersectsIn p _)))   _ = Just (seg, Right p)
filterIntersections  _ (Just (_  , Left  (NoIntersection _ _))) _ = Nothing
filterIntersections  _ (Just (_  , Right PParallel))            _ = Nothing
filterIntersections  _ (Just (_  , Right PAntiParallel))        _ = Nothing
-- when we hit a end -> start -> end, look at the distance to tell where we hit.
filterIntersections (Just (seg1, Left (HitEndPoint   l1 ))) (Just (seg2, Left (HitStartPoint _ ))) (Just (seg3 , Left (HitEndPoint   l2)))
 | distance (endPoint seg1) (startPoint seg2) < fudgeFactor*15 = Just (seg2, Left $ endPoint l1)
 | distance (startPoint seg2) (endPoint seg3) < fudgeFactor*15 = Just (seg2, Left $ endPoint l2)
 | otherwise = error "wtf"
 -- only count the first start point, when going in one direction..
filterIntersections  _                                     (Just (seg , Left (HitStartPoint _ ))) (Just (_    , Left (HitEndPoint     l1))) = Just (seg, Left $ endPoint l1)
filterIntersections (Just (_ , Left (HitStartPoint  _  ))) (Just (_   , Left (HitEndPoint   _ )))  _                                        = Nothing
-- and only count the first start point, when going in the other direction.
filterIntersections (Just (_ , Left (HitEndPoint    l1 ))) (Just (seg , Left (HitStartPoint _ )))  _                                        = Just (seg, Left $ endPoint l1)
filterIntersections  _                                     (Just (_   , Left (HitEndPoint   _ ))) (Just (_    , Left (HitStartPoint _   ))) = Nothing
-- Ignore the end and start point that comes before / after a collinear section.
filterIntersections (Just (_, Right PCollinear          )) (Just (_   , Left (HitStartPoint _ )))  _                                        = Nothing
filterIntersections _                                      (Just (_   , Left (HitEndPoint   _ ))) (Just (_    , Right PCollinear         )) = Nothing
filterIntersections (Just (_, Right PCollinear          )) (Just (_   , Left (HitEndPoint   _ )))  _                                        = Nothing
filterIntersections _                                      (Just (_   , Left (HitStartPoint _ ))) (Just (_    , Right PCollinear         )) = Nothing
-- Ignore the end and start point that comes before / after an anticollinear section.
filterIntersections (Just (_, Right PAntiCollinear      )) (Just (_   , Left (HitStartPoint _ )))  _                                        = Nothing
filterIntersections _                                      (Just (_   , Left (HitEndPoint   _ ))) (Just (_    , Right PAntiCollinear     )) = Nothing
filterIntersections (Just (_, Right PAntiCollinear      )) (Just (_   , Left (HitEndPoint   _ )))  _                                        = Nothing
filterIntersections _                                      (Just (_   , Left (HitStartPoint _ ))) (Just (_    , Right PAntiCollinear     )) = Nothing
-- FIXME: we should return the (anti-)collinear segments so they can be stitched out, not just ignore them.
filterIntersections _                                      (Just (_   , Right PCollinear       ))  _                                        = Nothing
filterIntersections _                                      (Just (_   , Right PAntiCollinear   ))  _                                        = Nothing
-- And now handle the end segments, where there is nothing on the other side.
-- FIXME: these can't all be endpoint.
filterIntersections  _                                      Nothing                                _                                        = Nothing
filterIntersections (Just (_ , Left (NoIntersection _ _))) (Just (seg , Left (HitEndPoint   l1)))  Nothing                                  = Just (seg, Left $ endPoint l1)
filterIntersections  Nothing                               (Just (seg , Left (HitEndPoint   l1))) (Just (_    , Left (NoIntersection _ _))) = Just (seg, Left $ endPoint l1)
filterIntersections  Nothing                               (Just (seg , Left (HitStartPoint l1))) (Just (_    , Left (NoIntersection _ _))) = Just (seg, Left $ startPoint l1)
filterIntersections (Just (_ , Left (NoIntersection _ _))) (Just (seg , Left (HitStartPoint l1)))  Nothing                                  = Just (seg, Left $ startPoint l1)
-- a segment, alone.
filterIntersections  Nothing                               (Just (seg , Left (HitEndPoint   l1)))  Nothing                                  = Just (seg, Left $ endPoint l1)
filterIntersections  Nothing                               (Just (seg , Left (HitStartPoint l1)))  Nothing                                  = Just (seg, Left $ startPoint l1)
-- FIXME: what are these for?
filterIntersections  Nothing                               (Just (seg , Left (HitStartPoint l1))) (Just (_    , Right _))                   = Just (seg, Left $ startPoint l1)
filterIntersections  Nothing                               (Just (seg , Left (HitEndPoint   l1))) (Just (_    , Right _))                   = Just (seg, Left $ endPoint l1)
filterIntersections (Just (_ , Right _))                   (Just (seg , Left (HitStartPoint l1)))  Nothing                                  = Just (seg, Left $ startPoint l1)
filterIntersections (Just (_ , Right _))                   (Just (seg , Left (HitEndPoint   l1)))  Nothing                                  = Just (seg, Left $ endPoint l1)
filterIntersections l1 l2 l3 = error
                               $ "insane result of filterIntersections\n"
                               <> show l1 <> "\n"
                               <> (if isJust l1
                                   then "Endpoint: " <> show (endPoint $ lSeg $ fromJust l1) <> "\nLength: " <> show (lineLength $ fromJust l1) <> "\n"
                                   else "")
                               <> show l2 <> "\n"
                               <> (if isJust l2
                                   then "Endpoint: " <> show (endPoint $ lSeg $ fromJust l2) <> "\nLength: " <> show (lineLength $ fromJust l2) <> "\n"
                                   else "")
                               <> show l3 <> "\n"
                               <> (if isJust l3
                                   then "Endpoint: " <> show (endPoint $ lSeg $ fromJust l3) <> "\nLength: " <> show (lineLength $ fromJust l3) <> "\n"
                                   else "")
      where
        lSeg :: (LineSeg, Either Intersection PIntersection) -> LineSeg
        lSeg (myseg,_) = myseg
        lineLength :: (LineSeg, Either Intersection PIntersection) -> ℝ
        lineLength (mySeg, _) = distance (startPoint mySeg) (endPoint mySeg)
