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

module Graphics.Slicer.Math.ContourIntersections (
  contourIntersectionCount,
  getLineContourIntersections,
  getMotorcycleContourIntersections,
  getMotorcycleSegSetIntersections
  ) where

import Prelude (Bool, Either(Left, Right), Int, Show(show), (<$>), (<>), (<=), (==), ($), (/=), (&&), error, fst, length, mempty, odd, otherwise, zip)

import Data.List (filter)

import Data.Maybe (Maybe(Just, Nothing), catMaybes, fromJust, isJust)

import Data.MemoTrie (memo2)

import Slist.Type (Slist)

import Slist (len, slist)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(endPoint, startPoint), Point2, distance, lineSegsOfContour, makeLineSeg, mapWithNeighbors)

-- import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.Intersections (outputIntersectsLineSeg)

import Graphics.Slicer.Math.PGA (Intersection(HitEndPoint, HitStartPoint, NoIntersection), PIntersection(IntersectsIn, PParallel, PAntiParallel, PCollinear, PAntiCollinear), ProjectivePoint, ProjectiveLine, ProjectiveLine2, PLine2Err, distance2PP, eToPL, intersectsWithErr, normalizeL, pLineIsLeft, pToEP, eToPP)

import Graphics.Slicer.Math.Skeleton.Definitions (Motorcycle(Motorcycle))

-- | Memoized entry point for contourIntersectionCount'
contourIntersectionCount :: Contour -> (Point2, Point2) -> Int
contourIntersectionCount = memo2 contourIntersectionCount'

-- | return the number of intersections with a given contour when traveling in a straight line from the beginning of the given line segment to the end of the line segment.
-- Not for use when line segments can overlap or are collinear with one of the line segments that are a part of the contour.
contourIntersectionCount' :: Contour -> (Point2, Point2) -> Int
contourIntersectionCount' contour (start, end) = len $ getIntersections contour (start, end)
  where
    getIntersections :: Contour -> (Point2, Point2) -> Slist (LineSeg, Crossover)
    getIntersections c (pt1, pt2) = slist $ catMaybes $ mapWithNeighbors filterAllIntersections $ openCircuit $ zip (lineSegsOfContour contour) $ intersectsWithErr targetSeg <$> segs
      where
        segs :: [Either LineSeg (ProjectiveLine, PLine2Err)]
        segs =  Left <$> lineSegsOfContour c
        targetSeg :: Either LineSeg (ProjectiveLine, PLine2Err)
        targetSeg = Left $ makeLineSeg pt1 pt2
        openCircuit v = Just <$> v

-- | Get the intersections between a Line and a contour as a series of points. always returns an even number of intersections.
{-# INLINABLE getLineContourIntersections #-}
getLineContourIntersections :: (ProjectiveLine2 a) => (a, PLine2Err) -> Contour -> Maybe [Point2]
getLineContourIntersections (line, lineErr) c
  | odd $ length res = Nothing
  | otherwise = Just res
  where
    res = getPoints $ catMaybes $ mapWithNeighbors filterIntersections $ openCircuit $ zip (lineSegsOfContour c) $ intersectsWithErr targetLine <$> segs
      where
        segs :: [Either LineSeg (ProjectiveLine, PLine2Err)]
        segs =  Left <$> lineSegsOfContour c
        -- NOTE: we have to use a concrete type here.
        targetLine :: Either LineSeg (ProjectiveLine, PLine2Err)
        targetLine = Right (nLine, lineErr <> nLineErr)
        (nLine, nLineErr) = normalizeL line
        openCircuit v = Just <$> v
        getPoints :: [(LineSeg, Either Point2 ProjectivePoint)] -> [Point2]
        getPoints vs = getPoint <$> vs
          where
            getPoint (_, Left v) = v
            getPoint (_, Right v) = fst $ pToEP v

-- | Get all possible intersections between the motorcycle and the contour.
-- Filters out the input and output segment of the motorcycle.
getMotorcycleContourIntersections :: Motorcycle -> Contour -> [(LineSeg, Either Point2 ProjectivePoint)]
getMotorcycleContourIntersections m@(Motorcycle (inSeg, outSeg) _ _) c = stripInSegOutSeg $ catMaybes $ mapWithNeighbors filterIntersections $ openCircuit $ zip contourLines $ outputIntersectsLineSeg m <$> contourLines
  where
    openCircuit v = Just <$> v
    contourLines = lineSegsOfContour c
    stripInSegOutSeg :: [(LineSeg, Either Point2 ProjectivePoint)] -> [(LineSeg, Either Point2 ProjectivePoint)]
    stripInSegOutSeg = filter fun
      where
        -- filter out inSeg and outSeg outSeg
        fun (seg,_) = seg /= inSeg && seg /= outSeg

-- | Get all possible intersections between the motorcycle and the given list of segments.
-- Filters out the input and output segment of the motorcycle.
getMotorcycleSegSetIntersections :: Motorcycle -> [LineSeg] -> [(LineSeg, Either Point2 ProjectivePoint)]
getMotorcycleSegSetIntersections m@(Motorcycle (inSeg, outSeg) _ _) segs = stripInSegOutSeg $ catMaybes $ mapWithNeighbors filterIntersections $ mightIntersect <$> bufferLineSegs segs
  where
    -- terminate the list with Nothings, and encapsulate the values with Justs, so that the saneIntersections pattern matching logic can deal with "there is no neighbor, but i hit a start/end point"
    bufferLineSegs :: [LineSeg] -> [Maybe LineSeg]
    bufferLineSegs mySegs = Nothing : (Just <$> mySegs) <> [Nothing]
    mightIntersect :: Maybe LineSeg -> Maybe (LineSeg, Either Intersection PIntersection)
    mightIntersect maybeSeg = case maybeSeg of
                                Nothing -> Nothing
                                (Just seg) -> Just (seg, outputIntersectsLineSeg m seg)
    stripInSegOutSeg :: [(LineSeg, Either Point2 ProjectivePoint)] -> [(LineSeg, Either Point2 ProjectivePoint)]
    stripInSegOutSeg = filter fun
      where
        -- make sure neither of these segments are inSeg or outSeg
        fun (seg,_) = seg /= inSeg && seg /= outSeg

-- | Compatibility wrapper. To handle functions that cannot handle Segment crossover results.
filterIntersections :: Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Point2 ProjectivePoint)
filterIntersections a b c = dropCollinears $ filterAllIntersections a b c
  where
    dropCollinears v
      | isJust v = case fromJust v of
                      (lineSeg, EPoint p) -> Just (lineSeg, Left p)
                      (lineSeg, PPoint p) -> Just (lineSeg, Right p)
                      _                   -> Nothing
      | otherwise = Nothing

data Crossover = EPoint Point2
               | PPoint ProjectivePoint
               | Segment LineSeg

segIsLeft :: LineSeg -> LineSeg -> Maybe Bool
segIsLeft a b = (fst $ eToPL b) `pLineIsLeft` (fst $ eToPL a)

-- | Filter the intersections, only returning results when it is appropriate to do so..
-- The purpose of this function is to ensure we only count the crossing over of a contour's edge once. So if it hits a startpoint, make sure we don't count the endpoint of the next line.. etc.
-- Note: Can not take into account (anti)collinear line segments if the first or last argument are Nothing..
filterAllIntersections :: Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Crossover)
filterAllIntersections  _ (Just (seg, Right (IntersectsIn p _)))   _ = Just (seg, PPoint p)
filterAllIntersections  _ (Just (_  , Left  (NoIntersection _ _))) _ = Nothing
filterAllIntersections  _ (Just (_  , Right PParallel))            _ = Nothing
filterAllIntersections  _ (Just (_  , Right PAntiParallel))        _ = Nothing
-- When we hit a end -> start -> end, look at the distance to tell where we hit.
filterAllIntersections (Just (seg1, Left (HitEndPoint   l1 ))) (Just (seg2, Left (HitStartPoint _ ))) (Just (seg3 , Left (HitEndPoint   l2)))
  | distance1To2 <= ulpVal distance1To2Err = Just (seg2, EPoint $ endPoint l1)
  | distance2To3 <= ulpVal distance2To3Err = Just (seg2, EPoint $ endPoint l2)
  | otherwise = error "wtf"
  where
    (distance1To2, (_,_, distance1To2Err)) = distance2PP (eToPP $ endPoint seg1, mempty) (eToPP $ startPoint seg2, mempty)
    (distance2To3, (_,_, distance2To3Err)) = distance2PP (eToPP $ startPoint seg2, mempty) (eToPP $ endPoint seg3, mempty)
-- Only count the first start point, when intersecting with lines going in one direction..
filterAllIntersections  _                                     (Just (seg , Left (HitStartPoint l1))) (Just (_    , Left (HitEndPoint   _   ))) = Just (seg, EPoint $ startPoint l1)
filterAllIntersections (Just (_ , Left (HitStartPoint  _  ))) (Just (_   , Left (HitEndPoint   _ )))  _                                        = Nothing
-- Only count the first start point, when intersecting with lines going in the other direction..
filterAllIntersections (Just (_ , Left (HitEndPoint    _  ))) (Just (seg , Left (HitStartPoint l1)))  _                                        = Just (seg, EPoint $ startPoint l1)
filterAllIntersections  _                                     (Just (_   , Left (HitEndPoint   _ ))) (Just (_    , Left (HitStartPoint _   ))) = Nothing
-- Ignore the end and start point that comes before / after a collinear section.
filterAllIntersections (Just (_, Right PCollinear          )) (Just (_   , Left (HitStartPoint _ )))  _                                        = Nothing
filterAllIntersections _                                      (Just (_   , Left (HitEndPoint   _ ))) (Just (_    , Right PCollinear         )) = Nothing
filterAllIntersections (Just (_, Right PCollinear          )) (Just (_   , Left (HitEndPoint   _ )))  _                                        = Nothing
filterAllIntersections _                                      (Just (_   , Left (HitStartPoint _ ))) (Just (_    , Right PCollinear         )) = Nothing
-- Ignore the end and start point that comes before / after an anticollinear section.
filterAllIntersections (Just (_, Right PAntiCollinear      )) (Just (_   , Left (HitStartPoint _ )))  _                                        = Nothing
filterAllIntersections _                                      (Just (_   , Left (HitEndPoint   _ ))) (Just (_    , Right PAntiCollinear     )) = Nothing
filterAllIntersections (Just (_, Right PAntiCollinear      )) (Just (_   , Left (HitEndPoint   _ )))  _                                        = Nothing
filterAllIntersections _                                      (Just (_   , Left (HitStartPoint _ ))) (Just (_    , Right PAntiCollinear     )) = Nothing
-- Return the (anti-)collinear segments so they can be stitched out, not just ignore them.
filterAllIntersections (Just (lastSeg, _))                    (Just (seg , Right PCollinear       )) (Just (nextSeg, _))                       = if segIsLeft lastSeg seg == segIsLeft seg nextSeg
                                                                                                                                                 then Just (seg, Segment seg)
                                                                                                                                                 else Nothing
filterAllIntersections (Just (lastSeg, _))                    (Just (seg , Right PAntiCollinear   )) (Just (nextSeg, _))                       = if segIsLeft lastSeg seg == segIsLeft seg nextSeg
                                                                                                                                                 then Just (seg, Segment seg)
                                                                                                                                                 else Nothing
-- FIXME: still undeterminable. how do we represent this usefully?
filterAllIntersections _                                      (Just (_   , Right PCollinear       ))  _                                        = Nothing
filterAllIntersections _                                      (Just (_   , Right PAntiCollinear   ))  _                                        = Nothing
-- And now handle the end segments, where there is nothing on the other side.
filterAllIntersections  _                                      Nothing                                _                                        = Nothing
filterAllIntersections (Just (_ , Left (NoIntersection _ _))) (Just (seg , Left (HitEndPoint   l1)))  Nothing                                  = Just (seg, EPoint $ endPoint l1)
filterAllIntersections  Nothing                               (Just (seg , Left (HitEndPoint   l1))) (Just (_    , Left (NoIntersection _ _))) = Just (seg, EPoint $ endPoint l1)
filterAllIntersections  Nothing                               (Just (seg , Left (HitStartPoint l1))) (Just (_    , Left (NoIntersection _ _))) = Just (seg, EPoint $ startPoint l1)
filterAllIntersections (Just (_ , Left (NoIntersection _ _))) (Just (seg , Left (HitStartPoint l1)))  Nothing                                  = Just (seg, EPoint $ startPoint l1)
-- Handle intersecting with a single segment, alone.
filterAllIntersections  Nothing                               (Just (seg , Left (HitEndPoint   l1)))  Nothing                                  = Just (seg, EPoint $ endPoint l1)
filterAllIntersections  Nothing                               (Just (seg , Left (HitStartPoint l1)))  Nothing                                  = Just (seg, EPoint $ startPoint l1)
-- Handle PParallel and PAntiParallel. Note that PCollinear and PAntiCollinear are weeded out above.
filterAllIntersections  Nothing                               (Just (seg , Left (HitStartPoint l1))) (Just (_    , Right _))                   = Just (seg, EPoint $ startPoint l1)
filterAllIntersections  Nothing                               (Just (seg , Left (HitEndPoint   l1))) (Just (_    , Right _))                   = Just (seg, EPoint $ endPoint l1)
filterAllIntersections (Just (_ , Right _))                   (Just (seg , Left (HitStartPoint l1)))  Nothing                                  = Just (seg, EPoint $ startPoint l1)
filterAllIntersections (Just (_ , Right _))                   (Just (seg , Left (HitEndPoint   l1)))  Nothing                                  = Just (seg, EPoint $ endPoint l1)
-- Error dumper. Should Not Happen(TM).
filterAllIntersections l1 l2 l3 = error
                               $ "insane result of filterAllIntersections\n"
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
