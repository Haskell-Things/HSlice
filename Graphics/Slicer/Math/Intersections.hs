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

{-# LANGUAGE TupleSections #-}

module Graphics.Slicer.Math.Intersections (getMotorcycleSegSetIntersections, getMotorcycleContourIntersections, contourIntersectionCount, getContourLineSegIntersections, getLineSegIntersections, intersectionOf, intersectionBetween, noIntersection, isCollinear, isAntiCollinear, isParallel, isAntiParallel) where

import Prelude (Bool(True), Either(Left,Right), any, error, otherwise, show, (&&), (<>), ($), (<$>), (/=), (.), zip, not, Int, (<), (*), fst, (||), (==))

import Data.Maybe( Maybe(Just,Nothing), catMaybes, mapMaybe)

import Data.List as L (filter)

import Slist.Type (Slist)

import Slist (len, slist)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg, Point2, mapWithNeighbors, startPoint, distance, lineSegsOfContour, lineSegFromEndpoints, handleLineSegError, fudgeFactor)

import Graphics.Slicer.Math.Line (endPoint)

import Graphics.Slicer.Math.PGA (Arcable(outOf), PPoint2, PIntersection(IntersectsIn, PParallel, PAntiParallel, PCollinear, PAntiCollinear), Intersection(HitEndPoint, HitStartPoint, NoIntersection), PLine2, intersectsWith, angleBetween, distanceBetween2PLine2s, eToPPoint2, eToPLine2, pLineFromEndpointsWithErr, plinesIntersectIn, pToEPoint2, normalizePLine2)

import Graphics.Slicer.Math.GeometricAlgebra (UlpSum(UlpSum))

import Graphics.Slicer.Math.Skeleton.Definitions (Motorcycle(Motorcycle))

-- | get all possible intersections between the motorcycle and the given list of segments.
getMotorcycleSegSetIntersections :: Motorcycle -> [LineSeg] -> [(LineSeg, Either Point2 PPoint2)]
getMotorcycleSegSetIntersections m@(Motorcycle (inSeg, outSeg) _) segs = stripInSegOutSeg $ catMaybes $ mapWithNeighbors saneIntersections $ shortCircuit $ zip bufferedLineSegs $ mightIntersect <$> bufferedLineSegs
  where
    -- since this is a list of segments, we terminate the list with Nothings, so that the saneIntersections pattern matching logic can deal with "there is no neighbor, but i hit a start/end point"
    bufferedLineSegs :: [Maybe LineSeg]
    bufferedLineSegs = Nothing : (Just <$> segs) <> [Nothing]
    mightIntersect :: Maybe LineSeg -> Maybe (Either Intersection PIntersection)
    mightIntersect maybeSeg = case maybeSeg of
                                Nothing -> Nothing
                                (Just seg) -> Just $ intersectsWith (Right $ outOf m) $ Left seg
    shortCircuit :: [(Maybe LineSeg, Maybe (Either Intersection PIntersection))] -> [Maybe (LineSeg, Either Intersection PIntersection)]
    shortCircuit items = shortCircuitItem <$> items
      where
        shortCircuitItem (Nothing, Nothing) = Nothing
        shortCircuitItem (Just seg, Just intersection) = Just (seg, intersection)
        shortCircuitItem item = error $ "cannot short circuit item: " <> show item <> "\n"
    stripInSegOutSeg :: [(LineSeg, Either Point2 PPoint2)] -> [(LineSeg, Either Point2 PPoint2)]
    stripInSegOutSeg = L.filter fun
      where
        -- make sure neither of these segments are inSeg or outSeg
        fun (seg,_) = seg /= inSeg && seg /= outSeg
    saneIntersections :: Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Point2 PPoint2)
    saneIntersections  _ (Just (seg, Right (IntersectsIn p _)))    _ = Just (seg, Right p)
    saneIntersections  _ (Just (_  , Left  (NoIntersection _ _)))  _ = Nothing
    saneIntersections  _ (Just (_  , Right PParallel))             _ = Nothing
    saneIntersections  _ (Just (_  , Right PAntiParallel))         _ = Nothing
    saneIntersections (Just (seg, Left (HitEndPoint   _ pt))) (Just (seg2, Left (HitStartPoint _ _)))  (Just (seg3 , Left (HitEndPoint   _ pt2)))= if distance (endPoint seg) (startPoint seg2) < fudgeFactor*15
                                                                                                                                                   then Just (seg2, Left pt)
                                                                                                                                                   else if distance (startPoint seg2) (endPoint seg3) < fudgeFactor*15
                                                                                                                                                        then Just (seg2, Left pt2)
                                                                                                                                                        else error "wtf"
    saneIntersections  _                                      (Just (seg , Left (HitStartPoint _ _)))  (Just (_    , Left (HitEndPoint   _ pt))) = Just (seg, Left pt)
    saneIntersections (Just (_  , Left (HitStartPoint _ _ ))) (Just (_   , Left (HitEndPoint   _ _)))   _                                        = Nothing
    saneIntersections  _                                      (Just (_   , Left (HitEndPoint   _ _)))  (Just (_    , Left (HitStartPoint _ _)))  = Nothing
    saneIntersections (Just (_ , Left (HitEndPoint    _ pt))) (Just (seg , Left (HitStartPoint _ _)))   _                                        = Just (seg, Left pt)
    saneIntersections  _                                      Nothing                                   _                                        = Nothing
    saneIntersections (Just (_  , Left (NoIntersection _ _))) (Just (seg , Left (HitEndPoint  _ pt)))   Nothing                                  = Just (seg, Left pt)
    saneIntersections  Nothing                                (Just (seg , Left (HitEndPoint  _ pt)))  (Just (_ , Left (NoIntersection _ _)))    = Just (seg, Left pt)
    saneIntersections  Nothing                                (Just (seg , Left (HitStartPoint _ pt))) (Just (_ , Left (NoIntersection _ _)))    = Just (seg, Left pt)
    saneIntersections  Nothing                                (Just (seg , Left (HitStartPoint _ pt)))  Nothing                                  = Just (seg, Left pt)
    saneIntersections  Nothing                                (Just (seg , Left (HitEndPoint  _ pt)))   Nothing                                  = Just (seg, Left pt)
    saneIntersections  Nothing                                (Just (seg , Left (HitStartPoint _ pt))) (Just (_ , Right _))                      = Just (seg, Left pt)
    saneIntersections  Nothing                                (Just (seg , Left (HitEndPoint  _ pt)))  (Just (_ , Right _))                      = Just (seg, Left pt)
    saneIntersections (Just (_ , Right _))                    (Just (seg , Left (HitStartPoint _ pt)))  Nothing                                  = Just (seg, Left pt)
    saneIntersections (Just (_ , Right _))                    (Just (seg , Left (HitEndPoint  _ pt)))   Nothing                                  = Just (seg, Left pt)
    saneIntersections l1 l2 l3 = error
                                 $ "insane result of saneIntersections:\n" <> show l1 <> "\n" <> show l2 <> "\n" <> show l3 <> "\n"
                                 <> "When intersection motorcycle out:\n" <> show (outOf m) <> "\n"
                                 <> "against segments:\n" <> show segs <> "\n"
                                 <> show bufferedLineSegs <> "\n"

-- get all possible intersections between the motorcycle and the contour.
getMotorcycleContourIntersections :: Motorcycle -> Contour -> [(LineSeg, Either LineSeg PPoint2)]
getMotorcycleContourIntersections m@(Motorcycle (inSeg, outSeg) _) c = stripInSegOutSeg $ catMaybes $ mapWithNeighbors saneIntersections res
  where
    res = zip contourLines $ intersectsWith (Right (outOf m)) . Left <$> contourLines
    stripInSegOutSeg :: [(LineSeg, Either LineSeg PPoint2)] -> [(LineSeg, Either LineSeg PPoint2)]
    stripInSegOutSeg myIntersections
      | not (any fun myIntersections) = error
                                        $ "no remaining segment, after removing motorcycle's inSeg and OutSeg.\n"
                                        <> "motorcycle: " <> show m <> "\n"
                                        <> "Received: " <> show myIntersections <> "\n"
                                        <> "contourLines: " <> show contourLines <> "\n"
                                        <> "PLine: " <> show (outOf m) <> "\n"
                                        <> "Results: " <> show res <> "\n"
      | otherwise = L.filter fun myIntersections
      where
        -- make sure neither of these segments are inSeg or outSeg
        fun (seg,eitherSeg) = (seg /= inSeg && seg /= outSeg)
                              && (case eitherSeg of
                                     (Left isSeg) -> isSeg /= inSeg && isSeg /= outSeg
                                     (Right _) -> True)
    contourLines = lineSegsOfContour c
    saneIntersections :: (LineSeg, Either Intersection PIntersection) -> (LineSeg, Either Intersection PIntersection) -> (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either LineSeg PPoint2)
    saneIntersections  _ (seg, Right (IntersectsIn p _))    _ = Just (seg, Right p)
    saneIntersections  _ (_  , Left  (NoIntersection _ _))  _ = Nothing
    saneIntersections  _ (_  , Right PParallel)             _ = Nothing
    saneIntersections  _ (_  , Right PAntiParallel)         _ = Nothing
    saneIntersections (seg, Left (HitEndPoint _ _))   (seg2, Left (HitStartPoint _ _)) (seg3 , Left (HitEndPoint   _ _)) = if distance (endPoint seg) (startPoint seg2) < fudgeFactor*15
                                                                                                                           then Just (seg, Left seg2)
                                                                                                                           else Just (seg3, Left seg2)
    saneIntersections  _                              (seg2, Left (HitStartPoint _ _)) (seg  , Left (HitEndPoint   _ _)) = Just (seg, Left seg2)
    saneIntersections (_  , Left (HitStartPoint _ _)) (_   , Left (HitEndPoint   _ _))  _                                = Nothing
    saneIntersections  _                              (_   , Left (HitEndPoint   _ _)) (_    , Left (HitStartPoint _ _)) = Nothing
    saneIntersections (seg, Left (HitEndPoint _ _))   (seg2, Left (HitStartPoint _ _))  _                                = Just (seg, Left seg2)
    saneIntersections l1 l2 l3 = error
                                 $ "insane result of saneIntersections:\n"
                                 <> show l1 <> "\nEndpoint: " <> show (endPoint $ lSeg l1) <> "\nLength: " <> show (lineLength l1) <> "\nAngle: " <> show (angleBetween (eToPLine2 $ lSeg l1) (normalizePLine2 $ outOf m)) <> "\n"
                                 <> show l2 <> "\nEndpoint: " <> show (endPoint $ lSeg l2) <> "\nLength: " <> show (lineLength l2) <> "\nAngle: " <> show (angleBetween (eToPLine2 $ lSeg l2) (normalizePLine2 $ outOf m)) <> "\n"
                                 <> show l3 <> "\nEndpoint: " <> show (endPoint $ lSeg l3) <> "\nLength: " <> show (lineLength l3) <> "\nAngle: " <> show (angleBetween (eToPLine2 $ lSeg l3) (normalizePLine2 $ outOf m)) <> "\n"
      where
        lSeg :: (LineSeg, Either Intersection PIntersection) -> LineSeg
        lSeg (myseg,_) = myseg
        lineLength :: (LineSeg, Either Intersection PIntersection) -> ℝ
        lineLength (mySeg, _) = distance (startPoint mySeg) (endPoint mySeg)

-- | return the number of intersections with a given contour when traveling in a straight line from srcPoint to dstPoint.
-- Not for use against line segments that overlap and are collinear with one of the line segments that are a part of the contour.
contourIntersectionCount :: Contour -> (Point2, Point2) -> Int
contourIntersectionCount contour (start, end) = len $ getIntersections contour (start, end)
  where
    getIntersections :: Contour -> (Point2, Point2) -> Slist (LineSeg, Maybe LineSeg, PPoint2)
    getIntersections c pts = slist $ catMaybes $ mapWithNeighbors saneIntersection $ zip (lineSegsOfContour contour) $ intersectsWith (Left $ lineFromPoints pts) . Left <$> lineSegsOfContour c
      where
        -- The line we are checking for intersections along.
        lineFromPoints (lstart, lend) = handleLineSegError $ lineSegFromEndpoints lstart lend
        pLine = fst $ pLineFromEndpointsWithErr start end
        -- a filter for results that make sense.
        saneIntersection :: (LineSeg, Either Intersection PIntersection) -> (LineSeg, Either Intersection PIntersection) -> (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Maybe LineSeg, PPoint2)
        saneIntersection _ (seg,Right (IntersectsIn p _)) _ = Just (seg, Nothing, p)
        saneIntersection _ (_,Left (NoIntersection _ _))  _ = Nothing
        saneIntersection _ (_,Right PParallel)            _ = Nothing
        saneIntersection _ (_,Right PAntiParallel)        _ = Nothing
        saneIntersection  _                              (seg , Left (HitStartPoint _ point)) (seg2 , Left (HitEndPoint   _ _)) = Just (seg, Just seg2, eToPPoint2 point)
        saneIntersection (_  , Left (HitStartPoint _ _)) (_   , Left (HitEndPoint   _ _))      _                                = Nothing
        saneIntersection  _                              (_   , Left (HitEndPoint   _ _))     (_    , Left (HitStartPoint _ _)) = Nothing
        saneIntersection (seg, Left (HitEndPoint   _ _)) (seg2, Left (HitStartPoint _ point))  _                                = Just (seg, Just seg2, eToPPoint2 point)
        -- handle hitting the startPoint of lineFromPoints.
        saneIntersection  _                              (seg , Left (HitStartPoint seg2 _ ))  _                             = if seg2 /= seg
                                                                                                                                  then Nothing
                                                                                                                                  else error "insane"
        saneIntersection l1 l2 l3 = error
                                    $ "insane result of saneIntersections:\n"
                                    <> show l1 <> "\n" <> show (lEnd l1) <> "\n" <> show (angleBetween (eToPLine2 $ lSeg l1) (eToPLine2 $ lSeg l2)) <> "\n" <> show (angleBetween (eToPLine2 $ lSeg l1) $ normalizePLine2 pLine) <> "\n"
                                    <> show l2 <> "\n" <> show (lEnd l2) <> "\n" <> show (angleBetween (eToPLine2 $ lSeg l2) (eToPLine2 $ lSeg l3)) <> "\n" <> show (angleBetween (eToPLine2 $ lSeg l1) $ normalizePLine2 pLine) <> "\n"
                                    <> show l3 <> "\n" <> show (lEnd l3) <> "\n"                                                                            <> show (angleBetween (eToPLine2 $ lSeg l1) $ normalizePLine2 pLine) <> "\n"
          where
            lSeg :: (LineSeg, Either Intersection PIntersection) -> LineSeg
            lSeg (myseg,_) = myseg
            lEnd :: (LineSeg, Either Intersection PIntersection) -> Point2
            lEnd (myseg,_) = endPoint myseg

getContourLineSegIntersections :: Contour -> LineSeg -> Slist Point2
getContourLineSegIntersections contour line = slist $ mapMaybe (saneIntersection . intersectsWith (Left line) . Left) $ lineSegsOfContour contour
  where
    saneIntersection :: Either Intersection PIntersection -> Maybe Point2
    saneIntersection (Left (NoIntersection _ _))   = Nothing
    saneIntersection (Right (IntersectsIn p _))    = Just $ pToEPoint2 p
    saneIntersection (Right PAntiParallel)         = Nothing
    saneIntersection (Right PParallel)             = Nothing
    -- FIXME: fix the remaining cases. steal the code / algorithms from closedRegion
    saneIntersection res = error $ "insane result drawing a line to the edge: " <> show res <> "\n"

getLineSegIntersections :: PLine2 -> Contour -> [Point2]
getLineSegIntersections myline c = saneIntersections $ zip (lineSegsOfContour c) $ intersectsWith (Right myline) . Left <$> lineSegsOfContour c
  where
    -- FIXME: why were we snapping to grid here?
    saneIntersections :: [(LineSeg, Either Intersection PIntersection)] -> [Point2]
    saneIntersections xs = catMaybes $ mapWithNeighbors saneIntersection xs
      where
        saneIntersection :: (LineSeg, Either Intersection PIntersection) -> (LineSeg, Either Intersection PIntersection) -> (LineSeg, Either Intersection PIntersection) -> Maybe Point2
        saneIntersection _ (_, Right (IntersectsIn p _))  _ = Just $ pToEPoint2 p
        saneIntersection _ (_, Left (NoIntersection _ _)) _ = Nothing
        saneIntersection _ (_, Right PParallel)           _ = Nothing
        saneIntersection _ (_, Right PAntiParallel)       _ = Nothing
        -- Since every stop point of one line segment in a contour should be the same as the next start point...
        -- only count the first start point, when going in one direction..
        saneIntersection _                               (_, Left (HitStartPoint _ point)) (_, Left (HitEndPoint   _ _))  = Just point
        saneIntersection (_, Left (HitStartPoint _ _))   (_, Left (HitEndPoint   _ _    )) _                              = Nothing
        -- and only count the first start point, when going in the other direction.
        saneIntersection _                               (_, Left (HitEndPoint   _ _    )) (_, Left (HitStartPoint _ _))  = Nothing
        saneIntersection (_, Left (HitEndPoint   _ _))   (_, Left (HitStartPoint _ point)) _                              = Just point
        -- Ignore the end and start point that comes before / after a collinear section.
        saneIntersection (_, Right PCollinear)           (_, Left (HitStartPoint _ _    )) _                              = Nothing
        saneIntersection _                               (_, Left (HitEndPoint   _ _    )) (_, Right PCollinear)          = Nothing
        saneIntersection (_, Right PCollinear)           (_, Left (HitEndPoint   _ _    )) _                              = Nothing
        saneIntersection _                               (_, Left (HitStartPoint _ _    )) (_, Right PCollinear)          = Nothing
        -- FIXME: we should 'stitch out' collinear segments, not just ignore them.
        saneIntersection _                               (_, Right PCollinear)              _                             = Nothing
        saneIntersection r1 r2 r3 = error $ "insane result of intersecting a line (" <> show myline <> ") with a contour " <> show c <> "\n" <> show r1 <> "\n" <> show r2 <> "\n" <> show r3 <> "\n"

-- | Get the intersection point of two lines we know have an intersection point.
intersectionOf :: PLine2 -> PLine2 -> PPoint2
intersectionOf pl1 pl2 = saneIntersection $ plinesIntersectIn pl1 pl2
  where
    saneIntersection PAntiCollinear     = error $ "cannot get the intersection of anti-collinear lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PCollinear         = error $ "cannot get the intersection of collinear lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PParallel          = error $ "cannot get the intersection of parallel lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PAntiParallel      = error $ "cannot get the intersection of antiparallel lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection (IntersectsIn p _) = p

-- | Get the intersection point of two lines.
intersectionBetween :: PLine2 -> PLine2 -> Maybe (Either PLine2 PPoint2)
intersectionBetween pl1 pl2 = saneIntersection $ plinesIntersectIn pl1 pl2
  where
    saneIntersection PAntiCollinear     = Just $ Left pl1
    saneIntersection PCollinear         = Just $ Left pl1
    saneIntersection PParallel          = if distanceBetween2PLine2s (normalizePLine2 pl1) (normalizePLine2 pl2) < fudgeFactor
                                          then Just $ Left pl1
                                          else Nothing
    saneIntersection PAntiParallel      = if distanceBetween2PLine2s (normalizePLine2 pl1) (normalizePLine2 pl2) < fudgeFactor
                                          then Just $ Left pl1
                                          else Nothing
    saneIntersection (IntersectsIn p _) = Just $ Right p


-- | check if two lines cannot intersect.
noIntersection :: PLine2 -> PLine2 -> Bool
noIntersection pline1 pline2 = isCollinear pline1 pline2 || isParallel pline1 pline2 || isAntiCollinear pline1 pline2 || isAntiParallel pline1 pline2

-- | check if two lines are really the same line.
isCollinear :: PLine2 -> PLine2 -> Bool
isCollinear pline1 pline2 = plinesIntersectIn pline1 pline2 == PCollinear

-- | check if two lines are really the same line.
isAntiCollinear :: PLine2 -> PLine2 -> Bool
isAntiCollinear pline1 pline2 = plinesIntersectIn pline1 pline2 == PAntiCollinear

-- | check if two lines are parallel.
isParallel :: PLine2 -> PLine2 -> Bool
isParallel pline1 pline2 = plinesIntersectIn pline1 pline2 == PParallel

-- | check if two lines are anti-parallel.
isAntiParallel :: PLine2 -> PLine2 -> Bool
isAntiParallel pline1 pline2 = plinesIntersectIn pline1 pline2 == PAntiParallel
