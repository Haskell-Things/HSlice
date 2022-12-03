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

module Graphics.Slicer.Math.Intersections (filterIntersections, intersectionOf, intersectionBetween, noIntersection, isCollinear, isAntiCollinear, isParallel, isAntiParallel) where

import Prelude (Bool, Either(Left,Right), error, otherwise, show, (<>), ($), (<), (*), (||), (==), mempty, realToFrac)

import Data.Maybe (Maybe(Just,Nothing), isJust, fromJust)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (LineSeg, Point2, startPoint, distance, endPoint, fudgeFactor)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.PGA (CPPoint2, PIntersection(IntersectsIn, PParallel, PAntiParallel, PCollinear, PAntiCollinear), Intersection(HitEndPoint, HitStartPoint, NoIntersection), PLine2, PLine2Err, PPoint2Err, ProjectiveLine2, distance2PL, plinesIntersectIn)

-- | filter the intersections given.
-- The purpose of this function is to ensure we only count the crossing of a line (segment) across a contour's edge more than once. so if it hits a sttartpoint, make sure we don't count the endpoint.. etc.
-- FIXME: does not take into account (anti)collinear line segments correctly.
filterIntersections :: Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Either Point2 CPPoint2)
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

-- | Get the intersection point of two lines we know have an intersection point.
intersectionOf :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (CPPoint2, PPoint2Err)
intersectionOf pl1 pl2 = saneIntersection $ plinesIntersectIn pl1 pl2
  where
    saneIntersection PAntiCollinear     = error $ "cannot get the intersection of anti-collinear lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PCollinear         = error $ "cannot get the intersection of collinear lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PParallel          = error $ "cannot get the intersection of parallel lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PAntiParallel      = error $ "cannot get the intersection of antiparallel lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection (IntersectsIn p (_,_, pErr)) = (p,pErr)

-- | Get the intersection point of two lines.
intersectionBetween :: PLine2 -> PLine2 -> Maybe (Either PLine2 (CPPoint2, PPoint2Err))
intersectionBetween pl1 pl2 = saneIntersection $ plinesIntersectIn (pl1,mempty) (pl2,mempty)
  where
    (foundDistance, (_,_, foundErr)) = distance2PL pl1 pl2
    saneIntersection PAntiCollinear     = Just $ Left pl1
    saneIntersection PCollinear         = Just $ Left pl1
    saneIntersection PParallel          = if foundDistance < realToFrac (ulpVal foundErr)
                                          then Just $ Left pl1
                                          else Nothing
    saneIntersection PAntiParallel      = if foundDistance < realToFrac (ulpVal foundErr)
                                          then Just $ Left pl1
                                          else Nothing
    saneIntersection (IntersectsIn p (_,_, pErr)) = Just $ Right (p,pErr)

-- | check if two lines cannot intersect.
noIntersection :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
noIntersection pline1@(pl1,_) pline2@(pl2,_) = isCollinear pline1 pline2 || isParallel pl1 pl2 || isAntiCollinear pline1 pline2 || isAntiParallel pl1 pl2

-- | check if two lines are really the same line.
isCollinear :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isCollinear pline1 pline2 = plinesIntersectIn pline1 pline2 == PCollinear

-- | check if two lines are really the same line.
isAntiCollinear :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isAntiCollinear pline1 pline2 = plinesIntersectIn pline1 pline2 == PAntiCollinear

-- | check if two lines are parallel.
isParallel :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
isParallel pline1 pline2 = plinesIntersectIn (pline1, mempty) (pline2, mempty) == PParallel

-- | check if two lines are anti-parallel.
isAntiParallel :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
isAntiParallel pline1 pline2 = plinesIntersectIn (pline1, mempty) (pline2, mempty) == PAntiParallel
