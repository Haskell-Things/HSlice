{-
 - Copyright 2021 Julia Longtin
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

{- Purpose of this file: to hold the logic and routines required for coliding
   motorcycles, making motorcycle graphs, and generally reasoning about
   motorcycles.
-}

-- inherit instances when deriving.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

-- So we can section tuples
{-# LANGUAGE TupleSections #-}

module Graphics.Slicer.Math.Skeleton.Motorcycles (Collision(HeadOn), CrashTree(CrashTree), Motorcycle(Motorcycle), motorcycleToENode, Crash(Crash), motorcycleIntersectsAt, intersectionSameSide, crashMotorcycles, collisionResult, convexMotorcycles) where

import Prelude (Bool(True, False), Either(Left,Right), Eq, error, filter, head, last, length, notElem, otherwise, show, (&&), (<>), ($), (<$>), (==), (/=), (||), (.), zip, null)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, isJust, fromJust, isNothing)

import Graphics.Slicer.Math.Line (LineSeg)

import Graphics.Slicer.Math.PGA (PLine2(PLine2), PPoint2, eToPLine2, flipPLine2, lineIsLeft, pPointsOnSameSideOfPLine, PIntersection(IntersectsIn,PParallel,PAntiParallel), Intersection(HitEndPoint, HitStartPoint, NoIntersection), intersectsWith)

import Graphics.Slicer.Math.Definitions (Contour, mapWithFollower, mapWithNeighbors)

import Graphics.Slicer.Math.Skeleton.Definitions (Motorcycle(Motorcycle), ENode(ENode), concavePLines, linesOfContour, linePairs, pPointOf, isCollinear, outOf)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPair)

-- | The collision of two motorcycles. one lives, and one doesn't, unless it's a head on collision, in which case both die, and there is no survivor.
data Crash = Crash { _inMotorcycles :: [Motorcycle], _survivor :: Maybe Motorcycle, _collisionType :: Collision}
  deriving (Eq)

-- | the type of collision. only normal collisions (motorcycle to the other motorcycle's path) are survivable, and then only by the motorcycle who's path was collided with.
data Collision = Normal | HeadOn | SideSwipe
  deriving (Eq)

-- | the resulting node graph for a given contour.
data CrashTree = CrashTree { _motorcycles :: [Motorcycle], _survivors :: [Motorcycle], _crashes :: [[Crash]] }
  deriving (Eq)

collisionResult :: Crash -> Collision
collisionResult (Crash _ _ collision) = collision

-- | convert a Motorcycle to an ENode
motorcycleToENode :: Motorcycle -> ENode
motorcycleToENode (Motorcycle segs mcpath) = ENode segs mcpath

crashMotorcycles :: Contour -> [Contour] -> Maybe CrashTree
crashMotorcycles contour holes
  | null holes = getCrashTree firstMotorcycles [] [] False
  | otherwise = Nothing
  where
    firstMotorcycles
      | null holes = convexMotorcycles contour
      | otherwise = error "cannot crash with holes yet."

    -- FIXME: not yet used.
    firstMotorcyclesOfHoles = concaveMotorcycles <$> holes

    -- Function meant to be recursed, to give us a CrashTree. when it's complete...
    -- For now, just cover the cases we know what to do with.
    getCrashTree :: [Motorcycle] -> [Motorcycle] -> [[Crash]] -> Bool -> Maybe CrashTree
    getCrashTree inMotorcycles crashedMotorcycles inCrashes hasHoles
      -- We're done.
      | null findSurvivors = Just $ CrashTree inMotorcycles findSurvivors inCrashes
      -- there is no-one to collide with.
      | length inMotorcycles == 1 && null crashedMotorcycles = Just $ CrashTree inMotorcycles inMotorcycles []
      -- One crash, no survivors.
      | length inMotorcycles == 2 && null crashedMotorcycles
        && crashOf (head inMotorcycles) (last inMotorcycles) == Just HeadOn = Just $ CrashTree inMotorcycles [] [[Crash inMotorcycles Nothing HeadOn]]
      | otherwise = Nothing
        where
          -- determine the set of motorcycles have not yet had a crash.
          findSurvivors = filter (\a -> a `notElem` crashedMotorcycles) inMotorcycles

          -- Crash two motorcycles.
          crashOf mot1 mot2@(Motorcycle (seg1, seg2) _)
            | isCollinear (outOf mot1) (outOf mot2) && motorcycleIntersectsAt contour mot1 == (seg1, Just seg2) = Just HeadOn
            | otherwise = Nothing

-- | Find the non-reflex virtexes of a contour and draw motorcycles from them. Useful for contours that are a 'hole' in a bigger contour.
--   This function is meant to be used on the exterior contour.
convexMotorcycles :: Contour -> [Motorcycle]
convexMotorcycles contour = catMaybes $ onlyMotorcycles <$> zip (linePairs contour) (mapWithFollower convexPLines $ linesOfContour contour)
  where
    onlyMotorcycles :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe Motorcycle
    onlyMotorcycles ((seg1, seg2), maybePLine)
      | isJust maybePLine = Just $ Motorcycle (seg1, seg2) $ flipPLine2 $ fromJust maybePLine
      | otherwise         = Nothing
    -- | Examine two line segments that are part of a Contour, and determine if they are convex toward the interior of the Contour. if they are, construct a PLine2 bisecting them, pointing toward the interior of the Contour.
    convexPLines :: LineSeg -> LineSeg -> Maybe PLine2
    convexPLines seg1 seg2
      | Just True == lineIsLeft seg1 seg2  = Nothing
      | otherwise                          = Just $ PLine2 $ addVecPair pv1 pv2
      where
        (PLine2 pv1) = eToPLine2 seg1
        (PLine2 pv2) = flipPLine2 $ eToPLine2 seg2

-- | Find the non-reflex virtexes of a contour and draw motorcycles from them.
--   A reflex virtex is any point where the line in and the line out are convex, when looked at from inside of the contour.
--   This function is for use on interior contours.
-- FIXME: why does this look so different from the previous function?
concaveMotorcycles :: Contour -> [Motorcycle]
concaveMotorcycles contour = catMaybes $ onlyMotorcycles <$> zip (linePairs contour) (mapWithFollower concavePLines $ linesOfContour contour)
  where
    onlyMotorcycles :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe Motorcycle
    onlyMotorcycles ((seg1, seg2), maybePLine)
      | isJust maybePLine = Just $ Motorcycle (seg1,seg2) $ fromJust maybePLine
      | otherwise         = Nothing

-- | Find where a motorcycle intersects a contour, if the motorcycle is emitted from between the two given segments.
--   If the motorcycle lands between two segments, return the second segment, as well.
motorcycleIntersectsAt :: Contour -> Motorcycle -> (LineSeg, Maybe LineSeg)
motorcycleIntersectsAt contour (Motorcycle (inSeg,outSeg) path)
  | length (getMotorcycleIntersections path contour) == 2 && length foundSegEvents == 1 = head foundSegEvents
  | otherwise = error $ "handle more than one intersection point here." <> show (getMotorcycleIntersections path contour) <> "\n"
  where
    foundSegEvents = filter (\(seg, maybeSeg) -> (seg /= inSeg && seg /= outSeg) &&
                                                   (isNothing maybeSeg ||
                                                    (fromJust maybeSeg /= inSeg && fromJust maybeSeg /= outSeg))) $ getMotorcycleIntersections path contour
    -- find one of the two segments given, returning the one closest to the head of the given contour.
    getMotorcycleIntersections :: PLine2 -> Contour -> [(LineSeg, Maybe LineSeg)]
    getMotorcycleIntersections myline c = catMaybes $ mapWithNeighbors saneIntersections $ zip (linesOfContour c) $ intersectsWith (Right myline) . Left <$> linesOfContour c
      where
        saneIntersections :: (LineSeg, Either Intersection PIntersection) -> (LineSeg, Either Intersection PIntersection) -> (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Maybe LineSeg)
        saneIntersections  _ (seg, Right (IntersectsIn _))      _ = Just (seg, Nothing)
        saneIntersections  _ (_  , Left  NoIntersection)        _ = Nothing
        saneIntersections  _ (_  , Right PParallel)             _ = Nothing
        saneIntersections  _ (_  , Right PAntiParallel)         _ = Nothing
        saneIntersections  _                              (seg , Left (HitStartPoint _ _)) (seg2 , Left (HitEndPoint   _ _)) = Just (seg, Just seg2)
        saneIntersections (_  , Left (HitStartPoint _ _)) (_   , Left (HitEndPoint   _ _))  _                                = Nothing
        saneIntersections  _                              (_   , Left (HitEndPoint   _ _)) (_    , Left (HitStartPoint _ _)) = Nothing
        saneIntersections (seg, Left (HitEndPoint   _ _)) (seg2, Left (HitStartPoint _ _))  _                                = Just (seg, Just seg2)
        saneIntersections l1 l2 l3 = error $ "insane result of saneIntersections:\n" <> show l1 <> "\n" <> show l2 <> "\n" <> show l3 <> "\n"

-- | Determine if a node is on one side of a motorcycle, or the other.
--   Assumes the starting point of the second line segment is a point on the path.
intersectionSameSide :: Motorcycle -> PPoint2 -> ENode -> Maybe Bool
intersectionSameSide (Motorcycle _ path) pointOnSide node = pPointsOnSameSideOfPLine (pPointOf node) pointOnSide path




