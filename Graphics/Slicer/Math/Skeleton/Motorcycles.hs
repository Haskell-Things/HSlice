{- ORMOLU_DISABLE -}
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
{-# LANGUAGE DerivingStrategies #-}

module Graphics.Slicer.Math.Skeleton.Motorcycles (CollisionType(HeadOn), CrashTree(CrashTree), motorcycleToENode, Collision(Collision), motorcycleIntersectsAt, intersectionSameSide, crashMotorcycles, collisionResult, convexMotorcycles) where

import Prelude (Bool(True, False), Either(Left,Right), Eq, error, notElem, otherwise, show, (&&), (<>), ($), (<$>), (==), (/=), (.), zip, null)

import Data.Maybe( Maybe(Just,Nothing), catMaybes)

import Data.List as L (filter)

import Slist (slist)

import Slist as SL (filter)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Contour (lineSegsOfContour)

import Graphics.Slicer.Math.Line (LineSeg)

import Graphics.Slicer.Math.PGA (PLine2(PLine2), PPoint2, eToPLine2, flipPLine2, lineIsLeft, pPointsOnSameSideOfPLine, PIntersection(IntersectsIn,PParallel,PAntiParallel), Intersection(HitEndPoint, HitStartPoint, NoIntersection), intersectsWith)

import Graphics.Slicer.Math.Definitions (Contour, mapWithFollower, mapWithNeighbors)

import Graphics.Slicer.Math.Skeleton.Definitions (Motorcycle(Motorcycle), ENode(ENode), linePairs, pPointOf, isCollinear, outOf)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPair)

-- | The collision of two motorcycles. one lives, and one doesn't, unless it's a head on collision, in which case both die, and there is no survivor.
data Collision = Collision { _inMotorcycles :: !(Motorcycle, Motorcycle, Slist Motorcycle), _survivor :: !(Maybe Motorcycle), collisionResult :: !CollisionType }
  deriving (Eq)

-- | the type of collision.
-- only normal collisions (motorcycle intersects the other motorcycle's path) are survivable, and then only by the motorcycle who's path was collided with.
data CollisionType = Normal | HeadOn | SideSwipe
  deriving (Eq)

-- | the resulting node graph for a given contour.
data CrashTree = CrashTree { _motorcycles :: !(Slist Motorcycle), _survivors :: !(Slist Motorcycle), _crashes :: !(Slist Collision) }
  deriving (Eq)

-- | convert a Motorcycle to an ENode
motorcycleToENode :: Motorcycle -> ENode
motorcycleToENode (Motorcycle segs mcpath) = ENode segs mcpath

crashMotorcycles :: Contour -> [Contour] -> Maybe CrashTree
crashMotorcycles contour holes
  | null holes = getCrashTree (slist firstMotorcycles) [] (slist []) False
  | otherwise = Nothing
  where
    firstMotorcycles
      | null holes = convexMotorcycles contour
      | otherwise = error "cannot crash with holes yet."

    -- FIXME: not yet used.
    --firstMotorcyclesOfHoles = concaveMotorcycles <$> holes

    -- Function meant to be recursed, to give us a CrashTree. when it's complete...
    -- For now, just cover the cases we know what to do with.
    getCrashTree :: Slist Motorcycle -> [Motorcycle] -> Slist Collision -> Bool -> Maybe CrashTree
    getCrashTree inMotorcycles crashedMotorcycles inCrashes hasHoles
      | hasHoles = error "do not support holes yet"
      -- We're done.
      | null findSurvivors = Just $ CrashTree inMotorcycles findSurvivors inCrashes
      | null crashedMotorcycles = case inMotorcycles of
                                    -- there is no-one to collide with.
                                    (Slist [] _) -> Just $ CrashTree inMotorcycles inMotorcycles (slist [])
                                    (Slist _ 1) -> Just $ CrashTree inMotorcycles inMotorcycles (slist [])
                                  -- One crash, no survivors.
                                    (Slist [firstMC, secondMC] 2) -> if crashOf firstMC secondMC == Just HeadOn
                                                                     then Just $ CrashTree inMotorcycles (slist []) (slist [Collision (firstMC, secondMC,slist []) Nothing HeadOn])
                                                                     else Nothing
                                    (Slist (_:_) _) -> Nothing
      -- Note that to solve this case, we will have to have a concept of speed of the motorcycle.
      | otherwise = Nothing
        where
          -- determine the set of motorcycles have not yet had a crash.
          findSurvivors = SL.filter (`notElem` crashedMotorcycles) inMotorcycles

          -- Crash two motorcycles.
          crashOf mot1 mot2@(Motorcycle (seg1, seg2) _)
            | isCollinear (outOf mot1) (outOf mot2) && motorcycleIntersectsAt contour mot1 == (seg1, Just seg2) = Just HeadOn
            | otherwise = Nothing

-- | Find the non-reflex virtexes of a contour and draw motorcycles from them. Useful for contours that are a 'hole' in a bigger contour.
--   This function is meant to be used on the exterior contour.
convexMotorcycles :: Contour -> [Motorcycle]
convexMotorcycles contour = catMaybes $ onlyMotorcycles <$> zip (linePairs contour) (mapWithFollower convexPLines $ lineSegsOfContour contour)
  where
    onlyMotorcycles :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe Motorcycle
    onlyMotorcycles ((seg1, seg2), maybePLine) = case maybePLine of
                                                   (Just pLine) -> Just $ Motorcycle (seg1, seg2) $ flipPLine2 pLine
                                                   Nothing -> Nothing
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
{-
concaveMotorcycles :: Contour -> [Motorcycle]
concaveMotorcycles contour = catMaybes $ onlyMotorcycles <$> zip (linePairs contour) (mapWithFollower concavePLines $ lineSegsOfContour contour)
  where
    onlyMotorcycles :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe Motorcycle
    onlyMotorcycles ((seg1, seg2), maybePLine)
      | isJust maybePLine = Just $ Motorcycle (seg1,seg2) $ fromJust maybePLine
      | otherwise         = Nothing
-}

-- | Find where a motorcycle intersects a contour.
--   If the motorcycle lands between two segments, return the second segment, as well.
motorcycleIntersectsAt :: Contour -> Motorcycle -> (LineSeg, Maybe LineSeg)
motorcycleIntersectsAt contour motorcycle@(Motorcycle (inSeg,outSeg) _) = case intersections of
                                                                            [] -> error "no intersections?"
                                                                            [a] -> error $ "not enough intersections: " <> show a <> "\n"
                                                                            [_a,_b] -> foundSegEvents intersections
                                                                            (_:_) -> error "too many intersections?"
  where
    intersections = getMotorcycleIntersections motorcycle contour
    -- find where the motorcycle intersects the contour, by filtering the possibilities.
    foundSegEvents :: [(LineSeg, Maybe LineSeg)] -> (LineSeg, Maybe LineSeg)
    foundSegEvents myIntersections = case L.filter fun myIntersections of
                       [] -> error "no found segment."
                       [a] -> a
                       (_:_) -> error "too many intersections?"
                     where
                       -- make sure neither of these segments are inSeg or outSeg
                       fun (seg,maybeSeg) = (seg /= inSeg && seg /= outSeg)
                                            && (case maybeSeg of
                                                  (Just isSeg) -> isSeg /= inSeg && isSeg /= outSeg
                                                  Nothing -> True)
    -- get all possible intersections between the motorcycle and the contour.
    getMotorcycleIntersections :: Motorcycle -> Contour -> [(LineSeg, Maybe LineSeg)]
    getMotorcycleIntersections m c = catMaybes $ mapWithNeighbors saneIntersections $ zip contourLines $ intersectsWith (Right $ outOf m) . Left <$> contourLines
      where
        contourLines = lineSegsOfContour c
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
intersectionSameSide :: PPoint2 -> ENode -> Motorcycle -> Maybe Bool
intersectionSameSide pointOnSide node (Motorcycle _ path) = pPointsOnSameSideOfPLine (pPointOf node) pointOnSide path

