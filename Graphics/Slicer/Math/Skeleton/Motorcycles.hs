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

module Graphics.Slicer.Math.Skeleton.Motorcycles (CollisionType(HeadOn), CrashTree(CrashTree), motorcycleToENode, Collision(Collision), motorcycleIntersectsAt, intersectionSameSide, crashMotorcycles, collisionResult, convexMotorcycles, lastCrashType, motorcyclesAreAntiCollinear, motorcyclesInDivision, motorcycleMightIntersectWith, motorcycleDivisor) where

import Prelude (Bool(True, False), Either(Left,Right), Eq((==)), Show(show), Ordering (EQ, GT, LT), (&&), (<>), ($), (<), (>), compare, error, fst, notElem, null, otherwise, zip)

import Prelude as PL (init, last)

import Data.Maybe( Maybe(Just,Nothing), fromMaybe, mapMaybe)

import Data.List (sortBy)

import Slist (slist, safeLast, head, len)

import Slist as SL (filter)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Arcs (getInsideArc)

import Graphics.Slicer.Math.Contour (pointsOfContour)

import Graphics.Slicer.Math.ContourIntersections (getMotorcycleContourIntersections, getMotorcycleSegSetIntersections)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg, Point2, mapWithNeighbors, startPoint, endPoint, makeLineSeg)

import Graphics.Slicer.Math.Intersections (intersectionOf, isAntiCollinear, noIntersection, outputIntersectsLineSeg, outputIntersectsPLineAt)

import Graphics.Slicer.Math.Lossy (pPointBetweenPPoints, distanceBetweenPPoints, distanceBetweenPPointsWithErr, eToPLine2, join2PPoint2, pToEPoint2)

import Graphics.Slicer.Math.PGA (CPPoint2, PLine2, PLine2Err, PPoint2, PPoint2Err, Arcable(outOf), Pointable(canPoint, cPPointOf, ePointOf), cPPointAndErrOf, eToPL, flipL, pLineIsLeft, pPointsOnSameSideOfPLine, PIntersection(IntersectsIn,PAntiCollinear), ProjectivePoint2, distance2PP, eToPP, join2EP, oppositeDirection, outAndErrOf, plinesIntersectIn, translateL)

import Graphics.Slicer.Math.Skeleton.Definitions (CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), ENode(ENode), Motorcycle(Motorcycle), MotorcycleIntersection(WithLineSeg, WithENode, WithMotorcycle), getFirstLineSeg, linePairs)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

-- | The collision of two motorcycles. one lives, and one doesn't, unless it's a head on collision, in which case both die, and there is no survivor.
data Collision = Collision { _inMotorcycles :: !(Motorcycle, Motorcycle, Slist Motorcycle), _survivor :: !(Maybe Motorcycle), collisionResult :: !CollisionType }
  deriving (Eq, Show)

-- | the type of collision.
-- only normal collisions (motorcycle intersects the other motorcycle's path) are survivable, and then only by the motorcycle who's path was collided with.
-- If two motorcycles collide at the same time.. the solution cannot be found?
data CollisionType =
  Normal -- a motorcycle runs into the path of another motorcycle.
  | HeadOn -- two motorcycles are anti-parallel, and don't intersect the contour at any point other than each other's origin.
  | SideSwipe -- two motorcycles arrive at the same time, in the same location.
  deriving (Eq, Show)

-- | the resulting node graph for a given contour.
data CrashTree = CrashTree { _motorcycles :: !(Slist Motorcycle), _survivors :: !(Slist Motorcycle), _crashes :: !(Slist Collision) }
  deriving (Eq, Show)

-- | convert a Motorcycle to an ENode
motorcycleToENode :: Motorcycle -> ENode
motorcycleToENode (Motorcycle (seg1,seg2) mcpath mcErr) = ENode (startPoint seg1, startPoint seg2, endPoint seg2) mcpath mcErr

-- | Find the point where the propogation from a motorcycle equals the propogation of what it impacts, taking into account the weight of a motorcycle, and the weight of what it impacts.
motorcycleDivisor :: Motorcycle -> MotorcycleIntersection -> PPoint2
motorcycleDivisor motorcycle target = pPointBetweenPPoints (cPPointOf motorcycle) (fst pointOfTarget) (tSpeedOf target) (mSpeedOf motorcycle)
  where
    pointOfTarget = case target of
                      (WithLineSeg lineSeg) -> case outputIntersectsLineSeg motorcycle lineSeg of
                                        (Right (IntersectsIn p (_,_, pErr))) -> (p, pErr)
                                        v -> error $ "impossible!\n" <> show v <> "\n" <> show lineSeg <> "\n" <> show motorcycle <> "\n" <> show target <> "\n"
                      (WithENode eNode) -> (cPPointAndErrOf eNode)
                      (WithMotorcycle motorcycle2) -> (cPPointAndErrOf motorcycle2)
    tSpeedOf :: MotorcycleIntersection -> ℝ
    tSpeedOf myTarget = case myTarget of
                  (WithLineSeg lineSeg) -> distanceBetweenPPointsWithErr
                                           (fromMaybe (error "no outArc?") $ outputIntersectsPLineAt motorcycle (translateL (eToPLine2 lineSeg) 1))
                                           (fromMaybe (error "no outArc?") $ outputIntersectsPLineAt motorcycle (eToPL lineSeg))
                  (WithENode eNode) -> distanceBetweenPPointsWithErr
                                       (cPPointAndErrOf eNode)
                                       (fromMaybe (error "no outArc?") $ outputIntersectsPLineAt eNode (translateL (eToPLine2 $ getFirstLineSeg eNode) 1))
                  (WithMotorcycle motorcycle2) -> mSpeedOf motorcycle2
    mSpeedOf :: Motorcycle -> ℝ
    mSpeedOf myMotorcycle@(Motorcycle (seg1,_) _ _) = distanceBetweenPPoints (cPPointOf myMotorcycle) (justIntersectsIn $ plinesIntersectIn (translateL (eToPLine2 seg1) 1) (outAndErrOf myMotorcycle))
    justIntersectsIn :: PIntersection -> CPPoint2
    justIntersectsIn res = case res of
                             (IntersectsIn p _) -> p
                             v -> error $ "intersection failure." <> show v <> show target <> "\n" <> show motorcycle <> "\n"

-- | Create a crash tree for all of the motorcycles in the given contour, with the given holes.
-- FIXME: may fail, returning Nothing.
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
                                    (Slist [] _) -> -- there is no-one to collide with.
                                      Just $ CrashTree inMotorcycles inMotorcycles (slist [])
                                    (Slist _ 1) -> -- There is only one motorcycle.
                                      Just $ CrashTree inMotorcycles inMotorcycles (slist [])
                                    (Slist [firstMC, secondMC] 2) -> case crashOf firstMC secondMC of
                                                                       Just collision ->
                                                                         Just $ CrashTree inMotorcycles (slist []) (slist [collision])
                                                                       Nothing ->
                                                                         Just $ CrashTree inMotorcycles inMotorcycles (slist [])
                                    (Slist (_:_) _) -> Nothing
      -- Note that to solve this case, we will have to have a concept of speed of the motorcycle.
      | otherwise = Nothing
        where
          -- determine the set of motorcycles have not yet had a crash.
          findSurvivors = SL.filter (`notElem` crashedMotorcycles) inMotorcycles

          -- Crash just two motorcycles. Returns Nothing when the motorcycles can't collide.
          crashOf :: Motorcycle -> Motorcycle -> Maybe Collision
          crashOf mot1 mot2@(Motorcycle (inSeg2, _) _ _)
            -- If we have a clear path between mot1 and the origin of mot2
            | isAntiCollinear (outAndErrOf mot1) (outAndErrOf mot2) && motorcycleIntersectsAt contour mot1 == (inSeg2, Left $ endPoint inSeg2) = Just $ Collision (mot1,mot2, slist []) Nothing HeadOn
            | noIntersection (outAndErrOf mot1) (outAndErrOf mot2) = Nothing
            | intersectionIsBehind mot1 = Nothing
            | intersectionIsBehind mot2 = Nothing
            -- FIXME: this should be providing a distance to intersectionPPoint along the motorcycle to check.
            | otherwise = case getMotorcycleContourIntersections mot1 contour of
                          [] -> case fst (distance2PP (cPPointAndErrOf mot1) intersectionPPoint) `compare` fst (distance2PP (cPPointAndErrOf mot2) intersectionPPoint) of
                                 GT -> Just $ Collision (mot1, mot2, slist []) (Just mot2) Normal
                                 LT -> Just $ Collision (mot1, mot2, slist []) (Just mot1) Normal
                                 EQ -> Just $ Collision (mot1, mot2, slist []) (Just mot1) SideSwipe
                          _ -> Nothing
              where
                intersectionPPoint = intersectionOf (outAndErrOf mot1) (outAndErrOf mot2)
                intersectionIsBehind m = oppositeDirection (outOf m) (join2PPoint2 (cPPointOf m) (fst intersectionPPoint))

-- | Find the non-reflex virtexes of a contour and draw motorcycles from them. Useful for contours that are a 'hole' in a bigger contour.
--   This function is meant to be used on the exterior contour.
convexMotorcycles :: Contour -> [Motorcycle]
convexMotorcycles contour = mapMaybe onlyMotorcycles $ zip (rotateLeft $ linePairs contour) (mapWithNeighbors convexPLines $ pointsOfContour contour)
  where
    rotateLeft a = PL.last a : PL.init a
    onlyMotorcycles :: ((LineSeg, LineSeg), Maybe (PLine2, PLine2Err)) -> Maybe Motorcycle
    onlyMotorcycles ((seg1, seg2), maybePLine) = case maybePLine of
                                                   (Just (pLine, pLineErr)) -> Just $ Motorcycle (seg1, seg2) pLine pLineErr
                                                   Nothing -> Nothing
    -- | Examine two line segments that are part of a Contour, and determine if they are convex from the perspective of the interior of the Contour. if they are, construct a PLine2 bisecting them, pointing toward the interior.
    --   Note that we know that the inside is to the left of the first line given, and that the first line points toward the intersection.
    convexPLines :: Point2 -> Point2 -> Point2 -> Maybe (PLine2, PLine2Err)
    convexPLines p1 p2 p3
      | Just True == pLineIsLeft pl1 pl2 = Nothing
      | otherwise                        = Just resPLine
        where
          resPLine = motorcycleFromPoints p1 p2 p3
          pl1 = fst $ eToPL $ makeLineSeg p1 p2
          pl2 = fst $ eToPL $ makeLineSeg p2 p3

-- | generate the PLine2 of a motorcycle created by the three points given.
motorcycleFromPoints :: Point2 -> Point2 -> Point2 -> (PLine2, PLine2Err)
motorcycleFromPoints p1 p2 p3 = (flipL res, resErr)
  where
    (res, resErr) = getInsideArc firstLine secondLine
      where
        firstLine = join2EP p1 p2
        secondLine = join2EP p2 p3

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

-- | Find where a motorcycle intersects a set of line segments, if it does.
motorcycleMightIntersectWith :: [LineSeg] -> Motorcycle -> Maybe (LineSeg, Either Point2 CPPoint2)
motorcycleMightIntersectWith lineSegs motorcycle
  | null lineSegs = error "no line segments to intersect motorcycle with?"
  | otherwise = case intersections of
                  [] -> Nothing
                  [a] -> filterIntersection a
                  (_:_) -> if len results > 0
                           then Just $ head results
                           else Nothing
  where
    intersections = getMotorcycleSegSetIntersections motorcycle lineSegs
    results :: Slist (LineSeg, Either Point2 CPPoint2)
    results = slist $ sortByDistance $ mapMaybe filterIntersection intersections
    sortByDistance :: [(LineSeg, Either Point2 CPPoint2)] -> [(LineSeg, Either Point2 CPPoint2)]
    sortByDistance = sortBy compareDistances
    motorcyclePoint = cPPointOf motorcycle
    compareDistances i1 i2 = case i1 of
                               (_, Right intersectionPPoint1) ->
                                 case i2 of
                                   (_, Right intersectionPPoint2) ->
                                     distanceBetweenPPoints motorcyclePoint intersectionPPoint1 `compare` distanceBetweenPPoints motorcyclePoint intersectionPPoint2
                                   (_, Left intersectionPoint2) ->
                                     distanceBetweenPPoints motorcyclePoint intersectionPPoint1 `compare` distanceBetweenPPoints motorcyclePoint (eToPP intersectionPoint2)
                               (_, Left intersectionPoint1) ->
                                 case i2 of
                                   (_, Right intersectionPPoint2) ->
                                     distanceBetweenPPoints motorcyclePoint (eToPP intersectionPoint1) `compare` distanceBetweenPPoints motorcyclePoint intersectionPPoint2
                                   (_, Left intersectionPoint2) ->
                                     distanceBetweenPPoints motorcyclePoint (eToPP intersectionPoint1) `compare` distanceBetweenPPoints motorcyclePoint (eToPP intersectionPoint2)
    filterIntersection :: (LineSeg, Either Point2 CPPoint2) -> Maybe (LineSeg, Either Point2 CPPoint2)
    filterIntersection intersection = case intersection of
                                        (_, Right intersectionCPPoint) -> if intersectionCPPointIsBehind intersectionCPPoint
                                                                          then Nothing
                                                                          else Just intersection
                                        (_, Left intersectionPoint) -> if intersectionPointIsBehind intersectionPoint
                                                                       then Nothing
                                                                       else Just intersection
      where
        intersectionPointIsBehind point = oppositeDirection (outOf motorcycle) (eToPLine2 $ makeLineSeg (ePointOf motorcycle) point)
        intersectionCPPointIsBehind pPoint = oppositeDirection (outOf motorcycle) (eToPLine2 $ makeLineSeg (ePointOf motorcycle) (pToEPoint2 pPoint))

-- | Find the closest place where a motorcycle intersects a contour that is not the point where it ejects from.
--   If the motorcycle lands between two segments, return the second line segment, otherwise return the PPoint2 of the intersection with the first LineSeg.
motorcycleIntersectsAt :: Contour -> Motorcycle -> (LineSeg, Either Point2 CPPoint2)
motorcycleIntersectsAt contour motorcycle = case intersections of
                                              [] -> error "no intersections?"
                                              [a] -> fromMaybe (error $ "eliminated my only option\n" <> show a
                                                               ) $ filterIntersection a
                                              manyIntersections@(_:_) -> if len res > 0 then head res else error $ "no options: " <> show (len res) <> "\n" <> show res <> "\n"
                                                where
                                                  res = slist $ sortBy compareDistances $ mapMaybe filterIntersection manyIntersections
                                                  compareDistances :: (LineSeg, Either Point2 CPPoint2) -> (LineSeg, Either Point2 CPPoint2) -> Ordering
                                                  compareDistances i1 i2 = case i1 of
                                                                             (_, Right intersectionPPoint1) ->
                                                                               case i2 of
                                                                                 (_, Right intersectionPPoint2) ->
                                                                                   distanceBetweenPPoints motorcyclePoint intersectionPPoint1 `compare` distanceBetweenPPoints motorcyclePoint intersectionPPoint2
                                                                                 (_, Left intersectionPoint2) ->
                                                                                   distanceBetweenPPoints motorcyclePoint intersectionPPoint1 `compare` distanceBetweenPPoints motorcyclePoint (eToPP intersectionPoint2)
                                                                             (_, Left intersectionPoint1) ->
                                                                               case i2 of
                                                                                 (_, Right intersectionPPoint2) ->
                                                                                   distanceBetweenPPoints motorcyclePoint (eToPP intersectionPoint1) `compare` distanceBetweenPPoints motorcyclePoint intersectionPPoint2
                                                                                 (_, Left intersectionPoint2) ->
                                                                                   distanceBetweenPPoints motorcyclePoint (eToPP intersectionPoint1) `compare` distanceBetweenPPoints motorcyclePoint (eToPP intersectionPoint2)
  where
    filterIntersection :: (LineSeg, Either Point2 CPPoint2) -> Maybe (LineSeg, Either Point2 CPPoint2)
    filterIntersection intersection = case intersection of
                                        (_, Right intersectionPPoint) -> if intersectionPPointIsBehind intersectionPPoint
                                                                         then Nothing
                                                                         else Just intersection
                                        (_, Left intersectionPoint) -> if intersectionPointIsBehind intersectionPoint
                                                                       then Nothing
                                                                       else Just intersection
      where
        intersectionPointIsBehind point = oppositeDirection (outOf motorcycle) (eToPLine2 $ makeLineSeg (ePointOf motorcycle) point)
        intersectionPPointIsBehind pPoint = oppositeDirection (outOf motorcycle) (eToPLine2 $ makeLineSeg (ePointOf motorcycle) (pToEPoint2 pPoint))
    motorcyclePoint = cPPointOf motorcycle
    intersections = getMotorcycleContourIntersections motorcycle contour

-- | Determine if a node is on one side of a motorcycle, or the other.
--   Assumes the starting point of the second line segment is a point on the path.
{-# INLINABLE intersectionSameSide #-}
intersectionSameSide :: (ProjectivePoint2 a, Pointable b) => (a, PPoint2Err) -> b -> Motorcycle -> Maybe Bool
intersectionSameSide point@(pp1, _) node (Motorcycle _ path _)
  | canPoint node && d < ulpVal dErr = Just True
  | canPoint node = pPointsOnSameSideOfPLine (cPPointOf node) pp1 path
  | otherwise = error $ "cannot resolve provided item to a point: " <> show node <> "\n"
    where
      (d, (_,_, dErr)) = distance2PP (cPPointAndErrOf node) point
-- | Check if the output of two motorcycles are anti-collinear with each other.
motorcyclesAreAntiCollinear :: Motorcycle -> Motorcycle -> Bool
motorcyclesAreAntiCollinear motorcycle1 motorcycle2 = plinesIntersectIn (outAndErrOf motorcycle1) (outAndErrOf motorcycle2) == PAntiCollinear

-- | Return the total set of motorcycles in the given CellDivide
motorcyclesInDivision :: CellDivide -> [Motorcycle]
motorcyclesInDivision (CellDivide (DividingMotorcycles a (Slist b _)) _) = a : b

-- Determine the type of the last crash that occured in a crashtree. only useful when we're dealing with a pair motorcycles, and want to find out if we can treat them like one motorcycle.
lastCrashType :: CrashTree -> Maybe CollisionType
lastCrashType crashTree = case lastCrash crashTree of
                            (Just crash) -> if collisionResult crash == HeadOn
                                            then Just HeadOn
                                            else Nothing
                            Nothing -> Nothing
  where
    lastCrash :: CrashTree -> Maybe Collision
    lastCrash (CrashTree _ _ crashes) = case safeLast crashes of
                                          Nothing -> Nothing
                                          (Just crash) -> Just crash
