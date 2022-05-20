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

import Prelude (Bool(True, False), Either(Left,Right), Eq((==)), Show(show), Ordering (EQ, GT, LT), (&&), (<>), ($), (<$>), (<), (>), (+), compare, error, notElem, null, otherwise, realToFrac, zip)

import Prelude as PL (init, last)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, fromMaybe)

import Data.List (sortBy)

import Slist (slist, safeLast, head, len)

import Slist as SL (filter)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Contour (pointsOfContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2, distance, mapWithNeighbors, startPoint, lineSegFromEndpoints, handleLineSegError)

import Graphics.Slicer.Math.Intersections (getMotorcycleSegSetIntersections, getMotorcycleContourIntersections, intersectionOf, isAntiCollinear, noIntersection)

import Graphics.Slicer.Math.Line (endPoint)

import Graphics.Slicer.Math.PGA (NPLine2(NPLine2), PLine2(PLine2), PPoint2, Arcable(outOf), Pointable(canPoint, ePointOf, pPointOf), eToPLine2, eToNPLine2, flipPLine2, pLineIsLeft, pPointsOnSameSideOfPLine, PIntersection(IntersectsIn,PAntiCollinear), plinesIntersectIn, plineFromEndpoints, pToEPoint2, angleBetweenWithErr, distanceBetweenPPoints, eToPPoint2, normalizePLine2, outputIntersectsLineSeg, pPointBetweenPPoints, translatePerp, ulpOfPLine2)

import Graphics.Slicer.Math.Skeleton.Definitions (Motorcycle(Motorcycle), ENode(ENode), getFirstLineSeg, linePairs, CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), MotorcycleIntersection(WithLineSeg, WithENode, WithMotorcycle))

import Graphics.Slicer.Math.GeometricAlgebra (addVecPair, UlpSum(UlpSum))

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
motorcycleToENode (Motorcycle (seg1,seg2) mcpath mcUlp mcMag) = ENode (startPoint seg1, startPoint seg2, endPoint seg2) mcpath mcUlp mcMag

-- | Find the point where the propogation from a motorcycle equals the propogation of what it impacts, taking into account the weight of a motorcycle, and the weight of what it impacts.
motorcycleDivisor :: Motorcycle -> MotorcycleIntersection -> PPoint2
motorcycleDivisor motorcycle target = pPointBetweenPPoints (pPointOf motorcycle) pointOfTarget (mSpeedOf motorcycle) (tSpeedOf target)
  where
    pointOfTarget :: PPoint2
    pointOfTarget = case target of
                      (WithLineSeg lineSeg) -> case outputIntersectsLineSeg motorcycle (lineSeg, UlpSum 0) of
                                        (Right (IntersectsIn p _)) -> p
                                        v -> error $ "impossible!\n" <> show v <> "\n" <> show lineSeg <> "\n" <> show motorcycle <> "\n" <> show target <> "\n"
                      (WithENode eNode) -> pPointOf eNode
                      (WithMotorcycle motorcycle2) -> pPointOf motorcycle2
    tSpeedOf :: MotorcycleIntersection -> ℝ
    tSpeedOf myTarget = case myTarget of
                  (WithLineSeg lineSeg) -> distanceBetweenPPoints (justIntersectsIn $ plinesIntersectIn (translatePerp (eToPLine2 lineSeg) 1) (outOf motorcycle)) (justIntersectsIn $ plinesIntersectIn (eToPLine2 lineSeg) (outOf motorcycle))
                  (WithENode eNode) -> distanceBetweenPPoints (pPointOf eNode) (justIntersectsIn $ plinesIntersectIn (translatePerp (eToPLine2 $ getFirstLineSeg eNode) 1) (outOf eNode))
                  (WithMotorcycle motorcycle2) -> mSpeedOf motorcycle2
    mSpeedOf :: Motorcycle -> ℝ
    mSpeedOf myMotorcycle@(Motorcycle (seg1,_) _ _ _) = distanceBetweenPPoints (pPointOf myMotorcycle) (justIntersectsIn $ plinesIntersectIn (translatePerp (eToPLine2 seg1) 1) (outOf myMotorcycle))
    justIntersectsIn :: PIntersection -> PPoint2
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
          crashOf mot1 mot2@(Motorcycle (inSeg2, outSeg2) _ _ _)
            -- If we have a clear path between mot1 and the origin of mot2
            | isAntiCollinear (outOf mot1) (outOf mot2) && motorcycleIntersectsAt contour mot1 == (inSeg2, Left outSeg2) = Just $ Collision (mot1,mot2, slist []) Nothing HeadOn
            | noIntersection (outOf mot1) (outOf mot2) = Nothing
            | intersectionIsBehind mot1 = Nothing
            | intersectionIsBehind mot2 = Nothing
            -- FIXME: this should be providing a distance to intersectionPPoint along the motorcycle to check.
            | otherwise = case getMotorcycleContourIntersections mot1 contour of
                          [] -> case distanceBetweenPPoints (pPointOf mot1) intersectionPPoint `compare` distanceBetweenPPoints (pPointOf mot2) intersectionPPoint of
                                 GT -> Just $ Collision (mot1, mot2, slist []) (Just mot2) Normal
                                 LT -> Just $ Collision (mot1, mot2, slist []) (Just mot1) Normal
                                 EQ -> Just $ Collision (mot1, mot2, slist []) (Just mot1) SideSwipe
                          _ -> Nothing
              where
                intersectionPPoint = intersectionOf (outOf mot1) (outOf mot2)
                intersectionIsBehind m = angleFound < realToFrac angleErr
                  where
                    (angleFound, UlpSum angleErr) = angleBetweenWithErr (normalizePLine2 $ outOf m) (eToNPLine2 $ lineSegToIntersection m)
                lineSegToIntersection m = handleLineSegError $ lineSegFromEndpoints (ePointOf m) (pToEPoint2 intersectionPPoint)

-- | Find the non-reflex virtexes of a contour and draw motorcycles from them. Useful for contours that are a 'hole' in a bigger contour.
--   This function is meant to be used on the exterior contour.
convexMotorcycles :: Contour -> [Motorcycle]
convexMotorcycles contour = catMaybes $ onlyMotorcycles <$> zip (rotateLeft $ linePairs contour) (mapWithNeighbors convexPLines $ pointsOfContour contour)
  where
    rotateLeft a = PL.last a : PL.init a
    onlyMotorcycles :: ((LineSeg, LineSeg), Maybe (PLine2, UlpSum, ℝ)) -> Maybe Motorcycle
    onlyMotorcycles ((seg1, seg2), maybePLine) = case maybePLine of
                                                   (Just (pLine, pLineUlp, pLineMag)) -> Just $ Motorcycle (seg1, seg2) pLine pLineUlp pLineMag
                                                   Nothing -> Nothing
    -- | Examine two line segments that are part of a Contour, and determine if they are convex from the perspective of the interior of the Contour. if they are, construct a PLine2 bisecting them, pointing toward the interior.
    --   Note that we know that the inside is to the left of the first line given, and that the first line points toward the intersection.
    convexPLines :: Point2 -> Point2 -> Point2 -> Maybe (PLine2, UlpSum, ℝ)
    convexPLines p1 p2 p3
      | Just True == pLineIsLeft pl1 pl2 = Nothing
      | otherwise                        = Just (resPLine, ulpOfPLine2 resPLine, distance p1 p2 + distance p2 p3)
        where
          resPLine = motorcycleFromPoints p1 p2 p3
          pl1 = plineFromEndpoints p1 p2
          pl2 = plineFromEndpoints p2 p3

-- | generate the un-normalized PLine2 of a motorcycle created by the three points given.
motorcycleFromPoints :: Point2 -> Point2 -> Point2 -> PLine2
motorcycleFromPoints p1 p2 p3 = getOutsideArc (plineFromEndpoints p1 p2) (plineFromEndpoints p2 p3)
  where
    -- | Get a PLine along the angle bisector of the intersection of the two given line segments, pointing in the 'obtuse' direction.
    --   Note that we do not normalize our output, or require normalized inputs, but we normalize our values before adding them.
    getOutsideArc :: PLine2 -> PLine2 -> PLine2
    getOutsideArc pline1 pline2
      | noIntersection pline1 pline2 = error
                                       $ "not collinear, but not intersecting?\n"
                                       <> "PLine1: " <> show pline1 <> "\n"
                                       <> "PLine2: " <> show pline2 <> "\n"
                                       <> "result: " <> show (plinesIntersectIn pline1 pline2) <> "\n"
      | otherwise = flipPLine2 $ PLine2 $ addVecPair flippedNPV1 npv2
      where
        (PLine2 flippedNPV1) = flipPLine2 $ (\(NPLine2 a) -> PLine2 a) $ normalizePLine2 pline1
        (NPLine2 npv2) = normalizePLine2 pline2

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
motorcycleMightIntersectWith :: [LineSeg] -> Motorcycle -> Maybe (LineSeg, Either Point2 PPoint2)
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
    results :: Slist (LineSeg, Either Point2 PPoint2)
    results = slist $ sortByDistance $ catMaybes $ filterIntersection <$> intersections
    sortByDistance :: [(LineSeg, Either Point2 PPoint2)] -> [(LineSeg, Either Point2 PPoint2)]
    sortByDistance = sortBy compareDistances
    compareDistances i1 i2 = case i1 of
                               (_, Right intersectionPPoint1) ->
                                 case i2 of
                                   (_, Right intersectionPPoint2) ->
                                     distanceBetweenPPoints (pPointOf motorcycle) intersectionPPoint1 `compare` distanceBetweenPPoints (pPointOf motorcycle) intersectionPPoint2
                                   (_, Left intersectionPoint2) ->
                                     distanceBetweenPPoints (pPointOf motorcycle) intersectionPPoint1 `compare` distanceBetweenPPoints (pPointOf motorcycle) (eToPPoint2 intersectionPoint2)
                               (_, Left intersectionPoint1) ->
                                 case i2 of
                                   (_, Right intersectionPPoint2) ->
                                     distanceBetweenPPoints (pPointOf motorcycle) (eToPPoint2 intersectionPoint1) `compare` distanceBetweenPPoints (pPointOf motorcycle) intersectionPPoint2
                                   (_, Left intersectionPoint2) ->
                                     distanceBetweenPPoints (pPointOf motorcycle) (eToPPoint2 intersectionPoint1) `compare` distanceBetweenPPoints (pPointOf motorcycle) (eToPPoint2 intersectionPoint2)
    filterIntersection :: (LineSeg, Either Point2 PPoint2) -> Maybe (LineSeg, Either Point2 PPoint2)
    filterIntersection intersection = case intersection of
                                        (_, Right intersectionPPoint) -> if intersectionPPointIsBehind intersectionPPoint
                                                                         then Nothing
                                                                         else Just intersection
                                        (_, Left intersectionPoint) -> if intersectionPointIsBehind intersectionPoint
                                                                       then Nothing
                                                                       else Just intersection
      where
        intersectionPointIsBehind point = angleFound < realToFrac angleErr
          where
            (angleFound, UlpSum angleErr) = angleBetweenWithErr (normalizePLine2 $ outOf motorcycle) (eToNPLine2 $ lineSegToIntersection point)
        intersectionPPointIsBehind pPoint = angleFound < realToFrac angleErr
          where
            (angleFound, UlpSum angleErr) = angleBetweenWithErr (normalizePLine2 $ outOf motorcycle) (eToNPLine2 $ lineSegToIntersectionP pPoint)
        lineSegToIntersection myPoint = handleLineSegError $ lineSegFromEndpoints (ePointOf motorcycle) myPoint
        lineSegToIntersectionP myPPoint = handleLineSegError $ lineSegFromEndpoints (ePointOf motorcycle) (pToEPoint2 myPPoint)

-- | Find the closest place where a motorcycle intersects a contour that is not the point where it ejects from.
--   If the motorcycle lands between two segments, return the second line segment, otherwise return the PPoint2 of the intersection with the first LineSeg.
motorcycleIntersectsAt :: Contour -> Motorcycle -> (LineSeg, Either LineSeg PPoint2)
motorcycleIntersectsAt contour motorcycle = case intersections of
                                              [] -> error "no intersections?"
                                              [a] -> fromMaybe (error $ "eliminated my only option\n" <> show a
                                                               ) $ filterIntersection a
                                              manyIntersections@(_:_) -> if len res > 0 then head res else error $ "no options: " <> show (len res) <> "\n" <> show res <> "\n"
                                                where
                                                  res = slist $ sortBy compareDistances $ catMaybes $ filterIntersection <$> manyIntersections
                                                  compareDistances :: (LineSeg, Either LineSeg PPoint2) -> (LineSeg, Either LineSeg PPoint2) -> Ordering
                                                  compareDistances i1 i2 = case i1 of
                                                                             (_, Right intersectionPPoint1) ->
                                                                               case i2 of
                                                                                 (_, Right intersectionPPoint2) ->
                                                                                   distanceBetweenPPoints (pPointOf motorcycle) intersectionPPoint1 `compare` distanceBetweenPPoints (pPointOf motorcycle) intersectionPPoint2
                                                                                 (_, Left (LineSeg intersectionPoint2 _)) ->
                                                                                   distanceBetweenPPoints (pPointOf motorcycle) intersectionPPoint1 `compare` distanceBetweenPPoints (pPointOf motorcycle) (eToPPoint2 intersectionPoint2)
                                                                             (_, Left (LineSeg intersectionPoint1 _)) ->
                                                                               case i2 of
                                                                                 (_, Right intersectionPPoint2) ->
                                                                                   distanceBetweenPPoints (pPointOf motorcycle) (eToPPoint2 intersectionPoint1) `compare` distanceBetweenPPoints (pPointOf motorcycle) intersectionPPoint2
                                                                                 (_, Left (LineSeg intersectionPoint2 _)) ->
                                                                                   distanceBetweenPPoints (pPointOf motorcycle) (eToPPoint2 intersectionPoint1) `compare` distanceBetweenPPoints (pPointOf motorcycle) (eToPPoint2 intersectionPoint2)
  where
    filterIntersection :: (LineSeg, Either LineSeg PPoint2) -> Maybe (LineSeg, Either LineSeg PPoint2)
    filterIntersection intersection = case intersection of
                                        (_, Right intersectionPPoint) -> if intersectionPPointIsBehind intersectionPPoint
                                                                         then Nothing
                                                                         else Just intersection
                                        (_, Left (LineSeg intersectionPoint _)) -> if intersectionPointIsBehind intersectionPoint
                                                                                   then Nothing
                                                                                   else Just intersection
      where
        intersectionPointIsBehind point = angleFound < realToFrac angleErr
          where
            (angleFound, UlpSum angleErr) = angleBetweenWithErr (normalizePLine2 $ outOf motorcycle) (eToNPLine2 $ lineSegToIntersection point)
        intersectionPPointIsBehind pPoint = angleFound < realToFrac angleErr
          where
            (angleFound, UlpSum angleErr) = angleBetweenWithErr (normalizePLine2 $ outOf motorcycle) (eToNPLine2 $ lineSegToIntersectionP pPoint)
        lineSegToIntersection myPoint = handleLineSegError $ lineSegFromEndpoints (ePointOf motorcycle) myPoint
        lineSegToIntersectionP myPPoint = handleLineSegError $ lineSegFromEndpoints (ePointOf motorcycle) (pToEPoint2 myPPoint)
    intersections = getMotorcycleContourIntersections motorcycle contour

-- | Determine if a node is on one side of a motorcycle, or the other.
--   Assumes the starting point of the second line segment is a point on the path.
intersectionSameSide :: (Pointable a, Show a) => PPoint2 -> a -> Motorcycle -> Maybe Bool
intersectionSameSide pointOnSide node (Motorcycle _ path _ _)
  | canPoint node && pPointOf node == pointOnSide = Just True
  | canPoint node = pPointsOnSameSideOfPLine (pPointOf node) pointOnSide path
  | otherwise = error $ "cannot resolve provided item to a point: " <> show node <> "\n"

-- | Check if the output of two motorcycles are anti-collinear with each other.
motorcyclesAreAntiCollinear :: Motorcycle -> Motorcycle -> Bool
motorcyclesAreAntiCollinear motorcycle1 motorcycle2 = plinesIntersectIn (outOf motorcycle1) (outOf motorcycle2) == PAntiCollinear

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
