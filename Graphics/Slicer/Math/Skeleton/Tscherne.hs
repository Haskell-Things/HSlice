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

{- Purpose of this file: to hold the logic and routines required for building
   a Straight Skeleton of a contour, with a set of sub-contours cut out of it.
   This will contain logic for handling all contours with no holes, using
   the algorithm in Christopher Tscherne's masters thesis.
-}

module Graphics.Slicer.Math.Skeleton.Tscherne (tscherneCheat, leftRegion, rightRegion) where

import Prelude (Bool(True, False), Either(Left, Right), otherwise, ($), last, (<$>), (==), (++), error, length, (&&), head, fst, (<>), show, uncurry, null, (||), filter, (+), Int, (.), zip, (/=), drop, take, (-))

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode, NodeTree(NodeTree), Motorcycle(Motorcycle), Arcable(outOf), pPointOf, linesOfContour, motorcycleToENode)

import Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg))

import Graphics.Slicer.Math.Definitions (Contour, Point2, mapWithNeighbors, addPoints)

import Data.List (elemIndex)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, isJust, fromJust, fromMaybe, isNothing)

import Graphics.Slicer.Math.PGA (PLine2, PPoint2, plinesIntersectIn, Intersection(NoIntersection, HitEndPoint, HitStartPoint), PIntersection(IntersectsIn,PParallel,PAntiParallel), intersectsWith, eToPPoint2, pPointsOnSameSideOfPLine)

-- | use observations from christopher tscherne's masters thesis to cover corner cases that do not require the whole algorithm.
tscherneCheat :: Contour -> Motorcycle -> Maybe (Either Motorcycle ENode) -> Maybe StraightSkeleton
tscherneCheat contour dividingMotorcycle@(Motorcycle (LineSeg rightPoint _, LineSeg startPoint2 endDistance2) path) opposition
  -- If the two sides do not have an influence on one another, and the last line out of the two sides intersects the motorcycle at the same point, tie the sides and the motorcycle together.
  | null (crossoverNodes leftSide leftPoint dividingMotorcycle) &&
    null (crossoverNodes rightSide rightPoint dividingMotorcycle) &&
    isNothing opposition &&
    plinesIntersectIn (finalPLine leftSide) path == plinesIntersectIn (finalPLine rightSide) path =
      Just $ StraightSkeleton [[leftSide, rightSide, NodeTree [motorcycleToENode dividingMotorcycle] []]] []
  -- If the two sides do not have an influence on one another, and the last line out of the two sides intersects the motorcycle at the same point, tie the sides, the motorcycle, and the opposing motorcycle together.
  -- FIXME: ensure that nodeSets are always be stored in clockwise order.
  | null (crossoverNodes leftSide leftPoint dividingMotorcycle) &&
    null (crossoverNodes rightSide rightPoint dividingMotorcycle) &&
    isJust opposition &&
    plinesIntersectIn (finalPLine leftSide) path == plinesIntersectIn (finalPLine rightSide) path =
      Just $ StraightSkeleton [[leftSide, rightSide, NodeTree [motorcycleToENode dividingMotorcycle] [], opposingNodeTree]] []
  | otherwise = Nothing
  | otherwise = error $ "failing to apply Tscherne's method.\n" <>
                        show (crossoverNodes leftSide leftPoint dividingMotorcycle)  <> "\n" <>
                        show (crossoverNodes rightSide rightPoint dividingMotorcycle)  <> "\n" <>
                        show opposition <> "\n" <>
                        show (finalPLine leftSide) <> "\n" <>
                        show (finalPLine rightSide) <> "\n" <>
                        show leftSide <> "\n" <>
                        show rightSide <> "\n" <>
                        show dividingMotorcycle <> "\n"
  where
    opposingNodeTree = NodeTree [cooked (fromJust opposition)] []
      where
        cooked :: (Either Motorcycle ENode) -> ENode
        cooked (Left motorcycle) = motorcycleToENode motorcycle
        cooked (Right eNode) = eNode

    leftSide  = leftRegion contour dividingMotorcycle
    rightSide = rightRegion contour dividingMotorcycle
    leftPoint = addPoints startPoint2 endDistance2
    finalPLine :: NodeTree -> PLine2
    finalPLine (NodeTree _ generations)
      | null generations = error "cannot have final PLine of empty side!\n"
      | otherwise = outOf $ last $ last generations
    crossoverNodes :: NodeTree -> Point2 -> Motorcycle -> [ENode]
    crossoverNodes (NodeTree eNodes _) pointOnSide (Motorcycle _ mcpath) = filter (\a -> Just True == intersectionSameSide mcpath (eToPPoint2 pointOnSide) a) eNodes
  -- | Determine if a node is on one side of a motorcycle, or the other.
  --   Assumes the starting point of the second line segment is a point on the path.
    intersectionSameSide :: PLine2 -> PPoint2 -> ENode -> Maybe Bool
    intersectionSameSide mcpath pointOnSide node = pPointsOnSameSideOfPLine (pPointOf node) pointOnSide mcpath

-- | Calculate a partial straight skeleton, for the part of a contour that is on the left side of the point that a motorcycle's path starts at.
--   meaning we will evaluate the line segments from the point the motorcycle left from, to the segment it intersects, in the order they are in the original contour.
leftRegion :: Contour -> Motorcycle -> NodeTree
leftRegion contour motorcycle = skeletonOfConcaveRegion (matchLineSegments contour motorcycle) False
  where
    -- Return the line segments we're responsible for straight skeletoning.
    matchLineSegments :: Contour -> Motorcycle -> [LineSeg]
    matchLineSegments c (Motorcycle (inSeg,outSeg) path)
      | wrapDirection   = drop stopSegmentIndex (linesOfContour c) ++ take startSegmentIndex (linesOfContour c)
      | unwrapDirection = take (startSegmentIndex - stopSegmentIndex) $ drop stopSegmentIndex $ linesOfContour c
      | otherwise = error "this should be impossible."
        where
          -- test whether we can gather our segments from the stop segment to the end ++ first one until the segment the motorcycle hits...
          wrapDirection   = findSegFromStart c outSeg motorcycleInSegment == outSeg
          -- .. or by starting at the stop segment, and stopping after the segment the motorcycle hits
          unwrapDirection = findSegFromStart c outSeg motorcycleInSegment == motorcycleInSegment
          stopSegmentIndex = segIndex motorcycleOutSegment (linesOfContour c)
          -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the first of the two segments (from the beginning of the contour).
          motorcycleInSegment  = fst motorcycleIntersection
          -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the last of the two segments (from the beginning of the contour).
          motorcycleOutSegment = uncurry fromMaybe motorcycleIntersection
          motorcycleIntersection = motorcycleIntersectsAt c inSeg outSeg path
          startSegmentIndex = segIndex outSeg (linesOfContour c)

-- | Calculate a partial straight skeleton, for the part of a contour that is on the 'right' side of a contour, when the contour is bisected by a motorcycle.
--   by right side, we mean consisting of the segments from the point the motorcycle left from, to the intersection, in the order they are in the original contour.
rightRegion :: Contour -> Motorcycle -> NodeTree
rightRegion contour motorcycle = skeletonOfConcaveRegion (matchLineSegments contour motorcycle) False
  where
    -- Return the line segments we're responsible for straight skeletoning.
    matchLineSegments :: Contour -> Motorcycle -> [LineSeg]
    matchLineSegments c (Motorcycle (inSeg,outSeg) path)
      | wrapDirection   = drop startSegmentIndex (linesOfContour c) ++ take stopSegmentIndex (linesOfContour c)
      | unwrapDirection = take (stopSegmentIndex - startSegmentIndex) $ drop startSegmentIndex $ linesOfContour c
      | otherwise = error "this should be impossible."
        where
          -- test whether we can gather our segments from the stop segment to the end ++ first one until the segment the motorcycle hits...
          wrapDirection    = findSegFromStart c outSeg motorcycleInSegment == motorcycleInSegment
          -- .. or by starting at the stop segment, and stopping after the segment the motorcycle hits
          unwrapDirection  = findSegFromStart c outSeg motorcycleInSegment == outSeg
          stopSegmentIndex = 1 + segIndex motorcycleInSegment (linesOfContour c)
          -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the first of the two segments (from the beginning of the contour).
          motorcycleInSegment  = fst motorcycleIntersection
          -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the last of the two segments (from the beginning of the contour).
          -- motorcycleOutSegment = fromMaybe (fst motorcycleIntersection) (snd motorcycleIntersection)
          motorcycleIntersection = motorcycleIntersectsAt c inSeg outSeg path
          startSegmentIndex = segIndex outSeg (linesOfContour c)

-- | Get the index of a specific segment, in a list of segments.
segIndex :: LineSeg -> [LineSeg] -> Int
segIndex seg segs = fromMaybe (error "cannot find item") $ elemIndex seg segs

-- | Find where a motorcycle intersects a contour, if the motorcycle is emitted from between the two given segments.
--   If the motorcycle lands between two segments, return the second segment, as well.
motorcycleIntersectsAt :: Contour -> LineSeg -> LineSeg -> PLine2 -> (LineSeg, Maybe LineSeg)
motorcycleIntersectsAt contour inSeg outSeg path
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

-- | Search a contour starting at the beginning, and return the first of the two line segments given
findSegFromStart :: Contour -> LineSeg -> LineSeg -> LineSeg
findSegFromStart c seg1 seg2 = head $ catMaybes $ foundSeg seg1 seg2 <$> linesOfContour c
  where
    foundSeg s1 s2 sn
      | sn == s1  = Just s1
      | sn == s2  = Just s2
      | otherwise = Nothing

-- | Apply Christopher Tscherne's algorithm from his master's thesis.
