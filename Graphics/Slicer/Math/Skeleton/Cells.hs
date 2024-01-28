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

-- for deriving stock.
{-# LANGUAGE DerivingStrategies #-}

-- |  This file contains the entry point for the logic and routines required for dividing
--    a contour into cells.
module Graphics.Slicer.Math.Skeleton.Cells (
  INodeDirection(TowardIn, TowardOut),
  addNodeTreesAlongDivide,
  crossoverLinesOfDivision,
  crossoverPointOfDivision,
  endOfDivide,
  findDivisions,
  findFirstCellOfContour,
  findNextCell,
  findRemainder,
  getRawNodeTreeOfCell,
  landingPointOf,
  matchDirectionOfSegments,
  nodeTreeFromDivision,
  nodeTreesDoNotOverlap,
  redirectLastOut,
  startBeforeEnd,
  startOfDivide
  ) where

import Prelude (Bool(True, False), Eq, Ordering(LT, GT, EQ), Show, ($), (>), (<$>), (==), (-), (+), (<>), (&&), (/=), (||), (<), (<=), compare, concat, error, filter, fst, mempty, null, otherwise, show, snd)

import Data.Either (Either(Left, Right))

import Data.List (dropWhile, elemIndex, sortBy, takeWhile)

import qualified Data.List as DL (head)

import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, isJust)

import Slist (isEmpty, len, slist, init)

import qualified Slist as SL (head, last)

import Slist.Type (Slist(Slist), one)

import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour, skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), INodeSet(INodeSet), NodeTree(NodeTree), RemainingContour(RemainingContour), Motorcycle(Motorcycle), Cell(Cell), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), INode, MotorcycleIntersection(WithLineSeg, WithENode, WithMotorcycle), allINodesOf, ancestorsOf, eNodesOfSide, finalINodeOf, finalOutAndErrOf, getFirstLineSeg, getLastLineSeg, insOf, isLoop, makeINode, makeSide, oneSideOf)

import Graphics.Slicer.Math.Skeleton.Motorcycles (CollisionType(HeadOn), CrashTree(CrashTree), motorcyclesInDivision, intersectionSameSide, lastCrashType, motorcyclesAreAntiCollinear, motorcycleToENode, motorcycleMightIntersectWith, motorcycleDivisor, motorcycleIntersectsAt)

import Graphics.Slicer.Math.Skeleton.NodeTrees (MaybeMatch(FirstLast, LastFirst, NoMatch), firstSegOf, lastSegOf, makeNodeTree, mergeNodeTrees)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2, endPoint, lineSegsOfContour, makeLineSeg, startPoint)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.Intersections (intersectionBetweenArcsOf, intersectionOf, isAntiCollinear, noIntersection, outputIntersectsPLineAt)

import Graphics.Slicer.Math.Lossy (distanceBetweenPPoints, distanceBetweenPPointsWithErr, eToPLine2, pToEPoint2)

import Graphics.Slicer.Math.PGA (Arcable(outOf), PLine2Err, Pointable(canPoint, ePointOf), ProjectiveLine, ProjectivePoint, angleBetween2PL, cPPointAndErrOf, distance2PP, eToPL, eToPP, flipL, join2PP, outAndErrOf, interpolate2PL)

-- | Get a raw node tree for a given cell. This does not take into account any input segments from other cells, or divides.
-- Warning: in the cases where the cell has nodes that cross over a divide, you must use tscherne's algorithm to merge two cells.
getRawNodeTreeOfCell :: Cell -> (NodeTree, Slist LineSeg)
getRawNodeTreeOfCell (Cell (Slist [(extSegs@(Slist rawExtSegs _), Nothing)] _))
  | len extSegs < 2 = error $ "Cell has less than two segments, and no remainder?\n" <> show extSegs <> "\n"
  | otherwise = (skeletonOfConcaveRegion (one rawExtSegs) [], mempty)
getRawNodeTreeOfCell (Cell (Slist [(extSegs@(Slist rawExtSegs _), Just _)] _))
  | len extSegs < 2 = error $ "Cell has less than two segments?\n" <> show extSegs <> "\n"
  | otherwise = (skeletonOfConcaveRegion (one rawExtSegs) [], mempty)
getRawNodeTreeOfCell (Cell (Slist [(extSegs1@(Slist rawExtSegs1 _), Just _),(extSegs2@(Slist rawExtSegs2 _), Nothing)] _))
  | len extSegs1 == 1 && len extSegs2 == 1 = (skeletonOfConcaveRegion (mempty) [], slist $ rawExtSegs1 <> rawExtSegs2)
  | len extSegs1 == 1                      = (skeletonOfConcaveRegion (slist [rawExtSegs2]) [], extSegs1)
  | len extSegs2 == 1                      = (skeletonOfConcaveRegion (slist [rawExtSegs1]) [], extSegs2)
  | otherwise                              = (skeletonOfConcaveRegion (slist [rawExtSegs1, rawExtSegs2]) [], mempty)
getRawNodeTreeOfCell input = error
                             $ "unsupported cell layout:\n"
                             <> show input <> "\n"

-- | find the divisions of a given contour without holes. Divisions are points where motorcycles cross the contour.
findDivisions :: Contour -> CrashTree -> [CellDivide]
findDivisions contour crashTree = case motorcyclesIn crashTree of
                                    (Slist [] _) -> []
                                    (Slist [inMC] _) -> [CellDivide (DividingMotorcycles inMC mempty) $ landingPointOf contour inMC]
                                    (Slist [firstMC,secondMC] _) -> if lastCrashType crashTree == Just HeadOn
                                                                    then
                                                                      [CellDivide (DividingMotorcycles firstMC mempty) $ WithMotorcycle secondMC]
                                                                    else if intersectionIsBehind firstMC || intersectionIsBehind secondMC
                                                                         then -- These motorcycles cannot intersect.
                                                                           [CellDivide (DividingMotorcycles firstMC mempty) $ landingPointOf contour firstMC,
                                                                            CellDivide (DividingMotorcycles secondMC mempty) $ landingPointOf contour secondMC]
                                                                         else
                                                                           -- FIXME: We should be able to see if these intersect inside of the contour.
                                                                           -- LOWHANGINGFRUIT: what about two motorcycles that are anticolinear?
                                                                           error "don't know what to do with these motorcycles."
                                      where
                                        intersectionIsBehind m = angleFound <= ulpVal angleErr
                                          where
                                            (angleFound, (_,_, angleErr)) = angleBetween2PL (outOf m) (eToPLine2 $ lineSegToIntersection m)
                                        lineSegToIntersection m = makeLineSeg (ePointOf m) (pToEPoint2 intersectionPPoint)
                                        (intersectionPPoint, _) = fromMaybe (error "has arcs, but no intersection?") $ intersectionBetweenArcsOf firstMC secondMC
                                    (Slist (_:_) _) -> error "too many motorcycles."
  where
    motorcyclesIn (CrashTree motorcycles _ _) = motorcycles

-- | find where the last motorcycle of a divide lands
landingPointOf :: Contour -> Motorcycle -> MotorcycleIntersection
landingPointOf myContour myMotorcycle =
  case eNodesInPath of
    [] -> WithLineSeg $ fst $ motorcycleIntersectsAt myContour myMotorcycle
    -- Yes, we want the ENodes to be slightly smaller than the line segments. this is to pre
    [oneNode] -> if intersectionSegment == getFirstLineSeg oneNode ||
                    intersectionSegment == getLastLineSeg oneNode ||
                    motorcycleENodeDistance <= motorcycleLineSegDistance
                 then WithENode oneNode
                 else WithLineSeg intersectionSegment
      where
        intersectionSegment = fst (motorcycleIntersectsAt myContour myMotorcycle)
        motorcycleENodeDistance = rawMotorcycleENodeDistance - ulpVal motENodeDistanceErr
        (rawMotorcycleENodeDistance, (_,_, motENodeDistanceErr)) = distance2PP cMotorcyclePoint cNodePoint
        motorcycleLineSegDistance = distanceBetweenPPointsWithErr cMotorcyclePoint $ fromMaybe (error "no outArc?") $ outputIntersectsPLineAt myMotorcycle (eToPL $ fst $ motorcycleIntersectsAt myContour myMotorcycle)
        cMotorcyclePoint = cPPointAndErrOf myMotorcycle
        cNodePoint = cPPointAndErrOf oneNode
    (_:_) -> error "more than one opposing exterior node. cannot yet handle this situation."
  where
    eNodesInPath = opposingNodes myContour myMotorcycle
      where
        opposingNodes :: Contour -> Motorcycle -> [ENode]
        opposingNodes c m = filter (\eNode -> isAntiCollinear (outAndErrOf eNode) (outAndErrOf m)) $ eNodesOfOutsideContour c

-- | Find a single Cell of the given contour. always finds the cell on the 'open end' of the contour.
findFirstCellOfContour :: Contour -> [CellDivide] -> ((Cell, Maybe CellDivide), [RemainingContour])
findFirstCellOfContour contour divides = findNextCell $ RemainingContour (slist contourSegs) Nothing divides
  where
    -- | convert a single contour to a single cell.
    contourSegs = lineSegsOfContour contour

-- | Find a single Cell of the given contour. always finds the cell on the 'open end' of the contour.
-- FIXME: implement all of these!
findNextCell :: RemainingContour -> ((Cell, Maybe CellDivide), [RemainingContour])
findNextCell (RemainingContour (Slist [] _) _ _ ) = error "cannot get a cell when the remainder has no line segments."
-- When there are no remaining motorcycles, and there are no holes, we can just treat the whole remainder as a single cell.
findNextCell (RemainingContour lineSegs inDivide []) = case inDivide of
                                                         Nothing -> ((contourFromCell, Nothing), mempty)
                                                         Just oneDivide -> ((contourFromCell, Just oneDivide), mempty)
  where
    contourFromCell = Cell (slist [(lineSegs, Nothing)])
findNextCell (RemainingContour lineSegs inDivide divides) =
      if len (remainingSegmentsOf remainder) < len lineSegs
      then if len (remainingSegmentsOf remainder) > 1
           then ((cell, Just closestDivide), [remainder])
           else errorTooFew
      else errorTooMany
  where
    cell = createCellFromStraightWall lineSegs closestDivide
    remainder = findRemainder cell lineSegs divides
    remainingSegmentsOf (RemainingContour l _ _) = l
    closestDivide = if fst (fst $ SL.head divideClosestSorted) == fst (fst $ SL.head divideFurthestSorted)
                    then snd $ SL.head divideClosestSorted
                    else error $ "Divide collision:\n" <> show divideClosestSorted <> "\n" <> show divideFurthestSorted <> "\n"
    divideClosestSorted = slist $ sortBy (compareDivides lineSegs) $ closestSegOfDivide lineSegs <$> divides
    divideFurthestSorted = slist $ sortBy (compareDivides lineSegs) $ furthestSegOfDivide lineSegs <$> divides
    errorTooFew = error $ "too few segments.\n"
                       <> "lineSegs: " <> show lineSegs <> "\n"
                       <> "divides: " <> show divides <> "\n"
                       <> "inDivide: " <> show inDivide <> "\n"
                       <> "cell: " <> show cell <> "\n"
                       <> "remainingSegment: " <> show (remainingSegmentsOf remainder) <> "\n"
    errorTooMany = error $ "too many segments.\n"
                        <> "lineSegs: " <> show lineSegs <> "\n"
                        <> "divides: " <> show divides <> "\n"
                        <> "inDivide: " <> show inDivide <> "\n"
                        <> "cell: " <> show cell <> "\n"
                        <> "remainingSegment: " <> show (remainingSegmentsOf remainder) <> "\n"
    -- | Find the place where a divide intersects a contour (start OR end), closest to the beginning of the contour.
    closestSegOfDivide :: Slist LineSeg -> CellDivide -> ((LineSeg, Either Point2 ProjectivePoint), CellDivide)
    closestSegOfDivide (Slist contourSegs segCount) divide
      | segCount == 0 = error "no segments!"
      | otherwise = if elemIndex (fst $ startOfDivide contourSegs divide) contourSegs < elemIndex (fst $ endOfDivide divide) contourSegs
                    then (startOfDivide contourSegs divide, divide)
                    else (endOfDivide divide, divide)
    -- | Find the place where a divide intersects a contour (start OR end), closest to the end of the contour
    furthestSegOfDivide :: Slist LineSeg -> CellDivide -> ((LineSeg, Either Point2 ProjectivePoint), CellDivide)
    furthestSegOfDivide (Slist contourSegs segCount) divide
      | segCount == 0 = error "no segments!"
      | otherwise = if elemIndex (fst $ endOfDivide divide) contourSegs < elemIndex (fst $ startOfDivide contourSegs divide) contourSegs
                    then (endOfDivide divide, divide)
                    else (startOfDivide contourSegs divide, divide)
    -- Compare two divides, for sorting.
    compareDivides :: Slist LineSeg -> ((LineSeg, Either Point2 ProjectivePoint), CellDivide) -> ((LineSeg, Either Point2 ProjectivePoint), CellDivide) -> Ordering
    compareDivides (Slist contourSegs segCount) div1 div2
      | segCount == 0 = error "no segments!"
      | otherwise =
      case elemIndex (fst $ fst div1) contourSegs `compare` elemIndex (fst $ fst div2) contourSegs of
        LT -> LT
        GT -> GT
        EQ -> distanceBetweenPPoints (startPPoint $ fst $ fst div1) (toPPoint2 $ snd $ fst div1) `compare` distanceBetweenPPoints (startPPoint $ fst $ fst div2) (toPPoint2 $ snd $ fst div2)
      where
        toPPoint2 :: Either Point2 ProjectivePoint -> ProjectivePoint
        toPPoint2 (Left point2) = eToPP point2
        toPPoint2 (Right ppoint2) = ppoint2
        startPPoint (LineSeg start _) = eToPP start

-- | use a single straight division to cut a section of a contour out, converting it to a cell.
-- Always creates cells from the open side of the list of segments, or, if the divide starts between the end and beginning of the segment list, the seegments toward the end of the list.
-- Always assumes the open side does not create a loop in the contour.
-- NOTE: only handles one divide, not two intersecting divides.
createCellFromStraightWall :: Slist LineSeg -> CellDivide -> Cell
createCellFromStraightWall (Slist [] _) _ = error "empty slist."
createCellFromStraightWall segs@(Slist segments _) divide@(CellDivide (DividingMotorcycles (Motorcycle (inSeg,outSeg) _ _) _) _)
  | resIsID res = dump
  | resIsTooShort res = dump
  | otherwise = Cell $ slist res
  where
    res
      | pointOfFirstMotorcycle divide == startPoint (SL.head segs) = [(leadingRes, Just divide)]
      | isLoop (slist [segments])                                  = [(afterRes, Just divide)]
      | otherwise                                                  = [(preceedingRes, Just divide),
                                                                      (followingRes, Nothing)]
    -- the line segments before the divide intersects the contour, when the divide starts at the first segment.
    leadingRes
      | startBeforeEnd segments divide = segsBeneathForwardDivide segs divide
      | otherwise                      = segsBeneathBackwardDivide segs divide
    -- the line segments after the divide, in a looped sequence.
    afterRes
      | startBeforeEnd segments divide = remainingSegsBackward segs divide inSeg
      | otherwise                      = remainingSegsForward segs divide outSeg
    -- Yes, we're abusing the two functions we call below for their opposite use cases.
    -- the line segments before the divide, in a non-looped sequence.
    preceedingRes
      | startBeforeEnd segments divide = segsAfterForwardDivide segs divide
      | otherwise                      = segsBeforeBackwardDivide segs divide
    -- the line segments after the divide, in a non-looped sequence.
    followingRes
      | startBeforeEnd segments divide = segsBeforeForwardDivide segs divide
      | otherwise                      = segsAfterBackwardDivide segs divide
    pointOfFirstMotorcycle (CellDivide (DividingMotorcycles m _) _) = ePointOf m
    resIsID [(myRes, Just _)] = len myRes == len segs
    resIsID _ = False
    resIsTooShort [(myRes, Just _)] = len myRes == 1 && isLoop (slist [segments])
    resIsTooShort _ = False
    dump = error $ "segs: " <> show segs <> "\n"
                <> show divide <> "\n"
                <> "result: " <> show res <> "\n"
                <> show (atOrAround segs divide) <> "\n"
                <> "preceedingRes: " <> show preceedingRes <> "\n"
                <> "motorcycle at beginning/end: " <> show (pointOfFirstMotorcycle divide == startPoint (SL.head segs)) <> "\n"
                <> "isLoop: " <> show (isLoop $ slist [segments]) <> "\n"
                <> "startBeforeEnd: " <> show (startBeforeEnd segments divide) <> "\n"

-- | use a single straight division to find the portion of a contour remaining after a cell has been cut out.
findRemainder :: Cell -> Slist LineSeg -> [CellDivide] -> RemainingContour
findRemainder (Cell (Slist [] _)) _ _ = error "not enough"
findRemainder (Cell (Slist (_:_:_:_) _)) _ _ = error "too much"
findRemainder (Cell (Slist [(_, Nothing)] _)) _ _ = error "nonsensical"
findRemainder (Cell segSets) allSegs divides
  | null res = error "empty result."
  | len allSegs <= len res = dump
  | len res /= expectedLength = dump
  | len segSets == 1 || len segSets == 2 = RemainingContour res (Just divide) remainingDivides
  | otherwise = error "wtf"
  where
    dump = error $ "dump:\n"
                <> "total segments: " <> show allSegs <> "\n"
                <> "divide:" <> show divide <> "\n"
                <> "cell segments: " <> show segSets <> "\n"
                <> show (atOrAround allSegs divide) <> "\n"
                <> "motorcycle at beginning/end: " <> show (pointOfFirstMotorcycle divide == startPoint (SL.head allSegs)) <> "\n"
                <> "start before end: " <> show myStartBeforeEnd <> "\n"
                <> "result: " <> show res <> "\n"
                <> "expected length: " <> show expectedLength <> "\n"
    res

      | len segSets == 1 && pointOfFirstMotorcycle divide == startPoint (SL.head allSegs) && myStartBeforeEnd = segsAfterForwardDivide allSegs divide
      | len segSets == 1 && pointOfFirstMotorcycle divide == startPoint (SL.head allSegs)                     = segsBeforeBackwardDivide allSegs divide
      | len segSets == 1 && myStartBeforeEnd                                                                  = remainingSegsForward allSegs divide (SL.head firstSegSet)
      | len segSets == 1                                                                                      = remainingSegsBackward allSegs divide (SL.last firstSegSet)
      | len segSets == 2 && myStartBeforeEnd                                                                  = segsBehindForwardDivide allSegs divide
      | len segSets == 2                                                                                      = segsBehindBackwardDivide allSegs divide
      | otherwise = error "wtf"
    expectedLength = case snd (atOrAround allSegs divide) of
                       At -> 1 + len allSegs - lenAllSegSets
                       _ -> len allSegs - lenAllSegSets
      where
        lenAllSegSets
          | len segSets == 1 = len firstSegSet
          | len segSets == 2 = len firstSegSet + len lastSegSet
          | otherwise = error "too many segment sets."
    -- determine whether the motorcycle points toward the beginning of the list of segments, or the end.
    myStartBeforeEnd
      | len segSets == 1 = startBeforeEnd ((\(Slist a _) -> a) firstSegSet) divide
      | len segSets == 2 && pointOfFirstMotorcycle divide == startPoint (SL.last firstSegSet) = True
      | len segSets == 2 && pointOfFirstMotorcycle divide == startPoint (SL.head lastSegSet)  = False
      | len segSets == 2 = error "eep!"
      | otherwise = error "WTF"
    divide
      | len segSets < 3 = divideOfSegSet $ SL.head segSets
      | otherwise = error "wtf"
    lastSegSet = lineSegsOfSegSet $ SL.last segSets
    firstSegSet = lineSegsOfSegSet $ SL.head segSets
    lineSegsOfSegSet :: (Slist LineSeg, Maybe CellDivide) -> Slist LineSeg
    lineSegsOfSegSet = fst
    divideOfSegSet :: (Slist LineSeg, Maybe CellDivide) -> CellDivide
    divideOfSegSet (_, maybeCellDivide) = fromMaybe (error "no divide") maybeCellDivide
    remainingDivides = filter (/= divide) divides
    pointOfFirstMotorcycle (CellDivide (DividingMotorcycles m _) _) = ePointOf m

-- get the remaining segments, when the motorcycle being eliminated starts from a low index segment, and ends at a high index segment.
remainingSegsForward :: Slist LineSeg -> CellDivide -> LineSeg -> Slist LineSeg
remainingSegsForward allSegs divide trimEnd = slist $ case atOrAround allSegs divide of
                                                        (trimStart, Before) ->
                                                          takeWhile (/= trimEnd) $ dropWhile (/= segmentAfter allSegs trimStart) segsDoubled
                                                        (trimStart, At) ->
                                                          takeWhile (/= trimEnd) $ dropWhile (/= trimStart) segsDoubled
                                                        (trimStart, After) ->
                                                          takeWhile (/= trimEnd) $ dropWhile (/= segmentAfter allSegs trimStart) segsDoubled

  where
    -- the list of our segments twice, so that we can cut out of it 'across' the last<->head divide.
    segsDoubled = (\(Slist a _) -> a) $ allSegs <> allSegs

-- get the remaining segments, when the motorcycle being eliminated starts from a high index segment, and ends at a low index segment.
remainingSegsBackward :: Slist LineSeg -> CellDivide -> LineSeg -> Slist LineSeg
remainingSegsBackward allSegs divide trimStart = slist $ case atOrAround allSegs divide of
                                                           (trimEnd, Before) ->
                                                             takeWhile (/= segmentAfter allSegs trimEnd) $ dropWhile (/= segmentAfter allSegs trimStart) segsDoubled
                                                           (trimEnd, At) ->
                                                             takeWhile (/= segmentAfter allSegs trimEnd) $ dropWhile (/= segmentAfter allSegs trimStart) segsDoubled
                                                           (trimEnd, After) ->
                                                             takeWhile (/= trimEnd) $ dropWhile (/= segmentAfter allSegs trimStart) segsDoubled
  where
    -- the list of our segments twice, so that we can cut out of it 'across' the last<->head divide.
    segsDoubled = (\(Slist a _) -> a) $ allSegs <> allSegs

-- If the motorcyccle points toward later than it is emitted, get the segment after the divide.
segsAfterForwardDivide :: Slist LineSeg -> CellDivide -> Slist LineSeg
segsAfterForwardDivide allSegs@(Slist allSegsRaw _) divide = slist $ case atOrAround allSegs divide of
                                                                       (trimEnd, Before) ->
                                                                         dropWhile (/= segmentAfter allSegs trimEnd) allSegsRaw
                                                                       (trimEnd, At) ->
                                                                         dropWhile (/= trimEnd) allSegsRaw
                                                                       (trimEnd, After) ->
                                                                         dropWhile (/= trimEnd) allSegsRaw

-- If the motorcycle points toward earlier than it is emitted, get the segments after the divide.
segsAfterBackwardDivide :: Slist LineSeg -> CellDivide -> Slist LineSeg
segsAfterBackwardDivide (Slist allSegsRaw _) divide = slist $ dropWhile (/= trimEnd) allSegsRaw
  where
    trimEnd = outSegOfDivide divide
    outSegOfDivide (CellDivide (DividingMotorcycles (Motorcycle (_, outSeg) _ _) _) _) = outSeg

-- If the divide does not start between the last segment and the first segment, return the portion of the contour cut off from the ends.
segsBehindForwardDivide :: Slist LineSeg -> CellDivide -> Slist LineSeg
segsBehindForwardDivide allSegs@(Slist allSegsRaw _) divide = slist $ case atOrAround allSegs divide of
                                                                        (trimEnd, Before) ->
                                                                          dropWhile (/= trimStart) $ takeWhile (/= trimEnd) allSegsRaw
                                                                        (trimEnd, At) ->
                                                                          dropWhile (/= trimStart) $ takeWhile (/= segmentAfter allSegs trimEnd) allSegsRaw
                                                                        (trimEnd, After) ->
                                                                          dropWhile (/= trimStart) $ takeWhile (/= segmentAfter allSegs trimEnd) allSegsRaw
  where
    trimStart = outSegOfDivide divide
    outSegOfDivide (CellDivide (DividingMotorcycles (Motorcycle (_, outSeg) _ _) _) _) = outSeg

-- If the divide does not start between the last segment and the first segment, return the portion of the contour cut off from the ends.
segsBehindBackwardDivide :: Slist LineSeg -> CellDivide -> Slist LineSeg
segsBehindBackwardDivide allSegs@(Slist allSegsRaw _) divide = slist $ case atOrAround allSegs divide of
                                                                         (trimStart, Before) ->
                                                                           dropWhile (/= trimStart)                      $ takeWhile (/= trimEnd) allSegsRaw
                                                                         (trimStart, At) ->
                                                                           dropWhile (/= trimStart)                      $ takeWhile (/= trimEnd) allSegsRaw
                                                                         (trimStart, After) ->
                                                                           dropWhile (/= segmentAfter allSegs trimStart) $ takeWhile (/= trimEnd) allSegsRaw
  where
    trimEnd = outSegOfDivide divide
    outSegOfDivide (CellDivide (DividingMotorcycles (Motorcycle (_, outSeg) _ _) _) _) = outSeg

-- If the divide starts between the last segment and the first segment, use this simpler cut routine.
segsBeneathBackwardDivide :: Slist LineSeg -> CellDivide -> Slist LineSeg
segsBeneathBackwardDivide allSegs@(Slist allSegsRaw _) divide = slist $ case atOrAround allSegs divide of
                                                                          (trimEnd, Before) ->
                                                                            dropWhile (/= segmentBefore allSegs trimEnd) allSegsRaw
                                                                          (trimEnd, At) ->
                                                                            dropWhile (/= segmentBefore allSegs trimEnd) allSegsRaw
                                                                          (trimEnd, After) ->
                                                                            dropWhile (/= trimEnd) allSegsRaw

-- If the divide starts between the last segment and the first segment, and the motorcycle points toward later than it is emitted, use this simple cut routine.
segsBeneathForwardDivide :: Slist LineSeg -> CellDivide -> Slist LineSeg
segsBeneathForwardDivide allSegs@(Slist allSegsRaw _) divide = slist $ case atOrAround allSegs divide of
                                                                         (trimStart, Before) ->
                                                                           takeWhile (/= segmentAfter allSegs trimStart) allSegsRaw
                                                                         (trimStart, At) ->
                                                                           takeWhile (/= segmentAfter allSegs trimStart) allSegsRaw
                                                                         (trimStart, After) ->
                                                                           takeWhile (/= trimStart) allSegsRaw

-- If the motorcycle points toward earlier than it is emitted, get the segments before the divide.
segsBeforeBackwardDivide :: Slist LineSeg -> CellDivide -> Slist LineSeg
segsBeforeBackwardDivide allSegs@(Slist allSegsRaw _) divide = slist $ case atOrAround allSegs divide of
                                                                         (trimStart, Before) ->
                                                                           takeWhile (/= trimStart) allSegsRaw
                                                                         (trimStart, At) ->
                                                                           takeWhile (/= segmentAfter allSegs trimStart) allSegsRaw
                                                                         (trimStart, After) ->
                                                                           takeWhile (/= segmentAfter allSegs trimStart) allSegsRaw

-- If the motorcycle points toward later than it is emitted, get the segments before the divide.
segsBeforeForwardDivide :: Slist LineSeg -> CellDivide -> Slist LineSeg
segsBeforeForwardDivide (Slist allSegsRaw _) divide = slist $ takeWhile (/= trimEnd) allSegsRaw
  where
    trimEnd = outSegOfDivide divide
    outSegOfDivide (CellDivide (DividingMotorcycles (Motorcycle (_, outSeg) _ _) _) _) = outSeg

segmentBefore :: Slist LineSeg -> LineSeg -> LineSeg
segmentBefore segs@(Slist rawSegList _) target
  | len segs > 0 = fromMaybe (SL.last segs) $ segBefore target rawSegList
  | SL.head segs == target = SL.last segs
  | otherwise = error "no input segments"
  where
    segBefore _ [] = Nothing
    segBefore _ [_] = Nothing
    segBefore myTarget (x:y:xs)
      | y == myTarget = Just x
      | otherwise = segBefore myTarget (y:xs)

-- | find the segment immediately following a given segment.
-- FIXME: heads possibly incomplete list.
segmentAfter :: Slist LineSeg -> LineSeg -> LineSeg
segmentAfter segs@(Slist rawSegList _) target
  | len segs > 0 = fromMaybe (SL.head segs) $ segAfter target rawSegList
  | SL.last segs == target = SL.head segs
  | otherwise = error "no input segments."
  where
    segAfter _ [] = Nothing
    segAfter _ [_] = Nothing
    segAfter myTarget (x:y:xs)
      | x == myTarget = Just y
      | otherwise = segAfter myTarget (y:xs)

-- | Where the intersection intersects a contour.
data AtOrAround = At -- intersects in the middle of a line segment
                | Before -- intersects at the beginning of a line segment
                | After -- intersects at the end of a line segment
  deriving (Eq, Show)

-- | determine if the end of a divide colides with a segment at the beginning, middle, or end of that segment.
atOrAround :: Slist LineSeg -> CellDivide -> (LineSeg, AtOrAround)
atOrAround allSegs divide = case endOfDivide divide of
                              (seg, Left pt) -> if pt == endPoint (segmentAfter allSegs seg)
                                                then (seg, After)
                                                else (seg, Before)
                              (seg, Right _) -> (seg, At)

-- | determine if the point where the motorcycle of the divide comes out of is closer to the beginning of our segment list than where it lands.
startBeforeEnd :: [LineSeg] -> CellDivide -> Bool
startBeforeEnd segments cellDivide = pointOfFirstMotorcycle cellDivide == startPoint (DL.head segments) ||
                                     elemIndex (fst $ startOfDivide segments cellDivide) segments < elemIndex (fst $ endOfDivide cellDivide) segments
  where
    pointOfFirstMotorcycle (CellDivide (DividingMotorcycles m _) _) = ePointOf m

-- Get the segment the divide intersects that is closest to the beginning of the list of a contour's line segments.
startOfDivide :: [LineSeg] -> CellDivide -> (LineSeg, Either Point2 ProjectivePoint)
startOfDivide _ (CellDivide (DividingMotorcycles (Motorcycle (inSeg, outSeg) _ _) _) _) = (inSeg, Left $ startPoint outSeg)

-- Get the segment the divide intersects that is closest to the end of the list of a contour's line segments.
endOfDivide :: CellDivide -> (LineSeg, Either Point2 ProjectivePoint)
endOfDivide divide = fromMaybe (error "missed!") $ maybeEndOfDivide divide

-- | Get the segment and location on the segment the given divide intersects that is closest to the end of the list of a contour's line segments.
-- FIXME: yes, this is woefully incomplete.
maybeEndOfDivide :: CellDivide -> Maybe (LineSeg, Either Point2 ProjectivePoint)
maybeEndOfDivide (CellDivide (DividingMotorcycles m ms) lastIntersection)
  | isEmpty ms = case lastIntersection of
                   (WithENode eNode) -> Just (getFirstLineSeg eNode, Left $ ePointOf eNode)
                   (WithMotorcycle m1) -> Just (startSegOfMotorcycle m1, Left $ ePointOf m1)
                   (WithLineSeg lineSeg) -> motorcycleMightIntersectWith [lineSeg] m
  | len ms == 1 = case lastIntersection of
                    (WithENode eNode) -> Just (getFirstLineSeg eNode, Left $ ePointOf eNode)
                    (WithMotorcycle m2) -> Just (startSegOfMotorcycle m2, Left $ ePointOf m2)
                    (WithLineSeg lineSeg) -> motorcycleMightIntersectWith [lineSeg] $ SL.head ms
  | otherwise = Nothing
  where
    startSegOfMotorcycle :: Motorcycle -> LineSeg
    startSegOfMotorcycle (Motorcycle (startSeg, _) _ _) = startSeg

-- when constructing an INode representing the divide, what direction should the output point in?
data INodeDirection =
  TowardMotorcycle
  | TowardIn
  | TowardOut
  deriving (Eq, Show)

-- | Add a pair of NodeTrees together along a Divide, to create a new nodeTree.
-- The intersection point for the nodeTrees along the CellDivide is calculated, and then the out of final INode of the two sides is adjusted to pass through that point.
-- NOTE: since a division can generate two non-neighboring nodetrees, make sure to add them to a side first before adding them together..
addNodeTreesAlongDivide :: NodeTree -> NodeTree -> CellDivide -> NodeTree
addNodeTreesAlongDivide nodeTree1@(NodeTree _ maybeINodeSet1) nodeTree2@(NodeTree _ maybeINodeSet2) division = mergeNodeTrees [firstNodeTree, divisionNodeTree, lastNodeTree]
  where
    (firstNodeTree, lastNodeTree)
      | matchDirection == FirstLast = (redirectLastOut nodeTree1 crossoverLine1,
                                       redirectLastOut nodeTree2 crossoverLine2)
      | matchDirection == LastFirst = (redirectLastOut nodeTree1 crossoverLine2,
                                       redirectLastOut nodeTree2 crossoverLine1)
      | otherwise = error "wtf!"
    divisionNodeTree = nodeTreeFromDivision division crossoverIn crossoverOut iNodeOutDirection matchDirection
    (crossoverIn, crossoverOut)
     | isJust maybeINodeSet1 && isJust maybeINodeSet2 = crossoverLinesOfDivision division
     | isJust maybeINodeSet1 && isJust (finalOutAndErrOf nodeTree2) = case matchDirection of
                                                                        FirstLast -> (fst $ crossoverLinesOfDivision division, fromJust $ finalOutAndErrOf nodeTree2)
                                                                        LastFirst -> (fromJust $ finalOutAndErrOf nodeTree2, snd $ crossoverLinesOfDivision division)
                                                                        NoMatch -> error "no match direction"
     | isJust (finalOutAndErrOf nodeTree1) && isJust maybeINodeSet2 = case matchDirection of
                                                                        FirstLast -> (fromJust $ finalOutAndErrOf nodeTree1, snd $ crossoverLinesOfDivision division)
                                                                        LastFirst -> (fst $ crossoverLinesOfDivision division, fromJust $ finalOutAndErrOf nodeTree1)
                                                                        NoMatch -> error "no match direction"
     | isJust (finalOutAndErrOf nodeTree1) && isJust (finalOutAndErrOf nodeTree2) = case matchDirection of
                                                                        FirstLast -> (fromJust $ finalOutAndErrOf nodeTree1, fromJust $ finalOutAndErrOf nodeTree2)
                                                                        LastFirst -> (fromJust $ finalOutAndErrOf nodeTree2, fromJust $ finalOutAndErrOf nodeTree1)
                                                                        NoMatch -> error "no match direction"
     | otherwise = error $ "tried to add a nodetree along a divide, when the nodetree has no output!\n" <> show nodeTree1 <> "\n" <> show (finalOutAndErrOf nodeTree2) <> "\n"
    -- when we create an INode for the divide, what direction should the output be?
    iNodeOutDirection
      | isJust (finalOutAndErrOf nodeTree1) &&
        isJust (finalOutAndErrOf nodeTree2) = TowardMotorcycle
      | matchDirection == FirstLast = TowardOut
      | otherwise = TowardIn
    matchDirection = case matchDirectionOfSegments (firstSegOf nodeTree1) (lastSegOf nodeTree1) division of
                       FirstLast -> FirstLast
                       LastFirst -> LastFirst
                       NoMatch -> case matchDirectionOfSegments (firstSegOf nodeTree2) (lastSegOf nodeTree2) division of
                                    FirstLast -> LastFirst
                                    LastFirst -> FirstLast
                                    NoMatch -> error "could not find match direction."
    (crossoverLine1, crossoverLine2) = crossoverLinesOfDivision division

-- Take two segments, representing the end of an opening of a cell, and determine what side of the divide the cell connects to.
matchDirectionOfSegments :: LineSeg -> LineSeg -> CellDivide -> MaybeMatch
matchDirectionOfSegments firstSegOfNodeTree lastSegOfNodeTree (CellDivide (DividingMotorcycles ((Motorcycle (inSeg, outSeg) _ _)) moreMotorcycles) _)
  | null moreMotorcycles = findMatchDirection
  | otherwise = error "complex divide."
  where
    findMatchDirection
      | firstSegOfNodeTree == inSeg = FirstLast
      | firstSegOfNodeTree == outSeg = LastFirst
      | lastSegOfNodeTree == inSeg = FirstLast
      | lastSegOfNodeTree == outSeg = LastFirst
      | otherwise = NoMatch

-- | Adjust the last inode of the NodeTree's output so that it goes through the line it's supposed to.
redirectLastOut :: NodeTree -> (ProjectiveLine, PLine2Err) -> NodeTree
redirectLastOut inNodeTree@(NodeTree eNodes maybeINodeSet) myCrossoverLine
  | isJust maybeINodeSet =
      case insOf $ finalINodeOf $ fromJust maybeINodeSet of
        [] -> error "unpossible."
        [_] -> NodeTree eNodes $ if null $ ancestorsOf (fromJust maybeINodeSet)
                                 then Nothing
                                 else Just $ pruneParent (fromJust maybeINodeSet)
        (_:_) -> NodeTree eNodes $ Just $ INodeSet childGens $ makeINode (insOf $ finalINodeOf $ fromJust maybeINodeSet) (Just myCrossoverLine)
  -- No INodes? no adjustment, then.
  | otherwise = inNodeTree
  where
    childGens
      | isJust maybeINodeSet = (\(INodeSet foundChildGens _) -> foundChildGens) $ fromJust maybeINodeSet
      | otherwise = error "no inode set to get child gens of."
    pruneParent :: INodeSet -> INodeSet
    pruneParent (INodeSet _ parent)
      | isEmpty childGens = error "tried to prune the last INode from an INodeSet."
      | otherwise = case lastGen of
                      [] -> error "encountered an empty generation."
                      [oneINode] -> INodeSet (init childGens) oneINode
                      manyINodes -> INodeSet (init childGens <> manyINodes `withoutINode` newParent) newParent
      where
        lastGen = SL.last childGens
        withoutINode iNodes iNode = slist [filter (/= iNode) iNodes]
        newParent = DL.head $ filter (\a -> outAndErrOf a == DL.head (insOf parent)) lastGen

-- | Find the single point that a straight skeleton passes through a cell division, assuming that there are no crossoverINodes in the cells on either side of the divide.
crossoverPointOfDivision :: CellDivide -> ProjectivePoint
crossoverPointOfDivision division = case division of
                                      (CellDivide (DividingMotorcycles firstMotorcycle (Slist [] _)) target) -> -- no intersecting ENode, or intersecting Motorcycle.
                                        motorcycleDivisor firstMotorcycle target
                                      (CellDivide _ _) -> error "cannot generate crossoverPoint."

-- | Construct the lines through the crossover point on each side of a CellDivide.
crossoverLinesOfDivision :: CellDivide -> ((ProjectiveLine, PLine2Err), (ProjectiveLine, PLine2Err))
crossoverLinesOfDivision division@(CellDivide (DividingMotorcycles motorcycle@(Motorcycle (motInSeg, motOutSeg) motPL motPLErr) moreMotorcycles) target)
  | isEmpty moreMotorcycles = (firstLine, secondLine)
  | otherwise = error "cannot generate crossoverPoint."
  where
    firstLine
     -- handle antiParallel separately.
     | noIntersection (eToPL motInSeg) (eToPL targetOutSeg) = (\(a, (_,_,b)) -> (a,b)) $ interpolate2PL (flipL $ fst $ eToPL motInSeg) (fst $ eToPL targetOutSeg) motorcycleToCrossoverDistance (motorcycleTravelDistance - motorcycleToCrossoverDistance)
     | otherwise = (\(a,(_,_,b)) -> (flipL a,b)) $ join2PP inIntersect crossoverPoint
    secondLine
     -- handle antiParallel separately.
     | noIntersection (eToPL motOutSeg) (eToPL targetInSeg) = (\(a, (_,_,b)) -> (a,b)) $ interpolate2PL (flipL $ fst $ eToPL motOutSeg) (fst $ eToPL targetInSeg) motorcycleToCrossoverDistance (motorcycleTravelDistance - motorcycleToCrossoverDistance)
     | otherwise = (\(a,(_,_,b)) -> (flipL a,b)) $ join2PP outIntersect crossoverPoint
    crossoverPoint = crossoverPointOfDivision division
    -- FIXME: if noIntersection then translate
    inIntersect = fst $ intersectionOf (eToPL motInSeg) (eToPL targetOutSeg)
    outIntersect = fst $ intersectionOf (eToPL motOutSeg) (eToPL targetInSeg)
    motorcycleTravelDistance = fst $ distance2PP (cPPointAndErrOf motorcycle) (intersectionOf (motPL, motPLErr) (eToPL targetInSeg))
    motorcycleToCrossoverDistance = fst $ distance2PP (cPPointAndErrOf motorcycle) (crossoverPointOfDivision division, mempty)
    (targetInSeg, targetOutSeg) = case target of
                                    (WithLineSeg lineSeg) -> (lineSeg, lineSeg)
                                    (WithENode (ENode (point1, point2, point3) _ _)) -> (makeLineSeg point1 point2, makeLineSeg point2 point3)
                                    (WithMotorcycle (Motorcycle (myMotInSeg, myMotOutSeg) _ _)) -> (myMotInSeg, myMotOutSeg)

-- | Create the NodeTrees corresponding to the CellDivide given.
--   Note that we do not just call crossoverLinesOfDivision, so that the caller can handle cells that have no INodes.
nodeTreeFromDivision :: CellDivide -> (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> INodeDirection -> MaybeMatch -> NodeTree
nodeTreeFromDivision cellDivision@(CellDivide motorcycles target) crossoverIn crossoverOut iNodeDirection matchDirection =
  case motorcycles of
    (DividingMotorcycles _ (Slist [] 0)) -> res
    (DividingMotorcycles firstMotorcycle (Slist [secondMotorcycle] 1)) -> if motorcyclesAreAntiCollinear firstMotorcycle secondMotorcycle
                                                                          then res
                                                                          else errorOut
    (DividingMotorcycles _ (Slist _ _)) -> errorOut
  where
    res = case target of
            (WithENode eNode) -> makeNodeTree ((\a -> makeSide [a]) <$> (eNode : (motorcycleToENode <$> motorcyclesInDivision cellDivision))) (Just $ INodeSet mempty $ iNodeOfENodeDivision cellDivision crossoverIn crossoverOut iNodeDirection matchDirection eNode)
            (WithLineSeg _) -> makeNodeTree ((\a -> makeSide [a]) <$> (motorcycleToENode <$> motorcyclesInDivision cellDivision)) (Just $ INodeSet mempty $ iNodeOfPlainDivision cellDivision crossoverIn crossoverOut iNodeDirection matchDirection)
            (WithMotorcycle _) -> error "intersected a motorcycle?"
    errorOut = error "tried to generate NodeTrees from a non-bilateral cellDivide"

-- | make an INode coresponding to the given division.
-- Note: assumes we intersected with an ENode, which is anticolinear with the motorcycle.
-- Note: If we are using the motorcycle as an out, we use nothing as an out, and use the motorcycle as an in.
iNodeOfENodeDivision :: CellDivide -> (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> INodeDirection -> MaybeMatch -> ENode -> INode
iNodeOfENodeDivision cellDivision crossoverIn crossoverOut iNodeDirection matchDirection eNode
  -- FIXME: we don't care about match direction here? flipping the order causes fun. maybe call a sort instead of hand building this?
  | iNodeDirection == TowardMotorcycle = makeINode [eNodeOut, crossoverIn, motorcycle, crossoverOut] Nothing
  | iNodeDirection == TowardOut && matchDirection == LastFirst = makeINode [motorcycle, crossoverIn, eNodeOut] (Just crossoverOut)
  | iNodeDirection == TowardOut && matchDirection == FirstLast = makeINode [eNodeOut, crossoverIn, motorcycle] (Just crossoverOut)
  | iNodeDirection == TowardIn  && matchDirection == LastFirst = makeINode [motorcycle, crossoverOut, eNodeOut] (Just crossoverIn)
  | iNodeDirection == TowardIn  && matchDirection == FirstLast = makeINode [eNodeOut, crossoverOut, motorcycle] (Just crossoverIn)
  | matchDirection == NoMatch = error "no match!"
  | otherwise = error "wtf!"
  where
    motorcycle = DL.head $ outAndErrOf <$> motorcyclesInDivision cellDivision
    eNodeOut = outAndErrOf eNode

-- | make an INode coresponding to the given division.
-- Note: assumes we intersected with a line segment, and therefore do not need to include it in our INode..
-- Note: If we are using the motorcycle as an out, we use nothing as an out, and use the motorcycle as an in.
iNodeOfPlainDivision :: CellDivide -> (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> INodeDirection -> MaybeMatch -> INode
iNodeOfPlainDivision cellDivision crossoverIn crossoverOut iNodeDirection matchDirection
  -- FIXME: we don't care about match direction here? flipping the order causes fun. maybe call a sort instead of hand building this?
  | iNodeDirection == TowardMotorcycle = makeINode [crossoverIn, motorcycle, crossoverOut] Nothing
  | iNodeDirection == TowardOut && matchDirection == LastFirst = makeINode [motorcycle, crossoverIn] (Just crossoverOut)
  | iNodeDirection == TowardOut && matchDirection == FirstLast = makeINode [crossoverIn, motorcycle] (Just crossoverOut)
  | iNodeDirection == TowardIn  && matchDirection == LastFirst = makeINode [motorcycle, crossoverOut] (Just crossoverIn)
  | iNodeDirection == TowardIn  && matchDirection == FirstLast = makeINode [crossoverOut, motorcycle] (Just crossoverIn)
  | matchDirection == NoMatch = error "no match!"
  | otherwise = error "wtf!"
  where
    motorcycle = DL.head $ outAndErrOf <$> motorcyclesInDivision cellDivision

-- | Check whether the NodeTrees of two cells have an effect on each other.
nodeTreesDoNotOverlap :: NodeTree -> NodeTree -> CellDivide -> Bool
nodeTreesDoNotOverlap nodeTree1 nodeTree2 cellDivide = res
  where
    res = null (crossoverINodes nodeTree1 cellDivide) &&
          null (crossoverINodes nodeTree2 cellDivide)

-- | Given a nodeTree and a closing division, return all of the INodes where the point of the INode is on the opposite side of the division to the points of the ENodes.
crossoverINodes :: NodeTree -> CellDivide -> [INode]
crossoverINodes nodeTree@(NodeTree _ maybeINodeSet) cellDivide
  | isJust maybeINodeSet = case cellDivide of
                             (CellDivide (DividingMotorcycles motorcycle (Slist [] 0)) _) -> res motorcycle
                             -- FIXME: we should be able to find some true cases for more of this.
                             (CellDivide (DividingMotorcycles firstMotorcycle (Slist [secondMotorcycle] 1)) _) -> if motorcyclesAreAntiCollinear firstMotorcycle secondMotorcycle
                                                                                                                  then res firstMotorcycle
                                                                                                                  else mempty
                             -- FIXME: we should be able to find some true cases for this.
                             _ -> mempty

  | otherwise = mempty
  where
    res m = iNodesWhichCrossoverMotorcycle nodeTree m

-- | Given a nodeTree and a closing motorcycle, return all of the INodes where the point of the INode is on the opposite side of the motorcycle.
-- NOTE: this skips checking nodes that cannot be resolved to a point.
iNodesWhichCrossoverMotorcycle :: NodeTree -> Motorcycle -> [INode]
iNodesWhichCrossoverMotorcycle nodeTree@(NodeTree _ maybeINodeSet) dividingMotorcycle
  | isJust maybeINodeSet = filter (nodeCrosses dividingMotorcycle) (filter canPoint $ concat $ (\(Slist a _) -> a) $ allINodesOf $ fromJust maybeINodeSet)
  | otherwise = []
  where
    nodeCrosses :: Motorcycle -> INode -> Bool
    nodeCrosses myDividingMotorcycle iNode = intersectionSameSide pointOnSide iNode myDividingMotorcycle == Just False
    pointOnSide = eToPP $ pointInCell nodeTree
    -- technically a point on the edge of the cell, but this is fine, as long as it's not a point on, or on the other side of the motorcycle.
    pointInCell (NodeTree eNodeSet _) = ePointOf $ DL.head $ eNodesOfSide $ oneSideOf eNodeSet
