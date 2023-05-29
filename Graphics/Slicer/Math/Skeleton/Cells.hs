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

-- for making (,thing) a function
{-# LANGUAGE TupleSections #-}

-- |  This file contains the entry point for the logic and routines required for dividing
--    a contour into cells.
module Graphics.Slicer.Math.Skeleton.Cells (
  UnsupportedReason(INodeCrossesDivide),
  addNodeTreesAlongDivide,
  crossoverLinesOfDivision,
  crossoverPointOfDivision,
  endOfDivide,
  findDivisions,
  findFirstCellOfContour,
  findNextCell,
  findRemainder,
  gatherLineSegsPreceedingDivide,
  getNodeTreeOfCell,
  getRawNodeTreeOfCell,
  landingPointOf,
  nodeTreesDoNotOverlap,
  startBeforeEnd,
  startOfDivide
  ) where

import Prelude (Bool(False), Eq, Ordering(LT, GT, EQ), Show, ($), (<$>), (==), (-), (<>), (&&), (/=), (||), (<), (<=), compare, concat, elem, error, filter, fst, mempty, null, otherwise, show, snd)

import Data.Either (Either(Left, Right))

import Data.List (elemIndex, sortBy, dropWhile, takeWhile)

import qualified Data.List as DL (head)

import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, isJust)

import Slist (isEmpty, len, slist, safeLast, last, init)

import qualified Slist as SL (head)

import Slist.Type (Slist(Slist), one)

import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour, skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), INodeSet(INodeSet), NodeTree(NodeTree), RemainingContour(RemainingContour), Motorcycle(Motorcycle), Cell(Cell), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), INode, MotorcycleIntersection(WithLineSeg, WithENode, WithMotorcycle), allINodesOf, ancestorsOf, finalOutAndErrOf, finalPLine, getFirstLineSeg, makeINode, insOf, finalINodeOf, isLoop)

import Graphics.Slicer.Math.Skeleton.Motorcycles (CollisionType(HeadOn), CrashTree(CrashTree), motorcyclesInDivision, intersectionSameSide, lastCrashType, motorcyclesAreAntiCollinear, motorcycleToENode, motorcycleMightIntersectWith, motorcycleDivisor, motorcycleIntersectsAt)

import Graphics.Slicer.Math.Skeleton.NodeTrees (MaybeMatch(FirstLast, LastFirst, NoMatch), firstSegOf, lastSegOf, makeNodeTree, mergeNodeTrees)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2, endPoint, lineSegsOfContour, makeLineSeg, startPoint)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.Intersections (intersectionBetweenArcsOf, intersectionOf, isAntiCollinear, noIntersection, outputIntersectsPLineAt)

import Graphics.Slicer.Math.Lossy (distanceBetweenPPoints, distanceBetweenPPointsWithErr, eToPLine2, pToEPoint2)

import Graphics.Slicer.Math.PGA (Arcable(outOf), PLine2Err, Pointable(canPoint, ePointOf), ProjectiveLine, ProjectivePoint, angleBetween2PL, cPPointAndErrOf, distance2PP, eToPL, eToPP, flipL, join2PP, outAndErrOf, interpolate2PL)

data UnsupportedReason = INodeCrossesDivide ![(INode,CellDivide)] !NodeTree
  deriving Show

-- | get a node tree for a given cell.
-- Warning: in the cases where the cell has nodes outside of the cell wall, you must use tscherne's algorithm to merge two cells.
getNodeTreeOfCell :: Cell -> Either UnsupportedReason NodeTree
getNodeTreeOfCell (Cell (Slist [(Slist extSegs _, Nothing)] _)) = Right $ skeletonOfConcaveRegion $ one extSegs
getNodeTreeOfCell (Cell (Slist [(Slist extSegs _, Just divide)] _))
  | null $ crossoverINodes res divide = Right res
  | otherwise = Left $ INodeCrossesDivide ((,divide) <$> crossoverINodes res divide) (error "whoops. should not have evaluated me.")
  where
    res = skeletonOfConcaveRegion $ one extSegs
getNodeTreeOfCell (Cell (Slist [(Slist extSegs1 _, Just divide),(Slist extSegs2 _, Nothing)] _))
  | null (crossoverINodes res divide) = Right res
  | otherwise = Left $ INodeCrossesDivide ((,divide) <$> crossoverINodes res divide) (error "whoops. also should not have evaluated me.")
  where
    res = skeletonOfConcaveRegion (slist [extSegs1, extSegs2])
getNodeTreeOfCell input = error
                          $ "unsupported cell layout:\n"
                          <> show input <> "\n"

-- | get a raw node tree for a given cell.
-- Warning: in the cases where the cell has nodes outside of the cell wall, you must use tscherne's algorithm to merge two cells.
getRawNodeTreeOfCell :: Cell -> NodeTree
getRawNodeTreeOfCell (Cell (Slist [(Slist extSegs _, Nothing)] _)) = skeletonOfConcaveRegion $ one extSegs
getRawNodeTreeOfCell (Cell (Slist [(Slist extSegs _, Just _)] _)) = skeletonOfConcaveRegion $ one extSegs
getRawNodeTreeOfCell (Cell (Slist [(Slist extSegs1 _, Just _),(Slist extSegs2 _, Nothing)] _)) = skeletonOfConcaveRegion (slist [extSegs1, extSegs2])
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
    [oneNode] -> if motorcycleENodeDistance <= motorcycleLineSegDistance
                 then WithENode oneNode
                 else WithLineSeg $ fst $ motorcycleIntersectsAt myContour myMotorcycle
      where
        cMotorcyclePoint = cPPointAndErrOf myMotorcycle
        cNodePoint = cPPointAndErrOf oneNode
        motorcycleENodeDistance = distanceBetweenPPointsWithErr cMotorcyclePoint cNodePoint
        motorcycleLineSegDistance = distanceBetweenPPointsWithErr cMotorcyclePoint $ fromMaybe (error "no outArc?") $ outputIntersectsPLineAt myMotorcycle (eToPL $ fst $ motorcycleIntersectsAt myContour myMotorcycle)
    (_:_) -> error "more than one opposing exterior node. cannot yet handle this situation."
  where
    eNodesInPath = opposingNodes myContour myMotorcycle
      where
        opposingNodes :: Contour -> Motorcycle -> [ENode]
        opposingNodes c m = filter (\eNode -> isAntiCollinear (outAndErrOf eNode) (outAndErrOf m)) $ eNodesOfOutsideContour c

-- | Find a single Cell of the given contour. always finds the cell on the 'open end' of the contour.
findFirstCellOfContour :: Contour -> [CellDivide] -> Maybe (Cell, Maybe [RemainingContour])
findFirstCellOfContour contour divides = findNextCell (RemainingContour (slist [(slist contourSegs,divides)]))
  where
    -- | convert a single contour to a single cell.
    contourSegs = lineSegsOfContour contour

-- | Find a single Cell of the given contour. always finds the cell on the 'open end' of the contour.
-- FIXME: implement all of these!
findNextCell :: RemainingContour -> Maybe (Cell, Maybe [RemainingContour])
findNextCell (RemainingContour (Slist [] _) ) = error "cannot handle no contour remainders."
findNextCell (RemainingContour (Slist (_:_:_) _) ) = error "cannot handle multiple contour remainders."
findNextCell (RemainingContour (Slist [(Slist lineSegs _, divides)] _) ) =
  case divides of
    [] -> -- When there are no motorcycles, and there are no holes, we can just treat the whole remainder as a single cell.
      Just (contourFromCell, Nothing)
      where
        -- | This does the conversion.
        contourFromCell = Cell (slist [(slist lineSegs, Nothing)])
    _ ->
      if len (remainingSegmentsOf remainder) < len (slist lineSegs)
      then Just (cell, Just [remainder])
      else error "too many remaining segments."
      where
        cell = createCellFromStraightWall (slist [lineSegs]) closestDivide
        remainder = findRemainder cell lineSegs divides
        remainingSegmentsOf (RemainingContour l) = l
        closestDivide = if fst (fst $ SL.head divideClosestSorted) == fst (fst $ SL.head divideFurthestSorted)
                        then snd $ SL.head divideClosestSorted
                        else error $ "Divide collision:\n" <> show divideClosestSorted <> "\n" <> show divideFurthestSorted <> "\n"
        divideClosestSorted = slist $ sortBy (compareDivides lineSegs) $ closestSegOfDivide lineSegs <$> divides
        divideFurthestSorted = slist $ sortBy (compareDivides lineSegs) $ furthestSegOfDivide lineSegs <$> divides
  where
    -- | Find the place where a divide intersects a contour (start OR end), closest to the beginning of the contour.
    closestSegOfDivide :: [LineSeg] -> CellDivide -> ((LineSeg, Either Point2 ProjectivePoint), CellDivide)
    closestSegOfDivide contourSegs divide = if elemIndex (fst $ startOfDivide contourSegs divide) contourSegs < elemIndex (fst $ endOfDivide divide) contourSegs
                                            then (startOfDivide contourSegs divide, divide)
                                            else (endOfDivide divide, divide)
    -- | Find the place where a divide intersects a contour (start OR end), closest to the end of the contour
    furthestSegOfDivide :: [LineSeg] -> CellDivide -> ((LineSeg, Either Point2 ProjectivePoint), CellDivide)
    furthestSegOfDivide contourSegs divide = if elemIndex (fst $ endOfDivide divide) contourSegs < elemIndex (fst $ startOfDivide contourSegs divide) contourSegs
                                             then (endOfDivide divide, divide)
                                             else (startOfDivide contourSegs divide, divide)
    -- Compare two divides, for sorting.
    compareDivides :: [LineSeg] -> ((LineSeg, Either Point2 ProjectivePoint), CellDivide) -> ((LineSeg, Either Point2 ProjectivePoint), CellDivide) -> Ordering
    compareDivides contourSegs div1 div2 =
      case elemIndex (fst $ fst div1) contourSegs `compare` elemIndex (fst $ fst div2) contourSegs of
        LT -> LT
        GT -> GT
        EQ -> distanceBetweenPPoints (startPPoint $ fst $ fst div1) (toPPoint2 $ snd $ fst div1) `compare` distanceBetweenPPoints (startPPoint $ fst $ fst div2) (toPPoint2 $ snd $ fst div2)
      where
        toPPoint2 :: Either Point2 ProjectivePoint -> ProjectivePoint
        toPPoint2 (Left point2) = eToPP point2
        toPPoint2 (Right ppoint2) = ppoint2
        startPPoint (LineSeg start _) = eToPP start

-- | Where the intersection intersects a contour.
data AtOrAround = At
                | Before
                | After

-- | use a single straight division to find the portion of a contour remaining after a cell has been cut out.
findRemainder :: Cell -> [LineSeg] -> [CellDivide] -> RemainingContour
findRemainder (Cell (Slist [] _)) _ _ = error "not enough"
findRemainder (Cell (Slist (_:_:_:_) _)) _ _ = error "too much"
findRemainder (Cell (Slist [(_, Nothing)] _)) _ _ = error "nonsensical"
findRemainder (Cell segSets) contourSegList divides
  | len segSets == 1 = if myStartBeforeEnd
                       then RemainingContour $ slist [(remainingSegsForward (last firstSegSet) (SL.head firstSegSet), remainingDivides)]
                       else RemainingContour $ slist [(remainingSegsBackward (SL.head firstSegSet) (last firstSegSet), remainingDivides)]
  | len segSets == 2 = if myStartBeforeEnd
                       then RemainingContour $ slist [(remainingSegsForward (last firstSegSet) (SL.head lastSegSet), remainingDivides)]
                       else RemainingContour $ slist [(remainingSegsBackward (SL.head lastSegSet) (last firstSegSet), remainingDivides)]
  | otherwise = error "wtf"
  where
    divide
      | len segSets < 3 = divideOfSegSet $ SL.head segSets
      | otherwise = error "wtf"
    lastSegSet = lineSegsOfSegSet $ last segSets
    firstSegSet = lineSegsOfSegSet $ SL.head segSets
    lineSegsOfSegSet :: (Slist LineSeg, Maybe CellDivide) -> Slist LineSeg
    lineSegsOfSegSet = fst
    divideOfSegSet :: (Slist LineSeg, Maybe CellDivide) -> CellDivide
    divideOfSegSet (_, maybeCellDivide) = fromMaybe (error "no divide") maybeCellDivide
    remainingSegsForward trimStart trimEnd = case atOrAround of
                                               Before ->
                                                 slist $ takeWhile (/= trimEnd)                                                       $ dropWhile (/= segmentAfter contourSegs trimStart) (contourSegList <> contourSegList)
                                               At ->
                                                 slist $ takeWhile (/= segmentAfter contourSegs trimEnd)                              $ dropWhile (/= segmentAfter contourSegs trimStart) (contourSegList <> contourSegList)
                                               After ->
                                                 slist $ takeWhile (/= (segmentAfter contourSegs $ segmentAfter contourSegs trimEnd)) $ dropWhile (/= segmentAfter contourSegs trimStart) (contourSegList <> contourSegList)
    remainingSegsBackward trimStart trimEnd = case atOrAround of
                                                Before ->
                                                  slist $ takeWhile (/= trimStart) $ dropWhile (/= segmentBefore trimEnd) (contourSegList <> contourSegList)
                                                At ->
                                                  slist $ takeWhile (/= trimStart) $ dropWhile (/= trimEnd) (contourSegList <> contourSegList)
                                                After ->
                                                  slist $ takeWhile (/= trimStart) $ dropWhile (/= segmentAfter contourSegs trimEnd) (contourSegList <> contourSegList)
    segmentBefore seg = fromMaybe (fromMaybe (error "empty list?" ) $ safeLast contourSegs) $ segBefore seg contourSegList
      where
        segBefore _ [] = Nothing
        segBefore _ [_] = Nothing
        segBefore mySeg (x:y:xs)
            | y == mySeg = Just x
            | otherwise = segBefore mySeg (y:xs)
    atOrAround = case maybeEndOfDivide divide of
                   Nothing -> error $ "missed!\n" <> show contourSegs <> "\n" <> show divide <> "\n" <> show segSets <> "\n"
                   (Just (_, Left pt)) -> if pt == endPoint (segmentAfter contourSegs $ fst $ endOfDivide divide)
                                          then After
                                          else Before
                   (Just (_, Right _)) -> At
    -- | use the found cell and the motorcycle to determine what direction the motorcycle is going.
    myStartBeforeEnd
      | len segSets == 1 = distance <= ulpVal distanceErr ||
                           pointOfFirstMotorcycle divide /= startPoint (SL.head firstSegSet) && error "could not use input cell to determine motorcycle direction."
      | len segSets == 2 = distance <= ulpVal distanceErr ||
                           pointOfFirstMotorcycle divide /= startPoint (SL.head lastSegSet) && error "could not use input cell to determine motorcycle direction."
      | otherwise = error "wtf"
      where
        (distance, (_,_, distanceErr)) = distance2PP (cPPointAndErrOfFirstMotorcycle divide) (eToPP $ endPoint $ fromMaybe (error "empty list. wth?") $ safeLast firstSegSet, mempty)
    remainingDivides = filter (/= divide) divides
    cPPointAndErrOfFirstMotorcycle (CellDivide (DividingMotorcycles m _) _) = cPPointAndErrOf m
    pointOfFirstMotorcycle (CellDivide (DividingMotorcycles m _) _) = ePointOf m
    contourSegs = slist contourSegList

-- | use a single straight division to cut a section of a contour out, converting it to a cell.
-- Always creates cells from the open side of the list of segments.
-- Always assumes the open side does not create a loop in the contour.
-- NOTE: only handles one divide, not two intersecting divides.
createCellFromStraightWall :: Slist [LineSeg] -> CellDivide -> Cell
createCellFromStraightWall (Slist [] _) _ = error "empty slist."
createCellFromStraightWall (Slist (_:_:_) _) _ = error "too many segsets."
createCellFromStraightWall segSets@(Slist [segments] _) cellDivide@(CellDivide (DividingMotorcycles motorcycle@(Motorcycle (_,outSeg) _ _) _) _)
  | isLoop segSets && len segSets == len afterRes = error "passing back full segment list, which should not be possible."
  | isLoop segSets = Cell $ slist [(afterRes, Just cellDivide)]
  | len segSets /= len preceedingRes = error "passing back incomplete segment list, which should not be possible."
  | otherwise = Cell $ slist [(preceedingRes, Just cellDivide),
                              (followingRes, Nothing)]
  where
    -- the line segments before the divide, in a looped sequence.
    preceedingRes = gatherLineSegsPreceedingDivide segments cellDivide outSeg motorcycleOutSegment
    -- the line segments after the divide, in a looped sequence.
    followingRes = gatherLineSegsFollowingDivide segments cellDivide outSeg motorcycleOutSegment
    -- the line segments after the divide, in a non-looped sequence.
    afterRes = gatherLineSegsAfterDivide segments cellDivide outSeg motorcycleOutSegment
    -- the first segment that a motorcycle intersects the contour on, andor if it intersected between two segments, the former of the two segments (from the beginning of the contour).
    (motorcycleInSegment, eitherMotorcycleOutPoint) = fromMaybe (error "no intersections?") $ motorcycleMightIntersectWith segments motorcycle
    -- the segment that a motorcycle intersects the contour at, or if it intersected between two segments, the latter of the two segments (from the beginning of the contour).
    motorcycleOutSegment = case eitherMotorcycleOutPoint of
                             (Left point2) -> if endDistance <= ulpVal endDistanceErr
                                              then segmentAfter (slist segments) motorcycleInSegment
                                              else if startDistance <= ulpVal startDistanceErr
                                                   then motorcycleInSegment
                                                   else error $ show point2 <> "\n" <> show segments <> "\n" <> show motorcycleInSegment
                               where
                                 (endDistance, (_,_, endDistanceErr)) = distance2PP (eToPP point2, mempty) (eToPP $ endPoint motorcycleInSegment, mempty)
                                 (startDistance, (_,_, startDistanceErr)) = distance2PP (eToPP point2, mempty) (eToPP $ startPoint motorcycleInSegment, mempty)
                             (Right _) -> motorcycleInSegment

-- | find the segment immediately following a given segment.
-- FIXME: heads possibly incomplete list.
segmentAfter :: Slist LineSeg -> LineSeg -> LineSeg
segmentAfter segs@(Slist rawSegList _) target = fromMaybe (SL.head segs) $ segAfter target rawSegList
  where
    segAfter _ [] = Nothing
    segAfter _ [_] = Nothing
    segAfter myTarget (x:y:xs)
      | x == myTarget = Just y
      | otherwise = segAfter myTarget (y:xs)

-- | Return the line segments preceeding the given divide, starting from the beginning of the list of segments.
gatherLineSegsPreceedingDivide :: [LineSeg] -> CellDivide -> LineSeg -> LineSeg -> Slist LineSeg
gatherLineSegsPreceedingDivide segments cellDivide stopSegment motorcycleOutSegment
  | startBeforeEnd segments cellDivide = slist $ takeWhile (/= stopSegment) segments
  | otherwise =                          slist $ takeWhile (/= segmentAfter (slist segments) motorcycleOutSegment) segments

-- | Return the line segments after the given divide.
gatherLineSegsAfterDivide :: [LineSeg] -> CellDivide -> LineSeg -> LineSeg -> Slist LineSeg
gatherLineSegsAfterDivide segments cellDivide stopSegment motorcycleOutSegment
  | startBeforeEnd segments cellDivide = slist $ takeWhile (/= segmentAfter (slist segments) motorcycleOutSegment) $ dropWhile (/= stopSegment) segments
  | otherwise =                          slist $ takeWhile (/= stopSegment)                                        $ dropWhile (/= motorcycleOutSegment) segments

-- | Return the line segments following the divide, toward the opening.
gatherLineSegsFollowingDivide :: [LineSeg] -> CellDivide -> LineSeg -> LineSeg -> Slist LineSeg
gatherLineSegsFollowingDivide segments cellDivide stopSegment motorcycleOutSegment
  | startBeforeEnd segments cellDivide = slist $ dropWhile (/= motorcycleOutSegment) segments
  | otherwise =                          slist $ dropWhile (/= stopSegment) segments

-- | determine if the point where the motorcycle of the divide comes out of is closer to the beginning of our segment list than where it lands.
startBeforeEnd :: [LineSeg] -> CellDivide -> Bool
startBeforeEnd segments cellDivide = elemIndex (fst $ startOfDivide segments cellDivide) segments < elemIndex (fst $ endOfDivide cellDivide) segments

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
  deriving Eq

-- | Add a pair of NodeTrees together along a Divide, to create a new nodeTree.
-- The intersection point for the nodeTrees along the CellDivide is calculated, and then the out of final INode of the two sides is adjusted to pass through that point.
-- NOTE: since a division can generate two non-neighboring nodetrees, make sure to add them to a side first before adding them together..
addNodeTreesAlongDivide :: NodeTree -> NodeTree -> CellDivide -> NodeTree
addNodeTreesAlongDivide nodeTree1@(NodeTree _ maybeINodeSet1) nodeTree2@(NodeTree _ maybeINodeSet2) division = mergeNodeTrees [adjustedNodeTree1, divisionNodeTree, adjustedNodeTree2]
  where
    (adjustedNodeTree1, adjustedNodeTree2)
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
                                                                        _ -> error "no match direction"
     | isJust (finalOutAndErrOf nodeTree1) && isJust maybeINodeSet2 = case matchDirection of
                                                                        FirstLast -> (fromJust $ finalOutAndErrOf nodeTree1, snd $ crossoverLinesOfDivision division)
                                                                        LastFirst -> (fst $ crossoverLinesOfDivision division, fromJust $ finalOutAndErrOf nodeTree1)
                                                                        _ -> error "no match direction"
     | isJust (finalOutAndErrOf nodeTree1) && isJust (finalOutAndErrOf nodeTree2) = case matchDirection of
                                                                        FirstLast -> (fromJust $ finalOutAndErrOf nodeTree1, fromJust $ finalOutAndErrOf nodeTree2)
                                                                        LastFirst -> (fromJust $ finalOutAndErrOf nodeTree2, fromJust $ finalOutAndErrOf nodeTree1)
                                                                        _ -> error "no match direction"
     | otherwise = error "tried to add a nodetree along a divide, when the nodetree has no output!"
    matchDirection =
      case division of
        -- one motorcycle, hits one target.
        (CellDivide (DividingMotorcycles ((Motorcycle (inSeg, outSeg) _ _)) (Slist [] 0)) _) -> findMatchDirection
          where
            findMatchDirection
              | firstSegOf nodeTree1 == inSeg = FirstLast
              | firstSegOf nodeTree1 == outSeg = LastFirst
              | lastSegOf nodeTree1 == inSeg = FirstLast
              | lastSegOf nodeTree1 == outSeg = LastFirst
              | firstSegOf nodeTree2 == inSeg = LastFirst
              | firstSegOf nodeTree2 == outSeg = FirstLast
              | lastSegOf nodeTree2 == inSeg = LastFirst
              | lastSegOf nodeTree2 == outSeg = FirstLast
              | otherwise = NoMatch
        _ -> error "oh no"
    -- adjust the last output of the NodeTree so that it goes through the point it's supposed to.
    redirectLastOut :: NodeTree -> (ProjectiveLine, PLine2Err) -> NodeTree
    redirectLastOut inNodeTree@(NodeTree eNodes maybeINodeSet) myCrossoverLine
      | isJust maybeINodeSet =
        case insOf $ finalINodeOf $ fromJust maybeINodeSet of
          [] -> error "unpossible."
          [_] -> NodeTree eNodes $ if ancestorsOf (fromJust maybeINodeSet) == []
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
                          (manyINodes) -> INodeSet (init childGens <> manyINodes `withoutINode` newParent) newParent
          where
            lastGen = last childGens
            withoutINode iNodes iNode = slist [filter (\a -> a /= iNode) iNodes]
            newParent = DL.head $ filter (\a -> outAndErrOf a == DL.head (insOf parent)) lastGen
    -- when we create an INode for the divide, what direction should the output be?
    iNodeOutDirection
      | isJust (finalOutAndErrOf nodeTree1) &&
        isJust (finalOutAndErrOf nodeTree2) = TowardMotorcycle
      | matchDirection == FirstLast = TowardOut
      | otherwise = TowardIn
    (crossoverLine1, crossoverLine2) = crossoverLinesOfDivision division

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
            (WithENode eNode) -> makeNodeTree (eNode : (motorcycleToENode <$> motorcyclesInDivision cellDivision)) (Just $ INodeSet mempty $ iNodeOfENodeDivision cellDivision crossoverIn crossoverOut iNodeDirection matchDirection eNode)
            (WithLineSeg _) -> makeNodeTree (motorcycleToENode <$> motorcyclesInDivision cellDivision) (Just $ INodeSet mempty $ iNodeOfPlainDivision cellDivision crossoverIn crossoverOut iNodeDirection matchDirection)
            (WithMotorcycle _) -> error "intersected a motorcycle?"
    errorOut = error "tried to generate NodeTrees from a non-bilateral cellDivide"

-- | make an INode coresponding to the given division.
-- Note: assumes we intersected with an ENode, which is anticolinear with the motorcycle.
-- Note: If we are using the motorcycle as an out, we use nothing as an out, and use the motorcycle as an in.
iNodeOfENodeDivision :: CellDivide -> (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> INodeDirection -> MaybeMatch -> ENode -> INode
iNodeOfENodeDivision cellDivision crossoverIn crossoverOut iNodeDirection matchDirection eNode
  | iNodeDirection == TowardOut && matchDirection == LastFirst = makeINode [motorcycle, crossoverIn, eNodeOut] (Just crossoverOut)
  | iNodeDirection == TowardOut && matchDirection == FirstLast = makeINode [eNodeOut, crossoverIn, motorcycle] (Just crossoverOut)
  | iNodeDirection == TowardIn  && matchDirection == LastFirst = makeINode [motorcycle, crossoverOut, eNodeOut] (Just crossoverIn)
  | iNodeDirection == TowardIn  && matchDirection == FirstLast = makeINode [eNodeOut, crossoverOut, motorcycle] (Just crossoverIn)
  | iNodeDirection == TowardMotorcycle && matchDirection == LastFirst = makeINode [crossoverOut, motorcycle, crossoverIn, eNodeOut] Nothing
  | iNodeDirection == TowardMotorcycle && matchDirection == FirstLast = makeINode [eNodeOut, crossoverIn, motorcycle, crossoverOut] Nothing
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
  | iNodeDirection == TowardOut && matchDirection == LastFirst = makeINode [motorcycle, crossoverIn] (Just crossoverOut)
  | iNodeDirection == TowardOut && matchDirection == FirstLast = makeINode [crossoverIn, motorcycle] (Just crossoverOut)
  | iNodeDirection == TowardIn  && matchDirection == LastFirst = makeINode [motorcycle, crossoverOut] (Just crossoverIn)
  | iNodeDirection == TowardIn  && matchDirection == FirstLast = makeINode [crossoverOut, motorcycle] (Just crossoverIn)
  | iNodeDirection == TowardMotorcycle && matchDirection == LastFirst = makeINode [crossoverOut, motorcycle, crossoverIn] Nothing
  | iNodeDirection == TowardMotorcycle && matchDirection == FirstLast = makeINode [crossoverIn, motorcycle, crossoverOut] Nothing
  | matchDirection == NoMatch = error "no match!"
  | otherwise = error "wtf!"
  where
    motorcycle = DL.head $ outAndErrOf <$> motorcyclesInDivision cellDivision

-- | Check whether the NodeTrees of two cells have an effect on each other.
nodeTreesDoNotOverlap :: NodeTree -> NodeTree -> CellDivide -> Bool
nodeTreesDoNotOverlap nodeTree1 nodeTree2 cellDivide@(CellDivide motorcycles1 _) = case motorcycles1 of
                                                                                     (DividingMotorcycles _ (Slist [] 0)) -> res
                                                                                     (DividingMotorcycles firstMotorcycle (Slist [secondMotorcycle] 1)) -> motorcyclesAreAntiCollinear firstMotorcycle secondMotorcycle && res
                                                                                     (DividingMotorcycles _ (Slist _ _)) -> False
  where
    res = null (crossoverINodes nodeTree1 cellDivide) &&
          null (crossoverINodes nodeTree2 cellDivide) &&
          lastOutsIntersect nodeTree1 nodeTree2 cellDivide
    -- | Check that the outputs of the NodeTrees collide at the same point at the division between the two cells the NodeTrees correspond to.
    lastOutsIntersect :: NodeTree -> NodeTree -> CellDivide -> Bool
    lastOutsIntersect nt1 nt2 (CellDivide motorcycles _) = case motorcycles of
                                                             (DividingMotorcycles m (Slist _ 0)) -> d <= ulpVal dErr
                                                               where
                                                                 (d, (_,_, dErr)) = distance2PP point1 point2
                                                                 point1 = fromMaybe (error "no outArc?") $ outputIntersectsPLineAt m (finalPLine nt1)
                                                                 point2 = fromMaybe (error "no outArc?") $ outputIntersectsPLineAt m (finalPLine nt2)
                                                             (DividingMotorcycles _ (Slist _ _)) -> error "cannot yet check outpoint intersections of more than one motorcycle."

-- | Given a nodeTree and a closing division, return all of the INodes where the point of the INode is on the opposite side of the division.
-- NOTE: this skips checking nodes that cannot be resolved to a point.
crossoverINodes :: NodeTree -> CellDivide -> [INode]
crossoverINodes nodeTree@(NodeTree _ maybeINodeSet) cellDivision
  | isJust maybeINodeSet = filter nodeCrosses (filter canPoint $ concat $ (\(Slist a _) -> a) $ allINodesOf $ fromJust maybeINodeSet)
  | otherwise = []
  where
    nodeCrosses :: INode -> Bool
    nodeCrosses a = Just False `elem` (intersectionSameSide pointOnSide a <$> motorcyclesInDivision cellDivision)
    pointOnSide = eToPP $ pointInCell nodeTree cellDivision
    pointInCell cell (CellDivide (DividingMotorcycles m _) _)
      | firstSegOf cell == lastCSegOf m = endPoint $ firstSegOf cell
      | lastSegOf cell == firstCSegOf m = startPoint $ lastSegOf cell
      | otherwise = error $ "unhandled case: " <> show cell <> "\n" <> show m <> "\n" <> show (lastSegOf cell) <> "\n" <> show (firstSegOf cell) <> "\n"
      where
        firstCSegOf (Motorcycle (seg1,_) _ _) = seg1
        lastCSegOf (Motorcycle (_, seg2) _ _) = seg2
