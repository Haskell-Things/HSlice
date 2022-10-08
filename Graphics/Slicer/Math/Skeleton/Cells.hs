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
module Graphics.Slicer.Math.Skeleton.Cells (UnsupportedReason(INodeCrossesDivide), findDivisions, findFirstCellOfContour, findNextCell, getNodeTreeOfCell, nodeTreesDoNotOverlap, addNodeTreesAlongDivide, nodeTreesFromDivision, startOfDivide, endOfDivide, findRemainder, createCellFromStraightWalls, gatherLineSegsPreceedingDivide, startBeforeEnd) where

import Prelude (Bool(False), Eq, Ordering(LT, GT, EQ), Show, elem, filter, null, otherwise, ($), (<$>), (==), error, (<>), show, (&&), compare, concat, (/=), (||), (<), (<=), fst, snd, (*), mempty)

import Data.Bifunctor (second)

import Data.Either(Either(Left, Right))

import Data.List (elemIndex, sortBy, dropWhile, takeWhile, nub)

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

import Slist (head, len, slist, safeLast, last, init)

import Slist.Type (Slist(Slist), one)

import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour, skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode, INodeSet(INodeSet), NodeTree(NodeTree), RemainingContour(RemainingContour), Motorcycle(Motorcycle), Cell(Cell), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), INode, MotorcycleIntersection(WithLineSeg, WithENode, WithMotorcycle), finalPLine, getFirstLineSeg, makeINode, insOf, lastINodeOf, isLoop)

import Graphics.Slicer.Math.Ganja (dumpGanja)

import Graphics.Slicer.Math.Skeleton.Motorcycles (CollisionType(HeadOn), CrashTree(CrashTree), motorcyclesInDivision, intersectionSameSide, lastCrashType, motorcyclesAreAntiCollinear, motorcycleToENode, motorcycleMightIntersectWith, motorcycleDivisor, motorcycleIntersectsAt)

import Graphics.Slicer.Math.Skeleton.NodeTrees (firstSegOf, lastSegOf, makeNodeTree, mergeNodeTrees)

import Graphics.Slicer.Math.Contour (lineSegsOfContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2, distance, endPoint, startPoint, fudgeFactor, makeLineSeg)

import Graphics.Slicer.Math.Intersections (outputIntersectsPLine, outputsIntersect, isAntiCollinear)

import Graphics.Slicer.Math.Lossy (distanceBetweenPPoints, eToNPLine2, eToPLine2, pToEPoint2)

import Graphics.Slicer.Math.PGA (Arcable(outOf,errOfOut), Pointable(canPoint, ePointOf, pPointOf), ProjectivePoint, angleBetweenWithErr, eToPPoint2, join2PPointsWithErr)

data UnsupportedReason = INodeCrossesDivide ![(INode,CellDivide)] !NodeTree
  deriving (Show, Eq)

-- | get a naieve node tree for a given cell.
-- Warning: in the cases where the cell has nodes outside of the cell wall, you must use tscherne's algorithm to merge two cells.
getNodeTreeOfCell :: Cell -> Either UnsupportedReason NodeTree
getNodeTreeOfCell (Cell (Slist [(Slist extSegs _, Nothing)] _)) = Right $ skeletonOfConcaveRegion $ one extSegs
getNodeTreeOfCell (Cell (Slist [(Slist extSegs _, Just divide)] _))
  | null $ crossoverINodes res divide = Right res
  | otherwise = Left $ INodeCrossesDivide ((,divide) <$> crossoverINodes res divide) (error $ dumpGanja res)
  where
    res = skeletonOfConcaveRegion $ one extSegs
getNodeTreeOfCell (Cell (Slist [(Slist extSegs1 _, Just divide),(Slist extSegs2 _, Nothing)] _))
  | null (crossoverINodes res divide) = Right res
  | otherwise = Left $ INodeCrossesDivide ((,divide) <$> crossoverINodes res divide) (error $ dumpGanja res)
  where
    res = skeletonOfConcaveRegion (slist [extSegs1, extSegs2])
getNodeTreeOfCell input = error
                          $ "unsupported cell layout:\n"
                          <> show input <> "\n"

-- | find the divisions of a given contour without holes. Divisions are points where motorcycles cross the contour.
findDivisions :: Contour -> CrashTree -> [CellDivide]
findDivisions contour crashTree = case motorcyclesIn crashTree of
                                    (Slist [] _) -> []
                                    (Slist [inMC] _) -> [CellDivide (DividingMotorcycles inMC (Slist [] 0)) $ landingPointOf contour inMC]
                                    (Slist [firstMC,secondMC] _) -> case findDivides crashTree of
                                                                      [] -> error "no divides, but two motorcycles?"
                                                                      [_] -> [CellDivide (DividingMotorcycles firstMC (Slist [] 0)) $ WithMotorcycle secondMC]
                                                                      [_,_] ->
                                                                           [CellDivide (DividingMotorcycles firstMC (Slist [] 0)) $ landingPointOf contour firstMC,
                                                                            CellDivide (DividingMotorcycles secondMC (Slist [] 0)) $ landingPointOf contour secondMC]
                                                                      (_:_) -> error "don't know what to do with these line segments."
                                    (Slist (_:_) _) -> error "too many motorcycles."
  where
    -- | find the divides
    findDivides :: CrashTree -> [[Motorcycle]]
    findDivides myCrashTree = case motorcyclesIn myCrashTree of
                                             (Slist [] _) -> []
                                             (Slist [inMC] _) -> [[inMC]]
                                             (Slist [firstMC, secondMC] _) -> if lastCrashType myCrashTree == Just HeadOn
                                                                              then [[firstMC, secondMC]]
                                                                              else if intersectionIsBehind firstMC || intersectionIsBehind secondMC
                                                                                   then -- These motorcycles cannot intersect.
                                                                                     [[firstMC],[secondMC]]
                                                                                   else -- FIXME: We should be able to see if these intersect inside of the contour.
                                                                                     -- LOWHANGINGFRUIT: what about two motorcycles that are anticolinear?
                                                                                     error "don't know what to do with these motorcycles."
                                               where
                                                 intersectionIsBehind m = angleFound < 0
                                                   where
                                                     (angleFound, _) = angleBetweenWithErr (outOf m) (eToNPLine2 $ lineSegToIntersection m)
                                                 lineSegToIntersection m = makeLineSeg (ePointOf m) (pToEPoint2 intersectionPPoint)
                                                 intersectionPPoint = outputsIntersect firstMC secondMC
                                             (Slist (_:_) _) -> error "too many motorcycles."
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
            cMotorcyclePoint = pPointOf myMotorcycle
            cNodePoint = pPointOf oneNode
            motorcycleENodeDistance = distanceBetweenPPoints cMotorcyclePoint cNodePoint
            motorcycleLineSegDistance = distanceBetweenPPoints cMotorcyclePoint $ outputIntersectsPLine myMotorcycle (eToPLine2 $ fst $ motorcycleIntersectsAt myContour myMotorcycle, mempty)
        (_:_) -> error "more than one opposing exterior node. cannot yet handle this situation."
      where
        eNodesInPath = opposingNodes myContour myMotorcycle
          where
            opposingNodes :: Contour -> Motorcycle -> [ENode]
            opposingNodes c m = filter (\eNode -> isAntiCollinear (outOf eNode,errOfOut eNode) (outOf m,errOfOut m) ) $ eNodesOfOutsideContour c

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
    [] -> -- When there are no motorcycles, and there are no holes, we can just treat the whole contour as a single cell. This does the conversion.
      Just (contourFromCell, Nothing)
      where
        contourFromCell = Cell (slist [(slist lineSegs, Nothing)])
    _ ->
      if len (remainingSegmentsOf remainder) < len (slist lineSegs)
      then Just (cell, Just [remainder])
      else error "too many remaining segments."
      where
        cell = createCellFromStraightWalls (slist [lineSegs]) [closestDivide]
        remainder = findRemainder cell lineSegs divides
        remainingSegmentsOf (RemainingContour l) = l
        closestDivide = if fst (fst $ head divideClosestSorted) == fst ( fst $ head divideFurthestSorted)
                        then snd $ head divideClosestSorted
                        else error $ "Divide collision:\n" <> show divideClosestSorted <> "\n" <> show divideFurthestSorted <> "\n"
        divideClosestSorted = slist $ sortBy (compareDivides lineSegs) $ closestSegOfDivide lineSegs <$> divides
        divideFurthestSorted = slist $ sortBy (compareDivides lineSegs) $ furthestSegOfDivide lineSegs <$> divides
  where
    -- | Find the place where a divide intersects a contour (start OR end), closest to the beginning of the contour
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
        toPPoint2 (Left point2) = eToPPoint2 point2
        toPPoint2 (Right ppoint2) = ppoint2
        startPPoint (LineSeg start _) = eToPPoint2 start

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
                       then RemainingContour $ slist [(remainingSegsForward (last firstSegSet) (head firstSegSet), remainingDivides)]
                       else RemainingContour $ slist [(remainingSegsBackward (head firstSegSet) (last firstSegSet), remainingDivides)]
  | len segSets == 2 = if myStartBeforeEnd
                       then RemainingContour $ slist [(remainingSegsForward (last firstSegSet) (head lastSegSet), remainingDivides)]
                       else RemainingContour $ slist [(remainingSegsBackward (head lastSegSet) (last firstSegSet), remainingDivides)]
  | otherwise = error "wtf"
  where
    divide
      | len segSets < 3 = divideOfSegSet $ head segSets
      | otherwise = error "wtf"
    lastSegSet = lineSegsOfSegSet $ last segSets
    firstSegSet = lineSegsOfSegSet $ head segSets
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
                     where
                   (Just (_, Right _)) -> At
    -- | use the found cell and the motorcycle to determine what direction the motorcycle is going.
    myStartBeforeEnd
      | len segSets == 1 = distance (pointOfFirstMotorcycle divide) (endPoint $ fromMaybe (error "empty list. wth?") $ safeLast firstSegSet) < fudgeFactor*15 ||
                           pointOfFirstMotorcycle divide /= startPoint (head firstSegSet) && error "could not use input cell to determine motorcycle direction."
      | len segSets == 2 = distance (pointOfFirstMotorcycle divide) (endPoint $ fromMaybe (error "empty list. wth?") $ safeLast firstSegSet) < fudgeFactor*15 ||
                           pointOfFirstMotorcycle divide /= startPoint (head lastSegSet) && error "could not use input cell to determine motorcycle direction."
      | otherwise = error "wtf"
    remainingDivides = filter (/= divide) divides
    pointOfFirstMotorcycle (CellDivide (DividingMotorcycles m _) _) = ePointOf m
    contourSegs = slist contourSegList

-- | use a single straight division to cut a section of a contour out, converting it to a cell.
-- Always assumes the open side.
-- Always assumes the open side does not create a loop in the contour.
-- FIXME: only handles one divide, not two intersecting divides.
createCellFromStraightWalls :: Slist [LineSeg] -> [CellDivide] -> Cell
createCellFromStraightWalls (Slist [] _) _ = error "empty slist."
createCellFromStraightWalls (Slist (_:_:_) _) _ = error "too many segsets."
createCellFromStraightWalls _ [] = error "no celldivide."
createCellFromStraightWalls _ (_:_:_) = error "too many celldivides."
createCellFromStraightWalls segSets@(Slist [segments] _) [cellDivide@(CellDivide (DividingMotorcycles motorcycle@(Motorcycle (_,outSeg) _ _) _) _)]
  | len segSets == 0 = error "recieved no line segments. unpossible."
  | isLoop segSets && len segSets == len afterRes = error "passing back full segment list, which should not be possible."
  | isLoop segSets = Cell (slist [(afterRes, Just cellDivide)])
  | len segSets /= len preceedingRes = error "passing back incomplete segment list, which should not be possible."
  | otherwise = Cell (slist [(preceedingRes, Just cellDivide),
                             (followingRes, Nothing)])
  where
    afterRes = gatherLineSegsAfterDivide segments cellDivide outSeg motorcycleOutSegment
    preceedingRes = gatherLineSegsPreceedingDivide segments cellDivide outSeg motorcycleOutSegment
    followingRes = slist gatherLineSegsFollowingDivide
    -- | Return the line segments following the first divide, toward the opening.
    gatherLineSegsFollowingDivide = if startBeforeEnd segments cellDivide
                                    then dropWhile (/= motorcycleOutSegment) segments
                                    else dropWhile (/= outSeg) segments
    -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the former of the two segments (from the beginning of the contour).
    (motorcycleInSegment, eitherMotorcycleOutPoint) = fromMaybe (error "no intersections?") $ motorcycleMightIntersectWith segments motorcycle
    -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the latter of the two segments (from the beginning of the contour).
    motorcycleOutSegment = case eitherMotorcycleOutPoint of
                             (Left point2) -> if distance point2 (endPoint motorcycleInSegment) < fudgeFactor*15
                                              then segmentAfter (slist segments) motorcycleInSegment
                                              else if distance point2 (startPoint motorcycleInSegment) < fudgeFactor*15
                                                   then motorcycleInSegment
                                                   else error $ show point2 <> "\n" <> show segments <> "\n" <> show motorcycleInSegment
                             (Right _) -> motorcycleInSegment

-- | find the segment immediately following a given segment.
segmentAfter :: Slist LineSeg -> LineSeg -> LineSeg
segmentAfter segs@(Slist rawSegList _) target = fromMaybe (head segs) $ segAfter target rawSegList
  where
    segAfter _ [] = Nothing
    segAfter _ [_] = Nothing
    segAfter myTarget (x:y:xs)
      | x == myTarget = Just y
      | otherwise = segAfter myTarget (y:xs)

-- | Return a group of line segments preceeding the given divide, starting from the beginning of the list of segments.
gatherLineSegsPreceedingDivide :: [LineSeg] -> CellDivide -> LineSeg -> LineSeg -> Slist LineSeg
gatherLineSegsPreceedingDivide segments cellDivide stopSegment motorcycleOutSegment
  | startBeforeEnd segments cellDivide = slist $ takeWhile (/= stopSegment) segments
  | otherwise =                          slist $ takeWhile (/= segmentAfter (slist segments) motorcycleOutSegment) segments

-- | Return the line segments after the given divide.
gatherLineSegsAfterDivide :: [LineSeg] -> CellDivide -> LineSeg -> LineSeg -> Slist LineSeg
gatherLineSegsAfterDivide segments cellDivide stopSegment motorcycleOutSegment
  | startBeforeEnd segments cellDivide = forwardRes
  | otherwise = backwardRes
  where
    forwardRes =  slist $ takeWhile (/= segmentAfter (slist segments) motorcycleOutSegment) $ dropWhile (/= stopSegment) segments
    backwardRes = slist $ takeWhile (/= stopSegment)                                        $ dropWhile (/= motorcycleOutSegment) segments

-- | determine if the point where the motorcycle of the divide comes out of is closer to the beginning of our segment list than where it lands.
startBeforeEnd :: [LineSeg] -> CellDivide -> Bool
startBeforeEnd segments cellDivide = elemIndex (fst $ startOfDivide segments cellDivide) segments < elemIndex (fst $ endOfDivide cellDivide) segments

-- Get the segment the divide intersects that is closest to the beginning of the list of a contour's line segments.
startOfDivide :: [LineSeg] -> CellDivide -> (LineSeg, Either Point2 ProjectivePoint)
startOfDivide _ (CellDivide (DividingMotorcycles (Motorcycle (inSeg,LineSeg start _) _ _) _) _) = (inSeg, Left start)

-- Get the segment the divide intersects that is closest to the end of the list of a contour's line segments.
endOfDivide :: CellDivide -> (LineSeg, Either Point2 ProjectivePoint)
endOfDivide divide = fromMaybe (error "missed!") $ maybeEndOfDivide divide

-- | Get the segment and location on the segment the given divide intersects that is closest to the end of the list of a contour's line segments.
-- FIXME: yes, this is woefully incomplete.
maybeEndOfDivide :: CellDivide -> Maybe (LineSeg, Either Point2 ProjectivePoint)
maybeEndOfDivide (CellDivide (DividingMotorcycles m ms) lastIntersection)
  | len ms == 0 = case lastIntersection of
                    (WithENode eNode) -> Just (getFirstLineSeg eNode, Left $ ePointOf eNode)
                    (WithMotorcycle m1) -> Just (startSegOfMotorcycle m1, Left $ ePointOf m1)
                    (WithLineSeg lineSeg) -> motorcycleMightIntersectWith [lineSeg] m
  | len ms == 1 = case lastIntersection of
                    (WithENode eNode) -> Just (getFirstLineSeg eNode, Left $ ePointOf eNode)
                    (WithMotorcycle m2) -> Just (startSegOfMotorcycle m2, Left $ ePointOf m2)
                    (WithLineSeg lineSeg) -> motorcycleMightIntersectWith [lineSeg] $ head ms
  | otherwise = Nothing
  where
    startSegOfMotorcycle :: Motorcycle -> LineSeg
    startSegOfMotorcycle (Motorcycle (startSeg, _) _ _) = startSeg

-- | Add a pair of NodeTrees together along a Divide, to create a new nodeTree.
-- The intersection point for the nodeTrees along the CellDivide is calculated, and then the out of final INode of the two sides is adjusted to pass through that point.
-- NOTE: since a division can generate two non-neighboring nodetrees, make sure to add them to a side first before adding them together..
addNodeTreesAlongDivide :: NodeTree -> NodeTree -> CellDivide -> NodeTree
addNodeTreesAlongDivide nodeTree1 nodeTree2 division = mergeNodeTrees (adjustedNodeTree1:nodeTreesFromDivision division <> [adjustedNodeTree2])
  where
    adjustedNodeTree1 = redirectLastOut nodeTree1 crossoverPoint
    adjustedNodeTree2 = redirectLastOut nodeTree2 crossoverPoint
    -- adjust the last output of the nodetree so that it goes through the point it's supposed to.
    redirectLastOut nodeTree@(NodeTree eNodes iNodeGens@(INodeSet gens)) myCrossover =
      -- Drop INodes with two identical inputs at this stage.
      case nub $ insOf $ lastINodeOf iNodeGens of
        [] -> error "unpossible."
        [_] -> NodeTree eNodes $ INodeSet $ init gens
        (_:_) -> NodeTree eNodes $ INodeSet $ init gens <> one [makeINode (nub $ insOf $ lastINodeOf iNodeGens) (Just myOut)]
          where
            myOut = second (\(_,_,a) -> a) joinOut
            joinOut = join2PPointsWithErr (finalPointOfNodeTree nodeTree) myCrossover
    -- | find the last resolvable point in a NodeTree
    finalPointOfNodeTree (NodeTree _ iNodeGens)
      | canPoint (lastINodeOf iNodeGens) = pPointOf $ lastINodeOf iNodeGens
      | otherwise = error "last INode not pointable?"
    crossoverPoint = case division of
                       (CellDivide (DividingMotorcycles motorcycle1 (Slist [] _)) target) -> -- no eNode, and no opposing motorcycle.
                         motorcycleDivisor motorcycle1 target
                       (CellDivide _ _) -> error "cannot generate crossoverPoint."

-- | Create the NodeTrees corresponding to the CellDivide given.
nodeTreesFromDivision :: CellDivide -> [NodeTree]
nodeTreesFromDivision cellDivision@(CellDivide motorcycles target) = case motorcycles of
                                                                       (DividingMotorcycles _ (Slist [] 0)) -> res
                                                                       (DividingMotorcycles firstMotorcycle (Slist [secondMotorcycle] 1)) -> if motorcyclesAreAntiCollinear firstMotorcycle secondMotorcycle
                                                                                                                                             then res
                                                                                                                                             else errorOut
                                                                       (DividingMotorcycles _ (Slist _ _)) -> errorOut
  where
    res = case target of
            (WithENode eNode) -> [makeNodeTree (motorcycleToENode <$> motorcyclesInDivision cellDivision) (INodeSet $ slist []), makeNodeTree [eNode] (INodeSet $ slist [])]
            (WithLineSeg _) -> [makeNodeTree (motorcycleToENode <$> motorcyclesInDivision cellDivision) (INodeSet $ slist [])]
            (WithMotorcycle _) -> [makeNodeTree (motorcycleToENode <$> motorcyclesInDivision cellDivision) (INodeSet $ slist [])]
    errorOut = error "tried to generate NodeTrees from a non-bilateral cellDivide"

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
                                                             (DividingMotorcycles m (Slist _ 0)) -> outputIntersectsPLine m (finalPLine nt1, mempty) == outputIntersectsPLine m (finalPLine nt2, mempty)
                                                             (DividingMotorcycles _ (Slist _ _)) -> error "cannot yet check outpoint intersections of more than one motorcycle."

-- | Given a nodeTree and it's closing division, return all of the ENodes where the point of the node is on the opposite side of the division.
-- NOTE: this skips checking nodes that cannot be resolved to a point.
crossoverINodes :: NodeTree -> CellDivide -> [INode]
crossoverINodes nodeTree@(NodeTree _ (INodeSet (Slist iNodes _))) cellDivision = filter nodeCrosses (filter canPoint $ concat iNodes)
  where
    nodeCrosses :: INode -> Bool
    nodeCrosses a = Just False `elem` (intersectionSameSide pointOnSide a <$> motorcyclesInDivision cellDivision)
    pointOnSide = eToPPoint2 $ pointInCell nodeTree cellDivision
    pointInCell cell (CellDivide (DividingMotorcycles m _) _)
      | firstSegOf cell == lastCSegOf m = endPoint $ firstSegOf cell
      | lastSegOf cell == firstCSegOf m = startPoint $ lastSegOf cell
      | otherwise = error $ "unhandled case: " <> show cell <> "\n" <> show m <> "\n" <> show (lastSegOf cell) <> "\n" <> show (firstSegOf cell) <> "\n"
      where
        firstCSegOf (Motorcycle (seg1,_) _ _) = seg1
        lastCSegOf (Motorcycle (_, seg2) _ _) = seg2
