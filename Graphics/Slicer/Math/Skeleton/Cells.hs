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
module Graphics.Slicer.Math.Skeleton.Cells (RemainingContour, UnsupportedReason(INodeCrossesDivide), findDivisions, findFirstCellOfContour, findNextCell, getNodeTreeOfCell, nodeTreesDoNotOverlap, addNodeTreesAlongDivide, nodeTreesFromDivision) where

import Prelude (Bool(False), Eq, Ordering(LT, GT, EQ), Show, elem, filter, null, otherwise, ($), (<$>), (==), error, (<>), show, (&&), compare, concat, (/=), (||), (<), (<=), fst, snd)

import Data.Either(Either(Left, Right))

import Data.List (elemIndex, sortBy, dropWhile, takeWhile)

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

import Slist (head, len, slist, safeLast, last, init)

import Slist.Type (Slist(Slist), one)

import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour, skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), INodeSet(INodeSet), NodeTree(NodeTree), RemainingContour(RemainingContour), Motorcycle(Motorcycle), Cell(Cell), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), INode, MotorcycleIntersection(WithLineSeg, WithENode, WithMotorcycle), ePointOf, finalPLine, intersectionOf, outOf, makeINode, insOf, lastINodeOf, pPointOf)

import Graphics.Slicer.Math.Ganja (dumpGanja)

import Graphics.Slicer.Math.Skeleton.Motorcycles (CollisionType(HeadOn), CrashTree(CrashTree), motorcyclesInDivision, intersectionSameSide, lastCrashType, motorcyclesAreAntiCollinear, motorcycleToENode, motorcycleMightIntersectWith, motorcycleDivisor, motorcycleIntersectsAt)

import Graphics.Slicer.Math.Skeleton.NodeTrees (firstSegOf, lastSegOf, makeNodeTree, mergeNodeTrees)

import Graphics.Slicer.Math.Contour (lineSegsOfContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2, startPoint)

import Graphics.Slicer.Math.Line (endPoint, handleLineSegError, lineSegFromEndpoints)

import Graphics.Slicer.Math.PGA (PPoint2, PIntersection(PAntiCollinear, IntersectsIn), angleBetween, distanceBetweenPPoints, eToPLine2, eToPPoint2, pToEPoint2, plinesIntersectIn, join2PPoint2)

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
                                                                      _ -> error "don't know what to do with these line segments."
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
                                                 intersectionIsBehind m = angleBetween (outOf m) (eToPLine2 $ lineSegToIntersection m) < 0
                                                 lineSegToIntersection m = handleLineSegError $ lineSegFromEndpoints (ePointOf m) (pToEPoint2 intersectionPPoint)
                                                 intersectionPPoint = intersectionOf (outOf firstMC) (outOf secondMC)
                                             (Slist (_:_) _) -> error "too many motorcycles."
    motorcyclesIn (CrashTree motorcycles _ _) = motorcycles
    -- | find where the last motorcycle of a divide lands
    landingPointOf :: Contour -> Motorcycle -> MotorcycleIntersection
    landingPointOf myContour myMotorcycle =
      case eNodesInPath of
        [] -> WithLineSeg $ fst $ motorcycleIntersectsAt myContour myMotorcycle
        [oneNode] -> if motorcycleToENodeDistance <= motorcycleToLineSegDistance
                     then WithENode oneNode
                     else WithLineSeg $ fst $ motorcycleIntersectsAt myContour myMotorcycle
          where
            motorcycleToENodeDistance = distanceBetweenPPoints (pPointOf myMotorcycle) (pPointOf oneNode)
            motorcycleToLineSegDistance = distanceBetweenPPoints (pPointOf myMotorcycle) (justIntersectsIn $ plinesIntersectIn (outOf myMotorcycle) (eToPLine2 $ fst $ motorcycleIntersectsAt myContour myMotorcycle))
        (_:_) -> error "more than one opposing exterior node. cannot yet handle this situation."
      where
        justIntersectsIn :: PIntersection -> PPoint2
        justIntersectsIn res = case res of
                                 (IntersectsIn p) -> p
                                 v -> error $ "intersection failure." <> show v <> show myContour <> "\n" <> show myMotorcycle <> "\n"
        eNodesInPath = opposingNodes myContour myMotorcycle
          where
            opposingNodes :: Contour -> Motorcycle -> [ENode]
            opposingNodes c m = filter (\eNode -> plinesIntersectIn (outOf eNode) (outOf m) == PAntiCollinear) $ eNodesOfOutsideContour c

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
    [oneDivide] ->
      Just (cell, Just [findRemainder cell lineSegs divides])
      where
        cell = createCellFromStraightWalls (slist [lineSegs]) [oneDivide]
    _ ->
      Just (cell, Just [findRemainder cell lineSegs divides])
      where
        cell = createCellFromStraightWalls (slist [lineSegs]) [closestDivide]
        closestDivide = if fst (fst $ head divideClosestSorted) == fst ( fst $ head divideFurthestSorted)
                        then snd $ head divideClosestSorted
                        else error $ "Divide collision:\n" <> show divideClosestSorted <> "\n" <> show divideFurthestSorted <> "\n"
        divideClosestSorted = slist $ sortBy (compareDivides lineSegs) $ closestSegOfDivide lineSegs <$> divides
        divideFurthestSorted = slist $ sortBy (compareDivides lineSegs) $ furthestSegOfDivide lineSegs <$> divides
  where
    -- | Find the place where a divide intersects a contour (start OR end), closest to the beginning of the contour
    closestSegOfDivide :: [LineSeg] -> CellDivide -> ((LineSeg, Either Point2 PPoint2), CellDivide)
    closestSegOfDivide contourSegs divide = if elemIndex (fst $ startOfDivide contourSegs divide) contourSegs < elemIndex (fst $ endOfDivide divide) contourSegs
                                            then (startOfDivide contourSegs divide, divide)
                                            else (endOfDivide divide, divide)

    -- | Find the place where a divide intersects a contour (start OR end), closest to the end of the contour
    furthestSegOfDivide :: [LineSeg] -> CellDivide -> ((LineSeg, Either Point2 PPoint2), CellDivide)
    furthestSegOfDivide contourSegs divide = if elemIndex (fst $ endOfDivide divide) contourSegs < elemIndex (fst $ startOfDivide contourSegs divide) contourSegs
                                             then (endOfDivide divide, divide)
                                             else (startOfDivide contourSegs divide, divide)

    -- Compare two divides, for sorting.
    compareDivides :: [LineSeg] -> ((LineSeg, Either Point2 PPoint2), CellDivide) -> ((LineSeg, Either Point2 PPoint2), CellDivide) -> Ordering
    compareDivides contourSegs div1 div2 =
      case elemIndex (fst $ fst div1) contourSegs `compare` elemIndex (fst $ fst div2) contourSegs of
        LT -> LT
        GT -> GT
        EQ -> distanceBetweenPPoints (startPPoint $ fst $ fst div1) (toPPoint2 $ snd $ fst div1) `compare` distanceBetweenPPoints (startPPoint $ fst $ fst div2) (toPPoint2 $ snd $ fst div2)
      where
        toPPoint2 :: Either Point2 PPoint2 -> PPoint2
        toPPoint2 (Left point2) = eToPPoint2 point2
        toPPoint2 (Right ppoint2) = ppoint2
        startPPoint (LineSeg start _) = eToPPoint2 start

data AtOrAround = At
                | Before
                | After

-- | use a single straight division to find the portion of a contour remaining after a cell has been cut out.
findRemainder :: Cell -> [LineSeg] -> [CellDivide] -> RemainingContour
findRemainder (Cell (Slist [] _)) _ _ = error "not enough"
findRemainder (Cell (Slist (_:_:_:_) _)) _ _ = error "too much"
findRemainder (Cell (Slist [(_, Nothing)] _)) _ _ = error "nonsensical"
findRemainder (Cell segSets) contourSegList divides
  | len segSets == 1 = if startBeforeEnd
                       then RemainingContour $ slist [(remainingSegsForward (last firstSegSet) (head firstSegSet), remainingDivides)]
                       else RemainingContour $ slist [(remainingSegsBackward (head firstSegSet) (last firstSegSet), remainingDivides)]
  | len segSets == 2 = if startBeforeEnd
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
                                                 slist $ takeWhile (/= trimEnd)                               $ dropWhile (/= segmentAfter trimStart) (contourSegList <> contourSegList)
                                               At ->
                                                 slist $ takeWhile (/= segmentAfter trimEnd)                  $ dropWhile (/= segmentAfter trimStart) (contourSegList <> contourSegList)
                                               After ->
                                                 slist $ takeWhile (/= (segmentAfter $ segmentAfter trimEnd)) $ dropWhile (/= segmentAfter trimStart) (contourSegList <> contourSegList)
    remainingSegsBackward trimStart trimEnd = case atOrAround of
                                                Before ->
                                                  slist $ takeWhile (/= trimStart) $ dropWhile (/= segmentBefore trimEnd) (contourSegList <> contourSegList)
                                                At ->
                                                  slist $ takeWhile (/= trimStart) $ dropWhile (/= trimEnd) (contourSegList <> contourSegList)
                                                After ->
                                                  slist $ takeWhile (/= trimStart) $ dropWhile (/= segmentAfter trimEnd) (contourSegList <> contourSegList)
    segmentAfter seg = fromMaybe (head contourSegs) $ segAfter seg contourSegList
      where
        segAfter _ [] = Nothing
        segAfter _ [_] = Nothing
        segAfter mySeg (x:y:xs)
            | x == mySeg = Just y
            | otherwise = segAfter mySeg (y:xs)
    segmentBefore seg = fromMaybe (fromMaybe (error "empty list?" ) $ safeLast contourSegs) $ segBefore seg contourSegList
      where
        segBefore _ [] = Nothing
        segBefore _ [_] = Nothing
        segBefore mySeg (x:y:xs)
            | y == mySeg = Just x
            | otherwise = segBefore mySeg (y:xs)
    atOrAround = case maybeEndOfDivide divide of
                   Nothing -> error $ "missed!\n" <> show contourSegs <> "\n" <> show divide <> "\n" <> show segSets <> "\n"
                   (Just (_, Left pt)) -> if pt == endPoint (segmentAfter $ fst $ endOfDivide divide)
                                          then After
                                          else Before
                     where
                   (Just (_, Right _)) -> At
    -- | use the found cell and the motorcycle to determine what direction the motorcycle is going.
    startBeforeEnd
      | len segSets == 1 = pointOfFirstMotorcycle divide == endPoint (fromMaybe (error "empty list. wth?") $ safeLast firstSegSet) ||
                           pointOfFirstMotorcycle divide /= startPoint (head firstSegSet) && error "could not use input cell to determine motorcycle direction."
      | len segSets == 2 = pointOfFirstMotorcycle divide == endPoint (fromMaybe (error "empty list. wth?") $ safeLast firstSegSet) ||
                           pointOfFirstMotorcycle divide /= startPoint (head lastSegSet) && error "could not use input cell to determine motorcycle direction."
      | otherwise = error "wtf"
    remainingDivides = filter (/= divide) divides
    pointOfFirstMotorcycle (CellDivide (DividingMotorcycles m _) _) = ePointOf m
    contourSegs = slist contourSegList

-- | use a single straight division to cut a section of a contour out, converting it to a cell.
-- Always assumes the open side.
-- Always assumes the open side does not create a loop in the contour.
createCellFromStraightWalls :: Slist [LineSeg] -> [CellDivide] -> Cell
createCellFromStraightWalls (Slist [] _) _ = error "empty slist."
createCellFromStraightWalls (Slist (_:_:_) _) _ = error "too many segsets."
createCellFromStraightWalls _ [] = error "no celldivide."
createCellFromStraightWalls _ (_:_:_) = error "too many celldivides."
createCellFromStraightWalls segSetSlist@(Slist [segSet] _) [cellDivide@(CellDivide (DividingMotorcycles motorcycle@(Motorcycle (_,outSeg) _) _) _)]
  | len segSetSlist < 1 = error "recieved a single segment. unpossible."
  | segsAreClosed (slist $ head segSetSlist) = Cell (slist [(slist gatherLineSegsAfterDivide, Just cellDivide)])
  | otherwise = Cell (slist [(slist gatherLineSegsPreceedingDivide, Just cellDivide),
                             (slist gatherLineSegsFollowingDivide, Nothing)])
  where
    segsAreClosed mySegs = startPoint (head mySegs) == endPoint (last mySegs)
    -- |  Return the line segments preceeding the first divide, from the opening.
    gatherLineSegsPreceedingDivide = if startBeforeEnd
                                     then takeWhile (/= outSeg) segSet
                                     else  takeWhile (/= segmentAfter motorcycleOutSegment) segSet
    -- |  Return the line segments following the first divide, toward the opening.
    gatherLineSegsFollowingDivide = if startBeforeEnd
                                    then dropWhile (/= motorcycleOutSegment) segSet
                                    else dropWhile (/= outSeg) segSet
    -- |  Return the line segments after the first divide.
    gatherLineSegsAfterDivide = if startBeforeEnd
                                then takeWhile (/= segmentAfter motorcycleOutSegment) $ dropWhile (/= outSeg) segSet
                                else takeWhile (/= outSeg)                            $ dropWhile (/= motorcycleOutSegment) segSet
    -- | determine the direction of a divide.
    startBeforeEnd = elemIndex (fst $ startOfDivide segSet cellDivide) segSet < elemIndex (fst $ endOfDivide cellDivide) segSet
    -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the former of the two segments (from the beginning of the contour).
    (motorcycleInSegment, eitherMotorcycleOutPoint) = fromMaybe (error "no intersections?") $ motorcycleMightIntersectWith segSet motorcycle
    -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the latter of the two segments (from the beginning of the contour).
    motorcycleOutSegment = case eitherMotorcycleOutPoint of
                             (Left point2) -> if point2 == endPoint motorcycleInSegment
                                              then segmentAfter motorcycleInSegment
                                              else if point2 == startPoint motorcycleInSegment
                                                   then motorcycleInSegment
                                                   else error $ show point2 <> "\n" <> show segSet <> "\n" <> show motorcycleInSegment
                             (Right _) -> motorcycleInSegment
    segmentAfter :: LineSeg -> LineSeg
    segmentAfter seg = fromMaybe (head $ slist $ head segSetSlist) $ segAfter seg segSet
      where
        segAfter _ [] = Nothing
        segAfter _ [_] = Nothing
        segAfter mySeg (x:y:xs)
            | x == mySeg = Just y
            | otherwise = segAfter mySeg (y:xs)

-- Get the segment the divide intersects that is closest to the beginning of the list of a contour's line segments.
startOfDivide :: [LineSeg] -> CellDivide -> (LineSeg, Either Point2 PPoint2)
startOfDivide _ (CellDivide (DividingMotorcycles (Motorcycle (inSeg,LineSeg start _) _) _) _) = (inSeg, Left start)

-- Get the segment the divide intersects that is closest to the end of the list of a contour's line segments.
endOfDivide :: CellDivide -> (LineSeg, Either Point2 PPoint2)
endOfDivide divide = fromMaybe (error "missed!") $ maybeEndOfDivide divide

-- | Get the segment and location on the segment the given divide intersects that is closest to the end of the list of a contour's line segments.
-- FIXME: yes, this is woefully incomplete.
maybeEndOfDivide :: CellDivide -> Maybe (LineSeg, Either Point2 PPoint2)
maybeEndOfDivide (CellDivide (DividingMotorcycles m ms) lastIntersection)
  | len ms == 0 = case lastIntersection of
                    (WithENode eNode@(ENode (startSeg,_) _)) -> Just (startSeg, Left $ ePointOf eNode)
                    (WithMotorcycle m1) -> Just (startSegOfMotorcycle m1, Left $ ePointOf m1)
                    (WithLineSeg lineSeg) -> motorcycleMightIntersectWith [lineSeg] m
  | len ms == 1 = case lastIntersection of
                    (WithENode eNode@(ENode (startSeg,_) _)) -> Just (startSeg, Left $ ePointOf eNode)
                    (WithMotorcycle m2) -> Just (startSegOfMotorcycle m2, Left $ ePointOf m2)
                    (WithLineSeg lineSeg) -> motorcycleMightIntersectWith [lineSeg] $ head ms
  | otherwise = Nothing
  where
    startSegOfMotorcycle :: Motorcycle -> LineSeg
    startSegOfMotorcycle (Motorcycle (startSeg, _) _) = startSeg

-- | Add a pair of NodeTrees together along a Divide, to create a new nodeTree.
-- The intersection point for the nodeTrees along the CellDivide is calculated, and then the out of final INode of the two sides is adjusted to pass through that point.
-- NOTE: since a division can generate two non-neighboring nodetrees, make sure to add them to a side first before adding them together..
addNodeTreesAlongDivide :: NodeTree -> NodeTree -> CellDivide -> NodeTree
addNodeTreesAlongDivide nodeTree1 nodeTree2 division = mergeNodeTrees (adjustedNodeTree1:nodeTreesFromDivision division <> [adjustedNodeTree2])
  where
    adjustedNodeTree1 = redirectLastOut nodeTree1 crossoverPoint
    adjustedNodeTree2 = redirectLastOut nodeTree2 crossoverPoint
    -- adjust the last output of the nodetree so that it goes through the point it's supposed to.
    redirectLastOut (NodeTree eNodes iNodeGens@(INodeSet gens)) myCrossover = NodeTree eNodes $ INodeSet $ init gens <> (one [makeINode (insOf $ lastINodeOf iNodeGens) (Just $ join2PPoint2 (pPointOf $ lastINodeOf iNodeGens) myCrossover)])
    crossoverPoint = case division of
                       (CellDivide (DividingMotorcycles motorcycle1 (Slist [] _)) target) -> -- no eNode, and no opposing motorcycle.
                         motorcycleDivisor motorcycle1 target
                       _ -> error "cannot generate crossoverPoint."

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
                                                             (DividingMotorcycles m (Slist _ 0)) -> plinesIntersectIn (finalPLine nt1) (outOf m) == plinesIntersectIn (finalPLine nt2) (outOf m)
                                                             (DividingMotorcycles _ (Slist _ _)) -> error "cannot yet check outpoint intersections of more than one motorcycle."

-- | Given a nodeTree and it's closing division, return all of the ENodes where the point of the node is on the opposite side of the division.
crossoverINodes :: NodeTree -> CellDivide -> [INode]
crossoverINodes nodeTree@(NodeTree _ (INodeSet (Slist iNodes _))) cellDivision = filter nodeCrosses (concat iNodes)
  where
    nodeCrosses :: INode -> Bool
    nodeCrosses a = Just False `elem` (intersectionSameSide pointOnSide a <$> motorcyclesInDivision cellDivision)
    pointOnSide = eToPPoint2 $ pointInCell nodeTree cellDivision
    pointInCell cell (CellDivide (DividingMotorcycles m _) _)
      | firstSegOf cell == lastCSegOf m = endPoint $ firstSegOf cell
      | lastSegOf cell == firstCSegOf m = startPoint $ lastSegOf cell
      | otherwise = error $ "unhandled case: " <> show cell <> "\n" <> show m <> "\n" <> show (lastSegOf cell) <> "\n" <> show (firstSegOf cell) <> "\n"
      where
        firstCSegOf (Motorcycle (seg1,_) _) = seg1
        lastCSegOf (Motorcycle (_, seg2) _) = seg2
