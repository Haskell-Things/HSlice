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

import Prelude (Bool(False), Eq, Ordering(LT, GT, EQ), Show, elem, filter, null, otherwise, ($), (<$>), (==), error, (<>), show, (&&), compare, concat, (/=), (||), (<), fst, snd)

import Data.Either(Either(Left, Right))

import Data.List (elemIndex, sortBy, dropWhile, takeWhile)

import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isNothing)

import Slist (head, last, len, slist, safeLast)

import Slist.Type (Slist(Slist), one)

import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour, skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), INodeSet(INodeSet), NodeTree(NodeTree), RemainingContour(RemainingContour), Motorcycle(Motorcycle), Cell(Cell), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), INode, ePointOf, finalPLine, intersectionOf, outOf)

import Graphics.Slicer.Math.Ganja (dumpGanja)

import Graphics.Slicer.Math.Skeleton.Motorcycles (CollisionType(HeadOn), CrashTree(CrashTree), motorcyclesInDivision, intersectionSameSide, lastCrashType, motorcyclesAreAntiCollinear, motorcycleToENode, motorcycleMightIntersectWith)

import Graphics.Slicer.Math.Skeleton.NodeTrees (firstSegOf, lastSegOf, makeNodeTree, mergeNodeTrees)

import Graphics.Slicer.Math.Contour (lineSegsOfContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2, startPoint)

import Graphics.Slicer.Math.Line (endPoint, handleLineSegError, lineSegFromEndpoints)

import Graphics.Slicer.Math.PGA (PPoint2, PIntersection(PAntiCollinear), angleBetween, distanceBetweenPPoints, eToPLine2, eToPPoint2, pToEPoint2, plinesIntersectIn)

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
                                    (Slist [inMC] _) -> [CellDivide (DividingMotorcycles inMC (Slist [] 0)) (maybeOpposingENodeOf contour crashTree)]
                                    (Slist [firstMC,secondMC] _) -> if lastCrashType crashTree == Just HeadOn
                                                              then [CellDivide (DividingMotorcycles firstMC (Slist [secondMC] 1)) Nothing]
                                                              else if intersectionIsBehind firstMC || intersectionIsBehind secondMC
                                                                   then -- These sections cannot intersect.
                                                                     [CellDivide (DividingMotorcycles firstMC (Slist [] 0)) Nothing,
                                                                      CellDivide (DividingMotorcycles secondMC (Slist [] 0)) Nothing]
                                                                   else error "don't know what to do with these line segments."
                                      where
                                        intersectionPPoint = intersectionOf (outOf firstMC) (outOf secondMC)
                                        intersectionIsBehind m = angleBetween (outOf m) (eToPLine2 $ lineSegToIntersection m) < 0
                                        lineSegToIntersection m = handleLineSegError $ lineSegFromEndpoints (ePointOf m) (pToEPoint2 intersectionPPoint)
                                    (Slist (_:_) _) -> error "too many motorcycles."
  where
    motorcyclesIn (CrashTree motorcycles _ _) = motorcycles
    -- | find enodes where the arc coresponding to them is collinear with the dividing Motorcycle.
    maybeOpposingENodeOf :: Contour -> CrashTree -> Maybe ENode
    maybeOpposingENodeOf myContour myCrashTree = case motorcyclesIn myCrashTree of
                                                   (Slist [inMC] 1) -> case opposingNodes myContour inMC of
                                                                         [] -> Nothing
                                                                         [oneNode] -> Just oneNode
                                                                         (_:_) -> error "more than one opposing exterior node. cannot yet handle this situation."
                                                   (Slist _ 2) -> if lastCrashType myCrashTree == Just HeadOn
                                                                  then Nothing
                                                                  else error "cannot handle anything but headon pairs of motorcycles yet."
                                                   (Slist _ _) -> error "cannot handle anything but one motorcycle yet."
    opposingNodes :: Contour -> Motorcycle -> [ENode]
    opposingNodes c m = filter (\eNode -> plinesIntersectIn (outOf eNode) (outOf m) == PAntiCollinear) $ eNodesOfOutsideContour c

-- | Find a single Cell of the given contour. always finds the cell on the 'open end' of the contour.
findFirstCellOfContour :: Contour -> [CellDivide] -> Maybe (Cell, Maybe [RemainingContour])
findFirstCellOfContour contour divides =
  case divides of
    [] -> -- When there are no motorcycles, and there are no holes, we can just treat the whole contour as a single cell. This does the conversion.
      Just (contourFromCell, Nothing)
    [oneDivide] ->
      Just ( cell
           , Just [findRemainder cell contourSegs divides]
           )
      where
        cell = createCellFromStraightWalls (slist [lineSegsOfContour contour]) [oneDivide]
    _ ->
      Just ( cell
           , Just [findRemainder cell contourSegs divides]
           )
      where
        cell = createCellFromStraightWalls (slist [lineSegsOfContour contour]) [closestDivide]
        closestDivide = if snd (head divideClosestSorted) == snd (head divideFurthestSorted)
                        then snd $ head divideClosestSorted
                        else error $ "ugh\n" <> show divideClosestSorted <> "\n" <> show divideFurthestSorted <> "\n"
  where
    -- | convert a single contour to a single cell.
    contourFromCell = Cell (slist [(slist contourSegs, Nothing)])
    contourSegs = lineSegsOfContour contour
    divideClosestSorted = slist $ sortBy compareDivides $ closestSegOfDivide contour <$> divides
    divideFurthestSorted = slist $ sortBy compareDivides $ furthestSegOfDivide contour <$> divides
    compareDivides :: ((LineSeg, Either Point2 PPoint2), CellDivide) -> ((LineSeg, Either Point2 PPoint2), CellDivide) -> Ordering
    compareDivides div1 div2 = case elemIndex (fst $ fst div1) contourSegs `compare` elemIndex (fst $ fst div2) contourSegs of
                                                         LT -> LT
                                                         GT -> GT
                                                         EQ -> distanceBetweenPPoints (startPPoint $ fst $ fst div1) (toPPoint2 $ snd $ fst div1) `compare` distanceBetweenPPoints (startPPoint $ fst $ fst div2) (toPPoint2 $ snd $ fst div2)
    closestSegOfDivide myContour divide = if elemIndex (fst $ startIntersect myContour divide) contourSegs < elemIndex (fst $ stopIntersect myContour divide) contourSegs
                                          then (startIntersect myContour divide, divide)
                                          else (stopIntersect myContour divide, divide)
    furthestSegOfDivide myContour divide = if elemIndex (fst $ startIntersect myContour divide) contourSegs < elemIndex (fst $ stopIntersect myContour divide) contourSegs
                                           then (stopIntersect myContour divide, divide)
                                           else (startIntersect myContour divide, divide)
    startIntersect :: Contour -> CellDivide -> (LineSeg, Either Point2 PPoint2)
    startIntersect myContour = startOfDivide (lineSegsOfContour myContour)
    stopIntersect :: Contour -> CellDivide -> (LineSeg, Either Point2 PPoint2)
    stopIntersect myContour = endOfDivide (lineSegsOfContour myContour)
    toPPoint2 :: Either Point2 PPoint2 -> PPoint2
    toPPoint2 (Left point2) = eToPPoint2 point2
    toPPoint2 (Right ppoint2) = ppoint2
    startPPoint (LineSeg start _) = eToPPoint2 start

-- | Find a single Cell of the given contour. always finds the cell on the 'open end' of the contour.
-- FIXME: implement all of these!
findNextCell :: RemainingContour -> Maybe (Cell, Maybe [RemainingContour])
findNextCell (RemainingContour (Slist [] _) ) = error "cannot handle no contour remainders."
findNextCell (RemainingContour (Slist (_:_:_) _) ) = error "cannot handle multiple contour remainders."
findNextCell (RemainingContour (Slist [(Slist lineSegs _, divides)] _) ) =
  case divides of
    [] -> -- When there are no motorcycles, and there are no holes, we can just treat the whole contour as a single cell. This does the conversion.
      Just (contourFromCell, Nothing)
    [oneDivide] ->
      Just (cell, Just [findRemainder cell lineSegs divides])
      where
        cell = createCellFromStraightWalls (slist [lineSegs]) [oneDivide]
    _ ->
      Just (cell, Just [findRemainder cell lineSegs divides])
      where
        cell = createCellFromStraightWalls (slist [lineSegs]) [closestDivide]
  where
    -- | convert a single contour to a single cell.
    contourFromCell = Cell (slist [(slist lineSegs, Nothing)])
    closestDivide = if fst (fst $ head divideClosestSorted) == fst ( fst $ head divideFurthestSorted)
                    then snd $ head divideClosestSorted
                    else error $ "Divide collision:\n" <> show divideClosestSorted <> "\n" <> show divideFurthestSorted <> "\n"
    divideClosestSorted = slist $ sortBy compareDivides $ closestSegOfDivide lineSegs <$> divides
    divideFurthestSorted = slist $ sortBy compareDivides $ furthestSegOfDivide lineSegs <$> divides
    compareDivides :: ((LineSeg, Either Point2 PPoint2), CellDivide) -> ((LineSeg, Either Point2 PPoint2), CellDivide) -> Ordering
    compareDivides div1 div2 = case elemIndex (fst $ fst div1) lineSegs `compare` elemIndex (fst $ fst div2) lineSegs of
                                                         LT -> LT
                                                         GT -> GT
                                                         EQ -> distanceBetweenPPoints (startPPoint $ fst $ fst div1) (toPPoint2 $ snd $ fst div1) `compare` distanceBetweenPPoints (startPPoint $ fst $ fst div2) (toPPoint2 $ snd $ fst div2)
    closestSegOfDivide :: [LineSeg] -> CellDivide -> ((LineSeg, Either Point2 PPoint2), CellDivide)
    closestSegOfDivide myLineSegs divide = if elemIndex (fst $ startIntersect divide) myLineSegs < elemIndex (fst $ endIntersect divide) lineSegs
                                           then (startIntersect divide, divide)
                                           else (endIntersect divide, divide)
    furthestSegOfDivide :: [LineSeg] -> CellDivide -> ((LineSeg, Either Point2 PPoint2), CellDivide)
    furthestSegOfDivide myLineSegs divide = if elemIndex (fst $ endIntersect divide) myLineSegs < elemIndex (fst $ startIntersect divide) lineSegs
                                            then (endIntersect divide, divide)
                                            else (startIntersect divide, divide)
    startIntersect :: CellDivide -> (LineSeg, Either Point2 PPoint2)
    startIntersect = startOfDivide lineSegs
    endIntersect :: CellDivide -> (LineSeg, Either Point2 PPoint2)
    endIntersect = endOfDivide lineSegs
    toPPoint2 :: Either Point2 PPoint2 -> PPoint2
    toPPoint2 (Left point2) = eToPPoint2 point2
    toPPoint2 (Right ppoint2) = ppoint2
    startPPoint (LineSeg a _) = eToPPoint2 a

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
    remainingSegsForward trimStart trimEnd = case atOrAround trimEnd of
                                               Before ->
                                                 slist $ takeWhile (/= trimEnd)                               $ dropWhile (/= segmentAfter trimStart) (contourSegList <> contourSegList)
                                               At ->
                                                 slist $ takeWhile (/= segmentAfter trimEnd)                  $ dropWhile (/= segmentAfter trimStart) (contourSegList <> contourSegList)
                                               After ->
                                                 slist $ takeWhile (/= (segmentAfter $ segmentAfter trimEnd)) $ dropWhile (/= segmentAfter trimStart) (contourSegList <> contourSegList)
    remainingSegsBackward trimStart trimEnd = case atOrAround trimEnd of
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
    atOrAround seg@(LineSeg start _) = case maybeEndOfDivide [seg] divide of
                                         Nothing -> error $ "missed!\n" <> show contourSegs <> "\n" <> show divide <> "\n" <> show segSets <> "\n" <> show seg <> "\n"
                                         (Just (_, Left pt)) -> if pt == start
                                                                then Before
                                                                else After
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
    startBeforeEnd = elemIndex (fst $ startOfDivide segSet cellDivide) segSet < elemIndex (fst $ endOfDivide segSet cellDivide) segSet
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
endOfDivide :: [LineSeg] -> CellDivide -> (LineSeg, Either Point2 PPoint2)
endOfDivide segSet divide = fromMaybe (error "missed!") $ maybeEndOfDivide segSet divide

-- | Get the segment and location on the segment the given divide intersects that is closest to the end of the list of a contour's line segments.
maybeEndOfDivide :: [LineSeg] -> CellDivide -> Maybe (LineSeg, Either Point2 PPoint2)
maybeEndOfDivide lineSegs (CellDivide (DividingMotorcycles m ms) maybeENode)
  | len ms == 0 = case maybeENode of
                    Nothing -> motorcycleMightIntersectWith lineSegs m
                    (Just eNode@(ENode (startSeg,_) _)) -> Just (startSeg, Left $ ePointOf eNode)
  -- FIXME: yes, this is woefully incomplete.
  | len ms == 1 && isNothing maybeENode = Just (startSegOfMotorcycle $ head ms, Left $ ePointOf $ head ms)
  | otherwise = error "impossible situation finding the end of a straight divide."
  where
    startSegOfMotorcycle :: Motorcycle -> LineSeg
    startSegOfMotorcycle (Motorcycle (startSeg, _) _) = startSeg

-- | Add a pair of NodeTrees together along a Divide, to create a new nodeTree.
-- NOTE: since a division can generate two non-neighboring nodetrees, make sure to add them to a side first.
addNodeTreesAlongDivide :: NodeTree -> NodeTree -> CellDivide -> NodeTree
addNodeTreesAlongDivide nodeTree1 nodeTree2 division = mergeNodeTrees (nodeTree1:nodeTreesFromDivision division <> [nodeTree2])

-- | Create the NodeTrees corresponding to the CellDivide given.
nodeTreesFromDivision :: CellDivide -> [NodeTree]
nodeTreesFromDivision cellDivision@(CellDivide motorcycles maybeENode) = case motorcycles of
                                                                           (DividingMotorcycles _ (Slist [] 0)) -> res
                                                                           (DividingMotorcycles firstMotorcycle (Slist [secondMotorcycle] 1)) -> if motorcyclesAreAntiCollinear firstMotorcycle secondMotorcycle
                                                                                                                                                 then res
                                                                                                                                                 else errorOut
                                                                           (DividingMotorcycles _ (Slist _ _)) -> errorOut
  where
    res = case maybeENode of
            (Just eNode) -> [makeNodeTree (motorcycleToENode <$> motorcyclesInDivision cellDivision) (INodeSet $ slist []), makeNodeTree [eNode] (INodeSet $ slist [])]
            Nothing -> [makeNodeTree (motorcycleToENode <$> motorcyclesInDivision cellDivision) (INodeSet $ slist [])]
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
