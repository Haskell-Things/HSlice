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
module Graphics.Slicer.Math.Skeleton.Cells (RemainingContour, cellAfter, cellBefore, findDivisions, findFirstCellOfContour, findNextCell, getNodeTreeOfCell, nodeTreesDoNotOverlap, addNodeTreesOnDivide) where

import Prelude (Bool(False), Eq, Ordering(LT, GT, EQ), Show, elem, filter, null, otherwise, ($), (<$>), (==), (++), error, (+), Int, drop, take, (-), error, (<>), show, (&&), compare, concat, (/=), (||), (<), fst, snd)

import Prelude as P (head, last)

import Data.Either(Either(Left, Right))

import Data.List (elemIndex, sortBy, dropWhile, takeWhile)

import Data.Maybe (Maybe(Just, Nothing), catMaybes, fromMaybe)

import Slist (slist, len)

import Slist as SL (head, last)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), INodeSet(INodeSet), NodeTree(NodeTree), RemainingContour(RemainingContour), StraightSkeleton(StraightSkeleton), Motorcycle(Motorcycle), Cell(Cell), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), StraightSkeleton, INode, ePointOf, finalPLine, intersectionOf, outOf)

import Graphics.Slicer.Math.Skeleton.Motorcycles (CollisionType(HeadOn), CrashTree(CrashTree), motorcycleIntersectsAt, motorcyclesInDivision, intersectionSameSide, lastCrashType, motorcyclesAreAntiCollinear, motorcycleToENode, motorcycleMightIntersectWith)

import Graphics.Slicer.Math.Skeleton.NodeTrees (firstSegOf, lastSegOf, makeNodeTree, sortNodeTrees)

import Graphics.Slicer.Math.Contour (lineSegsOfContour)

import Graphics.Slicer.Math.Skeleton.Concave (eNodesOfOutsideContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2)

import Graphics.Slicer.Math.Line (endpoint, handleLineSegError, lineSegFromEndpoints)

import Graphics.Slicer.Math.PGA (PPoint2, PIntersection(PAntiCollinear), angleBetween, eToPLine2, eToPPoint2, pToEPoint2, plinesIntersectIn)

data UnsupportedReason = INodeCrossesDivide [CellDivide]
  deriving (Show, Eq)

-- | get a naieve node tree for a given cell.
-- Warning: in the cases where the cell has nodes outside of the cell wall, you must use tscherne's algorithm to merge two cells.
getNodeTreeOfCell :: Cell -> Either UnsupportedReason NodeTree
getNodeTreeOfCell (Cell (Slist [((Slist extSegs _), Nothing)] _)) = Right $ skeletonOfConcaveRegion extSegs
getNodeTreeOfCell (Cell (Slist [((Slist extSegs _), Just divide)] _))
  | null $ crossoverINodes res divide = Right $ res
  | otherwise = Left $ INodeCrossesDivide [divide]
  where
    res = skeletonOfConcaveRegion extSegs
getNodeTreeOfCell _ = error "unsupported."

-- A flag for which side of a dividing motorcycle to cut a cell from, the side after or before the start of the motorcycle.
data Side = SideAfter
          | SideBefore
  deriving (Eq)

-- | Calculate a partial straight skeleton for the motorcycle cell that is on the left side of the point that a motorcycle\'s path starts at, ending where the motorcycle intersects the contour.
cellAfter :: Contour -> CellDivide -> NodeTree
cellAfter contour cellDivide = case getNodeTreeOfCell (createCellFromStraightWall contour cellDivide SideAfter) of
                                 (Right res) -> res
                                 (Left err) -> error $ "got error: " <> show err <> "\n"

-- | Calculate a partial straight skeleton for the motorcycle cell that is on the right side of the point that a motorcycle\'s path starts at, ending where the motorcycle intersects the contour.
cellBefore :: Contour -> CellDivide -> NodeTree
cellBefore contour cellDivide = case getNodeTreeOfCell (createCellFromStraightWall contour cellDivide SideBefore) of
                                 (Right res) -> res
                                 (Left err) -> error $ "got error: " <> show err <> "\n"

-- | find the divisions of a given contour. Divisions are points where motorcycles cross the contour.
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

-- | Find the non-reflex virtexes of a contour and draw motorcycles from them. Useful for contours that are a 'hole' in a bigger contour.
-- | Find a single Cell of the given contour. always finds the cell on the 'open end' of the contour.
findFirstCellOfContour :: Contour -> [CellDivide] -> Maybe (Cell, Maybe CellDivide, Maybe [RemainingContour])
findFirstCellOfContour contour divides = case divides of
                                         [] -> -- When there are no motorcycles, and there are no holes, we can just treat the whole contour as a single cell. This does the conversion.
                                           Just (contourFromCell, Nothing, Nothing)
                                         [oneDivide] ->
                                           Just ( cell
                                                 ,Just oneDivide
                                                 ,Just [findRemainder cell contourSegs divides]
                                                )
                                           where
                                             cell = createCellFromStraightWall contour oneDivide (openSide oneDivide)
                                         _ ->
                                           Just ( cell
                                                 ,Just closestDivide
                                                 ,Just [findRemainder cell contourSegs divides]
                                                )
                                           where
                                             cell = createCellFromStraightWall contour closestDivide (openSide closestDivide)
                                             closestDivide = if snd (SL.head divideClosestSorted) == snd (SL.head divideFurthestSorted)
                                                             then snd $ SL.head divideClosestSorted
                                                             else error $ "ugh\n" <> show divideClosestSorted <> "\n" <> show divideFurthestSorted <> "\n"
  where
    openSide divide = case elemIndex (contourStartOfDivide contour divide) contourSegs `compare` elemIndex (contourEndOfDivide contour divide) contourSegs of
                        LT -> SideAfter
                        GT -> SideBefore
                        EQ -> error "FIXME"
    -- | convert a single contour to a single cell.
    contourFromCell = Cell (slist [(slist contourSegs, Nothing)])
    contourSegs = lineSegsOfContour contour
    divideClosestSorted = slist $ sortBy (\div1 div2 -> case elemIndex (fst div1) contourSegs `compare` elemIndex (fst div2) contourSegs of
                                                         LT -> LT
                                                         GT -> GT
--                                               EQ ->
                                        ) $ closestSegOfDivide contour <$> divides
    divideFurthestSorted = slist $ sortBy (\div1 div2 -> case elemIndex (fst div1) contourSegs `compare` elemIndex (fst div2) contourSegs of
                                                         LT -> LT
                                                         GT -> GT
--                                               EQ ->
                                        ) $ furthestSegOfDivide contour <$> divides
    closestSegOfDivide myContour divide = if elemIndex (contourStartOfDivide myContour divide) contourSegs < elemIndex (contourEndOfDivide myContour divide) contourSegs
                                          then (contourStartOfDivide myContour divide, divide)
                                          else (contourEndOfDivide myContour divide, divide)
    furthestSegOfDivide myContour divide = if elemIndex (contourStartOfDivide myContour divide) contourSegs < elemIndex (contourEndOfDivide myContour divide) contourSegs
                                           then (contourEndOfDivide myContour divide, divide)
                                           else (contourStartOfDivide myContour divide, divide)

-- Get the segment the divide intersects that is closest to the beginning of the list of a contour's line segments.
contourStartOfDivide :: Contour -> CellDivide -> LineSeg
contourStartOfDivide _ (CellDivide (DividingMotorcycles (Motorcycle (inSeg, _) _) _) _) = inSeg

-- Get the segment the divide intersects that is closest to the end of the list of a contour's line segments.
contourEndOfDivide :: Contour -> CellDivide -> LineSeg
contourEndOfDivide contour divide = justSeg
      where
        (Just (justSeg,_)) = endOfDivide (lineSegsOfContour contour) divide
        -- Get the segment and location on the segment the given divide intersects that is closest to the end or beginning of the list of a contour's line segments.

-- | Find the non-reflex virtexes of a contour and draw motorcycles from them. Useful for contours that are a 'hole' in a bigger contour.
-- | Find a single Cell of the given contour. always finds the cell on the 'open end' of the contour.
findNextCell :: RemainingContour -> Maybe (Cell, Maybe CellDivide, Maybe [RemainingContour])
findNextCell (RemainingContour (Slist [(Slist lineSegs _, divides)] _) ) =
  case divides of
    [] -> -- When there are no motorcycles, and there are no holes, we can just treat the whole contour as a single cell. This does the conversion.
      Just (contourFromCell, Nothing, Nothing)
    [oneDivide] ->
      Just (cell
           ,Just oneDivide
           ,Just [findRemainder cell lineSegs divides]
           )
      where
        cell = createCellFromStraightWalls (slist [lineSegs]) [oneDivide]
    _ ->
      Just (cell
           ,Just closestDivide
           ,Just [findRemainder cell lineSegs divides]
           )
      where
        cell = createCellFromStraightWalls (slist [lineSegs]) [closestDivide]
        closestDivide = if snd (SL.head divideClosestSorted) == snd (SL.head divideFurthestSorted)
                        then snd $ SL.head divideClosestSorted
                        else error $ "ugh\n" <> show divideClosestSorted <> "\n" <> show divideFurthestSorted <> "\n"
  where
    -- | convert a single contour to a single cell.
    contourFromCell = Cell (slist [(slist lineSegs, Nothing)])
    divideClosestSorted = slist $ sortBy (\div1 div2 -> case elemIndex (fst div1) lineSegs `compare` elemIndex (fst div2) lineSegs of
                                                         LT -> LT
                                                         GT -> GT
--                                               EQ ->
                                        ) $ closestSegOfDivide lineSegs <$> divides
    divideFurthestSorted = slist $ sortBy (\div1 div2 -> case elemIndex (fst div1) lineSegs `compare` elemIndex (fst div2) lineSegs of
                                                         LT -> LT
                                                         GT -> GT
--                                               EQ ->
                                        ) $ furthestSegOfDivide lineSegs <$> divides
    closestSegOfDivide segSet divide = if elemIndex startSeg lineSegs < elemIndex endSeg lineSegs
                                       then (startSeg, divide)
                                       else (endSeg, divide)
      where
        startSeg = fromMaybe (error "miss") $ segSetStartOfDivide segSet divide
        endSeg = fromMaybe (error "miss") $ segSetEndOfDivide segSet divide
    furthestSegOfDivide segSet divide = if elemIndex endSeg lineSegs < elemIndex startSeg lineSegs
                                       then (endSeg, divide)
                                       else (startSeg, divide)
      where
        startSeg = fromMaybe (error "miss") $ segSetStartOfDivide segSet divide
        endSeg = fromMaybe (error "miss") $ segSetEndOfDivide segSet divide

data AtOrAround = At
                | Before
                | After

-- | use a single straight division to find the portion of a contour remaining after a cell has been cut out.
findRemainder :: Cell -> [LineSeg] -> [CellDivide] -> RemainingContour
findRemainder (Cell (Slist [(lineSegs, (Just divide))] 1)) contourSegs divides
  | startBeforeEnd = RemainingContour $ slist $ [(remainingSegsForward (SL.last lineSegs) (SL.head lineSegs), remainingDivides)]
  | otherwise = RemainingContour $ slist $ [(remainingSegsBackward (SL.head lineSegs) (SL.last lineSegs), remainingDivides)]
  where
    remainingSegsForward trimStart trimEnd = case atOrAround trimEnd of
                                               Before ->
                                                 slist $ takeWhile (\a -> a /= trimEnd) $ dropWhile (\a -> a /= segmentAfter trimStart) contourSegs
                                               At ->
                                                 slist $ takeWhile (\a -> a /= segmentAfter trimEnd)                   $ dropWhile (\a -> a /= segmentAfter trimStart) contourSegs
                                               After ->
                                                 slist $ takeWhile (\a -> a /= (segmentAfter $ segmentAfter trimEnd))  $ dropWhile (\a -> a /= segmentAfter trimStart) contourSegs
    remainingSegsBackward trimStart trimEnd = case atOrAround trimEnd of
                                                Before ->
                                                  slist $ takeWhile (\a -> a /= trimStart) $ dropWhile (\a -> a /= segmentBefore trimEnd) contourSegs
                                                At ->
                                                  slist $ takeWhile (\a -> a /= trimStart) $ dropWhile (\a -> a /= trimEnd) contourSegs
                                                After ->
                                                  slist $ takeWhile (\a -> a /= trimStart) $ dropWhile (\a -> a /= segmentAfter trimEnd) contourSegs
    segmentAfter seg = fromMaybe (P.head contourSegs) $ segAfter seg contourSegs
      where
        segAfter _ [] = Nothing
        segAfter _ [_] = Nothing
        segAfter mySeg (x:y:xs)
            | x == mySeg = Just y
            | otherwise = segAfter mySeg (y:xs)
    segmentBefore seg = fromMaybe (P.last contourSegs) $ segBefore seg contourSegs
      where
        segBefore _ [] = Nothing
        segBefore _ [_] = Nothing
        segBefore mySeg (x:y:xs)
            | y == mySeg = Just x
            | otherwise = segBefore mySeg (y:xs)
    atOrAround seg@(LineSeg start _) = case endOfDivide [seg] divide of
                                         Nothing -> error $ "missed!\n" <> show seg <> "\n" <> show divide <> "\n"
                                         (Just (_, Left pt)) -> if pt == start
                                                                then Before
                                                                else After
                                         (Just (_, Right _)) -> At
    (Slist lineSegSet _) = lineSegs
    startBeforeEnd = elemIndex (fromMaybe (error "missed") $ segSetStartOfDivide lineSegSet divide) contourSegs `compare` elemIndex (fromMaybe (error "also missed") $ segSetEndOfDivide lineSegSet divide) contourSegs == LT
    remainingDivides = filter (\d -> d /= divide) divides

-- Get the segment the divide intersects that is closest to the beginning of the list of a contour's line segments.
segSetStartOfDivide :: [LineSeg] -> CellDivide -> Maybe LineSeg
segSetStartOfDivide _ (CellDivide (DividingMotorcycles (Motorcycle (inSeg, _) _) _) _) = Just inSeg

-- Get the segment the divide intersects that is closest to the end of the list of a contour's line segments.
segSetEndOfDivide :: [LineSeg] -> CellDivide -> Maybe LineSeg
segSetEndOfDivide segSet divide = justSeg
      where
        justSeg = case endOfDivide segSet divide of
                    Nothing -> Nothing
                    Just (firstSeg, _) -> Just firstSeg

-- | Get the segment and location on the segment the given divide intersects that is closest to the end of the list of a contour's line segments.
endOfDivide :: [LineSeg] -> CellDivide -> Maybe (LineSeg, Either Point2 PPoint2)
endOfDivide lineSegs (CellDivide (DividingMotorcycles m ms) maybeENode)
  | len ms == 0 = case maybeENode of
                    Nothing -> motorcycleMightIntersectWith lineSegs m
                    (Just eNode@(ENode (startSeg,_) _)) -> Just (startSeg, Left $ ePointOf eNode)
  -- FIXME: yes, this is woefully incomplete.
  | len ms == 1 && maybeENode == Nothing = Just (startSegOfMotorcycle $ SL.head ms, Left $ ePointOf $ SL.head ms)
  | otherwise = error "impossible situation finding the end of a straight divide."
  where
    startSegOfMotorcycle :: Motorcycle -> LineSeg
    startSegOfMotorcycle (Motorcycle (startSeg, _) _) = startSeg

-- | use a single straight division to cut a section of a contour out, converting it to a cell.
createCellFromStraightWall :: Contour -> CellDivide -> Side -> Cell
createCellFromStraightWall contour cellDivide@(CellDivide (DividingMotorcycles motorcycle@(Motorcycle (_,outSeg) _) _) _) side = Cell (slist [(slist $ gatherLineSegs side, Just cellDivide)])
  where
    contourSegs = lineSegsOfContour contour
    startSegmentIndex = segIndex outSeg contourSegs
    -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the first of the two segments (from the beginning of the contour).
    (motorcycleInSegment, eitherMotorcycleOutSegment) = motorcycleIntersectsAt contour motorcycle
    -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the last of the two segments (from the beginning of the contour).
    motorcycleOutSegment = case eitherMotorcycleOutSegment of
                             (Left motorcycleOutSeg) -> motorcycleOutSeg
                             (Right _) -> motorcycleInSegment
    afterOpenSide   = drop afterStopSegmentIndex contourSegs ++ take startSegmentIndex contourSegs
    afterClosedSide = take (startSegmentIndex - afterStopSegmentIndex) $ drop afterStopSegmentIndex contourSegs
    afterStopSegmentIndex = segIndex motorcycleOutSegment contourSegs
    beforeOpenSide   = drop startSegmentIndex contourSegs ++ take beforeStopSegmentIndex contourSegs
    beforeClosedSide = take (beforeStopSegmentIndex - startSegmentIndex) $ drop startSegmentIndex contourSegs
    beforeStopSegmentIndex = 1 + segIndex motorcycleInSegment contourSegs

    -- |  Return the line segments we're responsible for straight skeletoning.
    gatherLineSegs :: Side -> [LineSeg]
    gatherLineSegs s =
      case s of
        SideAfter -> if findSegFromStart contour outSeg motorcycleInSegment == outSeg
                     -- test whether we can gather our segments from the stop segment to the end ++ first one until the segment the motorcycle hits...
                     then afterOpenSide
                     -- .. or by starting at the stop segment, and stopping after the segment the motorcycle hits
                     else afterClosedSide
        SideBefore -> if findSegFromStart contour outSeg motorcycleInSegment == motorcycleInSegment
                      -- test whether we can gather our segments from the stop segment to the end ++ first one until the segment the motorcycle hits...
                      then beforeOpenSide
                      -- .. or by starting at the stop segment, and stopping after the segment the motorcycle hits
                      else beforeClosedSide

    -- | Get the index of a specific segment, in a list of segments.
    segIndex :: LineSeg -> [LineSeg] -> Int
    segIndex seg segs = fromMaybe (error "cannot find item") $ elemIndex seg segs

-- | use a single straight division to cut a section of a contour out, converting it to a cell.
-- Always assumes the open side.
-- Always assumes the open side does not create a loop in the contour.
createCellFromStraightWalls :: Slist [LineSeg] -> [CellDivide] -> Cell
createCellFromStraightWalls (Slist [segSet] _) [cellDivide@(CellDivide (DividingMotorcycles motorcycle@(Motorcycle (_,outSeg) _) _) _)] = Cell (slist [(slist $ gatherLineSegs, Just cellDivide)])
  where
    startSegmentIndex = segIndex outSeg segSet
    -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the first of the two segments (from the beginning of the contour).
    (motorcycleInSegment, eitherMotorcycleOutPoint) = fromMaybe (error "no intersections?") $ motorcycleMightIntersectWith segSet motorcycle
    -- the segment that a motorcycle intersects the contour on, or if it intersected between two segments, the last of the two segments (from the beginning of the contour).
    motorcycleOutSegment = case eitherMotorcycleOutPoint of
                             (Left point2) -> error $ "not doing anything with: " <> show point2 <> "\n"
                             (Right _) -> motorcycleInSegment
    afterOpenSide   = drop afterStopSegmentIndex segSet ++ take startSegmentIndex segSet
    afterStopSegmentIndex = segIndex motorcycleOutSegment segSet
    beforeOpenSide   = drop startSegmentIndex segSet ++ take beforeStopSegmentIndex segSet
    beforeStopSegmentIndex = 1 + segIndex motorcycleInSegment segSet

    -- |  Return the line segments we're responsible for straight skeletoning.
    gatherLineSegs :: [LineSeg]
    gatherLineSegs = if startBeforeEnd then beforeOpenSide else afterOpenSide
      where
        startBeforeEnd = elemIndex (fromMaybe (error "missed") $ segSetStartOfDivide segSet cellDivide) segSet `compare` elemIndex (fromMaybe (error "also missed") $ segSetEndOfDivide segSet cellDivide) segSet == LT

    -- | Get the index of a specific segment, in a list of segments.
    segIndex :: LineSeg -> [LineSeg] -> Int
    segIndex seg segs = fromMaybe (error "cannot find item") $ elemIndex seg segs

-- | Search a contour starting at the beginning, and return the first of the two line segments given
findSegFromStart :: Contour -> LineSeg -> LineSeg -> LineSeg
findSegFromStart c seg1 seg2 = case catMaybes (foundSeg seg1 seg2 <$> lineSegsOfContour c) of
                                 [] -> error "could not find requested segment."
                                 [a] -> a
                                 (a:_) -> a
  where
    foundSeg s1 s2 sn
      | sn == s1  = Just s1
      | sn == s2  = Just s2
      | otherwise = Nothing

-- | Add a pair of NodeTrees together along a Divide, to create a straight skeleton. The straight skeleton should have it's NodeTrees in order.
addNodeTreesOnDivide :: NodeTree -> NodeTree -> CellDivide -> StraightSkeleton
addNodeTreesOnDivide nodeTree1 nodeTree2 division = StraightSkeleton [sortNodeTrees $ nodeTree1 : nodeTree2 : nodetreesFromDivision division] (slist [])
  where
    nodetreesFromDivision :: CellDivide -> [NodeTree]
    nodetreesFromDivision cellDivision@(CellDivide motorcycles maybeENode) = case motorcycles of
                                                                               (DividingMotorcycles _ (Slist [] 0)) -> res
                                                                               (DividingMotorcycles firstMotorcycle (Slist [secondMotorcycle] 1)) -> if motorcyclesAreAntiCollinear firstMotorcycle secondMotorcycle
                                                                                                                                                     then res
                                                                                                                                                     else errorOut
                                                                               (DividingMotorcycles _ (Slist _ _)) -> errorOut
      where
        res = case maybeENode of
                (Just eNode) -> [makeNodeTree (motorcycleToENode <$> motorcyclesInDivision cellDivision) (INodeSet $ slist []), makeNodeTree [eNode] (INodeSet $ slist [])]
                Nothing -> [makeNodeTree (motorcycleToENode <$> motorcyclesInDivision cellDivision) (INodeSet $ slist [])]
        errorOut = error "tried to add two NodeTrees with a non-bilateral cellDivide"

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
      | firstSegOf cell == lastCSegOf m = endpoint $ firstSegOf cell
      | lastSegOf cell == firstCSegOf m = startPoint $ lastSegOf cell
      | otherwise = error $ "unhandled case: " <> show cell <> "\n" <> show m <> "\n" <> show (lastSegOf cell) <> "\n" <> show (firstSegOf cell) <> "\n"
      where
        startPoint (LineSeg a _) = a
        firstCSegOf (Motorcycle (seg1,_) _) = seg1
        lastCSegOf (Motorcycle (_, seg2) _) = seg2
