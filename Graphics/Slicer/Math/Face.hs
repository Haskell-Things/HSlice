-- Slicer.
{-
 - Copyright 2020 Julia Longtin
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

module Graphics.Slicer.Math.Face (Face, makeFaces, addLineSegs, leftRegion, rightRegion, convexMotorcycles, Node(Node), makeFirstNodes, Motorcycle(Motorcycle), findStraightSkeleton, StraightSkeleton(StraightSkeleton), Spine(Spine)) where

import Prelude (Int, (==), otherwise, (<$>), ($), length, Show, (/=), error, (<>), show, Eq, Show, (<>), (<), (/), floor, fromIntegral, Either(Left, Right), (+), (*), (-), (++), (>), min, Bool(True,False), zip, head, (&&), (.), (||), fst, take, drop, filter, init, null, tail, last, concat, snd, not)

import Data.List (elemIndex)

import Data.Maybe( Maybe(Just,Nothing), fromMaybe,  catMaybes, isJust, fromJust, isNothing)

import Graphics.Slicer.Math.Definitions (Contour(PointSequence), Point2, mapWithFollower, mapWithNeighbors, addPoints)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPair)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg), lineSegFromEndpoints, LineSegError(LineSegFromPoint), makeLineSegsLooped)

import Graphics.Slicer.Math.PGA (lineIsLeft, pointOnPerp, distancePPointToPLine, pToEPoint2, PLine2(PLine2), PPoint2, plinesIntersectIn, Intersection(NoIntersection, HitEndPoint, HitStartPoint), PIntersection(PColinear,IntersectsIn,PParallel,PAntiParallel), eToPLine2, translatePerp, plineFromEndpoints, intersectsWith, eToPPoint2, flipPLine2, pPointsOnSameSideOfPLine)

import Graphics.Implicit.Definitions (ℝ, Fastℕ)

-- | A Face.
--   A portion of a contour, with a real side, and arcs (line segments between nodes) dividing it from other faces.
--   Faces have no holes, and their arcs and nodes (lines and points) are from a straight skeleton of a contour.
data Face = Face { _edge :: LineSeg, _firstArc :: PLine2, _arcs :: [PLine2], _lastArc :: PLine2}
  deriving (Show, Eq)

-- | A Node
--   A point in our straight skeleton where two arcs intersect, resulting in another arc, OR a point where two lines of a contour intersect, emmiting a line toward the interior of a contour.
data Node = Node { _inArc1 :: Either LineSeg PLine2, _inArc2 :: Either LineSeg PLine2, _outArc :: PLine2 }
  deriving (Show, Eq)

-- | A Spine component.
--   like a node, only without the in and out heirarchy. always connects to outArcs from the last item in a Node set.
--   Used for glueing node sets together.
--   FIXME: spines and nodes make triangles complicated. handle specially.
data Spine = Spine {_arc1 :: Either SpineID PLine2, _arc2 :: Either SpineID PLine2, _arc3 :: Either SpineID PLine2}
  deriving (Show, Eq)

-- The index of the spine this spine connects to.
type SpineID = Int

-- A motorcycle. a Ray eminating from an intersection between two line segments toward the interior of a contour. the angle between tho line segments must make this a reflex vertex.
data Motorcycle = Motorcycle { _inSeg :: LineSeg, _outSeg :: LineSeg, _path :: PLine2 }
  deriving (Show, Eq)

data StraightSkeleton = StraightSkeleton { _nodeSets :: [[[Node]]], _spineNodes :: [Spine] }
  deriving (Show, Eq)

-- | construct a set of faces, using a straight skeleton to divide up the area of a contour, and the holes in the contour.
-- FIXME: woefully incomplete.
makeFaces :: Contour -> [Contour] -> [Face]
makeFaces contour holes = facesFromStraightSkeleton $ findStraightSkeleton contour holes

findStraightSkeleton :: Contour -> [Contour] -> StraightSkeleton
findStraightSkeleton contour holes
  -- Use the algorithm from Christopher Tscherne's master's thesis.
  | null holes && length outsideContourMotorcycles == 1 = tscherneMerge dividingMotorcycle maybeOpposingNode leftSide rightSide
  | otherwise = error "Do not know how to calculate a straight skeleton for this contour!"
  where
    dividingMotorcycle = head outsideContourMotorcycles
    leftSide  = leftRegion contour dividingMotorcycle
    rightSide = rightRegion contour dividingMotorcycle
    -- | find nodes where the arc coresponding to them is colinear with the dividing Motorcycle.
    -- FIXME: Yes, this is implemented wrong, and needs to find only the one node opposing the dividing motorcycle. construct a line segment from the node and the motorcycle, and see what segments intersect?
    maybeOpposingNode
      | length outsideContourMotorcycles == 1 && length opposingNodes == 1 = Just $ head $ opposingNodes
      | length outsideContourMotorcycles == 1 && length opposingNodes == 0 = Nothing
      | otherwise                                                          = error "more than one opposing node. impossible situation."
      where
        opposingNodes =  filter (\(Node _ _ outArc) -> plinesIntersectIn outArc (pathOf dividingMotorcycle) == PColinear) $ concaveNodes contour
        pathOf (Motorcycle _ _ path) = path
    outsideContourMotorcycles = convexMotorcycles contour
    -- | not yet used, but at least implemented properly.
--    motorcyclesOfHoles = concaveMotorcycles <$> holes


-- | Apply the merge algorithm from Christopher Tscherne's master's thesis.
tscherneMerge :: Motorcycle -> Maybe Node -> [[Node]] -> [[Node]] -> StraightSkeleton
tscherneMerge dividingMotorcycle@(Motorcycle (LineSeg rightPoint _) (LineSeg startPoint2 endDistance2) path) opposingNodes leftSide rightSide
  -- If the two sides do not have an influence on one another, and the last line out of the two sides intersects the motorcycle at the same point, tie the sides and the motorcycle together, and we're done.
  | (null $ crossoverNodes leftSide leftPoint dividingMotorcycle) &&
    (null $ crossoverNodes rightSide rightPoint dividingMotorcycle) &&
    (isNothing opposingNodes) &&
    plinesIntersectIn (finalPLine leftSide) path == plinesIntersectIn (finalPLine rightSide) path =
      StraightSkeleton [leftSide, rightSide, [[motorcycleToNode dividingMotorcycle]]] [Spine (Right $ finalPLine leftSide) (Right $ finalPLine rightSide) (Right $ finalPLine $ [[motorcycleToNode dividingMotorcycle]])]
  -- If the two sides do not have an influence on one another, and the last line out of the two sides intersects the motorcycle at the same point, tie the sides and the motorcycle together.. and... FIXME?.
  | (null $ crossoverNodes leftSide leftPoint dividingMotorcycle) &&
    (null $ crossoverNodes rightSide rightPoint dividingMotorcycle) &&
    (isJust opposingNodes) &&
    plinesIntersectIn (finalPLine leftSide) path == plinesIntersectIn (finalPLine rightSide) path =
      StraightSkeleton [leftSide, rightSide, [[motorcycleToNode dividingMotorcycle]]] [Spine (Right $ finalPLine leftSide) (Right $ finalPLine rightSide) (Right $ finalPLine $ [[motorcycleToNode dividingMotorcycle]])]
  | otherwise = error $ "failing to apply Tscherne's method.\n" <>
                        show (crossoverNodes leftSide leftPoint dividingMotorcycle)  <> "\n" <>
                        show (crossoverNodes rightSide rightPoint dividingMotorcycle)  <> "\n" <>
                        show dividingMotorcycle <> "\n"
  where
    leftPoint = addPoints startPoint2 endDistance2
    finalPLine :: [[Node]] -> PLine2
    finalPLine generations
      | null generations = error $ "cannot have final PLine of empty side!\n"
      | otherwise = plineOut $ target $ last generations
      where
        plineOut (Node _ _ pline) = pline
        target generation = head generation
    motorcycleToNode :: Motorcycle -> Node
    motorcycleToNode (Motorcycle inSeg outSeg mcpath) = Node (Left inSeg) (Left outSeg) mcpath
    crossoverNodes :: [[Node]] -> Point2 -> Motorcycle -> [Node]
    crossoverNodes generations pointOnSide (Motorcycle _ _ mcpath) = concat (crossoverGen <$> init generations)
      where
        crossoverGen :: [Node] -> [Node]
        crossoverGen generation = filter (\a -> Just True == (intersectionSameSide mcpath (eToPPoint2 pointOnSide) a)) generation
  -- determine if a node is on one side of the motorcycle, or the other.
  -- assumes the starting point of the second line segment is a point on the path.
    intersectionSameSide :: PLine2 -> PPoint2 -> Node -> Maybe Bool
    intersectionSameSide path pointOnSide node = pPointsOnSameSideOfPLine (saneIntersection $ plinesIntersectIn (fst $ plineAncestors node) (snd $ plineAncestors node)) pointOnSide path
      where
        plineAncestors :: Node -> (PLine2, PLine2)
        plineAncestors (Node (Left seg1)    (Left seg2) _)    = (eToPLine2 seg1, eToPLine2 seg2)
        plineAncestors (Node (Left seg)     (Right pline) _)  = (eToPLine2 seg, pline)
        plineAncestors (Node (Right pline)  (Left seg) _)     = (pline, eToPLine2 seg)
        plineAncestors (Node (Right pline1) (Right pline2) _) = (pline1, pline2)
        saneIntersection :: PIntersection -> PPoint2
        saneIntersection (IntersectsIn ppoint) = ppoint
        saneIntersection a = error $ "insane result of intersection of two ancestor lines:" <> show a <> "\n"

-- | Calculate a partial straight skeleton, for the part of a contour that is on the left side of the point that a motorcycle's path starts at.
--   meaning we will evaluate the line segments from the point the motorcycle left from, to the segment it intersects, in the order they are in the original contour.
leftRegion :: Contour -> Motorcycle -> [[Node]]
leftRegion contour motorcycle = concaveStraightSkeleton $ matchLineSegments contour motorcycle
  where
    -- Return the line segments we're responsible for straight skeletoning.
    matchLineSegments :: Contour -> Motorcycle -> [LineSeg]
    matchLineSegments c (Motorcycle inSeg outSeg path)
      | findSegFromStart c outSeg motorcycleEndSegment ==               outSeg = (drop (stopSegmentIndex) $ linesOfContour c) ++ take (startSegmentIndex outSeg) (linesOfContour c)
      | findSegFromStart c outSeg motorcycleEndSegment == motorcycleEndSegment = take ((startSegmentIndex outSeg) - stopSegmentIndex) $ drop (stopSegmentIndex) (linesOfContour c)
      | otherwise = error "this should be impossible."
        where
          -- the segment that a motorcycle intersects the contour on.
          motorcycleEndSegment = fst $ motorcycleIntersectsAt c inSeg outSeg path
          startSegmentIndex seg = segIndex seg (linesOfContour c)
          stopSegmentIndex = segIndex motorcycleEndSegment (linesOfContour c)

-- | Calculate a partial straight skeleton, for the part of a contour that is on the 'right' side of a contour, when the contour is bisected by a motorcycle.
--   by right side, we mean consisting of the segments from the point the motorcycle left from, to the intersection, in the order they are in the original contour.
rightRegion :: Contour -> Motorcycle -> [[Node]]
rightRegion contour motorcycle@(Motorcycle inSeg outSeg path) = concaveStraightSkeleton $ matchLineSegments contour motorcycle
  where
    -- Return the line segments we're responsible for straight skeletoning.
    matchLineSegments :: Contour -> Motorcycle -> [LineSeg]
    matchLineSegments c (Motorcycle seg1 seg2 mc)
      | findSegFromStart c seg2 motorcycleEndSegment ==                 seg2 = take (1 + stopSegmentIndex - startSegmentIndex seg2) $ drop (startSegmentIndex seg2) (linesOfContour c)
      | findSegFromStart c seg2 motorcycleEndSegment == motorcycleEndSegment =(drop (startSegmentIndex seg2) $ linesOfContour c) ++ take (stopSegmentIndex+1) (linesOfContour c)
      | otherwise = error "this should be impossible."
        where
          -- the segment that a motorcycle intersects the contour on.
          motorcycleEndSegment = fst $ motorcycleIntersectsAt c seg1 seg2 mc
          startSegmentIndex seg = segIndex seg (linesOfContour c)
          stopSegmentIndex = segIndex motorcycleEndSegment (linesOfContour c)

-- | Find the non-reflex virtexes of a contour and draw motorcycles from them.
--   A reflex virtex is any point where the line in and the line out are convex, when looked at from inside of the contour.
--   This function is meant to be used on interior contours.
concaveMotorcycles :: Contour -> [Motorcycle]
concaveMotorcycles contour = catMaybes $ onlyMotorcycles <$> zip (linePairs contour) (mapWithFollower concavePLines $ linesOfContour contour)
  where
    onlyMotorcycles :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe Motorcycle
    onlyMotorcycles ((seg1, seg2), maybePLine)
      | isJust maybePLine = Just $ Motorcycle seg1 seg2 $ fromJust maybePLine
      | otherwise         = Nothing

-- | Find the non-reflex virtexes of a contour and draw motorcycles from them. Useful for contours that are a 'hole' in a bigger contour.
--   This function is meant to be used on the exterior contour.
convexMotorcycles :: Contour -> [Motorcycle]
convexMotorcycles contour = catMaybes $ onlyMotorcycles <$> zip (linePairs contour) (mapWithFollower convexPLines $ linesOfContour contour)
  where
    onlyMotorcycles :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe Motorcycle
    onlyMotorcycles ((seg1, seg2), maybePLine)
      | isJust maybePLine = Just $ Motorcycle seg1 seg2 $ fromJust maybePLine
      | otherwise         = Nothing

-- | Find the non-reflex virtexes of a contour, and draw Nodes from them.
--   This function is meant to be used on the exterior contour.
concaveNodes :: Contour -> [Node]
concaveNodes contour = catMaybes $ onlyNodes <$> zip (linePairs contour) (mapWithFollower concavePLines $ linesOfContour contour)
  where
    onlyNodes :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe Node
    onlyNodes ((seg1, seg2), maybePLine)
      | isJust maybePLine = Just $ Node (Left seg1) (Left seg2) $ fromJust maybePLine
      | otherwise         = Nothing

-- | Find the reflex virtexes of a contour, and draw Nodes from them.
--   This function is meant to be used on interior contours.
convexNodes :: Contour -> [Node]
convexNodes contour = catMaybes $ onlyNodes <$> zip (linePairs contour) (mapWithFollower convexPLines $ linesOfContour contour)
  where
    onlyNodes :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe Node
    onlyNodes ((seg1, seg2), maybePLine)
      | isJust maybePLine = Just $ Node (Left seg1) (Left seg2) $ fromJust maybePLine
      | otherwise         = Nothing

-- look at the two line segments, and determine if they are convex. if they are, construct a PLine2 bisecting them.
convexPLines :: LineSeg -> LineSeg -> Maybe PLine2
convexPLines seg1 seg2
  | fromMaybe (False) $ lineIsLeft seg1 seg2  = Just $ PLine2 $ addVecPair pv1 pv2
  | otherwise                                 = Nothing
  where
    (PLine2 pv1) = eToPLine2 seg1
    (PLine2 pv2) = flipPLine2 $ eToPLine2 seg2

-- look at the two line segments, and determine if they are concave. if they are, construct a PLine2 bisecting them.

concavePLines :: LineSeg -> LineSeg -> Maybe PLine2
concavePLines seg1 seg2
  | fromMaybe (False) $ lineIsLeft seg1 seg2  = Nothing
  | otherwise                                 = Just $ PLine2 $ addVecPair pv1 pv2
  where
    (PLine2 pv1) = eToPLine2 seg1
    (PLine2 pv2) = flipPLine2 $ eToPLine2 seg2

-- | create a skeleton for the given segments. requires there to be no reflex virtexes between the segments, and for the segments to be in order.
concaveStraightSkeleton segs
  | null segs = error "got empty list at concaveStraightSkeleton.\n"
  | otherwise = straightSkeletonOf [makeFirstNodes segs]

-- | Make a first generation set of nodes, AKA, a set of motorcycles that come from the points where line segments meet, on the inside of the contour.
makeFirstNodes :: [LineSeg] -> [Node]
makeFirstNodes segs
  | null segs = error "got empty list at makeFirstNodes.\n"
  | otherwise = init $ mapWithFollower makeFirstNode $ segs
  where
    makeFirstNode :: LineSeg -> LineSeg -> Node
    makeFirstNode seg1 seg2 = Node (Left seg1) (Left seg2) $ getArc seg1 seg2

-- | Get a PLine in the direction of the inside of the contour, at the angle bisector of the intersection of the two given line segments.
getArc :: LineSeg -> LineSeg -> PLine2
getArc seg1 seg2 = PLine2 $ addVecPair pv1 pv2
  where
    (PLine2 pv1) = eToPLine2 seg1
    (PLine2 pv2) = flipPLine2 $ eToPLine2 seg2

-- | Recurse a set of nodes until we have a complete straight skeleton (down to one node in the final generation).
straightSkeletonOf :: [[Node]] -> [[Node]]
straightSkeletonOf [] = []
straightSkeletonOf (firstGen:[])
  | length firstGen == 2 = [firstGen,[averageNodes (head firstGen) (head $ tail firstGen)]]
  | otherwise = error $ "cannot handle node count:" <> show (length firstGen) <> "\n" <> concat ((<> "\n") . show <$> firstGen) <> "\n"
straightSkeletonOf (firstGen:moreGens)
  | length (last moreGens) == 2 = [firstGen,[averageNodes (head (last moreGens)) (head $ tail (last moreGens))]]
  | otherwise = error $ "cannot handle node count:" <> show (length (last moreGens)) <> "\n"

averageNodes :: Node -> Node -> Node
averageNodes (Node _ _ pLine1@(PLine2 pv1)) (Node _ _ pLine2) = Node (Right pLine1) (Right pLine2) (PLine2 $ addVecPair pv1 pv2)
  where
    (PLine2 pv2) = flipPLine2 $ pLine2

linesOfContour :: Contour -> [LineSeg]
linesOfContour (PointSequence contourPoints) = makeLineSegsLooped contourPoints

-- Get pairs of lines from the contour, including one pair that is the last line paired with the first.
linePairs :: Contour -> [(LineSeg, LineSeg)]
linePairs c = mapWithFollower (\a b -> (a,b)) $ linesOfContour c

-- Get the index of a specific segment, in a list of segments.
segIndex :: LineSeg -> [LineSeg] -> Int
segIndex seg segs = fromMaybe (error "cannot find item") $ elemIndex seg segs

-- search a contour starting at the beginning, and return the first of the two line segments given
findSegFromStart :: Contour -> LineSeg -> LineSeg -> LineSeg
findSegFromStart c seg1 seg2 = head $ catMaybes $ foundSeg seg1 seg2 <$> linesOfContour c
  where
    foundSeg s1 s2 sn = if sn == s1 then Just s1 else if sn == s2 then Just s2 else Nothing

-- | Find what line segment a motorcycle runs into, if the motorcycle is emitted from between the two given segments.
--   If the motorcycle lands between two segments, return the second segment, as well.
motorcycleIntersectsAt :: Contour -> LineSeg -> LineSeg -> PLine2 -> (LineSeg, Maybe LineSeg)
motorcycleIntersectsAt contour inSeg outSeg path
  | length (getMotorcycleIntersections path contour) == 2 && length foundSegEvents == 1 = (fst (head $ foundSegEvents), snd (head $ foundSegEvents))
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
        saneIntersections  _ (seg, (Right (IntersectsIn _))) _ = Just (seg, Nothing)
        saneIntersections  _ (_  , (Left  NoIntersection))        _ = Nothing
        saneIntersections  _ (_  , (Right PParallel))             _ = Nothing
        saneIntersections  _ (_  , (Right PAntiParallel))         _ = Nothing
        saneIntersections  _                                 (seg , (Left (HitStartPoint _ _))) (seg2 , (Left (HitEndPoint   _ _))) = Just (seg, Just seg2)
        saneIntersections (_   , (Left (HitStartPoint _ _))) (_   , (Left (HitEndPoint   _ _)))  _                                  = Nothing
        saneIntersections  _                                 (_   , (Left (HitEndPoint   _ _))) (_   , (Left (HitStartPoint  _ _))) = Nothing
        saneIntersections (seg , (Left (HitEndPoint   _ _))) (seg2, (Left (HitStartPoint _ _)))  _                                  = Just (seg, Just seg2)
        saneIntersections l1 l2 l3 = error $ "insane result of saneIntersections:\n" <> show l1 <> "\n" <> show l2 <> "\n" <> show l3 <> "\n"

-- | take a straight skeleton, and create faces from it.
facesFromStraightSkeleton :: StraightSkeleton -> [Face]
facesFromStraightSkeleton (StraightSkeleton nodeLists spines) = error "incomplete!"

-- | Place line segments on a face. Might return remainders, in the form of one or multiple un-filled faces.
addLineSegs :: Face -> Fastℕ -> ℝ -> ([LineSeg], Maybe [Face])
-- | handle faces with five or more sides. basically the same as the 4 side case, only deal with finding the closest intersection point differently.
addLineSegs (Face edge firstArc (a2:a3:xs) lastArc) n lw = error "undefined!"
-- | handle faces that have four sides.
addLineSegs (Face edge firstArc (midArc:[]) lastArc) n lw = ((foundLineSegs maybeFoundLineSegs)  ++ subSides, remainder)
  where
    maybeFoundLineSegs     = [ lineSegFromEndpoints (pToEPoint2 $ intersectionOf newSide firstArc) (pToEPoint2 $ intersectionOf newSide lastArc) | newSide <- newSides ]
    newSides               = [ translatePerp (eToPLine2 edge) ((lw/2)+(lw*(fromIntegral (segmentNum-1)))) | segmentNum <- [1..linesToRender] ]
    finalLine              = translatePerp (eToPLine2 edge) ((fromIntegral linesToRender)*lw)
    finalSide              = errorIfLeft $ lineSegFromEndpoints (pToEPoint2 $ intersectionOf finalLine firstArc) (pToEPoint2 $ intersectionOf finalLine lastArc)
    remainder              = if (distancePPointToPLine (intersectionOf firstArc midArc) (eToPLine2 edge)) /= (distancePPointToPLine (intersectionOf midArc lastArc) (eToPLine2 edge))
                             then subRemains
                             else Nothing
    (subSides, subRemains) = if firstArcLonger
                             then addLineSegs (Face finalSide firstArc [] lastArc) n lw
                             else addLineSegs (Face finalSide midArc   [] lastArc) n lw
    linesUntilEnd :: Fastℕ
    linesUntilEnd          = floor $ if firstArcLonger
                                     then (distancePPointToPLine (intersectionOf firstArc midArc) (eToPLine2 edge)) / lw
                                     else (distancePPointToPLine (intersectionOf midArc lastArc) (eToPLine2 edge)) / lw
    firstArcLonger         = (distancePPointToPLine (intersectionOf firstArc midArc) (eToPLine2 edge)) > (distancePPointToPLine (intersectionOf midArc lastArc) (eToPLine2 edge))
    linesToRender          = min linesUntilEnd n
-- | handle faces that are triangular wedges. easiest case.
addLineSegs (Face edge firstArc [] lastArc) n lw = (foundLineSegs maybeFoundLineSegs, remainder)
  where
    maybeFoundLineSegs     = [ lineSegFromEndpoints (pToEPoint2 $ intersectionOf newSide firstArc) (pToEPoint2 $ intersectionOf newSide lastArc) | newSide <- newSides ]
    newSides               = [ translatePerp (eToPLine2 edge) ((lw/2)+(lw*(fromIntegral (segmentNum-1)))) | segmentNum <- [1..linesToRender] ]
    finalLine              = translatePerp (eToPLine2 edge) ((fromIntegral linesToRender)*lw)
    finalSide              = errorIfLeft $ lineSegFromEndpoints (pToEPoint2 $ intersectionOf finalLine firstArc) (pToEPoint2 $ intersectionOf finalLine lastArc)
    remainder              = if (distancePPointToPLine (intersectionOf firstArc lastArc) (eToPLine2 edge)) /= (fromIntegral (linesToRender-1))*lw
                             then Just [(Face finalSide firstArc [] lastArc)]
                             else Nothing
    linesUntilEnd :: Fastℕ
    linesUntilEnd          = floor $ (distancePPointToPLine (intersectionOf firstArc lastArc) (eToPLine2 edge)) / lw
    linesToRender          = min linesUntilEnd n

-- | Add infill to the area of a set of faces that was not covered in lines.
addInfill :: [Face] -> [LineSeg]
addInfill = error $ "unimplemented!"

foundLineSegs :: [Either LineSegError LineSeg] -> [LineSeg]
foundLineSegs segs     = errorIfLeft <$> segs

errorIfLeft :: Either LineSegError LineSeg -> LineSeg
errorIfLeft lnSeg      = case lnSeg of
  Left (LineSegFromPoint point) -> error $ "tried to construct a line segment from two identical points: " <> show point <> "\n"
  Right                 lineSeg -> lineSeg

intersectionOf :: PLine2 -> PLine2 -> PPoint2
intersectionOf pl1 pl2 = saneIntersection $ plinesIntersectIn pl1 pl2
  where
    saneIntersection PColinear            = error "impossible!"
    saneIntersection PParallel            = error "impossible!"
    saneIntersection PAntiParallel        = error "impossible!"
    saneIntersection (IntersectsIn point) = point
