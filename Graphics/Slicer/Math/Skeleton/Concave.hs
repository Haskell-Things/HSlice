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

{-
   This contains a geometric solver handling the creation of a straight skeleton for all concave contours without holes
-}

-- inherit instances when deriving.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

-- So we can section tuples
{-# LANGUAGE TupleSections #-}

module Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion, getFirstArc, makeFirstENodes, averageNodes) where
import Prelude (Eq, Show, Bool(True, False), Either(Left, Right), String, Ord, otherwise, ($), last, (<$>), (==), (++), error, length, (&&), head, fst, and, (<>), show, not, max, concat, compare, uncurry, null, (||), min, snd, filter, init, id, notElem, elem, (+))
  
import Graphics.Slicer.Math.Definitions (Point2, mapWithFollower)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), INode(INode), NodeTree(NodeTree), Arcable(hasArc, outOf), Pointable(canPoint, ePointOf), noIntersection, intersectionOf, pPointOf, isCollinear, getPairs, isParallel)

import Graphics.Slicer.Math.PGA (pToEPoint2, PLine2(PLine2), PPoint2, eToPLine2, flipPLine2, normalizePLine2, distanceBetweenPPoints)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, fromJust)

import Data.List (takeWhile, nub, sortBy)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg), lineSegFromEndpoints)

import Graphics.Implicit.Definitions (ℝ)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPair)

-- error type.
data PartialNodes = PartialNodes [[INode]] String
  deriving (Show, Eq)

-- Think: like Maybe, but min treats empty as greater than a value, rather than less.
data Topped x = Something x | Empty
  deriving (Show, Ord, Eq)

isSomething :: Topped a -> Bool
isSomething (Something _) = True
isSomething Empty         = False

-- | Recurse on a set of nodes until we have a complete NodeTree.
--   Only works on a sequnce of concave line segments, when there are no holes in the effected area.
skeletonOfConcaveRegion :: [LineSeg] -> Bool -> NodeTree
skeletonOfConcaveRegion inSegs loop = getNodeTree (firstENodes inSegs loop)
  where
    -- Generate the first generation of nodes, from the passed in line segments.
    -- If the line segments are a loop, use the appropriate function to create the initial Nodes.
    firstENodes :: [LineSeg] -> Bool -> [ENode]
    firstENodes firstSegs segsInLoop
      | segsInLoop = makeFirstENodesLooped firstSegs
      | otherwise  = makeFirstENodes firstSegs

    -- | get a NodeTree from a set of generations of nodes.
    -- FIXME: geometry may require more than one NodeTree, or may require spines, which are still a concept in flux.
    getNodeTree :: [ENode] -> NodeTree
    getNodeTree [] = error "no Nodes to generate a nodetree from?"
    getNodeTree initialGeneration = NodeTree initialGeneration $ res initialGeneration
      where
        -- | apply the recursive NodeTree solver.
        res :: [ENode] -> [[INode]]
        res inGen = errorIfLeft (skeletonOfNodes inGen [])

    -- | Handle the recursive resolver failing.
    errorIfLeft :: Either PartialNodes [[INode]] -> [[INode]]
    errorIfLeft (Left failure) = error $ "Fail!\n" <> show failure <> "\ninSegs: " <> show inSegs <> "\nloop:" <> show loop <> "\n"
    errorIfLeft (Right val)    = val

    -- | Apply a recursive algorithm to solve the node set.
    --   FIXME: does not handle more than two point intersections of arcs properly.
    skeletonOfNodes :: [ENode] -> [INode] -> Either PartialNodes [[INode]]
    skeletonOfNodes eNodes iNodes
      --  zero nodes == return emptyset. allows us to simplify our return loop.
      | null eNodes && null iNodes = Right []
      -- Handle single nodes.
      --  A one node loop makes no sense, reject.
     | length eNodes + length iNodes == 1 && loop = Left $ PartialNodes [iNodes] "NOMATCH - Length 1?"
      --  FIXME: this is right, i think?
      | length eNodes == 1 && null iNodes = Right [[eNodeToINode $ head eNodes]]
      | null eNodes && length iNodes == 1 = Right [iNodes]
      -- Handle a pair of nodes.
      | isCollinearPair = Right [[makeCollinearPair]]
      | null eNodes && length iNodes == 2 && intersectsInPoint (head iNodes) (last iNodes) && not loop = Right [[averageNodes (head iNodes) (last iNodes)]]
      | length eNodes == 2 && null iNodes && intersectsInPoint (head eNodes) (last eNodes) && not loop = Right [[averageNodes (head eNodes) (last eNodes)]]
      | length eNodes == 1 && length iNodes == 1 && intersectsInPoint (head eNodes) (head iNodes) && not loop = Right [[averageNodes (head eNodes) (head iNodes)]]
      | length eNodes + length iNodes == 2 = Left $ PartialNodes [iNodes] "NOMATCH - length 2?"
      --   Handle the the case of 3 or more nodes.
      | endsAtSamePoint = Right [[INode ((outOf <$> eNodes) ++ (outOf <$> iNodes)) Nothing]]
      | hasShortestPair         = Right $ [averageOfShortestPairs] ++ errorIfLeft (skeletonOfNodes remainingENodes (remainingINodes ++ averageOfShortestPairs))
      | otherwise = error $ "shortestPairDistance: " <> show (shortestPairDistance)
                                <> "\nePairDistance: " <> show (shortestEPairDistance) <> "\nnum of ePairs Found: " <> show (length $ shortestPairs eNodes) <> "\nshortestEPairs: " <> show (shortestPairs eNodes) <> "\nePairResults: " <> show (uncurry averageNodes <$> shortestPairs eNodes) <> "\n" <> show (isSomething shortestEPairDistance) <> "\n" -- <> show (shortestEPairDistance == shortestPairDistance)
                                <> "\niPairDistance: " <> show (shortestIPairDistance) <> "\nnum of iPairs Found: " <> show (length $ shortestPairs iNodes) <> "\nshortestIPairs: " <> show (shortestPairs iNodes) <> "\niPairResults: " <> show (uncurry averageNodes <$> shortestPairs iNodes) <> "\n" <> show (isSomething shortestIPairDistance) <> "\n" -- <> show (shortestEPairDistance == shortestPairDistance) 
                                <> "\nmixedPairDistance: " <> show (shortestMixedPairDistance) <> "\nnum of mixedPairs Found: " <> show (length $ shortestMixedPairs) <> "\nshortestMixedPairs: " <> show shortestMixedPairs <> "\nMixedPairResults: " <> show (uncurry averageNodes <$> shortestMixedPairs) <> "\n" <> show (isSomething shortestMixedPairDistance) <> "\n" <> show (shortestEPairDistance == shortestPairDistance) 
                                <> "\nresultingENodes: " <> show remainingENodes <> "\nresultingNodes: " <> show remainingINodes <> "\nthisGen: " <> show averageOfShortestPairs
--      | otherwise = Left $ PartialNodes [] "NOMATCH"
      where
        -- | When a set of nodes end in the same point, we may need to create a Node with all of the nodes as input. This checks for that case.
        endsAtSamePoint :: Bool
        endsAtSamePoint = and $ mapWithFollower (==) $ mapWithFollower intersectionOf ((outOf <$> nonCollinearNodes eNodes (collinearNodePairsOf eNodes)
                                                                                               ++ firstCollinearNodes (collinearNodePairsOf eNodes)) ++
                                                                                       (outOf <$> nonCollinearNodes iNodes (collinearNodePairsOf iNodes)
                                                                                               ++ firstCollinearNodes (collinearNodePairsOf iNodes)))
          where
            firstCollinearNodes nodePairs = fst <$> nodePairs
            -- find the nodes that do not have a collinear pair.
            nonCollinearNodes :: (Eq a) => [a] -> [(a,a)] -> [a]
            nonCollinearNodes myNodes nodePairs = filter (\a -> not $ a `elem` allCollinearNodes nodePairs) myNodes
              where
                allCollinearNodes myNodePairs = nub $ (fst <$> myNodePairs) ++ (snd <$> myNodePairs)

        -- | make sure we have a potential intersection between two nodes to work with. 
        hasShortestPair :: Bool
        hasShortestPair = not $ (null $ intersectingNodePairsOf eNodes) && (null $ intersectingNodePairsOf iNodes) && (null $ intersectingMixedNodePairs)

        -- | make sure we have a potential intersection between two nodes to work with. 
        isCollinearPair :: Bool
        isCollinearPair = length eNodes + length iNodes == 2 && isCollinear (head allNodeOutputs) (last allNodeOutputs)
          where
            allNodeOutputs = (outOf <$> eNodes) ++ (outOf <$> iNodes)

        -- | make sure we have a potential intersection between two nodes to work with. 
        makeCollinearPair :: INode
        makeCollinearPair
          | null eNodes        && length iNodes == 2 = INode [(outOf $ head iNodes), (outOf $ last iNodes)] Nothing
          | length eNodes == 2 && null iNodes        = INode [(outOf $ head eNodes), (outOf $ last eNodes)] Nothing
          | length eNodes == 1 && length iNodes == 1 = INode [(outOf $ head eNodes), (outOf $ head iNodes)] Nothing
          | otherwise = error "cannot construct a collinear pear: we don't have two nodes to work with."

        -- | determine the exterior nodes available for calculation during the next recurse.
        remainingENodes :: [ENode]
        remainingENodes = (if isSomething shortestMixedPairDistance && shortestPairDistance == shortestMixedPairDistance
                           then filter (\a -> a `notElem` (fst <$> shortestMixedPairs))
                           else id) $
                          (if isSomething shortestEPairDistance && shortestPairDistance == shortestEPairDistance
                           then filter (\a -> a `notElem` (fst <$> shortestPairs eNodes) ++ (snd <$> shortestPairs eNodes))
                           else id) eNodes

        -- | determine the interior nodes available for calculation during the next recurse.
        remainingINodes :: [INode]
        remainingINodes = (if isSomething shortestMixedPairDistance && shortestPairDistance == shortestMixedPairDistance
                           then filter (\a -> a `notElem` (snd <$> shortestMixedPairs))
                           else id) $
                          (if isSomething shortestIPairDistance && shortestPairDistance == shortestIPairDistance
                           then filter (\a -> a `notElem` (fst <$> shortestPairs iNodes) ++ (snd <$> shortestPairs iNodes))
                           else id) iNodes

        -- | create our set of result nodes.
        -- FIXME: these should have a specific order. what should it be?
        averageOfShortestPairs :: [INode]
        averageOfShortestPairs = ePairsFound ++ mixedPairsFound ++ iPairsFound

        ePairsFound = if isSomething shortestEPairDistance && shortestEPairDistance == shortestPairDistance
                      then (uncurry averageNodes <$> shortestPairs eNodes)
                      else []
        mixedPairsFound = if isSomething shortestMixedPairDistance && shortestMixedPairDistance == shortestPairDistance
                          then (uncurry averageNodes <$> shortestMixedPairs)
                          else []
        iPairsFound = if isSomething shortestIPairDistance && shortestIPairDistance == shortestPairDistance
                      then (uncurry averageNodes <$> shortestPairs iNodes)
                      else []

        shortestPairDistance = min (min shortestEPairDistance shortestMixedPairDistance) (min shortestIPairDistance shortestMixedPairDistance)
        shortestIPairDistance
          | null (shortestPairs iNodes) = Empty
          | otherwise = Something $ fromJust $ uncurry distanceToIntersection $ head $ shortestPairs iNodes
        shortestEPairDistance
          | null (shortestPairs eNodes) = Empty
          | otherwise = Something $ fromJust $ uncurry distanceToIntersection $ head $ shortestPairs eNodes
        shortestMixedPairDistance
          | null shortestMixedPairs = Empty
          | otherwise = Something $ fromJust $ uncurry distanceToIntersection $ head shortestMixedPairs

        -- | get the pairs of intersecting nodes we're putting into this generation.
        shortestPairs :: (Arcable a, Pointable a, Show a) => [a] -> [(a, a)]
        shortestPairs myNodes
          | null myNodes = []
          | otherwise    = takeWhile (\a -> uncurry distanceToIntersection a == uncurry distanceToIntersection (head $ nodePairsSortedByDistance myNodes)) (nodePairsSortedByDistance myNodes)
          where
            -- | get the intersection of each node pair, sorted based on which one has the shortest maximum distance of the two line segments from it's ancestor nodes to the intersection point.
            nodePairsSortedByDistance :: (Arcable a, Pointable a, Show a) => [a] -> [(a, a)]
            nodePairsSortedByDistance myNodes' = sortBy (\(p1n1, p1n2) (p2n1, p2n2) -> distanceToIntersection p1n1 p1n2 `compare` distanceToIntersection p2n1 p2n2) $ intersectingNodePairsOf myNodes'

        -- | get the pairs of intersecting nodes of differing types that we might be putting into this generation.
        shortestMixedPairs :: [(ENode, INode)]
        shortestMixedPairs
          | null iNodes || null eNodes = []
          | otherwise = takeWhile (\a -> uncurry distanceToIntersection a == uncurry distanceToIntersection (head nodePairsSortedByDistance)) nodePairsSortedByDistance
          where
            -- | get the intersection of each node pair, sorted based on which one has the shortest maximum distance of the two line segments from it's ancestor nodes to the intersection point.
            nodePairsSortedByDistance :: [(ENode, INode)]
            nodePairsSortedByDistance = sortBy (\(p1n1, p1n2) (p2n1, p2n2) -> distanceToIntersection p1n1 p1n2 `compare` distanceToIntersection p2n1 p2n2) $ intersectingMixedNodePairs

        -- | find nodes of two different types that can intersect.
        intersectingMixedNodePairs :: [(ENode, INode)]
        intersectingMixedNodePairs = catMaybes $ (\(node1, node2) -> if intersectsInPoint node1 node2 then Just (node1, node2) else Nothing) <$> getMixedPairs eNodes iNodes
          where
            getMixedPairs ::  [a] -> [b] -> [(a, b)]
            getMixedPairs set1 set2 = concat $ (\a -> (a,) <$> set2) <$> set1

        -- | find nodes that can intersect.
        intersectingNodePairsOf :: (Arcable a, Show a) => [a] -> [(a, a)]
        intersectingNodePairsOf inNodes = catMaybes $ (\(node1, node2) -> if intersectsInPoint node1 node2 then Just (node1, node2) else Nothing) <$> getPairs inNodes

        -- | find nodes that have output segments that are collinear with one another.
        collinearNodePairsOf :: (Arcable a) => [a] -> [(a, a)]
        collinearNodePairsOf inNodes = catMaybes $ (\(node1, node2) -> if outSegsCollinear node1 node2 then Just (node1, node2) else Nothing) <$> getPairs inNodes
          where
            outSegsCollinear :: (Arcable a) => a -> a -> Bool
            outSegsCollinear node1 node2
              | hasArc node1 && hasArc node2 = isCollinear (outOf node1) (outOf node2)
              | otherwise = False

        -- | for a given pair of nodes, find the longest distance between one of the two nodes and the intersection of the two output plines.
        distanceToIntersection :: (Pointable a, Arcable a, Show a, Pointable b, Arcable b, Show b) => a -> b -> Maybe ℝ
        distanceToIntersection node1 node2
          | canPoint node1 && canPoint node2 && intersectsInPoint node1 node2 = Just $ max (distanceBetweenPPoints (pPointOf node1) (intersectionOf (outOf node1) (outOf node2))) (distanceBetweenPPoints (pPointOf node2) (intersectionOf (outOf node1) (outOf node2)))
          | otherwise                                                         = Nothing

        -- | Check if the intersection of two nodes results in a point or not.
        intersectsInPoint :: (Arcable a, Show a, Arcable b, Show b) => a -> b -> Bool
        intersectsInPoint node1 node2
          | hasArc node1 && hasArc node2 = not $ noIntersection (outOf node1) (outOf node2)
          | otherwise                    = error $ "cannot intersect a node with no output:\nNode1: " <> show node1 <> "\nNode2: " <> show node2 <> "\nnodes: " <> show iNodes <> "\n"

eNodeToINode :: ENode -> INode
eNodeToINode (ENode (seg1, seg2) arc) = INode [eToPLine2 seg1, eToPLine2 seg2] (Just arc)

-- | For a given set of nodes, construct a new internal node, where it's parents are the given nodes, and the line leaving it is along the the obtuse bisector.
--   Note: this should be hidden in skeletonOfConcaveRegion, but it's exposed here, for testing.
averageNodes :: (Arcable a, Pointable a, Show a, Arcable b, Pointable b, Show b) => a -> b -> INode
averageNodes n1 n2
  | not (hasArc n1) || not (hasArc n2) = error $ "Cannot get the average of nodes if one of the nodes does not have an out!\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | not (canPoint n1) || not (canPoint n2) = error $ "Cannot get the average of nodes if we cannot resolve them to a point!\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | isParallel  (outOf n1) (outOf n2) = error $ "Cannot get the average of nodes if their outputs never intersect!\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | isCollinear (outOf n1) (outOf n2) = error $ "Cannot (yet) handle two input plines that are collinear.\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | otherwise                 = INode [outOf n1,outOf n2] $ Just $ getOutsideArc (ePointOf n1) (outOf n1) (ePointOf n2) (outOf n2)

-- | Get a PLine along the angle bisector of the intersection of the two given line segments, pointing in the 'obtuse' direction.
--   Note: we normalize our output lines, but don't bother normalizing our input lines, as the ones we output and the ones getFirstArc outputs are normalized.
--   Note: the outer PLine returned by two PLines in the same direction should be two PLines, whch are the same line in both directions.
getOutsideArc :: Point2 -> PLine2 -> Point2 -> PLine2 -> PLine2
getOutsideArc point1 pline1 point2 pline2
  | pline1 == pline2 = error "need to be able to return two PLines."
  | noIntersection pline1 pline2 = error $ "no intersection between pline " <> show pline1 <> " and " <> show pline2 <> ".\n"
  | l1TowardPoint && l2TowardPoint = flipPLine2 $ getInsideArc point1 pline1 (pToEPoint2 $ intersectionOf pline1 pline2) (flipPLine2 pline2)
  | l1TowardPoint                  = flipPLine2 $ getInsideArc point1 pline1 point2 pline2
  | l2TowardPoint                  = getInsideArc point2 pline2 point1 pline1
  | otherwise                      = getInsideArc (pToEPoint2 $ intersectionOf pline1 pline2) (flipPLine2 pline2) point1 pline1
    where
      l1TowardPoint = towardIntersection point1 pline1 (intersectionOf pline1 pline2)
      l2TowardPoint = towardIntersection point2 pline2 (intersectionOf pline1 pline2)

-- Determine if the line segment formed by the two given points starts with the first point, or the second.
-- Note: PLine must be normalized.
towardIntersection :: Point2 -> PLine2 -> PPoint2 -> Bool
towardIntersection p1 pl1 in1
  | p1 == pToEPoint2 in1                                  = False
  | normalizePLine2 (eToPLine2 constructedLineSeg) == pl1 = True
  | otherwise                                             = False
  where
    constructedLineSeg :: LineSeg
    constructedLineSeg = foundLineSeg maybeConstructedLineSeg
      where
      foundLineSeg (Left a)  = error $ "encountered " <> show a <> "error."
      foundLineSeg (Right a) = a
      maybeConstructedLineSeg = lineSegFromEndpoints p1 (pToEPoint2 in1)

-- | Get a PLine along the angle bisector of the intersection of the two given line segments, pointing in the 'acute' direction.
--   Note that we normalize our output, but don't bother normalizing our input lines, as the ones we output and the ones getFirstArc outputs are normalized.
--   Note that we know that the inside is to the right of the first line given, and that the first line points toward the intersection.
getInsideArc :: Point2 -> PLine2 -> Point2 -> PLine2 -> PLine2
getInsideArc _ pline1 _ pline2@(PLine2 pv2)
  | pline1 == pline2 = error "need to be able to return two PLines."
  | noIntersection pline1 pline2 = error $ "no intersection between pline " <> show pline1 <> " and " <> show pline2 <> ".\n"
  | otherwise = normalizePLine2 $ PLine2 $ addVecPair flippedPV1 pv2
  where
      (PLine2 flippedPV1) = flipPLine2 pline1

-- | Make a first generation set of nodes, AKA, a set of arcs that come from the points where line segments meet, toward the inside of the contour.
makeFirstENodes :: [LineSeg] -> [ENode]
makeFirstENodes segs
  | null segs = error "got empty list at makeENodes.\n"
  | otherwise = init $ mapWithFollower makeFirstENode segs

-- | Make a first generation set of nodes, AKA, a set of arcs that come from the points where line segments meet, toward the inside of the contour.
makeFirstENodesLooped :: [LineSeg] -> [ENode]
makeFirstENodesLooped segs
  | null segs = error "got empty list at makeFirstNodes.\n"
  | otherwise = mapWithFollower makeFirstENode segs

-- | Make a first generation node.
makeFirstENode :: LineSeg -> LineSeg -> ENode
makeFirstENode seg1 seg2 = ENode (seg1,seg2) $ getFirstArc seg1 seg2

-- | Get a PLine in the direction of the inside of the contour, at the angle bisector of the intersection of the two given line segments.
--   Note that we normalize the output of eToPLine2, because by default, it does not output normalized lines.
getFirstArc :: LineSeg -> LineSeg -> PLine2
getFirstArc seg1@(LineSeg start1 _) seg2@(LineSeg start2 _) = getInsideArc start1 (normalizePLine2 $ eToPLine2 seg1) start2 (normalizePLine2 $ eToPLine2 seg2)


