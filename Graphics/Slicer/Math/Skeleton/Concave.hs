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

{-
   This contains a geometric solver handling the creation of a straight skeleton for all concave contours without holes
-}

-- inherit instances when deriving.
{-# LANGUAGE DerivingStrategies #-}

-- So we can section tuples
{-# LANGUAGE TupleSections #-}

module Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion, getFirstArc, getOutsideArc, makeENodes, averageNodes, eNodesOfOutsideContour) where

import Prelude (Eq, Show, Bool(True, False), Either(Left, Right), String, Ord, Ordering(GT,LT), notElem, otherwise, ($), (>), (<), (<$>), (==), (/=), error, (&&), fst, and, (<>), show, not, max, concat, compare, uncurry, null, (||), min, snd, filter, id, notElem, zip)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, isJust)

import Data.List (takeWhile, sortBy)

import Data.List as DL (head, last)

import Data.List.Extra (unsnoc)

import Slist.Type (Slist(Slist))

import Slist (slist, one, cons, len)

import Slist as SL (head, tail)

import Graphics.Implicit.Definitions (ℝ)

import Graphics.Slicer.Math.Contour (lineSegsOfContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), mapWithFollower, distance)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPair)

import Graphics.Slicer.Math.Line (endpoint)

import Graphics.Slicer.Math.PGA (PLine2(PLine2), PPoint2, eToPLine2, flipPLine2, normalizePLine2, distanceBetweenPPoints, pLineIsLeft, angleBetween, join2PPoint2)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), INode(INode), INodeSet(INodeSet), NodeTree, Arcable(hasArc, outOf), Pointable(canPoint, pPointOf), concavePLines, eNodeToINode, noIntersection, intersectionOf, isAntiCollinear, finalOutOf, getPairs, isCollinear, indexPLinesTo, isParallel, linePairs, makeINode, sortedPLines)

import Graphics.Slicer.Math.Skeleton.NodeTrees (makeNodeTree)

-- | error type.
data PartialNodes = PartialNodes !INodeSet !String
  deriving (Show, Eq)

-- | Think: like Maybe, but min treats empty as greater than a value, rather than less.
data Topped x = Something !x | Empty
  deriving (Show, Ord, Eq)

-- | whether a Topped has contents or not.
isSomething :: Topped a -> Bool
isSomething (Something _) = True
isSomething Empty         = False

justToSomething :: Maybe a -> Topped a
justToSomething val = case val of
                        Nothing -> Empty
                        (Just a) -> Something a

-- | Recurse on a set of nodes until we have a complete NodeTree.
--   Only works on a sequnce of concave line segments, when there are no holes in the effected area.
skeletonOfConcaveRegion :: [LineSeg] -> NodeTree
skeletonOfConcaveRegion inSegs
  | loop == False && isJust (finalOutOf result) = result
  | loop == True && finalOutOf result == Nothing = result
  | otherwise = error $ "illegal nodeTree." <> show inSegs <> "\n" <> show result <> "\n"
  where
    result = getNodeTree (firstENodes inSegs loop)
    -- are the incoming line segments a loop?
    loop = endpoint (DL.last inSegs) == startPoint (DL.head inSegs)
           || distance (endpoint $ DL.last inSegs) (startPoint $ DL.head inSegs) < fudgeFactor
      where
        startPoint (LineSeg p1 _) = p1
        -- Note: fudgefactor is to make up for Double being Double, and math not necessarilly being perfect.
        fudgeFactor :: ℝ
        fudgeFactor = 0.000000000000002


    -- Generate the first generation of nodes, from the passed in line segments.
    -- If the line segments are a loop, use the appropriate function to create the initial Nodes.
    firstENodes :: [LineSeg] -> Bool -> [ENode]
    firstENodes firstSegs segsInLoop
      | segsInLoop = makeENodesLooped firstSegs
      | otherwise  = makeENodes firstSegs

    -- | get a NodeTree from a set of generations of nodes.
    -- FIXME: geometry may require more than one NodeTree, or may require spines, which are still a concept in flux.
    getNodeTree :: [ENode] -> NodeTree
    getNodeTree [] = error "no Nodes to generate a nodetree from?"
    getNodeTree initialGeneration = makeNodeTree initialGeneration $ res initialGeneration
      where
        -- | apply the recursive NodeTree solver.
        res :: [ENode] -> INodeSet
        res inGen = INodeSet $ sortINodesByENodes $ errorIfLeft (skeletonOfNodes inGen [])
          where
            sortINodesByENodes (INodeSet generations)
             | len generations == 0 = generations
             | len generations == 1 = slist $ [[orderInsByENodes (DL.head $ SL.head generations)]]
             | len generations == 2 && firstGenWithoutFlips == [] = slist $ [[lastGen (rawSortGeneration $ SL.head generations) (DL.head $ SL.head $ SL.tail generations)]]
             | len generations == 2 = slist $ [firstGenWithoutFlips] <> [[lastGen (rawSortGeneration $ SL.head generations) (DL.head $ SL.head $ SL.tail generations)]]
             | otherwise = error "too many generations?"
              where
                -- the first PLine in the input enode set.
                firstPLine = outOf $ DL.head inGen
                firstGenWithoutFlips = indexTo $ rawSortGeneration $ SL.head generations
                lastGen :: [INode] -> INode -> INode
                lastGen rawPriorGen oneInode = case flippedINodesOf rawPriorGen of
                                                 Nothing -> oneInode
                                                 (Just flippedINode) -> addINodeToParent flippedINode oneInode
{-                nGenWithoutFlips rawPriorGen thisGen = if null $ flippedINodesOf rawPriorGen
                                                       then indexTo $ rawSortGeneration thisGen
                                                       else error $ "found flipped INode: " <> show (flippedINodesOf rawPriorGen) <> "\n"
-}
                -- force a list of nodes to start with the node closest to the firstPLine, but not before the firstPLine.
                indexTo :: [INode] -> [INode]
                indexTo iNodes = iNodesBeforePLine iNodes <> iNodesAfterPLine iNodes
                  where
                    iNodesBeforePLine myINodes = filter (\a -> firstPLine `pLineIsLeft` firstInOf a /= Just False) myINodes
                    -- nodes in the right order, after the divide.
                    iNodesAfterPLine myINodes = withoutFlippedINodes $ filter (\a -> firstPLine `pLineIsLeft` firstInOf a == Just False) myINodes
                    withoutFlippedINodes maybeFlippedINodes = filter (\a -> a `notElem` (flippedINodesOf maybeFlippedINodes) ) maybeFlippedINodes
                -- FIXME: there's no way this is right.
                flippedINodesOf :: [INode] -> Maybe INode
                flippedINodesOf inodes = case filter (\a -> firstPLine `pLineIsLeft` firstInOf a == Just False) inodes of
                                           [] -> Nothing
                                           [a] -> Just a
                                           (xs) -> error $ "more than one flipped inode?" <> show xs <> "\n"
                -- Sort a generation by the first in PLine.
                rawSortGeneration = sortBy (\a b -> if firstInOf a `pLineIsLeft` firstInOf b == Just False then LT else GT)
                firstInOf (INode firstIn _ _ _) = firstIn
--                lastInOf (INode _ secondIn moreIns _)
--                  | len moreIn == 0 = secondIn
--                  | otherwise = SL.last moreIns
                orderInsByENodes (INode firstIn secondIn (Slist moreIn _) out) = makeINode (indexPLinesTo firstPLine $ sortedPLines $ firstIn:secondIn:moreIn) out
                addINodeToParent (INode _ _ _ Nothing) _ = error "cannot merge an inode with no output!"
                addINodeToParent (INode firstIn1 secondIn1 (Slist moreIn1 _) (Just out1)) (INode firstIn2 secondIn2 (Slist moreIn2 _) out2) =
                  makeINode (indexPLinesTo firstPLine $ sortedPLines $ (firstIn1:secondIn1:moreIn1) <> (withoutPLine out1 $ firstIn2:secondIn2:moreIn2)) out2
                  where
                    withoutPLine :: PLine2 -> [PLine2] -> [PLine2]
                    withoutPLine myPLine pLines = filter (\a -> a /= myPLine) pLines
    -- | Handle the recursive resolver failing.
    errorIfLeft :: Either PartialNodes INodeSet -> INodeSet
    errorIfLeft (Left failure) = error $ "Fail!\n" <> show failure <> "\ninSegs: " <> show inSegs <> "\n" <> show (firstENodes inSegs loop) <> "\nloop:" <> show loop <> "\n"
    errorIfLeft (Right val)    = val

    -- | Apply a recursive algorithm to solve the node set.
    --   FIXME: does not handle more than two point intersections of arcs properly.
    skeletonOfNodes :: [ENode] -> [INode] -> Either PartialNodes INodeSet
    skeletonOfNodes eNodes iNodes =
      case eNodes of
        [] -> case iNodes of
                --  zero nodes == return emptyset. allows us to simplify our return loop.
                [] -> Right $ INodeSet $ slist []
                [INode {}] -> if not loop
                           then Right $ INodeSet $ one iNodes -- just hand back single node requests.
                           else errorLen1 -- A one node loop makes no sense, reject.
                [iNode1,iNode2] -> handleTwoNodes iNode1 iNode2
                (_:_:_:_) -> handleThreeOrMoreNodes
        [eNode] -> case iNodes of
                     [] -> if not loop
                           then Right $ INodeSet $ one [eNodeToINode eNode] -- just hand back single node requests.
                           else errorLen1  -- A one node loop makes no sense, reject.
                     [iNode] -> handleTwoNodes eNode iNode
                     (_:_:_) -> handleThreeOrMoreNodes
        [eNode1,eNode2] -> case iNodes of
                             [] -> handleTwoNodes eNode1 eNode2
                             (_:_) -> handleThreeOrMoreNodes
        (_:_:_:_) -> handleThreeOrMoreNodes
      where
        errorLen1 = Left $ PartialNodes (INodeSet $ one iNodes) "NOMATCH - length 1?"
        --   Handle the the case of two nodes.
        handleTwoNodes :: (Arcable a, Pointable a, Show a, Arcable b, Pointable b, Show b) => a -> b -> Either PartialNodes INodeSet
        handleTwoNodes node1 node2
          | isAntiCollinear (outOf node1) (outOf node2) = Right $ INodeSet $ one [makeAntiCollinearPair node1 node2]
          | isCollinear (outOf node1) (outOf node2) = Left $ PartialNodes (INodeSet $ one iNodes) $ "cannot handle collinear nodes:\n" <> show node1 <> "\n" <> show node2 <> "\n"
          | intersectsInPoint node1 node2 && not loop = Right $ INodeSet $ one [averageNodes node1 node2]
          | intersectsInPoint node1 node2 &&
            (distanceBetweenPPoints (pPointOf node1) (intersectionOf (outOf node1) (outOf node2)) < fudgeFactor
            || distanceBetweenPPoints (pPointOf node2) (intersectionOf (outOf node1) (outOf node2)) < fudgeFactor)
            && loop = Right $ INodeSet $ one [makeINode (sortedPLines [outOf node1,outOf node2]) Nothing]
          | otherwise = errorLen2
          where
            errorLen2 = Left $ PartialNodes (INodeSet $ one iNodes) $ "NOMATCH - length 2?\n" <> show node1 <> "\n" <> show node2 <> "\n"
            -- Note: fudgefactor is to make up for Double being Double, and math not necessarilly being perfect.
            fudgeFactor :: ℝ
            fudgeFactor = 0.000000000000002

        --   Handle the the case of 3 or more nodes.
        handleThreeOrMoreNodes
          | endsAtSamePoint = Right $ INodeSet $ one [makeINode (sortedPLines $ (outOf <$> eNodes) <> (outOf <$> iNodes)) Nothing]
          | hasShortestPair = Right $ INodeSet $ averageOfShortestPairs `cons` inodesOf (errorIfLeft (skeletonOfNodes remainingENodes (remainingINodes <> averageOfShortestPairs)))
          | otherwise = errorLen3
          where
            inodesOf (INodeSet set) = set
        errorLen3 = error $ "shortestPairDistance: " <> show shortestPairDistance
                    <> "\nePairDistance: " <> show shortestEPairDistance <> "\nshortestEPairs: " <> show (shortestPairs eNodes) <> "\nePairResults: " <> show (uncurry averageNodes <$> shortestPairs eNodes) <> "\n" <> show (isSomething shortestEPairDistance) <> "\n"
                    <> "\niPairDistance: " <> show shortestIPairDistance <> "\nshortestIPairs: " <> show (shortestPairs iNodes) <> "\niPairResults: " <> show (uncurry averageNodes <$> shortestPairs iNodes) <> "\n" <> show (isSomething shortestIPairDistance) <> "\n"
                    <> "\nmixedPairDistance: " <> show shortestMixedPairDistance <> "\nshortestMixedPairs: " <> show shortestMixedPairs <> "\nMixedPairResults: " <> show (uncurry averageNodes <$> shortestMixedPairs) <> "\n" <> show (isSomething shortestMixedPairDistance) <> "\n" <> show (shortestEPairDistance == shortestPairDistance)
                    <> "\nresultingENodes: " <> show remainingENodes <> "\nresultingNodes: " <> show remainingINodes <> "\nthisGen: " <> show averageOfShortestPairs

        -- | When a set of nodes end in the same point, we may need to create a Node with all of the nodes as input. This checks for that case.
        endsAtSamePoint :: Bool
        endsAtSamePoint = and $ mapWithFollower (==) $ mapWithFollower intersectionOf ((outOf <$> nonAntiCollinearNodes eNodes (antiCollinearNodePairsOf eNodes)
                                                                                               <> firstAntiCollinearNodes (antiCollinearNodePairsOf eNodes)) <>
                                                                                       (outOf <$> nonAntiCollinearNodes iNodes (antiCollinearNodePairsOf iNodes)
                                                                                               <> firstAntiCollinearNodes (antiCollinearNodePairsOf iNodes)))
          where
            -- since anti-collinear nodes end at the same point, only count one of them.
            firstAntiCollinearNodes nodePairs = fst <$> nodePairs
            -- find the nodes that do not have an anti-collinear pair.
            -- FIXME: is there a better way do do this with Ord?
            nonAntiCollinearNodes :: (Eq a) => [a] -> [(a,a)] -> [a]
            nonAntiCollinearNodes myNodes nodePairs = filter (\a -> notElem a $ allAntiCollinearNodes nodePairs) myNodes
              where
                allAntiCollinearNodes myNodePairs = (fst <$> myNodePairs) <> (snd <$> myNodePairs)

        -- | make sure we have a potential intersection between two nodes to work with.
        hasShortestPair :: Bool
        hasShortestPair = not $ null (intersectingNodePairsOf eNodes) && null (intersectingNodePairsOf iNodes) && null intersectingMixedNodePairs

        -- | make sure we have a potential intersection between two nodes to work with.
        makeAntiCollinearPair :: (Arcable a, Arcable b) => a -> b -> INode
        makeAntiCollinearPair node1 node2 = makeINode (sortedPair node1 node2) Nothing

        -- | determine the exterior nodes available for calculation during the next recurse.
        remainingENodes :: [ENode]
        remainingENodes = (if isSomething shortestMixedPairDistance && shortestPairDistance == shortestMixedPairDistance
                           then filter (\a -> a `notElem` (fst <$> shortestMixedPairs))
                           else id) $
                          (if isSomething shortestEPairDistance && shortestPairDistance == shortestEPairDistance
                           then filter (\a -> a `notElem` (fst <$> shortestPairs eNodes) <> (snd <$> shortestPairs eNodes))
                           else id) eNodes

        -- | determine the interior nodes available for calculation during the next recurse.
        remainingINodes :: [INode]
        remainingINodes = (if isSomething shortestMixedPairDistance && shortestPairDistance == shortestMixedPairDistance
                           then filter (\a -> a `notElem` (snd <$> shortestMixedPairs))
                           else id) $
                          (if isSomething shortestIPairDistance && shortestPairDistance == shortestIPairDistance
                           then filter (\a -> a `notElem` (fst <$> shortestPairs iNodes) <> (snd <$> shortestPairs iNodes))
                           else id) iNodes

        -- | collect our set of result nodes.
        -- FIXME: these should have a specific order. what should it be?
        averageOfShortestPairs :: [INode]
        averageOfShortestPairs = ePairsFound <> mixedPairsFound <> iPairsFound
          where
            ePairsFound = if isSomething shortestEPairDistance && shortestEPairDistance == shortestPairDistance
                          then uncurry averageNodes <$> shortestPairs eNodes
                          else []
            mixedPairsFound = if isSomething shortestMixedPairDistance && shortestMixedPairDistance == shortestPairDistance
                              then uncurry averageNodes <$> shortestMixedPairs
                              else []
            iPairsFound = if isSomething shortestIPairDistance && shortestIPairDistance == shortestPairDistance
                          then uncurry averageNodes <$> shortestPairs iNodes
                          else []

        -- | calculate the distances to the shortest pairs of nodes. the shortest pair, along with all of the pairs of the same length, will be in our result set.
        shortestPairDistance = min (min shortestEPairDistance shortestMixedPairDistance) (min shortestIPairDistance shortestMixedPairDistance)
        shortestIPairDistance = case shortestPairs iNodes of
                                  [] -> Empty
                                  (firstPair:_) -> justToSomething $ uncurry distanceToIntersection firstPair
        shortestEPairDistance = case shortestPairs eNodes of
                                  [] -> Empty
                                  (firstPair:_) -> justToSomething $ uncurry distanceToIntersection firstPair
        shortestMixedPairDistance = case shortestMixedPairs of
                                      [] -> Empty
                                      (firstPair:_) -> justToSomething $ uncurry distanceToIntersection firstPair

        -- | get the list of sorted pairs of intersecting nodes.
        shortestPairs :: (Arcable a, Pointable a, Show a) => [a] -> [(a, a)]
        shortestPairs myNodes = case nodePairsSortedByDistance myNodes of
                                  [] -> []
                                  [onePair] -> [onePair]
                                  (pair:morePairs) -> pair : takeWhile (\a -> uncurry distanceToIntersection a == uncurry distanceToIntersection pair) morePairs
          where
            -- | get the intersection of each node pair, sorted based on which one has the shortest maximum distance of the two line segments from it's ancestor nodes to the intersection point.
            nodePairsSortedByDistance :: (Arcable a, Pointable a, Show a) => [a] -> [(a, a)]
            nodePairsSortedByDistance myNodes' = sortBy (\(p1n1, p1n2) (p2n1, p2n2) -> distanceToIntersection p1n1 p1n2 `compare` distanceToIntersection p2n1 p2n2) $ intersectingNodePairsOf myNodes'

        -- | get the pairs of intersecting nodes of differing types that we might be putting into this generation.
        shortestMixedPairs :: [(ENode, INode)]
        shortestMixedPairs = case nodePairsSortedByDistance of
                               [] -> []
                               [onePair] -> [onePair]
                               (pair:morePairs) -> pair : takeWhile (\a -> uncurry distanceToIntersection a == uncurry distanceToIntersection pair) morePairs
          where
            -- | get the intersection of each node pair, sorted based on which one has the shortest maximum distance of the two line segments from it's ancestor nodes to the intersection point.
            nodePairsSortedByDistance :: [(ENode, INode)]
            nodePairsSortedByDistance = sortBy (\(p1n1, p1n2) (p2n1, p2n2) -> distanceToIntersection p1n1 p1n2 `compare` distanceToIntersection p2n1 p2n2) intersectingMixedNodePairs

        -- | find nodes of two different types that can intersect.
        intersectingMixedNodePairs :: [(ENode, INode)]
        intersectingMixedNodePairs = catMaybes $ (\(node1, node2) -> if intersectsInPoint node1 node2 then Just (node1, node2) else Nothing) <$> getMixedPairs eNodes iNodes
          where
            getMixedPairs ::  [a] -> [b] -> [(a, b)]
            getMixedPairs set1 set2 = concat $ (\a -> (a,) <$> set2) <$> set1

        -- | find nodes that can intersect.
        intersectingNodePairsOf :: (Arcable a, Show a) => [a] -> [(a, a)]
        intersectingNodePairsOf inNodes = catMaybes $ (\(node1, node2) -> if intersectsInPoint node1 node2 then Just (node1, node2) else Nothing) <$> getPairs inNodes

        -- | find nodes that have output segments that are antiCollinear with one another.
        antiCollinearNodePairsOf :: (Arcable a) => [a] -> [(a, a)]
        antiCollinearNodePairsOf inNodes = catMaybes $ (\(node1, node2) -> if outSegsAntiCollinear node1 node2 then Just (node1, node2) else Nothing) <$> getPairs inNodes
          where
            outSegsAntiCollinear :: (Arcable a) => a -> a -> Bool
            outSegsAntiCollinear node1 node2
              | hasArc node1 && hasArc node2 = isAntiCollinear (outOf node1) (outOf node2)
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

-- | For a given par of nodes, construct a new internal node, where it's parents are the given nodes, and the line leaving it is along the the obtuse bisector.
--   Note: this should be hidden in skeletonOfConcaveRegion, but it's exposed here, for testing.
averageNodes :: (Arcable a, Pointable a, Show a, Arcable b, Pointable b, Show b) => a -> b -> INode
averageNodes n1 n2
  | not (hasArc n1) || not (hasArc n2) = error $ "Cannot get the average of nodes if one of the nodes does not have an out!\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | not (canPoint n1) || not (canPoint n2) = error $ "Cannot get the average of nodes if we cannot resolve them to a point!\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | isParallel (outOf n1) (outOf n2) = error $ "Cannot get the average of nodes if their outputs never intersect!\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | isCollinear (outOf n1) (outOf n2) = error $ "Cannot (yet) handle two input plines that are collinear.\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | isAntiCollinear (outOf n1) (outOf n2) = error $ "Cannot (yet) handle two input plines that are collinear.\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | otherwise                 = makeINode (sortedPair n1 n2) $ Just $ getOutsideArc (pPointOf n1) (outOf n1) (pPointOf n2) (outOf n2)

-- | take a pair of arcables, and return their outOf, in a sorted order.
sortedPair :: (Arcable a, Arcable b) => a -> b -> [PLine2]
sortedPair n1 n2 = sortedPLines [outOf n1, outOf n2]

-- | Get a PLine along the angle bisector of the intersection of the two given line segments, pointing in the 'obtuse' direction.
--   Note: Ensure input line segments are normalised.
--   Note: we normalize our output lines, but don't bother normalizing our input lines, as the ones we output and the ones getFirstArc outputs are normalized.
--   Note: the outer PLine returned by two PLines in the same direction should be two PLines, whch are the same line in both directions.
getOutsideArc :: PPoint2 -> PLine2 -> PPoint2 -> PLine2 -> PLine2
getOutsideArc ppoint1 pline1 ppoint2 pline2
  | pline1 == pline2 = error "need to be able to return two PLines."
  | noIntersection pline1 pline2 = error $ "no intersection between pline " <> show pline1 <> " and " <> show pline2 <> ".\n"
  | l1TowardPoint && l2TowardPoint = flipPLine2 $ getInsideArc pline1 (flipPLine2 pline2)
  | l1TowardPoint                  = flipPLine2 $ getInsideArc pline1 pline2
  | l2TowardPoint                  = getInsideArc pline2 pline1
  | otherwise                      = getInsideArc (flipPLine2 pline2) pline1
    where
      l1TowardPoint = towardIntersection ppoint1 pline1 (intersectionOf pline1 pline2)
      l2TowardPoint = towardIntersection ppoint2 pline2 (intersectionOf pline1 pline2)
      -- Determine if the line segment formed by the two given points starts with the first point, or the second.
      -- Note that due to numeric uncertainty, we should not rely on Eq here, and must check the sign of the angle.
      -- FIXME: sometimes this breaks down, if pp1 and pl1 have distance between them?
      towardIntersection :: PPoint2 -> PLine2 -> PPoint2 -> Bool
      towardIntersection pp1 pl1 in1
        | pp1 == in1                           = False
        | angleBetween constructedLine pl1 > 0 = True
        | otherwise                            = False
        where
          constructedLine = join2PPoint2 pp1 in1

-- | Get a PLine along the angle bisector of the intersection of the two given line segments, pointing in the 'acute' direction.
--   Note that we normalize our output, but don't bother normalizing our input lines, as the ones we output and the ones getFirstArc outputs are normalized.
--   Note that we know that the inside is to the right of the first line given, and that the first line points toward the intersection.
getInsideArc :: PLine2 -> PLine2 -> PLine2
getInsideArc pline1 pline2@(PLine2 pv2)
  | pline1 == pline2 = error "need to be able to return two PLines."
  | noIntersection pline1 pline2 = error $ "no intersection between pline " <> show pline1 <> " and " <> show pline2 <> ".\n"
  | otherwise = normalizePLine2 $ PLine2 $ addVecPair flippedPV1 pv2
  where
      (PLine2 flippedPV1) = flipPLine2 pline1

-- | Make a first generation set of nodes, AKA, a set of arcs that come from the points where line segments meet, toward the inside of the contour.
makeENodes :: [LineSeg] -> [ENode]
makeENodes segs = case segs of
                         [] -> error "got empty list at makeENodes.\n"
                         [a] -> error $ "not enough line segments at makeENodes: " <> show a <> "\n"
                         [a,b] -> [makeENode a b]
                         xs -> case unsnoc $ mapWithFollower makeENode xs of
                                 Nothing -> error "impossible!"
                                 Just (i,_) -> i

-- | Find the non-reflex virtexes of a contour, and create ENodes from them.
--   This function is meant to be used on an exterior contour.
eNodesOfOutsideContour :: Contour -> [ENode]
eNodesOfOutsideContour contour = catMaybes $ onlyNodes <$> zip (linePairs contour) (mapWithFollower concavePLines $ lineSegsOfContour contour)
  where
    onlyNodes :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe ENode
    onlyNodes ((seg1, seg2), Just _) = Just $ makeENode seg1 seg2
    onlyNodes ((_, _), Nothing) = Nothing

-- | Make a first generation set of nodes, AKA, a set of arcs that come from the points where line segments meet, toward the inside of the contour.
makeENodesLooped :: [LineSeg] -> [ENode]
makeENodesLooped segs
  | null segs = error "got empty list at makeNodes.\n"
  | otherwise = mapWithFollower makeENode segs

-- | Make a first generation node.
makeENode :: LineSeg -> LineSeg -> ENode
makeENode seg1 seg2 = ENode (seg1,seg2) $ getFirstArc seg1 seg2

-- | Get a PLine in the direction of the inside of the contour, at the angle bisector of the intersection of the two given line segments.
--   Note that we normalize the output of eToPLine2, because by default, it does not output normalized lines.
getFirstArc :: LineSeg -> LineSeg -> PLine2
getFirstArc seg1 seg2 = getInsideArc (normalizePLine2 $ eToPLine2 seg1) (normalizePLine2 $ eToPLine2 seg2)

-- | Find the reflex virtexes of a contour, and draw Nodes from them.
--   This function is for use on interior contours.
{-
convexNodes :: Contour -> [Node]
convexNodes contour = catMaybes $ onlyNodes <$> zip (linePairs contour) (mapWithFollower convexPLines $ linesOfContour contour)
  where
    onlyNodes :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe Node
    onlyNodes ((seg1, seg2), maybePLine)
      | isJust maybePLine = Just $ Node (Left (seg1,seg2)) $ fromJust maybePLine
      | otherwise         = Nothing
-}

