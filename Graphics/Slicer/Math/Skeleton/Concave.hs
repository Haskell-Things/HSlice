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

module Graphics.Slicer.Math.Skeleton.Concave (skeletonOfConcaveRegion, findINodes, makeENode, makeENodes, averageNodes, eNodesOfOutsideContour) where

import Prelude (Eq, Show, Bool(True, False), Either(Left, Right), String, Ord, Ordering(GT,LT), notElem, otherwise, ($), (>), (<), (<$>), (==), (/=), (>=), error, (&&), fst, and, (<>), show, not, max, concat, compare, uncurry, null, (||), min, snd, filter, zip, any, (*), (+), Int, (.), (-), mempty, realToFrac)

import Prelude as PL (head, last, tail, init)

import Data.Either (lefts,rights)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, fromMaybe, isJust, isNothing, mapMaybe)

import Data.List (takeWhile, dropWhile, sortBy, nub)

import Numeric.Rounded.Hardware (getRounded)

import Data.List.Extra (unsnoc)

import Slist.Type (Slist(Slist))

import Slist (slist, one, cons, len, isEmpty, safeHead)

import Slist as SL (head, last, tail)

import Graphics.Implicit.Definitions (ℝ)

import Graphics.Slicer.Math.Arcs (getFirstArc, getInsideArc, getOutsideArc)

import Graphics.Slicer.Math.Contour (lineSegsOfContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2, endPoint, mapWithFollower, fudgeFactor, startPoint)

import Graphics.Slicer.Math.GeometricAlgebra (UlpSum(UlpSum))

import Graphics.Slicer.Math.Intersections (intersectionOf, intersectionBetween, isCollinear, isParallel, isAntiCollinear, noIntersection)

import Graphics.Slicer.Math.Lossy as Lossy (distancePPointToPLine)

import Graphics.Slicer.Math.PGA (Arcable(errOfOut, hasArc, outOf), Pointable(canPoint, pPointOf), PLine2, PLine2Err, CPPoint2(CPPoint2), PPoint2(PPoint2), distance2PP, eToPL, flipL, outAndErrOf, pLineIsLeft, distancePPointToPLineWithErr)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), ENodeSet(ENodeSet), INode(INode), INodeSet(INodeSet), NodeTree(NodeTree), concavePLines, getFirstLineSeg, getLastLineSeg, finalOutOf, firstInOf, getPairs, indexPLinesTo, insOf, lastINodeOf, linePairs, makeINode, sortedPLinesWithErr, isLoop)

import Graphics.Slicer.Math.Skeleton.NodeTrees (makeNodeTree, findENodeByOutput)

-- | Error type.
data PartialNodes = PartialNodes !INodeSet !String
  deriving (Show, Eq)

-- | Think: like Maybe, but min treats empty as greater than a value, rather than less.
data Topped x = Something !x | Empty
  deriving (Show, Ord, Eq)

-- | Whether a Topped has contents or not.
isSomething :: Topped a -> Bool
isSomething (Something _) = True
isSomething Empty         = False

-- | convert from Maybe to Something.
justToSomething :: Maybe a -> Topped a
justToSomething val = case val of
                        Nothing -> Empty
                        (Just a) -> Something a

-- | find a complete NodeTree for the given collection of line segments.
--   Only works on a sequnce of concave line segments, when there are no holes in the effected area.
skeletonOfConcaveRegion :: Slist [LineSeg] -> NodeTree
skeletonOfConcaveRegion inSegSets
  | not (isLoop inSegSets) && isJust (finalOutOf result) = result
  | not (isLoop inSegSets) && isNothing (finalOutOf result) && isHallway result = result
  | isLoop inSegSets && isNothing (finalOutOf result) = result
  | otherwise = error $ "generated illegal nodeTree:" <> show inSegSets <> "\n" <> show (isLoop inSegSets) <> "\n" <> show result <> "\n"
  where
    result = makeNodeTree initialENodes $ sortINodesByENodes (isLoop inSegSets) inSegSets $ findINodes inSegSets
    initialENodes = makeInitialGeneration (isLoop inSegSets) inSegSets

-- | Find a raw set of INodes representing the INodes of the solved NodeTree for this part of a contour.
findINodes :: Slist [LineSeg] -> INodeSet
findINodes inSegSets
  | len inSegSets == 1 =
      -- One continuous wall without gaps. may gap between the beginning and end of the contour, if this is not a loop.
      -- Just return the output of skeletonOfNodes.
      errorIfLeft $ skeletonOfNodes (isLoop inSegSets) inSegSets []
  | len inSegSets == 2 =
    -- Two walls, no closed ends. solve the ends of a hallway region, so we can then hand off the solutioning to our regular process.
    case initialENodes of
      [] -> INodeSet $ slist [[makeINode [getInsideArc (eToFlippedPL firstSeg) (eToPL lastSeg), getInsideArc (eToPL firstSeg) (eToFlippedPL lastSeg)] Nothing]]
        where
          firstSeg = SL.head $ slist $ SL.head inSegSets
          lastSeg = SL.head $ slist $ SL.last inSegSets
          eToFlippedPL lineSeg = (flipL res, resErr)
            where
              (res, resErr) = eToPL lineSeg
      [a] -> INodeSet $ slist [[makeINode [getInsideArc (eToPL lastSeg) (eToPL shortSide), getInsideArc (eToPL firstSeg) (eToPL shortSide)] (Just (flipL $ outOf a, errOfOut a))]]
        where
          firstSeg = fromMaybe (error "no first segment?") $ safeHead $ slist longSide
          lastSeg = SL.last $ slist longSide
          (shortSide,longSide)
            | null (SL.head inSegSets) = (SL.head $ slist $ SL.last inSegSets, SL.head inSegSets)
            | otherwise = (SL.head $ slist $ SL.head inSegSets, SL.last inSegSets)
      (_:_) -> error
               $ "too many items in makeInitialGeneration.\n"
               <> show initialENodes <> "\n"
               <> show inSegSets <> "\n"
               <> show (isLoop inSegSets) <> "\n"
  | otherwise = error "wrong count of inSegSets."
  where
    initialENodes = makeInitialGeneration (isLoop inSegSets) inSegSets

-- | check if a set of INodes in a given NodeTree is just a hallway.
-- A hallway is a portion of a contour consisting of only two sides. it is cut off from the closed ends of the contour by divides.
-- FIXME: looks dubious.
isHallway :: NodeTree -> Bool
isHallway (NodeTree _ iNodeSet) = iNodeSetHasOneMember iNodeSet
  where
    iNodeSetHasOneMember (INodeSet myINodeSet) = len myINodeSet == 1

-- | Create the set of ENodes for a set of segments
makeInitialGeneration :: Bool -> Slist [LineSeg] -> [ENode]
makeInitialGeneration gensAreLoop inSegSets = concat (firstENodes <$> inSegSets) <> maybeLoop
  where
    -- Generate the first generation of nodes, from the passed in line segments.
    -- If the line segments are a loop, use the appropriate function to create the initial Nodes.
    firstENodes :: [LineSeg] -> [ENode]
    firstENodes firstSegs = case firstSegs of
                              [] -> []
                              [LineSeg {}] -> []
                              (_:_) -> makeENodes firstSegs
    -- Add a closing ENode if this is a closed loop.
    maybeLoop = if gensAreLoop
                then [loopOfSegSets inSegSets]
                else []

loopOfSegSets :: Slist [LineSeg] -> ENode
loopOfSegSets inSegSets = case inSegSets of
                            (Slist [] _) -> error "no"
                            oneOrMoreSets@(Slist ((_:_:_):_) _) -> makeENode (startPoint $ PL.last $ SL.last oneOrMoreSets) (startPoint $ PL.head $ SL.head oneOrMoreSets) (endPoint $ PL.head $ SL.head oneOrMoreSets)
                            oneOrMoreSets@(Slist (_:_:_) _) -> makeENode (startPoint $ PL.last $ SL.last oneOrMoreSets) (startPoint $ PL.head $ SL.head oneOrMoreSets) (endPoint $ PL.head $ SL.head oneOrMoreSets)
                            (Slist _ _) -> error "yes"

-- | Handle the recursive resolver failing.
errorIfLeft :: Either PartialNodes INodeSet -> INodeSet
errorIfLeft (Left failure) = error $ "Fail!\n" <> show failure
errorIfLeft (Right val)    = val

-- | For a given par of nodes, construct a new internal node, where it's parents are the given nodes, and the line leaving it is along the the obtuse bisector.
--  Note: this should be hidden in skeletonOfConcaveRegion, but it's exposed here, for testing.
--  Note: assumes outOf the two input nodes is already normalized.
averageNodes :: (Arcable a, Pointable a, Arcable b, Pointable b) => a -> b -> INode
averageNodes n1 n2
  | not (hasArc n1) || not (hasArc n2) = error $ "Cannot get the average of nodes if one of the nodes does not have an out!\n" <> dumpInput
  | not (canPoint n1) || not (canPoint n2) = error $ "Cannot get the average of nodes if we cannot resolve them to a point!\n" <> dumpInput
  | isParallel (outOf n1) (outOf n2) = error $ "Cannot get the average of nodes if their outputs never intersect!\n" <> dumpInput
  | isCollinear (outAndErrOf n1) (outAndErrOf n2) = error $ "Cannot (yet) handle two input plines that are collinear.\n" <> dumpInput
  | nodesAreAntiCollinear n1 n2 = error $ "Cannot (yet) handle two input plines that are collinear.\n" <> dumpInput
  | n1Distance < getRounded n1Err = error $ "intersection is AT the point of n1!\n" <> dumpInput
  | n2Distance < getRounded n2Err = error $ "intersection is AT the point of n2!\n" <> dumpInput
  | otherwise                 = makeINode (sortedPair n1 n2) $ Just $ getOutsideArc (pPointOf n1) (outAndErrOf n1) (pPointOf n2) (outAndErrOf n2)
  where
    (n1Distance, (_,_, UlpSum n1Err)) = distance2PP (intersectionOf (outAndErrOf n1) (outAndErrOf n2)) (pPointOf n1, mempty)
    (n2Distance, (_,_, UlpSum n2Err)) = distance2PP (intersectionOf (outAndErrOf n1) (outAndErrOf n2)) (pPointOf n2, mempty)
    dumpInput =    "Node1: " <> show n1
                <> "\nNode2: " <> show n2
                <> "\nNode1Out: " <> show (outOf n1)
                <> "\nNode2Out: "<> show (outOf n2)
                <> "\nNode1PPoint: " <> show (pPointOf n1)
                <> "\nNode2PPoint: " <> show (pPointOf n2) <> "\n"

-- | Take a pair of arcables, and return their outOfs, in a sorted order.
sortedPair :: (Arcable a, Arcable b) => a -> b -> [(PLine2, PLine2Err)]
sortedPair n1 n2 = sortedPLinesWithErr [outAndErrOf n1, outAndErrOf n2]

-- | Make a first generation node.
makeENode :: Point2 -> Point2 -> Point2 -> ENode
makeENode p1 p2 p3 = ENode (p1,p2,p3) arc arcErr
  where
    (arc, arcErr) = getFirstArc p1 p2 p3

-- | Make a first generation set of nodes, AKA, a set of arcs that come from the points where line segments meet, toward the inside of the contour.
makeENodes :: [LineSeg] -> [ENode]
makeENodes segs = case segs of
                         [] -> error "got empty list.\n"
                         [a] -> error $ "not enough line segments: " <> show a <> "\n"
                         [a,b] -> [makeENode (startPoint a) (startPoint b) (endPoint b)]
                         (a:b:xs) -> [makeENode (startPoint a) (startPoint b) (endPoint b)] <> makeENodes (b:xs)

-- | Find the non-reflex virtexes of a contour, and create ENodes from them.
--   This function is meant to be used on an exterior contour.
eNodesOfOutsideContour :: Contour -> [ENode]
eNodesOfOutsideContour contour = mapMaybe onlyNodes $ zip (linePairs contour) (mapWithFollower concavePLines $ lineSegsOfContour contour)
  where
    onlyNodes :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe ENode
    onlyNodes ((seg1, seg2), Just _) = Just $ makeENode (startPoint seg1) (startPoint seg2) (endPoint seg2)
    onlyNodes ((_, _), Nothing) = Nothing

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

-- | A better anticollinear checker.
-- distance is used here to get a better anticollinear than PGA has, because we have a point, and floating point hurts us.
-- FIXME: shouldn't this be pulled into PGA.hs, as part of an outsIntersectIn?
nodesAreAntiCollinear :: (Pointable a, Arcable a, Pointable b, Arcable b) => a -> b -> Bool
nodesAreAntiCollinear node1 node2
  | hasArc node1 && hasArc node2 && isAntiCollinear (outOf node1) (outOf node2) = True
  | canPoint node1 && canPoint node2 && hasArc node1 && hasArc node2 = (distancePPointToPLine (pPointOf node1) (outOf node2) < fudgeFactor*50) && (distancePPointToPLine (pPointOf node2) (outOf node1) < fudgeFactor*50)
  | otherwise = False

-- | Walk the result tree, and find our enodes. Used to test the property that a walk of our result tree should result in the input ENodes in order.
findENodesInOrder :: ENodeSet -> [[INode]] -> [ENode]
findENodesInOrder (ENodeSet (Slist [] _)) _ = []
findENodesInOrder eNodeSet@(ENodeSet (Slist [(_,_)] _)) generations = findENodesRecursive generations
  where
    findENodesRecursive :: [[INode]] -> [ENode]
    findENodesRecursive myGens =
      case unsnoc myGens of
        Nothing -> []
        (Just (ancestorGens,workingGen)) -> concat $ findENodesOfInRecursive <$> nub (insOf (onlyINodeIn workingGen) <> maybePLineOut (onlyINodeIn workingGen))
          where
            maybePLineOut :: INode -> [(PLine2,PLine2Err)]
            maybePLineOut myINode = if hasArc myINode
                                    then [outAndErrOf myINode]
                                    else []
            -- for a generation with only one inode, retrieve that inode.
            onlyINodeIn :: [INode] -> INode
            onlyINodeIn [oneItem] = oneItem
            onlyINodeIn a = error $ "more than one inode: " <> show a <> "\n"
            findENodesOfInRecursive :: (PLine2,PLine2Err) -> [ENode]
            findENodesOfInRecursive myPLine
              | isENode (fst myPLine) = [myENode]
              | otherwise = -- must be an INode. recurse.
                case unsnoc ancestorGens of
                  Nothing -> []
                  (Just (newAncestors, newLastGen)) -> findENodesRecursive $ newAncestors <> lastGenWithOnlyMyINode
                    where
                      -- strip the new last generation until it only contains the INode matching myPLine.
                      lastGenWithOnlyMyINode :: [[INode]]
                      lastGenWithOnlyMyINode = case filter (\a -> outOf a == fst myPLine) newLastGen of
                                                 [] -> []
                                                 a -> [[first a]]
                                                   where
                                                     first :: [v] -> v
                                                     first [] = error "found no INode?"
                                                     first (v:_) = v
              where
                myENode = fromMaybe (error "could not find ENode?") $ findENodeByOutput eNodeSet (fst myPLine)
                -- Determine if a PLine matches the output of an ENode.
                isENode :: PLine2 -> Bool
                isENode myPLine2 = isJust $ findENodeByOutput eNodeSet myPLine2
findENodesInOrder a b = error $ "cannot find ENodes for :" <> show a <> "\n" <> show b <> "\n"

-- | Re-order an INodeSet such that the eNodes that the inodes point to are in the order of the given enode list.
-- also performs 'safe' node tree transforms.
sortINodesByENodes :: Bool -> Slist [LineSeg] -> INodeSet -> INodeSet
sortINodesByENodes loop inSegSets inGens@(INodeSet rawGenerations)
 | generationsIn res == 1 && inCountOf (onlyINodeOf $ INodeSet $ resSlist rawGenerations) > len (slist initialENodes) + (len inSegSets*2-2) = errorTooManyIns
 -- skip the next property test for hallways.
 -- FIXME: how do we perform this test for a hallway?
 | len inSegSets == 2 = INodeSet $ resSlist rawGenerations
 -- test for the property that a walk of the INodes we are returning results in our input ENode list.
 | len rawGenerations > 0 && initialENodes /= findENodesInOrder (eNodeSetOf $ slist initialENodes) res = errorInsWrongOrder
 | otherwise = INodeSet $ resSlist rawGenerations
  where
    initialENodes = makeInitialGeneration loop inSegSets
    res :: [[INode]]
    res = unSlist $ resSlist rawGenerations
      where
        unSlist (Slist a _) = a
    -- our first attempt at a recursive handler.
    resSlist :: Slist [INode] -> Slist [INode]
    resSlist generations
     | isEmpty generations = generations -- errorEmpty
     | len generations == 1 = -- nothing to do for a single INode.
         one [onlyINodeOf inGens]
     | len generations == 2 =
         case flippedINodeOf rawFirstGeneration of
           Nothing ->
             case rawFirstGeneration of
               [] -> -- Not possible?
                 errorEmpty
               [oneINode] -> -- check if we should perform a tail pruning.
                 if canPruneTail rawLastINode
                 then pruneTail oneINode rawLastINode
                 else if canFlipINodes oneINode rawLastINode
                      then flipINodePair oneINode rawLastINode
                      else one [orderInsByENodes oneINode] <> one [orderInsByENodes rawLastINode]
               v ->
                 one (indexTo $ sortGeneration v) <> one [iNodeWithFlips rawLastINode]
           (Just flippedINode) ->
             case genWithoutFlips rawFirstGeneration of
               [] -> -- after transform #1, there is no first generation left. just return the second, after the transform.
                 one [iNodeWithFlips rawLastINode]
               [oneINode] -> -- after transform #1, there is just one INode left.
                 if inCountOf rawLastINode == 2 && canMergeWith flippedINode rawLastINode
                 then one [orderInsByENodes oneINode] <> mergeWith flippedINode rawLastINode
                 else if canPruneTail rawLastINode
                      then pruneTail oneINode rawLastINode
                      else if canFlipINodes oneINode rawLastINode
                           then flipINodePair oneINode rawLastINode
                           else one [orderInsByENodes oneINode] <> one [orderInsByENodes rawLastINode]
               v ->
                 one (indexTo $ sortGeneration v) <> one [iNodeWithFlips rawLastINode]
     | len generations == 3 =
         case flippedINodeOf rawFirstGeneration of
           Nothing ->
             one (indexTo $ sortGeneration rawFirstGeneration) <> resSlist (slist $ [secondGen] <> [[rawLastINode]])
           (Just flippedINode) ->
             case genWithoutFlips rawFirstGeneration of
               [] -> -- without the flipped INode, there is no first generation left. Let's find something to do with the flipped inode.
                 case secondGen of
                   [] -> error "impossible!"
                   [secondINode] -> -- ok, the second generation is a single iNode. see if we can flip with it.
                     if canFlipINodes flippedINode secondINode
                     then resSlist $ flipINodePair flippedINode secondINode <> slist [[rawLastINode]]
                     else -- ok, we can't flip. maybe we can merge the flipped INode with the last INode?
                       if inCountOf rawLastINode == 2 && canMergeWith flippedINode rawLastINode
                       then resSlist $ slist [secondGen] <> mergeWith flippedINode rawLastINode
                       else -- .. alright, maybe try to merge the second and last INode?
                         if inCountOf rawLastINode == 2 && canMergeWith secondINode rawLastINode
                         then resSlist $ slist [[flippedINode]] <> mergeWith secondINode rawLastINode
                         else error "ran out of options"
                   x@(_:_) -> errorTooManyNodes x
               [oneINode] ->
                 error
                 $ show oneINode <> "\n"
                 <> show (one [orderInsByENodes oneINode] <> resSlist (slist $ [flippedINode:secondGen] <> [[rawLastINode]])) <> "\n"
                 <> show initialENodes <> "\n"
               x@(_:_) -> error $ "way too many nodes:" <> show x <> "\n"
      | otherwise = error "too many generations?"
      where
        -- the first generation, as given to us.
        rawFirstGeneration = fromMaybe (error "no first generation!") $ safeHead generations

        -- construct an INode including the inputs of the crossover node from the first generation merged, if it exists.
        iNodeWithFlips :: INode -> INode
        iNodeWithFlips = lastGen (sortGeneration rawFirstGeneration)
          where
            lastGen :: [INode] -> INode -> INode
            lastGen firstGen oneINode = orderInsByENodes $ case flippedINodeOf firstGen of
                                                             Nothing -> oneINode
                                                             (Just flippedINode) -> addINodeToParent flippedINode oneINode

        -- The last INode, as given to us in the recursive loop.
        rawLastINode :: INode
        rawLastINode
          | hasArc result && loop = errorIllegalLast
          | otherwise = result
          where
            result = lastINodeOf (INodeSet generations)

        secondGen = fromMaybe (error $ "no second generation:\n" <> show generations) $ safeHead $ SL.tail generations

    -- transform #1: if a first generation inode connects the last and the first ENodes, remove it, and point to those enodes with the last INode.
    -- Place the first generation in ENode order, and remove a 'flipped' node if it exists.
    -- NOTE: If one of the nodes is constructed from the first and last ENodes, we filter it out. it will be merged into the last generation.
    genWithoutFlips myGeneration = indexTo $ sortGeneration myGeneration

    -- Force a list of INodes to start with the INode closest to the firstPLine, but not before the firstPLine.
    indexTo :: [INode] -> [INode]
    indexTo iNodes = iNodesBeforePLine iNodes <> iNodesAfterPLine iNodes
      where
        iNodesBeforePLine :: [INode] -> [INode]
        iNodesBeforePLine = filter (\a -> (firstPLine, mempty) `pLineIsLeft` (firstInOf a, mempty) /= Just False)
        -- nodes in the right order, after the divide.
        iNodesAfterPLine myINodes = withoutFlippedINodes $ filter (\a -> (firstPLine, mempty) `pLineIsLeft` (firstInOf a, mempty) == Just False) myINodes
        withoutFlippedINodes maybeFlippedINodes = case flippedINodeOf maybeFlippedINodes of
                                                    Nothing -> maybeFlippedINodes
                                                    (Just a) -> filter (/= a) maybeFlippedINodes

    errorEmpty = error $ "empty INodeSet for nodes:\n" <> show initialENodes <> "\nloop: " <> show loop <> "\n"

    errorTooManyNodes nodes = error
                              $ "don't know how to handle a case with these nodes:\n"
                              <> show nodes <> "\n"
                              <> "rawGenerations:      " <> show rawGenerations <> "\n"
                              <> "initialENodes:       " <> show initialENodes <> "\n"
                              <> "flippedINode:        " <> show (flippedINodeOf $ SL.head rawGenerations) <> "\n"
                              <> "loop:                " <> show loop <> "\n"

    errorTooManyIns = error $ "generating a single INode with more inputs than possible: " <> show res <> "\n"
                           <> "rawGenerations: " <> show rawGenerations <> "\n"
                           <> "ENodes:         " <> show initialENodes <> "\n"
                           <> "inSegSets:      " <> show inSegSets <> "\n"
                           <> "loop:           " <> show loop <> "\n"

    errorInsWrongOrder = error
                         $ "ENodes outs should be:" <> show (outOf <$> initialENodes) <> "\n"
                         <> "ENode outs are:       " <> show (outOf <$> findENodesInOrder (eNodeSetOf $ slist initialENodes) res) <> "\n"
                         <> "rawGenerations:       " <> show rawGenerations <> "\n"
                         <> "returned inodes:      " <> show res <> "\n"
                         <> "flippedINode:         " <> show (flippedINodeOf $ SL.head rawGenerations) <> "\n"

    errorIllegalLast = error
                       $ "illegal last generation:\n"
                       <> "rawGenerations:    " <> show rawGenerations <> "\n"
                       <> "initialENodes:     " <> show initialENodes <> "\n"
                       <> "inSegSets:         " <> show inSegSets <> "\n"
                       <> "loop:              " <> show loop <> "\n"
                       <> "loop:              " <> show loop <> "\n"

    -- if the object is closed, and the last generation consists of an INode that points to just one ENode and one INode, merge the last generation into the prior generation.
    -- assuming that really, this should have been just another in to the previous generation.
    canPruneTail :: INode -> Bool
    canPruneTail lastGen = loop && hasENode lastGen && hasINode lastGen && inCountOf lastGen == 2

    -- actually do a tail pruning. add the two INodes together.
    pruneTail :: INode -> INode -> Slist [INode]
    pruneTail iNode1 iNode2 = one [addINodeToParent iNode1 iNode2]

    -- check to see if an INode can be merged with another INode.
    canMergeWith :: INode -> INode -> Bool
    canMergeWith inode1@(INode _ _ _ maybeOut) inode2 = isJust maybeOut && hasIn inode2 (outOf inode1)
      where
        hasIn :: INode -> PLine2 -> Bool
        hasIn iNode pLine2 = case filter (\a -> fst a == pLine2) $ insOf iNode of
                               [] -> False
                               [_] -> True
                               (_:_) -> error "filter passed too many options."
    -- Merge two INodes.
    mergeWith :: INode -> INode -> Slist [INode]
    mergeWith iNode1 iNode2 = one [addINodeToParent iNode1 iNode2]

    -- in situations where the first generation contains pointers to the first and last ENodes and the object is closed, we may be able to move the node up the tree of INodes.
    -- The idea is that these should get to the last generation.
    canFlipINodes :: INode -> INode -> Bool
    canFlipINodes firstGen secondGen = loop && inCountOf secondGen > 2 && isJust (flippedINodeOf [firstGen])

    -- Transform the two INodes, and return them in the same generation.
    flipINodePair :: INode -> INode -> Slist [INode]
    flipINodePair iNode1 iNode2@(INode _ _ _ maybeOut2) = one [orderInsByENodes newINode1] <> one [orderInsByENodes newINode2]
      where
        -- like iNode2, only with our flipped connecting line.
        newINode1 = makeINode (withoutConnectingPLine $ insOf iNode2) (Just newConnectingPLine)
        -- like iNode1, but with our flipped connecting line in, and iNode2's original out.
        newINode2 = makeINode ([newConnectingPLine] <> insOf iNode1) maybeOut2
        newConnectingPLine = (flipL $ fst oldConnectingPLine, snd oldConnectingPLine)
        oldConnectingPLine = case iNodeInsOf iNode2 of
                               [] -> error "could not find old connecting PLine."
                               [v] -> v
                               (_:_) -> error "filter passed too many connecting PLines."
        iNodeInsOf myINode = filter (\a -> isNothing . findENodeByOutput (eNodeSetOf $ slist initialENodes) $ fst a) $  insOf myINode
        withoutConnectingPLine = filter (\a -> a /= oldConnectingPLine)

    -- Determine if the given INode has a PLine that points to an ENode.
    hasENode iNode = any (isJust . findENodeByOutput (eNodeSetOf $ slist initialENodes)) $ fst <$> insOf iNode
    -- Determine if the given INode has a PLine that points to another INode.
    hasINode iNode = any (isNothing . findENodeByOutput (eNodeSetOf $ slist initialENodes)) $ fst <$> insOf iNode

    -- Construct an ENodeSet
    eNodeSetOf :: Slist ENode -> ENodeSet
    eNodeSetOf eNodes
      | null eNodes = error "cannot construct an empty ENodeSet"
      | otherwise = ENodeSet (slist [(SL.head eNodes, SL.tail eNodes)])

    -- the number of generations.
    generationsIn :: [[INode]] -> Int
    generationsIn = len . slist

    -- how many input PLines does an INode have.
    inCountOf (INode in1 in2 moreIns _)
      -- handle two identical inputs.
      | in1 == in2 = 1+len moreIns
      | otherwise = 2+len moreIns

    -- the only inode of an INodeSet. must have one generation only.
    onlyINodeOf :: INodeSet -> INode
    onlyINodeOf (INodeSet (Slist [[a]] _))
      | hasArc a && loop = errorIllegalLast
      | otherwise = a
    onlyINodeOf a = error
                    $ "not only inode!\n"
                    <> show a <> "\n"
                    <> show initialENodes <> "\n"
                    <> show rawGenerations <> "\n"

    -- | Sort a generation by the first in PLine.
    sortGeneration :: [INode] -> [INode]
    sortGeneration = sortBy (\a b -> if (firstInOf a, mempty) `pLineIsLeft` (firstInOf b, mempty) == Just False then LT else GT)

    -- Find an inode connecting the first and last ENode, if it exists.
    -- FIXME: this functions, but i don't know why. :)
    flippedINodeOf :: [INode] -> Maybe INode
    flippedINodeOf inodes = case filter (\a -> (firstPLine, mempty) `pLineIsLeft` (firstInOf a, mempty) == Just False) inodes of
                              [] -> Nothing
                              [a] -> -- if there is only one result, it's going to only point to enodes.
                                Just a
                              xs -> -- if there is more than one result, then one of the descendants of the right answer is caught in the filter.
                                case filter allInsAreENodes xs of
                                  [] -> Nothing
                                  [a] -> Just a
                                  vs -> error
                                        $ "more than one flipped inode?" <> show vs <> "\n"
                                        <> show initialENodes <> "\n"
                                where
                                  allInsAreENodes iNode = not $ hasINode iNode

    -- the output PLine of the first ENode in the input ENode set.
    firstPLine = outOf firstENode
      where
        -- the first ENode given to us. for sorting uses.
        firstENode = first initialENodes
          where
            first :: [a] -> a
            first [] = error "no first enode?"
            first (a:_) = a

    -- | add together a child and it's parent.
    addINodeToParent :: INode -> INode -> INode
    addINodeToParent (INode _ _ _ Nothing) _ = error "cannot merge a child inode with no output!"
    addINodeToParent iNode1@(INode _ _ _ (Just out1)) iNode2@(INode _ _ _ out2) = orderInsByENodes $ makeINode (insOf iNode1 <> withoutPLine (fst out1) (insOf iNode2)) out2
      where
        withoutPLine :: PLine2 -> [(PLine2,PLine2Err)] -> [(PLine2,PLine2Err)]
        withoutPLine myPLine = filter (\a -> fst a /= myPLine)

    -- Order the input nodes of an INode.
    orderInsByENodes :: INode -> INode
    orderInsByENodes inode@(INode _ _ _ out) = makeINode (indexPLinesTo firstPLine $ sortedPLinesWithErr $ indexPLinesTo firstPLine $ insOf inode) out

-- | Apply a recursive algorithm to obtain a raw INode set.
--   FIXME: does not handle more than two point intersections of arcs properly.
skeletonOfNodes :: Bool -> Slist [LineSeg] -> [INode] -> Either PartialNodes INodeSet
skeletonOfNodes connectedLoop inSegSets iNodes =
  case eNodes of
    [] -> case iNodes of
            [] -> -- zero nodes == return emptyset. allows us to simplify our return loop.
              Right $ INodeSet $ slist []
            [INode {}] ->
              if contourLooped
              then errorLen1 -- A one node loop makes no sense, reject.
              else Right $ INodeSet $ one iNodes -- just hand back single node requests.
            [iNode1,iNode2] -> handleTwoNodes iNode1 iNode2
            (_:_) -> handleThreeOrMoreNodes
    [eNode] -> case iNodes of
                 [] ->
                   if contourLooped
                   then errorLen1 -- A one node loop makes no sense, reject.
                   else
                     -- Construct an INode with two identical inputs, and return it.
                     -- FIXME: shouldn't we be able to return an empty set, instead?
                     Right $ INodeSet $ one [INode (outAndErrOf eNode) (outAndErrOf eNode) (slist []) Nothing]
                 [iNode] -> handleTwoNodes eNode iNode
                 (_:_) -> handleThreeOrMoreNodes
    [eNode1,eNode2] -> case iNodes of
                         [] -> handleTwoNodes eNode1 eNode2
                         (_:_) -> handleThreeOrMoreNodes
    (_:_:_:_) -> handleThreeOrMoreNodes
  where
    -- Did this contour start out as a loop?
    contourLooped = isLoop inSegSets
    eNodes = makeInitialGeneration connectedLoop inSegSets
    errorLen1 = Left $ PartialNodes (INodeSet $ one iNodes) ("NOMATCH - length 1?\n" <> show eNodes <> "\n" <> show iNodes <> "\n" <> show inSegSets <> "\n")
    --   Handle the the case of two nodes.
    handleTwoNodes :: (Arcable a, Pointable a, Arcable b, Pointable b) => a -> b -> Either PartialNodes INodeSet
    handleTwoNodes node1 node2
      | isCollinear (outAndErrOf node1) (outAndErrOf node2) = Left $ PartialNodes (INodeSet $ one iNodes) $ "cannot handle collinear nodes:\n" <> show node1 <> "\n" <> show node2 <> "\n"
      | nodesAreAntiCollinear node1 node2 && contourLooped = Right $ INodeSet $ one [makeLastPair node1 node2]
      | contourLooped = -- this is a complete loop, so this last INode will be re-written in sortINodesByENodes anyways.
        Right $ INodeSet $ one [makeINode (sortedPLinesWithErr [outAndErrOf node1,outAndErrOf node2]) Nothing]
      | intersectsInPoint node1 node2 = Right $ INodeSet $ one [averageNodes node1 node2]
      | otherwise = errorLen2
      where
        errorLen2 = Left $ PartialNodes (INodeSet $ one iNodes) $ "NOMATCH - length 2?\n" <> show node1 <> "\n" <> show node2 <> "\n" <> show contourLooped <> "\n" <> show eNodes <> "\n" <> show iNodes <> "\n"

    --   Handle the the case of 3 or more nodes.
    handleThreeOrMoreNodes
      | endsAtSamePoint && contourLooped = Right $ INodeSet $ one [makeINode (sortedPLinesWithErr $ (outAndErrOf <$> eNodes) <> (outAndErrOf <$> iNodes)) Nothing]
      -- FIXME: this can happen for non-loops. which means this Nothing is wrong. it should be the result of the intersection tree from the first and last node in the segment.
      | endsAtSamePoint && not contourLooped = error $ show $ INodeSet $ one [makeINode (sortedPLinesWithErr $ (outAndErrOf <$> eNodes) <> (outAndErrOf <$> iNodes)) Nothing]
      | hasShortestNeighboringPair = Right $ INodeSet $ averageOfShortestPairs `cons` inodesOf (errorIfLeft (skeletonOfNodes remainingLoop remainingLineSegs (remainingINodes <> averageOfShortestPairs)))
      | otherwise = errorLen3
      where
        inodesOf (INodeSet set) = set
    errorLen3 = error
                $ "shortestPairDistance: " <> show shortestPairDistance <> "\n"
                <> "ePairDistance: " <> show shortestEPairDistance <> "\n"
                <> "shortestEPairs: " <> show (shortestPairs eNodes) <> "\n"
                <> "ePairResults: " <> show (uncurry averageNodes <$> shortestPairs eNodes) <> "\n"
                <> show (isSomething shortestEPairDistance) <> "\n"
                <> "iPairDistance: " <> show shortestIPairDistance <> "\n"
                <> "shortestIPairs: " <> show (shortestPairs iNodes) <> "\n"
                <> "iPairResults: " <> show (uncurry averageNodes <$> shortestPairs iNodes) <> "\n"
                <> show (isSomething shortestIPairDistance) <> "\n"
                <> "mixedPairDistance: " <> show shortestMixedPairDistance <> "\n"
                <> "shortestMixedPairs: " <> show shortestMixedPairs <> "\n"
                <> "MixedPairResults: " <> show (uncurry averageNodes <$> shortestMixedPairs) <> "\n"
                <> show (isSomething shortestMixedPairDistance) <> "\n"
                <> show (shortestEPairDistance == shortestPairDistance) <> "\n"
                <> "remainingLineSegs: " <> show remainingLineSegs <> "\n"
                <> "remainingINodes: " <> show remainingINodes <> "\n"
                <> "thisGen: " <> show averageOfShortestPairs <> "\n"

    -- | When all of our nodes end in the same point we should create a single Node with all of them as input. This checks for that case.
    endsAtSamePoint :: Bool
    endsAtSamePoint
      | and $ isJust <$> intersections = and $ pointsCloseEnough <> linesCloseEnough
      | otherwise = False
      where
        intersections = mapWithFollower intersectionBetween ((outOf <$> nonAntiCollinearNodes eNodes (antiCollinearNodePairsOf eNodes) <> firstAntiCollinearNodes (antiCollinearNodePairsOf eNodes)) <>
                                                             (outOf <$> nonAntiCollinearNodes iNodes (antiCollinearNodePairsOf iNodes) <> firstAntiCollinearNodes (antiCollinearNodePairsOf iNodes)))
        pointIntersections = rights $ catMaybes intersections
        lineIntersections = lefts $ catMaybes intersections
        pointsCloseEnough = mapWithFollower pairCloseEnough pointIntersections
          where
            pairCloseEnough a b = res < realToFrac errRes
              where
                (res, (_,_,UlpSum errRes)) = distance2PP a b
        linesCloseEnough =
          case lineIntersections of
            [] -> []
            [a] -> case pointIntersections of
                     [] -> error "one line, no points.. makes no sense."
                     (x:_) -> [and pointsCloseEnough && foundDistance < realToFrac foundErr]
                       where
                         (foundDistance, (_,_,_,_,_,UlpSum foundErr)) = distancePPointToPLineWithErr ((\(CPPoint2 v,_) -> PPoint2 v) x, mempty) (a, mempty)
            (_:_) -> error
                     $ "detected multiple lines?\n"
                     <> show lineIntersections <> "\n"
                     <> show pointIntersections <> "\n"
        -- since anti-collinear nodes end at the same point, only count one of them.
        firstAntiCollinearNodes nodePairs = fst <$> nodePairs
        -- find nodes that do not have an anti-collinear pair.
        -- FIXME: is there a better way do do this with Ord?
        nonAntiCollinearNodes :: (Eq a) => [a] -> [(a,a)] -> [a]
        nonAntiCollinearNodes myNodes nodePairs = filter (`notElem` allAntiCollinearNodes nodePairs) myNodes
          where
            allAntiCollinearNodes myNodePairs = (fst <$> myNodePairs) <> (snd <$> myNodePairs)

    -- | make sure we have a potential intersection between two nodes to work with.
    hasShortestNeighboringPair :: Bool
    hasShortestNeighboringPair = not $ null (intersectingNeighboringNodePairsOf $ mapWithFollower (,) eNodes) && null (intersectingNodePairsOf iNodes) && null intersectingMixedNodePairs

    -- | construct the last pair of a closed concave region.
    makeLastPair :: (Arcable a, Arcable b) => a -> b -> INode
    makeLastPair node1 node2 = makeINode (sortedPair node1 node2) Nothing

    -- | which ENodes contributed to this generation.
    foundENodes = (fst <$> mixedPairsFound) <> (fst <$> ePairsFound) <> (snd <$> ePairsFound)

    -- | is the looping ENode still present?
    remainingLoop = connectedLoop && not (any (\a -> a == loopOfSegSets inSegSets) foundENodes)

    -- | determine the line segments available for drawing ENodes from during the next recurse.
    remainingLineSegs :: Slist [LineSeg]
    remainingLineSegs
      | null foundENodes = inSegSets
      | otherwise = case inSegSets of
                      (Slist [] _) -> error "cannot remove segment from empty set."
                      (Slist segLists _) -> slist $ concat $ removeENodesFromSegList (slist foundENodes) <$> segLists
      where
        removeENodesFromSegList :: Slist ENode -> [LineSeg] -> [[LineSeg]]
        removeENodesFromSegList eNodesIn lineSegs =
          filterTooShorts $ case eNodesIn of
                              (Slist [] _) -> [lineSegs]
                              (Slist (oneENode:moreENodes) _) -> concat $ removeENodeFromSegList oneENode <$> removeENodesFromSegList (slist moreENodes) lineSegs
          where
            filterTooShorts sets = mapMaybe isTooShort sets
            isTooShort set = case set of
                               [] -> Nothing
                               [val] -> Just [val]
                               vals -> Just vals
            removeENodeFromSegList :: ENode -> [LineSeg] -> [[LineSeg]]
            removeENodeFromSegList eNode rawInSegs =
              case rawInSegs of
                [] -> []
                [a] -> [[a]]
                (_:_) -> if len (slist removeENode) > len (slist rawInSegs)
                         then error "missed!"
                         else removeENode
                  where
                    -- Does not really remove anything from the input segments, but breaks the lists right where the ENode used to be generated, preventing it from being generated in the next round.
                    removeENode
                     | getFirstLineSeg eNode == PL.head rawInSegs = [[PL.head rawInSegs]] <> [PL.tail rawInSegs]
                     | getLastLineSeg eNode == PL.last rawInSegs = [PL.init rawInSegs] <> [[PL.last rawInSegs]]
                     | otherwise = [takeWhile (\a -> getLastLineSeg eNode /= a) rawInSegs] <>
                                   [dropWhile (\a -> getLastLineSeg eNode /= a) rawInSegs]

    -- | determine the interior nodes available for calculation during the next recurse.
    remainingINodes :: [INode]
    remainingINodes =  filter (`notElem` (snd <$> mixedPairsFound) <> (fst <$> iPairsFound) <> (snd <$> iPairsFound)) iNodes

    -- | collect our set of result nodes.
    -- Note: we're putting these in the order "most likely to contain INodes" to least likely. to make the optimizer easier to write.
    averageOfShortestPairs :: [INode]
    averageOfShortestPairs = (uncurry averageNodes <$> iPairsFound) <> (uncurry averageNodes <$> mixedPairsFound) <> (uncurry averageNodes <$> ePairsFound)

    iPairsFound =
      if isSomething shortestIPairDistance && shortestIPairDistance == shortestPairDistance
      then shortestPairs iNodes
      else []

    mixedPairsFound =
      if isSomething shortestMixedPairDistance && shortestMixedPairDistance == shortestPairDistance
      then filterINodesOf shortestMixedPairs
      else []
      where
        -- | remove pairs containing INodes from the shortest INode pairs from a list of mixed pairs.
        filterINodesOf :: [(ENode, INode)] -> [(ENode, INode)]
        filterINodesOf = filter (\(_,myINode) -> myINode `notElem` ((fst <$> iPairsFound) <> (snd <$> iPairsFound)))

    ePairsFound =
      if isSomething shortestEPairDistance && shortestEPairDistance == shortestPairDistance
      then filterENodesOf $ shortestNeighboringPairs $ mapWithFollower (,) eNodes
      else []
      where
        -- | remove pairs containing ENodes from the shortest mixed pairs from a list of ENode pairs
        filterENodesOf :: [(ENode, ENode)] -> [(ENode, ENode)]
        filterENodesOf = filter (\(myENode1,myENode2) -> myENode1 `notElem` (fst <$> mixedPairsFound) && myENode2 `notElem` (fst <$> mixedPairsFound))

    -- | calculate the distances to the shortest pairs of nodes. the shortest pair, along with all of the pairs of the same length, will be in our result set.
    shortestPairDistance = min (min shortestEPairDistance shortestMixedPairDistance) (min shortestIPairDistance shortestMixedPairDistance)
    shortestIPairDistance = case shortestPairs iNodes of
                              [] -> Empty
                              (firstPair:_) -> justToSomething $ uncurry distanceToIntersection firstPair
    shortestEPairDistance = case shortestNeighboringPairs $ mapWithFollower (,) eNodes of
                              [] -> Empty
                              (firstPair:_) -> justToSomething $ uncurry distanceToIntersection firstPair
    shortestMixedPairDistance = case shortestMixedPairs of
                                  [] -> Empty
                                  (firstPair:_) -> justToSomething $ uncurry distanceToIntersection firstPair

    -- | get the list of sorted pairs of intersecting nodes.
    shortestPairs :: (Arcable a, Pointable a, Eq a) => [a] -> [(a, a)]
    shortestPairs myNodes = case nodePairsSortedByDistance myNodes of
                              [] -> []
                              [onePair] -> [onePair]
                              (pair:morePairs) -> filterCommonIns $ pair : takeWhile (\a -> uncurry distanceToIntersection a == uncurry distanceToIntersection pair) morePairs
      where
        -- | get the intersection of each node pair, sorted based on which one has the shortest maximum distance of the two line segments from it's ancestor nodes to the intersection point.
        nodePairsSortedByDistance :: (Arcable a, Pointable a) => [a] -> [(a, a)]
        nodePairsSortedByDistance myNodes' = sortBy (\(p1n1, p1n2) (p2n1, p2n2) -> distanceToIntersection p1n1 p1n2 `compare` distanceToIntersection p2n1 p2n2) $ intersectingNodePairsOf myNodes'
        filterCommonIns :: Eq a => [(a, a)] -> [(a, a)]
        filterCommonIns pairs = case pairs of
                                  [] -> []
                                  [a] -> [a]
                                  (x@(node1, node2) :xs) -> x : filterCommonIns (filter (\(myNode1, myNode2) -> node1 /= myNode1 && node2 /= myNode1 && node1 /= myNode2 && node2 /= myNode2) xs)

    -- | get the list of sorted pairs of intersecting nodes.
    shortestNeighboringPairs :: (Arcable a, Pointable a, Eq a) => [(a,a)] -> [(a, a)]
    shortestNeighboringPairs myNodePairs = case nodePairsSortedByDistance myNodePairs of
                                         [] -> []
                                         [onePair] -> [onePair]
                                         (pair:morePairs) -> filterCommonIns $ pair : takeWhile (\a -> uncurry distanceToIntersection a == uncurry distanceToIntersection pair) morePairs
      where
        -- | get the intersection of each node pair, sorted based on which one has the shortest maximum distance of the two line segments from it's ancestor nodes to the intersection point.
        nodePairsSortedByDistance :: (Arcable a, Pointable a) => [(a,a)] -> [(a, a)]
        nodePairsSortedByDistance myNodesPairs' = sortBy (\(p1n1, p1n2) (p2n1, p2n2) -> distanceToIntersection p1n1 p1n2 `compare` distanceToIntersection p2n1 p2n2) $ intersectingNeighboringNodePairsOf myNodesPairs'
        filterCommonIns :: Eq a => [(a, a)] -> [(a, a)]
        filterCommonIns pairs = case pairs of
                                  [] -> []
                                  [a] -> [a]
                                  (x@(node1, node2) :xs) -> x : filterCommonIns (filter (\(myNode1, myNode2) -> node1 /= myNode1 && node2 /= myNode1 && node1 /= myNode2 && node2 /= myNode2) xs)

    -- | get the pairs of intersecting nodes of differing types that we might be putting into this generation.
    shortestMixedPairs :: [(ENode, INode)]
    shortestMixedPairs = case nodePairsSortedByDistance of
                           [] -> []
                           [onePair] -> [onePair]
                           (pair:morePairs) -> filterCommonIns $ pair : takeWhile (\a -> uncurry distanceToIntersection a == uncurry distanceToIntersection pair) morePairs
      where
        -- | get the intersection of each node pair, sorted based on which one has the shortest maximum distance of the two line segments from it's ancestor nodes to the intersection point.
        nodePairsSortedByDistance :: [(ENode, INode)]
        nodePairsSortedByDistance = sortBy (\(p1n1, p1n2) (p2n1, p2n2) -> distanceToIntersection p1n1 p1n2 `compare` distanceToIntersection p2n1 p2n2) intersectingMixedNodePairs
        filterCommonIns :: [(ENode, INode)] -> [(ENode, INode)]
        filterCommonIns pairs = case pairs of
                                  [] -> []
                                  [a] -> [a]
                                  (x@(eNode, iNode) :xs) -> x : filterCommonIns (filter (\(myENode, myINode) -> eNode /= myENode && iNode /= myINode) xs)

    -- | find nodes of two different types that can intersect.
    intersectingMixedNodePairs :: [(ENode, INode)]
    intersectingMixedNodePairs = mapMaybe (\(node1, node2) -> if intersectsInPoint node1 node2 then Just (node1, node2) else Nothing) $ getMixedPairs eNodes iNodes
      where
        getMixedPairs ::  [a] -> [b] -> [(a, b)]
        getMixedPairs set1 set2 = concat $ (\a -> (a,) <$> set2) <$> set1

    -- | find nodes of the same type that can intersect.
    intersectingNodePairsOf :: (Arcable a, Pointable a) => [a] -> [(a, a)]
    intersectingNodePairsOf inNodes = mapMaybe (\(node1, node2) -> if intersectsInPoint node1 node2 then Just (node1, node2) else Nothing) $ getPairs inNodes

    -- | find nodes of the same type that can intersect.
    -- NOTE: accepts node pairs, so that we can ensure we check just following ENodes.
    intersectingNeighboringNodePairsOf :: (Arcable a, Pointable a) => [(a,a)] -> [(a, a)]
    intersectingNeighboringNodePairsOf inNodePairs = mapMaybe (\(node1, node2) -> if intersectsInPoint node1 node2 then Just (node1, node2) else Nothing) $ inNodePairs

    -- | find nodes that have output segments that are antiCollinear with one another.
    antiCollinearNodePairsOf :: (Pointable a, Arcable a) => [a] -> [(a, a)]
    antiCollinearNodePairsOf inNodes = mapMaybe (\(node1, node2) -> if nodesAreAntiCollinear node1 node2 then Just (node1, node2) else Nothing) $ getPairs inNodes

    -- | for a given pair of nodes, find the longest distance between one of the two nodes and the intersection of the two output plines.
    distanceToIntersection :: (Pointable a, Arcable a, Pointable b, Arcable b) => a -> b -> Maybe ℝ
    distanceToIntersection node1 node2
      | canPoint node1
        && canPoint node2
        && intersectsInPoint node1 node2 =
        Just $ fst (distance2PP (pPointOf node1, mempty) (intersectionOf (outAndErrOf node1) (outAndErrOf node2)))
               `max`
               fst (distance2PP (pPointOf node2, mempty) (intersectionOf (outAndErrOf node1) (outAndErrOf node2)))
      | otherwise = Nothing
    -- | Check if the intersection of two nodes results in a point or not.
    intersectsInPoint :: (Arcable a, Pointable a, Arcable b, Pointable b) => a -> b -> Bool
    intersectsInPoint node1 node2
      | hasArc node1 && hasArc node2 = not (noIntersection (outAndErrOf node1) (outAndErrOf node2))
                                       && (dist1 >= realToFrac dist1Err)
                                       && (dist2 >= realToFrac dist2Err)
      | otherwise                    = error $ "cannot intersect a node with no output:\nNode1: " <> show node1 <> "\nNode2: " <> show node2 <> "\nnodes: " <> show iNodes <> "\n"
      where
        (dist1, (_,_,UlpSum dist1Err)) = distance2PP (pPointOf node1, mempty) (intersectionOf (outAndErrOf node1) (outAndErrOf node2)) 
        (dist2, (_,_,UlpSum dist2Err)) = distance2PP (pPointOf node2, mempty) (intersectionOf (outAndErrOf node1) (outAndErrOf node2)) 
