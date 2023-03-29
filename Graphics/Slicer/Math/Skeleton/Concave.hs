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

module Graphics.Slicer.Math.Skeleton.Concave (averageNodes, skeletonOfConcaveRegion, skeletonOfNodes, findINodes, makeENode, makeENodes, eNodesOfOutsideContour) where

import Prelude (Eq, Show, Bool(True, False), Either(Left, Right), String, Ord, Ordering(GT,LT), all, concatMap, notElem, otherwise, ($), (>), (<=), (<$>), (==), (/=), error, (&&), fst, (<>), show, not, max, compare, uncurry, null, (||), min, snd, filter, zip, any, (*), (+), Int, (.), (-), mempty)

import qualified Prelude as PL (head, last, tail, init)

import Data.Maybe( Maybe(Just,Nothing), fromJust, isJust, isNothing, fromMaybe, mapMaybe)

import Data.List (takeWhile, dropWhile, sortBy, nub)

import Data.List.Extra (unsnoc)

import Slist.Type (Slist(Slist))

import Slist (slist, one, len, isEmpty, safeHead)

import qualified Slist as SL (head, init, last, tail)

import Graphics.Implicit.Definitions (ℝ)

import Graphics.Slicer.Math.Arcs (getFirstArc, getInsideArc, getOutsideArc)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2, endPoint, lineSegsOfContour, mapWithFollower, startPoint)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.Intersections (noIntersection, intersectionBetweenArcsOf, intersectionsAtSamePoint, intersectionOf, isCollinear, isParallel, isAntiCollinear, isAntiParallel)

import Graphics.Slicer.Math.Lossy (distanceBetweenPPointsWithErr)

import Graphics.Slicer.Math.PGA (Arcable(errOfOut, hasArc, outOf), Pointable(canPoint), ProjectiveLine, PLine2Err, cPPointAndErrOf, eToPL, flipL, distance2PP, outAndErrOf, pLineIsLeft)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), ENodeSet(ENodeSet), INode(INode), INodeSet(INodeSet), NodeTree(NodeTree), concavePLines, getFirstLineSeg, getLastLineSeg, finalOutOf, firstInOf, getPairs, indexPLinesTo, insOf, finalINodeOf, linePairs, makeINode, makeSide, sortedPLines, isLoop)

import Graphics.Slicer.Math.Skeleton.NodeTrees (makeNodeTree, findENodeByOutput)

-- | Error type.
data PartialNodes = PartialNodes ![INode] !String
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
    result = makeNodeTree initialENodes $ Just $ sortINodesByENodes (isLoop inSegSets) initialENodes inSegSets $ findINodes inSegSets
    initialENodes = makeInitialGeneration (isLoop inSegSets) inSegSets

-- | Find a raw set of INodes representing the INodes of the solved NodeTree for this part of a contour.
findINodes :: Slist [LineSeg] -> INodeSet
findINodes inSegSets
  | len inSegSets == 1 =
      -- One continuous wall without gaps. may gap between the beginning and end of the contour, if this is not a loop.
      -- Just return the output of skeletonOfNodes.
      errorIfLeft $ skeletonOfNodes (isLoop inSegSets) inSegSets inSegSets []
  | len inSegSets == 2 =
    -- Two walls, no closed ends. solve the ends of a hallway region, so we can then hand off the solutioning to our regular process.
    case initialENodes of
      [] -> INodeSet mempty $ makeINode [getInsideArc firstLineFlipped lastLine, getInsideArc firstLine lastLineFlipped] Nothing
        where
          firstLine@(fs, fsErr) = eToPL $ SL.head $ slist $ SL.head inSegSets
          firstLineFlipped = (flipL fs, fsErr)
          lastLine@(ls, lsErr) = eToPL $ SL.head $ slist $ SL.last inSegSets
          lastLineFlipped = (flipL ls, lsErr)
      [a] -> INodeSet mempty $ makeINode [getInsideArc lastLine shortSide, getInsideArc firstLine shortSide] (Just (flipL $ outOf a, errOfOut a))
        where
          firstLine = eToPL $ fromMaybe (error "no first segment?") $ safeHead $ slist longSide
          lastLine = eToPL $ SL.last $ slist longSide
          (shortSide, longSide)
            | null (SL.head inSegSets) = (eToPL $ SL.head $ slist $ SL.last inSegSets, SL.head inSegSets)
            | otherwise = (eToPL $ SL.head $ slist $ SL.head inSegSets, SL.last inSegSets)
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
isHallway :: NodeTree -> Bool
isHallway (NodeTree _ iNodeSet) = isJust iNodeSet && hasOneMember (fromJust iNodeSet)
  where
    hasOneMember (INodeSet children _) = isEmpty children

-- | Create the set of ENodes for a set of segments
makeInitialGeneration :: Bool -> Slist [LineSeg] -> [ENode]
makeInitialGeneration gensAreLoop inSegSets = concatMap firstENodes inSegSets <> maybeLoop
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

-- | For a given pair of nodes, construct a new internal node, where it's parents are the given nodes, and the line leaving it is along the the obtuse bisector.
--   Note: this should be hidden in skeletonOfConcaveRegion, but it's exposed here, for testing.
averageNodes :: (Arcable a, Pointable a, Arcable b, Pointable b) => a -> b -> INode
averageNodes n1 n2 = makeINode (sortedPair n1 n2) $ Just $ getOutsideArc (cPPointAndErrOf n1) (outAndErrOf n1) (cPPointAndErrOf n2) (outAndErrOf n2)

-- | Take a pair of arcables, and return their outOfs, in a sorted order.
sortedPair :: (Arcable a, Arcable b) => a -> b -> [(ProjectiveLine, PLine2Err)]
sortedPair n1 n2
  | hasArc n1 && hasArc n2 = sortedPLines [outAndErrOf n1, outAndErrOf n2]
  | otherwise = error "Cannot get the average of nodes if one of the nodes does not have an out!\n"

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
    onlyNodes :: ((LineSeg, LineSeg), Maybe ProjectiveLine) -> Maybe ENode
    onlyNodes ((seg1, seg2), Just _) = Just $ makeENode (startPoint seg1) (startPoint seg2) (endPoint seg2)
    onlyNodes ((_, _), Nothing) = Nothing

-- | Find the reflex virtexes of a contour, and draw Nodes from them.
--   This function is for use on interior contours.
{-
convexNodes :: Contour -> [Node]
convexNodes contour = catMaybes $ onlyNodes <$> zip (linePairs contour) (mapWithFollower convexPLines $ linesOfContour contour)
  where
    onlyNodes :: ((LineSeg, LineSeg), Maybe ProjectiveLine) -> Maybe Node
    onlyNodes ((seg1, seg2), maybePLine)
      | isJust maybePLine = Just $ Node (Left (seg1,seg2)) $ fromJust maybePLine
      | otherwise         = Nothing
-}

-- | A better anticollinear checker.
-- distance is used here to get a better anticollinear than PGA has, because we have a point, and floating point hurts us.
-- FIXME: shouldn't this be pulled into PGA.hs, as part of an outsIntersectIn?
nodesAreAntiCollinear :: (Arcable a, Arcable b) => a -> b -> Bool
nodesAreAntiCollinear node1 node2
  | hasArc node1 && hasArc node2 = isAntiCollinear (outAndErrOf node1) (outAndErrOf node2)
  | otherwise = False

-- | Walk the INode tree, and find the ENodes in order.
-- Used to test the property that a walk of our result tree should result in the input ENodes in order.
findENodesInOrder :: ENodeSet -> INodeSet -> [ENode]
findENodesInOrder (ENodeSet (Slist [] _)) _ = []
findENodesInOrder eNodeSet@(ENodeSet (Slist [_] _)) (INodeSet childGenerations parent) = findENodesRecursive $ (\(Slist a _) -> a) $ childGenerations <> one [parent]
  where
    findENodesRecursive :: [[INode]] -> [ENode]
    findENodesRecursive myGens =
      case unsnoc myGens of
        Nothing -> []
        (Just (ancestorGens,workingGen)) -> concatMap findENodesOfInRecursive $ fst <$> nub (insOf (onlyINodeIn workingGen) <> maybePLineOut (onlyINodeIn workingGen))
          where
            maybePLineOut :: INode -> [(ProjectiveLine, PLine2Err)]
            maybePLineOut myINode = if hasArc myINode
                                    then [outAndErrOf myINode]
                                    else []
            -- for a generation with only one inode, retrieve that inode.
            onlyINodeIn :: [INode] -> INode
            onlyINodeIn [oneItem] = oneItem
            onlyINodeIn a = error $ "more than one inode: " <> show a <> "\n"
            findENodesOfInRecursive :: ProjectiveLine -> [ENode]
            findENodesOfInRecursive myPLine
              | isENode myPLine = [myENode]
              | otherwise = -- must be an INode. recurse.
                case unsnoc ancestorGens of
                  Nothing -> []
                  (Just (newAncestors, newLastGen)) -> findENodesRecursive $ newAncestors <> lastGenWithOnlyMyINode
                    where
                      -- strip the new last generation until it only contains the INode matching myPLine.
                      lastGenWithOnlyMyINode :: [[INode]]
                      lastGenWithOnlyMyINode = case filter (\a -> hasArc a && isCollinear (outAndErrOf a) (myPLine, mempty)) newLastGen of
                                                 [] -> []
                                                 a -> [[first a]]
                                                   where
                                                     first :: [v] -> v
                                                     first [] = error "found no INode?"
                                                     first (v:_) = v
              where
                myENode = fromMaybe (error "could not find ENode?") $ findENodeByOutput eNodeSet myPLine
                -- Determine if a PLine matches the output of an ENode.
                isENode :: ProjectiveLine -> Bool
                isENode myPLine2 = isJust $ findENodeByOutput eNodeSet myPLine2
findENodesInOrder a b = error $ "cannot find ENodes for :" <> show a <> "\n" <> show b <> "\n"

-- | Restructure an INodeSet such that the eNodes that the INodes point to are in the order of the given enode list.
-- This ordering is required, so we can draw Faces using the INodeSet.
sortINodesByENodes :: Bool -> [ENode] -> Slist [LineSeg] -> INodeSet -> INodeSet
sortINodesByENodes loop eNodes inSegSets inINodeSet@(INodeSet inChildGenerations inParent)
 | generationsIn res == 1 && inCountOf (onlyINodeOf res) > len (slist eNodes) + (len inSegSets*2-2) = errorTooManyIns
 -- skip the next property test for hallways.
 | len inSegSets == 2 = res
 -- test for the property that a walk of the INodes we are returning results in our input ENode list.
 | eNodes /= findENodesInOrder (eNodeSetOf $ slist eNodes) res = errorInsWrongOrder
 | otherwise = res
  where
    -- call our recursive handler.
    res :: INodeSet
    res = res' False inINodeSet
    -- our recursive handler.
    res' :: Bool -> INodeSet -> INodeSet
    res' hasPruned iNodeSet@(INodeSet childGenerations parent)
     | isEmpty childGenerations =
         -- nothing to do for a single INode.
         INodeSet mempty (orderInsByENodes parent)
     -- Transform #1: If the last INode is a ENode -> INode bridge, merge it into the INode its pointing to.
     | not hasPruned && canPruneTail rawLastINode =
       -- FIXME: actually look up what INode to merge with, and change it in place.
       case oldestChildGeneration of
         [] -> errorEmpty
         [oneINode] -> res' True $ INodeSet (SL.init childGenerations) $ pruneTail oneINode rawLastINode
         iNodes -> res' True $ INodeSet (SL.init childGenerations <> one (generationWithout iNodes (prunableINodeFrom iNodes rawLastINode))) $ pruneTail (prunableINodeFrom iNodes rawLastINode) rawLastINode
     -- transform #2: move inodes that have ENodes that are both before and after our sort index from the left side to the right side.
     -- FIXME: this could warp the whole tree. handle this better!
     | isJust (flippedINodeOf youngestGeneration) =
         case (generationWithout youngestGeneration flippedINode) of
           -- without the flipped INode, there is no first generation left. Let's find something to do with the flipped inode.
           [] -> case nextGeneration of
                   -- There is no next generation? ok, merge with the last INode, and terminate.
                   [] -> INodeSet mempty (iNodeWithFlips rawLastINode)
                   -- ok, the second generation is a single iNode. see if we can flip with it.
                   [secondINode] ->
                     if canFlipINodes flippedINode secondINode
                     then res' hasPruned $ addNewParent (flipINodePair flippedINode secondINode) rawLastINode
                     else -- ok, we can't flip. maybe we can merge the flipped INode with the parent INode?
                       if inCountOf rawLastINode == 2 && flippedINode `isChildOf` rawLastINode
                       then res' hasPruned $ INodeSet (one nextGeneration) $ mergeWith flippedINode rawLastINode
                       else error $ "ran out of options?"
                            <> show (inCountOf rawLastINode) <> "\n"
                   xs -> errorTooManyNodes xs
           [oneINode] -> if inCountOf rawLastINode == 2 && flippedINode `isChildOf` rawLastINode
                         then res' hasPruned $ INodeSet (one [orderInsByENodes oneINode]) $ mergeWith flippedINode rawLastINode
                         else if canFlipINodes oneINode rawLastINode
                              then res' hasPruned $ flipINodePair oneINode rawLastINode
                              else res' hasPruned $ INodeSet (one [orderInsByENodes oneINode]) $ orderInsByENodes rawLastINode
           remainingINodes -> res' hasPruned $ INodeSet (one $ indexTo $ sortGeneration $ orderInsByENodes <$> remainingINodes) $ iNodeWithFlips rawLastINode
     -- FIXME: replace this with a truely recursive function.
     | len childGenerations == 1 =
         case youngestGeneration of
           [] -> -- Not possible?
             errorEmpty
           [oneINode] ->
             case nextGeneration of
               -- check if, in order to present our ENodes in order, we need to re-order our parent, and the single item in the child generation.
               [] -> if canFlipINodes oneINode rawLastINode
                     then flipINodePair oneINode rawLastINode
                     else addYoungerGen ([orderInsByENodes oneINode]) $ res' hasPruned remainingINodeSet
               _ ->
                 res' hasPruned remainingINodeSet
           v ->
             addYoungerGen (indexTo $ sortGeneration $ orderInsByENodes <$> v) $ res' hasPruned remainingINodeSet
     | otherwise = addYoungerGen (indexTo $ sortGeneration $ orderInsByENodes <$> youngestGeneration) $ res' hasPruned remainingINodeSet
      where
        -- The first generation of INodes, as given to the recursive resolver.
        youngestGeneration = fromMaybe (error "no first generation!") $ safeHead childGenerations

        nextGeneration = fromMaybe ([]) $ safeHead $ SL.tail childGenerations

        oldestChildGeneration
          | len childGenerations > 0 = SL.last childGenerations
          | otherwise = error "tried to get a non-existent oldestGeneration"

        -- The last INode, as given to us in the recursive loop.
        rawLastINode :: INode
        rawLastINode
          | hasArc result && loop = errorIllegalLast
          | otherwise = result
            where
              result = finalINodeOf iNodeSet

        remainingINodeSet = INodeSet (SL.tail childGenerations) rawLastINode

        -- Add a new parent, making the old parent into the oldest child generation.
        addNewParent :: INodeSet -> INode -> INodeSet
        addNewParent (INodeSet myChildGens myNewLastChildGen) myNewParent = INodeSet (myChildGens <> one [myNewLastChildGen]) myNewParent

        flippedINode = fromMaybe (error "tried to inspect flippedINode when no flippedINode exists") $ flippedINodeOf youngestGeneration

        -- construct an INode including the inputs of the crossover node from the first generation merged, if it exists.
        iNodeWithFlips :: INode -> INode
        iNodeWithFlips = lastGen (sortGeneration youngestGeneration)
          where
            lastGen :: [INode] -> INode -> INode
            lastGen generation oneINode = orderInsByENodes $ case flippedINodeOf generation of
                                                             Nothing -> oneINode
                                                             (Just iNode) -> addINodeToParent iNode oneINode

    -- Force a list of INodes to start with the INode closest to the firstPLine, but not before the firstPLine.
    indexTo :: [INode] -> [INode]
    indexTo iNodes = iNodesBeforePLine iNodes <> iNodesAfterPLine iNodes
      where
        iNodesBeforePLine :: [INode] -> [INode]
        iNodesBeforePLine = filter (\a -> fst firstPLine `pLineIsLeft` fst (firstInOf a) /= Just False)
        -- nodes in the right order, after the divide.
        iNodesAfterPLine myINodes = withoutFlippedINodes $ filter (\a -> fst firstPLine `pLineIsLeft` fst (firstInOf a) == Just False) myINodes
        withoutFlippedINodes maybeFlippedINodes = case flippedINodeOf maybeFlippedINodes of
                                                    Nothing -> maybeFlippedINodes
                                                    (Just a) -> filter (/= a) maybeFlippedINodes

    errorEmpty = error $ "empty INodeSet for nodes:\n" <> show eNodes <> "\nloop: " <> show loop <> "\n"

    errorTooManyNodes nodes = error
                              $ "don't know how to handle a case with these nodes:\n"
                              <> show nodes <> "\n"
                              <> "inInodeSet:          " <> show inINodeSet <> "\n"
                              <> "eNodes:              " <> show eNodes <> "\n"
                              <> "flippedINode:        " <> show (flippedINodeOf $ SL.head inChildGenerations) <> "\n"
                              <> "loop:                " <> show loop <> "\n"

    errorTooManyIns = error $ "generating a single INode with more inputs than possible: " <> show res <> "\n"
                           <> "inInodeSet:         " <> show inINodeSet <> "\n"
                           <> "eNodes:             " <> show eNodes <> "\n"
                           <> "inSegSets:          " <> show inSegSets <> "\n"
                           <> "loop:               " <> show loop <> "\n"

    errorInsWrongOrder = error
                         $ "ENodes outs should be: " <> show (outOf <$> eNodes) <> "\n"
                         <> "ENode outs are:       " <> show (outOf <$> findENodesInOrder (eNodeSetOf $ slist eNodes) res) <> "\n"
                         <> "input inINodeSet:     " <> show inINodeSet <> "\n"
                         <> "returned INodeSet:    " <> show res <> "\n"
                         <> "flippedINode:         " <> show (flippedINodeOf $ SL.head inChildGenerations) <> "\n"
                         <> "canPruneTail:         " <> show (canPruneTail inParent) <> "\n"

    errorIllegalLast = error
                       $ "illegal last generation:\n"
                       <> "inINodeSet:            " <> show inINodeSet <> "\n"
                       <> "eNodes:                " <> show eNodes <> "\n"
                       <> "inSegSets:             " <> show inSegSets <> "\n"
                       <> "loop:                  " <> show loop <> "\n"

    -- if the object is closed, and the last generation consists of an INode that points to just one ENode and one INode, merge the last generation into the prior generation.
    -- Assuming that really, this should have been just another input to the INode it connects to.
    canPruneTail :: INode -> Bool
    canPruneTail lastGen = loop && hasENode lastGen && hasINode lastGen && inCountOf lastGen == 2

    prunableINodeFrom :: [INode] -> INode -> INode
    prunableINodeFrom iNodes lastINode
      | iNodes == [] = error "tried to get prunable inode from an empty generation."
      | otherwise = case filter (\a -> canPruneWith lastINode a) iNodes of
                      [] -> error "could not find an INode to prune with,"
                      [a] -> a
                      _ -> error "found too many INodes to prune with!"

    -- check to see if the last INode is able to be pruned into a given INode.
    canPruneWith lastGen iNode = hasArc iNode && hasIn lastGen (outAndErrOf iNode)

    -- actually do tail pruning. add the two INodes together.
    pruneTail :: INode -> INode -> INode
    pruneTail iNode1 iNode2 = addINodeToParent iNode1 iNode2

    -- check to see if an INode can be merged with another INode.
    isChildOf :: INode -> INode -> Bool
    isChildOf inode1 inode2 = hasArc inode1 && hasIn inode2 (outAndErrOf inode1)

    -- Merge two INodes.
    mergeWith :: INode -> INode -> INode
    mergeWith iNode1 iNode2 = addINodeToParent iNode1 iNode2

    -- check to see if an input of the given INode is parallel to he given PLine.
    hasIn :: INode -> (ProjectiveLine, PLine2Err) -> Bool
    hasIn iNode pLine2 = case filter (\a -> isCollinear a pLine2) $ insOf iNode of
                           [] -> False
                           [_] -> True
                           (_:_) -> error "filter passed too many options."

    generationWithout :: [INode] -> INode -> [INode]
    generationWithout inGen iNode
      | inGen == outGen = error "did not filter out anything"
      | otherwise = outGen
      where
        outGen = filter (\a -> a /= iNode) inGen

    -- in situations where an INode contains pointers to ENodes that are and the object is closed, we may be able to move the node up the tree of INodes.
    -- The idea is that these should get to the last generation.
    canFlipINodes :: INode -> INode -> Bool
    canFlipINodes firstINode secondINode = loop && inCountOf secondINode > 2 && isJust (flippedINodeOf [firstINode])

    -- Transform the two INodes.
    -- make the first inode into the final inode, keeping the old final inode's output, and
    -- make the final inode into the first inode, flipping the direction of the connecting line.
    flipINodePair :: INode -> INode -> INodeSet
    flipINodePair iNode1 iNode2@(INode _ _ _ maybeOut2) = INodeSet (one [orderInsByENodes newINode1]) (orderInsByENodes newINode2)
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
        iNodeInsOf myINode = filter (\a -> isNothing $ findENodeByOutput (eNodeSetOf $ slist eNodes) (fst a)) $ insOf myINode
        withoutConnectingPLine = filter (/= oldConnectingPLine)

    -- Determine if the given INode has a PLine that points to an ENode.
    hasENode iNode = any (isJust . findENodeByOutput (eNodeSetOf $ slist eNodes)) (fst <$> insOf iNode)
    -- Determine if the given INode has a PLine that points to another INode.
    hasINode iNode = any (isNothing . findENodeByOutput (eNodeSetOf $ slist eNodes)) (fst <$> insOf iNode)

    -- Construct an ENodeSet
    eNodeSetOf :: Slist ENode -> ENodeSet
    eNodeSetOf myENodes
      | null myENodes = error "cannot construct an empty ENodeSet"
      | otherwise = ENodeSet (one $ makeSide $ (\(Slist a _) -> a ) myENodes)

    generationsIn :: INodeSet -> Int
    generationsIn (INodeSet childGenerations _) = len childGenerations + 1

    -- how many input PLines does an INode have.
    inCountOf (INode in1 in2 moreIns _)
      -- HACK: handle two identical inputs.
      | in1 == in2 = 1+len moreIns
      | otherwise = 2+len moreIns

    -- the only inode of an INodeSet. must have one generation only.
    onlyINodeOf :: INodeSet -> INode
    onlyINodeOf (INodeSet (Slist [] _) a)
      | hasArc a && loop = errorIllegalLast
      | otherwise = a
    onlyINodeOf a = error
                    $ "not only inode!\n"
                    <> show a <> "\n"
                    <> show eNodes <> "\n"
                    <> show inChildGenerations <> "\n"

    -- | Sort a generation by the first in PLine.
    sortGeneration :: [INode] -> [INode]
    sortGeneration = sortBy (\a b -> if fst (firstInOf a) `pLineIsLeft` fst (firstInOf b) == Just False then LT else GT)

    -- Find an inode connecting the first and last ENode, if it exists.
    -- FIXME: this functions, but i don't know why. :)
    flippedINodeOf :: [INode] -> Maybe INode
    flippedINodeOf inodes = case filter (\a -> fst firstPLine `pLineIsLeft` fst (firstInOf a) == Just False) inodes of
                              [] -> Nothing
                              [a] -> -- if there is only one result, it's going to only point to enodes.
                                Just a
                              xs -> -- if there is more than one result, then one of the descendants of the right answer is caught in the filter.
                                case filter allInsAreENodes xs of
                                  [] -> Nothing
                                  [a] -> Just a
                                  vs -> error
                                        $ "more than one flipped inode?" <> show vs <> "\n"
                                        <> show eNodes <> "\n"
                                where
                                  allInsAreENodes iNode = not $ hasINode iNode

    -- the output PLine of the first ENode in the input ENode set.
    firstPLine = outAndErrOf firstENode
      where
        -- the first ENode given to us. for sorting uses.
        firstENode = first eNodes
          where
            first :: [a] -> a
            first [] = error "no first enode?"
            first (a:_) = a

    -- | add together a child and it's parent.
    addINodeToParent :: INode -> INode -> INode
    addINodeToParent iNode1 iNode2@(INode _ _ _ out2)
      | hasArc iNode1 = orderInsByENodes $ makeINode (insOf iNode1 <> withoutPLine (outAndErrOf iNode1) (insOf iNode2)) out2
      | otherwise = error "cannot merge a child inode with no output!"
      where
        withoutPLine :: (ProjectiveLine, PLine2Err) -> [(ProjectiveLine, PLine2Err)] -> [(ProjectiveLine, PLine2Err)]
        withoutPLine myPLine = filter (\a -> fst a /= fst myPLine)

    -- Order the input nodes of an INode.
    orderInsByENodes :: INode -> INode
    orderInsByENodes inode@(INode _ _ _ out) = makeINode (indexPLinesTo firstPLine $ sortedPLines $ indexPLinesTo firstPLine $ insOf inode) out

-- | Add a new generation to an existing INodeSet.
addYoungerGen :: [INode] -> INodeSet -> INodeSet
addYoungerGen newGen (INodeSet myChildGens myLastINode) = INodeSet (one newGen <> myChildGens) myLastINode

-- | Apply a recursive algorithm to obtain a raw INodeSet.
-- This generates an INodeSet where each generation is composed of the closest together intersection of arcs, and thus does not discover in ENode order.
-- FIXME: does not handle more than two point intersections of arcs properly.
-- NOTE: the first two arguments are passed through to later recursive calls.
skeletonOfNodes :: Bool -> Slist [LineSeg] -> Slist [LineSeg] -> [INode] -> Either PartialNodes INodeSet
skeletonOfNodes connectedLoop origSegSets inSegSets iNodes =
  case eNodes of
    [] -> case iNodes of
            [] -> -- zero nodes == return emptyset. allows us to simplify our return loop.
              errorLen0 -- A one node loop makes no sense, reject.
            [iNode] ->
              if contourLooped
              then errorLen1 -- A one node loop makes no sense, reject.
              else Right $ INodeSet mempty iNode -- just hand back single node requests.
            [iNode1,iNode2] -> handleTwoNodes iNode1 iNode2
            (_:_) -> handleThreeOrMoreNodes
    [eNode] -> case iNodes of
                 [] ->
                   if contourLooped
                   then errorLen1 -- A one node loop makes no sense, reject.
                   else
                     -- Construct an INode with two identical inputs, and return it.
                     -- FIXME: shouldn't we be able to have a Nothing instead?
                     Right $ INodeSet mempty $ makeINode [outAndErrOf eNode,outAndErrOf eNode] Nothing
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
    errorLen0 = error "we should have not recursed."
    errorLen1 = Left $ PartialNodes iNodes ("NOMATCH - length 1?\n" <> show eNodes <> "\n" <> show iNodes <> "\n" <> show inSegSets <> "\n")
    --   Handle the the case of two nodes.
    handleTwoNodes :: (Arcable a, Pointable a, Arcable b, Pointable b) => a -> b -> Either PartialNodes INodeSet
    handleTwoNodes node1 node2
      | not (hasArc node1) || not (hasArc node2) = error $ "ran across a node without an output?\n" <> show node1 <> "\n" <> show node2 <> "\n"
      | isCollinear (outAndErrOf node1) (outAndErrOf node2) = Left $ PartialNodes iNodes $ "cannot handle collinear nodes:\n" <> show node1 <> "\n" <> show node2 <> "\n"
      | nodesAreAntiCollinear node1 node2 && contourLooped = Right $ INodeSet mempty $ makeLastPair node1 node2
      | contourLooped =
        -- this is a complete loop, so this last INode will be re-written in sortINodesByENodes anyways.
        Right $ INodeSet mempty $ makeINode (sortedPLines [outAndErrOf node1,outAndErrOf node2]) Nothing
      | intersectsInPoint node1 node2 = Right $ INodeSet mempty $ safeAverageNodes node1 node2
      | otherwise = errorLen2
      where
        errorLen2 = Left $ PartialNodes iNodes $ "NOMATCH - length 2?\n" <> show node1 <> "\n" <> show node2 <> "\n" <> show contourLooped <> "\n" <> show eNodes <> "\n" <> show iNodes <> "\n"
    --   Handle the the case of 3 or more nodes.
    handleThreeOrMoreNodes :: Either PartialNodes INodeSet
    handleThreeOrMoreNodes
      | not (all hasArc iNodes) = error "found an Inode without an output!"
      | endsAtSamePoint && contourLooped = Right $ INodeSet mempty $ makeINode (sortedPLines $ (outAndErrOf <$> eNodes) <> (outAndErrOf <$> iNodes)) Nothing
      -- FIXME: this can happen for non-loops. which means this Nothing is wrong. it should be the result of the intersection tree from the first and last node in the segment.
      | endsAtSamePoint && not contourLooped = error $ show $ makeINode (sortedPLines $ (outAndErrOf <$> eNodes) <> (outAndErrOf <$> iNodes)) Nothing
      | hasShortestNeighboringPair = Right $ addYoungerGen averageOfShortestPairs $ errorIfLeft (skeletonOfNodes remainingLoop origSegSets remainingLineSegs (remainingINodes <> averageOfShortestPairs))
      | otherwise = error $ "len3\n" <> errorLen3
    errorLen3 =    "shortestPairDistance: " <> show shortestPairDistance <> "\n"
                <> "ePairDistance: " <> show shortestEPairDistance <> "\n"
                <> "shortestEPairs: " <> show (shortestPairs eNodes) <> "\n"
                <> show (isSomething shortestEPairDistance) <> "\n"
                <> "iPairDistance: " <> show shortestIPairDistance <> "\n"
                <> "shortestIPairs: " <> show (shortestPairs iNodes) <> "\n"
                <> show (isSomething shortestIPairDistance) <> "\n"
                <> "mixedPairDistance: " <> show shortestMixedPairDistance <> "\n"
                <> "shortestMixedPairs: " <> show shortestMixedPairs <> "\n"
                <> show (isSomething shortestMixedPairDistance) <> "\n"
                <> show (shortestEPairDistance == shortestPairDistance) <> "\n"
                <> "remainingLineSegs: " <> show remainingLineSegs <> "\n"
                <> "remainingINodes: " <> show remainingINodes <> "\n"
                <> "origSegSets: " <> show origSegSets <> "\n"
                <> "endsAtSamePoint: " <> show endsAtSamePoint <> "\n"

    -- | check to see if all of our nodes end in the same point
    -- If so, this is a sign that we should create a single Node with all of them as input.
    endsAtSamePoint :: Bool
    endsAtSamePoint = intersectionsAtSamePoint allOuts
      where
        allOuts = (outAndErrOf <$> eNodes) <> (outAndErrOf <$> iNodes)

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
                      (Slist segLists _) -> slist $ concatMap (removeENodesFromSegList (slist foundENodes)) segLists
      where
        removeENodesFromSegList :: Slist ENode -> [LineSeg] -> [[LineSeg]]
        removeENodesFromSegList eNodesIn lineSegs =
          mapMaybe isTooShort $ case eNodesIn of
                                  (Slist [] _) -> [lineSegs]
                                  (Slist (oneENode:moreENodes) _) -> concatMap (removeENodeFromSegList oneENode) $ removeENodesFromSegList (slist moreENodes) lineSegs
          where
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
    averageOfShortestPairs = (uncurry safeAverageNodes <$> iPairsFound) <> (uncurry safeAverageNodes <$> mixedPairsFound) <> (uncurry safeAverageNodes <$> ePairsFound)

    iPairsFound =
      if isSomething shortestIPairDistance && shortestIPairDistance == shortestPairDistance
      then shortestPairs iNodes
      else []

    mixedPairsFound =
      if isSomething shortestMixedPairDistance && shortestMixedPairDistance == shortestPairDistance
      then removeFoundINodesOf shortestMixedPairs
      else []
      where
        -- | Filter out any node pairs in our list that contain an INode from the iPairsFound list.
        removeFoundINodesOf :: [(ENode, INode)] -> [(ENode, INode)]
        removeFoundINodesOf = filter withoutINodes
          where
            withoutINodes (_,myINode) = myINode `notElem` ((fst <$> iPairsFound) <> (snd <$> iPairsFound))

    ePairsFound =
      if isSomething shortestEPairDistance && shortestEPairDistance == shortestPairDistance
      then removeFoundENodesOf $ shortestNeighboringPairs $ mapWithFollower (,) eNodes
      else []
      where
        -- | Filter out any node pairs in our list that contain an ENode from the mixedPairsFound list.
        removeFoundENodesOf :: [(ENode, ENode)] -> [(ENode, ENode)]
        removeFoundENodesOf = filter withoutENodes
          where
            withoutENodes (myENode1, myENode2) = myENode1 `notElem` (fst <$> mixedPairsFound) && myENode2 `notElem` (fst <$> mixedPairsFound)

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
                              [(node1, node2)] -> [(node1, node2)]
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

    safeAverageNodes :: (Arcable a, Pointable a, Arcable b, Pointable b) => a -> b -> INode
    safeAverageNodes n1 n2
      | not (hasArc n1) || not (hasArc n2) = error $ "Cannot get the average of nodes if one of the nodes does not have an out!\n" <> errorLen3
      | not (canPoint n1) || not (canPoint n2) = error $ "Cannot get the average of nodes if we cannot resolve them to a point!\n" <> errorLen3
      | isParallel (outAndErrOf n1) (outAndErrOf n2) = error $ "Cannot get the average of nodes if their outputs never intersect!\n" <> errorLen3
      | isAntiParallel (outAndErrOf n1) (outAndErrOf n2) = error $ "Cannot get the average of nodes if their outputs never intersect!\n" <> errorLen3
      | isCollinear (outAndErrOf n1) (outAndErrOf n2) = error $ "Cannot (yet) handle two input plines that are collinear.\n" <> errorLen3
      | nodesAreAntiCollinear n1 n2 = error $ "Cannot (yet) handle two input plines that are collinear.\n" <> errorLen3
      | n1Distance <= ulpVal n1Err = error $ "intersection is AT the point of n1!\n" <> show n1Distance <> "\n" <> show n2Distance <> "\n" <> show intersectionPoint <> "\n" <> show n1 <> "\n" <> show n2 <> "\n" <> errorLen3
      | n2Distance <= ulpVal n2Err = error $ "intersection is AT the point of n2!\n" <> show n1Distance <> "\n" <> show n2Distance <> "\n" <> show intersectionPoint <> "\n" <> show n1 <> "\n" <> show n2 <> "\n" <> errorLen3
      | n1Distance > ulpVal n1Err && n2Distance > ulpVal n2Err = averageNodes n1 n2
      | otherwise = error $ "found node too close:\n"
                          <> show n1 <> "\n"
                          <> show n2 <> "\n"
      where
        intersectionPoint = fromMaybe (error "has arcs, but no intersection?") $ intersectionBetweenArcsOf n1 n2
        (n1Distance, (_,_, n1Err)) = distance2PP intersectionPoint (cPPointAndErrOf n1)
        (n2Distance, (_,_, n2Err)) = distance2PP intersectionPoint (cPPointAndErrOf n2)

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
    intersectingMixedNodePairs = filter (uncurry intersectsInPoint) $ getMixedPairs eNodes iNodes
      where
        getMixedPairs ::  [a] -> [b] -> [(a, b)]
        getMixedPairs set1 set2 = concatMap (\a -> (a,) <$> set2) set1

    -- | find nodes of the same type that can intersect.
    intersectingNodePairsOf :: (Arcable a, Pointable a) => [a] -> [(a, a)]
    intersectingNodePairsOf inNodes = filter (uncurry intersectsInPoint) $ getPairs inNodes

    -- | find nodes of the same type that can intersect.
    -- NOTE: accepts node pairs, so that we can ensure we check just following ENodes.
    intersectingNeighboringNodePairsOf :: (Arcable a, Pointable a) => [(a,a)] -> [(a, a)]
    intersectingNeighboringNodePairsOf = filter (uncurry intersectsInPoint)

    -- | for a given pair of nodes, find the longest distance between one of the two nodes and the intersection of the two output plines.
    distanceToIntersection :: (Pointable a, Arcable a, Pointable b, Arcable b) => a -> b -> Maybe ℝ
    distanceToIntersection node1 node2
      | canPoint node1
        && canPoint node2
        && hasArc node1
        && hasArc node2
        && intersectsInPoint node1 node2 =
        Just $ distanceBetweenPPointsWithErr (cPPointAndErrOf node1) (intersectionOf (outAndErrOf node1) (outAndErrOf node2))
               `max`
               distanceBetweenPPointsWithErr (cPPointAndErrOf node2) (intersectionOf (outAndErrOf node1) (outAndErrOf node2))

      | otherwise = Nothing
    -- | Check if the intersection of two nodes results in a point or not.
    intersectsInPoint :: (Arcable a, Pointable a, Arcable b, Pointable b) => a -> b -> Bool
    intersectsInPoint node1 node2
      | hasArc node1 && hasArc node2 = not (noIntersection (outAndErrOf node1) (outAndErrOf node2))
                                       && not (dist1 <= ulpVal dist1Err)
                                       && not (dist2 <= ulpVal dist2Err)
      | otherwise                    = error $ "cannot intersect a node with no output:\nNode1: " <> show node1 <> "\nNode2: " <> show node2 <> "\nnodes: " <> show iNodes <> "\n"
      where
        (dist1, (_,_, dist1Err)) = distance2PP (intersectionOf (outAndErrOf node1) (outAndErrOf node2)) (cPPointAndErrOf node1)
        (dist2, (_,_, dist2Err)) = distance2PP (intersectionOf (outAndErrOf node1) (outAndErrOf node2)) (cPPointAndErrOf node2)
