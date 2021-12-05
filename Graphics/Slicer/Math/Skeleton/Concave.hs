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

import Prelude (Eq, Show, Bool(True, False), Either(Left, Right), String, Ord, Ordering(GT,LT), notElem, otherwise, ($), (>), (<), (<$>), (==), (/=), error, (&&), fst, and, (<>), show, not, max, concat, compare, uncurry, null, (||), min, snd, filter, id, zip, any, (*), (+), Int, (.))

import Data.Maybe( Maybe(Just,Nothing), catMaybes, isJust, isNothing, fromMaybe)

import Data.List (takeWhile, sortBy)

import Data.List.Extra (unsnoc)

import Slist.Type (Slist(Slist))

import Slist (slist, one, cons, len, isEmpty, safeLast)

import Slist as SL (head, last, tail, init)

import Graphics.Implicit.Definitions (ℝ)

import Graphics.Slicer.Math.Contour (lineSegsOfContour)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg, mapWithFollower, distance, fudgeFactor, startPoint)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPair)

import Graphics.Slicer.Math.Line (endPoint)

import Graphics.Slicer.Math.PGA (PLine2(PLine2), PPoint2, eToPLine2, flipPLine2, normalizePLine2, distanceBetweenPPoints, pLineIsLeft, angleBetween, join2PPoint2, distancePPointToPLine, flipPLine2)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), ENodeSet(ENodeSet), INode(INode), INodeSet(INodeSet), NodeTree, Arcable(hasArc, outOf), Pointable(canPoint, pPointOf), concavePLines, eNodeToINode, noIntersection, intersectionOf, isAntiCollinear, finalOutOf, getPairs, isCollinear, indexPLinesTo, isParallel, linePairs, makeINode, sortedPLines)

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

-- | Recurse on a set of nodes until we have a complete NodeTree.
--   Only works on a sequnce of concave line segments, when there are no holes in the effected area.
skeletonOfConcaveRegion :: [LineSeg] -> NodeTree
skeletonOfConcaveRegion inSegs
  | not loop && isJust (finalOutOf result) = result
  | loop && isNothing (finalOutOf result) = result
  | otherwise = error $ "illegal nodeTree:" <> show inSegs <> "\n" <> show loop <> "\n" <> show result <> "\n"
  where
    result = getNodeTree (firstENodes inSegs loop)
    -- are the incoming line segments a loop?
    loop = endPoint lastSeg == startPoint firstSeg
           || distance (endPoint lastSeg) (startPoint firstSeg) < (fudgeFactor*15)
      where
        lastSeg = SL.last $ slist inSegs
        firstSeg = SL.head $ slist inSegs
        
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
    getNodeTree initialGeneration = makeNodeTree initialGeneration $ sortINodesByENodes (errorIfLeft (skeletonOfNodes loop initialGeneration [])) initialGeneration loop

-- | Handle the recursive resolver failing.
errorIfLeft :: Either PartialNodes INodeSet -> INodeSet
errorIfLeft (Left failure) = error $ "Fail!\n" <> show failure -- <> "\ninSegs: " <> show inSegs <> "\n" <> show (firstENodes inSegs loop) <> "\nloop:" <> show loop <> "\n"
errorIfLeft (Right val)    = val

-- | For a given par of nodes, construct a new internal node, where it's parents are the given nodes, and the line leaving it is along the the obtuse bisector.
--   Note: this should be hidden in skeletonOfConcaveRegion, but it's exposed here, for testing.
averageNodes :: (Arcable a, Pointable a, Show a, Arcable b, Pointable b, Show b) => a -> b -> INode
averageNodes n1 n2
  | not (hasArc n1) || not (hasArc n2) = error $ "Cannot get the average of nodes if one of the nodes does not have an out!\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | not (canPoint n1) || not (canPoint n2) = error $ "Cannot get the average of nodes if we cannot resolve them to a point!\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | isParallel (outOf n1) (outOf n2) = error $ "Cannot get the average of nodes if their outputs never intersect!\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | isCollinear (outOf n1) (outOf n2) = error $ "Cannot (yet) handle two input plines that are collinear.\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | nodesAreAntiCollinear n1 n2 = error $ "Cannot (yet) handle two input plines that are collinear.\nNode1: " <> show n1 <> "\nNode2: " <> show n2 <> "\n"
  | otherwise                 = makeINode (sortedPair n1 n2) $ Just $ getOutsideArc (pPointOf n1) (outOf n1) (pPointOf n2) (outOf n2)

-- | Take a pair of arcables, and return their outOfs, in a sorted order.
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

-- | Make a first generation node.
makeENode :: LineSeg -> LineSeg -> ENode
makeENode seg1 seg2 = ENode (seg1,seg2) $ getFirstArc seg1 seg2

-- | Make a first generation set of nodes, AKA, a set of arcs that come from the points where line segments meet, toward the inside of the contour.
makeENodes :: [LineSeg] -> [ENode]
makeENodes segs = case segs of
                         [] -> error "got empty list at makeENodes.\n"
                         [a] -> error $ "not enough line segments at makeENodes: " <> show a <> "\n"
                         [a,b] -> [makeENode a b]
                         xs -> case unsnoc $ mapWithFollower makeENode xs of
                                 Nothing -> error "impossible!"
                                 Just (i,_) -> i

-- | Make a first generation set of nodes, AKA, a set of arcs that come from the points where line segments meet, toward the inside of the contour.
makeENodesLooped :: [LineSeg] -> [ENode]
makeENodesLooped segs
  | null segs = error "got empty list at makeNodes.\n"
  | otherwise = mapWithFollower makeENode segs

-- | Find the non-reflex virtexes of a contour, and create ENodes from them.
--   This function is meant to be used on an exterior contour.
eNodesOfOutsideContour :: Contour -> [ENode]
eNodesOfOutsideContour contour = catMaybes $ onlyNodes <$> zip (linePairs contour) (mapWithFollower concavePLines $ lineSegsOfContour contour)
  where
    onlyNodes :: ((LineSeg, LineSeg), Maybe PLine2) -> Maybe ENode
    onlyNodes ((seg1, seg2), Just _) = Just $ makeENode seg1 seg2
    onlyNodes ((_, _), Nothing) = Nothing

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

-- | A better anticollinear checker.
-- distance is used here to get a better anticollinear than PGA has, because we have a point, and floating point hurts us.
nodesAreAntiCollinear :: (Pointable a, Arcable a, Pointable b, Arcable b) => a -> b -> Bool
nodesAreAntiCollinear node1 node2
  | hasArc node1 && hasArc node2 && isAntiCollinear (outOf node1) (outOf node2) = True
  | canPoint node1 && hasArc node2 = distancePPointToPLine (pPointOf node1) (outOf node2) < fudgeFactor*50
  | canPoint node2 && hasArc node1 = distancePPointToPLine (pPointOf node2) (outOf node1) < fudgeFactor*50
  | otherwise = False

-- Walk the result tree, and find our enodes. Used to test the property that a walk of our result tree should result in the input ENodes in order.
findENodesInOrder :: ENodeSet -> [[INode]] -> [ENode]
findENodesInOrder eNodeSet@(ENodeSet (Slist [(_,_)] _)) = findENodesRecursive
  where
    findENodesRecursive :: [[INode]] -> [ENode]
    findENodesRecursive [] = []
    findENodesRecursive myGens =
      case unsnoc myGens of
        Nothing -> []
        (Just (ancestorGens,workingGen)) -> concat $ findENodesOfInRecursive <$> insOf (onlyINodeIn workingGen)
          where
            -- for a generation with only one inode, retrieve that inode.
            onlyINodeIn :: [INode] -> INode
            onlyINodeIn [oneItem] = oneItem
            onlyINodeIn a = error $ "more than one inode: " <> show a <> "\n"
            findENodesOfInRecursive :: PLine2 -> [ENode]
            findENodesOfInRecursive myPLine
              | isENode myPLine = [myENode]
              | otherwise = -- must be an INode. recurse.
                case unsnoc ancestorGens of
                  Nothing -> []
                  (Just (newAncestors, newLastGen)) -> findENodesRecursive $ newAncestors <> lastGenWithOnlyMyINode
                    where
                      -- strip the new last generation until it only contains the INode matching myPLine.
                      lastGenWithOnlyMyINode :: [[INode]]
                      lastGenWithOnlyMyINode = case filter (\a -> outOf a == myPLine) newLastGen of
                                                 [] -> []
                                                 a -> [[first a]]
                                                   where
                                                     first :: [a] -> a
                                                     first [] = error "found no INode?"
                                                     first (a:_) = a
              where
                myENode = fromMaybe (error "could not find ENode?") $ findENodeByOutput eNodeSet myPLine
                -- Determine if a PLine matches the output of an ENode.
                isENode :: PLine2 -> Bool
                isENode myPLine2 = isJust $ findENodeByOutput eNodeSet myPLine2

-- | place our inodes in a state such that the eNodes that the inodes point to are in the order of the given enode list.
-- also performs 'safe' node tree transforms:
sortINodesByENodes :: INodeSet -> [ENode] -> Bool -> INodeSet
sortINodesByENodes inGens@(INodeSet rawGenerations) initialGeneration loop
 | generationsIn res == 1 && inCountOf (onlyINodeOf $ INodeSet $ resSlist rawGenerations) > len (slist initialGeneration) = errorTooManyIns
 -- test for the property that a walk of the INodes we are returning results in our input ENode list.
 | initialGeneration /= findENodesInOrder (eNodeSetOf $ slist initialGeneration) res = errorInsWrongOrder
 | otherwise = INodeSet $ resSlist rawGenerations
  where
    res :: [[INode]]
    res = unSlist $ resSlist rawGenerations
      where
        unSlist (Slist a _) = a
    -- our first attempt at a recursive handler.
    resSlist :: Slist [INode] -> Slist [INode]
    resSlist generations
     | isEmpty generations = errorEmpty
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
                 else if canFlipGenerations oneINode rawLastINode
                      then flipINodePair oneINode rawLastINode
                      else one [orderInsByENodes oneINode] <> one [orderInsByENodes rawLastINode]   
               v -> one (indexTo $ sortGeneration v) <> one [iNodeWithFlips rawLastINode]
           (Just flippedINode) ->
             case genWithoutFlips rawFirstGeneration of
               [] -> -- after transform #1, there is no first generation left. just return the second, after the transform.
                 one [iNodeWithFlips rawLastINode]
               [oneINode] -> -- after transform #1, there is just one INode left.
                 if inCountOf rawLastINode == 2 && canMergeWith flippedINode rawLastINode
                 then one [orderInsByENodes oneINode] <> mergeWith flippedINode rawLastINode
                 else if canPruneTail rawLastINode
                      then pruneTail oneINode rawLastINode
                      else if canFlipGenerations oneINode rawLastINode
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
               [] -> -- after transform #1, there is no first generation left. collapse our flipped inode into the next generation.
                 resSlist (slist $ [flippedINode:secondGen] <> [[rawLastINode]])
               [oneINode] ->
                 error
                 $ show oneINode <> "\n"
                 <> show (one [orderInsByENodes oneINode] <> resSlist (slist $ [flippedINode:secondGen] <> [[rawLastINode]])) <> "\n"
                 <> show initialGeneration <> "\n"
      where
        -- the first generation, as given to us.
        rawFirstGeneration = SL.head generations
        -- the last generation, as given to us.
        rawLastGeneration = SL.last generations

        -- construct an INode including the inputs of the crossover node from the first generation merged, if it exists.
        iNodeWithFlips :: INode -> INode
        iNodeWithFlips = lastGen (sortGeneration rawFirstGeneration)
          where
            lastGen :: [INode] -> INode -> INode
            lastGen firstGen oneINode = orderInsByENodes $ case flippedINodeOf firstGen of
                                                             Nothing -> oneINode
                                                             (Just flippedINode) -> addINodeToParent flippedINode oneINode

        -- The last INode, as given to us.
        rawLastINode :: INode
        rawLastINode
          | hasArc result && loop = errorIllegalLast
          | otherwise = result
          where
            result = first rawLastGeneration
              where
                first :: [a] -> a
                first [] = error "no first of the last generation?"
                first (a:_) = a

        secondToLastGen = fromMaybe (error "no second to last generation?") $ safeLast $ SL.init moreGens
        firstAndMids = case genWithoutFlips rawFirstGeneration of
                         [] -> midGens
                         v  -> one v <> midGens
        midGens = SL.init $ SL.tail generations
        secondGen = SL.head $ SL.tail generations
        moreGens = SL.tail generations

    -- transform #1: if a first generation inode connects the last and the first ENodes, remove it, and point to those enodes with the last INode.
    -- Place the first generation in ENode order, and remove a 'flipped' node if it exists.
    -- NOTE: If one of the nodes is constructed from the first and last ENodes, we filter it out. it will be merged into the last generation.
    genWithoutFlips myGeneration = indexTo $ sortGeneration myGeneration

    -- Force a list of INodes to start with the INode closest to the firstPLine, but not before the firstPLine.
    indexTo :: [INode] -> [INode]
    indexTo iNodes = iNodesBeforePLine iNodes <> iNodesAfterPLine iNodes
      where
        iNodesBeforePLine :: [INode] -> [INode]
        iNodesBeforePLine = filter (\a -> firstPLine `pLineIsLeft` firstInOf a /= Just False) 
        -- nodes in the right order, after the divide.
        iNodesAfterPLine myINodes = withoutFlippedINodes $ filter (\a -> firstPLine `pLineIsLeft` firstInOf a == Just False) myINodes
        withoutFlippedINodes maybeFlippedINodes = case flippedINodeOf maybeFlippedINodes of
                                                    Nothing -> maybeFlippedINodes
                                                    (Just a) -> filter (/= a) maybeFlippedINodes

    errorEmpty = error $ "empty INodeSet for nodes:\n" <> show initialGeneration <> "\nloop: " <> show loop <> "\n" 

    errorTooManyIns = error $ "generating a single INode with more inputs than possible: " <> show res <> "\n"
                           <> "rawGenerations:                 " <> show rawGenerations <> "\n"
                           <> "ENodes:                         " <> show initialGeneration <> "\n"

    errorInsWrongOrder = error
                         $ "ENodes should be:  " <> show (outOf <$> initialGeneration) <> "\n"
                         <> "ENode outs are:    " <> show (outOf <$> findENodesInOrder (eNodeSetOf $ slist initialGeneration) res) <> "\n"
                         <> "rawGenerations:    " <> show rawGenerations <> "\n"
                         <> "returned inodes:   " <> show res <> "\n"
                         <> "loop:              " <> show loop <> "\n"

    errorIllegalLast = error
                       $ "illegal last generation:\n"
                       <> "rawGenerations:    " <> show rawGenerations <> "\n"
                       <> "initialGeneration: " <> show initialGeneration <> "\n"
                       <> "loop:              " <> show loop <> "\n"

    -- if the object is closed, and the last generation consists of an INode that points to just one ENode and one INode, merge the last generation into the prior generation.
    -- assuming that really, this should have been just another in to the previous generation.
    canPruneTail lastGen = loop && hasENode lastGen && hasINode lastGen && inCountOf lastGen == 2
    pruneTail :: INode -> INode -> Slist [INode]
    pruneTail iNode1 iNode2 = one [addINodeToParent iNode1 iNode2]

    -- check to see if an INode can be merged with another INode. 
    canMergeWith :: INode -> INode -> Bool
    canMergeWith source@(INode _ _ _ maybeSourceOut) destination
      | isNothing maybeSourceOut = False
      | hasIn destination (outOf source) = True
      | otherwise = False
      where
        hasIn :: INode -> PLine2 -> Bool
        hasIn iNode pLine2 = case filter (==pLine2) $ insOf iNode of
                               [] -> False
                               (_:[]) -> True
                               (_:_) -> error "filter passed too many options."
    -- Merge two INodes.
    mergeWith :: INode -> INode -> Slist [INode]
    mergeWith iNode1 iNode2 = one [addINodeToParent iNode1 iNode2]

    -- if the object is closed, and the first generation contains the pointers to the first and last ENodes, make the first generation the second, and the second into the first.
    canFlipGenerations firstGen secondGen = loop && hasENode secondGen && hasINode secondGen && isJust (flippedINodeOf [firstGen])
    -- determine if the given INode has a direct in from an ENode.
    flipINodePair :: INode -> INode -> Slist [INode]
    flipINodePair iNode1 iNode2@(INode _ _ _ maybeOut2) = one [orderInsByENodes newINode1] <> one [orderInsByENodes newINode2]
      where
        newINode1 = makeINode (withoutConnectingPLine $ insOf iNode2) (Just newConnectingPLine)
        newINode2 = makeINode ([newConnectingPLine] <> insOf iNode1) maybeOut2
        newConnectingPLine = flipPLine2 oldConnectingPLine
        oldConnectingPLine = case filter (isNothing . findENodeByOutput (eNodeSetOf $ slist initialGeneration)) $ insOf iNode2 of
                               [] -> error "could not find old connecting PLine."
                               [v] -> v
                               (_:_) -> error "filter passed too many connecting PLines."
        withoutConnectingPLine = filter (/= oldConnectingPLine)

    -- Determine if the given INode has a PLine that points to an ENode.
    hasENode iNode = any (isJust . findENodeByOutput (eNodeSetOf $ slist initialGeneration)) $ insOf iNode
    -- Determine if the given INode has a PLine that points to another INode.
    hasINode iNode = any (isNothing . findENodeByOutput (eNodeSetOf $ slist initialGeneration)) $ insOf iNode

    -- Construct an ENodeSet
    eNodeSetOf :: Slist ENode -> ENodeSet
    eNodeSetOf eNodes
      | null eNodes = error "cannot construct an empty ENodeSet"
      | otherwise = ENodeSet (slist [(SL.head eNodes, SL.tail eNodes)])

    -- the number of generations.
    generationsIn :: [[INode]] -> Int
    generationsIn = len . slist

    -- how many input PLines does an INode have.
    inCountOf (INode _ _ moreIns _) = 2+len moreIns

    -- the only inode of an INodeSet. must have one generation only.
    onlyINodeOf :: INodeSet -> INode
    onlyINodeOf (INodeSet (Slist [[a]] _))
      | hasArc a && loop = errorIllegalLast
      | otherwise = a
    onlyINodeOf a = error
                    $ "not only inode!\n"
                    <> show a <> "\n"
                    <> show initialGeneration <> "\n"
                    <> show rawGenerations <> "\n"

    -- | Sort a generation by the first in PLine.
    sortGeneration :: [INode] -> [INode]
    sortGeneration = sortBy (\a b -> if firstInOf a `pLineIsLeft` firstInOf b == Just False then LT else GT)

    -- Find an inode connecting the first and last ENode, if it exists.
    -- FIXME: this functions, but i don't know why. :)
    flippedINodeOf :: [INode] -> Maybe INode
    flippedINodeOf inodes = case filter (\a -> firstPLine `pLineIsLeft` firstInOf a == Just False) inodes of
                              [] -> Nothing
                              [a] -> Just a
                              xs -> error
                                    $ "more than one flipped inode?" <> show xs <> "\n"
                                    <> show initialGeneration <> "\n"

    -- Return the first input to a given INode.
    firstInOf (INode firstIn _ _ _) = firstIn

    -- the output PLine of the first ENode in the input ENode set.
    firstPLine = outOf firstENode
      where
        -- the first ENode given to us. for sorting uses.
        firstENode = first initialGeneration
          where
            first :: [a] -> a
            first [] = error "no first enode?"
            first (a:_) = a
    -- | add together a child and it's parent.
    addINodeToParent :: INode -> INode -> INode
    addINodeToParent (INode _ _ _ Nothing) _ = error "cannot merge a child inode with no output!"
    addINodeToParent iNode1@(INode _ _ _ (Just out1)) iNode2@(INode _ _ _ out2) =
      orderInsByENodes $ makeINode (insOf iNode1 <> withoutPLine out1 (insOf iNode2)) out2
      where
        withoutPLine :: PLine2 -> [PLine2] -> [PLine2]
        withoutPLine myPLine = filter (/= myPLine)

    -- Order the input nodes of an INode.
    orderInsByENodes :: INode -> INode
    orderInsByENodes inode@(INode _ _ _ out) = makeINode (indexPLinesTo firstPLine $ sortedPLines $ indexPLinesTo firstPLine $ insOf inode) out

-- Produce a list of the inputs to a given INode.
insOf :: INode -> [PLine2]
insOf (INode firstIn secondIn (Slist moreIns _) _) = firstIn:secondIn:moreIns



-- | Apply a recursive algorithm to solve the node set.
--   FIXME: does not handle more than two point intersections of arcs properly.
skeletonOfNodes :: Bool -> [ENode] -> [INode] -> Either PartialNodes INodeSet
skeletonOfNodes loop eNodes iNodes =
  case eNodes of
    [] -> case iNodes of
            [] -> -- zero nodes == return emptyset. allows us to simplify our return loop.
              Right $ INodeSet $ slist []
            [INode {}] ->
              if loop
              then errorLen1 -- A one node loop makes no sense, reject.
              else Right $ INodeSet $ one iNodes -- just hand back single node requests.
            [iNode1,iNode2] -> handleTwoNodes iNode1 iNode2
            (_:_) -> handleThreeOrMoreNodes
    [eNode] -> case iNodes of
                 [] ->
                   if loop
                   then errorLen1 -- A one node loop makes no sense, reject.
                   else Right $ INodeSet $ one [eNodeToINode eNode] -- just hand back single node requests.
                 [iNode] -> handleTwoNodes eNode iNode
                 (_:_) -> handleThreeOrMoreNodes
    [eNode1,eNode2] -> case iNodes of
                         [] -> handleTwoNodes eNode1 eNode2
                         (_:_) -> handleThreeOrMoreNodes
    (_:_:_:_) -> handleThreeOrMoreNodes
  where
    errorLen1 = Left $ PartialNodes (INodeSet $ one iNodes) "NOMATCH - length 1?"
    --   Handle the the case of two nodes.
    handleTwoNodes :: (Arcable a, Pointable a, Show a, Arcable b, Pointable b, Show b) => a -> b -> Either PartialNodes INodeSet
    handleTwoNodes node1 node2
      | isCollinear (outOf node1) (outOf node2) = Left $ PartialNodes (INodeSet $ one iNodes) $ "cannot handle collinear nodes:\n" <> show node1 <> "\n" <> show node2 <> "\n"
      | nodesAreAntiCollinear node1 node2 && loop = Right $ INodeSet $ one [makeLastPair node1 node2]
      | loop = Right $ INodeSet $ one [makeINode (sortedPLines [outOf node1,outOf node2]) Nothing]
      | intersectsInPoint node1 node2 = Right $ INodeSet $ one [averageNodes node1 node2]
      | otherwise = errorLen2
      where
        errorLen2 = Left $ PartialNodes (INodeSet $ one iNodes) $ "NOMATCH - length 2?\n" <> show node1 <> "\n" <> show node2 <> "\n" <> show loop <> "\n" <> show eNodes <> "\n" <> show iNodes <> "\n"

    --   Handle the the case of 3 or more nodes.
    handleThreeOrMoreNodes
      | endsAtSamePoint = Right $ INodeSet $ one [makeINode (sortedPLines $ (outOf <$> eNodes) <> (outOf <$> iNodes)) Nothing]
      | hasShortestPair = Right $ INodeSet $ averageOfShortestPairs `cons` inodesOf (errorIfLeft (skeletonOfNodes loop remainingENodes (remainingINodes <> averageOfShortestPairs)))
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
                <> "resultingENodes: " <> show remainingENodes <> "\n"
                <> "resultingNodes: " <> show remainingINodes <> "\n"
                <> "thisGen: " <> show averageOfShortestPairs <> "\n"

    -- | When all of our nodes end in the same point and we know this is a closed loop, we should create a Node with all of them as input. This checks for that case.
    endsAtSamePoint :: Bool
    endsAtSamePoint = and $ mapWithFollower (\a b -> distanceBetweenPPoints a b < fudgeFactor) $ mapWithFollower intersectionOf ((outOf <$> nonAntiCollinearNodes eNodes (antiCollinearNodePairsOf eNodes)
                                                                                                                                   <> firstAntiCollinearNodes (antiCollinearNodePairsOf eNodes)) <>
                                                                                                                                 (outOf <$> nonAntiCollinearNodes iNodes (antiCollinearNodePairsOf iNodes)
                                                                                                                                   <> firstAntiCollinearNodes (antiCollinearNodePairsOf iNodes)))
      where
        -- since anti-collinear nodes end at the same point, only count one of them.
        firstAntiCollinearNodes nodePairs = fst <$> nodePairs
        -- find nodes that do not have an anti-collinear pair.
        -- FIXME: is there a better way do do this with Ord?
        nonAntiCollinearNodes :: (Eq a) => [a] -> [(a,a)] -> [a]
        nonAntiCollinearNodes myNodes nodePairs = filter (`notElem` allAntiCollinearNodes nodePairs) myNodes
          where
            allAntiCollinearNodes myNodePairs = (fst <$> myNodePairs) <> (snd <$> myNodePairs)

    -- | make sure we have a potential intersection between two nodes to work with.
    hasShortestPair :: Bool
    hasShortestPair = not $ null (intersectingNodePairsOf eNodes) && null (intersectingNodePairsOf iNodes) && null intersectingMixedNodePairs

    -- | construct the last pair of a closed concave region.
    makeLastPair :: (Arcable a, Arcable b) => a -> b -> INode
    makeLastPair node1 node2 = makeINode (sortedPair node1 node2) Nothing

    -- | determine the exterior nodes available for calculation during the next recurse.
    remainingENodes :: [ENode]
    remainingENodes = (if isSomething shortestMixedPairDistance && shortestPairDistance == shortestMixedPairDistance
                       then filter (`notElem` (fst <$> shortestMixedPairs))
                       else id) $
                      (if isSomething shortestEPairDistance && shortestPairDistance == shortestEPairDistance
                       then filter (`notElem` (fst <$> shortestPairs eNodes) <> (snd <$> shortestPairs eNodes))
                       else id) eNodes

    -- | determine the interior nodes available for calculation during the next recurse.
    remainingINodes :: [INode]
    remainingINodes = (if isSomething shortestMixedPairDistance && shortestPairDistance == shortestMixedPairDistance
                       then filter (`notElem` (snd <$> shortestMixedPairs))
                       else id) $
                      (if isSomething shortestIPairDistance && shortestPairDistance == shortestIPairDistance
                       then filter (`notElem` (fst <$> shortestPairs iNodes) <> (snd <$> shortestPairs iNodes))
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
    shortestPairs :: (Arcable a, Pointable a, Show a, Eq a) => [a] -> [(a, a)]
    shortestPairs myNodes = case nodePairsSortedByDistance myNodes of
                              [] -> []
                              [onePair] -> [onePair]
                              [(a,b),(c,d)] -> if a == c || a == d || b == c || b == d
                                               then [(a,b)]
                                               else [(a,b),(c,d)]
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
    antiCollinearNodePairsOf :: (Pointable a, Arcable a) => [a] -> [(a, a)]
    antiCollinearNodePairsOf inNodes = catMaybes $ (\(node1, node2) -> if nodesAreAntiCollinear node1 node2 then Just (node1, node2) else Nothing) <$> getPairs inNodes

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
