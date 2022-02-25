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

-- | utility functions for working with NodeTrees.
module Graphics.Slicer.Math.Skeleton.NodeTrees (firstENodeOf, firstSegOf, lastENodeOf, lastSegOf, pathFirst, pathLast, findENodeByOutput, findINodeByOutput, makeNodeTree, mergeNodeTrees) where

import Prelude (Bool(True,False), Eq, Show, (==), otherwise, snd, ($), error, (<>), notElem, show, (&&), (/=), null, (<$>), fst)

import Data.Maybe( Maybe(Just, Nothing), fromMaybe, isJust)

import Slist.Type (Slist(Slist))

import Slist (cons, len, slist)

import Slist as SL (filter, last, head, init, isEmpty)

import Graphics.Slicer.Math.Definitions (LineSeg)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode, INode(INode), ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), Arcable(hasArc, outOf), finalINodeOf, finalPLine, getFirstLineSeg, getLastLineSeg, hasNoINodes, ancestorsOf, indexPLinesTo, makeINode, sortedPLines)

import Graphics.Slicer.Math.PGA (PLine2)

lastSegOf :: NodeTree -> LineSeg
lastSegOf nodeTree = getLastLineSeg $ lastENodeOf nodeTree

firstSegOf :: NodeTree -> LineSeg
firstSegOf nodeTree = getFirstLineSeg $ firstENodeOf nodeTree

lastENodeOf :: NodeTree -> ENode
lastENodeOf (NodeTree (ENodeSet sides) _) = if null $ snd res
                                            then fst res
                                            else SL.last $ snd res
  where
    res = SL.last sides

firstENodeOf :: NodeTree -> ENode
firstENodeOf (NodeTree (ENodeSet sides) _) = fst $ SL.head sides

-- | whether to follow the first or the last node in a branch, when branching in pathTo.
data Direction = Head
               | Last

pathFirst, pathLast :: NodeTree -> ([PLine2], [INode], ENode)
pathFirst nodeTree = pathTo nodeTree Head
pathLast nodeTree = pathTo nodeTree Last

-- | Find all of the Nodes and all of the arcs between the last item in the nodeTree and the node that is part of the original contour on the given side.
pathTo :: NodeTree -> Direction -> ([PLine2], [INode], ENode)
pathTo (NodeTree (ENodeSet (Slist [] _)) _) _ = error "unable to pathTo a Nodetree without ENodes."
pathTo (NodeTree eNodeSet@(ENodeSet eNodeSides) iNodeSet@(INodeSet generations)) direction
  | isEmpty generations = case eNodeSides of
                            (Slist [] _) -> error "looking for a first ENode in an empty ENodeSet."
                            (Slist [(firstENode,_)] _) -> ([outOf firstENode], [], firstENode)
                            (Slist _ _) -> error "looking for a first ENode in an ENodeSet with more than one side."
  | otherwise = pathInner (ancestorsOf iNodeSet) eNodeSet (finalINodeOf iNodeSet)
  where
    pathInner :: INodeSet -> ENodeSet -> INode -> ([PLine2], [INode], ENode)
    pathInner myINodeSet@(INodeSet myGenerations) myENodeSet target@(INode firstPLine secondPLine morePLines _)
      | hasArc target = (outOf target : childPlines, target: endNodes, finalENode)
      | otherwise     = (               childPlines, target: endNodes, finalENode)
      where
        pLineToFollow = case direction of
                          Head -> firstPLine
                          Last -> SL.last (cons secondPLine morePLines)
        iNodeOnThisLevel = findINodeByOutput myINodeSet pLineToFollow False
        iNodeOnLowerLevel = findINodeByOutput (ancestorsOf myINodeSet) pLineToFollow True
        result = findENodeByOutput myENodeSet pLineToFollow
        terminate = case result of
                      (Just eNode) -> ([outOf eNode], [], eNode)
                      Nothing -> error "FIXME: cannot happen."
        myError = error $ "could not find enode for " <> show pLineToFollow <> "\n" <> show eNodeSides <> "\n" <> show myINodeSet <> "\n"
        (childPlines, endNodes, finalENode) = if isJust result
                                              then terminate
                                              else case iNodeOnThisLevel of
                                                     (Just res) -> pathInner myINodeSet myENodeSet (snd res)
                                                     Nothing -> case myGenerations of
                                                                  (Slist [] _) -> myError
                                                                  (Slist ([]:_) _) -> myError
                                                                  (Slist [INode {} :_] _) -> myError
                                                                  (Slist ((INode {} :_):_) _) ->  case iNodeOnLowerLevel of
                                                                                        (Just (resINodeSet,resINode)) -> pathInner (ancestorsOf resINodeSet) myENodeSet resINode
                                                                                        Nothing -> myError

-- | Find an exterior Node with an output of the PLine given.
findENodeByOutput :: ENodeSet -> PLine2 -> Maybe ENode
findENodeByOutput (ENodeSet eNodeSides) plineOut =
  case eNodeSides of
    (Slist [] _) -> Nothing
    (Slist [oneSide] _) -> findENodeOnSideByOutput oneSide plineOut
    (Slist sides _) -> case res of
                         [] -> Nothing
                         [a] -> a
                         (_:_) -> error "more than one eNode found?"
      where res = (`findENodeOnSideByOutput` plineOut) <$> sides
  where
  findENodeOnSideByOutput :: (ENode,Slist ENode) -> PLine2 -> Maybe ENode
  findENodeOnSideByOutput (firstENode,moreENodes) myPlineOut = case nodesMatching of
                                                                 (Slist [] _) -> Nothing
                                                                 (Slist [oneNode] _) -> Just oneNode
                                                                 (Slist (_:_) _)->  error "more than one exterior node with the same PLine out!"
    where
      nodesMatching = SL.filter (\eNode -> outOf eNode == myPlineOut) (cons firstENode moreENodes)

-----------------------------------------------------------------------------
-- dependent utility functions. used by internal components. not exported. --
-----------------------------------------------------------------------------

-- | Find an INode that has an output of the PLine given. Check the most recent generation, and if recurse is set, check previous generations.
--   Also returns the generation the INode was found in, it's generation, and it's prior generations.
findINodeByOutput :: INodeSet -> PLine2 -> Bool -> Maybe (INodeSet, INode)
findINodeByOutput iNodeSet@(INodeSet generations) plineOut recurse
  | isEmpty generations = Nothing
  | otherwise = case nodesMatching of
                  (Slist [] _) -> if recurse
                                  then case generations of
                                         (Slist [] _) -> Nothing
                                         (Slist [INode {} :_] _) -> Nothing
                                         (Slist (_:_) _) -> findINodeByOutput (ancestorsOf iNodeSet) plineOut recurse
                                  else Nothing
                  (Slist [iNode] _) -> Just (iNodeSet, iNode)
                  (Slist (_:_) _) -> error "more than one node in a the same generation with the same PLine out!"
  where
    nodesMatching = SL.filter (\(INode _ _ _ a) -> a == Just plineOut) $ slist (SL.last generations)

-- | a smart constructor for a NodeTree
makeNodeTree :: [ENode] -> INodeSet -> NodeTree
makeNodeTree eNodes iNodeSet = case eNodes of
                                  [] -> NodeTree (ENodeSet (slist [])) iNodeSet
                                  [eNode] -> NodeTree (ENodeSet (slist [(eNode,slist [])])) iNodeSet
                                  (eNode:moreENodes) -> NodeTree (ENodeSet (slist [(eNode,slist moreENodes)])) iNodeSet

-- | Merge a set of nodetrees together.
mergeNodeTrees :: [NodeTree] -> NodeTree
mergeNodeTrees nodeTrees =
  case nodeTrees of
    [] -> error "no nodeTrees to merge"
    [a] -> a
    [a,b] -> fromMaybe (error "could not merge") $ mergeTwoNodeTrees a b
    (a:bs) -> fromMaybe (error "could not merge") $ mergeTwoNodeTrees a $ mergeNodeTrees bs
-- mergeNodeTrees nodeTrees = foldt mergeTwoNodeTrees (NodeTree (ENodeSet (slist [])) (INodeSet (slist []))) nodeTrees
  where
    mergeTwoNodeTrees :: NodeTree -> NodeTree -> Maybe NodeTree
    mergeTwoNodeTrees nodeTree1@(NodeTree eNodeSet1 _) nodeTree2@(NodeTree eNodeSet2 _) =
      Just $ NodeTree mergedENodeSets mergedINodeSets
      where
        mergedENodeSets = mergeENodeSets eNodeSet1 eNodeSet2
        mergedINodeSets = mergeINodeSets nodeTree1 nodeTree2
    mergeINodeSets :: NodeTree -> NodeTree -> INodeSet
    mergeINodeSets myNodeTree1@(NodeTree eNodeSet1 _) myNodeTree2@(NodeTree eNodeSet2 _)
      | isOneSide eNodeSet1 && isOneSide eNodeSet2 = addOneSidedINodeSets myNodeTree1 myNodeTree2
      | otherwise = error "make me."
    addOneSidedINodeSets :: NodeTree -> NodeTree -> INodeSet
    addOneSidedINodeSets nt1@(NodeTree _ iNodeSet1@(INodeSet rawINodeSet1)) nt2@(NodeTree _ iNodeSet2@(INodeSet rawINodeSet2))
      | hasNoINodes iNodeSet1 && hasNoINodes iNodeSet2 = error $ show $ nodeTreesInOrder nt1 nt2
      | hasNoINodes iNodeSet1 && hasArc (finalINodeOf iNodeSet2) = INodeSet $ rawINodeSet2 <> slist (nodeTreesInOrder nt1 nt2)
      | hasNoINodes iNodeSet1 = INodeSet $ SL.init rawINodeSet2 <> slist (nodeTreeAndInsInOrder nt1 nt2)
      | hasNoINodes iNodeSet2 && hasArc (finalINodeOf iNodeSet1) = INodeSet $ rawINodeSet1 <> slist (nodeTreesInOrder nt2 nt1)
      | hasNoINodes iNodeSet2 = INodeSet $ SL.init rawINodeSet1 <> slist (nodeTreeAndInsInOrder nt2 nt1)
      | hasArc (finalINodeOf iNodeSet1) && hasArc (finalINodeOf iNodeSet2) = error $ show (mergeINodeSetsInOrder nt1 nt2) <> "\n" <> show (nodeTreesInOrder nt1 nt2) <> "\n"
      | hasArc (finalINodeOf iNodeSet1) = INodeSet $ slist $ mergeAncestorAndINodeSetInOrder nt1 nt2 <> nodeTreeAndInsInOrder nt1 nt2
      | hasArc (finalINodeOf iNodeSet2) = error $ show (nodeTreeAndInsInOrder nt2 nt1) <> "\n" <> show (insOf $ finalINodeOf iNodeSet1) <> "\n"
      | otherwise = error $ "cannot merge two NodeTrees without outputs.\n" <> show nt1 <> "\n" <> show nt2 <> "\n"
      where
        nodeTreesInOrder myNodeTree1@(NodeTree myENodeSet1 _) myNodeTree2@(NodeTree myENodeSet2 _)
          | isOneSide myENodeSet1 && isOneSide myENodeSet2 = case compareSides (firstSide myENodeSet1) (firstSide myENodeSet2) of
                                                               FirstLast -> [[makeINode [finalPLine myNodeTree1, finalPLine myNodeTree2] Nothing]]
                                                               LastFirst -> [[makeINode [finalPLine myNodeTree2, finalPLine myNodeTree1] Nothing]]
                                                               NoMatch -> error "failed to connect"
          | otherwise = error "multi-sided ENodeSets not yet supported."
        nodeTreeAndInsInOrder myNodeTree1@(NodeTree myENodeSet1 _) myNodeTree2@(NodeTree myENodeSet2 _)
          | isOneSide myENodeSet1 && isOneSide myENodeSet2 = case compareSides (firstSide myENodeSet1) (firstSide myENodeSet2) of
                                                               FirstLast -> [[makeINode (indexPLinesTo (outOf $ firstENodeOf myNodeTree2) $ sortedPLines $ insOf (finalINodeOf iNodeSet2) <> [finalPLine myNodeTree1]) Nothing]]
                                                               LastFirst -> [[makeINode (indexPLinesTo (outOf $ firstENodeOf myNodeTree1) $ sortedPLines $ finalPLine myNodeTree1 : insOf (finalINodeOf iNodeSet2)) Nothing]]
                                                               NoMatch -> error "failed to connect"
          | otherwise = error "multi-sided ENodeSets not yet supported."
        mergeINodeSetsInOrder (NodeTree myENodeSet1 myINodeSet1) (NodeTree myENodeSet2 myINodeSet2)
          | isOneSide myENodeSet1 && isOneSide myENodeSet2 = case compareSides (firstSide myENodeSet1) (firstSide myENodeSet2) of
                                                               FirstLast -> rawMergeINodeSets myINodeSet1 myINodeSet2
                                                               LastFirst -> rawMergeINodeSets myINodeSet2 myINodeSet1
                                                               NoMatch -> error "failed to connect"
          | otherwise = error "multi-sided ENodeSets not yet supported."
        mergeAncestorAndINodeSetInOrder (NodeTree myENodeSet1 myINodeSet1) (NodeTree myENodeSet2 myINodeSet2)
          | isOneSide myENodeSet1 && isOneSide myENodeSet2 = case compareSides (firstSide myENodeSet1) (firstSide myENodeSet2) of
                                                               FirstLast -> rawMergeINodeSets (ancestorsOf myINodeSet2) myINodeSet1
                                                               LastFirst -> rawMergeINodeSets (ancestorsOf myINodeSet1) myINodeSet2
                                                               NoMatch -> error "failed to connect"
          | otherwise = error "multi-sided ENodeSets not yet supported."
        insOf (INode a b (Slist cs _) _) = a:b:cs
        rawMergeINodeSets (INodeSet myINodeSet1) (INodeSet myINodeSet2) = mergeINodeSetsInner myINodeSet1 myINodeSet2
          where
            mergeINodeSetsInner :: Slist [INode] -> Slist [INode] -> [[INode]]
            mergeINodeSetsInner set1@(Slist rawSet1 _) set2@(Slist rawSet2 _)
              | null rawSet1 && null rawSet2 = []
              | null rawSet1 = rawSet2
              | null rawSet2 = rawSet1
              | null (SL.init set1) && null (SL.init set2) = [SL.last set1 <> SL.last set2]
              | otherwise = mergeINodeSetsInner (SL.init set1) (SL.init set2) <> [SL.last set1 <> SL.last set2]
    mergeENodeSets :: ENodeSet -> ENodeSet -> ENodeSet
    mergeENodeSets myENodeSet1@(ENodeSet sides1) myENodeSet2@(ENodeSet sides2)
      | len sides1 == 0 && len sides2 == 0 = myENodeSet1
      | len sides1 /= 0 && len sides2 == 0 = myENodeSet1
      | len sides1 == 0 && len sides2 /= 0 = myENodeSet2
      | isOneSide myENodeSet1 = addENodeSetToOneSide myENodeSet2 myENodeSet1
      | isOneSide myENodeSet2 = addENodeSetToOneSide myENodeSet1 myENodeSet2
      | otherwise = error "unsure how to merge."
      where
        addENodeSetToOneSide :: ENodeSet -> ENodeSet -> ENodeSet
        addENodeSetToOneSide _ (ENodeSet (Slist [] _)) = error "empty set?"
        addENodeSetToOneSide _ (ENodeSet (Slist (_:_:_) _)) = error "too many sides?"
        addENodeSetToOneSide myENodeSet@(ENodeSet sides) (ENodeSet (Slist [oneSide] _)) = ENodeSet resultSides
          where
            resultSides = cons newSide $ SL.filter (`notElem` matchSides) sides
            -- can match 1 or 2 times.
            matchSides :: [(ENode, Slist ENode)]
            matchSides
              | isEmpty matchHead && isEmpty matchTail = error $ "no way to connect:\n" <> show oneSide <> "\n" <> show sides <> "\n" <> show (compareSides oneSide (lastSide myENodeSet)) <> "\n" <> show nodeTrees <> "\n"
              | isEmpty matchHead = [SL.head matchTail]
              | isEmpty matchTail = [SL.head matchHead]
              | matchHead == matchTail = [SL.head matchHead]
              | otherwise = SL.head matchHead : [SL.head matchTail]
            -- construct a new side, including matchSides and the oneSide given.
            newSide :: (ENode, Slist ENode)
            newSide
              | isEmpty matchHead && isEmpty matchTail = error $ "no way to connect\n" <> show oneSide <> "\n" <> show sides <> "\n" <> show (compareSides oneSide (lastSide myENodeSet)) <> "\n"
              | isEmpty matchHead = makeSide $ eNodesOfSide oneSide <> eNodesOfSide (SL.head matchTail)
              | isEmpty matchTail = makeSide $ eNodesOfSide (SL.head matchHead) <> eNodesOfSide oneSide
              | matchHead == matchTail = makeSide $ eNodesOfSide (SL.head matchHead) <> eNodesOfSide oneSide
              | otherwise = makeSide $ eNodesOfSide (SL.head matchHead) <> eNodesOfSide oneSide <> eNodesOfSide (SL.head matchTail)
            -- find a side that ends where our oneSide begins
            matchHead = SL.filter (\given -> compareSides given oneSide == FirstLast) sides
            -- find a side that begins where our oneSide ends
            matchTail = SL.filter (\given -> compareSides given oneSide == LastFirst) sides
            eNodesOfSide :: (ENode, Slist ENode) -> [ENode]
            eNodesOfSide (first,Slist more _) = first : more
            makeSide :: [ENode] -> (ENode, Slist ENode)
            makeSide [] = error "cannot make an empty side."
            makeSide (a:bs) = (a, slist bs)
    isOneSide :: ENodeSet -> Bool
    isOneSide (ENodeSet sides) = len sides == 1
    compareSides :: (ENode, Slist ENode) -> (ENode, Slist ENode) -> MaybeMatch
    compareSides side1 side2
      | checkForFollower (lastOfSide side1) (firstOfSide side2) = FirstLast
      | checkForFollower (lastOfSide side2) (firstOfSide side1) = LastFirst
      | otherwise = NoMatch
    checkForFollower eNode1 eNode2 = getLastLineSeg eNode1 == getFirstLineSeg eNode2
    firstSide (ENodeSet sides)
      | len sides == 1 = head sides
      | otherwise = error "unexplored territory"
    lastSide (ENodeSet sides)
      | len sides /= 0 = SL.last sides
      | otherwise = error "unexplored territory"
    firstOfSide (firstSeg, _) = firstSeg
    lastOfSide (first,Slist [] _) = first
    lastOfSide (_, moreENodes) = SL.last moreENodes

-- the result of comparing two sides, and seeing if they follow each other.
data MaybeMatch = FirstLast
                | LastFirst
                | NoMatch
  deriving (Eq, Show)
