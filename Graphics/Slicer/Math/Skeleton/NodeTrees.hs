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

{- Purpose of this file: to hold utility functions for working with NodeTrees. -}

module Graphics.Slicer.Math.Skeleton.NodeTrees (firstENodeOf, firstSegOf, lastENodeOf, lastSegOf, pathFirst, pathLast, findENodeByOutput, sortNodeTrees, makeNodeTree) where

import Prelude (Bool(True,False), Ordering(LT,GT), (==), fst, otherwise, snd, ($), error, (<>), show, (<>), head, init, null)

import Prelude as P (filter, last)

import Data.List (sortBy)

import Data.Maybe( Maybe(Just, Nothing), fromJust, isJust)

import Slist.Type (Slist(Slist))

import Slist (slist, cons)

import Slist as SL (filter, last)

import Graphics.Slicer.Math.Line (LineSeg)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), INode(INode), ENodeList(ENodeList), NodeTree(NodeTree), Arcable(hasArc, outOf), finalINodeOf)

import Graphics.Slicer.Math.PGA (PLine2, pLineIsLeft)

lastSegOf :: NodeTree -> LineSeg
lastSegOf nodeTree = (\(ENode (_,outSeg) _) -> outSeg) (lastENodeOf nodeTree)

firstSegOf :: NodeTree -> LineSeg
firstSegOf nodeTree = (\(ENode (outSeg,_) _) -> outSeg) (firstENodeOf nodeTree)

lastENodeOf :: NodeTree -> ENode
lastENodeOf nodeTree = (\(_,_,c) -> c) $ pathLast nodeTree

firstENodeOf :: NodeTree -> ENode
firstENodeOf nodeTree = (\(_,_,c) -> c) $ pathFirst nodeTree

-- whether to follow the first or the last node in a branch, when branching in pathTo.
data Direction = Head
               | Last

pathFirst, pathLast :: NodeTree -> ([PLine2], [INode], ENode)
pathFirst nodeTree = pathTo nodeTree Head
pathLast nodeTree = pathTo nodeTree Last

-- | Find all of the Nodes and all of the arcs between the last item in the nodeTree and the node that is part of the original contour on the given side.
pathTo :: NodeTree -> Direction -> ([PLine2], [INode], ENode)
pathTo nodeTree@(NodeTree eNodeList@(ENodeList firstENode _) iNodeSets) direction
  | null iNodeSets = ([outOf firstENode], [], firstENode)
  | otherwise = pathInner (init iNodeSets) eNodeList (finalINodeOf nodeTree)
  where
    pathInner :: [[INode]] -> ENodeList -> INode -> ([PLine2], [INode], ENode)
    pathInner myINodeSets myENodeList target@(INode firstPLine secondPLine morePLines _)
      | hasArc target = (outOf target : childPlines, target: endNodes, finalENode)
      | otherwise     = (               childPlines, target: endNodes, finalENode)
      where
        pLineToFollow = case direction of
                          Head -> firstPLine
                          Last -> SL.last (cons secondPLine morePLines)
        iNodeOnThisLevel = findINodeByOutput myINodeSets pLineToFollow False
        iNodeOnLowerLevel = findINodeByOutput (init myINodeSets) pLineToFollow True
        result = findENodeByOutput myENodeList pLineToFollow
        terminate = ([outOf $ fromJust result], [], fromJust result)
        myError = error $ "could not find enode for " <> show pLineToFollow <> "\n" <> show eNodeList <> "\n" <> show myINodeSets <> "\n"
        (childPlines, endNodes, finalENode) = if isJust result
                                              then terminate
                                              else case iNodeOnThisLevel of
                                                     (Just res) -> pathInner myINodeSets myENodeList (snd res)
                                                     Nothing -> case myINodeSets of
                                                                  [] -> myError
                                                                  [_] -> myError
                                                                  (_:_) ->  case iNodeOnLowerLevel of
                                                                              (Just res) -> pathInner (init $ fst res) myENodeList (snd res)
                                                                              Nothing -> myError

-- | Find an exterior Node with an output of the PLine given.
findENodeByOutput :: ENodeList -> PLine2 -> Maybe ENode
findENodeByOutput (ENodeList firstENode moreENodes) plineOut = case nodesMatching of
                                                                 (Slist [] _) -> Nothing
                                                                 (Slist [oneNode] _) -> Just oneNode
                                                                 (Slist (_:_) _)->  error "more than one exterior node with the same PLine out!"
  where
    nodesMatching = SL.filter (\(ENode _ a) -> a == plineOut) (cons firstENode moreENodes)

-- Sort a set of nodeTrees. they should come out in order, so that the last segment of a preceeding NodeTree stops at the first segment of the current NodeTree
sortNodeTrees :: [NodeTree] -> [NodeTree]
sortNodeTrees = sortBy compareNodeTrees
  where
    compareNodeTrees nt1 nt2 = if outOf (lastENodeOf nt1) `pLineIsLeft` outOf (firstENodeOf nt2) == Just True
                               then LT
                               else GT

-- dependent utility functions. used by internal components. not exported.

-- | Find a node with an output of the PLine given. Start at the most recent generation, and check backwards.
findINodeByOutput :: [[INode]] -> PLine2 -> Bool -> Maybe ([[INode]],INode)
findINodeByOutput iNodeSets plineOut recurse
  | null iNodeSets = Nothing
  | otherwise = case nodesMatching of
                  [] -> if recurse
                        then case iNodeSets of
                               [] -> Nothing
                               [_] -> Nothing
                               (_:_) -> findINodeByOutput (init iNodeSets) plineOut recurse
                        else Nothing
                  [_] -> Just (iNodeSets, head nodesMatching)
                  (_:_) -> error "more than one node in a given generation with the same PLine out!"
  where
    nodesMatching = P.filter (\(INode _ _ _ a) -> a == Just plineOut) (P.last iNodeSets)

-- | a smart constructor for a NodeTree
makeNodeTree :: [ENode] -> [[INode]] -> NodeTree
makeNodeTree eNodes iNodeSets = case eNodes of
                                  [] -> error "not enough nodes to make a nodeTree"
                                  [eNode] -> NodeTree (ENodeList eNode (slist [])) iNodeSets
                                  (eNode:moreENodes) -> NodeTree (ENodeList eNode (slist moreENodes)) iNodeSets
