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

module Graphics.Slicer.Math.Skeleton.NodeTrees (firstENodeOf, firstSegOf, lastENodeOf, lastSegOf, pathFirst, pathLast, findENodeByOutput, sortNodeTrees) where

import Prelude (Bool(True), Ordering(LT,GT), (==), fst, otherwise, snd, ($), length, error, (<>), show, (<>), (>), head, (&&), filter, init, null, last)

import Data.List (sortBy)

import Data.Maybe( Maybe(Just, Nothing), fromJust, isJust)

import Graphics.Slicer.Math.Line (LineSeg)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), INode(INode), NodeTree(NodeTree), Arcable(hasArc, outOf), finalINodeOf)

import Graphics.Slicer.Math.PGA (PLine2, pLineIsLeft)

lastSegOf :: NodeTree -> LineSeg
lastSegOf nodeTree = (\(ENode (_,outSeg) _) -> outSeg) (lastENodeOf nodeTree)

firstSegOf :: NodeTree -> LineSeg
firstSegOf nodeTree = (\(ENode (outSeg,_) _) -> outSeg) (firstENodeOf nodeTree)

lastENodeOf :: NodeTree -> ENode
lastENodeOf nodeTree = (\(_,_,c) -> c) $ pathLast nodeTree

firstENodeOf :: NodeTree -> ENode
firstENodeOf nodeTree = (\(_,_,c) -> c) $ pathFirst nodeTree

-- dependent utility functions. used by last and first segment finder, and internal components.
-- FIXME: should lastSegOf, firstSegOf, and the below stuff be in a different place?

-- FIXME: merge pathFirst and pathLast. they differ by only one line.
-- | Find all of the Nodes and all of the arcs between the last of the nodeTree and the node that is part of the original contour.
--   When branching, follow the last PLine in a given node.
pathFirst :: NodeTree -> ([PLine2], [INode], ENode)
pathFirst nodeTree@(NodeTree eNodes iNodeSets)
  | null iNodeSets  = ([outOf (last eNodes)], [], last eNodes)
  | otherwise = pathFirstInner (init iNodeSets) eNodes (finalINodeOf nodeTree)
  where
    pathFirstInner :: [[INode]] -> [ENode] -> INode -> ([PLine2], [INode], ENode)
    pathFirstInner myINodeSets myENodes target@(INode (plinesIn) _)
      | hasArc target = (outOf target : childPlines, target: endNodes, finalENode)
      | otherwise     = (               childPlines, target: endNodes, finalENode)
      where
        pLineToFollow = head plinesIn
        iNodeOnThisLevel = findINodeByOutput myINodeSets pLineToFollow
        iNodeOnLowerLevel = findINodeByOutput (init myINodeSets) pLineToFollow
        result = findENodeByOutput myENodes pLineToFollow
        terminate = ([outOf $ fromJust $ result], [], fromJust $ result)
        myError = error $ "could not find enode for " <> show pLineToFollow <> "\n" <> show eNodes <> "\n" <> show myINodeSets <> "\n"
        (childPlines, endNodes, finalENode)
          | null myINodeSets &&
            isJust result            = terminate
          -- nothing left to do.
          | null myINodeSets         = myError
          -- cannot happen.
          | length myINodeSets == 1 &&
            isJust iNodeOnThisLevel &&
            isJust result            = myError
          | length myINodeSets == 1 &&
            isJust result            = terminate
          | isJust iNodeOnThisLevel  = pathFirstInner myINodeSets myENodes (snd $ fromJust iNodeOnThisLevel)
          -- nothing left to do.
          | length myINodeSets == 1  = myError
          | isJust iNodeOnLowerLevel = pathFirstInner (init $ fst $ fromJust iNodeOnLowerLevel) myENodes (snd $ fromJust iNodeOnLowerLevel)
          | isJust result            = terminate
          -- cannot happen
          | otherwise                = myError


-- | Find all of the Nodes and all of the arcs between the last of the nodeTree and the node that is part of the original contour.
--   When branching, follow the last PLine in a given node.
pathLast :: NodeTree -> ([PLine2], [INode], ENode)
pathLast nodeTree@(NodeTree eNodes iNodeSets)
  | null iNodeSets  = ([outOf (last eNodes)], [], last eNodes)
  | otherwise = pathLastInner (init iNodeSets) eNodes (finalINodeOf nodeTree)
  where
    pathLastInner :: [[INode]] -> [ENode] -> INode -> ([PLine2], [INode], ENode)
    pathLastInner myINodeSets myENodes target@(INode (plinesIn) _)
      | hasArc target = (outOf target : childPlines, target: endNodes, finalENode)
      | otherwise     = (               childPlines, target: endNodes, finalENode)
      where
        pLineToFollow = last plinesIn
        iNodeOnThisLevel = findINodeByOutput myINodeSets pLineToFollow
        iNodeOnLowerLevel = findINodeByOutput (init myINodeSets) pLineToFollow
        result = findENodeByOutput myENodes pLineToFollow
        terminate = ([outOf $ fromJust $ result], [], fromJust $ result)
        myError = error $ "could not find enode for " <> show pLineToFollow <> "\n" <> show eNodes <> "\n" <> show myINodeSets <> "\n"
        (childPlines, endNodes, finalENode)
          | null myINodeSets &&
            isJust result            = terminate
          -- nothing left to do.
          | null myINodeSets         = myError
          -- cannot happen.
          | length myINodeSets == 1 &&
            isJust iNodeOnThisLevel &&
            isJust result            = myError
          | length myINodeSets == 1 &&
            isJust result            = terminate
          | isJust iNodeOnThisLevel  = pathLastInner myINodeSets myENodes (snd $ fromJust iNodeOnThisLevel)
          -- nothing left to do.
          | length myINodeSets == 1  = myError
          | isJust iNodeOnLowerLevel = pathLastInner (init $ fst $ fromJust iNodeOnLowerLevel) myENodes (snd $ fromJust iNodeOnLowerLevel)
          | isJust result            = terminate
          -- cannot happen
          | otherwise                = myError

-- | Find a node with an output of the PLine given. Start at the most recent generation, and check backwards.
findINodeByOutput :: [[INode]] -> PLine2 -> Maybe ([[INode]],INode)
findINodeByOutput iNodeSets plineOut
  | null iNodeSets            = error "could not find inode. empty set?"
  | length nodesMatching == 1 = Just $ (iNodeSets, head nodesMatching)
  | length iNodeSets > 1 &&
    null nodesMatching        = findINodeByOutput (init iNodeSets) plineOut
  | null nodesMatching        = Nothing
  | otherwise                 = error "more than one node in a given generation with the same PLine out!"
  where
    nodesMatching = filter (\(INode _ a) -> a == Just plineOut) (last iNodeSets)

-- | Find an exterior Node with an output of the PLine given.
findENodeByOutput :: [ENode] -> PLine2 -> Maybe ENode
findENodeByOutput eNodes plineOut
  | null eNodes               = error "could not find enode. empty set?"
  | length nodesMatching == 1 = Just $ head nodesMatching
  | null nodesMatching        = Nothing
  | otherwise                 = error "more than one exterior node with the same PLine out!"
  where
    nodesMatching = filter (\(ENode _ a) -> a == plineOut) eNodes

-- Sort a set of nodeTrees. they should come out in order, so that the last segment of a preceeding NodeTree stops at the first segment of the current NodeTree
sortNodeTrees :: [NodeTree] -> [NodeTree]
sortNodeTrees nodeTrees = sortBy compareNodeTrees nodeTrees
          where
            compareNodeTrees nt1 nt2
              | Just True == outOf (lastENodeOf nt1) `pLineIsLeft` outOf (firstENodeOf nt2) = LT
              | otherwise                                                                   = GT
