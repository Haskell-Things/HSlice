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

-- inherit instances when deriving.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

{-
 - This file contains two things that should probably be in separate files:
 - Code for creating a series of faces, covering a straight skeleton.
 - Code for taking a series of faces, and applying inset line segments and infill to them.
 -}
module Graphics.Slicer.Math.Skeleton.Face (Face(Face), orderedFacesOf, facesOf, lastSegOf, firstSegOf) where

import Prelude ((==), otherwise, (<$>), ($), (.), length, (/=), error, (<>), show, Eq, Show, (<>), (++), (>), Bool, head, (&&), (||), take, filter, init, null, tail, last, concat, not, reverse)

import Data.List (dropWhile)

import Data.Maybe( Maybe(Just, Nothing), isNothing, fromJust)

import Graphics.Slicer.Math.Definitions (mapWithFollower)

import Graphics.Slicer.Math.Line (LineSeg)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode(ENode), INode(INode), NodeTree(NodeTree), Arcable(hasArc, outOf), finalINodeOf, finalOutOf)

import Graphics.Slicer.Math.PGA (PLine2)

--------------------------------------------------------------------
-------------------------- Face Placement --------------------------
--------------------------------------------------------------------

-- | A Face:
--   A portion of a contour, with a real side, and arcs (line segments between nodes) dividing it from other faces.
--   Faces have no holes, and their arcs and nodes (line segments and points) are generated from a StraightSkeleton of a Contour.
data Face = Face { _edge :: LineSeg, _firstArc :: PLine2, _arcs :: [PLine2], _lastArc :: PLine2 }
  deriving Eq
  deriving stock Show

-- | take a straight skeleton, and create faces from it.
-- accepts a line segment you want the first face to contain, and reorders the face list.
orderedFacesOf :: LineSeg -> StraightSkeleton -> [Face]
orderedFacesOf start skeleton = facesFromIndex start $ facesOf skeleton
  where
    facesFromIndex :: LineSeg -> [Face] -> [Face]
    facesFromIndex targetSeg rawFaces = take (length rawFaces) $ dropWhile (\(Face a _ _ _) -> a /= targetSeg) $ rawFaces ++ rawFaces

-- | take a straight skeleton, and create faces from it.
facesOf :: StraightSkeleton -> [Face]
facesOf (StraightSkeleton nodeLists spine)
  | null spine && length nodeLists == 1 = findFaces (head nodeLists)
  | otherwise                           = error "cannot yet handle spines, or more than one NodeList."
  where
    -- find all of the faces of a set of nodeTrees.
    findFaces :: [NodeTree] -> [Face]
    findFaces nodeTrees
      | null nodeTrees = []
      | length nodeTrees == 1 && isNothing (finalOutOf $ head nodeTrees) = rawFaces
      | length nodeTrees  > 1                                            = rawFaces
      | otherwise            = error $ "abandon hope!\n" <> show (length nodeLists) <> "\n" <> show nodeLists <> "\n" <> show (length nodeTrees) <> "\n" <> show nodeTrees <> "\n" <> show rawFaces <> "\n"
      where
        rawFaces = findFacesRecurse nodeTrees ++
                   if length nodeTrees > 1 then [intraNodeFace (last nodeTrees) (head nodeTrees)] else []
        -- Recursively find faces.
        findFacesRecurse :: [NodeTree] -> [Face]
        findFacesRecurse []               = []
        findFacesRecurse [tree1]          = facesOfNodeTree tree1
        findFacesRecurse [tree1,tree2]    = facesOfNodeTree tree2 ++ (intraNodeFace tree1 tree2 : facesOfNodeTree tree1)
        findFacesRecurse (tree1:tree2:xs) = findFacesRecurse (tree2:xs) ++ (intraNodeFace tree1 tree2 : facesOfNodeTree tree1)
        -- Create a single face for the space between two NodeTrees. like areaBetween, but for two separate NodeTrees.
        intraNodeFace :: NodeTree -> NodeTree -> Face
        intraNodeFace nodeTree1 nodeTree2
          | nodeTree1 == nodeTree2          = error $ "two identical nodes given.\n" <> show nodeTree1 <> "\n"
          | nodeTree1 `isRightOf` nodeTree2 = if last (firstPLinesOf nodeTree2) == last (lastPLinesOf nodeTree1)
                                              then makeFace (lastENodeOf nodeTree2) (init (firstPLinesOf nodeTree2) ++ tail (reverse $ init $ lastPLinesOf nodeTree1)) (firstENodeOf nodeTree1)
                                              else makeFace (lastENodeOf nodeTree2) (init (firstPLinesOf nodeTree2) ++       reverse  (init $ lastPLinesOf nodeTree1)) (firstENodeOf nodeTree1)
          | nodeTree1 `isLeftOf` nodeTree2  = if last (lastPLinesOf nodeTree1) == last (firstPLinesOf nodeTree2)
                                              then makeFace (firstENodeOf nodeTree1) (init (lastPLinesOf nodeTree1) ++ tail (reverse $ init $ firstPLinesOf nodeTree2)) (lastENodeOf nodeTree2)
                                              else makeFace (firstENodeOf nodeTree1) (init (lastPLinesOf nodeTree1) ++       reverse  (init $ firstPLinesOf nodeTree2)) (lastENodeOf nodeTree2)
          | otherwise = error $ "merp.\n" <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n" 
          where
            isLeftOf :: NodeTree -> NodeTree -> Bool
            isLeftOf nt1 nt2 = firstSegOf nt1 == lastSegOf nt2
            isRightOf :: NodeTree -> NodeTree -> Bool
            isRightOf nt1 nt2 = lastSegOf nt1 == firstSegOf nt2
            lastPLinesOf :: NodeTree -> [PLine2]
            lastPLinesOf nodeTree = (\(a,_,_) -> a) $ pathLast nodeTree
            firstPLinesOf :: NodeTree -> [PLine2]
            firstPLinesOf nodeTree = (\(a,_,_) -> a) $ pathFirst nodeTree

        -- | Create a set of faces from a nodetree.
        -- FIXME: doesn't handle more than one generation deep, yet.
        facesOfNodeTree :: NodeTree -> [Face]
        facesOfNodeTree nodeTree@(NodeTree myENodes myINodeSets)
          | null myINodeSets = []
          | otherwise = areaBeneath myENodes (init myINodeSets) $ finalINodeOf nodeTree
          where
            -- cover the space occupied by all of the ancestors of this node with a series of faces.
            areaBeneath :: [ENode] -> [[INode]] -> INode -> [Face]
            areaBeneath eNodes iNodeSets target@(INode (inArcs) _)
              | null iNodeSets && hasArc target              = init $ mapWithFollower makeTriangleFace $ fromJust . findENodeByOutput eNodes <$> inArcs
              | null iNodeSets                               =        mapWithFollower makeTriangleFace $ fromJust . findENodeByOutput eNodes <$> inArcs
              | length iNodeSets == 1 && not (hasArc target) = concat $ mapWithFollower (\a b -> areaBeneath eNodes (init iNodeSets) a ++ [areaBetween eNodes (init iNodeSets) target a b]) (head iNodeSets)
              | otherwise                                    = error $ "areabeneath: " <> show iNodeSets <> "\n" <> show target <> "\n" <> show (length iNodeSets) <> "\n"
              where
                -- | make a face from two nodes. the nodes must be composed of line segments on one side, and follow each other.
                makeTriangleFace :: ENode -> ENode -> Face
                makeTriangleFace node1 node2 = makeFace node1 [] node2

            -- cover the space between the last path of the first node and the first path of the second node with a single Face. It is assumed that both nodes have the same parent.
            areaBetween :: [ENode] -> [[INode]] -> INode -> INode -> INode -> Face
            areaBetween eNodes iNodeSets parent iNode1 iNode2
              | null iNodeSets = if (lastDescendent eNodes (iNode1)) /= (last eNodes) -- Handle the case where we are creating a face across the open end of the contour.
                                 then makeFace (lastDescendent eNodes iNode1) [lastPLineOf parent] (findMatchingDescendent eNodes iNode2 $ lastDescendent eNodes iNode1)
                                 else makeFace (firstDescendent eNodes iNode1) [firstPLineOf parent] (findMatchingDescendent eNodes iNode2 $ firstDescendent eNodes iNode1)
              | otherwise = error $
                               show iNode1 <> "\n" <> show (findENodeByOutput eNodes (firstPLineOf iNode1)) <> "\n" <> show (findENodeByOutput eNodes(lastPLineOf iNode1)) <> "\n"
                            <> show iNode2 <> "\n" <> show (findENodeByOutput eNodes (firstPLineOf iNode2)) <> "\n" <> show (findENodeByOutput eNodes (lastPLineOf iNode2)) <> "\n"
                            <> show iNodeSets <> "\n"
              where
                -- find the first immediate child of the given node.
                firstDescendent :: [ENode] -> INode -> ENode
                firstDescendent myNodeSets myParent = fromJust $ findENodeByOutput myNodeSets $ firstPLineOf myParent

                -- find the last immediate child of the given node.
                lastDescendent :: [ENode] -> INode -> ENode
                lastDescendent myNodeSets myParent = fromJust $ findENodeByOutput myNodeSets $ lastPLineOf myParent

                -- | using the set of all first generation nodes, a second generation node, and a first generation node, find out which one of the first generation children of the given second generation node shares a side with the first generation node.
                findMatchingDescendent :: [ENode] -> INode -> ENode -> ENode
                findMatchingDescendent nodes myParent target@(ENode (seg1,seg2) _)
                  | length res == 1 = head res
                  | otherwise = error $ show nodes <> "\n" <> show myParent <> "\n" <> show target <> "\n" <> show (firstDescendent nodes myParent) <> "\n" <> show (lastDescendent nodes myParent) <> "\n" <> show res <> "\n"
                  where
                    res = filter (\(ENode (sseg1, sseg2) _) -> sseg2 == seg1 || sseg1 == seg2) [firstDescendent nodes myParent, lastDescendent nodes myParent]

                firstPLineOf :: INode -> PLine2
                firstPLineOf (INode [] _) = error "empty iNode?"
                firstPLineOf (INode (a:_) _) = a
                lastPLineOf :: INode -> PLine2
                lastPLineOf (INode plines _)
                  | null plines = error "empty PLines?"
                  | otherwise   = last plines

    -- | make a face from two nodes, and a set of arcs. the nodes must be composed of line segments on one side, and follow each other.
    makeFace :: ENode -> [PLine2] -> ENode -> Face
    makeFace node1@(ENode (seg1,seg2) pline1) arcs node2@(ENode (seg3,seg4) pline2)
      | seg2 == seg3 = Face seg2 pline2 arcs pline1
      | seg1 == seg4 = Face seg1 pline1 arcs pline2
      | otherwise = error $ "cannot make a face from nodes that are not neighbors: \n" <> show node1 <> "\n" <> show node2 <> "\n"


-- utility functions exposed for tscherne.
lastSegOf :: NodeTree -> LineSeg
lastSegOf nodeTree = (\(ENode (_,outSeg) _) -> outSeg) (lastENodeOf nodeTree)

firstSegOf :: NodeTree -> LineSeg
firstSegOf nodeTree = (\(ENode (outSeg,_) _) -> outSeg) (firstENodeOf nodeTree)

-- dependent utility functions. used by last and first segment finder, and internal components.
-- FIXME: should lastSegOf, firstSegOf, and the below stuff be in a different place?
lastENodeOf :: NodeTree -> ENode
lastENodeOf nodeTree = (\(_,_,c) -> c) $ pathLast nodeTree

firstENodeOf :: NodeTree -> ENode
firstENodeOf nodeTree = (\(_,_,c) -> c) $ pathFirst nodeTree

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
        (childPlines, endNodes, finalENode)
          | length myINodeSets == 1 ||
            null myINodeSets        = ([outOf $ fromJust $ findENodeByOutput myENodes pLineToFollow], [], fromJust $ findENodeByOutput myENodes pLineToFollow)
          | otherwise               = pathFirstInner (init myINodeSets) myENodes (fromJust $ findINodeByOutput (init myINodeSets) pLineToFollow)


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
        (childPlines, endNodes, finalENode)
          | length myINodeSets == 1 = pathLastInner (init myINodeSets) myENodes (fromJust $ findINodeByOutput myINodeSets pLineToFollow)
          | null myINodeSets        = ([outOf $ fromJust $ findENodeByOutput myENodes pLineToFollow], [], fromJust $ findENodeByOutput myENodes pLineToFollow)
          | otherwise               = pathLastInner (init myINodeSets) myENodes (fromJust $ findINodeByOutput (init myINodeSets) pLineToFollow)

-- | Find a node with an output of the PLine given. start at the most recent generation, and check backwards.
findINodeByOutput :: [[INode]] -> PLine2 -> Maybe INode
findINodeByOutput iNodeSets plineOut
  | null iNodeSets            = error "could not find inode. empty set?"
  | length nodesMatching == 1 = Just $ head nodesMatching
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

