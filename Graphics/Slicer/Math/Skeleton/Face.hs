{- ORMOLU_DISABLE -}
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
{-# LANGUAGE DerivingStrategies #-}

-- | This file contains code for creating a series of Faces, covering a straight skeleton.
module Graphics.Slicer.Math.Skeleton.Face (Face(Face), orderedFacesOf, facesOf) where

import Prelude ((==), otherwise, (<$>), ($), length, (/=), error, (<>), show, Eq, Show, (<>), Bool, (||), take, filter, null, concat)

import Data.List (dropWhile)

import Data.List.Extra (unsnoc)

import Data.Maybe (isNothing, fromMaybe, Maybe(Just, Nothing))

import Safe (initSafe)

import Slist.Type (Slist(Slist))

import Slist (slist, cons, isEmpty, len, one, init, tail, reverse)

import Slist as SL (last)

import Graphics.Slicer.Math.Definitions (mapWithFollower)

import Graphics.Slicer.Math.Line (LineSeg)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode(ENode), INode(INode), ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), Arcable(hasArc), finalINodeOf, finalOutOf, ancestorsOf)

import Graphics.Slicer.Math.Skeleton.NodeTrees (lastSegOf, findENodeByOutput, firstSegOf, lastENodeOf, firstENodeOf, pathFirst, pathLast)

import Graphics.Slicer.Math.PGA (PLine2)

--------------------------------------------------------------------
-------------------------- Face Placement --------------------------
--------------------------------------------------------------------

-- | A Face:
--   A portion of a contour, with a real side, and arcs (line segments between nodes) dividing it from other faces.
--   Faces have no holes, and their arcs and nodes (line segments and points) are generated from a StraightSkeleton of a Contour.
data Face = Face { _edge :: !LineSeg, _firstArc :: !PLine2, _arcs :: !(Slist PLine2), _lastArc :: !PLine2 }
  deriving Eq
  deriving stock Show

-- | take a straight skeleton, and create faces from it.
-- accepts a line segment you want the first face to contain, and reorders the face list.
orderedFacesOf :: LineSeg -> StraightSkeleton -> [Face]
orderedFacesOf start skeleton = facesFromIndex start $ facesOf skeleton
  where
    facesFromIndex :: LineSeg -> [Face] -> [Face]
    facesFromIndex targetSeg rawFaces = take (length rawFaces) $ dropWhile (\(Face a _ _ _) -> a /= targetSeg) $ rawFaces <> rawFaces

-- | take a straight skeleton, and create faces from it.
facesOf :: StraightSkeleton -> [Face]
facesOf (StraightSkeleton nodeLists spine)
  | null spine = case nodeLists of
                   [] -> nodeListError
                   [oneNodeList] -> findFaces oneNodeList
                   (_:_) -> nodeListError
  | otherwise = error "cannot yet handle spines, or more than one NodeList."
  where
    nodeListError = error "cannot handle anything other than one NodeList in a straight skeleton."
    -- find all of the faces of a set of nodeTrees.
    findFaces :: [NodeTree] -> [Face]
    findFaces nodeTrees = case nodeTrees of
                            [] -> []
                            [oneNodeTree] -> if isNothing (finalOutOf oneNodeTree)
                                             then rawFaces
                                             else error $ "Only one NodeTree given, and it has an output arc. Don't know how to continue: " <> show oneNodeTree <> "\n"
                            (_:_) -> rawFaces
      where
        rawFaces = case nodeTrees of
                     [] -> error "Impossible. cannot happen."
                     [a] -> facesOfNodeTree a
                     [firstNodeTree, secondNodeTree] -> findFacesRecurse nodeTrees <> [intraNodeFace secondNodeTree firstNodeTree]
                     (firstNodeTree:moreNodeTrees) -> case unsnoc moreNodeTrees of
                                                        Nothing -> error "empty node tree?"
                                                        Just (_,lastNodeTree) -> findFacesRecurse nodeTrees <> [intraNodeFace lastNodeTree firstNodeTree]
        -- Recursively find faces.
        findFacesRecurse :: [NodeTree] -> [Face]
        findFacesRecurse myNodeTrees = case myNodeTrees of
                                         [] -> error "Impossible. cannot happen."
                                         [tree1] -> facesOfNodeTree tree1
                                         [tree1,tree2] -> facesOfNodeTree tree2 <> (intraNodeFace tree1 tree2 : facesOfNodeTree tree1)
                                         (tree1:tree2:xs) -> findFacesRecurse (tree2:xs) <> (intraNodeFace tree1 tree2 : facesOfNodeTree tree1)
        -- Create a single face for the space between two NodeTrees. like areaBetween, but for two separate NodeTrees.
        intraNodeFace :: NodeTree -> NodeTree -> Face
        intraNodeFace nodeTree1 nodeTree2
          | nodeTree1 `isLeftOf` nodeTree2  = if nodeTree1 `follows` nodeTree2
                                              then makeFace (firstENodeOf nodeTree1) (init (lastPLinesOf nodeTree1) <> tail (tail $ reverse $ firstPLinesOf nodeTree2)) (lastENodeOf nodeTree2)
                                              else makeFace (firstENodeOf nodeTree1) (init (lastPLinesOf nodeTree1) <>       tail  (reverse $ firstPLinesOf nodeTree2)) (lastENodeOf nodeTree2)
          | nodeTree1 `isRightOf` nodeTree2 = if nodeTree2 `follows` nodeTree1
                                              then makeFace (lastENodeOf nodeTree2) (init (firstPLinesOf nodeTree2) <> tail (tail $ reverse $ lastPLinesOf nodeTree1)) (firstENodeOf nodeTree1)
                                              else makeFace (lastENodeOf nodeTree2) (init (firstPLinesOf nodeTree2) <>       tail  (reverse $ lastPLinesOf nodeTree1)) (firstENodeOf nodeTree1)
          | nodeTree1 == nodeTree2          = error $ "two identical nodes given.\n" <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n"
          | otherwise = error $ "Two NodeTrees given that are not neighbors: " <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n"
          where
            follows :: NodeTree -> NodeTree -> Bool
            follows nt1 nt2 = SL.last (firstPLinesOf nt1) == SL.last (lastPLinesOf nt2)
            isLeftOf :: NodeTree -> NodeTree -> Bool
            isLeftOf nt1 nt2 = firstSegOf nt1 == lastSegOf nt2
            isRightOf :: NodeTree -> NodeTree -> Bool
            isRightOf nt1 nt2 = lastSegOf nt1 == firstSegOf nt2
            lastPLinesOf :: NodeTree -> Slist PLine2
            lastPLinesOf nodeTree = slist $ (\(a,_,_) -> a) $ pathLast nodeTree
            firstPLinesOf :: NodeTree -> Slist PLine2
            firstPLinesOf nodeTree = slist $ (\(a,_,_) -> a) $ pathFirst nodeTree

        -- | Create a set of faces from a nodetree.
        -- FIXME: doesn't handle more than one generation deep, yet.
        facesOfNodeTree :: NodeTree -> [Face]
        facesOfNodeTree nodeTree@(NodeTree myENodes iNodeSet@(INodeSet generations))
          | isEmpty generations = []
          | otherwise = areaBeneath myENodes (ancestorsOf iNodeSet) $ finalINodeOf nodeTree
          where
            -- cover the space occupied by all of the ancestors of this node with a series of faces.
            areaBeneath :: ENodeSet -> INodeSet -> INode -> [Face]
            areaBeneath eNodeList myINodeSet@(INodeSet myGenerations) target =
              case myGenerations of
                (Slist [] _) -> if hasArc target
                                then initSafe resultAsTriangles
                                else resultAsTriangles
                (Slist [oneGeneration] _) -> if hasArc target
                                             then errorHasArc
                                             else concat $ mapWithFollower (\a b -> areaBeneath eNodeList (ancestorsOf myINodeSet) a <> [areaBetween eNodeList target a b]) oneGeneration
                (Slist (_:_) _) -> errorTooMany
              where
                -- create triangles from every pair of arcs of this inode, in order. assumes that all of the arcs are connected to ENodes.
                resultAsTriangles = mapWithFollower makeTriangleFace $ eNodesOfINode target
                -- All of the ENodes coresponding to a node's arc.
                eNodesOfINode myINode = fromMaybe (error $ "failed to find an ENode for an arc of inode: " <> show myINode <> "\n") <$> (findENodeByOutput eNodeList <$> inArcsOf myINode)
                -- All of a node's arcs.
                inArcsOf (INode firstArc secondArc (Slist rawMoreArcs _) _)= firstArc : secondArc : rawMoreArcs
                errorHasArc = error $ "Has Arc: " <> show nodeTree <> "\n" <> show target <> "\n" <> show (len myGenerations) <> "\n"
                errorTooMany = error $ "Too Many: " <> show nodeTree <> "\n" <> show target <> "\n" <> show (len myGenerations) <> "\n"
                -- | make a face from two nodes. the nodes must be composed of line segments on one side, and follow each other.
                makeTriangleFace :: ENode -> ENode -> Face
                makeTriangleFace node1 node2 = makeFace node1 (Slist [] 0) node2

            -- cover the space between the last path of the first node and the first path of the second node with a single Face. It is assumed that both nodes have the same parent.
            areaBetween :: ENodeSet -> INode -> INode -> INode -> Face
            areaBetween eNodeList@(ENodeSet firstENode moreENodes) parent iNode1 iNode2
              -- Handle the case where we are creating a face across the open end of the contour.
              | lastDescendent eNodeList iNode1 /= SL.last (cons firstENode moreENodes) = makeFace (lastDescendent eNodeList iNode1) (one $ lastPLineOf parent) (findMatchingDescendent eNodeList iNode2 $ lastDescendent eNodeList iNode1)
              | otherwise                                                               = makeFace (firstDescendent eNodeList iNode1) (one $ firstPLineOf parent) (findMatchingDescendent eNodeList iNode2 $ firstDescendent eNodeList iNode1)
              where
                -- | using the set of all first generation nodes, a second generation node, and a first generation node, find out which one of the first generation children of the given second generation node shares a side with the first generation node.
                findMatchingDescendent :: ENodeSet -> INode -> ENode -> ENode
                findMatchingDescendent eNodes myParent (ENode (seg1,seg2) _) =
                  case res of
                    [] -> error "got no result looking for descendent"
                    [oneResult] -> oneResult
                    (_:_) -> error "got too many results looking for descendent."
                  where
                    res = filter (\(ENode (sseg1, sseg2) _) -> sseg2 == seg1 || sseg1 == seg2) [firstDescendent eNodes myParent, lastDescendent eNodes myParent]

                -- find the first immediate child of the given node.
                firstDescendent :: ENodeSet -> INode -> ENode
                firstDescendent myNodeSets myParent = fromMaybe (error "could not find ENode for firstPLineOf myParent?") $ findENodeByOutput myNodeSets $ firstPLineOf myParent

                -- find the last immediate child of the given node.
                lastDescendent :: ENodeSet -> INode -> ENode
                lastDescendent myNodeSets myParent = fromMaybe (error "could not find ENode for lastPLineOf myParent?") $ findENodeByOutput myNodeSets $ lastPLineOf myParent

                firstPLineOf :: INode -> PLine2
                firstPLineOf (INode a _ _ _) = a
                lastPLineOf :: INode -> PLine2
                lastPLineOf (INode _firstPLine secondPLine morePLines _) = SL.last (cons secondPLine morePLines)

    -- | make a face from two nodes, and a set of arcs. the nodes must be composed of line segments on one side, and follow each other.
    makeFace :: ENode -> Slist PLine2 -> ENode -> Face
    makeFace node1@(ENode (seg1,seg2) pline1) arcs node2@(ENode (seg3,seg4) pline2)
      | seg2 == seg3 = Face seg2 pline2 arcs pline1
      | seg1 == seg4 = Face seg1 pline1 arcs pline2
      | otherwise = error $ "cannot make a face from nodes that are not neighbors: \n" <> show node1 <> "\n" <> show node2 <> "\n"

