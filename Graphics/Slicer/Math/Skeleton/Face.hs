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

{-
 - This file contains code for creating a series of faces, covering a straight skeleton.
 -}
module Graphics.Slicer.Math.Skeleton.Face (Face(Face), orderedFacesOf, facesOf, lastSegOf, firstSegOf, lastENodeOf, firstENodeOf) where

import Prelude ((==), otherwise, (<$>), ($), (.), length, (/=), error, (<>), show, Eq, Show, (<>), (++), Bool, (||), take, filter, init, null, tail, concat, not, reverse)

import Prelude as P (last)

import Data.List (dropWhile)

import Data.Maybe(isNothing, fromJust)

import Slist.Type (Slist(Slist))

import Slist (slist, cons)

import Slist as SL (last)

import Graphics.Slicer.Math.Definitions (mapWithFollower)

import Graphics.Slicer.Math.Line (LineSeg)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode(ENode), INode(INode), ENodeList(ENodeList), NodeTree(NodeTree), Arcable(hasArc), finalINodeOf, finalOutOf)

import Graphics.Slicer.Math.Skeleton.NodeTrees (lastSegOf, findENodeByOutput, firstSegOf, lastENodeOf, firstENodeOf, pathFirst, pathLast)

import Graphics.Slicer.Math.PGA (PLine2)

--------------------------------------------------------------------
-------------------------- Face Placement --------------------------
--------------------------------------------------------------------

-- | A Face:
--   A portion of a contour, with a real side, and arcs (line segments between nodes) dividing it from other faces.
--   Faces have no holes, and their arcs and nodes (line segments and points) are generated from a StraightSkeleton of a Contour.
data Face = Face { _edge :: LineSeg, _firstArc :: PLine2, _arcs :: Slist PLine2, _lastArc :: PLine2 }
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
                     [firstNodeTree, secondNodeTree] -> findFacesRecurse nodeTrees ++ [intraNodeFace secondNodeTree firstNodeTree]
                     (firstNodeTree:_:lastNodeTrees) -> findFacesRecurse nodeTrees ++ [intraNodeFace (P.last lastNodeTrees) firstNodeTree]
        -- Recursively find faces.
        findFacesRecurse :: [NodeTree] -> [Face]
        findFacesRecurse myNodeTrees = case myNodeTrees of
                                         [] -> error "Impossible. cannot happen."
                                         [tree1] -> facesOfNodeTree tree1
                                         [tree1,tree2] -> facesOfNodeTree tree2 ++ (intraNodeFace tree1 tree2 : facesOfNodeTree tree1)
                                         (tree1:tree2:xs) -> findFacesRecurse (tree2:xs) ++ (intraNodeFace tree1 tree2 : facesOfNodeTree tree1)
        -- Create a single face for the space between two NodeTrees. like areaBetween, but for two separate NodeTrees.
        intraNodeFace :: NodeTree -> NodeTree -> Face
        intraNodeFace nodeTree1 nodeTree2
          | nodeTree1 == nodeTree2          = error $ "two identical nodes given.\n" <> show nodeTree1 <> "\n"
          | nodeTree1 `isRightOf` nodeTree2 = if P.last (firstPLinesOf nodeTree2) == P.last (lastPLinesOf nodeTree1)
                                              then makeFace (lastENodeOf nodeTree2) (init (firstPLinesOf nodeTree2) ++ tail (reverse $ init $ lastPLinesOf nodeTree1)) (firstENodeOf nodeTree1)
                                              else makeFace (lastENodeOf nodeTree2) (init (firstPLinesOf nodeTree2) ++       reverse  (init $ lastPLinesOf nodeTree1)) (firstENodeOf nodeTree1)
          | nodeTree1 `isLeftOf` nodeTree2  = if P.last (lastPLinesOf nodeTree1) == P.last (firstPLinesOf nodeTree2)
                                              then makeFace (firstENodeOf nodeTree1) (init (lastPLinesOf nodeTree1) ++ tail (reverse $ init $ firstPLinesOf nodeTree2)) (lastENodeOf nodeTree2)
                                              else makeFace (firstENodeOf nodeTree1) (init (lastPLinesOf nodeTree1) ++       reverse  (init $ firstPLinesOf nodeTree2)) (lastENodeOf nodeTree2)
          | otherwise = error $ "Two NodeTrees given that are not neighbors: " <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n"
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
            areaBeneath :: ENodeList -> [[INode]] -> INode -> [Face]
            areaBeneath eNodeList iNodeSets target@(INode firstArc secondArc (Slist rawMoreArcs _) _) =
              case iNodeSets of
                [] -> if hasArc target
                      then init result
                      else result
                [oneINode] -> if not (hasArc target)
                              then concat $ mapWithFollower (\a b -> areaBeneath eNodeList [] a ++ [areaBetween eNodeList target a b]) oneINode
                              else error $ "given only one INode Set, but our target has a result arc."
                (_:_) -> error $ "given multiple INode Sets. Cannot continue."
              where
                result = mapWithFollower makeTriangleFace $ fromJust . findENodeByOutput eNodeList <$> inArcs
                inArcs = firstArc : secondArc : rawMoreArcs
                -- | make a face from two nodes. the nodes must be composed of line segments on one side, and follow each other.
                makeTriangleFace :: ENode -> ENode -> Face
                makeTriangleFace node1 node2 = makeFace node1 [] node2

            -- cover the space between the last path of the first node and the first path of the second node with a single Face. It is assumed that both nodes have the same parent.
            areaBetween :: ENodeList -> INode -> INode -> INode -> Face
            areaBetween eNodeList@(ENodeList firstENode moreENodes) parent iNode1 iNode2
              -- Handle the case where we are creating a face across the open end of the contour.
              | lastDescendent eNodeList iNode1 /= SL.last (cons firstENode moreENodes) = makeFace (lastDescendent eNodeList iNode1) [lastPLineOf parent] (findMatchingDescendent eNodeList iNode2 $ lastDescendent eNodeList iNode1)
              | otherwise                                                               = makeFace (firstDescendent eNodeList iNode1) [firstPLineOf parent] (findMatchingDescendent eNodeList iNode2 $ firstDescendent eNodeList iNode1)
              where
                -- | using the set of all first generation nodes, a second generation node, and a first generation node, find out which one of the first generation children of the given second generation node shares a side with the first generation node.
                findMatchingDescendent :: ENodeList -> INode -> ENode -> ENode
                findMatchingDescendent eNodes myParent (ENode (seg1,seg2) _) =
                  case res of
                    [] -> error "got no result looking for descendent"
                    [oneResult] -> oneResult
                    (_:_) -> error "got too many results looking for descendent."
                  where
                    res = filter (\(ENode (sseg1, sseg2) _) -> sseg2 == seg1 || sseg1 == seg2) [firstDescendent eNodes myParent, lastDescendent eNodes myParent]

                -- find the first immediate child of the given node.
                firstDescendent :: ENodeList -> INode -> ENode
                firstDescendent myNodeSets myParent = fromJust $ findENodeByOutput myNodeSets $ firstPLineOf myParent

                -- find the last immediate child of the given node.
                lastDescendent :: ENodeList -> INode -> ENode
                lastDescendent myNodeSets myParent = fromJust $ findENodeByOutput myNodeSets $ lastPLineOf myParent

                firstPLineOf :: INode -> PLine2
                firstPLineOf (INode a _ _ _) = a
                lastPLineOf :: INode -> PLine2
                lastPLineOf (INode _firstPLine secondPLine morePLines _) = SL.last (cons secondPLine morePLines)

    -- | make a face from two nodes, and a set of arcs. the nodes must be composed of line segments on one side, and follow each other.
    makeFace :: ENode -> [PLine2] -> ENode -> Face
    makeFace node1@(ENode (seg1,seg2) pline1) arcs node2@(ENode (seg3,seg4) pline2)
      | seg2 == seg3 = Face seg2 pline2 (slist arcs) pline1
      | seg1 == seg4 = Face seg1 pline1 (slist arcs) pline2
      | otherwise = error $ "cannot make a face from nodes that are not neighbors: \n" <> show node1 <> "\n" <> show node2 <> "\n"

