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

import Prelude ((==), otherwise, (<$>), ($), length, (/=), error, (<>), show, Eq, Show, (<>), Bool(True), (||), take, filter, null, concat, and, snd, (&&), (>), (<), (*))

import Data.List (dropWhile)

import Data.List as DL (last)

import Data.List.Extra (unsnoc)

import Data.Maybe (isNothing, fromMaybe, Maybe(Just, Nothing), isJust, fromJust)

import Safe (initSafe)

import Slist.Type (Slist(Slist))

import Slist (slist, cons, isEmpty, len, one, init, tail, reverse)

import Slist as SL (last)

import Graphics.Slicer.Math.Definitions (LineSeg, mapWithFollower, startPoint, fudgeFactor)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode(ENode), INode(INode), ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), Arcable(hasArc), finalINodeOf, finalOutOf, ancestorsOf, pPointOf)

import Graphics.Slicer.Math.Skeleton.NodeTrees (lastSegOf, findENodeByOutput, findINodeByOutput, firstSegOf, lastENodeOf, firstENodeOf, pathFirst, pathLast)

import Graphics.Slicer.Math.PGA (PLine2, distanceBetweenPPoints, eToPPoint2)

import Graphics.Slicer.Math.Line (endPoint)

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
facesOf straightSkeleton@(StraightSkeleton nodeLists spine)
  | null spine = case nodeLists of
                   [] -> nodeListError
                   [oneNodeList] -> if facesInOrder (res oneNodeList)
                                    then res oneNodeList
                                    else error $ "faces out of order!\n" <> show (edgesOf $ res oneNodeList) <> "\n" <> show oneNodeList <> "\n" 
                   (_:_) -> nodeListError
  | otherwise = error "cannot yet handle spines, or more than one NodeList."
  where
    res nodeList = findFaces nodeList
    facesInOrder :: [Face] -> Bool
    facesInOrder faces
      | length faces > 1 = and $ mapWithFollower (\a b -> distanceBetweenPPoints (eToPPoint2 $ endPoint a) (eToPPoint2 $ startPoint b) < fudgeFactor*10) (edgesOf faces)
      | otherwise = True
    edgesOf :: [Face] -> [LineSeg]
    edgesOf faces = unwrap <$> faces
      where
        unwrap :: Face -> LineSeg
        unwrap (Face edge _ _ _) = edge 
    nodeListError = error "cannot handle anything other than one NodeList in a straight skeleton."
    -- find all of the faces of a set of nodeTrees.
    findFaces :: [NodeTree] -> [Face]
    findFaces nodeTrees = case nodeTrees of
                            [] -> []
                            [oneNodeTree] -> if isNothing (finalOutOf oneNodeTree)
                                             then rawFaces
                                             else error $ "Only one NodeTree given, and it has an output arc. Don't know how to continue: " <> show straightSkeleton <> "\n"
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

-- | Create a face covering the space between two NodeTrees. like areaBetween, but for two separate NodeTrees.
intraNodeFace :: NodeTree -> NodeTree -> Face
intraNodeFace nodeTree1 nodeTree2
  | nodeTree1 `isLeftOf` nodeTree2  = if nodeTree1 `follows` nodeTree2
                                      then fromMaybe errNodesNotNeighbors $
                                             makeFace (firstENodeOf nodeTree1) (init (lastPLinesOf nodeTree1) <> tail (tail $ reverse $ firstPLinesOf nodeTree2)) (lastENodeOf nodeTree2)
                                      else fromMaybe errNodesNotNeighbors $
                                             makeFace (firstENodeOf nodeTree1) (init (lastPLinesOf nodeTree1) <>       tail  (reverse $ firstPLinesOf nodeTree2)) (lastENodeOf nodeTree2)
  | nodeTree1 `isRightOf` nodeTree2 = if nodeTree2 `follows` nodeTree1
                                      then fromMaybe errNodesNotNeighbors $
                                             makeFace (lastENodeOf nodeTree2) (init (firstPLinesOf nodeTree2) <> tail (tail $ reverse $ lastPLinesOf nodeTree1)) (firstENodeOf nodeTree1)
                                      else fromMaybe errNodesNotNeighbors $
                                             makeFace (firstENodeOf nodeTree2) (init (firstPLinesOf nodeTree2) <>       tail  (reverse $ lastPLinesOf nodeTree1)) (lastENodeOf nodeTree1)
  | nodeTree1 == nodeTree2          = error $ "two identical nodes given.\n" <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n"
  | otherwise = error $ "Two NodeTrees given that are not neighbors: " <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n"
  where
    errNodesNotNeighbors = error $ "cannot make a face from nodes that are not neighbors: \n" <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n"
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

-- | Create a set of faces from a single nodetree.
-- FIXME: doesn't handle more than one generation deep, yet.
facesOfNodeTree :: NodeTree -> [Face]
facesOfNodeTree nodeTree@(NodeTree myENodes iNodeSet@(INodeSet generations))
  | isEmpty generations = []
  | otherwise = areaBeneath myENodes (ancestorsOf iNodeSet) $ finalINodeOf iNodeSet
  where
    -- cover the space occupied by all of the ancestors of this node with a series of faces.
    areaBeneath :: ENodeSet -> INodeSet -> INode -> [Face]
    areaBeneath eNodeList myINodeSet@(INodeSet myGenerations) target =
      case myGenerations of
        (Slist [] _) -> -- no more generations.
          if allENodes eNodeList target
          then if hasArc target
               then -- skip the last triangle, as the target's output is somewhere within it.
                 initSafe $ allENodesAsTriangles target
               else allENodesAsTriangles target
          else errorNoMoreINodes
        (Slist [oneGeneration] _) -> handleGeneration oneGeneration
        (Slist (xs) _) -> concat $ handleGeneration <$> xs 
      where
        -- check the ins of an inode, and make sure all of them point to as ENode.
        allENodes :: ENodeSet -> INode -> Bool
        allENodes eNodeSet myTarget = and $ isJust <$> (findENodeByOutput eNodeSet <$> inArcsOf myTarget)
        -- create triangles from every pair of arcs of this inode, in order. assumes that all of the arcs are connected to ENodes.
        allENodesAsTriangles :: INode -> [Face]
        allENodesAsTriangles myTarget =
          case eNodesOfINode myTarget of
            [] -> error $ "could not find any eNodes of INode: " <> show myTarget
            [a] -> error $ "asked to map with follower a single item:" <> show a <> "\n"
            _ -> mapWithFollower makeTriangleFace $ eNodesOfINode myTarget
            where
              -- All of the ENodes coresponding to a node's arc.
              eNodesOfINode myINode = fromMaybe (error $ "failed to find an ENode for an arc of inode: " <> show myINode <> "\n") <$> (findENodeByOutput eNodeList <$> inArcsOf myINode)

        handleGeneration oneGeneration = case oneGeneration of
                                           [] -> errorEmptyGeneration
                                           [oneINode] -> if allENodes eNodeList oneINode
                                                         then allENodesAsTriangles oneINode
                                                         else concat $ mapWithFollower (areaBeneathPair oneINode) $ inArcsOf oneINode
                                           _ -> concat $ mapWithFollower (\a b -> areaBeneath eNodeList (ancestorsOf myINodeSet) a <> [areaBetween eNodeList target a b]) oneGeneration

        areaBeneathPair :: INode -> PLine2 -> PLine2 -> [Face]
        areaBeneathPair myINode pLine1 pLine2
         | isENode pLine1 && isENode pLine2 = [makeTriangleFace (fromJust $ findENodeByOutput eNodeList pLine1) (fromJust $ findENodeByOutput eNodeList pLine2)]
         | isENode pLine1 = fromJust (makeFace (fromJust $ findENodeByOutput eNodeList pLine1) (slist $ pathToFirstDescendent pLine2) (firstDescendent pLine2)) :
                            (areaBeneath eNodeList (ancestorsOf myINodeSet) $ snd $ fromJust $ findINodeByOutput myINodeSet pLine2 True )
         | isENode pLine2 = [fromJust (makeFace (lastDescendent pLine1) (slist $ pathToFirstDescendent pLine2) (fromJust $ findENodeByOutput eNodeList pLine2))]
         | otherwise = [areaBetween eNodeList myINode (snd $ fromJust $ findINodeByOutput myINodeSet pLine1 True) (snd $ fromJust $ findINodeByOutput myINodeSet pLine2 True)]
         where
           isENode myPLine = isJust $ findENodeByOutput eNodeList myPLine
           pathToFirstDescendent :: PLine2 -> [PLine2]
           pathToFirstDescendent myPLine
            | isENode myPLine = []
            | otherwise = myPLine : pathToFirstDescendent (firstInOf $ snd $ fromJust $ findINodeByOutput myINodeSet myPLine True)
           pathToLastDescendent :: PLine2 -> [PLine2]
           pathToLastDescendent myPLine
            | isENode myPLine = []
            | otherwise = myPLine : pathToLastDescendent (lastInOf $ snd $ fromJust $ findINodeByOutput myINodeSet myPLine True)
           firstDescendent myPLine = fromJust $ findENodeByOutput eNodeList $ firstInOf $ snd $ fromJust $ findINodeByOutput myINodeSet (DL.last $ pathToFirstDescendent myPLine) True
           lastDescendent myPLine = fromJust $ findENodeByOutput eNodeList $ firstInOf $ snd $ fromJust $ findINodeByOutput myINodeSet (DL.last $ pathToLastDescendent myPLine) True

        -- All of an INode's input arcs.
        inArcsOf (INode firstArc secondArc (Slist rawMoreArcs _) _)= firstArc : secondArc : rawMoreArcs

        -- different error conditions
        errorEmptyGeneration = error $ "got an empty generation.\n" <> show (len myGenerations) <> "\n" <> show nodeTree <> "\n" <> show target <> "\n" 
        errorNoMoreINodes = error $ "one target, no generations, and target needs inodes?\n" <> show target <> "\n" <> show nodeTree <> "\n"

-- | Create a face covering the space between the last path of the first inode and the first path of the second inode with a single Face. It is assumed that both nodes have the same parent.
areaBetween :: ENodeSet -> INode -> INode -> INode -> Face
areaBetween (ENodeSet (Slist [] _)) _ _ _ = error "no sides?"
areaBetween (ENodeSet (Slist (_:_:_) _)) _ _ _ = error "too many sides?"
areaBetween eNodeList@(ENodeSet (Slist [(firstENode,moreENodes)] _)) parent iNode1 iNode2
  -- Handle the case where we are creating a face across the open end of the contour.
  | lastDescendent eNodeList iNode1 /= SL.last (cons firstENode moreENodes) = fromMaybe errNodesNotNeighbors $
                                                                                makeFace (lastDescendent eNodeList iNode1) (one $ lastInOf parent) (findMatchingDescendent eNodeList iNode2 $ lastDescendent eNodeList iNode1)
  | otherwise                                                               = fromMaybe errNodesNotNeighbors $
                                                                                makeFace (firstDescendent eNodeList iNode1) (one $ firstInOf parent) (findMatchingDescendent eNodeList iNode2 $ firstDescendent eNodeList iNode1)
  where
    -- our error condition.
    errNodesNotNeighbors = error $ "cannot make a face from nodes that are not neighbors: \n" <> show eNodeList <> "\n" <> show parent <> "\n" <> show iNode1 <> "\n" <> show iNode2 <> "\n"

    -- | given all of the first generation nodes, an inode, and an enode, find out which one of the first generation children of the given second generation node shares a side with the first generation node.
    findMatchingDescendent :: ENodeSet -> INode -> ENode -> ENode
    findMatchingDescendent eNodes myParent (ENode (seg1,seg2) _) =
      case res of
        [] -> error "got no result looking for descendent"
        [oneResult] -> oneResult
        (_:_) -> error "got too many results looking for descendent."
      where
        res = filter (\(ENode (sseg1, sseg2) _) -> sseg2 == seg1 || sseg1 == seg2) [firstDescendent eNodes myParent, lastDescendent eNodes myParent]

    -- find the first immediate child of the given INode.
    firstDescendent :: ENodeSet -> INode -> ENode
    firstDescendent myNodeSets myParent = fromMaybe (error "could not find first ENode of myParent?") $ findENodeByOutput myNodeSets $ firstInOf myParent

    -- find the last immediate child of the given INode.
    lastDescendent :: ENodeSet -> INode -> ENode
    lastDescendent myNodeSets myParent = fromMaybe (error "could not find last ENode of myParent?") $ findENodeByOutput myNodeSets $ lastInOf myParent

-- | find the last PLine of an INode.
lastInOf :: INode -> PLine2
lastInOf (INode _ secondPLine morePLines _)
  | len morePLines == 0 = secondPLine
  | otherwise           = SL.last morePLines

-- | find the first PLine of an INode.
firstInOf :: INode -> PLine2
firstInOf (INode a _ _ _) = a

-- | Construct a face from two nodes. the nodes must be composed of line segments on one side, and follow each other.
makeTriangleFace :: ENode -> ENode -> Face
makeTriangleFace node1 node2 = fromMaybe (error $ "cannot make a face from nodes that are not neighbors: \n" <> show node1 <> "\n" <> show node2 <> "\n") $ makeFace node1 (Slist [] 0) node2

-- | Construct a face from two nodes, and a set of arcs. the nodes must follow each other on the contour.
makeFace :: ENode -> Slist PLine2 -> ENode -> Maybe Face
makeFace (ENode (seg1,seg2) pline1) arcs (ENode (seg3,seg4) pline2)
  | seg2 == seg3 = Just $ Face seg2 pline2 arcs pline1
  | seg1 == seg4 = Just $ Face seg1 pline1 arcs pline2
  | otherwise = Nothing

