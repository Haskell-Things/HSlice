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

module Graphics.Slicer.Math.Skeleton.NodeTrees (firstENodeOf, firstSegOf, lastENodeOf, lastSegOf, pathFirst, pathLast, findENodeByOutput, sortNodeTrees, makeNodeTree, nodeTreesDoNotOverlap, lastOutsIntersect, crossoverENodes) where

import Prelude (Bool(True,False), Ordering(LT,GT), (==), otherwise, snd, ($), error, (<>), show, (<>), (&&), null, elem, (<$>))

import Prelude as P (filter)

import Data.List (sortBy)

import Data.Maybe( Maybe(Just, Nothing), isJust)

import Slist.Type (Slist(Slist))

import Slist (slist, cons, isEmpty)

import Slist as SL (filter, last)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg), endpoint)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), INode(INode), ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), Arcable(hasArc, outOf), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), Motorcycle(Motorcycle), finalINodeOf, ancestorsOf, finalPLine)

import Graphics.Slicer.Math.Skeleton.Motorcycles (motorcyclesAreCollinear, intersectionSameSide, motorcyclesInDivision)

import Graphics.Slicer.Math.PGA (PLine2, pLineIsLeft, plinesIntersectIn, eToPPoint2)

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
pathTo nodeTree@(NodeTree eNodeList@(ENodeSet firstENode _) iNodeSet@(INodeSet generations)) direction
  | isEmpty generations = ([outOf firstENode], [], firstENode)
  | otherwise = pathInner (ancestorsOf iNodeSet) eNodeList (finalINodeOf nodeTree)
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
        myError = error $ "could not find enode for " <> show pLineToFollow <> "\n" <> show eNodeList <> "\n" <> show myINodeSet <> "\n"
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
findENodeByOutput (ENodeSet firstENode moreENodes) plineOut = case nodesMatching of
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

-- | Find a node with an output of the PLine given. Check the most recent generation, and if recurse is set, check backwards.
findINodeByOutput :: INodeSet -> PLine2 -> Bool -> Maybe (INodeSet,INode)
findINodeByOutput iNodeSet@(INodeSet generations) plineOut recurse
  | isEmpty generations = Nothing
  | otherwise = case nodesMatching of
                  [] -> if recurse
                        then case generations of
                               (Slist [] _) -> Nothing
                               (Slist [INode {} :_] _) -> Nothing
                               (Slist (_:_) _) -> findINodeByOutput (ancestorsOf iNodeSet) plineOut recurse
                        else Nothing
                  [iNode] -> Just (iNodeSet, iNode)
                  (_:_) -> error "more than one node in a given generation with the same PLine out!"
  where
    nodesMatching = P.filter (\(INode _ _ _ a) -> a == Just plineOut) (SL.last generations)

-- | a smart constructor for a NodeTree
makeNodeTree :: [ENode] -> INodeSet -> NodeTree
makeNodeTree eNodes iNodeSet = case eNodes of
                                  [] -> error "not enough nodes to make a nodeTree"
                                  [eNode] -> NodeTree (ENodeSet eNode (slist [])) iNodeSet
                                  (eNode:moreENodes) -> NodeTree (ENodeSet eNode (slist moreENodes)) iNodeSet

-- | Check whether the NodeTrees of two cells have an effect on each other.
nodeTreesDoNotOverlap :: NodeTree -> NodeTree -> CellDivide -> Bool
nodeTreesDoNotOverlap nodeTree1 nodeTree2 cellDivide@(CellDivide motorcycles1 _) = case motorcycles1 of
                                                                                     (DividingMotorcycles _ (Slist [] 0)) -> res
                                                                                     (DividingMotorcycles firstMotorcycle (Slist [secondMotorcycle] 1)) -> motorcyclesAreCollinear firstMotorcycle secondMotorcycle && res
                                                                                     (DividingMotorcycles _ (Slist _ _)) -> False
  where
    res = null (crossoverENodes nodeTree1 cellDivide) &&
          null (crossoverENodes nodeTree2 cellDivide) &&
          lastOutsIntersect nodeTree1 nodeTree2 cellDivide

-- | Check that the outputs of the NodeTrees collide at the same point at the division between the two cells the NodeTrees correspond to.
lastOutsIntersect :: NodeTree -> NodeTree -> CellDivide -> Bool
lastOutsIntersect nodeTree1 nodeTree2 (CellDivide motorcycles _) = case motorcycles of
                                                                     (DividingMotorcycles m (Slist _ 0)) -> plinesIntersectIn (finalPLine nodeTree1) (outOf m) == plinesIntersectIn (finalPLine nodeTree2) (outOf m)
                                                                     (DividingMotorcycles _ (Slist _ _)) -> error "cannot yet check outpoint intersections of more than one motorcycle."

-- | Given a nodeTree and it's closing division, return all of the ENodes where the point of the node is on the opposite side of the division.
crossoverENodes :: NodeTree -> CellDivide -> [ENode]
crossoverENodes nodeTree@(NodeTree (ENodeSet firstENode (Slist moreRawNodes _)) _) cellDivision = P.filter (\a -> Just False `elem` (intersectionSameSide pointOnSide a <$> motorcyclesInDivision cellDivision)) (firstENode:moreRawNodes)
  where
    pointOnSide = eToPPoint2 $ pointInCell nodeTree cellDivision
    pointInCell cell (CellDivide (DividingMotorcycles m _) _)
      | firstSegOf cell == lastCSegOf m = endpoint $ firstSegOf cell
      | lastSegOf cell == firstCSegOf m = startPoint $ lastSegOf cell
      | otherwise = error $ "unhandled case: " <> show cell <> "\n" <> show m <> "\n" <> show (lastSegOf cell) <> "\n" <> show (firstSegOf cell) <> "\n"
      where
        startPoint (LineSeg a _) = a
        firstCSegOf (Motorcycle (seg1,_) _) = seg1
        lastCSegOf (Motorcycle (_, seg2) _) = seg2

