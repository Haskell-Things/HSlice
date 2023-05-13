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
module Graphics.Slicer.Math.Skeleton.NodeTrees (
  MaybeMatch(FirstLast, LastFirst, NoMatch),
  findENodeByOutput,
  findINodeByOutput,
  firstENodeOf,
  firstSegOf,
  lastENodeOf,
  lastSegOf,
  makeNodeTree,
  mergeNodeTrees,
  pathFirst,
  pathLast
  ) where

import Prelude (Bool(True,False), Eq, Show, (==), concat, mempty, not, otherwise, snd, ($), error, (<>), notElem, show, (&&), (/=), null, (<$>), fst)

import Data.Maybe (Maybe(Just, Nothing), catMaybes, fromJust, fromMaybe, isJust)

import Slist.Type (Slist(Slist))

import Slist (cons, len, slist)

import Slist as SL (filter, last, head, init, isEmpty)

import Graphics.Slicer.Math.Definitions (LineSeg)

import Graphics.Slicer.Math.Skeleton.Definitions (ENode, INode(INode), ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), Side(Side), allINodesOf, eNodesOfSide, finalINodeOf, finalOutAndErrOf, getFirstLineSeg, getLastLineSeg, iNodeHasIn, isOneSide, ancestorsOf, makeSide, oneSideOf)

import Graphics.Slicer.Math.PGA (PLine2Err, ProjectiveLine, Arcable(hasArc, outOf), outAndErrOf)

-- | The result of comparing two objects containing line segments, and seeing if they follow each other.
data MaybeMatch = FirstLast
                | LastFirst
                | NoMatch
  deriving (Eq, Show)

-- FIXME: does not have sides to handle. may miss regions with a side that only has one segment.
lastSegOf :: NodeTree -> LineSeg
lastSegOf nodeTree = getLastLineSeg $ lastENodeOf nodeTree

firstSegOf :: NodeTree -> LineSeg
firstSegOf nodeTree = getFirstLineSeg $ firstENodeOf nodeTree

lastENodeOf :: NodeTree -> ENode
lastENodeOf (NodeTree (ENodeSet sides) _) = lastOfSide $ last sides

firstENodeOf :: NodeTree -> ENode
firstENodeOf (NodeTree (ENodeSet sides) _) = firstOfSide $ head sides

-- | whether to follow the first or the last node in a branch, when branching in pathTo.
data Direction = Head
               | Last

pathFirst, pathLast :: NodeTree -> ([(ProjectiveLine, PLine2Err)], [INode], ENode)
pathFirst nodeTree = pathTo nodeTree Head
pathLast nodeTree = pathTo nodeTree Last

-- | Find all of the Nodes and all of the arcs between the last item in the nodeTree and the node that is part of the original contour on the given side.
pathTo :: NodeTree -> Direction -> ([(ProjectiveLine, PLine2Err)], [INode], ENode)
pathTo (NodeTree (ENodeSet (Slist [] _)) _) _ = error "unable to pathTo a Nodetree without ENodes."
pathTo (NodeTree eNodeSet iNodeSet) direction
  | isJust iNodeSet = fromMaybe (error "whoops!") $ maybeOnlyOneJust $ (\a -> pathInner a eNodeSet (finalINodeOf $ fromJust iNodeSet)) <$> ancestorsOf (fromJust iNodeSet)
  | otherwise = error "asked for pathTo for a NodeTree that has no INodes."
  where
    pathInner :: INodeSet -> ENodeSet -> INode -> Maybe ([(ProjectiveLine, PLine2Err)], [INode], ENode)
    pathInner myINodeSet myENodeSet target@(INode firstPLine secondPLine morePLines _)
      | hasArc target = Just (outAndErrOf target : childPlines, target: endNodes, finalENode)
      | otherwise     = Just (                     childPlines, target: endNodes, finalENode)
      where
        (childPlines, endNodes, finalENode) = if isJust result
                                              then returnResult (fromJust result)
                                              else case iNodeOnThisLevel of
                                                     (Just res) -> fromMaybe (error "no!") $ pathInner myINodeSet myENodeSet (snd res)
                                                     Nothing -> case ancestorsOf myINodeSet of
                                                                  [] -> myError
                                                                  _  ->  case iNodeOnLowerLevel of
                                                                           (Just (resINodeSet, resINode)) -> case ancestorsOf resINodeSet of
                                                                                                               [] -> myError
                                                                                                               [x] -> fromMaybe (error "nope!") $ pathInner x myENodeSet resINode
                                                                                                               _ -> error "got nowhere."
                                                                           Nothing -> myError
          where
            result = findENodeByOutput myENodeSet pLineToFollow
            returnResult eNode = ([outAndErrOf eNode], [], eNode)
            iNodeOnThisLevel = findINodeByOutput myINodeSet pLineToFollow False
            iNodeOnLowerLevel = maybeOnlyOneJust $ (\a -> findINodeByOutput a pLineToFollow True) <$> ancestorsOf (fromJust iNodeSet)
            pLineToFollow = case direction of
                              Head -> fst $ firstPLine
                              Last -> fst $ SL.last (cons secondPLine morePLines)
            myError = error $ "could not find enode for " <> show pLineToFollow <> "\n"
                           <> show myENodeSet <> "\n"
                           <> show myINodeSet <> "\n"

-- | Find an exterior Node with an output of the PLine given.
findENodeByOutput :: ENodeSet -> ProjectiveLine -> Maybe ENode
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
  findENodeOnSideByOutput :: Side -> ProjectiveLine -> Maybe ENode
  findENodeOnSideByOutput (Side (firstENode,moreENodes)) myPlineOut = case nodesMatching of
                                                                 (Slist [] _) -> Nothing
                                                                 (Slist [oneNode] _) -> Just oneNode
                                                                 (Slist (_:_) _)->  error "more than one exterior node with the same PLine out!"
    where
      nodesMatching = SL.filter (\eNode -> outOf eNode == myPlineOut) (cons firstENode moreENodes)

-- | Find an INode that has an output of the PLine given. Check the most recent generation, and if recurse is set, check previous generations.
--   Also returns the generation the INode was found in, it's generation, and it's prior generations.
findINodeByOutput :: INodeSet -> ProjectiveLine -> Bool -> Maybe (INodeSet, INode)
findINodeByOutput iNodeSet@(INodeSet _ parent) pLineOut recurse =
  case nodeMatching of
    Just iNode -> Just (iNodeSet, iNode)
    Nothing -> if recurse
                    then case ancestorsOf iNodeSet of
                           [] -> Nothing
                           [a] -> findINodeByOutput a pLineOut recurse
                           (xs) -> maybeOnlyOneJust $ (\a -> findINodeByOutput a pLineOut recurse) <$> xs
                    else Nothing
  where
    nodeMatching = if hasArc parent && outOf parent == pLineOut
                   then Just parent
                   else Nothing

maybeOnlyOneJust :: [Maybe a] -> Maybe a
maybeOnlyOneJust xs = case catMaybes xs of
                        [] -> Nothing
                        [a] -> Just a
                        _ -> error "too many!"

-- | a smart constructor for a NodeTree.
makeNodeTree :: [ENode] -> Maybe INodeSet -> NodeTree
makeNodeTree eNodes maybeINodeSet = case eNodes of
                                  [] -> NodeTree (ENodeSet (slist [])) maybeINodeSet
                                  [eNode] -> NodeTree (ENodeSet (slist [Side (eNode,slist [])])) maybeINodeSet
                                  (eNode:moreENodes) -> NodeTree (ENodeSet (slist [Side (eNode,slist moreENodes)])) maybeINodeSet

-- | Merge a set of NodeTrees together.
mergeNodeTrees :: [NodeTree] -> NodeTree
mergeNodeTrees nodeTrees =
  case nodeTrees of
    [] -> error "no nodeTrees to merge"
    [a] -> a
    [a,b] -> fromMaybe (error "could not merge") $ mergeTwoNodeTrees a b
    (a:bs) -> fromMaybe (error "could not merge") $ mergeTwoNodeTrees a $ mergeNodeTrees bs
  where
    mergeTwoNodeTrees :: NodeTree -> NodeTree -> Maybe NodeTree
    mergeTwoNodeTrees nodeTree1@(NodeTree eNodeSet1 iNodeSet1) nodeTree2@(NodeTree eNodeSet2 iNodeSet2) =
      Just $ NodeTree mergedENodeSets mergedINodeSets
      where
        mergedENodeSets = mergeENodeSets eNodeSet1 eNodeSet2
        mergedINodeSets
         | isJust iNodeSet1 && isJust iNodeSet2 = mergeINodeSets nodeTree1 nodeTree2
         | isJust iNodeSet1 && isJust (finalOutAndErrOf nodeTree2) && finalINodeOf (fromJust iNodeSet1) `iNodeHasIn` fromJust (finalOutAndErrOf nodeTree2) = iNodeSet1
         | isJust iNodeSet2 && isJust (finalOutAndErrOf nodeTree1) && finalINodeOf (fromJust iNodeSet2) `iNodeHasIn` fromJust (finalOutAndErrOf nodeTree1) = iNodeSet2
         | otherwise = Nothing -- error "nope."
    mergeINodeSets :: NodeTree -> NodeTree -> Maybe INodeSet
    mergeINodeSets myNodeTree1@(NodeTree eNodeSet1 _) myNodeTree2@(NodeTree eNodeSet2 _)
      -- FIXME: technically, one a cell can have a one sided ENodeSet, but have more than one side.
      | isOneSide eNodeSet1 && isOneSide eNodeSet2 = addOneSidedINodeSets myNodeTree1 myNodeTree2
      | otherwise = error "make me."
    addOneSidedINodeSets :: NodeTree -> NodeTree -> Maybe INodeSet
    addOneSidedINodeSets nt1@(NodeTree _ iNodeSet1) nt2@(NodeTree _ iNodeSet2)
      | isJust iNodeSet1 && isJust iNodeSet2 && isJust (finalOutAndErrOf nt1) && finalINodeOf (fromJust iNodeSet2) `iNodeHasIn` fromJust (finalOutAndErrOf nt1) = Just $ INodeSet (mergeAncestorsInOrder nt1 nt2 <> (slist [[finalINodeOf $ fromJust iNodeSet1]]) ) (finalINodeOf $ fromJust iNodeSet2)
      | isJust iNodeSet1 && isJust iNodeSet2 && isJust (finalOutAndErrOf nt2) && finalINodeOf (fromJust iNodeSet1) `iNodeHasIn` fromJust (finalOutAndErrOf nt2) = Just $ INodeSet (mergeAncestorsInOrder nt2 nt1 <> (slist [[finalINodeOf $ fromJust iNodeSet2]]) ) (finalINodeOf $ fromJust iNodeSet1)
      | isJust iNodeSet1 && isJust iNodeSet2 = Nothing -- error $ "what do we do here?\n" <> show nt1 <> "\n" <> show nt2 <> "\n"
      | otherwise = error $ "cannot merge two NodeTrees without outputs.\n" <> show nt1 <> "\n" <> show nt2 <> "\n"
      where
        -- Create a merged set of ancestors.
        -- skips the final inodes.
        mergeAncestorsInOrder :: NodeTree -> NodeTree -> Slist [INode]
        mergeAncestorsInOrder (NodeTree myENodeSet1 myINodeSet1) (NodeTree myENodeSet2 myINodeSet2) =
              case compareSides (oneSideOf myENodeSet1) (oneSideOf myENodeSet2) of
                FirstLast -> slist $ mergeChildren
                                       (if isJust myINodeSet2 then init $ allINodesOf $ fromJust myINodeSet2 else mempty)
                                       (if isJust myINodeSet1 then init $ allINodesOf $ fromJust myINodeSet1 else mempty)
                LastFirst -> slist $ mergeChildren
                                       (if isJust myINodeSet1 then init $ allINodesOf $ fromJust myINodeSet1 else mempty)
                                       (if isJust myINodeSet2 then init $ allINodesOf $ fromJust myINodeSet2 else mempty)
                NoMatch -> error "failed to connect"
        mergeChildren :: Slist [INode] -> Slist [INode] -> [[INode]]
        mergeChildren set1 set2
          | null set1 = (\(Slist a _) -> a) set2
          | null set2 = (\(Slist a _) -> a) set1
          | null (SL.init set1) && null (SL.init set2) = [SL.last set1 <> SL.last set2]
          | otherwise = mergeChildren (SL.init set1) (SL.init set2) <> [SL.last set1 <> SL.last set2]

mergeENodeSets :: ENodeSet -> ENodeSet -> ENodeSet
mergeENodeSets myENodeSet1@(ENodeSet sides1) myENodeSet2@(ENodeSet sides2)
  | isEmpty sides1 && isEmpty sides2 = myENodeSet1
  | not (isEmpty sides1) && isEmpty sides2 = myENodeSet1
  | len sides1 == 0 && len sides2 /= 0 = myENodeSet2
  | isOneSide myENodeSet1 = addENodeSetToOneSide myENodeSet2 (oneSideOf myENodeSet1)
  | isOneSide myENodeSet2 = addENodeSetToOneSide myENodeSet1 (oneSideOf myENodeSet2)
  | otherwise = error "unsure how to merge."
      where
        addENodeSetToOneSide :: ENodeSet -> Side -> ENodeSet
        addENodeSetToOneSide eNodeSet@(ENodeSet rawSides) oneSide = ENodeSet resultSides
          where
            resultSides = cons newSide $ SL.filter (`notElem` matchSides) rawSides
            -- return the sides that have a match (forward or backwards) with the given side.
            matchSides :: [Side]
            matchSides
              | null matchFirst && null matchLast = error $ "no way to connect:\n" <> show oneSide <> "\n" <> show eNodeSet <> "\n"
              | otherwise = matchFirst <> matchLast
            -- find a side that connects to our side's first ENode.
            matchFirst = zeroOrOne $ filter (\a -> compareSides a oneSide == FirstLast) $ sidesOfENodeSet eNodeSet
            -- find a side that connects to our side's last ENode.
            matchLast = zeroOrOne $ filter (\a -> compareSides a oneSide == LastFirst) $ sidesOfENodeSet eNodeSet
            -- construct a new side, including matchSides and the oneSide given.
            newSide :: Side
            newSide
              | matchFirst == [] && matchLast == [] = error $ "no way to connect\n" <> show eNodeSet <> "\n" <> show oneSide <> "\n"
              | otherwise = makeSide $ (concat $ eNodesOfSide <$> matchFirst) <> eNodesOfSide oneSide <> (concat $ eNodesOfSide <$> matchLast)

compareSides :: Side -> Side -> MaybeMatch
compareSides side1 side2
  | checkForFollower (lastOfSide side1) (firstOfSide side2) = FirstLast
  | checkForFollower (lastOfSide side2) (firstOfSide side1) = LastFirst
  | otherwise = NoMatch
  where
    checkForFollower eNode1 eNode2 = getLastLineSeg eNode1 == getFirstLineSeg eNode2

zeroOrOne :: Slist a -> [a]
zeroOrOne (Slist vals _) = case vals of
                             [] -> []
                             [a] -> [a]
                             _ -> error "found too many candidates."

sidesOfENodeSet :: ENodeSet -> (Slist Side)
sidesOfENodeSet (ENodeSet sides) = sides

firstOfSide :: Side -> ENode
firstOfSide (Side (firstSeg, _)) = firstSeg

lastOfSide :: Side -> ENode
lastOfSide (Side (first, Slist [] _)) = first
lastOfSide (Side (_, moreENodes)) = SL.last moreENodes

