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

import Prelude ((==), otherwise, (<$>), ($), length, error, (<>), show, Eq, Show, (<>), Bool(True), null, not, and, snd, (&&), (>), (/=), mempty)

import Data.List (uncons)

import Data.List.Extra (unsnoc)

import Data.Maybe (isNothing, fromMaybe, Maybe(Just, Nothing), isJust)

import Safe (initSafe)

import Slist.Type (Slist(Slist))

import Slist (slist, isEmpty, len, init, tail, take, dropWhile, head, one, last)

import Slist as SL (reverse)

import Graphics.Slicer.Math.Definitions (LineSeg, distance, fudgeFactor)

import Graphics.Slicer.Math.Intersections (isCollinear)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode, INode(INode), ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), getFirstLineSeg, getLastLineSeg, finalINodeOf, finalOutOf, ancestorsOf, firstInOf, lastInOf, sortedPLines)

import Graphics.Slicer.Math.Skeleton.NodeTrees (lastSegOf, findENodeByOutput, findINodeByOutput, firstSegOf, lastENodeOf, firstENodeOf, pathFirst, pathLast)

import Graphics.Slicer.Math.PGA (ProjectiveLine, Arcable(hasArc, outOf), ePointOf, outAndErrOf)

--------------------------------------------------------------------
-------------------------- Face Placement --------------------------
--------------------------------------------------------------------

-- | A Face:
--   A portion of a contour, with a real side, and arcs (line segments between nodes) dividing it from other faces.
--   Faces have no holes, and their arcs and nodes (line segments and points) are generated from a StraightSkeleton of a Contour.
-- FIXME: these arcs should be Arcable.
data Face = Face { _edge :: !LineSeg, _firstArc :: !ProjectiveLine, _arcs :: !(Slist ProjectiveLine), _lastArc :: !ProjectiveLine }
  deriving Eq
  deriving stock Show

-- | take a straight skeleton, and create faces from it.
-- accepts a line segment you want the first face to contain, and reorders the face list.
orderedFacesOf :: LineSeg -> StraightSkeleton -> Slist Face
orderedFacesOf start skeleton
 | isEmpty res = error $ "could not find the first face?\nLineSeg: " <> show start <> "\nFound faces: " <> show (facesOf skeleton) <> "\nStraightSkeleton: " <> show skeleton <> "\n"
 | otherwise = res
  where
    res = facesFromIndex start $ facesOf skeleton
    facesFromIndex :: LineSeg -> Slist Face -> Slist Face
    facesFromIndex targetSeg rawFaces = take (length rawFaces) $ dropWhile (\(Face a _ _ _) -> a /= targetSeg) $ rawFaces <> rawFaces

-- | take a straight skeleton, and create faces from it.
facesOf :: StraightSkeleton -> Slist Face
facesOf straightSkeleton@(StraightSkeleton nodeLists spine)
  | isEmpty nodeLists = nodeListError
  | len nodeLists == 1 && null spine = findFaces (head nodeLists)
  | not $ null spine = error "cannot yet handle spines, or more than one NodeList."
  | otherwise = error "whoops. don't know how we got here."
  where
    nodeListError = error "cannot handle anything other than one NodeList in a straight skeleton."
    -- find all of the faces of a set of nodeTrees.
    findFaces :: [NodeTree] -> Slist Face
    findFaces nodeTrees = slist $ case nodeTrees of
                                    [] -> []
                                    [oneNodeTree] -> if isNothing (finalOutOf oneNodeTree)
                                                     then rawFaces
                                                     else error $ "Only one NodeTree given, and it has an output arc. Don't know how to continue: " <> show straightSkeleton <> "\n"
                                    (_:_) -> rawFaces
      where
        rawFaces = case nodeTrees of
                     [] -> []
                     [firstNodeTree] -> facesOfNodeTree firstNodeTree
                     [firstNodeTree, secondNodeTree] -> facesOfNodeTree firstNodeTree <> [intraNodeFace firstNodeTree secondNodeTree] <> facesOfNodeTree secondNodeTree <> [intraNodeFace secondNodeTree firstNodeTree]
                     (firstNodeTree: _ : moreNodeTrees) -> case unsnoc moreNodeTrees of
                                                        Nothing -> error "empty node tree?"
                                                        Just (_,lastNodeTree) -> findFacesRecurse nodeTrees <> [intraNodeFace lastNodeTree firstNodeTree]

        -- Recursively find faces. will never find the face that closes the loop. that's for the caller to do.
        findFacesRecurse :: [NodeTree] -> [Face]
        findFacesRecurse myNodeTrees = case myNodeTrees of
                                         [] -> error "Impossible. cannot happen."
                                         [firstNodeTree] -> facesOfNodeTree firstNodeTree
                                         [firstNodeTree,secondNodeTree] -> facesOfNodeTree firstNodeTree <> [intraNodeFace firstNodeTree secondNodeTree] <> facesOfNodeTree secondNodeTree
                                         (firstNodeTree:secondNodeTree:xs) -> facesOfNodeTree firstNodeTree <> [intraNodeFace firstNodeTree secondNodeTree] <> findFacesRecurse (secondNodeTree:xs)

-- | Create a set of faces from a single nodetree.
facesOfNodeTree :: NodeTree -> [Face]
facesOfNodeTree nodeTree@(NodeTree myENodes iNodeSet@(INodeSet generations))
  | isEmpty generations = error $ "Cannot find faces when given an empty nodeTree: " <> show nodeTree
  | otherwise = areaBeneath myENodes (ancestorsOf iNodeSet) $ finalINodeOf iNodeSet
  where
    -- cover the space occupied by all of the ancestors of this node with a series of faces.
    areaBeneath :: ENodeSet -> INodeSet -> INode -> [Face]
    areaBeneath eNodes (INodeSet myGenerations) target
      | isEmpty myGenerations = -- no ancestor generations.
          if allInsAreENodes target
          then if hasArc target
               then -- skip the last triangle, as the target's output is somewhere within it.
                 initSafe $ rotateFaces iNodeSet myENodes target
               else rotateFaces iNodeSet myENodes target
          else errorNoMoreINodes
      | otherwise = -- one or more ancestor generations
        if hasArc target
        then initSafe $ rotateFaces iNodeSet myENodes target
        else rotateFaces iNodeSet myENodes target
      where
        errorNoMoreINodes = error "one target, no generations, and target needs inodes?\n"
         -- Check the ins of an INode, and make sure all of them point to an ENode.
        allInsAreENodes :: INode -> Bool
        allInsAreENodes myTarget = and $ isJust <$> (findENodeByOutput eNodes <$> inArcsOf myTarget)
          where
            -- Make a list of an INode's input arcs.
            inArcsOf (INode firstArc secondArc (Slist rawMoreArcs _) _) = firstArc : secondArc : rawMoreArcs

-- | wrap getFaces so the first line segment of the input set is the first face given.
rotateFaces :: INodeSet -> ENodeSet -> INode -> [Face]
rotateFaces iNodeSet eNodes iNode = rTail <> [rHead]
  where
    -- note that we place the first face last, because the first ENode is constructed from the first segment, which shifts the order of face placement one forward. this is to correct for that effect.
    (rHead, rTail) = case uncons (getFaces iNodeSet eNodes iNode) of
                       Nothing -> error "wtf?"
                       (Just (a,b)) -> (a,b)

-- | Get the faces for all of the NodeTree under the given INode.
-- uses a recursive resolver, and sometimes calls itsself, making it a co-recursive algorithm..
getFaces :: INodeSet -> ENodeSet -> INode -> [Face]
getFaces iNodeSet@(INodeSet myGenerations) eNodes iNode = findFacesRecurse iNode allPLines
  where
    allPLines = sortedPLines $ insOf iNode <> if hasArc iNode then [outOf iNode] else []
      where
        insOf (INode pLine1 pLine2 (Slist morePLines _) _) = pLine1 : pLine2 : morePLines
    firstPLine = head $ slist allPLines
    -- responsible for placing faces under the first pline given (if applicable), and between that pline, and the following pline. then.. recurse!
    findFacesRecurse :: INode -> [ProjectiveLine] -> [Face]
    findFacesRecurse myINode pLines =
      case pLines of
        [] -> error "we should never get here."
        -- Just one PLine? assume we're the last one. do not place a face, but do place faces under the PLine.
        [onePLine] -> placeFacesBeneath onePLine firstPLine
                      <> placeFaceBetween onePLine firstPLine
        -- More than one PLine? place faces under onePLine, place a face between onePLine and anotherPLine, and recurse!
        (onePLine : anotherPLine : myMorePLines) -> placeFacesBeneath onePLine anotherPLine
                                                    <> placeFaceBetween onePLine anotherPLine
                                                    <> findFacesRecurse myINode (anotherPLine:myMorePLines)
      where
        -- zero or one face, not a real list.
        placeFaceBetween :: ProjectiveLine -> ProjectiveLine -> [Face]
        placeFaceBetween onePLine anotherPLine
         | hasArc myINode && isCollinear (outAndErrOf myINode) (onePLine, mempty)     = [] -- don't place faces along the incoming PLine. the caller does that.
         | hasArc myINode && isCollinear (outAndErrOf myINode) (anotherPLine, mempty) = [] -- don't place faces along the incoming PLine. the caller does that.
         | otherwise = [areaBetween iNodeSet eNodes onePLine anotherPLine]
        placeFacesBeneath :: ProjectiveLine -> ProjectiveLine -> [Face]
        placeFacesBeneath onePLine anotherPLine
         | isENode eNodes onePLine                         = [] -- don't climb down an enode, you're done
         | hasArc myINode && isCollinear (outAndErrOf myINode) (onePLine, mempty)     = [] -- don't try to climb back up the tree
         | hasArc myINode && isCollinear (outAndErrOf myINode) (anotherPLine, mempty) = [] -- don't try to climb back up the tree
         | len myGenerations == 0 = error $ "wtf!\n" <> show onePLine <> "\n" <> show eNodes <> "\n" <> show iNodeSet
         | otherwise = getFaces (ancestorsOf iNodeSet) eNodes $ firstINodeOfPLine iNodeSet eNodes onePLine

-- | Create a face covering the space between two PLines with a single Face. Both PLines must be a part of the same INode.
areaBetween :: INodeSet -> ENodeSet -> ProjectiveLine -> ProjectiveLine -> Face
areaBetween _ (ENodeSet (Slist [] _)) _ _ = error "no sides?"
areaBetween _ (ENodeSet (Slist (_:_:_) _)) _ _ = error "too many sides?"
areaBetween iNodeSet eNodeSet@(ENodeSet (Slist [(_,_)] _)) pLine1 pLine2
  | reverseTriangle = fromMaybe errNodesNotNeighbors $
                      makeFace (lastDescendent iNodeSet eNodeSet pLine1) (SL.reverse (pathToLastDescendent iNodeSet eNodeSet pLine2) <> pathToFirstDescendent iNodeSet eNodeSet pLine1) (firstDescendent iNodeSet eNodeSet pLine2)
  | otherwise       = fromMaybe errNodesNotNeighbors $
                      makeFace (firstDescendent iNodeSet eNodeSet pLine1) (SL.reverse (pathToFirstDescendent iNodeSet eNodeSet pLine2) <> pathToLastDescendent iNodeSet eNodeSet pLine1) (lastDescendent iNodeSet eNodeSet pLine2)
  where
    -- Detect the case where we are creating a face across the open end of the contour.
    reverseTriangle = distance (ePointOf $ lastDescendent iNodeSet eNodeSet pLine1) (ePointOf $ firstDescendent iNodeSet eNodeSet pLine2) > fudgeFactor
    -- our error condition.
    errNodesNotNeighbors = error $ "cannot make a face from nodes that are not neighbors: \n" <> show eNodeSet <> "\n" <> show pLine1 <> "\n" <> show pLine2 <> "\n"

-- | Create a Face covering the space between two NodeTrees. like areaBetween, but for two separate NodeTrees.
intraNodeFace :: NodeTree -> NodeTree -> Face
intraNodeFace nodeTree1 nodeTree2
  | nodeTree1 `isLeftOf` nodeTree2  = if nodeTree1 `follows` nodeTree2
                                      then fromMaybe errNodesNotNeighbors $
                                             makeFace (firstENodeOf nodeTree1) (init (lastPLinesOf nodeTree1) <> tail (tail $ SL.reverse $ firstPLinesOf nodeTree2)) (lastENodeOf nodeTree2)
                                      else fromMaybe errNodesNotNeighbors $
                                             makeFace (firstENodeOf nodeTree1) (init (lastPLinesOf nodeTree1) <>       tail  (SL.reverse $ firstPLinesOf nodeTree2)) (lastENodeOf nodeTree2)
  | nodeTree1 `isRightOf` nodeTree2 = if nodeTree2 `follows` nodeTree1
                                      then fromMaybe errNodesNotNeighbors $
                                             makeFace (lastENodeOf nodeTree2) (init (firstPLinesOf nodeTree2) <> tail (tail $ SL.reverse $ lastPLinesOf nodeTree1)) (firstENodeOf nodeTree1)
                                      else fromMaybe errNodesNotNeighbors $
                                             makeFace (firstENodeOf nodeTree2) (init (firstPLinesOf nodeTree2) <>       tail  (SL.reverse $ lastPLinesOf nodeTree1)) (lastENodeOf nodeTree1)
  | nodeTree1 == nodeTree2          = error $ "two identical nodes given.\n" <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n"
  | otherwise = error $ "Two NodeTrees given that are not neighbors: " <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n"
  where
    errNodesNotNeighbors = error $ "cannot make a face from nodes that are not neighbors: \n" <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n"
    follows :: NodeTree -> NodeTree -> Bool
    follows nt1 nt2 = isCollinear (last $ firstPLinesOf nt1, mempty) (last $ lastPLinesOf nt2, mempty)
    isLeftOf :: NodeTree -> NodeTree -> Bool
    isLeftOf nt1 nt2 = firstSegOf nt1 == lastSegOf nt2
    isRightOf :: NodeTree -> NodeTree -> Bool
    isRightOf nt1 nt2 = lastSegOf nt1 == firstSegOf nt2
    lastPLinesOf :: NodeTree -> Slist ProjectiveLine
    lastPLinesOf nodeTree = slist $ (\(a,_,_) -> a) $ pathLast nodeTree
    firstPLinesOf :: NodeTree -> Slist ProjectiveLine
    firstPLinesOf nodeTree = slist $ (\(a,_,_) -> a) $ pathFirst nodeTree

-- Find the bottom ENode going down the last paths from the given ProjectiveLine.
lastDescendent :: INodeSet -> ENodeSet -> ProjectiveLine -> ENode
lastDescendent iNodeSet eNodeSet pLine
  | null $ pathToLastDescendent iNodeSet eNodeSet pLine = -- handle the case where we're asked for an ENode's output.
      fromMaybe (error "could not find ENode!") $ findENodeByOutput eNodeSet pLine
  | otherwise = fromMaybe (error $ "could not find ENode!\nLooking for: " <> show pLine <> "\nIn InodeSet: " <> show iNodeSet <> "\n") $ findENodeByOutput eNodeSet $ lastInOf $ lastINodeOfPLine iNodeSet eNodeSet pLine

-- Find the bottom ENode going down the first paths from the given ProjectiveLine.
firstDescendent :: INodeSet -> ENodeSet -> ProjectiveLine -> ENode
firstDescendent iNodeSet eNodeSet pLine
  | null $ pathToFirstDescendent iNodeSet eNodeSet pLine = -- handle the case where we're asked for an ENode's output.
      fromMaybe (error "could not find ENode!") $ findENodeByOutput eNodeSet pLine
  | otherwise = fromMaybe (error "could not find ENode!") $ findENodeByOutput eNodeSet $ firstInOf $ firstINodeOfPLine iNodeSet eNodeSet pLine

-- | find the First INode of a PLine.
firstINodeOfPLine :: INodeSet -> ENodeSet -> ProjectiveLine -> INode
firstINodeOfPLine iNodeSet eNodeSet pLine = snd $ fromMaybe (error $ "could not find INode!\nLooking for: " <> show pLine <> "\nIn InodeSet: " <> show iNodeSet <> "\n") $ findINodeByOutput iNodeSet (last $ pathToFirstDescendent iNodeSet eNodeSet pLine) True

-- | find the last INode of a PLine.
lastINodeOfPLine :: INodeSet -> ENodeSet -> ProjectiveLine -> INode
lastINodeOfPLine iNodeSet eNodeSet pLine = snd $ fromMaybe (error "could not find INode!") $ findINodeByOutput iNodeSet (last $ pathToLastDescendent iNodeSet eNodeSet pLine) True

-- | Find the parent inode of a given ProjectiveLine.
parentINodeOfPLine :: INodeSet -> ProjectiveLine -> INode
parentINodeOfPLine iNodeSet pLine = snd $ fromMaybe (error $ "could not find INode!\nLooking for: " <> show pLine <> "\nIn INodeSet: " <> show iNodeSet <> "\n") $ findINodeByOutput iNodeSet pLine True

-- | Determine if a PLine matches the output of an ENode.
isENode :: ENodeSet -> ProjectiveLine -> Bool
isENode eNodes pLine = isJust $ findENodeByOutput eNodes pLine

-- | Travel down the INode tree, taking the 'First' path, marking down all of the pLine2s as we go, until we get to the bottom.
pathToFirstDescendent :: INodeSet -> ENodeSet -> ProjectiveLine -> Slist ProjectiveLine
pathToFirstDescendent iNodeSet eNodeSet pLine
  | isENode eNodeSet pLine = slist []
  | otherwise = one pLine <> pathToFirstDescendent iNodeSet eNodeSet (firstInOf $ parentINodeOfPLine iNodeSet pLine)

-- | Travel down the INode tree, taking the 'Last' path, marking down all of the pLine2s as we go, until we get to the bottom.
pathToLastDescendent :: INodeSet -> ENodeSet -> ProjectiveLine -> Slist ProjectiveLine
pathToLastDescendent iNodeSet eNodeSet pLine
  | isENode eNodeSet pLine = slist []
  | otherwise = one pLine <> pathToLastDescendent iNodeSet eNodeSet (lastInOf $ parentINodeOfPLine iNodeSet pLine)

-- | Construct a face from two nodes, and a set of arcs. the nodes must follow each other on the contour.
makeFace :: ENode -> Slist ProjectiveLine -> ENode -> Maybe Face
makeFace e1 arcs e2
  | getLastLineSeg e1 == getFirstLineSeg e2 = Just $ Face (getFirstLineSeg e2) (outOf e2) arcs (outOf e1)
  | getFirstLineSeg e1 == getLastLineSeg e2 = Just $ Face (getFirstLineSeg e1) (outOf e1) arcs (outOf e2)
  | otherwise = error $ "failed to match inputs:\nE1: " <> show e1 <> "\nE2: " <> show e2 <> "\n"

