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

import Prelude ((==), otherwise, (<$>), ($), length, (/=), error, (<>), show, Eq, Show, (<>), Bool(True, False), null, not, and, snd, (&&))

import Data.List.Extra (unsnoc)

import Data.Maybe (isNothing, fromMaybe, Maybe(Just, Nothing), isJust)

import Safe (initSafe)

import Slist.Type (Slist(Slist))

import Slist (slist, cons, isEmpty, len, init, tail, take, dropWhile, head, one)

import Slist as SL (last, reverse)

import Graphics.Slicer.Math.Definitions (LineSeg)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode(ENode), INode(INode), ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), Arcable(hasArc), finalINodeOf, finalOutOf, ancestorsOf)

import Graphics.Slicer.Math.Skeleton.NodeTrees (lastSegOf, findENodeByOutput, findINodeByOutput, firstSegOf, lastENodeOf, firstENodeOf, pathFirst, pathLast)

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
orderedFacesOf :: LineSeg -> StraightSkeleton -> Slist Face
orderedFacesOf start skeleton = facesFromIndex start $ facesOf skeleton
  where
    facesFromIndex :: LineSeg -> Slist Face -> Slist Face
    facesFromIndex targetSeg rawFaces = take (length rawFaces) $ dropWhile (\(Face a _ _ _) -> a /= targetSeg) $ rawFaces <> rawFaces

-- | take a straight skeleton, and create faces from it.
facesOf :: StraightSkeleton -> Slist Face
facesOf straightSkeleton@(StraightSkeleton nodeLists spine)
  | len nodeLists == 0 = nodeListError
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

-- | Create a face covering the space between two NodeTrees. like areaBetween, but for two separate NodeTrees.
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
    areaBeneath eNodes myINodeSet@(INodeSet myGenerations) target
      | len myGenerations == 0 = -- no ancestor generations.
          if allInsAreENodes target
          then if hasArc target
               then -- skip the last triangle, as the target's output is somewhere within it.
                 initSafe $ getFaces target
               else getFaces target
          else errorNoMoreINodes
      | otherwise = -- one or more ancestor generations
        if hasArc target
        then initSafe $ getFaces target
        else getFaces target
      where
         -- Check the ins of an inode, and make sure all of them point to as ENode.
        allInsAreENodes :: INode -> Bool
        allInsAreENodes myTarget = and $ isJust <$> (findENodeByOutput eNodes <$> inArcsOf myTarget)
          where
            -- Make a list of an INode's input arcs.
            inArcsOf (INode firstArc secondArc (Slist rawMoreArcs _) _)= firstArc : secondArc : rawMoreArcs

        -- Get the faces for all of the nodetree under the given INode.
        getFaces (INode firstPLine secondPLine morePLines@(Slist morePLinesRaw _) _)
          | len morePLines == 0 = -- Just a pair
            case (isENode firstPLine,isENode secondPLine) of
            (True, True) -> areaBeneathPair firstPLine secondPLine
                            <> areaBeneathPair secondPLine firstPLine
            (True, False) -> areaBeneathPair firstPLine secondPLine
                            <> areaBeneath eNodes (ancestorsOf myINodeSet) secondINode
                            <> areaBeneathPair secondPLine firstPLine
            (False, True) -> areaBeneath eNodes (ancestorsOf myINodeSet) firstINode
                             <> areaBeneathPair firstPLine secondPLine
                             <> areaBeneathPair secondPLine firstPLine
            (False, False) -> areaBeneath eNodes (ancestorsOf myINodeSet) firstINode
                             <> areaBeneathPair firstPLine secondPLine
                             <> areaBeneath eNodes (ancestorsOf myINodeSet) secondINode
                             <> areaBeneathPair secondPLine firstPLine
          | otherwise = -- three or more input PLines
            resHead -- call the recursive resolver, and place the last face, completing the contour.
            <> findFacesRecurse target (secondPLine : morePLinesRaw)
            <> areaBeneathPair lastPLine firstPLine
            where
              firstINode = snd $ fromMaybe (error "could not find INode!") $ findINodeByOutput myINodeSet firstPLine True
              secondINode = snd $ fromMaybe (error "could not find INode!") $ findINodeByOutput myINodeSet secondPLine True
              resHead -- The first part of the result.
                | isENode firstPLine && isENode secondPLine = areaBeneathPair firstPLine secondPLine
                | isENode firstPLine = areaBeneathPair firstPLine secondPLine
                | isENode secondPLine = areaBeneath eNodes (ancestorsOf myINodeSet) firstINode
                                        <> areaBeneathPair firstPLine secondPLine
                | otherwise = areaBeneath eNodes (ancestorsOf myINodeSet) firstINode
                              <> areaBeneathPair firstPLine secondPLine
              lastPLine = case unsnoc morePLinesRaw of
                            Nothing -> errorImpossible
                            (Just (_,finalPLine)) -> finalPLine
              findFacesRecurse :: INode -> [PLine2] -> [Face]
              findFacesRecurse myINode pLines =
                case pLines of
                  [] -> []
                  [onePLine] -> if isENode onePLine
                                then []
                                else areaBeneath eNodes (ancestorsOf myINodeSet) $ iNodeOfPLine onePLine
                    where
                      iNodeOfPLine myPLine = snd $ fromMaybe (error "could not find INode!") $ findINodeByOutput myINodeSet myPLine True
                  (onePLine : anotherPLine : myMorePLines) -> recurse
                    where
                      recurse = areaBeneathPair onePLine anotherPLine
                                <> findFacesRecurse myINode (anotherPLine:myMorePLines)
        -- Find the area beneath two PLines, which share a common INode ancestor.
        -- Should include the area between the two PLines, and the area under the second one, if it's an INode.
        areaBeneathPair :: PLine2 -> PLine2 -> [Face]
        areaBeneathPair pLine1 pLine2
         | isENode pLine1 && isENode pLine2 = -- both enodes? make a triangle.
           [makeTriangleFace myENode1 myENode2]
         | isENode pLine1 = -- only pLine1 is an ENode.
           [fromMaybe errorMaybeFailPLine1 $ makeFace myENode1 (pathToFirstDescendent pLine2) (firstDescendent pLine2)]
           <> areaBeneath eNodes (ancestorsOf myINodeSet) (firstINodeOfPLine pLine2)
         | isENode pLine2 = -- only pLine2 is an ENode.
           [fromMaybe errorMaybeFailPLine2 $ makeFace (lastDescendent pLine1) (pathToLastDescendent pLine1) myENode2]
         | otherwise = [areaBetween eNodes (firstINodeOfPLine pLine2) pLine1 pLine2]
         where
           myENode1 = fromMaybe (error "could not find ENode!") $ findENodeByOutput eNodes pLine1
           myENode2 = fromMaybe (error "could not find ENode!") $ findENodeByOutput eNodes pLine2
           pathToFirstDescendent :: PLine2 -> Slist PLine2
           pathToFirstDescendent myPLine
            | isENode myPLine = slist []
            | otherwise = one myPLine <> pathToFirstDescendent (firstInOf $ parentINodeOfPLine myPLine)
           pathToLastDescendent :: PLine2 -> Slist PLine2
           pathToLastDescendent myPLine
            | isENode myPLine = slist []
            | otherwise = one myPLine <> pathToLastDescendent (lastInOf $ parentINodeOfPLine myPLine)
           firstDescendent myPLine
             | null $ pathToFirstDescendent myPLine = -- handle the case where we're asked for an ENode's output.
               fromMaybe (error "could not find ENode!") $ findENodeByOutput eNodes myPLine
             | otherwise = fromMaybe (error "could not find ENode!") $ findENodeByOutput eNodes $ firstInOf $ firstINodeOfPLine myPLine
           lastDescendent myPLine
             | null $ pathToLastDescendent myPLine = -- handle the case where we're asked for an ENode's output.
               fromMaybe (error "could not find ENode!") $ findENodeByOutput eNodes myPLine
             | otherwise = fromMaybe (error "could not find ENode!") $ findENodeByOutput eNodes $ lastInOf $ lastINodeOfPLine myPLine
           errorMaybeFailPLine1 = error
                                  $ "got Nothing from makeFace for PLine1\n"
                                  <> show pLine1 <> "\n"
                                  <> show (lastDescendent pLine1) <> "\n" <> show (isENode pLine1) <> "\n"
                                  <> show (pathToLastDescendent pLine1) <> "\n"
                                  <> show (pathToFirstDescendent pLine2) <> "\n"
                                  <> show (firstDescendent pLine2) <> "\n" <> show (isENode pLine2) <> "\n"
                                  <> show pLine2 <> "\n"
                                  <> show myENodes <> "\n" <> show iNodeSet <> "\n"
           errorMaybeFailPLine2 = error
                                  $ "got Nothing from makeFace for PLine2\n"
                                  <> show pLine1 <> "\n"
                                  <> show (lastDescendent pLine1) <> "\n" <> show (isENode pLine1) <> "\n"
                                  <> show (pathToLastDescendent pLine1) <> "\n"
                                  <> show (pathToFirstDescendent pLine2) <> "\n"
                                  <> show (firstDescendent pLine2) <> "\n" <> show (isENode pLine2) <> "\n"
                                  <> show pLine2 <> "\n"
                                  <> show myENodes <> "\n" <> show iNodeSet <> "\n"
           firstINodeOfPLine, lastINodeOfPLine, parentINodeOfPLine :: PLine2 -> INode
           firstINodeOfPLine myPLine = snd $ fromMaybe (error "could not find INode!") $ findINodeByOutput myINodeSet (SL.last $ pathToFirstDescendent myPLine) True
           lastINodeOfPLine myPLine = snd $ fromMaybe (error "could not find INode!") $ findINodeByOutput myINodeSet (SL.last $ pathToLastDescendent myPLine) True
           parentINodeOfPLine myPLine = snd $ fromMaybe (error "could not find INode!") $ findINodeByOutput myINodeSet myPLine True
           -- | Create a face covering the space between the two PLines with a single Face. Both nodes must have the same parent.
           areaBetween :: ENodeSet -> INode -> PLine2 -> PLine2 -> Face
           areaBetween (ENodeSet (Slist [] _)) _ _ _ = error "no sides?"
           areaBetween (ENodeSet (Slist (_:_:_) _)) _ _ _ = error "too many sides?"
           areaBetween myENodeList@(ENodeSet (Slist [(firstENode,moreENodes)] _)) parent myPLine1 myPLine2
             -- Handle the case where we are creating a face across the open end of the contour.
             -- FIXME: do we need the first reverse here?
             -- FIXME: we should tighten, and test this case
             | lastDescendent myPLine1 /= SL.last (cons firstENode moreENodes) =
                 fromMaybe errNodesNotNeighbors $
                 makeFace (lastDescendent myPLine1) (SL.reverse (pathToLastDescendent myPLine2) <> pathToFirstDescendent myPLine1) (firstDescendent myPLine2)
             | otherwise                                                       = fromMaybe errNodesNotNeighbors $
                                                                                 makeFace (firstDescendent myPLine1) (SL.reverse (pathToFirstDescendent myPLine2) <> pathToLastDescendent myPLine1) (lastDescendent myPLine2)
             where
               -- our error condition.
               errNodesNotNeighbors = error $ "cannot make a face from nodes that are not neighbors: \n" <> show myENodeList <> "\n" <> show parent <> "\n" <> show myPLine1 <> "\n" <> show myPLine2 <> "\n"

        -- Determine if a PLine matches the output of an ENode.
        isENode :: PLine2 -> Bool
        isENode myPLine = isJust $ findENodeByOutput eNodes myPLine

        -- Error conditions
        errorNoMoreINodes = error $ "one target, no generations, and target needs inodes?\n" <> show target <> "\n" <> show nodeTree <> "\n"
        errorImpossible = error "this will never be reached, the type system keeps this from happening?"

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

