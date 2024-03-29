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

import Prelude (Bool(True), Eq, Show, (==), (||), all, otherwise, (<$>), ($), length, error, (<>), show, null, not, and, snd, (&&), (.), (/=), fst)

import Data.Either (isRight)

import Data.List (filter, uncons)

import Data.List.Extra (unsnoc)

import Data.Maybe (isNothing, fromJust, fromMaybe, Maybe(Just, Nothing), isJust)

import Slist.Type (Slist(Slist))

import Slist (slist, isEmpty, len, init, tail, take, dropWhile, head, one, last)

import qualified Data.List as DL (head)

import qualified Slist as SL (reverse)

import Graphics.Slicer.Math.Definitions (LineSeg, mapWithFollower)

import Graphics.Slicer.Math.Intersections (noIntersection, intersectionBetween, isCollinear)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), ENode, INode(INode), ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), allPLinesOfINode, getFirstLineSeg, getLastLineSeg, finalINodeOf, finalOutOf, ancestorsOf, firstInOf, lastInOf)

import Graphics.Slicer.Math.Skeleton.NodeTrees (lastSegOf, findENodeByOutput, findINodeByOutput, firstSegOf, lastENodeOf, firstENodeOf, pathFirst, pathLast)

import Graphics.Slicer.Math.PGA (ProjectiveLine, PLine2Err, Arcable(hasArc), eToPL, flipL, outAndErrOf, pLineIsLeft)

--------------------------------------------------------------------
-------------------------- Face Placement --------------------------
--------------------------------------------------------------------

-- | A Face:
--   A portion of a contour, with a real side, and arcs (line segments between nodes) dividing it from other faces.
--   Faces have no holes, and their arcs and nodes (line segments and points) are generated from a StraightSkeleton of a Contour.
-- FIXME: these arcs should be Arcable.
data Face = Face { _edge :: !LineSeg, _firstArc :: !(ProjectiveLine, PLine2Err), _arcs :: !(Slist (ProjectiveLine, PLine2Err)), _lastArc :: !(ProjectiveLine, PLine2Err) }
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
  | len nodeLists == 1 && null spine = findDegenerates $ findFaces (head nodeLists)
  | not $ null spine = error "cannot yet handle spines, or more than one NodeList."
  | otherwise = error "whoops. don't know how we got here."
  where
    nodeListError = error "cannot handle anything other than one NodeList in a straight skeleton."
    -- | find all of the faces of a set of nodeTrees.
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
                     [firstNodeTree, secondNodeTree] -> facesOfNodeTree firstNodeTree <> (intraNodeFace firstNodeTree secondNodeTree : facesOfNodeTree secondNodeTree) <> [intraNodeFace secondNodeTree firstNodeTree]
                     (firstNodeTree: _ : moreNodeTrees) -> case unsnoc moreNodeTrees of
                                                        Nothing -> error "empty node tree?"
                                                        Just (_,lastNodeTree) -> findFacesRecurse nodeTrees <> [intraNodeFace lastNodeTree firstNodeTree]

        -- Recursively find faces. will never find the face that closes the loop. that's for the caller to do.
        findFacesRecurse :: [NodeTree] -> [Face]
        findFacesRecurse myNodeTrees = case myNodeTrees of
                                         [] -> error "Impossible. cannot happen."
                                         [firstNodeTree] -> facesOfNodeTree firstNodeTree
                                         [firstNodeTree,secondNodeTree] -> facesOfNodeTree firstNodeTree <> (intraNodeFace firstNodeTree secondNodeTree : facesOfNodeTree secondNodeTree)
                                         (firstNodeTree:secondNodeTree:xs) -> facesOfNodeTree firstNodeTree <> (intraNodeFace firstNodeTree secondNodeTree : findFacesRecurse (secondNodeTree:xs))

-- | Create a set of faces from a single nodetree.
facesOfNodeTree :: NodeTree -> [Face]
facesOfNodeTree nodeTree@(NodeTree eNodeSet iNodeSet)
  | isJust iNodeSet && isEmpty children = -- no ancestor generations.
      if allInsAreENodes parent
      then if hasArc parent
           then -- skip the last triangle, as the parent's output is somewhere within it.
             case unsnoc res of
               Nothing -> error "wtf"
               Just (xs,_) -> xs
           else res
      else errorNoMoreINodes
  | isJust iNodeSet = -- one or more ancestor generations
      if hasArc (finalINodeOf $ fromJust iNodeSet)
      then case unsnoc res of
             Nothing -> error "wtf"
             Just (xs,_) -> xs
      else res
  | otherwise = error "undefined!"
  where
    res = rotateFaces $ getFaces nodeTree parent
    parent = finalINodeOf $ fromJust iNodeSet
    children = childrenOf $ fromJust iNodeSet
    childrenOf :: INodeSet -> Slist [INode]
    childrenOf (INodeSet childGens _) = childGens
    errorNoMoreINodes = error "one parent, no generations, and parent needs inodes?\n"
    -- Check the ins of an INode, and make sure all of them point to an ENode.
    allInsAreENodes :: INode -> Bool
    allInsAreENodes myParent = and $ isJust <$> (findENodeByOutput eNodeSet <$> (fst <$> inArcsOf myParent))
      where
        -- Make a list of an INode's input arcs.
        inArcsOf (INode firstArc secondArc (Slist rawMoreArcs _) _) = firstArc : secondArc : rawMoreArcs

-- | wrap getFaces so the first line segment of the input set is the first face given.
rotateFaces :: [Face] -> [Face]
rotateFaces inFaces = rTail <> [rHead]
  where
    -- note that we place the first face last, because the first ENode is constructed from the first segment, which shifts the order of face placement one forward. this is to correct for that effect.
    (rHead, rTail) = case uncons inFaces of
                       Nothing -> error "wtf?"
                       (Just (a,b)) -> (a,b)

-- | Get the faces for all of the NodeTree under the given INode.
-- uses a recursive resolver, and sometimes calls itsself, making it a co-recursive algorithm..
getFaces :: NodeTree -> INode -> [Face]
getFaces (NodeTree eNodeSet iNodeSet) = getFaces' iNodeSet eNodeSet iNodeSet

-- | Get the faces for all of the NodeTree under the given INode.
-- May fail, as this is a recursive resolver.
getFaces' :: Maybe INodeSet -> ENodeSet -> Maybe INodeSet -> INode -> [Face]
getFaces' _ (ENodeSet (Slist [] _)) _ _ = error "no sides?"
getFaces' _ (ENodeSet (Slist (_:_:_) _)) _ _ = error "too many sides?"
getFaces' origINodeSet eNodeSet iNodeSet iNode = findFacesRecurse iNode myPLines
  where
    myPLines = (\(Slist a _) -> a) $ allPLinesOfINode iNode
    firstPLine = DL.head myPLines
    -- | responsible for placing faces under the first pline given (if applicable), and between that pline, and the following pline. then.. recurse!
    findFacesRecurse :: INode -> [(ProjectiveLine, PLine2Err)] -> [Face]
    findFacesRecurse myINode pLines =
      case pLines of
        [] -> error "we should never get here."
        -- Just one PLine? assume we're the last one. do not place a face, but do place faces under the PLine.
        [pLine] -> placeFacesBeneath pLine
                   <> placeFaceBetween pLine firstPLine
        -- More than one PLine? place faces under pLine1, place a face between pLine1 and pLine2, and recurse!
        (pLine1 : pLine2 : myMorePLines) -> placeFacesBeneath pLine1
                                            <> placeFaceBetween pLine1 pLine2
                                            <> findFacesRecurse myINode (pLine2:myMorePLines)
      where
        -- returns only zero or one face. not a real list.
        placeFaceBetween :: (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> [Face]
        placeFaceBetween pLine1 pLine2
         | hasArc myINode && isCollinear (outAndErrOf myINode) pLine1 = [] -- don't place faces along the incoming PLine. the caller does that.
         | hasArc myINode && isCollinear (outAndErrOf myINode) pLine2 = [] -- don't place faces along the incoming PLine. the caller does that.
         | getFirstLineSeg eNode1Last == getLastLineSeg eNode2First ||
           getLastLineSeg eNode1Last == getFirstLineSeg eNode2First = [areaBetween eNodeSet origINodeSet pLine1 eNode1Last pLine2 eNode2First]
         | getFirstLineSeg eNode1First == getLastLineSeg eNode2Last ||
           getLastLineSeg eNode1First == getFirstLineSeg eNode2Last = errBackwards -- [areaBetween eNodeSet origINodeSet pLine2 eNode2Last pLine1 eNode1First]
         | otherwise = errNotNeighbors
          where
             eNode1Last = lastDescendent eNodeSet origINodeSet pLine1
             eNode1First = firstDescendent eNodeSet origINodeSet pLine1
             eNode2First = firstDescendent eNodeSet origINodeSet pLine2
             eNode2Last = lastDescendent eNodeSet origINodeSet pLine2
             errNotNeighbors = error $ "asked to place a face between two ENodes that do not neighbor:\n"
                                     <> "ENode1Last: " <> show eNode1Last <> "\n"
                                     <> "ENode1First: " <> show eNode1First <> "\n"
                                     <> "ENode2First: " <> show eNode2First <> "\n"
                                     <> "ENode2Last: " <> show eNode2Last <> "\n"
                                     <> "origINodeSet: " <> show origINodeSet <> "\n"
                                     <> "myINode: " <> show myINode <> "\n"
                                     <> "pLines: " <> show pLines <> "\n"
                                     <> "pLine1: " <> show pLine1 <> "\n"
                                     <> "pLine2: " <> show pLine2 <> "\n"
             errBackwards = error $ "asked to place a face between two ENodes in reverse order:\n"
                                     <> "ENode1Last: " <> show eNode1Last <> "\n"
                                     <> "ENode1First: " <> show eNode1First <> "\n"
                                     <> "ENode2First: " <> show eNode2First <> "\n"
                                     <> "ENode2Last: " <> show eNode2Last <> "\n"
                                     <> "origINodeSet: " <> show origINodeSet <> "\n"
                                     <> "myINode: " <> show myINode <> "\n"
                                     <> "pLines: " <> show pLines <> "\n"
                                     <> "pLine1: " <> show pLine1 <> "\n"
                                     <> "pLine2: " <> show pLine2 <> "\n"
        placeFacesBeneath :: (ProjectiveLine, PLine2Err) -> [Face]
        placeFacesBeneath pLine
         | isENode eNodeSet (fst pLine)                              = [] -- don't climb down an enode, you're done
         | hasArc myINode && isCollinear (outAndErrOf myINode) pLine = [] -- don't try to climb back up the tree
         | isNothing iNodeSet = error "we need INodes here."
         | not $ null (ancestorsOf $ fromJust iNodeSet) = myGetFaces $ onlyOne $ filter (\a -> outAndErrOf (finalINodeOf a) == pLine) $ ancestorsOf (fromJust iNodeSet)
         | otherwise = error "no between to plant?"
          where
            onlyOne :: (Show a) => [a] -> a
            onlyOne [] = error $ "no item!\n" <> show pLine <> "\n" <> show iNodeSet <> "\n"
            onlyOne [a] = a
            onlyOne xs = error $ "too many items." <> show xs <> "\n"
            myGetFaces newINodeSet
              | isJust firstINode = getFaces' origINodeSet eNodeSet (Just newINodeSet) $ fromJust firstINode
              | otherwise = error "fail!"
              where
                -- FIXME: repair firstINodeOfPLine so it does not need the whole INodeSet.
                firstINode = firstINodeOfPLine eNodeSet (fromJust iNodeSet) pLine

-- | Create a single face covering the space between two PLine.
--   Both PLines must be a part of the same INode.
--   No other PLines from this iNode must travel between the two given PLines.
areaBetween :: ENodeSet -> Maybe INodeSet -> (ProjectiveLine, PLine2Err) -> ENode -> (ProjectiveLine, PLine2Err) -> ENode -> Face
areaBetween (ENodeSet (Slist [] _)) _ _ _ _ _ = error "no sides?"
areaBetween (ENodeSet (Slist (_:_:_) _)) _ _ _ _ _ = error "too many sides?"
areaBetween eNodeSet@(ENodeSet (Slist [_] _)) iNodeSet pLine1 eNode1 pLine2 eNode2
  | getFirstLineSeg eNode1 == getLastLineSeg eNode2 = faceOrError eNode1 (arcsToLastDescendent pLine1) (arcsToFirstDescendent pLine2) eNode2
  | getLastLineSeg eNode1 == getFirstLineSeg eNode2 = faceOrError eNode2 (arcsToLastDescendent pLine2) (arcsToFirstDescendent pLine1) eNode1
  | otherwise = error $ "found ENodes that do not follow.\n" <> show eNode1 <> "\n" <> show eNode2 <> "\n" <> show eNodeSet <> "\n" <> show iNodeSet <> "\n"
  where
    arcsToLastDescendent myPLine
      | getFirstLineSeg eNode1 == getLastLineSeg eNode2 = flipArc <$> pathToLastDescendent eNodeSet iNodeSet myPLine
      | getLastLineSeg eNode1 == getFirstLineSeg eNode2 = pathToLastDescendent eNodeSet iNodeSet myPLine
      | otherwise = error "found ENodes that do not follow."
    arcsToFirstDescendent myPLine
      | getFirstLineSeg eNode1 == getLastLineSeg eNode2 = pathToFirstDescendent eNodeSet iNodeSet myPLine
      | getLastLineSeg eNode1 == getFirstLineSeg eNode2 = flipArc <$> pathToFirstDescendent eNodeSet iNodeSet myPLine
      | otherwise = error "found ENodes that do not follow."
    -- append two slists of arcs, checking and eliminating the case where the first slist ends with an arc that is anti-collinear with the head of the second slist.
    mergeWithoutNonIntersecting :: Slist (ProjectiveLine, PLine2Err) -> Slist (ProjectiveLine, PLine2Err) -> Slist (ProjectiveLine, PLine2Err)
    mergeWithoutNonIntersecting arcs1 arcs2
      | isEmpty arcs1 = arcs2
      | isEmpty arcs2 = arcs1
      | otherwise = if noIntersection (last arcs1) (head arcs2)
                       -- make sure that we remove the arc going against the winding.
                    then if getFirstLineSeg eNode1 == getLastLineSeg eNode2
                         then init arcs1 <> arcs2
                         else arcs1 <> tail arcs2
                    else arcs1 <> arcs2
    -- Generate a face, or dump a useful error.
    faceOrError firstENode firstMidArcs lastMidArcs lastENode
      | isNothing segment = error $ "segments do not line up.\n"
                                 <> dumpEverything
      | otherwise = fromMaybe errMakingFace $ makeFace firstENode midArcs lastENode
      where
        midArcs = mergeWithoutNonIntersecting firstMidArcs lastMidArcs
        segment
          | getFirstLineSeg firstENode == getLastLineSeg lastENode = Just $ getFirstLineSeg firstENode
          | getFirstLineSeg lastENode == getLastLineSeg firstENode = Just $ getFirstLineSeg lastENode
          | otherwise = Nothing
        -- our new error condition
        errMakingFace = error $ "miswound:\n"
                        <> dumpEverything
        dumpEverything =
                        show iNodeSet <> "\n"
                        <> show eNodeSet <> "\n"
                        <> show pLine1 <> "\n"
                        <> show pLine2 <> "\n"
                        <> show segment <> "\n"
                        <> show firstENode <> "\n"
                        <> show firstMidArcs <> "\n"
                        <> show lastMidArcs <> "\n"
                        <> show lastENode <> "\n"
                        <> show (arcsAreLeft $
                                 [eToPL $ fromJust segment | isJust segment] <>
                                 [outAndErrOf firstENode] <>
                                 (\(Slist a _) -> a) (mergeWithoutNonIntersecting firstMidArcs lastMidArcs) <>
                                 [outAndErrOf lastENode]) <> "\n"
                        <> show (arcsAreLeft $
                                 [eToPL $ fromJust segment | isJust segment] <>
                                 [outAndErrOf firstENode] <>
                                 (flipArc <$> (\(Slist a _) -> a) firstMidArcs) <>
                                 (\(Slist a _) -> a) lastMidArcs <>
                                 [flipArc $ outAndErrOf lastENode]) <> "\n"

-- | Create a Face covering the space between two NodeTrees. like areaBetween, but for two separate NodeTrees.
intraNodeFace :: NodeTree -> NodeTree -> Face
intraNodeFace nodeTree1 nodeTree2
  | nodeTree1 `isLeftOf` nodeTree2  = if nodeTree1 `follows` nodeTree2
                                      then fromMaybe errNodesNotNeighbors $
                                             makeFace (firstENodeOf nodeTree1) (init (lastPLinesOf  nodeTree1) <> tail (tail $ SL.reverse $ firstPLinesOf nodeTree2)) (lastENodeOf nodeTree2)
                                      else fromMaybe errNodesNotNeighbors $
                                             makeFace (firstENodeOf nodeTree1) (init (lastPLinesOf  nodeTree1) <>       tail  (SL.reverse $ firstPLinesOf nodeTree2)) (lastENodeOf nodeTree2)
  | nodeTree1 `isRightOf` nodeTree2 = if nodeTree2 `follows` nodeTree1
                                      then fromMaybe errNodesNotNeighbors $
                                             makeFace (lastENodeOf nodeTree2)  (init (firstPLinesOf nodeTree2) <> tail (tail $ SL.reverse $ lastPLinesOf nodeTree1)) (firstENodeOf nodeTree1)
                                      else fromMaybe errNodesNotNeighbors $
                                             makeFace (firstENodeOf nodeTree2) (init (firstPLinesOf nodeTree2) <>       tail  (SL.reverse $ lastPLinesOf nodeTree1)) (lastENodeOf nodeTree1)
  | nodeTree1 == nodeTree2          = error $ "two identical nodes given.\n" <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n"
  | otherwise = error $ "Two NodeTrees given that are not neighbors: " <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n"
  where
    errNodesNotNeighbors = error $ "cannot make a face from nodes that are not neighbors: \n" <> show nodeTree1 <> "\n" <> show nodeTree2 <> "\n"
    follows :: NodeTree -> NodeTree -> Bool
    follows nt1 nt2 = isCollinear (last $ firstPLinesOf nt1) (last $ lastPLinesOf nt2)
    isLeftOf :: NodeTree -> NodeTree -> Bool
    isLeftOf nt1 nt2 = firstSegOf nt1 == lastSegOf nt2
    isRightOf :: NodeTree -> NodeTree -> Bool
    isRightOf nt1 nt2 = lastSegOf nt1 == firstSegOf nt2
    lastPLinesOf :: NodeTree -> Slist (ProjectiveLine, PLine2Err)
    lastPLinesOf nodeTree = slist $ (\(a,_,_) -> a) $ pathLast nodeTree
    firstPLinesOf :: NodeTree -> Slist (ProjectiveLine, PLine2Err)
    firstPLinesOf nodeTree = slist $ (\(a,_,_) -> a) $ pathFirst nodeTree

-- Find the bottom ENode going down the last paths from the given ProjectiveLine.
lastDescendent :: ENodeSet -> Maybe INodeSet -> (ProjectiveLine, PLine2Err) -> ENode
lastDescendent eNodeSet iNodeSet pLine
  -- handle the case where we're asked for an ENode's output.
  | null $ pathToLastDescendent eNodeSet iNodeSet pLine = fromMaybe (error "could not find ENode!") $ findENodeByOutput eNodeSet (fst pLine)
  | isNothing iNodeSet = error "need an INodeSet with contents!"
  | otherwise = fromMaybe (error $ "could not find ENode!\nLooking for: " <> show pLine <> "\nIn InodeSet: " <> show iNodeSet <> "\n") $ findENodeByOutput eNodeSet $ fst $ lastInOf $ lastINodeOfPLine eNodeSet (fromJust iNodeSet) pLine

-- Find the bottom ENode going down the first paths from the given ProjectiveLine.
firstDescendent :: ENodeSet -> Maybe INodeSet -> (ProjectiveLine, PLine2Err) -> ENode
firstDescendent eNodeSet iNodeSet pLine
  -- handle the case where we're asked for an ENode's output.
  | null $ pathToFirstDescendent eNodeSet iNodeSet pLine = fromMaybe (error "could not find ENode!") $ findENodeByOutput eNodeSet (fst pLine)
  | isNothing iNodeSet = error "need an INodeSet with contents!"
  | isJust firstINode = fromMaybe (error "could not find ENode!") $ findENodeByOutput eNodeSet $ fst $ firstInOf (fromJust firstINode)
  | otherwise = error "could not find ENode or INode."
    where
      firstINode = firstINodeOfPLine eNodeSet (fromJust iNodeSet) pLine

-- | find the First INode of a PLine.
firstINodeOfPLine :: ENodeSet -> INodeSet -> (ProjectiveLine, PLine2Err) -> Maybe INode
firstINodeOfPLine eNodeSet iNodeSet pLine
  | isJust res = Just $ snd (fromJust res)
  | otherwise = Nothing
  where
    res = findINodeByOutput iNodeSet (fst $ last $ pathToFirstDescendent eNodeSet (Just iNodeSet) pLine) True

-- | find the last INode of a PLine.
lastINodeOfPLine :: ENodeSet -> INodeSet -> (ProjectiveLine, PLine2Err) -> INode
lastINodeOfPLine eNodeSet iNodeSet pLine = snd $ fromMaybe (error "could not find INode!") $ findINodeByOutput iNodeSet (fst $ last $ pathToLastDescendent eNodeSet (Just iNodeSet) pLine) True

-- | Find the parent inode of a given ProjectiveLine.
parentINodeOfPLine :: INodeSet -> (ProjectiveLine, PLine2Err) -> INode
parentINodeOfPLine iNodeSet line@(pLine, _) = snd $ fromMaybe (error $ "could not find INode!\nLooking for: " <> show line <> "\nIn INodeSet: " <> show iNodeSet <> "\n") $ findINodeByOutput iNodeSet pLine True

-- | Determine if a PLine matches the output of an ENode.
isENode :: ENodeSet -> ProjectiveLine -> Bool
isENode eNodes pLine = isJust $ findENodeByOutput eNodes pLine

-- | Travel from the top, down the INode tree, taking the 'First' path, marking down all of the pLine2s as we go, until we get to the bottom.
pathToFirstDescendent :: ENodeSet -> Maybe INodeSet -> (ProjectiveLine, PLine2Err) -> Slist (ProjectiveLine, PLine2Err)
pathToFirstDescendent eNodeSet iNodeSet line@(pLine, _)
  | isENode eNodeSet pLine = slist []
  | isNothing iNodeSet = error "need an INodeSet with contents!"
  | otherwise = one line <> pathToFirstDescendent eNodeSet iNodeSet (firstInOf $ parentINodeOfPLine (fromJust iNodeSet) line)

-- | Travel down the INode tree, taking the 'Last' path, marking down all of the pLine2s as we go, until we get to the bottom.
pathToLastDescendent :: ENodeSet -> Maybe INodeSet -> (ProjectiveLine, PLine2Err) -> Slist (ProjectiveLine, PLine2Err)
pathToLastDescendent eNodeSet iNodeSet line@(pLine, _)
  | isENode eNodeSet pLine = slist []
  | isNothing iNodeSet = error "need an INodeSet with contents!"
  | otherwise = one line <> pathToLastDescendent eNodeSet iNodeSet (lastInOf $ parentINodeOfPLine (fromJust iNodeSet) line)

-- | Construct a face from two nodes, and a set of arcs. the nodes must follow each other on the contour.
makeFace :: ENode -> Slist (ProjectiveLine, PLine2Err) -> ENode -> Maybe Face
makeFace e1 arcs e2 = makeFace' (getFirstLineSeg e1) (outAndErrOf e1) arcs (flipArc $ outAndErrOf e2)
  where
    makeFace' :: LineSeg -> (ProjectiveLine, PLine2Err) -> Slist (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> Maybe Face
    makeFace' seg firstArc midArcs lastArc
  --  uncomment me when debugging miswound faces that should be convex.
  --  | not $ all (== True) $ catMaybes $ arcsAreLeft $ [eToPL seg] <> [firstArc] <> (\(Slist a _) -> a) midArcs <> [lastArc] = Nothing
  --  | otherwise
      = res
      where
        res = Just $ Face seg firstArc midArcs lastArc

-- | Ensure the list of arcs given all are to the left of each other, such that following them results in a close contour wound to the left.
arcsAreLeft :: [(ProjectiveLine, PLine2Err)] -> [Maybe Bool]
arcsAreLeft = mapWithFollower (\(pl1, _) (pl2, _) -> pl2 `pLineIsLeft` pl1)

-- | Throw an error if one of the faces has two following arcs that do not have an intersection between them.
findDegenerates :: Slist Face -> Slist Face
findDegenerates (Slist inFaces _) = slist $ checkFace <$> inFaces
  where
    checkFace inFace@(Face edge firstArc (Slist midArcs _) lastArc)
      | all (isRight . fromMaybe (error "whoops!")) intersections = inFace
      | otherwise = error $ "yup, busted.\n" <> show inFace <> "\n" <> show intersections <> "\n"
      where
        intersections = mapWithFollower intersectionBetween $ eToPL edge : firstArc : midArcs <> [lastArc]

flipArc :: (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err)
flipArc (arc, arcErr) = (flipL arc, arcErr)

