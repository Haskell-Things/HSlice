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

{- Purpose of this file:
   To hold common types and functions used in the code responsible for generating straight skeletons of contours.
-}

-- Inherit instances when deriving.
{-# LANGUAGE DerivingStrategies #-}

-- So we can section tuples.
{-# LANGUAGE TupleSections #-}

module Graphics.Slicer.Math.Skeleton.Definitions (
  Cell(Cell),
  CellDivide(CellDivide),
  DividingMotorcycles(DividingMotorcycles),
  ENode(ENode),
  ENodeSet(ENodeSet),
  INode(INode),
  INodeSet(INodeSet),
  Motorcycle(Motorcycle),
  MotorcycleCell(MotorcycleCell),
  MotorcycleCluster(MotorcycleCluster),
  MotorcycleIntersection(WithENode, WithMotorcycle, WithLineSeg),
  NodeTree(NodeTree),
  RemainingContour(RemainingContour),
  StraightSkeleton(StraightSkeleton),
  Side(Side),
  Spine(Spine),
  ancestorsOf,
  allINodesOf,
  allPLinesOfINode,
  concaveLines,
  eNodesOfSide,
  firstInOf,
  finalINodeOf,
  finalOutOf,
  finalOutAndErrOf,
  finalPLine,
  getFirstLineSeg,
  getLastLineSeg,
  getPairs,
  indexPLinesTo,
  iNodeHasIn,
  insOf,
  isLoop,
  isOneSide,
  lastInOf,
  linePairs,
  loopOfSegSets,
  makeENode,
  makeENodes,
  makeInitialGeneration,
  makeINode,
  makeSide,
  oneSideOf,
  sortedPLines,
  sortPLinePair,
  sortPLinesByReference
  ) where

import Prelude (Eq, Show, Bool(True, False), Ordering(EQ, LT,GT), any, concatMap, elem, not, otherwise, (.), ($), (<), (<$>), (==), (/=), (<=), error, (&&), fst, (<>), show, snd, mempty)

import qualified Prelude as PL (head, last)

import Data.List (filter, length, sortBy)

import Data.List.NonEmpty (NonEmpty)

import Data.List.Unique (count_)

import Data.Maybe (Maybe(Just,Nothing), fromJust, isJust, mapMaybe)

import Slist (isEmpty, len, safeLast, slist)

import qualified Slist as SL (last, head, init)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Arcs (getFirstArc)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2, endPoint, lineSegsOfContour, makeLineSeg, mapWithFollower, startPoint)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPair, ulpVal)

import Graphics.Slicer.Math.Intersections (intersectionsAtSamePoint, noIntersection, isAntiCollinear)

import Graphics.Slicer.Math.Lossy (eToPLine2)

import Graphics.Slicer.Math.PGA (Arcable(errOfOut, hasArc, outOf), PIntersection(IntersectsIn), PLine2Err, Pointable(canPoint, cPPointOf, errOfCPPoint, ePointOf), PPoint2Err, ProjectiveLine(PLine2), ProjectiveLine2, ProjectivePoint, distance2PP, eToPP, flipL, normalizeL, outAndErrOf, pToEP, plinesIntersectIn, pLineIsLeft, vecOfL)

-- | A point where two lines segments that are part of a contour intersect, emmiting an arc toward the interior of a contour.
-- FIXME: a source should have a different UlpSum for it's point and it's output.
data ENode = ENode
  -- Input points. three points in order, with the inside of the contour to the left.
  !(Point2, Point2, Point2)
  -- The projective line eminating from the middle point, bisecting the two lines created from the input point (first-middle, last-middle). refered to as an Arc.
  !ProjectiveLine
  -- The imprecision of the Arc.
  !PLine2Err
  deriving stock Show

-- | Since the PLine2 and PLine2Err of an ENode are derived from the input points, only check the points for Eq.
instance Eq ENode where
  (==) (ENode points _ _) (ENode morePoints _ _) = points == morePoints
  (/=) a b = not $ a == b

-- | An ENode always has an arc.
instance Arcable ENode where
  errOfOut (ENode _ _ outErr) = outErr
  hasArc _ = True
  outOf (ENode _ outArc _) = outArc

-- | An ENode is always resolvable to a point.
instance Pointable ENode where
  canPoint _ = True
  cPPointOf a = eToPP $ ePointOf a
  ePointOf (ENode (_,centerPoint,_) _ _) = centerPoint
  errOfCPPoint _ = mempty

-- | A point in our straight skeleton where arcs intersect, resulting in the creation of another arc.
-- FIXME: input arcs should have error quotents.
data INode = INode
  -- The first input arc. We break out the first and second input arc to expose that we need at least two arcs to the type system.
  !(ProjectiveLine, PLine2Err)
  -- The second input arc.
  !(ProjectiveLine, PLine2Err)
  -- More input arcs.
  !(Slist (ProjectiveLine, PLine2Err))
  -- An output arc.
  !(Maybe (ProjectiveLine, PLine2Err))
  deriving stock Show

-- | Since the outgoing PLine2 and PLine2Err are derived from the input arcs, only check the input arcs for Eq.
instance Eq INode where
  (==) (INode arcA1 arcA2 moreA _) (INode arcB1 arcB2 moreB _) = arcA1 == arcB1 && arcA2 == arcB2 && moreA == moreB
  (/=) a b = not $ a == b

-- | Not all INodes have an output Arc.
instance Arcable INode where
  errOfOut (INode _ _ _ outArc) = case outArc of
                                    (Just (_,rawOutErr)) -> rawOutErr
                                    Nothing -> error "tried to get an outArc that has no output arc."
  hasArc (INode _ _ _ outArc) = isJust outArc
  outOf (INode _ _ _ outArc) = case outArc of
                                 (Just (rawOutArc,_)) -> rawOutArc
                                 Nothing -> error "tried to get an outArc that has no output arc."

-- | INodes are only resolvable to a point sometimes.
instance Pointable INode where
  canPoint iNode = hasIntersectingPairs (allPLinesOfINode iNode)
    where
      hasIntersectingPairs (Slist pLines _) = any (\(pl1, pl2) -> not $ noIntersection pl1 pl2) $ getPairs pLines
  cPPointOf iNode = fst $ cPPointAndErrOfINode iNode
  -- Just convert our resolved point.
  ePointOf a = fst $ pToEP $ cPPointOf a
  errOfCPPoint iNode = snd $ cPPointAndErrOfINode iNode

-- Since an INode does not contain a point, we have to attempt to resolve one instead.
-- FIXME: if we have multiple intersecting pairs, is there a preferred pair to use for resolving? maybe a pair that is at as close as possible to a right angle?
cPPointAndErrOfINode :: INode -> (ProjectivePoint, PPoint2Err)
cPPointAndErrOfINode iNode
  | allPointsSame = case results of
                      [] -> error $ "cannot get a PPoint of this iNode: " <> show iNode <> "/n"
                      l -> PL.head l
  -- Allow the pebbles to vote.
  | otherwise = case safeLast (slist $ count_ results) of
                  Nothing -> error $ "cannot get a PPoint of this iNode: " <> show iNode <> "/n"
                  (Just a) -> fst a
  where
    results = intersectionsOfPairs $ allPLinesOfINode iNode
    allPointsSame = intersectionsAtSamePoint ((\(Slist l _) -> l) $ allPLinesOfINode iNode)
    intersectionsOfPairs (Slist pLines _) = mapMaybe (\(pl1, pl2) -> saneIntersect $ plinesIntersectIn pl1 pl2) $ getPairs pLines
      where
        saneIntersect (IntersectsIn p (_,_, pErr)) = Just (p, pErr)
        saneIntersect _                  = Nothing

-- | Get all of the PLines that come from, or exit an iNode.
allPLinesOfINode :: INode -> Slist (ProjectiveLine, PLine2Err)
allPLinesOfINode iNode@(INode firstPLine secondPLine (Slist morePLines _) _)
  | hasArc iNode = slist $ outAndErrOf iNode : firstPLine : secondPLine : morePLines
  | otherwise    = slist $ firstPLine : secondPLine : morePLines

-- | Produce a list of the inputs to a given INode.
insOf :: INode -> [(ProjectiveLine, PLine2Err)]
insOf (INode firstIn secondIn (Slist moreIns _) _) = firstIn:secondIn:moreIns

-- | A Motorcycle. a PLine eminating from an intersection between two line segments toward the interior or the exterior of a contour.
--   Motorcycles are emitted from convex (reflex) virtexes of the encircling contour, and concave virtexes of any holes.
--   FIXME: Note that a new motorcycle may be created in the case of degenerate polygons... with it's inSegs being two other motorcycles.
data Motorcycle = Motorcycle
  -- The two line segments from which this motorcycle projects.
  !(LineSeg, LineSeg)
  -- The output arc of this motorcycle. really, the motorcycle.
  !ProjectiveLine
  -- The error quotent of the output arc.
  !PLine2Err
  deriving stock Show

-- | Since the PLine2 and PLine2Err are derived from the line segments, only check them for Eq.
instance Eq Motorcycle where
  (==) (Motorcycle segsA _ _) (Motorcycle segsB _ _) = segsA == segsB
  (/=) a b = not $ a == b

-- | A Motorcycle always has an arc, which is it's path.
instance Arcable Motorcycle where
  errOfOut (Motorcycle _ _ outErr) = outErr
  hasArc _ = True
  outOf (Motorcycle _ outArc _) = outArc

-- | A motorcycle always contains a point.
instance Pointable Motorcycle where
  canPoint _ = True
  cPPointOf a = eToPP $ ePointOf a
  ePointOf (Motorcycle (_, LineSeg point _) _ _) = point
  errOfCPPoint _ = mempty

-- | The motorcycles that are involved in dividing two cells.
data DividingMotorcycles = DividingMotorcycles { firstMotorcycle :: !Motorcycle, moreMotorcycles :: !(Slist Motorcycle) }
  deriving Eq
  deriving stock Show

-- | A concave region of a contour.
newtype Cell = Cell { _walls :: Slist (Slist LineSeg, Maybe CellDivide)}
  deriving stock Show

-- | The border dividing two cells of a contour.
data CellDivide = CellDivide { _divMotorcycles :: !DividingMotorcycles, _intersects :: !MotorcycleIntersection }
  deriving Eq
  deriving stock Show

-- | A region of a contour. starts out concave.
-- Note: it is possible for a MotorcycleCell to belong to multiple motorcycle clusters.
-- FIXME: move floating segments into cellSides.
data MotorcycleCell = MotorcycleCell { _cellSides :: Slist Side, _floatingSegments :: Slist LineSeg, _cellMotorcycles :: Slist Motorcycle, cellNodeTree :: NodeTree }
  deriving Eq
  deriving stock Show

-- | A portion of a contour, containing a set of motorcycles that collide with each other, and every motorcycleCell that touches one of those motorcycles.
-- Note: it is possible for a MotorcycleCell to belong to multiple motorcycle clusters.
data MotorcycleCluster = MotorcycleCluster { _cells :: Slist MotorcycleCell, _clusterDivide :: CellDivide }
  deriving Eq
  deriving stock Show

-- | What the last dividing motorcycle in a cell divide intersects with.
--   Note that if the divide ends by coliding with an ENode, it's anticolinear to the last motorcycle in _divMotorcycles.
data MotorcycleIntersection =
    WithLineSeg !LineSeg
  | WithENode !ENode
  | WithMotorcycle !Motorcycle
  deriving Eq
  deriving stock Show

-- | The part of a contour that remains once we trim a concave section from it.
data RemainingContour = RemainingContour { _remainingSegments :: Slist LineSeg, _entryDivide :: Maybe CellDivide, _remainingDivides :: [CellDivide]}
  deriving Eq
  deriving stock Show

-- | A side, or a sequence of ENodes, in order.
-- FIXME: the first item in this tuple should be an Either LineSeg ENode, so we can represent a side with no breaks in it.
newtype Side = Side (ENode, Slist ENode)
  deriving Eq
  deriving stock Show

-- | The exterior nodes of a region of a contour.
newtype ENodeSet = ENodeSet { _eNodeSides :: Slist Side }
  deriving Eq
  deriving stock Show

isOneSide :: ENodeSet -> Bool
isOneSide (ENodeSet sides) = len sides == 1

makeSide :: [ENode] -> Side
makeSide [] = error "cannot make an empty side."
makeSide (a:bs) = Side (a, slist bs)

oneSideOf :: ENodeSet -> Side
oneSideOf (ENodeSet sides) = SL.head sides

-- | get the ENodes that a side is composed of.
eNodesOfSide :: Side -> [ENode]
eNodesOfSide (Side (first,Slist more _)) = first : more

-- | A set of Interior nodes that are intersections of ENodes or other INodes.
-- nodes are divided into 'generations', where each generation is a set of nodes that (may) result in the next set of nodes. the last generation always contains just one node.
-- Note that not all of the outArcs in a given generation necessarilly are used in the next generation, but they must all be used by following generations in order for a nodetree to be complete.
-- The last generation may not have an outArc in the case of a complete contour.
data INodeSet = INodeSet { _children :: Slist [INode], finalINodeOf :: INode}
  deriving Eq
  deriving stock Show

-- | The complete graph of exterior nodes, and their interior intersection. note this may be for a cell, a contour, or the border between two cells.
data NodeTree = NodeTree { _eNodes :: !ENodeSet, _iNodes :: !(Maybe INodeSet)}
  deriving stock Show

-- | All nodetrees with identical eNodes have identical iNodes.
instance Eq NodeTree where
  (==) (NodeTree enodeset1 _) (NodeTree enodeset2 _) = enodeset1 == enodeset2
  (/=) a b = not $ a == b

-- | A Spine component:
--   Similar to a node, only without the in and out heirarchy. always connects to inArcs from a NodeTree. One per generation. allows us to build loops.
newtype Spine = Spine { _spineArcs :: NonEmpty ProjectiveLine }
  deriving stock Show

-- | The straight skeleton of a contour.
data StraightSkeleton = StraightSkeleton { _nodeSets :: !(Slist [NodeTree]), _spineNodes :: !(Slist Spine) }
  deriving stock Show

-- | Cut a list into all possible pairs. Used in a few places, but here because the Pointable instance for INode uses it.
getPairs :: [a] -> [(a,a)]
getPairs [] = []
getPairs (x:xs) = ((x,) <$> xs) <> getPairs xs

---------------------------------------
-- Utility functions for our solvers --
---------------------------------------

-- | Determine if the given line segment set contains just one loop.
isLoop :: Slist [LineSeg] -> Bool
isLoop inSegSets@(Slist rawSegSets _)
  | len inSegSets == 1 && length (PL.head rawSegSets) == 1 = False
  | startPoint firstSeg == endPoint lastSeg = True
  | otherwise = gapDistance <= ulpVal gapDistanceErr
  where
    (gapDistance, (_,_, gapDistanceErr)) = distance2PP (eToPP $ endPoint lastSeg, mempty) (eToPP $ startPoint firstSeg, mempty)
    (lastSeg, firstSeg) = case inSegSets of
                            (Slist [] _) -> error "no segments!"
                            oneOrMoreSets@(Slist ((_:_:_):_) _) -> (PL.last $ SL.last oneOrMoreSets, PL.head $ SL.head oneOrMoreSets)
                            oneOrMoreSets@(Slist (_:_:_) _) -> (PL.last $ SL.last oneOrMoreSets, PL.head $ SL.head oneOrMoreSets)
                            (Slist _ _) -> error "just one segment?"

-- | Get the first line segment of an ENode.
getFirstLineSeg :: ENode -> LineSeg
getFirstLineSeg (ENode (p1,p2,_) _ _) = makeLineSeg p1 p2

-- | Get the second line segment of an ENode.
getLastLineSeg :: ENode -> LineSeg
getLastLineSeg (ENode (_,p2,p3) _ _) = makeLineSeg p2 p3

-- | Get pairs of lines from a contour, including the final pair that is the last line paired with the first.
linePairs :: Contour -> [(LineSeg, LineSeg)]
linePairs c = mapWithFollower (,) $ lineSegsOfContour c
-- FIXME: this implementation looks better, but causes code to break because of numeric instability?
{-
linePairs contour = rotateRight $ mapWithNeighbors (\a b c -> (handleLineSegError $ lineSegFromEndpoints a b,
                                                               handleLineSegError $ lineSegFromEndpoints b c)) $ pointsOfContour contour
  where
    rotateLeft a = PL.last a : PL.init a
    rotateRight a = PL.init a <> [PL.last a]
-}

-- | A smart constructor for INodes.
makeINode :: [(ProjectiveLine, PLine2Err)] -> Maybe (ProjectiveLine,PLine2Err) -> INode
makeINode pLines maybeOut = case pLines of
                              [] -> error "tried to construct an INode with no inputs"
                              [onePLine] -> error $ "tried to construct an INode from one input: " <> show onePLine <> "\n"
                              [first,second] -> INode first second (slist []) maybeOut
                              (first:second:more) -> INode first second (slist more) maybeOut

-- | Get the output of the given nodetree. fails if the nodetree has no output.
finalPLine :: NodeTree -> (ProjectiveLine, PLine2Err)
finalPLine (NodeTree eNodeSet iNodeSet)
  | isJust iNodeSet = outAndErrOf $ finalINodeOf $ fromJust iNodeSet
  | isOneSide eNodeSet = case eNodesOfSide $ oneSideOf eNodeSet of
                           [] -> error "impossible"
                           [a] -> outAndErrOf a
                           _ -> ourError
  | otherwise = ourError
    where
      ourError = error "tried to get finalPLine from a nodetree with no INode, and too many ENodes"

-- | Get the last output PLine of a NodeTree, if there is one. otherwise, Nothing.
finalOutOf :: NodeTree -> Maybe ProjectiveLine
finalOutOf (NodeTree eNodeSet iNodeSet)
  | isJust iNodeSet && hasArc (finalINodeOf $ fromJust iNodeSet) = Just $ outOf $ finalINodeOf $ fromJust iNodeSet
  | isOneSide eNodeSet = case eNodesOfSide $ oneSideOf eNodeSet of
                           [] -> error "impossible"
                           [a] -> Just $ outOf a
                           _ -> Nothing
  | otherwise = Nothing

-- | Get the last output PLine of a NodeTree, if there is one. otherwise, Nothing.
finalOutAndErrOf :: NodeTree -> Maybe (ProjectiveLine, PLine2Err)
finalOutAndErrOf (NodeTree eNodeSet iNodeSet)
  | isJust iNodeSet && hasArc (finalINodeOf $ fromJust iNodeSet) = Just $ outAndErrOf $ finalINodeOf $ fromJust iNodeSet
  | isOneSide eNodeSet = case eNodesOfSide $ oneSideOf eNodeSet of
                           [] -> error "impossible"
                           [a] -> Just $ outAndErrOf a
                           _ -> Nothing
  | otherwise = Nothing

-- | Strip off the latest generation of the given INodeSet.
-- FIXME: this may require completely disentangleing two trees.
ancestorsOf :: INodeSet -> [INodeSet]
ancestorsOf (INodeSet children _)
  | isEmpty children = []
  | otherwise = case SL.last children of
                  [] -> error "encountered an empty generation."
                  [a] -> [INodeSet (SL.init children) a]
                  newParents -> case SL.init children of
                                  (Slist [] 0) -> INodeSet mempty <$> newParents
                                  (Slist [a] 1) -> maybeWithChildren a <$> newParents
                                  _ -> error "this is still complicated"
  where
    maybeWithChildren :: [INode] -> INode -> INodeSet
    maybeWithChildren myChildren parent = INodeSet childrenOfParent parent
      where
        childrenOfParent = slist [filter (\a -> parent `iNodeHasIn` outAndErrOf a) myChildren]

allINodesOf :: INodeSet -> Slist [INode]
allINodesOf (INodeSet (Slist children _) parent) = slist $ children <> [[parent]]

-- | Check if an INode has a particular input.
iNodeHasIn :: INode -> (ProjectiveLine, PLine2Err) -> Bool
iNodeHasIn iNode outAndErr = elem outAndErr $ insOf iNode

-- | Examine two line segments that are part of a Contour, and determine if they are concave toward the interior of the Contour. if they are, construct a ProjectiveLine bisecting them, pointing toward the interior of the Contour.
concaveLines :: LineSeg -> LineSeg -> Maybe ProjectiveLine
concaveLines seg1 seg2
  = case eToPLine2 seg2 `pLineIsLeft` eToPLine2 seg1 of
      Just True -> Just $ PLine2 $ addVecPair pv1 pv2
      Just False -> Nothing
      Nothing -> error $ "asked whether two (anti)colinear lines are concave:\n" <> show seg1 <> "\n" <> show seg2 <> "\n"
  where
    pv1 = vecOfL $ eToPLine2 seg1
    pv2 = vecOfL $ flipL $ eToPLine2 seg2

-- | Sort a set of PLines in counterclockwise order, to match the counterclockwise order of contours.
-- NOTE: when given the same PLines in a different list, may chose a different head / tail.
{-# INLINABLE sortedPLines #-}
sortedPLines :: (ProjectiveLine2 a) => [(a, PLine2Err)] -> [(a, PLine2Err)]
sortedPLines pLines
  -- we cannot sort two or less PLines.
  | length pLines < 3 = pLines
  | otherwise = sortBy sortFun pLines
    where
      sortFun (pLine1,_) (pLine2,_) = case pLine2 `pLineIsLeft` pLine1 of
                                        Just True -> LT
                                        _ -> GT

-- | Sort a set of PLines in counterclockwise order, starting with the PLine clesest to the reference PLine.
-- Assumes all PLines meet in a point?
{-# INLINABLE sortPLinesByReference #-}
sortPLinesByReference :: (ProjectiveLine2 a) => (a, PLine2Err) -> [(a, PLine2Err)] -> [(a, PLine2Err)]
sortPLinesByReference refPLine@(rawRefPLine, _) pLines
  -- we cannot sort less than two plines
  | length pLines < 2 = pLines
  | otherwise = sortBy sortFun pLines
    where
      sortFun pLine1@(rawPLine1, _) pLine2@(rawPLine2, _) =
        case pLineOrderCCW pLine1 pLine2 refPLine of
          Nothing -> error $ "two or more (anti)colinear lines?\n"
                                    <> "PLine1: " <> show (fst $ normalizeL rawPLine1) <> "\n"
                                    <> "pLine2: " <> show (fst $ normalizeL rawPLine2) <> "\n"
                                    <> "Reference: " <> show (fst $ normalizeL rawRefPLine) <> "\n"
                                    <> "PLine1 `pLineIsLeft` Reference: " <> show (rawPLine1 `pLineIsLeft` rawRefPLine) <> "\n"
                                    <> "PLine2 `pLineIsLeft` Reference: " <> show (rawPLine2 `pLineIsLeft` rawRefPLine) <> "\n"
                                    <> "PLine1 `pLineIsLeft` PLine2: " <> show (rawPLine1 `pLineIsLeft` rawPLine2) <> "\n"
                                    <> "pLines: " <> show (fst . normalizeL . fst <$> pLines) <> "\n"
          (Just a) -> a

-- | sort two PLines against the reference PLine, flipped.
-- Returns the two PLines in a counterclockwise order, from the perspective of our reference PLine after flipping.
sortPLinePair :: (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> [(ProjectiveLine, PLine2Err)]
{-# INLINABLE sortPLinePair #-}
sortPLinePair pLine1@(rawPLine1,_) pLine2@(rawPLine2,_) (rawRefPLine, rawRefPLineErr)
  | refPLineFlipped == rawPLine2 = error "here."
  | otherwise = case pLineOrderCCW pLine1 pLine2 refPLineWithErr of
                  Just LT -> [pLine1, pLine2]
                  Just GT -> [pLine2, pLine1]
                  _ -> error $ "two or more (anti)colinear lines?\n"
                             <> "PLine1: " <> show rawPLine1 <> "\n"
                             <> "pLine2: " <> show rawPLine2 <> "\n"
                             <> "Reference: " <> show rawRefPLine <> "\n"
  where
    refPLineWithErr = (refPLineFlipped, rawRefPLineErr)
    -- We flip this, because for INodes, the outgoing PLines point away from a node, while the two PLines we're working with point toward.
    refPLineFlipped = flipL rawRefPLine

-- | When scanning where three lines meet, starting at the reference PLine, and going counterclockwise, the first PLine you run into is lesser..
-- Note: Nothing as a result is an error condition.
pLineOrderCCW :: (ProjectiveLine2 a) => (a, PLine2Err) -> (a, PLine2Err) -> (a, PLine2Err) -> Maybe Ordering
{-# INLINABLE pLineOrderCCW #-}
pLineOrderCCW pLine1@(rawPLine1,_) pLine2@(rawPLine2,_) refPLine@(rawRefPLine, _)
  | pLine1 == pLine2 = Just EQ
  | otherwise =
    case (rawPLine1 `pLineIsLeft` rawRefPLine, rawPLine2 `pLineIsLeft` rawRefPLine) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just True) -> compareWithAntiColinear pLine1 GT
        (Nothing, Just False) -> compareWithAntiColinear pLine1 LT
        (Just True, Nothing) -> compareWithAntiColinear pLine2 LT
        (Just False, Nothing) -> compareWithAntiColinear pLine2 GT
        (Just True, Just False) -> Just LT
        (Just False, Just True) -> Just GT
        _ -> -- dir1 and dir2 must be equal at this point.
          case (rawPLine1 `pLineIsLeft` rawPLine2) of
            Just True -> Just GT
            Just False -> Just LT
            Nothing -> Just EQ
  where
    compareWithAntiColinear colinearPLine ordering
      | colinearPLine `isAntiCollinear` refPLine = Just ordering
      | otherwise = case ordering of
                      GT -> Just LT
                      LT -> Just GT
                      _ -> error "wat"

-- | Take a sorted list of PLines, and make sure the list starts with the pline closest to (but not left of) the given PLine.
-- Does not require the input PLine to be in the set.
{-# INLINABLE indexPLinesTo #-}
indexPLinesTo :: (ProjectiveLine2 a) => (a, PLine2Err) -> [(a, PLine2Err)] -> [(a,PLine2Err)]
indexPLinesTo firstPLine pLines = pLinesBeforeIndex firstPLine pLines <> pLinesAfterIndex firstPLine pLines
  where
    pLinesBeforeIndex myFirstPLine = filter (\a -> fst a `pLineIsLeft` fst myFirstPLine /= Just False)
    pLinesAfterIndex myFirstPLine = filter (\a -> fst a `pLineIsLeft` fst myFirstPLine == Just False)

-- | Find the last PLine of an INode.
lastInOf :: INode -> (ProjectiveLine, PLine2Err)
lastInOf (INode _ secondPLine morePLines _)
  | isEmpty morePLines = secondPLine
  | otherwise          = SL.last morePLines

-- | Find the first PLine of an INode.
firstInOf :: INode -> (ProjectiveLine, PLine2Err)
firstInOf (INode a _ _ _) = a

-- | Create the set of ENodes for a set of segments
makeInitialGeneration :: Bool -> Slist [LineSeg] -> [ENode]
makeInitialGeneration gensAreLoop inSegSets = concatMap firstENodes inSegSets <> maybeLoop
  where
    -- Generate the first generation of nodes, from the passed in line segments.
    -- If the line segments are a loop, use the appropriate function to create the initial Nodes.
    firstENodes :: [LineSeg] -> [ENode]
    firstENodes firstSegs = case firstSegs of
                              [] -> []
                              [LineSeg {}] -> []
                              (_:_) -> makeENodes firstSegs
    -- Add a closing ENode if this is a closed loop.
    maybeLoop = [loopOfSegSets inSegSets | gensAreLoop]

-- | Make a first generation node.
makeENode :: Point2 -> Point2 -> Point2 -> ENode
makeENode p1 p2 p3 = ENode (p1,p2,p3) arc arcErr
  where
    (arc, arcErr) = getFirstArc p1 p2 p3

-- | Make a first generation set of nodes, AKA, a set of arcs that come from the points where line segments meet, toward the inside of the contour.
makeENodes :: [LineSeg] -> [ENode]
makeENodes segs = case segs of
                         [] -> error "got empty list.\n"
                         [a] -> error $ "not enough line segments: " <> show a <> "\n"
                         [a,b] -> [makeENode (startPoint a) (startPoint b) (endPoint b)]
                         (a:b:xs) -> [makeENode (startPoint a) (startPoint b) (endPoint b)] <> makeENodes (b:xs)

loopOfSegSets :: Slist [LineSeg] -> ENode
loopOfSegSets inSegSets = case inSegSets of
                            (Slist [] _) -> error "no"
                            oneOrMoreSets@(Slist ((_:_:_):_) _) -> makeENode (startPoint $ PL.last $ SL.last oneOrMoreSets) (startPoint $ PL.head $ SL.head oneOrMoreSets) (endPoint $ PL.head $ SL.head oneOrMoreSets)
                            oneOrMoreSets@(Slist (_:_:_) _) -> makeENode (startPoint $ PL.last $ SL.last oneOrMoreSets) (startPoint $ PL.head $ SL.head oneOrMoreSets) (endPoint $ PL.head $ SL.head oneOrMoreSets)
                            (Slist _ _) -> error "yes"

