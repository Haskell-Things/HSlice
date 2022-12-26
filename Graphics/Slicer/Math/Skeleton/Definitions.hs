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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

-- So we can section tuples.
{-# LANGUAGE TupleSections #-}

module Graphics.Slicer.Math.Skeleton.Definitions (RemainingContour(RemainingContour), StraightSkeleton(StraightSkeleton), Spine(Spine), ENode(ENode), INode(INode), ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), ancestorsOf, Motorcycle(Motorcycle), Cell(Cell), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), MotorcycleIntersection(WithENode, WithMotorcycle, WithLineSeg), concavePLines, getFirstLineSeg, getLastLineSeg, hasNoINodes, getPairs, linePairs, finalPLine, finalINodeOf, finalOutOf, makeINode, sortedPLines, sortedPLinesWithErr, indexPLinesTo, insOf, lastINodeOf, firstInOf, isLoop, lastInOf) where

import Prelude (Eq, Show, Bool(True, False), Ordering(LT,GT), ($), (<$>), (==), (/=), (||), (<>), (<), (*), (&&), any, error, fst, mempty, not, otherwise, show)

import Prelude as PL (head, last)

import Data.List (filter, sortBy, nub)

import Data.List.Extra (unsnoc)

import Data.List.NonEmpty (NonEmpty)

import Data.List.Unique (count_)

import Data.Maybe (Maybe(Just,Nothing), isJust, mapMaybe)

import Slist (isEmpty, safeLast, slist)

import Slist as SL (last, head, init)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2, distance, endPoint, fudgeFactor, lineSegsOfContour, makeLineSeg, mapWithFollower, startPoint)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPair)

import Graphics.Slicer.Math.Intersections (intersectionsAtSamePoint, noIntersection)

import Graphics.Slicer.Math.PGA (Arcable(errOfOut, hasArc, outOf), CPPoint2(CPPoint2), PIntersection(IntersectsIn), PLine2(PLine2), PLine2Err, Pointable(canPoint, pPointOf, ePointOf, errOfPPoint), PPoint2(PPoint2), eToPL, eToPP, flipL, outAndErrOf, plinesIntersectIn, pLineIsLeft, pToEP, vecOfL, vecOfP)

-- | A point where two lines segments that are part of a contour intersect, emmiting an arc toward the interior of a contour.
-- FIXME: a source should have a different UlpSum for it's point and it's output.
data ENode = ENode
  -- Input points. three points in order, with the inside of the contour to the left.
  !(Point2, Point2, Point2)
  -- The projective line eminating from the middle point. refered to as an Arc.
  !PLine2
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
  ePointOf (ENode (_,centerPoint,_) _ _) = centerPoint
  errOfPPoint _ = mempty
  -- FIXME: this causes double canonicalization.
  pPointOf a = PPoint2 $ vecOfP $ eToPP $ ePointOf a

-- | A point in our straight skeleton where arcs intersect, resulting in the creation of another arc.
data INode = INode
  -- The first input arc. We break out the first and second input arc to expose that we need at least two arcs to the type system.
  !(PLine2, PLine2Err)
  -- The second input arc.
  !(PLine2, PLine2Err)
  -- More input arcs.
  !(Slist (PLine2, PLine2Err))
  -- An output arc.
  !(Maybe (PLine2, PLine2Err))
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
  -- Since an INode does not contain a point, we have to attempt to resolve one instead.
  canPoint iNode = hasIntersectingPairs (allPLinesOfINode iNode)
    where
      hasIntersectingPairs (Slist pLines _) = any (\(pl1, pl2) -> not $ noIntersection pl1 pl2) $ getPairs pLines
  -- just convert our resolved point.
  ePointOf a = fst $ pToEP $ pPointOf a
  -- FIXME: implement this properly.
  errOfPPoint _ = mempty
  -- FIXME: if we have multiple intersecting pairs, is there a preferred pair to use for resolving? maybe a pair that is at as close as possible to a right angle?
  pPointOf iNode
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
          saneIntersect (IntersectsIn a _) = Just $ (\(CPPoint2 v) -> PPoint2 v) a
          saneIntersect _                  = Nothing

-- | Get all of the PLines that come from, or exit an iNode.
allPLinesOfINode :: INode -> Slist (PLine2, PLine2Err)
allPLinesOfINode iNode@(INode firstPLine secondPLine (Slist morePLines _) _)
  | hasArc iNode = slist $ nub $ (outAndErrOf iNode) : firstPLine : secondPLine : morePLines
  | otherwise    = slist $ nub $ firstPLine : secondPLine : morePLines

-- | Produce a list of the inputs to a given INode.
insOf :: INode -> [(PLine2, PLine2Err)]
insOf (INode firstIn secondIn (Slist moreIns _) _) = firstIn:secondIn:moreIns

lastINodeOf :: INodeSet -> INode
lastINodeOf (INodeSet gens) = case unsnoc (SL.last gens) of
                                Nothing -> error "no first of the last generation?"
                                Just (_,lastItem) -> lastItem

-- | A Motorcycle. a PLine eminating from an intersection between two line segments toward the interior or the exterior of a contour.
--   Motorcycles are emitted from convex (reflex) virtexes of the encircling contour, and concave virtexes of any holes.
--   FIXME: Note that a new motorcycle may be created in the case of degenerate polygons... with it's inSegs being two other motorcycles.
data Motorcycle = Motorcycle
  -- The two line segments from which this motorcycle projects.
  !(LineSeg, LineSeg)
  -- The output arc of this motorcycle. really, the motorcycle.
  !PLine2
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
  ePointOf (Motorcycle (_, LineSeg point _) _ _) = point
  errOfPPoint _ = mempty
  pPointOf a = (\(CPPoint2 v) -> PPoint2 v) $ eToPP $ ePointOf a

-- | The motorcycles that are involved in dividing two cells.
data DividingMotorcycles = DividingMotorcycles { firstMotorcycle :: !Motorcycle, moreMotorcycles :: !(Slist Motorcycle) }
  deriving Eq
  deriving stock Show

-- | A concave region of a contour.
newtype Cell = Cell { _sides :: Slist (Slist LineSeg, Maybe CellDivide)}
  deriving stock Show

-- | The border dividing two cells of a contour.
data CellDivide = CellDivide { _divMotorcycles :: !DividingMotorcycles, _intersects :: !MotorcycleIntersection }
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
newtype RemainingContour = RemainingContour (Slist (Slist LineSeg, [CellDivide]))

-- | The exterior nodes of a whole contour or just a cell of a contour.
newtype ENodeSet = ENodeSet { _eNodeSides :: Slist (ENode,Slist ENode) }
  deriving Eq
  deriving stock Show

-- | A set of Interior nodes that are intersections of ENodes or other INodes.
-- nodes are divided into 'generations', where each generation is a set of nodes that (may) result in the next set of nodes. the last generation always contains just one node.
-- Note that not all of the outArcs in a given generation necessarilly are used in the next generation, but they must all be used by following generations in order for a nodetree to be complete.
-- The last generation may not have an outArc in the case of a complete contour.
-- FIXME: move last generation into structure type?
newtype INodeSet = INodeSet (Slist [INode])
  deriving Eq
  deriving stock Show

-- | The complete graph of exterior nodes, and their interior intersection. note this may be for a cell, a contour, or the border between two cells.
data NodeTree = NodeTree { _eNodes :: !ENodeSet, _iNodes :: !INodeSet }
  deriving stock Show

-- | All nodetrees with identical eNodes have identical iNodes.
instance Eq NodeTree where
  (==) (NodeTree enodeset1 _) (NodeTree enodeset2 _) = enodeset1 == enodeset2
  (/=) a b = not $ a == b

-- | A Spine component:
--   Similar to a node, only without the in and out heirarchy. always connects to inArcs from a NodeTree. One per generation. allows us to build loops.
newtype Spine = Spine { _spineArcs :: NonEmpty PLine2 }
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
isLoop inSegSets = endPoint lastSeg == startPoint firstSeg || distance (endPoint lastSeg) (startPoint firstSeg) < (fudgeFactor*15)
  where
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

-- | Get pairs of lines from the contour, including one pair that is the last line paired with the first.
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
makeINode :: [(PLine2, PLine2Err)] -> Maybe (PLine2, PLine2Err) -> INode
makeINode pLines maybeOut = case pLines of
                              [] -> error "tried to construct a broken INode"
                              [onePLine] -> error $ "tried to construct a broken INode from one PLine2: " <> show onePLine <> "\n"
                              [first,second] -> INode first second (slist []) maybeOut
                              (first:second:more) -> INode first second (slist more) maybeOut

-- | Get the output of the given nodetree. fails if the nodetree has no output.
finalPLine :: NodeTree -> (PLine2, PLine2Err)
finalPLine (NodeTree (ENodeSet (Slist [(firstENode,moreENodes)] _)) iNodeSet)
  | hasNoINodes iNodeSet = if isEmpty moreENodes
                           then outAndErrOf firstENode
                           else error "cannot have final PLine of NodeTree with more than one ENode, and no generations!\n"
  | hasArc (finalINodeOf iNodeSet) = outAndErrOf $ finalINodeOf iNodeSet
  | otherwise = error "has inodes, has no out, has enodes?"
finalPLine (NodeTree _ iNodeSet)
  | hasNoINodes iNodeSet = error "cannot have final PLine of a NodeTree that is completely empty!"
  | hasArc (finalINodeOf iNodeSet) = outAndErrOf $ finalINodeOf iNodeSet
  | otherwise = error "has inodes, has no out, has no enodes?"

-- | Get the last output PLine of a NodeTree, if there is one. otherwise, Nothing.
finalOutOf :: NodeTree -> Maybe PLine2
finalOutOf (NodeTree eNodeSet iNodeSet)
  | hasNoINodes iNodeSet = case eNodeSet of
                             (ENodeSet (Slist [(firstNode,Slist [] _)] _)) -> Just $ outOf firstNode
                             (ENodeSet _) -> Nothing
  | hasArc (finalINodeOf iNodeSet) = Just $ outOf $ finalINodeOf iNodeSet
  | otherwise = Nothing

-- | In a NodeTree, the last generation is always a single item. retrieve this item.
finalINodeOf :: INodeSet -> INode
finalINodeOf (INodeSet generations)
  | isEmpty generations = error "cannot get final INode if there are no INodes."
  | otherwise = case finalGeneration of
                  [] -> error "empty INode list?"
                  [a] -> a
                  (_:_) -> error "final generation has too many members."
  where
    finalGeneration = case safeLast generations of
                        Nothing -> error "either infinite, or empty list"
                        (Just val) -> val

-- | Strip off the latest generation of the given INodeSet.
ancestorsOf :: INodeSet -> INodeSet
ancestorsOf (INodeSet generations)
  | isEmpty generations = error "cannot get all but the last generation of INodes if there are no INodes."
  | otherwise = INodeSet ancestors
  where
    ancestors = SL.init generations

-- | Examine two line segments that are part of a Contour, and determine if they are concave toward the interior of the Contour. if they are, construct a PLine2 bisecting them, pointing toward the interior of the Contour.
concavePLines :: LineSeg -> LineSeg -> Maybe PLine2
concavePLines seg1 seg2
  | Just True == pLineIsLeft (eToPL seg1) (eToPL seg2) = Just $ PLine2 $ addVecPair pv1 pv2
  | otherwise                          = Nothing
  where
    (PLine2 pv1,_) = eToPL seg1
    pv2 = vecOfL $ flipL pl2
    (pl2,_) = eToPL seg2

-- | Check if an INodeSet is empty.
hasNoINodes :: INodeSet -> Bool
hasNoINodes iNodeSet = case iNodeSet of
                         (INodeSet (Slist _ 0)) -> True
                         (INodeSet _) -> False

-- | Sort a set of PLines. yes, this is 'backwards', to match the counterclockwise order of contours.
sortedPLines :: [PLine2] -> [PLine2]
sortedPLines = sortBy (\n1 n2 -> if (n1, mempty) `pLineIsLeft` (n2, mempty) == Just True then LT else GT)

-- | Sort a set of PLines. yes, this is 'backwards', to match the counterclockwise order of contours.
sortedPLinesWithErr :: [(PLine2, PLine2Err)] -> [(PLine2, PLine2Err)]
sortedPLinesWithErr = sortBy (\n1 n2 -> if (n1 `pLineIsLeft` n2) == Just True then LT else GT)

-- | Take a sorted list of PLines, and make sure the list starts with the pline closest to (but not left of) the given PLine.
indexPLinesTo :: PLine2 -> [(PLine2, PLine2Err)] -> [(PLine2, PLine2Err)]
indexPLinesTo firstPLine pLines = pLinesBeforeIndex firstPLine pLines <> pLinesAfterIndex firstPLine pLines
  where
    pLinesBeforeIndex myFirstPLine = filter (\a -> (myFirstPLine, mempty) `pLineIsLeft` a /= Just False)
    pLinesAfterIndex myFirstPLine = filter (\a -> (myFirstPLine, mempty) `pLineIsLeft` a == Just False)

-- | Find the last PLine of an INode.
lastInOf :: INode -> PLine2
lastInOf (INode _ secondPLine morePLines _)
  | isEmpty morePLines = fst $ secondPLine
  | otherwise          = fst $ SL.last morePLines

-- | Find the first PLine of an INode.
firstInOf :: INode -> PLine2
firstInOf (INode a _ _ _) = fst a

