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

{- Purpose of this file: to hold the logic and routines required for building
   a Straight Skeleton of a contour, with a set of sub-contours cut out of it.
-}

-- inherit instances when deriving.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

-- So we can section tuples
{-# LANGUAGE TupleSections #-}

-- | Common types and functions used in the code responsible for generating straight skeletons.

module Graphics.Slicer.Math.Skeleton.Definitions (RemainingContour(RemainingContour), StraightSkeleton(StraightSkeleton), Spine(Spine), ENode(ENode), INode(INode), ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), ancestorsOf, Motorcycle(Motorcycle), Cell(Cell), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), MotorcycleIntersection(WithENode, WithMotorcycle, WithLineSeg), concavePLines, getFirstLineSeg, getLastLineSeg, hasNoINodes, getPairs, linePairs, finalPLine, finalINodeOf, finalOutOf, makeINode, sortedPLines, sortedPLinesWithErr, indexPLinesTo, insOf, lastINodeOf, firstInOf, isLoop, lastInOf) where

import Prelude (Eq, Show, Bool(True, False), Ordering(LT,GT), otherwise, ($), (<$>), (==), (/=), error, (>), (&&), any, fst, and, (||), (<>), show, (<), (*), mempty, realToFrac)

import Prelude as PL (head, last)

import Data.List (filter, sortBy, nub)

import Data.List.Extra (unsnoc)

import Data.List.NonEmpty (NonEmpty)

import Data.List.Unique (count_)

import Data.Maybe (Maybe(Just,Nothing), isJust, mapMaybe)

import Slist (len, cons, slist, isEmpty, safeLast)

import Slist as SL (last, head, init)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2, mapWithFollower, fudgeFactor, startPoint, distance, endPoint, lineSegsOfContour, makeLineSeg)

import Graphics.Slicer.Math.GeometricAlgebra (UlpSum(UlpSum), addVecPair)

import Graphics.Slicer.Math.PGA (plinesIntersectIn, PIntersection(IntersectsIn), flipL, PLine2(PLine2), PLine2Err, pLineIsLeft, distance2PP, Pointable(canPoint, pPointOf, ePointOf), Arcable(errOfOut, hasArc, outOf), CPPoint2(CPPoint2), PPoint2(PPoint2), eToPL, eToPP, outAndErrOf, pToEP, vecOfL)

-- | A point where two lines segments that are part of a contour intersect, emmiting an arc toward the interior of a contour.
-- FIXME: a source should have a different UlpSum for it's point and it's output.
-- FIXME: provide our own Eq instance, cause floats suck? :)
data ENode = ENode { _inPoints :: !(Point2, Point2, Point2), _arcOut :: !PLine2, _arcOutErr :: !PLine2Err}
  deriving Eq
  deriving stock Show

instance Arcable ENode where
  -- an ENode always has an arc.
  hasArc _ = True
  outOf (ENode _ outArc _) = outArc
  errOfOut (ENode _ _ outErr) = outErr

instance Pointable ENode where
  -- an ENode always contains a point.
  canPoint _ = True
  pPointOf a = (\(CPPoint2 v) -> PPoint2 v) $ eToPP $ ePointOf a
  ePointOf (ENode (_,centerPoint,_) _ _) = centerPoint

-- | A point in our straight skeleton where two arcs intersect, resulting in the creation of another arc.
-- FIXME: a source should have a different UlpSum for it's point and it's output.
data INode = INode { _firstInArc :: !(PLine2, PLine2Err), _secondInArc :: !(PLine2, PLine2Err), _moreInArcs :: !(Slist (PLine2, PLine2Err)), _outArc :: !(Maybe (PLine2, PLine2Err))}
  deriving Eq
  deriving stock Show

instance Arcable INode where
  -- an INode might just end here.
  errOfOut (INode _ _ _ outArc) =  case outArc of
                                 (Just (_,rawOutArcErr)) -> rawOutArcErr
                                 Nothing -> error "tried to get an outArc that has no output arc."
  hasArc (INode _ _ _ outArc) = isJust outArc
  outOf (INode _ _ _ outArc) = case outArc of
                                 (Just (rawOutArc,_)) -> rawOutArc
                                 Nothing -> error "tried to get an outArc that has no output arc."

instance Pointable INode where
  -- an INode does not contain a point, we have to attempt to resolve one instead.
  canPoint iNode@(INode firstPLine secondPLine morePLines _) = len allPLines > 1 && hasIntersectingPairs allPLines
    where
      allPLines = if hasArc iNode
                  then cons (outAndErrOf iNode) $ cons firstPLine $ cons secondPLine morePLines
                  else cons firstPLine $ cons secondPLine morePLines
      hasIntersectingPairs (Slist pLines _) = any (\(pl1, pl2) -> saneIntersect $ plinesIntersectIn pl1 pl2) $ getPairs pLines
        where
          saneIntersect (IntersectsIn _ _) = True
          saneIntersect _                  = False
  -- FIXME: if we have multiple intersecting pairs, is there a preferred pair to use for resolving? angle based, etc?
  pPointOf iNode@(INode firstPLine secondPLine (Slist rawPLines _) _)
    | allPointsSame = case results of
                        [] -> error $ "cannot get a PPoint of this iNode: " <> show iNode <> "/n"
                        [a] -> a
                        (a:_) -> a
    -- Allow the pebbles to vote.
    | otherwise = case safeLast (slist $ count_ results) of
                    Nothing -> error $ "cannot get a PPoint of this iNode: " <> show iNode <> "/n"
                    (Just a) -> fst a
    where
      results = intersectionsOfPairs allPLines
      allPointsSame = case intersectionsOfPairs allPLines of
                        [] -> error $ "no intersection of pairs for " <> show allPLines <> "\nINode: " <> show iNode <> "\n"
                        [_] -> True
                        points -> and $ mapWithFollower distanceWithinErr points
                          where
                            distanceWithinErr a b = res < realToFrac err
                              where
                                (res, (_,_,UlpSum err)) = distance2PP (a,mempty) (b,mempty)
      allPLines = if hasArc iNode
                  then slist $ nub $ (outAndErrOf iNode) : firstPLine : secondPLine : rawPLines
                  else slist $ nub $ firstPLine : secondPLine : rawPLines
      intersectionsOfPairs (Slist pLines _) = mapMaybe (\(pl1, pl2) -> saneIntersect $ plinesIntersectIn pl1 pl2) $ getPairs pLines
        where
          saneIntersect (IntersectsIn a _) = Just $ (\(CPPoint2 v) -> PPoint2 v) a
          saneIntersect _                  = Nothing
  ePointOf a = fst $ pToEP $ pPointOf a

-- Produce a list of the inputs to a given INode.
insOf :: INode -> [(PLine2, PLine2Err)]
insOf (INode firstIn secondIn (Slist moreIns _) _) = firstIn:secondIn:moreIns

lastINodeOf :: INodeSet -> INode
lastINodeOf (INodeSet gens) = case unsnoc (SL.last gens) of
                                Nothing -> error "no first of the last generation?"
                                Just (_,lastItem) -> lastItem

-- | A Motorcycle. a PLine eminating from an intersection between two line segments toward the interior or the exterior of a contour.
--   Motorcycles are emitted from convex (reflex) virtexes of the encircling contour, and concave virtexes of any holes.
--   FIXME: Note that a new motorcycle may be created in the case of degenerate polygons... with it's inSegs being two other motorcycles.
data Motorcycle = Motorcycle { _inCSegs :: !(LineSeg, LineSeg), _outPline :: !PLine2, _outPlineErr :: PLine2Err}
  deriving Eq
  deriving stock Show

instance Arcable Motorcycle where
  -- A Motorcycle always has an arc, which is it's path.
  hasArc _ = True
  outOf (Motorcycle _ outArc _) = outArc
  errOfOut (Motorcycle _ _ outErr) = outErr

instance Pointable Motorcycle where
  -- A motorcycle always contains a point.
  canPoint _ = True
  pPointOf a = (\(CPPoint2 v) -> PPoint2 v) $ eToPP $ ePointOf a
  ePointOf (Motorcycle (_, LineSeg point _) _ _) = point

-- | The motorcycles that are involved in dividing two cells.
data DividingMotorcycles = DividingMotorcycles { firstMotorcycle :: !Motorcycle, moreMotorcycles :: !(Slist Motorcycle) }
  deriving Eq
  deriving stock Show

-- A concave region of a contour.
newtype Cell = Cell { _sides :: Slist (Slist LineSeg, Maybe CellDivide)}
  deriving Eq
  deriving stock Show

-- | the border dividing two cells of a contour.
data CellDivide = CellDivide { _divMotorcycles :: !DividingMotorcycles, _intersects :: !MotorcycleIntersection }
  deriving Eq
  deriving stock Show

-- note that if there is an ENode that is part of the division, it's anticolinear to the last motorcycle in _divMotorcycles.
data MotorcycleIntersection =
    WithLineSeg !LineSeg
  | WithENode !ENode
  | WithMotorcycle !Motorcycle
  deriving Eq
  deriving stock Show

-- The part of a contour that remains once we trim a concave section from it.
newtype RemainingContour = RemainingContour (Slist (Slist LineSeg, [CellDivide]))
  deriving Eq
  deriving stock Show

-- | The exterior nodes of a whole contour or just a cell of a contour.
newtype ENodeSet = ENodeSet { _eNodeSides :: Slist (ENode,Slist ENode) }
  deriving Eq
  deriving stock Show

-- | a set of Interior nodes that are intersections of ENodes or other INodes.
-- nodes are divided into 'generations', where each generation is a set of nodes that (may) result in the next set of nodes. the last generation always contains just one node.
-- Note that not all of the outArcs in a given generation necessarilly are used in the next generation, but they must all be used by following generations in order for a nodetree to be complete.
-- The last generation may not have an outArc in the case of a complete contour.
-- FIXME: move last generation into structure type?
newtype INodeSet = INodeSet (Slist [INode])
  deriving Eq
  deriving stock Show

-- | The complete graph of exterior nodes, and their interior intersection. note this may be for a cell, a contour, or the border between two cells.
data NodeTree = NodeTree { _eNodes :: !ENodeSet, _iNodes :: !INodeSet }
  deriving Eq
  deriving stock Show

-- | A Spine component:
--   Similar to a node, only without the in and out heirarchy. always connects to inArcs from a NodeTree. One per generation. allows us to build loops.
newtype Spine = Spine { _spineArcs :: NonEmpty PLine2 }
  deriving newtype Eq
  deriving stock Show

-- | The straight skeleton of a contour.
data StraightSkeleton = StraightSkeleton { _nodeSets :: !(Slist [NodeTree]), _spineNodes :: !(Slist Spine) }
  deriving Eq
  deriving stock Show

-- | Cut a list into all possible pairs. Used in a few places, but here because the Pointable instance for INode uses it.
getPairs :: [a] -> [(a,a)]
getPairs [] = []
getPairs (x:xs) = ((x,) <$> xs) <> getPairs xs

---------------------------------------
-- Utility functions for our solvers --
---------------------------------------

-- | determine if the given line segment set contains just one loop.
isLoop :: Slist [LineSeg] -> Bool
isLoop inSegSets = endPoint lastSeg == startPoint firstSeg || distance (endPoint lastSeg) (startPoint firstSeg) < (fudgeFactor*15)
  where
    (lastSeg, firstSeg) = case inSegSets of
                            (Slist [] _) -> error "no segments!"
                            oneOrMoreSets@(Slist ((_:_:_):_) _) -> (PL.last $ SL.last oneOrMoreSets, PL.head $ SL.head oneOrMoreSets)
                            oneOrMoreSets@(Slist (_:_:_) _) -> (PL.last $ SL.last oneOrMoreSets, PL.head $ SL.head oneOrMoreSets)
                            (Slist _ _) -> error "just one segment?"

-- | get the first line segment of an ENode.
getFirstLineSeg :: ENode -> LineSeg
getFirstLineSeg (ENode (p1,p2,_) _ _) = makeLineSeg p1 p2

-- | get the second line segment of an ENode.
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
  | hasNoINodes iNodeSet = if len moreENodes == 0
                           then outAndErrOf firstENode
                           else error "cannot have final PLine of NodeTree with more than one ENode, and no generations!\n"
  | hasArc (finalINodeOf iNodeSet) = outAndErrOf $ finalINodeOf iNodeSet
  | otherwise = error "has inodes, has no out, has enodes?"
finalPLine (NodeTree _ iNodeSet)
  | hasNoINodes iNodeSet = error "cannot have final PLine of a NodeTree that is completely empty!"
  | hasArc (finalINodeOf iNodeSet) = outAndErrOf $ finalINodeOf iNodeSet
  | otherwise = error "has inodes, has no out, has no enodes?"

-- | get the last output PLine of a NodeTree, if there is one. otherwise, Nothing.
finalOutOf :: NodeTree -> Maybe PLine2
finalOutOf (NodeTree eNodeSet iNodeSet)
  | hasNoINodes iNodeSet = case eNodeSet of
                             (ENodeSet (Slist [(firstNode,Slist [] _)] _)) -> Just $ outOf firstNode
                             (ENodeSet _) -> Nothing
  | hasArc (finalINodeOf iNodeSet) = Just $ outOf $ finalINodeOf iNodeSet
  | otherwise = Nothing

-- | in a NodeTree, the last generation is always a single item. retrieve this item.
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

-- | strip off the latest generation of the given INodeSet.
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

-- | check if an INodeSet is empty.
hasNoINodes :: INodeSet -> Bool
hasNoINodes iNodeSet = case iNodeSet of
                         (INodeSet (Slist _ 0)) -> True
                         (INodeSet _) -> False

-- | Sort a set of PLines. yes, this is 'backwards', to match the counterclockwise order of contours.
sortedPLines :: [PLine2] -> [PLine2]
sortedPLines = sortBy (\n1 n2 -> if ((n1, mempty) `pLineIsLeft` (n2, mempty)) == Just True then LT else GT)

-- | Sort a set of PLines. yes, this is 'backwards', to match the counterclockwise order of contours.
sortedPLinesWithErr :: [(PLine2, PLine2Err)] -> [(PLine2, PLine2Err)]
sortedPLinesWithErr = sortBy (\n1 n2 -> if (n1 `pLineIsLeft` n2) == Just True then LT else GT)

-- | take a sorted list of PLines, and make sure the list starts with the pline closest to (but not left of) the given PLine.
indexPLinesTo :: PLine2 -> [(PLine2, PLine2Err)] -> [(PLine2, PLine2Err)]
indexPLinesTo firstPLine pLines = pLinesBeforeIndex firstPLine pLines <> pLinesAfterIndex firstPLine pLines
  where
    pLinesBeforeIndex myFirstPLine = filter (\a -> (myFirstPLine, mempty) `pLineIsLeft` a /= Just False)
    pLinesAfterIndex myFirstPLine = filter (\a -> (myFirstPLine, mempty) `pLineIsLeft` a == Just False)

-- | find the last PLine of an INode.
lastInOf :: INode -> PLine2
lastInOf (INode _ secondPLine morePLines _)
  | len morePLines == 0 = fst $ secondPLine
  | otherwise           = fst $ SL.last morePLines

-- | find the first PLine of an INode.
firstInOf :: INode -> PLine2
firstInOf (INode a _ _ _) = fst a

