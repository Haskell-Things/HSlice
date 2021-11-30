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

module Graphics.Slicer.Math.Skeleton.Definitions (RemainingContour(RemainingContour), StraightSkeleton(StraightSkeleton), Spine(Spine), ENode(ENode), INode(INode), ENodeSet(ENodeSet), INodeSet(INodeSet), NodeTree(NodeTree), Arcable(hasArc, outOf), Pointable(canPoint, ePointOf, pPointOf), ancestorsOf, eNodeToINode, Motorcycle(Motorcycle), Cell(Cell), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), concavePLines, noIntersection, isCollinear, isAntiCollinear, isParallel, intersectionOf, hasNoINodes, getPairs, linePairs, finalPLine, finalINodeOf, finalOutOf, makeINode, sortedPLines, indexPLinesTo) where

import Prelude (Eq, Show, Bool(True, False), Ordering(LT,GT), otherwise, ($), (<$>), (==), (/=), (++), error, (>), (&&), any, fst, and, (||), (<>), show, (<))

import Data.List (filter, sortBy)

import Data.List.NonEmpty (NonEmpty)

import Data.List.Unique (count_)

import Data.Maybe (Maybe(Just,Nothing), catMaybes, isJust)

import Slist (len, cons, slist, isEmpty, safeLast, init)

import Slist.Type (Slist(Slist))

import Graphics.Slicer.Math.Contour (lineSegsOfContour)

import Graphics.Slicer.Math.PGA (pToEPoint2, PPoint2, plinesIntersectIn, PIntersection(PCollinear,PAntiCollinear, IntersectsIn,PParallel,PAntiParallel), eToPPoint2, flipPLine2, lineIsLeft, PLine2(PLine2), eToPLine2, pLineIsLeft, distanceBetweenPPoints)

import Graphics.Slicer.Math.Definitions (Contour, LineSeg(LineSeg), Point2, mapWithFollower, fudgeFactor)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPair)

-- | Can this node be resolved into a point in 2d space?
class Pointable a where
  canPoint :: a -> Bool
  pPointOf :: a -> PPoint2
  ePointOf :: a -> Point2

-- | does this node have an output (resulting) pLine?
class Arcable a where
  hasArc :: a -> Bool
  outOf :: a -> PLine2

-- | A point where two lines segments that are part of a contour intersect, emmiting an arc toward the interior of a contour.
data ENode = ENode { _inSegs :: !(LineSeg, LineSeg), _arcOut :: !PLine2 }
  deriving Eq
  deriving stock Show

instance Arcable ENode where
  -- an ENode always has an arc.
  hasArc _ = True
  outOf (ENode _ outArc) = outArc

instance Pointable ENode where
  -- an ENode always contains a point.
  canPoint _ = True
  pPointOf a = eToPPoint2 $ ePointOf a
  ePointOf (ENode (_, LineSeg point _) _) = point

-- | A point in our straight skeleton where two arcs intersect, resulting in the creation of another arc.
data INode = INode { _firstInArc :: !PLine2, _secondInArc :: !PLine2, _moreInArcs :: !(Slist PLine2), _outArc :: !(Maybe PLine2) }
  deriving Eq
  deriving stock Show

instance Arcable INode where
  -- an INode might just end here.
  hasArc (INode _ _ _ outArc) = isJust outArc
  outOf (INode _ _ _ outArc) = case outArc of
                                 (Just rawOutArc) -> rawOutArc
                                 Nothing -> error "tried to get an outArc that has no output arc."

instance Pointable INode where
  -- an INode does not contain a point, we have to attempt to resolve one instead.
  canPoint iNode@(INode firstPLine secondPLine morePLines _) = len allPLines > 1 && hasIntersectingPairs allPLines
    where
      allPLines = if hasArc iNode
                  then cons (outOf iNode) $ cons firstPLine $ cons secondPLine morePLines
                  else cons firstPLine $ cons secondPLine morePLines
      hasIntersectingPairs (Slist pLines _) = any (\(pl1, pl2) -> saneIntersect $ plinesIntersectIn pl1 pl2) $ getPairs pLines
        where
          saneIntersect (IntersectsIn _) = True
          saneIntersect _                = False
  -- FIXME: if we have multiple intersecting pairs, is there a preferred pair to use for resolving? angle based, etc?
  pPointOf iNode@(INode firstPLine secondPLine morePLines _)
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
                        [] -> error $ "no intersection of pairs for " <> show allPLines
                        [_] -> True
                        points -> and $ mapWithFollower (\a b -> distanceBetweenPPoints a b < fudgeFactor) points
      allPLines = if hasArc iNode
                  then cons (outOf iNode) $ cons firstPLine $ cons secondPLine morePLines
                  else cons firstPLine $ cons secondPLine morePLines
      intersectionsOfPairs (Slist pLines _) = catMaybes $ (\(pl1, pl2) -> saneIntersect $ plinesIntersectIn pl1 pl2) <$> getPairs pLines
        where
          saneIntersect (IntersectsIn a) = Just a
          saneIntersect _                = Nothing
  ePointOf a = pToEPoint2 $ pPointOf a

-- | A Motorcycle. a PLine eminating from an intersection between two line segments toward the interior or the exterior of a contour.
--   Motorcycles are emitted from convex (reflex) virtexes of the encircling contour, and concave virtexes of any holes.
--   FIXME: Note that a new motorcycle may be created in the case of degenerate polygons... with it's inSegs being two other motorcycles.
data Motorcycle = Motorcycle { _inCSegs :: !(LineSeg, LineSeg), _outPline :: !PLine2 }
  deriving Eq
  deriving stock Show

instance Arcable Motorcycle where
  -- A Motorcycle always has an arc, which is it's path.
  hasArc _ = True
  outOf (Motorcycle _ outArc) = outArc

instance Pointable Motorcycle where
  -- A motorcycle always contains a point.
  canPoint _ = True
  pPointOf a = eToPPoint2 $ ePointOf a
  ePointOf (Motorcycle (_, LineSeg point _) _) = point

-- | The motorcycles that are involved in dividing two cells.
data DividingMotorcycles = DividingMotorcycles { firstMotorcycle :: !Motorcycle, moreMotorcycles :: !(Slist Motorcycle) }
  deriving Eq
  deriving stock Show

-- A concave region of a contour.
newtype Cell = Cell { _sides :: Slist (Slist LineSeg, Maybe CellDivide)}
  deriving Eq
  deriving stock Show

-- | the border dividing two cells of a contour.
-- note that if there is an ENode that is part of the division, it's anticolinnear to the last motorcycle in _divMotorcycles.
data CellDivide = CellDivide { _divMotorcycles :: !DividingMotorcycles, _divENode :: !(Maybe ENode) }
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
data StraightSkeleton = StraightSkeleton { _nodeSets :: ![[NodeTree]], _spineNodes :: !(Slist Spine) }
  deriving Eq
  deriving stock Show

-- | Cut a list into all possible pairs. Used in a few places, but here because the Pointable instance for INode uses it.
getPairs :: [a] -> [(a,a)]
getPairs [] = []
getPairs (x:xs) = ((x,) <$> xs) ++ getPairs xs

---------------------------------------
-- Utility functions for our solvers --
---------------------------------------

-- | convert an ENode to an INode.
eNodeToINode :: ENode -> INode
eNodeToINode (ENode (seg1, seg2) arc) = INode (eToPLine2 seg1) (eToPLine2 seg2) (slist []) (Just arc)

-- | check if two lines cannot intersect.
noIntersection :: PLine2 -> PLine2 -> Bool
noIntersection pline1 pline2 = isCollinear pline1 pline2 || isParallel pline1 pline2 || isAntiCollinear pline1 pline2 || isAntiParallel pline1 pline2

-- | check if two lines are really the same line.
isCollinear :: PLine2 -> PLine2 -> Bool
isCollinear pline1 pline2 = plinesIntersectIn pline1 pline2 == PCollinear

-- | check if two lines are really the same line.
isAntiCollinear :: PLine2 -> PLine2 -> Bool
isAntiCollinear pline1 pline2 = plinesIntersectIn pline1 pline2 == PAntiCollinear

-- | check if two lines are parallel.
isParallel :: PLine2 -> PLine2 -> Bool
isParallel pline1 pline2 = plinesIntersectIn pline1 pline2 == PParallel

-- | check if two lines are anti-parallel.
isAntiParallel :: PLine2 -> PLine2 -> Bool
isAntiParallel pline1 pline2 = plinesIntersectIn pline1 pline2 == PAntiParallel

-- | Get the intersection point of two lines we know have an intersection point.
intersectionOf :: PLine2 -> PLine2 -> PPoint2
intersectionOf pl1 pl2 = saneIntersection $ plinesIntersectIn pl1 pl2
  where
    saneIntersection PAntiCollinear   = error $ "cannot get the intersection of anti-collinear lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PCollinear       = error $ "cannot get the intersection of collinear lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PParallel        = error $ "cannot get the intersection of parallel lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PAntiParallel    = error $ "cannot get the intersection of antiparallel lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection (IntersectsIn point) = point

-- | Get pairs of lines from the contour, including one pair that is the last line paired with the first.
linePairs :: Contour -> [(LineSeg, LineSeg)]
linePairs c = mapWithFollower (,) $ lineSegsOfContour c

-- | A smart constructor for INodes.
makeINode :: [PLine2] -> Maybe PLine2 -> INode
makeINode pLines maybeOut = case pLines of
                              [] -> error "tried to construct a broken INode"
                              [onePLine] -> error $ "tried to construct a broken INode from one PLine2: " <> show onePLine <> "\n"
                              [first,second] -> INode first second (slist []) maybeOut
                              (first:second:more) -> INode first second (slist more) maybeOut

-- | Get the output of the given nodetree. fails if the nodetree has no output.
finalPLine :: NodeTree -> PLine2
finalPLine (NodeTree (ENodeSet (Slist [(firstENode,moreENodes)] _)) iNodeSet)
  | hasNoINodes iNodeSet = if len moreENodes == 0
                           then outOf firstENode
                           else error "cannot have final PLine of NodeTree with more than one ENode, and no generations!\n"
  | hasArc (finalINodeOf iNodeSet) = outOf $ finalINodeOf iNodeSet
  | otherwise = error "has inodes, has no out, has enodes?"
finalPLine (NodeTree _ iNodeSet)
  | hasNoINodes iNodeSet = error "cannot have final PLine of a NodeTree that is completely empty!"
  | hasArc (finalINodeOf iNodeSet) = outOf $ finalINodeOf iNodeSet
  | otherwise = error "has inodes, has no out, has no enodes?"

-- | get the last output PLine of a NodeTree, if there is one. otherwise, Nothing.
finalOutOf :: NodeTree -> Maybe PLine2
finalOutOf (NodeTree eNodeSet iNodeSet)
  | hasNoINodes iNodeSet = case eNodeSet of
                             (ENodeSet (Slist [(firstNode,Slist [] _)] _)) -> Just $ outOf firstNode
                             _ -> Nothing
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
    ancestors = init generations

-- | Examine two line segments that are part of a Contour, and determine if they are concave toward the interior of the Contour. if they are, construct a PLine2 bisecting them, pointing toward the interior of the Contour.
concavePLines :: LineSeg -> LineSeg -> Maybe PLine2
concavePLines seg1 seg2
  | Just True == lineIsLeft seg1 seg2  = Just $ PLine2 $ addVecPair pv1 pv2
  | otherwise                          = Nothing
  where
    (PLine2 pv1) = eToPLine2 seg1
    (PLine2 pv2) = flipPLine2 $ eToPLine2 seg2

-- | check if an INodeSet is empty.
hasNoINodes :: INodeSet -> Bool
hasNoINodes iNodeSet = case iNodeSet of
                         (INodeSet (Slist _ 0)) -> True
                         _ -> False

-- | Sort a set of PLines. yes, this is 'backwards', to match the counterclockwise order of contours.
sortedPLines :: [PLine2] -> [PLine2]
sortedPLines = sortBy (\n1 n2 -> if (n1 `pLineIsLeft` n2) == Just True then LT else GT)

-- | take a sorted list of PLines, and make sure the list starts with the pline closest to (but not left of) the given PLine.
indexPLinesTo :: PLine2 -> [PLine2] -> [PLine2]
indexPLinesTo firstPLine pLines = pLinesBeforeIndex firstPLine pLines <> pLinesAfterIndex firstPLine pLines
  where
    pLinesBeforeIndex myFirstPLine = filter (\a -> myFirstPLine `pLineIsLeft` a /= Just False)
    pLinesAfterIndex myFirstPLine = filter (\a -> myFirstPLine `pLineIsLeft` a == Just False)
