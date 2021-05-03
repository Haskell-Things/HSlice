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

module Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton(StraightSkeleton), Spine(Spine), ENode(ENode), INode(INode), NodeTree(NodeTree), Motorcycle(Motorcycle), Arcable(hasArc, outOf), Pointable(canPoint, ePointOf, pPointOf), eNodeToINode, noIntersection, isCollinear, isParallel, intersectionOf, getPairs, linesOfContour, linePairs, motorcycleToENode) where

import Prelude (Eq, Show, Bool(True, False), otherwise, ($), last, (<$>), (==), (++), error, length, (>), (&&), any, head, fst, and, (||), (<>), show)

import Data.List.NonEmpty (NonEmpty)

import Data.List.Unique (count_)

import Data.Maybe( Maybe(Just,Nothing), catMaybes, isJust, fromJust)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg), makeLineSegsLooped)

import Graphics.Slicer.Math.PGA (pToEPoint2, PPoint2, plinesIntersectIn, PIntersection(PCollinear,IntersectsIn,PParallel,PAntiParallel), eToPPoint2, PLine2, eToPLine2)

import Graphics.Slicer.Math.Definitions (Contour(PointSequence), Point2, mapWithFollower)

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
data ENode = ENode { _inSegs :: (LineSeg, LineSeg), _arcOut :: PLine2 }
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
data INode = INode { _inArcs :: [PLine2], _outArc :: Maybe PLine2 }
  deriving Eq
  deriving stock Show

instance Arcable INode where
  -- an INode might just end here.
  hasArc (INode _ outArc) = isJust outArc
  outOf (INode _ outArc)
    | isJust outArc = fromJust outArc
    | otherwise     = error "tried to get an outArc that has no output arc."

instance Pointable INode where
  -- an INode does not contain a point, we have to attempt to resolve one instead.
  canPoint iNode@(INode (plines) _) = length allPLines > 1 && hasIntersectingPairs
    where
      allPLines = if hasArc iNode
                  then outOf iNode : plines
                  else plines
      hasIntersectingPairs = any (\(pl1, pl2) -> saneIntersect $ plinesIntersectIn pl1 pl2) $ getPairs allPLines
        where
          saneIntersect (IntersectsIn _) = True
          saneIntersect _                = False
  -- FIXME: if we have multiple intersecting pairs, is there a preferred pair to use for resolving? angle based, etc?
  pPointOf iNode@(INode plines _)
    | allPointsSame = head intersectionsOfPairs
    -- Allow the pebbles to vote.
    | otherwise = fst $ last $ count_ intersectionsOfPairs
    where
      allPointsSame = and $ mapWithFollower (==) intersectionsOfPairs
      allPLines = if hasArc iNode
                  then outOf iNode : plines
                  else plines
      intersectionsOfPairs = catMaybes $ (\(pl1, pl2) -> saneIntersect $ plinesIntersectIn pl1 pl2) <$> getPairs allPLines
        where
          saneIntersect (IntersectsIn a) = Just a
          saneIntersect _                = Nothing
  ePointOf a = pToEPoint2 $ pPointOf a 

-- | A Motorcycle. a PLine eminating from an intersection between two line segments toward the interior or the exterior of a contour.
--   Motorcycles are emitted from convex (reflex) virtexes of the encircling contour, and concave virtexes of any holes.
data Motorcycle = Motorcycle { _inCSegs :: (LineSeg, LineSeg), _outPline :: PLine2 }
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

-- | The collision of two motorcycles. one lives, and one doesn't, unless it's a head on collision, in which case both die, and there is no survivor.
data Crash = Crash { _inMotorcycles :: (Motorcycle, Motorcycle), _survivor :: (Maybe Motorcycle)}

-- | A set of set of nodes, divided into 'generations', where each generation is a set of nodes that (may) result in the next set of nodes. the last generation contains just one node.
--   Note that not all of the outArcs in a given generation necessarilly are used in the next generation, but they must all be used by following generations in order for a nodetree to be complete.
--   The last generation may or may not have an outArc.
data NodeTree = NodeTree { _eNodes :: [ENode], _iNodes :: [[INode]] }
  deriving Eq
  deriving stock Show

-- | A Spine component:
--   Similar to a node, only without the in and out heirarchy. always connects to inArcs from a NodeTree. One per generation. allows us to build loops.
newtype Spine = Spine { _spineArcs :: NonEmpty PLine2 }
  deriving newtype Eq
  deriving stock Show

-- | The straight skeleton of a contour.
data StraightSkeleton = StraightSkeleton { _nodeSets :: [[NodeTree]], _spineNodes :: [Spine] }
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
eNodeToINode (ENode (seg1, seg2) arc) = INode [eToPLine2 seg1, eToPLine2 seg2] (Just arc)

-- | convert a Motorcycle to an ENode
motorcycleToENode :: Motorcycle -> ENode
motorcycleToENode (Motorcycle segs mcpath) = ENode segs mcpath

-- | check if two lines cannot intersect.
noIntersection :: PLine2 -> PLine2 -> Bool
noIntersection pline1 pline2 = isCollinear pline1 pline2 || isParallel pline1 pline2

-- | check if two lines are really the same line.
isCollinear :: PLine2 -> PLine2 -> Bool
isCollinear pline1 pline2 = plinesIntersectIn pline1 pline2 == PCollinear

-- | check if two lines are parallel.
isParallel :: PLine2 -> PLine2 -> Bool
isParallel pline1 pline2 =    plinesIntersectIn pline1 pline2 == PParallel
                           || plinesIntersectIn pline1 pline2 == PAntiParallel

-- | Get the intersection point of two lines we know have an intersection point.
intersectionOf :: PLine2 -> PLine2 -> PPoint2
intersectionOf pl1 pl2 = saneIntersection $ plinesIntersectIn pl1 pl2
  where
    saneIntersection PCollinear       = error $ "cannot get the intersection of collinear lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PParallel        = error $ "cannot get the intersection of parallel lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection PAntiParallel    = error $ "cannot get the intersection of antiparallel lines.\npl1: " <> show pl1 <> "\npl2: " <> show pl2 <> "\n"
    saneIntersection (IntersectsIn point) = point

linesOfContour :: Contour -> [LineSeg]
linesOfContour (PointSequence contourPoints) = makeLineSegsLooped contourPoints

-- Get pairs of lines from the contour, including one pair that is the last line paired with the first.
linePairs :: Contour -> [(LineSeg, LineSeg)]
linePairs c = mapWithFollower (,) $ linesOfContour c

