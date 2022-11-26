{- ORMOLU_DISABLE -}
{-
 - Copyright 2016 Noah Halford and Catherine Moresco
 - Copyright 2019 Julia Longtin
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
 -
 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{- | No reason to get excited. just some dumping facilities.

 to use:

 @
 juri@ni:/disk4/faikvm.com/HSlice/HSlice-current$ cabal repl
 Ok, 30 modules loaded.
 *Graphics.Slicer> import Prelude (($),putStrLn)
 *Graphics.Slicer Prelude> import Graphics.Slicer.Math.Ganja(dumpGanja)
 *Graphics.Slicer Prelude Graphics.Slicer.Math.Ganja> putStrLn $ dumpGanja (Point2 (1,1))
 Algebra(2,0,1,()=>{
   var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
   var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
   var pa = point(1.0,1.0);
   document.body.appendChild(this.graph([
     pa, "pa",
   ],{
     grid: true,
     labels: true,
     lineWidth: 3,
     pointRadius: 1,
     fontSize: 1,
     scale: 1,
   }));
 });
 @

 or:

 @
 juri@ni:/disk4/faikvm.com/HSlice/HSlice-current$ cabal repl
 Ok, 30 modules loaded.
 *Graphics.Slicer> import Prelude (($),putStrLn)
 *Graphics.Slicer Prelude> import Graphics.Slicer.Math.Ganja(dumpGanja)
 *Graphics.Slicer Prelude Graphics.Slicer.Math.Ganja> putStrLn $ dumpGanjas [toGanja (Point2 (1,1)), toGanja (Point2 (2,2))]
 Algebra(2,0,1,()=>{
   var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
   var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
   var a = point(1.0,1.0);
   var b = point(2.0,2.0);
   document.body.appendChild(this.graph([
     a, "a",
     b, "b",
   ],{
     grid: true,
     labels: true,
     lineWidth: 3,
     pointRadius: 1,
     fontSize: 1,
     scale: 1,
 }));
 });
 @

 cut, and paste the output into https://enkimute.github.io/ganja.js/examples/coffeeshop.html, click 'Run'
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- so we can define a Num instance for Positive.
{-# OPTIONS_GHC -Wno-orphans #-}

module Graphics.Slicer.Math.Ganja (GanjaAble, ListThree, Radian(Radian), edgesOf, generationsOf, toGanja, dumpGanja, dumpGanjas, randomTriangle, randomSquare, randomRectangle, randomConvexDualRightQuad, randomConvexSingleRightQuad, randomConvexBisectableQuad, randomConvexQuad, randomConcaveChevronQuad, randomENode, randomINode, randomPLine, randomPLineWithErr, randomLineSeg, cellFrom, remainderFrom, onlyOne, onlyOneOf, randomPLineThroughOrigin, randomLineSegFromOriginNotX1Y1, randomX1Y1LineSegToOrigin, randomX1Y1LineSegToPoint, randomLineSegFromPointNotX1Y1, randomPLineThroughPoint) where

import Prelude (Bool, Enum, Eq, Fractional, Num, Ord, Show, String, Int, (<>), (<>), (<$>), ($), (>=), (==), abs, concat, error, fromInteger, fromRational, fst, mod, otherwise, replicate, show, signum, snd, zip, (.), (+), (-), (*), (<), (/), (>), (<=), (&&), (/=))

import Data.Coerce (coerce)

import Data.List (sort, concatMap)

import Data.List.Unique (allUnique)

import Data.Maybe (Maybe(Nothing, Just), catMaybes, fromMaybe)

import Math.Tau (tau)

import Numeric(showFFloat, pi)

import Slist.Type (Slist(Slist))

import Slist (last, len)

import Test.QuickCheck (Arbitrary, Positive(Positive), NonZero(NonZero), arbitrary, shrink, suchThat, vector)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

import Graphics.Slicer.Math.Arcs (getOutsideArc)

import Graphics.Slicer.Math.Contour (makePointContour, maybeFlipContour, pointsOfContour, firstPointPairOfContour, pointFarOutsideContour)

import Graphics.Slicer.Math.Definitions (Contour, Point2(Point2), LineSeg, endPoint, mapWithFollower, startPoint, makeLineSeg)

import Graphics.Slicer.Math.GeometricAlgebra (GNum(GEPlus, GEZero), GVec(GVec), getVal, valOf)

import Graphics.Slicer.Math.Lossy (eToPLine2, join2PPoints, pPointBetweenPPoints, pToEPoint2, translateRotatePPoint2)

import Graphics.Slicer.Math.PGA (ProjectivePoint(CPPoint2, PPoint2), ProjectiveLine(NPLine2, PLine2), PLine2Err, PPoint2Err, eToPL, eToPPoint2, hasArc, flipL, normalizeL, outOf, pPointOf)

import Graphics.Slicer.Math.Skeleton.Concave (makeENode)

import Graphics.Slicer.Math.Skeleton.Definitions(Cell(Cell), ENode, ENodeSet(ENodeSet), INode(INode), INodeSet(INodeSet), Motorcycle(Motorcycle), NodeTree(NodeTree), StraightSkeleton(StraightSkeleton), RemainingContour(RemainingContour), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), getFirstLineSeg, getLastLineSeg, makeINode)

import Graphics.Slicer.Math.Skeleton.Face(Face(Face))

class GanjaAble a where
  toGanja :: a -> String -> (String, String)

instance GanjaAble String where
  toGanja string varname = (" // " <> varname <> " -- " <> string <> "\n","")

instance GanjaAble Point2 where
  toGanja (Point2 (x,y)) varname = (
    "  var " <> varname <> " = point(" <> showFullPrecision x <> "," <> showFullPrecision y <> ");\n",
    "    " <> varname <> ", " <> show varname <> ",\n")
    where
      -- because ganja's website does not handle scientific notation.
      showFullPrecision v = showFFloat Nothing v ""

instance GanjaAble LineSeg where
  toGanja l1 varname = (
       p1var
    <> p2var,
       "    0x882288,\n"
    <> "    [" <> varname <> "a," <> varname <> "b],\n"
    <> "    0x00AA88,\n"
    <> p1ref
    <> p2ref)
    where
      (p1var, p1ref) = toGanja (startPoint l1) (varname <> "a")
      (p2var, p2ref) = toGanja (endPoint l1) (varname <> "b")

instance GanjaAble ProjectivePoint where
  toGanja ppoint varname = (
    "  var " <> varname <> " = "
      <> showFullPrecision (valOf 0 (getVal [GEPlus 1, GEPlus 2] vals)) <> "e12"
      <> (if e02 >= 0 then "+" <> showFullPrecision e02 else showFullPrecision e02)
      <> "e02"
      <> (if e01 >= 0 then "+" <> showFullPrecision e01 else showFullPrecision e01)
      <> "e01;\n"
    ,
    "    " <> varname <> ", " <> show varname <> ",\n")
    where
      e02 = valOf 0 (getVal [GEZero 1, GEPlus 2] vals)
      e01 = valOf 0 (getVal [GEZero 1, GEPlus 1] vals)
      -- because ganja's website does not handle scientific notation.
      showFullPrecision v = showFFloat Nothing v ""
      vals = case ppoint of
        (PPoint2 (GVec v)) -> v
        (CPPoint2 (GVec v)) -> v

instance GanjaAble (ProjectivePoint,PPoint2Err) where
  toGanja (ppoint,pErr) varname = (
    "  var " <> varname <> " = "
      <> showFullPrecision (valOf 0 (getVal [GEPlus 1, GEPlus 2] vals)) <> "e12"
      <> (if e02 >= 0 then "+" <> showFullPrecision e02 else showFullPrecision e02)
      <> "e02"
      <> (if e01 >= 0 then "+" <> showFullPrecision e01 else showFullPrecision e01)
      <> "e01;\n"
      <> "// " <> show pErr <> "\n"
    ,
    "    " <> varname <> ", " <> show varname <> ",\n")
    where
      e02 = valOf 0 (getVal [GEZero 1, GEPlus 2] vals)
      e01 = valOf 0 (getVal [GEZero 1, GEPlus 1] vals)
      -- because ganja's website does not handle scientific notation.
      showFullPrecision v = showFFloat Nothing v ""
      vals = case ppoint of
        (PPoint2 (GVec v)) -> v
        (CPPoint2 (GVec v)) -> v

instance GanjaAble ProjectiveLine where
  toGanja pline varname = (
    "  var " <> varname <> " = "
      <> showFullPrecision (valOf 0 (getVal [GEPlus 1] vals)) <> "e1"
      <> (if e2 >= 0 then "+" <> showFullPrecision e2 else showFullPrecision e2)
      <> "e2"
      <> (if e0 >= 0 then "+" <> showFullPrecision e0 else showFullPrecision e0)
      <> "e0;\n"
    ,
    "    " <> varname <> ", " <> show varname <> ",\n")
    where
      e2 = valOf 0 (getVal [GEPlus 2] vals)
      e0 = valOf 0 (getVal [GEZero 1] vals)
      -- because ganja's website does not handle scientific notation.
      showFullPrecision v = showFFloat Nothing v ""
      vals = case pline of
        (PLine2 (GVec v)) -> v
        (NPLine2 (GVec v)) -> v

instance GanjaAble Contour where
  toGanja contour varname = (invars, inrefs)
    where
      contourPoints = pointsOfContour contour
      (invars, inrefs) = (concatMap fst res, concatMap snd res <> "    0x882288,\n" <> linePairs)
        where
          linePairs    = concat $ mapWithFollower (\(_,a) (_,b) -> "    [" <> varname <> a <> "," <> varname <> b <> "],\n") pairs
          res          = (\(a,b) -> toGanja a (varname <> b)) <$> pairs
          pairs        = zip contourPoints allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]

instance GanjaAble ENode where
  toGanja eNode varname = (
    l1var
    <> l2var
    <> plvar
    ,
    l1ref
    <> l2ref
    <> plref
    )
    where
      (l1var, l1ref) = toGanja (getFirstLineSeg eNode) (varname <> "a")
      (l2var, l2ref) = toGanja (getLastLineSeg eNode) (varname <> "b")
      (plvar, plref) = toGanja (outOf eNode) (varname <> "c")

instance GanjaAble Motorcycle where
  toGanja (Motorcycle (l1, l2) outPLine _) varname = (
    l1var
    <> l2var
    <> plvar
    ,
    l1ref
    <> l2ref
    <> plref
    )
    where
      (l1var, l1ref) = toGanja l1 (varname <> "a")
      (l2var, l2ref) = toGanja l2 (varname <> "b")
      (plvar, plref) = toGanja outPLine (varname <> "c")

instance GanjaAble Cell where
  toGanja (Cell segsDivides) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip allSides allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allSegs      = concatMap (listFromSlist . fst) segsDivides
          allDivides   = catMaybes $ listFromSlist $ snd <$> segsDivides
          allSides     = (toGanja <$> allSegs) <> (toGanja <$> allDivides)
          listFromSlist (Slist a _) = a

instance GanjaAble CellDivide where
  toGanja (CellDivide (DividingMotorcycles firstMotorcycle (Slist moreMotorcycles _)) _) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          res            = (\(a,b) -> toGanja a (varname <> b)) <$> zip allMotorcycles allStrings
          allStrings     = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allMotorcycles =   firstMotorcycle:moreMotorcycles

instance GanjaAble RemainingContour where
  toGanja (RemainingContour (Slist segsDivides _)) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip allSides allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allSegs      = concatMap (listFromSlist . fst) segsDivides
          allDivides   = concatMap snd segsDivides
          allSides     = (toGanja <$> allSegs) <> (toGanja <$> allDivides)
          listFromSlist (Slist a _) = a

instance GanjaAble INode where
  toGanja inode@(INode firstPLine secondPLine (Slist rawMorePLines _) _) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          res          = (\(a,b) -> toGanja a (varname <> b)) <$> zip allPLines allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allPLines    =   firstPLine:secondPLine:rawMorePLines <> (if hasArc inode then [outOf inode] else [])

instance GanjaAble StraightSkeleton where
  toGanja (StraightSkeleton (Slist [[nodetree]] _) _) = toGanja nodetree
  toGanja a = error $ "no a, only b: " <> show a <> "\n"

instance GanjaAble NodeTree where
  toGanja (NodeTree (ENodeSet eNodeSides) iNodeSet) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip (allEdges <> allINodes) allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allEdges     = toGanja <$> (firstLine <> remainingLines)
          allINodes    = toGanja <$> iNodesOf iNodeSet
          firstLine    = case eNodeSides of
                           (Slist [] _) -> []
                           (Slist [(firstNode,Slist [] _)] _) -> [getFirstLineSeg firstNode]
                           (Slist [(firstNode,otherNodes)] _) -> if getFirstLineSeg firstNode /= getLastLineSeg (last otherNodes)
                                                                 then [getFirstLineSeg firstNode]
                                                                 else []
                           (Slist _ _) -> error "too many sides."
          remainingLines
            | len eNodeSides == 0 = []
            | otherwise = getLastLineSeg <$> eNodesOf eNodeSides
            where
              eNodesOf (Slist [] _) = error "no enodes?"
              eNodesOf (Slist [(first,Slist more _)] _) = first : more
              eNodesOf (Slist _ _) = error "too many sides?"
          iNodesOf :: INodeSet -> [INode]
          iNodesOf (INodeSet (Slist inodes _)) = concat inodes

instance GanjaAble Face where
  toGanja (Face edge firstArc (Slist arcs _) lastArc) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip (toGanja edge : allPLines) allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allPLines    = toGanja <$> ([firstArc] <> arcs <> [lastArc])

instance GanjaAble (Slist Face) where
  toGanja (Slist faces _) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          allArcs      = concatMap (\(Face _ firstArc (Slist arcs _) lastArc) -> [firstArc] <> arcs <> [lastArc]) faces
          allEdges     = (\(Face edge _ _ _) -> toGanja edge) <$> faces
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip (allEdges <> allPLines) allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allPLines    = toGanja <$> allArcs

-- | Create a single program, covering a series of objects.
dumpGanjas :: [String -> (String, String)] -> String
dumpGanjas [] = error "no items to dump."
dumpGanjas [f] = ganjaHeader <> vars <> ganjaFooterStart <> refs <> ganjaFooterEnd
  where
    (vars, refs) = f "a"
dumpGanjas xs = ganjaHeader <> vars <> ganjaFooterStart <> refs <> ganjaFooterEnd
  where
    (vars, refs) =  (concatMap fst res, concatMap snd res)
      where
        res          = (\(a,b) -> a b) <$> zip xs allStrings
        allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]

-- | create a single program for a single object.
dumpGanja :: (GanjaAble a) => a -> String
dumpGanja target = ganjaHeader <> vars <> ganjaFooterStart <> refs <> ganjaFooterEnd
  where
    (vars, refs) = toGanja target "a"

-- | the headaer at the beginning of all programs.
ganjaHeader :: String
ganjaHeader = "Algebra(2,0,1,()=>{\n  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;\n  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);\n"

-- | the beginning of the footer section.
ganjaFooterStart :: String
ganjaFooterStart = "  document.body.appendChild(this.graph([\n"

-- | the end of the footer.
ganjaFooterEnd :: String
ganjaFooterEnd = "  ],{\n"
                 <> "    grid: true,\n"
                 <> "    labels: true,\n"
                 <> "    lineWidth: 3,\n"
                 <> "    pointRadius: 1,\n"
                 <> "    fontSize: 1,\n"
                 <> "    scale: 1,\n"
                 <> "}));\n"
                 <> "});\n"

-- moved the below here from the test suite, so that we can drop lines from the test suite into ghci directly.

-- A type for a list of three items. so we can gather a list of exactly three distances / radians.
newtype ListThree a = ListThree {getListThree :: [a]}
  deriving (Show, Ord, Eq)

instance (Arbitrary a) => Arbitrary (ListThree a) where
  arbitrary = ListThree <$> vector 3

-- Radians are always positive, and always between 0 and tau+minfloat.
newtype Radian a = Radian {getRadian :: ℝ}
  deriving (Show, Ord, Eq, Enum)

instance Arbitrary (Radian a) where
  arbitrary = Radian <$> (arbitrary `suchThat` (\a -> a > 0 && a <= tau))
  shrink (Radian x) = [ Radian x' | x' <- shrink x , x' > 0 ]

instance Num (Radian a) where
  (+) (Radian r1) (Radian r2) = Radian $ wrapIfNeeded $ r1 + r2
    where
      wrapIfNeeded :: ℝ -> ℝ
      wrapIfNeeded v
        | v <= tau   = v
        | otherwise = v-tau
  (-) (Radian r1) (Radian r2) = Radian $ wrapIfNeeded $ r1 - r2
    where
      wrapIfNeeded :: ℝ -> ℝ
      wrapIfNeeded v
        | v > 0     = v
        | otherwise = v+tau
  (*) (Radian r1) (Radian r2) = Radian $ recursiveWrap $ r1 * r2
    where
      recursiveWrap :: ℝ -> ℝ
      recursiveWrap v
        | v <= tau   = v
        | otherwise = recursiveWrap $ v-tau
  abs r1 = r1
  fromInteger v = Radian $ fromInteger $ mod v 6
  signum _ = 1

-- | so we can do some arithmatic on positive numbers.
-- FIXME: Yes, this is an orphan instance.
instance (Ord a, Num a) => Num (Positive a) where
  (+) (Positive r1) (Positive r2) = Positive $ r1 + r2
  (-) (Positive r1) (Positive r2)
    | r1 < r2 = Positive $  r1 - r2
    | otherwise = error "tried to produce a negative number."
  (*) (Positive r1) (Positive r2) = Positive $ r1 * r2
  abs r1 = r1
  fromInteger v = Positive $ fromInteger v
  signum _ = 1

instance Fractional (Radian a) where
  (/) (Radian r1) (Radian r2) = Radian $ r1 / r2
  fromRational a = Radian $ fromRational a

instance (Ord a, Num a, Fractional a) => Fractional (Positive a) where
  (/) (Positive r1) (Positive r2) = Positive $ r1 / r2
  fromRational a = Positive $ fromRational a

-- | Generate a random triangle.
-- FIXME: what stops this from trying to generate a triangle with all three points on the same line?
randomTriangle :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Contour
randomTriangle centerX centerY rawRadians rawDists = randomStarPoly centerX centerY $ makePairs dists radians
  where
    radians :: [Radian ℝ]
    radians = coerce rawRadians
    dists :: [Positive ℝ]
    dists = coerce rawDists

-- | Generate a random square.
randomSquare :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Contour
randomSquare centerX centerY tilt distanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
  where
    radians =
      [
        tilt
      , tilt + Radian (tau/4)
      , tilt + Radian (tau/2)
      , tilt + Radian (pi+(pi/2))
      ]
    distances = replicate 4 distanceToCorner

-- | Generate a random rectangle.
randomRectangle :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Contour
randomRectangle centerX centerY rawFirstTilt secondTilt distanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      -- Workaround: since first and second may be unique, but may not be 0, add them!
      firstTilt
        | rawFirstTilt == secondTilt = rawFirstTilt + secondTilt
        | otherwise = rawFirstTilt
      radians :: [Radian ℝ]
      radians =
        [
          firstTilt
        , secondTilt
        , flipRadian firstTilt
        , flipRadian secondTilt
        ]
      flipRadian :: Radian ℝ -> Radian ℝ
      flipRadian v
        | v < Radian pi = v + Radian pi
        | otherwise     = v - Radian pi
      distances = replicate 4 distanceToCorner

-- | Generate a random convex four sided polygon, with two right angles.
randomConvexDualRightQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Contour
randomConvexDualRightQuad centerX centerY rawFirstTilt rawSecondTilt rawThirdTilt distanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstTilt, secondTilt, thirdTilt] = sort $ ensureUnique $ clipRadian <$> sort [rawFirstTilt, rawSecondTilt, rawThirdTilt]
      ensureUnique :: [Radian ℝ] -> [Radian ℝ]
      ensureUnique vals
        | allUnique vals = vals
        | otherwise = ensureUnique $ sort [v*m | m <- [2,3,5] | v <- vals]
      radians :: [Radian ℝ]
      radians =
        [
          firstTilt
        , secondTilt
        , thirdTilt
        , flipRadian secondTilt
        ]
      flipRadian :: Radian ℝ -> Radian ℝ
      flipRadian v
        | v < Radian pi = v + Radian pi
        | otherwise     = v - Radian pi
      clipRadian v
        | v > Radian pi = v - Radian pi
        | otherwise = v
      distances = replicate 4 distanceToCorner

-- | Generate a random convex four sided polygon, with one right angle.
randomConvexSingleRightQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Contour
randomConvexSingleRightQuad centerX centerY rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstDistanceToCorner, secondDistanceToCorner] = sort $ ensureUniqueDistance $ sort [rawFirstDistanceToCorner, rawSecondDistanceToCorner]
      ensureUniqueDistance :: [Positive ℝ] -> [Positive ℝ]
      ensureUniqueDistance vals
        | allUnique vals = vals
        | otherwise = ensureUniqueDistance $ sort [v*m | m <- [2,3] | v <- vals]
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstTilt, secondTilt, thirdTilt] = sort $ ensureUnique $ clipRadian <$> sort [rawFirstTilt, rawSecondTilt, rawThirdTilt]
      ensureUnique :: [Radian ℝ] -> [Radian ℝ]
      ensureUnique vals
        | allUnique vals = vals
        | otherwise = ensureUnique $ sort [v*m | m <- [2,3,5] | v <- vals]
      radians :: [Radian ℝ]
      radians =
        [
          firstTilt
        , secondTilt
        , thirdTilt
        , flipRadian secondTilt
        ]
      flipRadian :: Radian ℝ -> Radian ℝ
      flipRadian v
        | v < Radian pi = v + Radian pi
        | otherwise     = v - Radian pi
      clipRadian v
        | v > Radian pi = v - Radian pi
        | otherwise = v
      distances = firstDistanceToCorner : replicate 3 secondDistanceToCorner

-- | Generate a random convex four sided polygon, with the property that it can be folded down an axis.
randomConvexBisectableQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Contour
randomConvexBisectableQuad centerX centerY rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstDistanceToCorner, secondDistanceToCorner] = sort $ ensureUniqueDistance $ sort [rawFirstDistanceToCorner, rawSecondDistanceToCorner]
      ensureUniqueDistance :: [Positive ℝ] -> [Positive ℝ]
      ensureUniqueDistance vals
        | allUnique vals = vals
        | otherwise = ensureUniqueDistance $ sort [v*m | m <- [2,3] | v <- vals]
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstTilt, secondTilt] = sort $ ensureUnique $ clipRadian <$> sort [rawFirstTilt, rawSecondTilt]
      ensureUnique :: [Radian ℝ] -> [Radian ℝ]
      ensureUnique vals
        | allUnique vals = vals
        | otherwise = ensureUnique $ sort [v*m | m <- [2,3] | v <- vals]
      thirdTilt = secondTilt + (secondTilt - firstTilt)
      radians :: [Radian ℝ]
      radians =
        [
          firstTilt
        , secondTilt
        , thirdTilt
        , flipRadian secondTilt
        ]
      flipRadian :: Radian ℝ -> Radian ℝ
      flipRadian v
        | v < Radian pi = v + Radian pi
        | otherwise     = v - Radian pi
      clipRadian v
        | v > Radian pi = v - Radian pi
        | otherwise = v
      distances = [firstDistanceToCorner, secondDistanceToCorner, firstDistanceToCorner, secondDistanceToCorner]

-- | Generate a random convex four sided polygon.
randomConvexQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Contour
randomConvexQuad centerX centerY rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstDistanceToCorner, secondDistanceToCorner, thirdDistanceToCorner] = sort $ ensureUniqueDistance $ sort [rawFirstDistanceToCorner, rawSecondDistanceToCorner, rawThirdDistanceToCorner]
      ensureUniqueDistance :: [Positive ℝ] -> [Positive ℝ]
      ensureUniqueDistance vals
        | allUnique vals = vals
        | otherwise = ensureUniqueDistance $ sort [v*m | m <- [2,3,5] | v <- vals]
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstTilt, secondTilt, thirdTilt] = sort $ ensureUnique $ clipRadian <$> sort [rawFirstTilt, rawSecondTilt, rawThirdTilt]
      ensureUnique :: [Radian ℝ] -> [Radian ℝ]
      ensureUnique vals
        | allUnique vals = vals
        | otherwise = ensureUnique $ sort [v*m | m <- [2,3,5] | v <- vals]
      radians :: [Radian ℝ]
      radians =
        [
          firstTilt
        , secondTilt
        , thirdTilt
        , flipRadian secondTilt
        ]
      flipRadian :: Radian ℝ -> Radian ℝ
      flipRadian v
        | v < Radian pi = v + Radian pi
        | otherwise     = v - Radian pi
      clipRadian v
        | v > Radian pi = v - Radian pi
        | otherwise = v
      distances = [firstDistanceToCorner, thirdDistanceToCorner, secondDistanceToCorner, thirdDistanceToCorner]

-- | Generate a concave four sided polygon, with the convex motorcycle impacting the opposing bend (a 'dart' per wikipedia. a chevron, or a ^.)
-- Note: the center point is always outside of this polygon.
randomConcaveChevronQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Contour
randomConcaveChevronQuad centerX centerY rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstDistanceToCorner, secondDistanceToCorner] = sort $ ensureUniqueDistance $ sort [rawFirstDistanceToCorner, rawSecondDistanceToCorner]
      ensureUniqueDistance :: [Positive ℝ] -> [Positive ℝ]
      ensureUniqueDistance vals
        | allUnique vals = vals
        | otherwise = ensureUniqueDistance $ sort [v*m | m <- [2,3] | v <- vals]
      thirdDistanceToCorner = secondDistanceToCorner / 2
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstTilt, secondTilt] = sort $ ensureUnique $ clipRadian <$> sort [rawFirstTilt, rawSecondTilt]
      ensureUnique :: [Radian ℝ] -> [Radian ℝ]
      ensureUnique vals
        | allUnique vals = vals
        | otherwise = ensureUnique $ sort [v*m | m <- [2,3] | v <- vals]
      radians :: [Radian ℝ]
      radians =
        [
          firstTilt
        , secondTilt
        , flipRadian firstTilt
        , secondTilt
        ]
      flipRadian :: Radian ℝ -> Radian ℝ
      flipRadian v
        | v < Radian pi = v + Radian pi
        | otherwise     = v - Radian pi
      clipRadian v
        | v > Radian pi = v - Radian pi
        | otherwise = v
      distances = [firstDistanceToCorner, secondDistanceToCorner, firstDistanceToCorner, thirdDistanceToCorner]

-- | generate a random polygon.
-- Idea stolen from: https://stackoverflow.com/questions/8997099/algorithm-to-generate-random-2d-polygon
-- note: the centerPoint is assumed to be inside of the contour.
randomStarPoly :: ℝ -> ℝ -> [(Positive ℝ,Radian ℝ)] -> Contour
randomStarPoly centerX centerY radianDistPairs = fromMaybe dumpError $ maybeFlipContour contour
  where
    contour            = makePointContour points
    points             = pToEPoint2 <$> pointsAroundCenter
    pointsAroundCenter = (\(distanceFromPoint, angle) -> translateRotatePPoint2 centerPPoint (coerce distanceFromPoint) (coerce angle)) <$> radianDistPairs
    centerPPoint       = eToPPoint2 $ Point2 (centerX, centerY)
    dumpError          = error $ "failed to flip a contour:" <> dumpGanjas [toGanja contour, toGanja (Point2 (centerX, centerY)), toGanja outsidePLine] <> "\n"
      where
        outsidePLine   = join2PPoints myMidPoint outsidePoint
        outsidePoint   = eToPPoint2 $ pointFarOutsideContour contour
        myMidPoint     = pPointBetweenPPoints (eToPPoint2 p1) (eToPPoint2 p2) 0.5 0.5
        (p1, p2)       = firstPointPairOfContour contour

randomENode :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> ENode
randomENode x y d1 rawR1 d2 rawR2 = makeENode p1 intersectionPoint p2
  where
    r1 = rawR1 / 2
    r2 = r1 + (rawR2 / 2)
    intersectionPoint = Point2 (x,y)
    pp1 = translateRotatePPoint2 intersectionPPoint (coerce d1) (coerce r1)
    pp2 = translateRotatePPoint2 intersectionPPoint (coerce d2) (coerce r2)
    p1 = pToEPoint2 pp1
    p2 = pToEPoint2 pp2
    intersectionPPoint = eToPPoint2 intersectionPoint

randomINode :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> Bool -> Bool -> INode
randomINode x y d1 rawR1 d2 rawR2 flipIn1 flipIn2 = makeINode [maybeFlippedpl1,maybeFlippedpl2] $ Just (bisector1,bisectorErr)
  where
    r1 = rawR1 / 2
    r2 = r1 + (rawR2 / 2)
    pl1 = (\(NPLine2 a) -> PLine2 a) $ fst $ normalizeL $ eToPLine2 $ getFirstLineSeg eNode
    pl2 = (\(NPLine2 a) -> PLine2 a) $ flipL $ eToPLine2 $ getLastLineSeg eNode
    intersectionPPoint = pPointOf eNode
    eNode = randomENode x y d1 rawR1 d2 rawR2
    pp1 = translateRotatePPoint2 intersectionPPoint (coerce d1) (coerce r1)
    pp2 = translateRotatePPoint2 intersectionPPoint (coerce d2) (coerce r2)
    maybeFlippedpl1 = if flipIn1 then flipL pl1 else pl1
    maybeFlippedpl2 = if flipIn2 then flipL pl2 else pl2
    (bisector1, (_, _, bisectorErr)) = getOutsideArc pp1 maybeFlippedpl1 pp2 maybeFlippedpl2

-- | A helper function. constructs a random PLine.
randomPLine :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> ProjectiveLine
randomPLine x y dx dy = fst $ randomPLineWithErr x y dx dy

-- | A helper function. constructs a random PLine.
randomPLineWithErr :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> (ProjectiveLine, PLine2Err)
randomPLineWithErr x y dx dy = eToPL $ makeLineSeg (Point2 (x, y)) (Point2 (coerce dx, coerce dy))

-- | A helper function. constructs a random LineSeg.
randomLineSeg :: ℝ -> ℝ -> ℝ -> ℝ -> LineSeg
randomLineSeg x y rawDx rawDy = makeLineSeg (Point2 (x,y)) (Point2 (rawDx, rawDy))

-- | A PLine that does not follow the X = Y line, and does not follow the other given line.
randomPLineThroughOrigin :: ℝ -> ℝ -> (ProjectiveLine, PLine2Err)
randomPLineThroughOrigin x y = eToPL $ makeLineSeg (Point2 (x,y)) (Point2 (0,0))

-- | A PLine that does not follow the X = Y line, and does not follow the other given line.
randomPLineThroughPoint :: ℝ -> ℝ -> ℝ -> (ProjectiveLine, PLine2Err)
randomPLineThroughPoint x y d = eToPL $ makeLineSeg (Point2 (x,y)) (Point2 (d,d))

-- | A line segment ending at the origin. additionally, guaranteed not to be on the X = Y line.
randomLineSegFromPointNotX1Y1 :: ℝ -> ℝ -> ℝ -> LineSeg
randomLineSegFromPointNotX1Y1 rawX rawY d = res
  where
    res = makeLineSeg (Point2 (d, d)) (Point2 (x, y))
    (x, y)
      | rawX == 0 && rawY == 0 = (0,0.1)
      | rawX == rawY = (rawX,0.1)
      | otherwise = (rawX, rawY)

-- | A line segment ending at the origin. additionally, guaranteed not to be on the X = Y line.
randomLineSegFromOriginNotX1Y1 :: ℝ -> ℝ -> LineSeg
randomLineSegFromOriginNotX1Y1 rawX rawY = res
  where
    res = makeLineSeg (Point2 (0, 0)) (Point2 (x, y))
    (x, y)
      | rawX == 0 && rawY == 0 = (0,0.1)
      | rawX == rawY = (rawX,0.1)
      | otherwise = (rawX, rawY)

randomX1Y1LineSegToOrigin :: NonZero ℝ -> LineSeg
randomX1Y1LineSegToOrigin rawD = res
  where
    res = makeLineSeg (Point2 (d,d)) (Point2 (0,0))
    d :: ℝ
    d = coerce rawD

randomX1Y1LineSegToPoint :: NonZero ℝ -> ℝ -> LineSeg
randomX1Y1LineSegToPoint rawD1 d2 = res
  where
    res = makeLineSeg (Point2 (d1,d1)) (Point2 (d2,d2))
    d1 :: ℝ
    d1 = coerce rawD1

-- | combine two lists. for feeding into randomStarPoly.
makePairs :: [a] -> [b] -> [(a,b)]
makePairs (a:as) (b:bs) = (a,b) : makePairs as bs
makePairs (_:_) [] = error "out of inputs"
makePairs [] (_:_) = []
makePairs [] [] = []

cellFrom :: Maybe (a,b) -> a
cellFrom (Just (v,_)) = v
cellFrom Nothing = error "whoops"

remainderFrom :: Maybe (a,b) -> b
remainderFrom (Just (_,v)) = v
remainderFrom Nothing = error "whoops"

onlyOne :: [a] -> a
onlyOne as = case as of
               [] -> error "none"
               [a] -> a
               (_:_) -> error "too many"

onlyOneOf :: [a] -> a
onlyOneOf as = case as of
                 [] -> error "none"
                 (a:_) -> a

edgesOf :: Slist Face -> [LineSeg]
edgesOf faces = unwrap <$> (\(Slist a _) -> a) faces
  where
    unwrap :: Face -> LineSeg
    unwrap (Face edge _ _ _) = edge

generationsOf :: Maybe StraightSkeleton -> Int
generationsOf Nothing = 0
generationsOf (Just (StraightSkeleton (Slist [] _) _)) = 0
generationsOf (Just (StraightSkeleton a@(Slist [_] _) _)) = len a
generationsOf a = error $ "what is this?" <> show a <> "\n"
