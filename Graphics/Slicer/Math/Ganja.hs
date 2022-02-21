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

module Graphics.Slicer.Math.Ganja (GanjaAble, ListThree, Radian(Radian), toGanja, dumpGanja, dumpGanjas, randomTriangle, randomSquare, randomRectangle, randomENode, randomINode, cellFrom, remainderFrom, onlyOne) where

import Prelude (Bool, Eq, Fractional, Num, Ord, Show, String, (<>), (<>), (<$>), ($), (>=), (==), abs, concat, error, fromInteger, fromRational, fst, mod, otherwise, replicate, show, signum, snd, zip, (.), (+), (-), (*), (<), (/), (>), (<=), (&&))

import Data.Coerce (coerce)

import Data.Maybe (Maybe(Nothing, Just), maybeToList, catMaybes)

import Math.Tau (tau)

import Numeric(showFFloat, pi)

import Slist.Type (Slist(Slist))

import Slist (last, len)

import Test.QuickCheck (Arbitrary, Positive(Positive), arbitrary, shrink, suchThat, vector)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

import Graphics.Slicer.Math.Contour (makePointContour, maybeFlipContour, pointsOfContour)

import Graphics.Slicer.Math.Definitions (Contour, Point2(Point2), LineSeg(LineSeg), mapWithFollower)

import Graphics.Slicer.Math.GeometricAlgebra (GNum(GEPlus, GEZero), GVec(GVec), getVals, valOf)

import Graphics.Slicer.Math.Line (lineSegFromEndpoints, handleLineSegError, endPoint)

import Graphics.Slicer.Math.PGA (PPoint2(PPoint2), PLine2(PLine2), eToPLine2, eToPPoint2, flipPLine2, normalizePLine2, pToEPoint2, translateRotatePPoint2)

import Graphics.Slicer.Math.Skeleton.Concave (makeENode, getOutsideArc)

import Graphics.Slicer.Math.Skeleton.Definitions(Cell(Cell), ENode(ENode), ENodeSet(ENodeSet), INode(INode), INodeSet(INodeSet), Motorcycle(Motorcycle), NodeTree(NodeTree), StraightSkeleton(StraightSkeleton), RemainingContour(RemainingContour), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), makeINode, pPointOf)

import Graphics.Slicer.Math.Skeleton.Face(Face(Face))

class GanjaAble a where
  toGanja :: a -> String -> (String, String)

instance GanjaAble Point2 where
  toGanja (Point2 (x,y)) varname = (
    "  var " <> varname <> " = point(" <> showFullPrecision x <> "," <> showFullPrecision y <> ");\n",
    "    " <> varname <> ", " <> show varname <> ",\n")
    where
      -- because ganja's website does not handle scientific notation.
      showFullPrecision v = showFFloat Nothing v ""

instance GanjaAble LineSeg where
  toGanja l1@(LineSeg p1 _) varname = (
       p1var
    <> p2var,
       "    0x882288,\n"
    <> "    [" <> varname <> "a," <> varname <> "b],\n"
    <> "    0x00AA88,\n"
    <> p1ref
    <> p2ref)
    where
      (p1var, p1ref) = toGanja p1 (varname <> "a")
      (p2var, p2ref) = toGanja (endPoint l1) (varname <> "b")

instance GanjaAble PPoint2 where
  toGanja (PPoint2 (GVec vals)) varname = (
    "  var " <> varname <> " = "
      <> showFullPrecision (valOf 0 (getVals [GEPlus 1, GEPlus 2] vals)) <> "e12"
      <> (if e02 >= 0 then "+" <> showFullPrecision e02 else showFullPrecision e02)
      <> "e02"
      <> (if e01 >= 0 then "+" <> showFullPrecision e01 else showFullPrecision e01)
      <> "e01;\n"
    ,
    "    " <> varname <> ", " <> show varname <> ",\n")
    where
      e02 = valOf 0 (getVals [GEZero 1, GEPlus 2] vals)
      e01 = valOf 0 (getVals [GEZero 1, GEPlus 1] vals)
      -- because ganja's website does not handle scientific notation.
      showFullPrecision v = showFFloat Nothing v ""

instance GanjaAble PLine2 where
  toGanja (PLine2 (GVec vals)) varname = (
    "  var " <> varname <> " = "
      <> showFullPrecision (valOf 0 (getVals [GEPlus 1] vals)) <> "e1"
      <> (if e2 >= 0 then "+" <> showFullPrecision e2 else showFullPrecision e2)
      <> "e2"
      <> (if e0 >= 0 then "+" <> showFullPrecision e0 else showFullPrecision e0)
      <> "e0;\n"
    ,
    "    " <> varname <> ", " <> show varname <> ",\n")
    where
      e2 = valOf 0 (getVals [GEPlus 2] vals)
      e0 = valOf 0 (getVals [GEZero 1] vals)
      -- because ganja's website does not handle scientific notation.
      showFullPrecision v = showFFloat Nothing v ""

instance GanjaAble Contour where
  toGanja contour varname = (invars, inrefs)
    where
      contourPoints = pointsOfContour contour
      (invars, inrefs) = (concat $ fst <$> res, concat (snd <$> res) <> "    0x882288,\n" <> linePairs)
        where
          linePairs    = concat $ mapWithFollower (\(_,a) (_,b) -> "    [" <> varname <> a <> "," <> varname <> b <> "],\n") pairs
          res          = (\(a,b) -> toGanja a (varname <> b)) <$> pairs
          pairs        = zip contourPoints allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]

instance GanjaAble ENode where
  toGanja (ENode (l1, l2) outPLine) varname = (
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

instance GanjaAble Motorcycle where
  toGanja (Motorcycle (l1, l2) outPLine) varname = (
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
      (invars, inrefs) = (concat $ fst <$> res, concat $ snd <$> res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip allSides allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allSegs      = concat $ listFromSlist . fst <$> segsDivides
          allDivides   = catMaybes $ listFromSlist $ snd <$> segsDivides
          allSides     = (toGanja <$> allSegs) <> (toGanja <$> allDivides)
          listFromSlist (Slist a _) = a

instance GanjaAble CellDivide where
  toGanja (CellDivide (DividingMotorcycles firstMotorcycle (Slist moreMotorcycles _)) _) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concat $ fst <$> res, concat $ snd <$> res)
        where
          res            = (\(a,b) -> toGanja a (varname <> b)) <$> zip allMotorcycles allStrings
          allStrings     = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allMotorcycles =   firstMotorcycle:moreMotorcycles

instance GanjaAble RemainingContour where
  toGanja (RemainingContour (Slist segsDivides _)) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concat $ fst <$> res, concat $ snd <$> res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip allSides allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allSegs      = concat $ listFromSlist . fst <$> segsDivides
          allDivides   = concat $ snd <$> segsDivides
          allSides     = (toGanja <$> allSegs) <> (toGanja <$> allDivides)
          listFromSlist (Slist a _) = a

instance GanjaAble INode where
  toGanja (INode firstPLine secondPLine (Slist rawMorePLines _) outPLine) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concat $ fst <$> res, concat $ snd <$> res)
        where
          res          = (\(a,b) -> toGanja a (varname <> b)) <$> zip allPLines allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allPLines    =   firstPLine:secondPLine:rawMorePLines <> maybeToList outPLine

instance GanjaAble StraightSkeleton where
  toGanja (StraightSkeleton (Slist [[nodetree]] _) _) = toGanja nodetree
  toGanja a = error $ "no a, only b: " <> show a <> "\n"

instance GanjaAble NodeTree where
  toGanja (NodeTree (ENodeSet eNodeSides) iNodeSet) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concat $ fst <$> res, concat $ snd <$> res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip (allEdges <> allINodes) allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allEdges     = toGanja <$> (firstLine <> remainingLines)
          allINodes    = toGanja <$> iNodesOf iNodeSet
          firstLine    = case eNodeSides of
                           (Slist [] _) -> []
                           (Slist [(firstNode,Slist [] _)] _) -> [inLine firstNode]
                           (Slist [(firstNode,otherNodes)] _) -> if inLine firstNode == outLine (last otherNodes)
                                                                 then []
                                                                 else [inLine firstNode]
                           (Slist _ _) -> error "too many sides."
            where
              inLine (ENode (a,_) _) = a
          remainingLines
            | len eNodeSides == 0 = []
            | otherwise = outLine <$> eNodesOf eNodeSides
            where
              eNodesOf (Slist [] _) = error "no enodes?"
              eNodesOf (Slist [(first,Slist more _)] _) = first : more
              eNodesOf (Slist _ _) = error "too many sides?"
          outLine (ENode (_,a) _)  = a
          iNodesOf :: INodeSet -> [INode]
          iNodesOf (INodeSet (Slist inodes _)) = concat inodes

instance GanjaAble Face where
  toGanja (Face edge firstArc (Slist arcs _) lastArc) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concat $ fst <$> res, concat $ snd <$> res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip (toGanja edge : allPLines) allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allPLines    = toGanja <$> ([firstArc] <> arcs <> [lastArc])

instance GanjaAble (Slist Face) where
  toGanja (Slist faces _) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concat $ fst <$> res, concat $ snd <$> res)
        where
          allArcs      = concat $ (\(Face _ firstArc (Slist arcs _) lastArc) -> [firstArc] <> arcs <> [lastArc]) <$> faces
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
    (vars, refs) =  (concat $ fst <$> res, concat $ snd <$> res)
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
  deriving (Show, Ord, Eq)

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

instance Fractional (Radian a) where
  (/) (Radian r1) (Radian r2) = Radian $ r1 / r2
  fromRational a = Radian $ fromRational a

randomTriangle :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Contour
randomTriangle centerX centerY rawRadians rawDists = randomStarPoly centerX centerY $ makePairs dists radians
  where
    radians :: [Radian ℝ]
    radians = coerce rawRadians
    dists :: [Positive ℝ]
    dists = coerce rawDists

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

randomRectangle :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Contour
randomRectangle centerX centerY firstTilt secondTilt distanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
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
        | v < (Radian pi) = v + Radian pi
        | otherwise       = v - Radian pi 
      distances = replicate 4 distanceToCorner

-- | generate a random polygon.
-- Idea stolen from: https://stackoverflow.com/questions/8997099/algorithm-to-generate-random-2d-polygon
-- note: the centerPoint is assumed to be inside of the contour.
randomStarPoly :: ℝ -> ℝ -> [(Positive ℝ,Radian ℝ)] -> Contour
randomStarPoly centerX centerY radianDistPairs = maybeFlipContour $ makePointContour $ points
  where
    points = pToEPoint2 <$> pointsAroundCenter
    pointsAroundCenter = (\(distanceFromPoint, angle) -> translateRotatePPoint2 centerPPoint (coerce distanceFromPoint) (coerce angle)) <$> radianDistPairs
    centerPPoint = eToPPoint2 $ Point2 (centerX, centerY)

randomENode :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> ENode
randomENode x y d1 rawR1 d2 rawR2 = makeENode l1 l2
  where
    r1 = rawR1 / 2
    r2 = r1 + (rawR2 / 2)
    intersectionPoint = Point2 (x,y)
    pp1 = translateRotatePPoint2 intersectionPPoint (coerce d1) (coerce r1)
    pp2 = translateRotatePPoint2 intersectionPPoint (coerce d2) (coerce r2)
    p1 = pToEPoint2 pp1
    p2 = pToEPoint2 pp2
    intersectionPPoint = eToPPoint2 intersectionPoint
    l1 = handleLineSegError $ lineSegFromEndpoints p1 intersectionPoint
    l2 = handleLineSegError $ lineSegFromEndpoints intersectionPoint p2

randomINode :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> Bool -> Bool -> INode
randomINode x y d1 rawR1 d2 rawR2 flipIn1 flipIn2 = makeINode [maybeFlippedpl1,maybeFlippedpl2] (Just bisector1)
  where
    r1 = rawR1 / 2
    r2 = r1 + (rawR2 / 2)
    (l1, l2) = (\(ENode segs _) -> segs) eNode
    pl1 = normalizePLine2 $ eToPLine2 l1
    pl2 = normalizePLine2 $ flipPLine2 $ eToPLine2 l2
    intersectionPPoint = pPointOf eNode
    eNode = randomENode x y d1 rawR1 d2 rawR2
    pp1 = translateRotatePPoint2 intersectionPPoint (coerce d1) (coerce r1)
    pp2 = translateRotatePPoint2 intersectionPPoint (coerce d2) (coerce r2)
    maybeFlippedpl1 = if flipIn1 then flipPLine2 pl1 else pl1
    maybeFlippedpl2 = if flipIn2 then flipPLine2 pl2 else pl2
    bisector1 = normalizePLine2 $ getOutsideArc pp1 maybeFlippedpl1 pp2 maybeFlippedpl2

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
onlyOne eNodes = case eNodes of
                   [] -> error "none"
                   [a] -> a
                   (_:_) -> error "too many"
