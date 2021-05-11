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

{- No reason to get excited. just some dumping facilities.
 -}


module Graphics.Slicer.Math.Ganja (toGanja, dumpGanja) where

import Prelude (String, (<>), show)

import Graphics.Slicer.Math.Definitions(Point2(Point2))
import Graphics.Slicer.Math.Line(LineSeg(LineSeg), endpoint)

class GanjaAble a where
  toGanja :: a -> String -> (String, String)

instance GanjaAble Point2 where
  toGanja (Point2 (x,y)) varname = (
    "  var p" <> varname <> " = point(" <> show x <> "," <> show y <> ")\n",
          "p" <> varname <> ", " <> show ("p" <> varname) <> ",\n")

instance GanjaAble LineSeg where
  toGanja l1@(LineSeg (Point2 (x1,y1)) _) varname = (
       "  var " <> varname <> "p1 = point(" <> show x1 <> "," <> show y1 <> ")\n"
    <> "  var " <> varname <> "p2 = point(" <> show x2 <> "," <> show y2 <> ")\n",
       "ls1" <> varname <> ", " <> show ("lp" <> varname) <> ",\n"
    <> "ls2" <> varname <> ", " <> show ("lp" <> varname) <> ",\n")
    where
      (Point2 (x2,y2)) = endpoint l1


dumpGanja :: (GanjaAble a) => a -> String
dumpGanja target = ganjaHeader <> vars <> ganjaFooterStart <> refs <> ganjaFooterEnd
  where
    (vars, refs) = toGanja target "a"
    ganjaHeader = "Algebra(2,0,1,()=>{\n  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;\n  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);\n"
    ganjaFooterStart = "  document.body.appendChild(this.graph([\n"
    ganjaFooterEnd = "],{\n"
                     <> "    grid: true,\n"
                     <> "    labels: true,\n"
                     <> "    lineWidth: 3,\n"
                     <> "    pointRadius: 1,\n"
                     <> "    fontSize: 1,\n"
                     <> "    scale: 1,\n"
                     <> "}));\n"
                     <> "});\n"
  
