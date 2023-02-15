# HSlice

[![IRC](https://img.shields.io/badge/irc.libera.chat-%23ImplicitCAD-blue.svg)](https://libera.chat/)

A slicer / G-code generator for RepRap firmwares written in Haskell, using Projective Geometric Algebra.

### To compile:
Run `make` from the root directory of this repository.

### To run:
Run `./extcuraengine` with options:

* `-l infile` Name of the ASCII STL file to read (default is `in.stl`)
* `-o outfile` Name of file to output to (default is `out.gcode`)

All other settings are supplied in KEY=VALUE form, using the same keys that CURA defines in it's JSON file. For instance:
* -s layer_height=0.1
* -s layer_height_0=0.3
* -s speed_layer_0=30
* -s machine_width=100
* -s machine_depth=100
* -s machine_height=100
* -s machine_nozzle_size=0.4
* -s material_diameter=2.85
* -s outer_inset_first=False
* -s infill_sparse_density=20
* -s wall_line_count=2
* -s infill_line_width=0.4
* -s infill_pattern=lines
* -s speed_infill=60
* -s speed_layer_0=30
* -s speed_travel=120
* -s speed_wall_0=30
* -s speed_wall_x=60
* -s top_bottom_thickness=0.8
* -s top_bottom_pattern_0=concentric

## FAQ 
#### *Wait but...why?*

Haskell all of the things.

#### *No but like...why Haskell?*

Functional programming is beautiful, and gets us closer to perfection.

#### *What's Projective Geometric Algebra?*

Projective Geometric algebra is a system that we use to represent geometry. Most programmers will be familiar with Linear Algebra, this is just a way of representing things that, once you understand it, is less confusing, less error prone, and represents objects (lines, points) with more values than linear algebra, which we hope translates into less error from using floating point math.

We're using a clifford algebra of (2,0,1)*, to represent 2D spaces (EG: layers) in this program. For more information, you should see the SIGGRAPH 2019 video "Geometric Algebra", or many of Charles Gunn's papers on the subject.

#### *What sick, twisted masochists did this?*

[Catherine Moresco](http://catmores.co) and [Noah Halford](http://noahhalford.com/) are two fourth-year undergraduates at the University of Chicago who originally wrote this.

[Julia Longtin](https://implicitcad.org) is the developer that picked it up, and is cleaning it off, to run her printers.

#### *Can I contribute?*

Pull requests welcome. Currency is welcome. Cheering is welcome! Drop by #implicitcad on libera IRC, or sign up for the implicitcad mailing list, OR email the author at julia.longtin@gmail.com!
