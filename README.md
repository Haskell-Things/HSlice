# HSlice

[![IRC](https://img.shields.io/badge/irc.freenode.net-%23ImplicitCAD-blue.svg)](https://freenode.net/)

A slicer / G-code generator for RepRap firmwares written in Haskell, using Projective Geometric Algebra.

### To compile:
Run `make` from the root directory of this repository.

### To run:
Run `./extcuraengine` with options:

* `-l infile` Name of the ASCII STL file to read (default is `in.stl`)
* `-o outfile` Name of file to output to (default is `out.gcode`)

All other settings are supplied in KEY=VALUE form, using the keys that CURA defines in it's JSON file. for instance:
* -s machine_width=200 -s machine_depth=200 -s machine_height=100 -s machine_nozzle_size=0.35 -s material_diameter=2.85 -s outer_inset_first=False -s infill_sparse_density=100

## FAQ 
#### *Wait but...why?*

Haskell all of the things.

#### *No but like...why Haskell?*

Functional programming is beautiful, and gets us closer to perfection.

#### *What sick, twisted masochists did this?*

[Catherine Moresco](http://catmores.co) and [Noah Halford](http://noahhalford.com/) are two fourth-year undergraduates at the University of Chicago who originally wrote this.

[Julia Longtin](http://implicitcad.org) is the developer that picked it up, and is cleaning it off, to run her printers.

#### *Can I contribute?*

Pull requests welcome. Drop by #implicitcad on freenode IRC, or sign up for the implicitcad mailing list, OR email the author at julia.longtin@gmail.com!
