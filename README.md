# HSlice

A slicer and G-code generator for RepRap firmwares, in Haskell.

### to compile
Run `make` from the root directory of this repository.

### to run 
Run `./slicer filename` with options:
* `-i infill` Infill percentage as an integer (default is 20)
* `-o outfile` Name of file to output to (default is `out.gcode`)
* `-p perimeter` Number of perimeter layers (default is 2)
* `-s` Print with support
* `-t thickness` Layer thickness in mm (default is 0.2)

## FAQ 
#### *Wait but...why?*

Haskell all of the things.

#### *No but like...why Haskell?*

Functional programming is beautiful.

#### *What sick, twisted masochists did this?*

[Catherine Moresco](http://catmores.co) and [Noah Halford](http://noahhalford.com/) are two fourth-year undergraduates at the University of Chicago who originally wrote this.

[Julia Longtin](http://implicitcad.org) is the developer that picked it up, and is cleaning it off, to run her printers.

#### *Can I contribute?*

Pull requests welcome. Drop by #implicitcad on freenode IRC, or sign up for the implicitcad mailing list, OR email the author at julia.longtin@gmail.com!
