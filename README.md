# whack

 a slicer and G-code generator for RepRap firmwares, in Haskell

### to compile
Run `ghc slicer` from the directory that contains `slicer.hs`.

### to run 
Run `./slicer filename` with options:
* `-i infill` Infill percentage as an integer (default is 20)
* `-o outfile` Name of file to output to (default is `out.gcode`)
* `p perimeter` Number of perimeter layers (default is 2)
* `-s` Print with support
* `-t thickness` Layer thickness in mm (default is 0.2)

## FAQ 
#### *Wait but...why?*

Glad you asked! This was written as a project for CMSC 22010 (Digital Fabrication) at the University of Chicago, taught by professor Rick Stevens.

#### *No but like...why Haskell?*

Haskell is beautiful.

#### *What sick, twisted masochists did this?*

[Catherine Moresco](http://catmores.co) and [Noah Halford](http://noahhalford.com/) are two fourth-year undergraduates at the University of Chicago.

#### *Can I contribute?*
Go ahead. At your own risk.
