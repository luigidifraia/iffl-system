# IFFL System Example
The example in this folder implements the following scenario:
- init the first part of the IFFL system, 
- relocate the IFFL loader to pages 2 and 3 in RAM, 
- init the second part of the IFFL system, 
- load a tune, 
- start tune playback using the vector at $fffe/$ffff in RAM, 
- load a multicolor bitmap picture, and 
- loop playing back the former while the latter is displayed.

We have to init the IFFL system in two stages as the IFFL loader resides in pages 2 and 3 in RAM. This is an issue because Kernal routines used in the first stage expect the standard meaning of location $02a1 and vector $0318/$0319.

## Notes
- The two-bit protocol is used in this example.
- Picture data is structured into bitmap data, padding, video RAM data, padding, color RAM data. This is so that a single file is used for all data, which simplifies the set of files to work with.
