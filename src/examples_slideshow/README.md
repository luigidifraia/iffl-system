# IFFL System Examples with Slideshow
Examples in this folder all implement the same scenario: load a tune, start playback, load a multicolor bitmap picture, wait for spacebar to be pressed, and load another picture.

## Notes
- The two-bit protocol is used in all examples.
- Picture data is structured into bitmap data, padding, video RAM data, padding, color RAM data. This is so that a single file is used for all data, which simplifies the set of files to work with.
