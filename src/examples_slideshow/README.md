# IFFL System Examples with Slideshow
Examples in this folder all implement the same scenario: load a tune, start playback, load a multicolor bitmap picture, wait for spacebar to be pressed, and load another picture.

## Notes
- The two-bit protocol is used in all examples.
- Picture data is structured into bitmap data, padding, video RAM data, padding, color RAM data. This is so that the former two end up in their final RAM locations already.
