# IFFL System Examples
Examples in this folder all implement the same scenario: load a tune, a multicolor bitmap picture, and loop playing the former while the latter is displayed.

"Linked" variants link together the packed tune and picture into a single file, hence a single IFFL file entry. This means that the tune playback dos not start immediately after its data is completed as linked files are loaded with a single loader call. Thus, unless you have to call a routine in between files, you might want to link level data in a single file and let the loader deal with loading all segments.

## Notes
- The two-bit protocol is used in all examples.
- Picture data is structured into bitmap data, padding, video RAM data, padding, color RAM data. This is so that the former two end up in their final RAM locations already.
- As the unpacked file loader does not support linked files, there isn't a "linked" variant of the unpacked data example.