# IFFL System Examples with Sprite
The examples in this folder implement a familiar scenario:
- load data for a few sprite images and a tune, 
- display a single sprite while tune playback  and the loading of a further multicolor bitmap picture are in progress.

## Notes
- The one-bit protocol is used in these examples, in order to allow a sprite on screen.

## Known issues
- Attempting to load a non-existing file can occasionally result in a deadlock for the examples presented here. Such an attempt is explicitly made in order to keep track of this issue.