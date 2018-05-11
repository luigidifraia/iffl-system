# IFFL System
Lasse Öörni's Integrated File Flexible Loader for Commodore 1541/1541-II/157x/1581 and CMD FD2000 drives.

Originally published on Covert Bitops' [C64 page](https://cadaver.github.io/rants/iffl.html).

## Notes
- Assemble source files with `dasm`. Version 2.12.04 from Covert Bitops' [Tools page](https://cadaver.github.io/tools.html) has been successfully tested.
- The `makedisk` tool is available from Covert Bitops' [Tools page](https://cadaver.github.io/tools.html).
- The `c1541` tool is available from VICE's [Web page](http://vice-emu.sourceforge.net/index.html#download).
- If you plan to use ByteBoozer2 crunching, get the extended version of the cruncher from [here](https://github.com/luigidifraia/ByteBoozer2).
- To use the provided `Makefile` you need a recent version of `make`. If you don't know where to get one you're probably using Windows, so try [this one](http://gnuwin32.sourceforge.net/packages/make.htm).
- Under Windows you might want to run `win32/ifflvar.bat` to setup your build environment, then either run `make` or any of the provided batch files.

## Fixes
- The `addiffl` tool does not overwrite existing records whose length's LSB happens to be 0.

## Extensions
- Integrated Lasse's 1-bit transfer protocol, for using sprites while loading (`TWOBIT_PROTOCOL = 0` as per Covert Bitops' [Loadersystem](https://cadaver.github.io/tools.html)).
- Integrated support for loading under the I/O area and Kernal ROM (`LOAD_UNDER_IO = 1` as per Covert Bitops' [Loadersystem](https://cadaver.github.io/tools.html)).
- Added optional buffer-less block receive support (`RECEIVE_BUFFER = 0` saves 254 bytes on the C=64, with some speed trade-off).
- Added support for ByteBoozer 2.0 (which does **not** require a depack buffer on the C=64), Pucrunch and Exomizer files. Furthermore, linked packed files are all loaded with a single loader call.
- Integrated Lasse's drive detection (Dreamload) and added support for Commodore 1581 and CMD FD2000 drives with Lasse's help and advice.

## Acknowledgments
This work would not be possible without Lasse's initial publication of his IFFL system and without frequent emails with him and Flavio.

## To do
- Mark sections of the code that are critical to keep in the same page in RAM.
- Add support for CMD FD4000, and SD2IEC.
