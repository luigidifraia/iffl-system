# IFFL System
Lasse Öörni's Integrated File Flexible Loader for Commodore 1541/1541-II/157x drives.

Originally published on Covert Bitops' [C64 page](https://cadaver.github.io/rants/iffl.html).

## Notes
- To use the provided `Makefile` you need a recent version of `make`. If you don't know where to get one, try [this one](http://gnuwin32.sourceforge.net/packages/make.htm).
- Assemble with `dasm`: v2.12.04 from Covert Bitops' [Tools page](https://cadaver.github.io/tools.html) has been tested.
- The `makedisk` tool is available from Covert Bitops' [Tools page](https://cadaver.github.io/tools.html).
- Use the extended version of the ByteBoozer2 cruncher from [here](https://github.com/luigidifraia/ByteBoozer2).
- Under Windows you might want to run `win32/ifflvar.bat` to setup your build environment, then either run `make` or any of the provided batch files.

## Fixes
- The `addiffl` tool does not overwrite existing records whose length's LSB happens to be 0.

## Extensions
- Added 1-bit transfer support for using sprites while loading (`TWOBIT_PROTOCOL = 0` as per Covert Bitops' [Loadersystem](https://cadaver.github.io/tools.html)).
- Added support for loading under the I/O area and Kernal ROM (`LOAD_UNDER_IO = 1` as per Covert Bitops' [Loadersystem](https://cadaver.github.io/tools.html)).
- Added optional buffer-less block receive support (`RECEIVE_BUFFER = 0` saves 254 bytes on the C=64, with some speed trade-off).
- Added support for ByteBoozer 2.0 (which does **not** require a depack buffer on the C=64), Pucrunch and Exomizer files. Furthermore, linked files are all loaded with a single loader call.

## In progress
- Support for Commodore 1581 drives is being developed in a separate branch with help and contributions from Lasse.

## To do
- Mark sections of the code that are critical to keep in the same page in RAM.
- Add support for 1581, FD2000/4000, IDE64, and SD2IEC.