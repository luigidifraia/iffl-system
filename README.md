# IFFL System
Lasse Öörni's Integrated File Flexible Loader for Commodore 1541/1541-II/157x/1581 and CMD FD2000 drives.

Originally published on Covert Bitops' [C64 page](https://cadaver.github.io/rants/iffl.html).

## Notes
- Assemble source files with `dasm`. Version 2.12.04 from Covert Bitops' [Tools page](https://cadaver.github.io/tools.html) has been successfully tested.
- The `makedisk` tool is available from Covert Bitops' [Tools page](https://cadaver.github.io/tools.html). A Windows executable is provided for user convenience.
- The `c1541` tool is available from VICE's [Web page](http://vice-emu.sourceforge.net/index.html#download). A Windows executable is provided for user convenience.
- If you plan to use ByteBoozer2 crunching, get the extended version of the cruncher from [here](https://github.com/luigidifraia/ByteBoozer2). A Windows executable is provided for user convenience.
- To use the provided `Makefile` you need GNU `make`. A Windows executable is provided for user convenience.
- Under Windows you might want to run `win32/ifflvar.bat` to setup your build environment, then either run `make` or any of the provided batch files.

## Fixes
- The `addiffl` tool does not overwrite existing records whose length's LSB happens to be 0.

## Extensions
- Integrated Lasse's 1-bit transfer protocol from his [diskdrive loader rant](https://cadaver.github.io/rants/irqload.html), for using sprites while loading (`TWOBIT_PROTOCOL = 0` as per Covert Bitops' [Loadersystem](https://cadaver.github.io/tools.html)).
- Integrated support for loading under the I/O area and Kernal ROM (`LOAD_UNDER_IO = 1` as per Covert Bitops' [Loadersystem](https://cadaver.github.io/tools.html)).
- Added optional buffer-less block receive support (`RECEIVE_BUFFER = 0` saves 254 bytes on the C=64, with some speed trade-off).
- Added support for ByteBoozer 2.0 (which does **not** require a depack buffer on the C=64), Pucrunch and Exomizer files. Furthermore, linked packed files are all loaded with a single loader call.
- Integrated Lasse's drive detection (Dreamload) and added support for Commodore 1581 and CMD FD2000 drives with Lasse's help and advice.

## Warning
If you need to change the VIC bank in between load requests, you might want to avoid setting $dd00 and set $dd02 instead.
Example for setting $dd02, from the documentation of [DreamLoad](https://csdb.dk/release/?id=6093):
```
    lda #%......xy
    sta $dd00
```
becomes:
```
    lda #0                  ;only once in your init routine
    sta $dd00               ;don't do this while loading

    lda #($3f EOR %xy)      ;substitute for setting the VIC
    sta $dd02               ;bank to %xy
```

## To do
- Mark sections of the code that are critical to keep in the same page in RAM.
- Add support for CMD FD4000, and SD2IEC.

## Productions that use this loader
This loader was used to put together my [IFFL version of Barbarian 2](https://www.luigidifraia.com/retroware/#IFFL_version_of_Barbarian_II_for_the_1541_disk_drive).

## Acknowledgments
It wouldn't be possible for me to contribute this project without Lasse's initial publication of his IFFL System and Loadersystem, and without a number of email exchanges I had with him and [Flavioweb](https://csdb.dk/scener/?id=23136).
