@ECHO off
rem
rem Batch file for BYTEBOOZER IFFL example
rem
DEL music.prg.b2 picture.prg.b2 iffldata_bb2 iffl_bb2.prg iffl_bb2.d64
b2 -b music.prg
b2 -b picture.prg
makeiffl iffldata_bb2
addiffl iffldata_bb2 music.prg.b2
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
addiffl iffldata_bb2 picture.prg.b2
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
dasm tst_bb2.s -oiffl_bb2.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
makedisk iffl_bb2.d64 iffl_bb2.seq "IFFL EXAMPLE      IF 2A" 10
DEL music.prg.b2 picture.prg.b2 iffldata_bb2 iffl_bb2.prg