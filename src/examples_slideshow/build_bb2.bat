@ECHO off
rem
rem Batch file for BYTEBOOZER IFFL example slideshow
rem
DEL music.prg.b2 picture1.prg.b2 picture2.prg.b2 iffldata_bb2 iffl_bb2.prg iffl_bb2.d64 iffl_bb2.d81
b2 -b music.prg
b2 -b picture1.prg
b2 -b picture2.prg
makeiffl iffldata_bb2
addiffl iffldata_bb2 music.prg.b2
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
addiffl iffldata_bb2 picture1.prg.b2
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
addiffl iffldata_bb2 picture2.prg.b2
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
dasm tst_bb2.s -oiffl_bb2.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
makedisk iffl_bb2.d64 iffl_bb2.seq "IFFL EXAMPLE      IF 2A" 10
c1541 < iffl_bb2_1581.seq
DEL music.prg.b2 picture1.prg.b2 picture2.prg.b2 iffldata_bb2 iffl_bb2.prg