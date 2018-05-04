@ECHO off
rem
rem Batch file for BYTEBOOZER IFFL example with linked files
rem
DEL music.prg.b2 picture.prg.b2 data_bb2_linked iffldata_bb2_linked iffl_bb2_linked.prg iffl_bb2_linked.d64
b2 -b music.prg
b2 -b picture.prg
copy /b music.prg.b2 + picture.prg.b2 data_bb2_linked
makeiffl iffldata_bb2_linked
addiffl iffldata_bb2_linked data_bb2_linked
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
dasm tst_bb2_linked.s -oiffl_bb2_linked.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
makedisk iffl_bb2_linked.d64 iffl_bb2_linked.seq "IFFL EXAMPLE      IF 2A" 10
c1541 < iffl_bb2_linked_1581.seq
DEL music.prg.b2 picture.prg.b2 data_bb2_linked iffldata_bb2_linked iffl_bb2_linked.prg