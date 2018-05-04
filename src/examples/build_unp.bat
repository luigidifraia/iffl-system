@ECHO off
rem
rem Batch file for UNPACKED IFFL example
rem
DEL iffldata_unp iffl_unp.prg iffl_unp.d64
makeiffl iffldata_unp
addiffl iffldata_unp music.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
addiffl iffldata_unp picture.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
dasm tst_unp.s -oiffl_unp.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
makedisk iffl_unp.d64 iffl_unp.seq "IFFL EXAMPLE      IF 2A" 10
c1541 < iffl_unp_1581.seq
DEL iffldata_unp iffl_unp.prg