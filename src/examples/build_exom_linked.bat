@ECHO off
rem
rem Batch file for EXOMIZER IFFL example with linked files
rem
DEL data_exom_linked iffldata_exom_linked iffl_exom_linked.prg iffl_exom_linked.d64
exomizer level music.prg picture.prg -o data_exom_linked
makeiffl iffldata_exom_linked
addiffl iffldata_exom_linked data_exom_linked
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
dasm tst_exom_linked.s -oiffl_exom_linked.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
makedisk iffl_exom_linked.d64 iffl_exom_linked.seq "IFFL EXAMPLE      IF 2A" 10
DEL data_exom_linked iffldata_exom_linked iffl_exom_linked.prg