@ECHO off
rem
rem Batch file for EXOMIZER IFFL example
rem
DEL music.exo picture.exo iffldata_exom iffl_exom.prg iffl_exom.d64
exomizer level music.prg -o music.exo
exomizer level picture.prg -o picture.exo
makeiffl iffldata_exom
addiffl iffldata_exom music.exo
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
addiffl iffldata_exom picture.exo
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
dasm tst_exom.s -oiffl_exom.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
makedisk iffl_exom.d64 iffl_exom.seq "IFFL EXAMPLE      IF 2A" 10
DEL music.exo picture.exo iffldata_exom iffl_exom.prg