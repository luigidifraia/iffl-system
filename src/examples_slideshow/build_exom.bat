@ECHO off
rem
rem Batch file for EXOMIZER IFFL example slideshow
rem
DEL music.exo picture1.exo picture2.exo iffldata_exom iffl_exom.prg iffl_exom.d64 iffl_exom.d81
exomizer level music.prg -o music.exo
exomizer level picture1.prg -o picture1.exo
exomizer level picture2.prg -o picture2.exo
makeiffl iffldata_exom
addiffl iffldata_exom music.exo
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
addiffl iffldata_exom picture1.exo
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
addiffl iffldata_exom picture2.exo
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
dasm tst_exom.s -oiffl_exom.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
makedisk iffl_exom.d64 iffl_exom.seq "IFFL EXAMPLE      IF 2A" 10
c1541 < iffl_exom_1581.seq
DEL music.exo picture1.exo picture2.exo iffldata_exom iffl_exom.prg