@ECHO off
rem
rem Batch file for PUCHRUNCH IFFL example slideshow
rem
DEL music.puc picture1.puc picture2.puc iffldata_pucr iffl_pucr.prg iffl_pucr.d64 iffl_pucr.d81
pucrunch -c0 music.prg music.puc
pucrunch -c0 picture1.prg picture1.puc
pucrunch -c0 picture2.prg picture2.puc
makeiffl iffldata_pucr
addiffl iffldata_pucr music.puc
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
addiffl iffldata_pucr picture1.puc
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
addiffl iffldata_pucr picture2.puc
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
dasm tst_pucr.s -oiffl_pucr.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
makedisk iffl_pucr.d64 iffl_pucr.seq "IFFL EXAMPLE      IF 2A" 10
c1541 < iffl_pucr_1581.seq
DEL music.puc picture1.puc picture2.puc iffldata_pucr iffl_pucr.prg