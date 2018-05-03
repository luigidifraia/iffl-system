@ECHO off
rem
rem Batch file for PUCRUNCH IFFL example
rem
DEL music.puc picture.puc iffldata_pucr iffl_pucr.prg iffl_pucr.d64
pucrunch -c0 music.prg music.puc
pucrunch -c0 picture.prg picture.puc
makeiffl iffldata_pucr
addiffl iffldata_pucr music.puc
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
addiffl iffldata_pucr picture.puc
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
dasm tst_pucr.s -oiffl_pucr.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
makedisk iffl_pucr.d64 iffl_pucr.seq "IFFL EXAMPLE      IF 2A" 10
DEL music.puc picture.puc iffldata_pucr iffl_pucr.prg