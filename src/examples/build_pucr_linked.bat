@ECHO off
rem
rem Batch file for PUCRUNCH IFFL example with linked files
rem
DEL music.puc picture.puc data_pucr_linked iffldata_pucr_linked iffl_pucr_linked.prg iffl_pucr_linked.d64
pucrunch -c0 music.prg music.puc
pucrunch -c0 picture.prg picture.puc
copy /b music.puc + picture.puc data_pucr_linked
makeiffl iffldata_pucr_linked
addiffl iffldata_pucr_linked data_pucr_linked
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
dasm tst_pucr_linked.s -oiffl_pucr_linked.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
makedisk iffl_pucr_linked.d64 iffl_pucr_linked.seq "IFFL EXAMPLE      IF 2A" 10
c1541 < iffl_pucr_linked_1581.seq
DEL music.puc picture.puc data_pucr_linked iffldata_pucr_linked iffl_pucr_linked.prg