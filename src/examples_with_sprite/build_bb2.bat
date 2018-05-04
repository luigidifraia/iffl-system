@ECHO off
rem
rem Batch file for BYTEBOOZER IFFL example with sprites
rem
DEL sprites.prg.b2 music.prg.b2 bitmap.prg.b2 video_ram.prg.b2 color_ram.prg.b2 sprites_and_music_data_bb2_linked picture_data_bb2_linked iffldata_bb2 iffl_bb2.prg iffl_bb2.d64
b2 -b sprites.prg
b2 -b music.prg
copy /b sprites.prg.b2 + music.prg.b2 sprites_and_music_data_bb2_linked
b2 -b bitmap.prg
b2 -b video_ram.prg
b2 -b color_ram.prg
copy /b bitmap.prg.b2 + video_ram.prg.b2 + color_ram.prg.b2 picture_data_bb2_linked
makeiffl iffldata_bb2
addiffl iffldata_bb2 sprites_and_music_data_bb2_linked
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
addiffl iffldata_bb2 picture_data_bb2_linked
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
dasm tst_bb2.s -oiffl_bb2.prg
IF %ERRORLEVEL% GEQ 1 EXIT /B 1
makedisk iffl_bb2.d64 iffl_bb2.seq "IFFL EXAMPLE      IF 2A" 10
c1541 < iffl_bb2_1581.seq
DEL sprites.prg.b2 music.prg.b2 bitmap.prg.b2 video_ram.prg.b2 color_ram.prg.b2 sprites_and_music_data_bb2_linked picture_data_bb2_linked iffldata_bb2 iffl_bb2.prg