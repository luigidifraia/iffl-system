# Makefile for IFFL example with sprite by Luigi Di Fraia 4/2018
#
# Note: the extended version of the ByteBoozer2 cruncher is used here
# https://github.com/luigidifraia/ByteBoozer2/tree/master/b2

all: iffl_bb2.d64

clean:
	rm -f *.d64 *.b2 iffl_*.prg iffldata_* picture_data_bb2_linked

iffl_bb2.d64: iffl_bb2.prg iffldata_bb2 iffl_bb2.seq
	makedisk $@ iffl_bb2.seq "IFFL EXAMPLE      IF 2A" 10

iffl_bb2.prg: tst_bb2.s cfg_bb2.s ../iffl_init.s ../iffl_loader.s
	dasm $< -o$@

iffldata_bb2: sprites.prg.b2 music.prg.b2 picture_data_bb2_linked
	makeiffl $@
	addiffl $@ sprites.prg.b2
	addiffl $@ music.prg.b2
	addiffl $@ picture_data_bb2_linked

sprites.prg.b2: sprites.prg
	b2 -b $<

music.prg.b2: music.prg
	b2 -b $<

picture_data_bb2_linked: bitmap.prg.b2 video_ram.prg.b2 color_ram.prg.b2
ifeq ($(findstring Windows,$(OS)),Windows)
	copy /b bitmap.prg.b2 + video_ram.prg.b2 + color_ram.prg.b2 $@
else
	cat $^ > $@
endif

bitmap.prg.b2: bitmap.prg
	b2 -b $<

video_ram.prg.b2: video_ram.prg
	b2 -b $<

color_ram.prg.b2: color_ram.prg
	b2 -b $<
