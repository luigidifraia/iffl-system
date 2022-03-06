# Makefile for IFFL examples by Luigi Di Fraia 3/2022
#
# Note: the extended version of the ByteBoozer2 cruncher is used here
# https://github.com/luigidifraia/ByteBoozer2/tree/master/b2

all: iffl_bb2.d81

clean:
	rm -f *.d64 *.d81 *.b2 iffl_*.prg iffldata_*

iffl_bb2.d81: iffl_bb2.d64 iffl_bb2_1581.seq
	c1541 < iffl_bb2_1581.seq

iffl_bb2.d64: iffl_bb2.prg iffldata_bb2 iffl_bb2.seq
	makedisk $@ iffl_bb2.seq "IFFL EXAMPLE      IF 2A" 10

iffl_bb2.prg: tst_bb2.s cfg_bb2.s ../iffl_init.s ../iffl_loader.s
	dasm $< -o$@

iffldata_bb2: music.prg.b2 picture.prg.b2
	makeiffl $@
	addiffl $@ music.prg.b2
	addiffl $@ picture.prg.b2

music.prg.b2: music.prg
	b2 -b $<

picture.prg.b2: picture.prg
	b2 -b $<
