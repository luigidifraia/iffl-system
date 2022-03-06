# Makefile for IFFL example slideshow by Luigi Di Fraia 4/2018
#
# Note: the extended version of the ByteBoozer2 cruncher is used here
# https://github.com/luigidifraia/ByteBoozer2/tree/master/b2

all: iffl_bb2.d81 iffl_exom.d81 iffl_pucr.d81

clean:
	rm -f *.d64 *.d81 *.b2 *.exo iffl_*.prg iffldata_*

iffl_bb2.d81: iffl_bb2.d64 iffl_bb2_1581.seq
	c1541 < iffl_bb2_1581.seq

iffl_exom.d81: iffl_exom.d64 iffl_exom_1581.seq
	c1541 < iffl_exom_1581.seq

iffl_pucr.d81: iffl_pucr.d64 iffl_pucr_1581.seq
	c1541 < iffl_pucr_1581.seq

iffl_bb2.d64: iffl_bb2.prg iffldata_bb2 iffl_bb2.seq
	makedisk $@ iffl_bb2.seq "IFFL EXAMPLE      IF 2A" 10

iffl_exom.d64: iffl_exom.prg iffldata_exom iffl_exom.seq
	makedisk $@ iffl_exom.seq "IFFL EXAMPLE      IF 2A" 10

iffl_pucr.d64: iffl_pucr.prg iffldata_pucr iffl_pucr.seq
	makedisk $@ iffl_pucr.seq "IFFL EXAMPLE      IF 2A" 10

iffl_bb2.prg: tst_bb2.s cfg_bb2.s ../iffl_init.s ../iffl_loader.s
	dasm $< -o$@

iffl_exom.prg: tst_exom.s cfg_exom.s ../iffl_init.s ../iffl_loader.s
	dasm $< -o$@

iffl_pucr.prg: tst_pucr.s cfg_pucr.s ../iffl_init.s ../iffl_loader.s
	dasm $< -o$@

iffldata_bb2: music.prg.b2 picture1.prg.b2 picture2.prg.b2
	makeiffl $@
	addiffl $@ music.prg.b2
	addiffl $@ picture1.prg.b2
	addiffl $@ picture2.prg.b2

iffldata_exom: music.exo picture1.exo picture2.exo
	makeiffl $@
	addiffl $@ music.exo
	addiffl $@ picture1.exo
	addiffl $@ picture2.exo

iffldata_pucr: music.puc picture1.puc picture2.puc
	makeiffl $@
	addiffl $@ music.puc
	addiffl $@ picture1.puc
	addiffl $@ picture2.puc

music.prg.b2: music.prg
	b2 -b $<

picture1.prg.b2: picture1.prg
	b2 -b $<

picture2.prg.b2: picture2.prg
	b2 -b $<

music.exo: music.prg
	exomizer level $< -o $@

picture1.exo: picture1.prg
	exomizer level $< -o $@

picture2.exo: picture2.prg
	exomizer level $< -o $@

music.puc: music.prg
	pucrunch -c0 $< $@

picture1.puc: picture1.prg
	pucrunch -c0 $< $@

picture2.puc: picture2.prg
	pucrunch -c0 $< $@
