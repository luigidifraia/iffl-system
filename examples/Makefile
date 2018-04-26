# Makefile for IFFL examples by Luigi Di Fraia 4/2018
#
# Note: the extended version of the ByteBoozer2 cruncher is used here
# https://github.com/luigidifraia/ByteBoozer2/tree/master/b2

all: iffl_bb2.d64 iffl_bb2_linked.d64 iffl_exom.d64 iffl_exom_linked.d64 iffl_pucr.d64 iffl_pucr_linked.d64 iffl_unp.d64

clean:
	rm -f *.d64 *.b2 *.exo *.puc data_*_linked iffl_*.prg iffldata_*

iffl_bb2.d64: iffl_bb2.prg iffldata_bb2
	c1541 < iffl_bb2.seq

iffl_bb2_linked.d64: iffl_bb2_linked.prg iffldata_bb2_linked
	c1541 < iffl_bb2_linked.seq

iffl_exom.d64: iffl_exom.prg iffldata_exom
	c1541 < iffl_exom.seq

iffl_exom_linked.d64: iffl_exom_linked.prg iffldata_exom_linked
	c1541 < iffl_exom_linked.seq

iffl_pucr.d64: iffl_pucr.prg iffldata_pucr
	c1541 < iffl_pucr.seq

iffl_pucr_linked.d64: iffl_pucr_linked.prg iffldata_pucr_linked
	c1541 < iffl_pucr_linked.seq

iffl_unp.d64: iffl_unp.prg iffldata_unp
	c1541 < iffl_unp.seq

iffl_bb2.prg: tst_bb2.s cfg_bb2.s ../iffl.s
	dasm $< -o$@

iffl_bb2_linked.prg: tst_bb2_linked.s cfg_bb2.s ../iffl.s
	dasm $< -o$@

iffl_exom.prg: tst_exom.s cfg_exom.s ../iffl.s
	dasm $< -o$@

iffl_exom_linked.prg: tst_exom_linked.s cfg_exom.s ../iffl.s
	dasm $< -o$@

iffl_pucr.prg: tst_pucr.s cfg_pucr.s ../iffl.s
	dasm $< -o$@

iffl_pucr_linked.prg: tst_pucr_linked.s cfg_pucr.s ../iffl.s
	dasm $< -o$@

iffl_unp.prg: tst_unp.s cfg_unp.s ../iffl.s
	dasm $< -o$@

iffldata_bb2: music.prg.b2 picture.prg.b2
	makeiffl $@
	addiffl $@ music.prg.b2
	addiffl $@ picture.prg.b2

iffldata_bb2_linked: music.prg.b2 picture.prg.b2
	makeiffl $@
ifeq ($(findstring Windows,$(OS)),Windows)
	copy /b music.prg.b2 + picture.prg.b2 data_bb2_linked
else
	cat $^ > data_bb2_linked
endif
	addiffl $@ data_bb2_linked

iffldata_exom: music.exo picture.exo
	makeiffl $@
	addiffl $@ music.exo
	addiffl $@ picture.exo

iffldata_exom_linked: music.exo picture.exo
	makeiffl $@
ifeq ($(findstring Windows,$(OS)),Windows)
	copy /b music.exo + picture.exo data_exom_linked
else
	cat $^ > data_exom_linked
endif
	addiffl $@ data_exom_linked

iffldata_pucr: music.puc picture.puc
	makeiffl $@
	addiffl $@ music.puc
	addiffl $@ picture.puc

iffldata_pucr_linked: music.puc picture.puc
	makeiffl $@
ifeq ($(findstring Windows,$(OS)),Windows)
	copy /b music.puc + picture.puc data_pucr_linked
else
	cat $^ > data_pucr_linked
endif
	addiffl $@ data_pucr_linked

iffldata_unp: music.prg picture.prg
	makeiffl $@
	addiffl $@ music.prg
	addiffl $@ picture.prg

music.prg.b2: music.prg
	b2 -b $<

picture.prg.b2: picture.prg
	b2 -b $<

music.exo: music.prg
	exomizer level $< -o $@

picture.exo: picture.prg
	exomizer level $< -o $@

music.puc: music.prg
	pucrunch -c0 $< $@

picture.puc: picture.prg
	pucrunch -c0 $< $@
