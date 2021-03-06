# Makefile for IFFL examples by Luigi Di Fraia 4/2018
#
# Note: the extended version of the ByteBoozer2 cruncher is used here
# https://github.com/luigidifraia/ByteBoozer2/tree/master/b2

all: iffl_bb2.d81 iffl_bb2_linked.d81 iffl_exom.d81 iffl_exom_linked.d81 iffl_pucr.d81 iffl_pucr_linked.d81 iffl_unp.d81

clean:
	rm -f *.d64 *.d81 *.b2 *.exo *.puc data_*_linked iffl_*.prg iffldata_*

iffl_bb2.d81: iffl_bb2.d64 iffl_bb2_1581.seq
	c1541 < iffl_bb2_1581.seq

iffl_bb2.d64: iffl_bb2.prg iffldata_bb2 iffl_bb2.seq
	makedisk $@ iffl_bb2.seq "IFFL EXAMPLE      IF 2A" 10

iffl_bb2_linked.d81: iffl_bb2_linked.d64 iffl_bb2_linked_1581.seq
	c1541 < iffl_bb2_linked_1581.seq

iffl_bb2_linked.d64: iffl_bb2_linked.prg iffldata_bb2_linked iffl_bb2_linked.seq
	makedisk $@ iffl_bb2_linked.seq "IFFL EXAMPLE      IF 2A" 10

iffl_exom.d81: iffl_exom.d64 iffl_exom_1581.seq
	c1541 < iffl_exom_1581.seq

iffl_exom.d64: iffl_exom.prg iffldata_exom iffl_exom.seq
	makedisk $@ iffl_exom.seq "IFFL EXAMPLE      IF 2A" 10

iffl_exom_linked.d81: iffl_exom_linked.d64 iffl_exom_linked_1581.seq
	c1541 < iffl_exom_linked_1581.seq

iffl_exom_linked.d64: iffl_exom_linked.prg iffldata_exom_linked iffl_exom_linked.seq
	makedisk $@ iffl_exom_linked.seq "IFFL EXAMPLE      IF 2A" 10

iffl_pucr.d81: iffl_pucr.d64 iffl_pucr_1581.seq
	c1541 < iffl_pucr_1581.seq

iffl_pucr.d64: iffl_pucr.prg iffldata_pucr iffl_pucr.seq
	makedisk $@ iffl_pucr.seq "IFFL EXAMPLE      IF 2A" 10

iffl_pucr_linked.d81: iffl_pucr_linked.d64 iffl_pucr_linked_1581.seq
	c1541 < iffl_pucr_linked_1581.seq

iffl_pucr_linked.d64: iffl_pucr_linked.prg iffldata_pucr_linked iffl_pucr_linked.seq
	makedisk $@ iffl_pucr_linked.seq "IFFL EXAMPLE      IF 2A" 10

iffl_unp.d81: iffl_unp.d64 iffl_unp_1581.seq
	c1541 < iffl_unp_1581.seq

iffl_unp.d64: iffl_unp.prg iffldata_unp iffl_unp.seq
	makedisk $@ iffl_unp.seq "IFFL EXAMPLE      IF 2A" 10

iffl_bb2.prg: tst_bb2.s cfg_bb2.s ../iffl_init.s ../iffl_loader.s
	dasm $< -o$@

iffl_bb2_linked.prg: tst_bb2_linked.s cfg_bb2.s ../iffl_init.s ../iffl_loader.s
	dasm $< -o$@

iffl_exom.prg: tst_exom.s cfg_exom.s ../iffl_init.s ../iffl_loader.s
	dasm $< -o$@

iffl_exom_linked.prg: tst_exom_linked.s cfg_exom.s ../iffl_init.s ../iffl_loader.s
	dasm $< -o$@

iffl_pucr.prg: tst_pucr.s cfg_pucr.s ../iffl_init.s ../iffl_loader.s
	dasm $< -o$@

iffl_pucr_linked.prg: tst_pucr_linked.s cfg_pucr.s ../iffl_init.s ../iffl_loader.s
	dasm $< -o$@

iffl_unp.prg: tst_unp.s cfg_unp.s ../iffl_init.s ../iffl_loader.s
	dasm $< -o$@

iffldata_bb2: music.prg.b2 picture.prg.b2
	makeiffl $@
	addiffl $@ music.prg.b2
	addiffl $@ picture.prg.b2

iffldata_bb2_linked: data_bb2_linked
	makeiffl $@
	addiffl $@ $<

data_bb2_linked: music.prg.b2 picture.prg.b2
ifeq ($(findstring Windows,$(OS)),Windows)
	copy /b music.prg.b2 + picture.prg.b2 $@
else
	cat $^ > $@
endif

iffldata_exom: music.exo picture.exo
	makeiffl $@
	addiffl $@ music.exo
	addiffl $@ picture.exo

iffldata_exom_linked: data_exom_linked
	makeiffl $@
	addiffl $@ $<

data_exom_linked: music.exo picture.exo
ifeq ($(findstring Windows,$(OS)),Windows)
	copy /b music.exo + picture.exo $@
else
	cat $^ > $@
endif

iffldata_pucr: music.puc picture.puc
	makeiffl $@
	addiffl $@ music.puc
	addiffl $@ picture.puc

iffldata_pucr_linked: data_pucr_linked
	makeiffl $@
	addiffl $@ $<

data_pucr_linked: music.puc picture.puc
ifeq ($(findstring Windows,$(OS)),Windows)
	copy /b music.puc + picture.puc $@
else
	cat $^ > $@
endif

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
