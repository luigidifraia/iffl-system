# Makefile for IFFL tools by Luigi Di Fraia 4/2018

all: makeiffl addiffl iffltool

makeiffl: makeiffl.c
	gcc -O3 -Wall -o $@ $<

addiffl: addiffl.c
	gcc -O3 -Wall -o $@ $<

iffltool: iffltool.c
	gcc -O3 -Wall -o $@ $<

clean:
	rm -f *.iffl *.o iffltool addiffl makeiffl
