EXE = #.exe
CFLAGS = -O2
HFLAGS = -O

all : scads2$(EXE) decrypt-tads2$(EXE)

Scads2_load.o : Scads2_load.c Scads2_load.h
	gcc -c ${CFLAGS} $<

decrypt-tads2.o : decrypt-tads2.c Scads2_load.h
	gcc -c ${CFLAGS} $<

scads2$(EXE) : Scads2*.hs Scads2_load.o
	ghc --make -fglasgow-exts -o $@ ${HFLAGS} Scads2.hs Scads2_load.o
	strip $@

decrypt-tads2$(EXE) : decrypt-tads2.o Scads2_load.o
	gcc -o $@ decrypt-tads2.o Scads2_load.o
