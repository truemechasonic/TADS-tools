EXE = .exe
CFLAGS = -O2
HFLAGS = -O

all : scads3$(EXE) decrypt-tads3$(EXE)

Scads3_load.o : Scads3_load.c Scads3_load.h
	gcc -c ${CFLAGS} $<

decrypt-tads3.o : decrypt-tads3.c Scads3_load.h
	gcc -c ${CFLAGS} $<

scads3$(EXE) : Scads3*.hs Scads3_load.o
	ghc --make -fglasgow-exts -o $@ ${HFLAGS} Scads3.hs Scads3_load.o
	strip $@

decrypt-tads3$(EXE) : decrypt-tads3.o Scads3_load.o
	gcc -o $@ decrypt-tads3.o Scads3_load.o
