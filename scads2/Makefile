EXE = .exe
CFLAGS = -O2
HFLAGS = -O -fno-warn-tabs

ifeq ($(OS),Windows_NT)
    cleancommand := del /F /Q objects\* scads2$(EXE) decrypt-tads2$(EXE)
else
    cleancommand := rm -f objects/* scads2$(EXE) decrypt-tads2$(EXE)
endif

all : objects scads2$(EXE) decrypt-tads2$(EXE)

objects : 
	mkdir objects

objects/Scads2_load.o : src/Scads2_load.c src/Scads2_load.h
	gcc -Isrc/ -c ${CFLAGS} $< -o $@

objects/decrypt-tads2.o : src/decrypt-tads2.c src/Scads2_load.h
	gcc -Isrc/ -c ${CFLAGS} $< -o $@

scads2$(EXE) : src/Scads2*.hs objects/Scads2_load.o
	ghc --make -isrc/ -XImplicitParams -outputdir objects -o $@ ${HFLAGS} src/Scads2.hs src/Scads2_load.c src/Scads2_load.h
	strip $@

decrypt-tads2$(EXE) : objects/decrypt-tads2.o objects/Scads2_load.o
	gcc -Isrc/ -o $@ objects/decrypt-tads2.o objects/Scads2_load.o

clean:
	${cleancommand}
