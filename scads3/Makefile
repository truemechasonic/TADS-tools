EXE =
CFLAGS = -g -O2
HFLAGS = -g -O -fno-warn-tabs

ifeq ($(OS),Windows_NT)
    cleancommand := del /F /Q objects\* scads3$(EXE) decrypt-tads3$(EXE)
    EXE = .exe
else
    cleancommand := rm -rf objects/ scads3$(EXE) decrypt-tads3$(EXE)
endif

all : objects scads3$(EXE) decrypt-tads3$(EXE)

objects : 
	mkdir objects

objects/Scads3_load.o : src/Scads3_load.c src/Scads3_load.h
	gcc -Isrc/ -c ${CFLAGS} $< -o $@

objects/decrypt-tads3.o : src/decrypt-tads3.c src/Scads3_load.h
	gcc -Isrc/ -c ${CFLAGS} $< -o $@

scads3$(EXE) : src/Scads3*.hs objects/Scads3_load.o
	ghc --make -isrc/ -XImplicitParams -outputdir objects -o $@ ${HFLAGS} src/Scads3.hs objects/Scads3_load.o
	strip $@

decrypt-tads3$(EXE) : objects/decrypt-tads3.o objects/Scads3_load.o
	gcc -Isrc/ -o $@ objects/decrypt-tads3.o objects/Scads3_load.o

clean:
	${cleancommand}
