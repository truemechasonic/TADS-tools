#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Scads3_load.h"

int
get2(const unsigned char *p)
{
	return p[0] + p[1]*256;
}

unsigned
get4(const unsigned char *p)
{
	return p[0] + p[1]*256 + p[2]*65536 + p[3]*16777216;
}

int
decrypt(unsigned char *buf, int len)
{
	int pos, version;
	if (memcmp(buf, "T3-image\015\012\032", 11) != 0) {
		puts("Not a T3 image file");
		return 0;
	}
	version = get2(buf+11);
	if (version != 1) {
		printf("Unknown T3 image version (%d, expected 1)\n", version);
		return 0;
	}
	pos = 69;
	while (pos <= len-10) {
		int i, mask;
		unsigned tag = get4(buf+pos);
		unsigned size = 10 + get4(buf+pos+4);
		switch (tag) {
		case 0x20464F45:	/* EOF */
			return 1;
		case 0x47505043:	/* CPPG */
			mask = buf[pos+16];
			for (i = 16; i < size; ++i)
				buf[pos+i] ^= mask;
			break;
		}
		pos += size;
	}
	puts("Warning: no EOF chunk");
	return 1;
}

#define GUARD 256

unsigned char *
load_and_decrypt_tads3(const char *filename)
{
	unsigned char *buffer;
	int length, read;
	FILE *f;

	f = fopen(filename, "rb");
	if (!f) {
		printf("unable to open \"%s\"\n", filename);
		return 0;
	}
	/* I don't know how portable this is... */
	fseek(f, 0, SEEK_END);
	length = ftell(f);
	buffer = malloc(sizeof(int)+length+GUARD);
	if (!buffer) {
		printf("unable to allocate %d bytes\n", sizeof(int)+length+GUARD);
		return 0;
	}
	*(int *)buffer = length;
	fseek(f, 0, SEEK_SET);
	read = fread(buffer+sizeof(int), 1, length, f);
	fclose(f);
	if (read != length) {
		printf("unable to read %d bytes\n", length);
		free(buffer);
		return 0;
	}
	memset(buffer+sizeof(int)+length, 0, GUARD);
	if (!decrypt(buffer+sizeof(int), length)) {
		free(buffer);
		return 0;
	}
	return buffer;
}
