#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Scads2_load.h"

int
get2(const unsigned char *p)
{
	return p[0] + p[1]*256;
}

int
get4(const unsigned char *p)
{
	return p[0] + p[1]*256 + p[2]*65536 + p[3]*16777216;
}

unsigned char g_xorseed = 0x3F, g_xorinc = 0x40;

void
decrypt_chunk(unsigned char *from, unsigned char *to)
{
	unsigned char xor = g_xorseed;
	while (from < to) {
		*from++ ^= xor;
		xor += g_xorinc;
	}
}

void
decrypt_obj(unsigned char *from, unsigned char *to)
{
	while (from < to) {
		int use = get2(from+5);
		unsigned char *next_from = from+7+use;
		decrypt_chunk(from+7, next_from);
		from = next_from;
	}
}

void
decrypt_voc(unsigned char *from, unsigned char *to)
{
	while (from < to) {
		int len = get2(from) + get2(from+2);
		unsigned char *next_from = from+10+len;
		decrypt_chunk(from+10, next_from);
		from = next_from;
	}
}

void
decrypt_other(unsigned char *from)
{
	int len = get2(from);
	decrypt_chunk(from+2, from+2+len);
}

int
decrypt(unsigned char *buf)
{
	int pos;
	if (strcmp((char *)buf, "TADS2 bin\x0A\x0D\x1A") != 0) {
		puts("Not a TADS2 game file");
		return 0;
	}
	if (!(buf[20] & 8)) {
		return 1;	/* not encrypted */
	}
	buf[20] &= ~8;
	pos = 48;
	for (;;) {
		int tag_len = buf[pos++];
		int next_pos = get4(&buf[pos+tag_len]);
		if (tag_len == 0) {
			puts("Warning: no $EOF chunk");
			return 1;
		}
		if (tag_len == 3 && memcmp(buf+pos, "XSI", 3) == 0) {
			g_xorseed = buf[pos+7];
			g_xorinc = buf[pos+8];
			buf[pos+7] = 0;
			buf[pos+8] = 0;
		} else if (tag_len == 4 && memcmp(buf+pos, "$EOF", 4) == 0) {
			return 1;
		} else if (tag_len == 4 && memcmp(buf+pos, "CMPD", 4) == 0) {
			decrypt_other(buf+pos+8);
		} else if (tag_len == 6 && memcmp(buf+pos, "FMTSTR", 6) == 0) {
			decrypt_other(buf+pos+10);
		} else if (tag_len == 3 && memcmp(buf+pos, "OBJ", 3) == 0) {
			decrypt_obj(buf+pos+7, buf+next_pos);
		} else if (tag_len == 8 && memcmp(buf+pos, "SPECWORD", 8) == 0) {
			decrypt_other(buf+pos+12);
		} else if (tag_len == 3 && memcmp(buf+pos, "VOC", 3) == 0) {
			decrypt_voc(buf+pos+7, buf+next_pos);
		}
		pos = next_pos;
	}
}

#define GUARD 256

unsigned char *
load_and_decrypt_tads2(const char *filename)
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
	if (!decrypt(buffer+sizeof(int))) {
		free(buffer);
		return 0;
	}
	return buffer;
}
