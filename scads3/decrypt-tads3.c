#include <stdio.h>
#include "Scads3_load.h"


int main(int argc, char **argv) {
	FILE *out;
	const unsigned char *buf;
	int length;

	if (argc < 2 || argc > 3) {
		puts("usage: decrypt-tads3 <infile.t3> <outfile.t3>\n"
		     "       decrypt-tads3 <inoutfile.t3>");
		return 1;
	}

	buf = load_and_decrypt_tads3(argv[1]);
	if (!buf) return 1;

	out = fopen(argv[argc-1], "wb");
	if (!out) {
		printf("unable to open \"%s\"\n", argv[argc-1]);
		return 1;
	}

	length = *(int *)buf;
	if (fwrite(buf + sizeof(int *), 1, length, out) != length) {
		printf("unable to write to \"%s\"\n", argv[argc-1]);
		return 1;
	}

	return 0;
}
