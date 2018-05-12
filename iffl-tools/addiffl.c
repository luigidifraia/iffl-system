//
// Add file to IFFL file
//

// To compile on Ubuntu or Mac OS X with xcode:
//   gcc -O3 -Wall -o addiffl addiffl.c

#include <stdio.h>

int main(int argc, char **argv)
{
        int length = 0;
        int filepos = 0;
        unsigned char lentbl[254];
        FILE *in, *out;


        if (argc != 3)
        {
                printf("Usage: addiffl <iffl_filename> <filename>\n"
                       "Adds file <filename> to the IFFL file. IFFL file can hold max. 127 files and\n"
                       "the files must be less than 65536 bytes long.\n");
                return 1;
        }

        out = fopen(argv[1], "r+b");
        if (!out)
        {
                printf("Couldn't open IFFL-file!\n");
                return 1;
        }
        fread(lentbl, 1, 254, out);
        fseek(out, 0, SEEK_END);

        for (filepos = 0; filepos < 127; filepos++)
        {
                if (!lentbl[filepos] && !lentbl[filepos+127]) break;
        }
        if (filepos >= 127)
        {
                fclose(out);
                printf("IFFL-directory already full (127 files)!\n");
                return 1;
        }

        in = fopen(argv[2], "rb");
        if (!in)
        {
                fclose(out);
                printf("Couldn't open source-file!\n");
                return 1;
        }

        for (;;)
        {
                int c = fgetc(in);

                if (c == EOF) break;
                length++;
                fputc(c, out);
                if (length == 65535) break;
        }
        fclose(in);

        lentbl[filepos] = length & 255;
        lentbl[filepos+127] = length >> 8;

        fseek(out, 0, SEEK_SET);
        fwrite(lentbl, 1, 254, out);
        fclose(out);
        return 0;
}
