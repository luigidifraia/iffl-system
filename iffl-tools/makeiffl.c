//
// Make empty IFFL file
//

// To compile on Ubuntu or Mac OS X with xcode:
//   gcc -O3 -Wall -o makeiffl makeiffl.c

#include <stdio.h>

int main(int argc, char **argv)
{
        int c;
        FILE *out;

        if (argc < 2)
        {
                printf("Usage: makeiffl <iffl_filename>\n"
                       "Creates new IFFL-file to which files can then be added.\n");
                return 1;
        }

        out = fopen(argv[1], "wb");
        if (!out)
        {
                printf("Couldn't open IFFL-file!\n");
                return 1;
        }

        for (c = 0; c < 254; c++)
        {
                fputc(0, out);
        }

        fclose(out);
        return 0;
}
