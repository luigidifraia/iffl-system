//
// Tool for managing IFFL files.
//

// To compile on Ubuntu or Mac OS X with xcode:
//   gcc -O3 -Wall -o iffltool iffltool.c

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FILENAME_MAX_LEN 256
#define IFFL_EMPTY_FILE_SIZE 254
#define IFFL_MAX_FILES 127
#define IFFL_MAX_FILE_SIZE 339968

int flush_iffl_file(char *filename)
{
    unsigned char lentbl[IFFL_EMPTY_FILE_SIZE];

    memset(lentbl, 0, IFFL_EMPTY_FILE_SIZE);

    // overwrite file with empty lentbl
    int fd = open(filename, O_WRONLY | O_TRUNC);
    if (fd < 0)
    {
        printf("Couldn't open file %s for writing!\n", filename);
        return 1;
    }
    write(fd, lentbl, IFFL_EMPTY_FILE_SIZE);
    close(fd);
    return 0;
}
int create_iffl_file(char *filename)
{

    int fd = open(filename, O_RDWR | O_CREAT, 0666);
    if (fd < 0)
    {
        printf("Couldn't open IFFL-file!\n");
        return 1;
    }
    if (ftruncate(fd, IFFL_EMPTY_FILE_SIZE) < 0)
    {
        printf("Couldn't truncate IFFL-file!\n");
        return 1;
    }
    close(fd);
    return 0;
};

int add_file_to_iffl(char *filename, char *iffl_filename)
{

    // read lentbl from the file
    int fd = open(iffl_filename, O_RDONLY);
    if (fd == -1)
    {
        // check if file is not existing
        if (access(iffl_filename, F_OK) == -1)
        {
            // ask user interaction if file should be created
            printf("IFFL-file doesn't exist! Create it? (y/n)\n");
            char answer;
            scanf("%c", &answer);
            if (answer == 'y')
            {
                create_iffl_file(iffl_filename); // create file
            }
            else
            {
                printf("IFFL-file not created.\n"); // abort
                return 1;
            }
        }
    }
    unsigned char lentbl[IFFL_EMPTY_FILE_SIZE];
    memset(lentbl, 0, IFFL_EMPTY_FILE_SIZE);

    int c;
    for (c = 0; c < IFFL_EMPTY_FILE_SIZE; c++)
    {
        read(fd, &lentbl[c], 1);
    }
    close(fd);

    // check number of entries in lentbl
    int filepos = 0;
    for (filepos = 0; filepos < IFFL_MAX_FILES; filepos++)
    {
        if (!lentbl[filepos] && !lentbl[filepos + IFFL_MAX_FILES])
            break;
    }
    if (filepos == IFFL_MAX_FILES)
    {
        printf("IFFL-file is full!\n");
        return 1;
    }

    // open source file
    fd = open(filename, O_RDONLY);
    if (fd == -1)
    {
        printf("Couldn't open file!\n");
        return 1;
    }
    // open iffl file
    int iffl_fd = open(iffl_filename, O_WRONLY);
    if (iffl_fd == -1)
    {
        printf("Couldn't open IFFL-file!\n");
        return 1;
    }

    lseek(fd, 0, SEEK_END);
    int file_size = lseek(fd, 0, SEEK_CUR);
    lseek(fd, 0, SEEK_SET);

    printf("file %s added at filepos %d, filesize %d\n", filename, filepos, file_size);

    lentbl[filepos] = file_size & 255;
    lentbl[filepos + 127] = file_size >> 8;

    for (c = 0; c < IFFL_EMPTY_FILE_SIZE; c++)
    {
        write(iffl_fd, &lentbl[c], 1);
    }

    // seek to end of iffl file
    lseek(iffl_fd, 0, SEEK_END);
    // copy file to end of iffl file
    unsigned char byte;
    while (read(fd, &byte, 1))
    {
        write(iffl_fd, &byte, 1);
    }

    close(fd);
    close(iffl_fd);

    return 0;
}

int list_files(char *iffl_filename)
{
    unsigned char lentbl[IFFL_EMPTY_FILE_SIZE];
    int c = 0;

    memset(lentbl, 0, IFFL_EMPTY_FILE_SIZE);

    // get file length of iffl_filename
    int fd = open(iffl_filename, O_RDONLY);
    if (fd == -1)
    {
        printf("Couldn't open IFFL-file!\n");
        return 1;
    }
    lseek(fd, 0, SEEK_END);
    int file_size = lseek(fd, 0, SEEK_CUR);
    lseek(fd, 0, SEEK_SET);

    // read lentbl from the file

    for (c = 0; c < IFFL_EMPTY_FILE_SIZE; c++)
    {
        read(fd, &lentbl[c], 1);
    }
    close(fd);

    // get number of files
    int num_files = 0;
    for (c = 0; c < IFFL_MAX_FILES; c++)
    {
        if (!lentbl[c] && !lentbl[c + IFFL_MAX_FILES])
            break;
        num_files++;
    }

    // read lentbl from the file

    read(fd, &lentbl[c], IFFL_EMPTY_FILE_SIZE);
    close(fd);

    printf("%s contains %d files. ", iffl_filename, num_files);
    printf("filesize is %d bytes.\n", file_size);

    // print file lengths from lentbl
    for (c = 0; c < IFFL_MAX_FILES; c++)
    {
        if (!lentbl[c] && !lentbl[c + IFFL_MAX_FILES])
            break;
        printf("file %d: %d bytes\n", c + 1, lentbl[c] + lentbl[c + IFFL_MAX_FILES] * 256);
    }

    return 0;
}

void print_help()
{
    printf("Usage: iffltool [options]\n");
    printf("Commands:\n");
    printf("  create <iffl-file>\t\tCreate a new IFFL file\n");
    printf("  add <file> <iffl-file>\tAdd a file to an IFFL file\n");
    printf("  list <iffl-file>\t\tList contents of IFFL file\n");
    printf("  flush <iffl-file>\t\tFlush a IFFL file\n");
    printf("  help\t\t\t\tPrint this help\n");
    printf("\n");
};

// main function with arguments
int main(int argc, char **argv)
{

    // check for correct number of arguments
    if (argc > 1 && strncmp(argv[1], "help", 4) == 0)
    {
        print_help();
        return 0;
    } 
    else if (argc < 2)
    {
        printf("%s\n\n", "No arguments given");
        print_help();
        return 0;
    }
    else if (argc > 2 && strncmp(argv[1], "create", 6) == 0)
    {
        create_iffl_file(argv[2]);
    }
    else if (argc > 3 && strncmp(argv[1], "add", 3) == 0)
    {
        add_file_to_iffl(argv[3], argv[2]);
    }
    else if (argc > 2 && strncmp(argv[1], "list", 4) == 0)
    {
        list_files(argv[2]);
    }
    else if (argc > 2 && strncmp(argv[1], "flush", 4) == 0)
    {
        flush_iffl_file(argv[2]);
    }

    return 0;
}
