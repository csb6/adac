/*
Ada compiler
Copyright (C) 2025  Cole Blakley

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include "ast.h"
#include "error.h"
#include "parser.h"
#include "debug.h"

static
void usage(const char* exe)
{
    fprintf(stderr, "Usage: %s source_file\n", exe);
}

static
off_t get_file_size(int fd)
{
    struct stat file_info = {0};
    if(fstat(fd, &file_info) < 0) {
        perror("Failed to read metadata of input file");
        return -1;
    }
    return file_info.st_size;
}

int main(int argc, char** argv)
{
    if(argc != 2) {
        usage(argv[0]);
        return 1;
    }

    const char* source_file_path = argv[1];
    error_set_source_file_path(source_file_path);
    int fd = open(source_file_path, O_RDONLY);
    if(fd < 0) {
        perror("Failed to open file");
        return 1;
    }
    off_t file_size = get_file_size(fd);
    if(file_size < 0) {
        perror("Failed to read metadata of input file");
        return 1;
    }

    void* input_file = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if(input_file == MAP_FAILED) {
        perror("Failed to read input file");
        return 1;
    }

    const char* input_start = input_file;
    const char* input_end = input_start + file_size;
    PackageSpec* spec = parser_parse(input_start, input_end);
    if(spec == NULL) {
        fprintf(stderr, "Failed to parse package spec\n");
        return 1;
    }
    print_package_spec(spec);

    if(munmap(input_file, file_size) < 0) {
        perror("Failed to unmap input file");
        return 1;
    }
    close(fd);

    return error_get_return_status();
}
