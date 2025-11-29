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
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include "ast.h"
#include "error.h"
#include "parser.h"
#include "debug.h"
#include "string_pool.h"
#include "string_view.h"

static
void usage(const char* exe)
{
    fprintf(stderr, "Usage: %s file...\n", exe);
}

static
off_t get_file_size(int fd)
{
    struct stat file_info = {0};
    if(fstat(fd, &file_info) < 0) {
        return -1;
    }
    return file_info.st_size;
}

static
bool ends_with(StringView s, const char* suffix)
{
    size_t suffix_len = strlen(suffix);
    return suffix_len <= s.len
        && memcmp(s.value + (s.len - suffix_len), suffix, suffix_len) == 0;
}

int main(int argc, char** argv)
{
    if(argc < 2) {
        usage(argv[0]);
        return 1;
    }

    string_pool_init();
    parser_init();
    for(int i = 1; i < argc; ++i) {
        const char* source_file_path = argv[i];
        StringView source_file_path_view = {.value = source_file_path, .len = strlen(source_file_path)};

        if(ends_with(source_file_path_view, ".ads")) {
            fprintf(stderr, "Error: Specification files are parsed on demand as needed - no need to pass %s explicitly\n", source_file_path);
            return 1;
        }
        if(!ends_with(source_file_path_view, ".adb")) {
            fprintf(stderr, "Error: Unrecognized source file extension (must be .adb)\n");
            return 1;
        }

        error_set_source_file_path(source_file_path);
        int fd = open(source_file_path, O_RDONLY);
        if(fd < 0) {
            fprintf(stderr, "Error: Failed to open %s: %s\n", source_file_path, strerror(errno));
            return 1;
        }
        off_t file_size = get_file_size(fd);
        if(file_size < 0) {
            fprintf(stderr, "Error: Failed to read metadata for %s: %s\n", source_file_path, strerror(errno));
            return 1;
        }

        void* input_file = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE, fd, 0);
        if(input_file == MAP_FAILED) {
            fprintf(stderr, "Error: Failed to read %s: %s\n", source_file_path, strerror(errno));
            return 1;
        }

        const char* input_start = input_file;
        const char* input_end = input_start + file_size;
        CompilationUnit* unit = parser_parse(input_start, input_end);
        if(unit == NULL) {
            fprintf(stderr, "Error: Failed to parse %s\n", source_file_path);
            return 1;
        }
        switch(unit->kind) {
            case COMP_UNIT_PACKAGE_SPEC:
                print_package_spec(&unit->u.package_spec);
                break;
            default:
                printf("Unhandled compilation unit\n");
        }

        if(munmap(input_file, file_size) < 0) {
            fprintf(stderr, "Error: Failed to unmap %s\n", source_file_path);
            return 1;
        }
        close(fd);
    }

    return 0;
}
