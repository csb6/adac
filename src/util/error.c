/*
adac - Ada compiler
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
#define _XOPEN_SOURCE 500
#include "error.h"
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "array.h"

typedef char* Path;
DEFINE_ARRAY_TYPE(Path)
DEFINE_ARRAY_OPS(Path)

static PathArray source_paths;

void error_init(void)
{
    PathArray_init(&source_paths);
}

uint16_t error_get_file_id(const char* source_path)
{
    uint16_t i;
    uint16_t source_paths_count = (uint16_t)PathArray_size(&source_paths);
    for(i = 0; i < source_paths_count; ++i) {
        if(strcmp(source_paths.data[i], source_path) == 0) {
            return i;
        }
    }
    // New path: add it
    PathArray_append(&source_paths, strdup(source_path));
    return PathArray_size(&source_paths) - 1;
}

void error_print(SourceLocation loc, const char *message, ...)
{
    const char* source_file_path = "(Unknown file)";
    if(loc.file_id < PathArray_size(&source_paths)) {
        source_file_path = source_paths.data[loc.file_id];
    }
    fprintf(stderr, "%s:%u ", source_file_path, loc.line_num);

    va_list args;
    va_start(args, message);
    fprintf(stderr, "Error: ");
    vfprintf(stderr, message, args);
    fputc('\n', stderr);
    va_end(args);
}

void error_print_general(const char* message, ...)
{
    va_list args;
    va_start(args, message);
    fprintf(stderr, "Error: ");
    vfprintf(stderr, message, args);
    fputc('\n', stderr);
    va_end(args);
}

void error_exit(void)
{
    exit(1);
}
