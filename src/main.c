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
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include "ast.h"
#include "error.h"
#include "parser.h"
#include "debug.h"
#include "string_pool.h"
#include "string_view.h"
#include "file_buffer.h"

static
void usage(const char* exe)
{
    fprintf(stderr, "Usage: %s file...\n", exe);
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

        FileBuffer file_buffer;
        if(!file_buffer_open(&file_buffer, source_file_path)) {
            return 1;
        }
        error_set_source_file_path(source_file_path);
        CompilationUnit* unit = parser_parse(file_buffer.start, file_buffer.end);
        if(unit == NULL) {
            fprintf(stderr, "Error: Failed to parse %s\n", source_file_path);
            return 1;
        }
        print_compilation_unit(unit);
        putchar('\n');

        if(!file_buffer_close(&file_buffer)) {
            return 1;
        }
    }

    return 0;
}
