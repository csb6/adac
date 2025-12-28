/*
lexer_gen - lexer generator
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
#include <stdio.h>
#include <string.h>
#include "ast.h"
#include "checker.h"
#include "error.h"
#include "generator.h"
#include "parser.h"
#include "file_buffer.h"

// TODO: generate move tables (will need to also ensure istates are successors of last value of StateTokenKind
//  so that both istates and tokens can be used as indices into move table)
int main(int argc, char** argv)
{
    if(argc != 4 || strcmp(argv[1], "-o") != 0) {
        fprintf(stderr, "Usage: %s -o output_dir lexer_spec_file\n", argv[0]);
        return 1;
    }

    FileBuffer file_buffer;
    if(!file_buffer_open(&file_buffer, argv[3])) {
        return 1;
    }
    error_set_source_file_path(file_buffer.path);
    const char* output_dir_path = argv[2];

    Module module = {0};
    parser_parse(file_buffer.start, file_buffer.end, &module);
    check_module(&module);
    generate(&module, output_dir_path);

    if(!file_buffer_close(&file_buffer)) {
        return 1;
    }

    return 0;
}
