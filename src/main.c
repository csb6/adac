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
#include <stdio.h>
#include <unistd.h>
#include "comp_manager.h"
#include "error.h"
#include "string_pool.h"

static
void usage(const char* exe_name);

int main(int argc, char * const* argv)
{
    error_init();
    string_pool_init();
    CompilationManager* comp_manager = comp_manager_init();

    int opt;
    while((opt = getopt(argc, argv, "I:h")) != -1) {
        switch(opt) {
            case 'I':
                comp_manager_add_source_dir(comp_manager, optarg);
                break;
            case 'h':
                usage(argv[0]);
                return 0;
            default:
                usage(argv[0]);
                return 1;
        }
    }

    if(optind >= argc) {
        fprintf(stderr, "Error: missing main unit name after options list\n");
        return 1;
    }

    const char* main_unit_name = argv[optind];
    comp_manager_parse_unit(comp_manager, main_unit_name);

    return 0;
}

static
void usage(const char* exe_name)
{
    fprintf(stderr, "Usage: %s [option...] main_unit\n", exe_name);
    fprintf(stderr, "\nOptions:\n"
                    "-I  Add search path for Ada source files\n"
                    "-h  Show help\n\n"
    );
}
