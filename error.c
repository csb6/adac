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
#include "error.h"
#include <stdio.h>

static int error_status = 0;
static const char* source_file_path = NULL;

void error_set_source_file_path(const char* path)
{
    source_file_path = path;
}

int error_get_return_status(void)
{
    return error_status;
}

void error_print(const char* message)
{
    if(source_file_path) {
        fprintf(stderr, "%s: ", source_file_path);
    }
    fprintf(stderr, "Error: %s\n", message);
    error_status = 1;
}
