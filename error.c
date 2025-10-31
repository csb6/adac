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
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>

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

void error_print(const char* text_start, const char* curr, const char* message, ...)
{
    if(source_file_path) {
        uint32_t line_num = 1;
        for(const char* c = text_start; c < curr; ++c) {
            if(*c == '\n') {
                ++line_num;
            }
        }
        fprintf(stderr, "%s:%u ", source_file_path, line_num);
    }

    va_list args;
    va_start(args, message);
    fprintf(stderr, "Error: ");
    vfprintf(stderr, message, args);
    fputc('\n', stderr);
    va_end(args);
    error_status = 1;
}
