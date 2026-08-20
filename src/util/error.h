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
#ifndef ADA_ERROR_H
#define ADA_ERROR_H

#include <stdint.h>

typedef struct {
    uint16_t file_id;
    uint16_t line_num;
} SourceLocation;

void error_init(void);
uint16_t error_get_file_id(const char* source_path);
void error_print(SourceLocation loc, const char *message, ...);
void error_print_general(const char* message, ...);
void error_exit(void) __attribute__ ((noreturn));

#endif /* ADA_ERROR_H */
