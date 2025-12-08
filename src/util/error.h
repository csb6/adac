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

void error_set_source_file_path(const char* path);
void error_print(const char* text_start, const char* curr, const char* message, ...);
void error_print2(uint32_t line_num, const char *message, ...);
void error_exit(void) __attribute__ ((noreturn));

#endif /* ADA_ERROR_H */
