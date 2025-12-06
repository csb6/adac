/*
Lexer generator
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
#ifndef ADA_FILE_BUFFER_H
#define ADA_FILE_BUFFER_H

#include <stdbool.h>

typedef struct {
    const char* start;
    const char* end;
    const char* path;
    int fd;
} FileBuffer;

bool file_buffer_open(FileBuffer* buffer, const char* path);
bool file_buffer_close(FileBuffer* buffer);

#endif /* ADA_FILE_BUFFER_H */
