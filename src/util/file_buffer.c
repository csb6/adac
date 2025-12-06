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
#include "file_buffer.h"
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

static
off_t get_file_size(int fd)
{
    struct stat file_info = {0};
    if(fstat(fd, &file_info) < 0) {
        return -1;
    }
    return file_info.st_size;
}

bool file_buffer_open(FileBuffer* buffer, const char* path)
{
    int fd = open(path, O_RDONLY);
    if(fd < 0) {
        fprintf(stderr, "Error: Failed to open %s: %s\n", path, strerror(errno));
        return false;
    }
    off_t file_size = get_file_size(fd);
    if(file_size < 0) {
        fprintf(stderr, "Error: Failed to read metadata for %s: %s\n", path, strerror(errno));
        return false;
    }

    void* input_file = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if(input_file == MAP_FAILED) {
        fprintf(stderr, "Error: Failed to read %s: %s\n", path, strerror(errno));
        return false;
    }

    buffer->start = input_file;
    buffer->end = buffer->start + file_size;
    buffer->path = path;
    buffer->fd = fd;
    return true;
}

bool file_buffer_close(FileBuffer* buffer)
{
    off_t file_size = buffer->end - buffer->start;
    if(munmap((void*)buffer->start, file_size) < 0) {
        fprintf(stderr, "Error: Failed to unmap %s\n", buffer->path);
        return false;
    }
    if(close(buffer->fd) < 0) {
        fprintf(stderr, "Error: Failed to close %s\n", buffer->path);
        return false;
    }
    return true;
}
