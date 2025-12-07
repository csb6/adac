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
#include "string_view.h"
#include <assert.h>
#include <string.h>

void escape_char(char c, char* buffer)
{
    buffer[0] = '\\';
    switch(c) {
        case '\n':
            buffer[1] = 'n';
            break;
        case '\r':
            buffer[1] = 'r';
            break;
        case '\v':
            buffer[1] = 'v';
            break;
        case '\t':
            buffer[1] = 't';
            break;
        case '\f':
            buffer[1] = 'f';
            break;
        default:
            // No need to escape since is printable
            buffer[0] = c;
            break;
    }
}

char unescape_char(char second)
{
    char c;
    switch(second) {
        case '\\':
            c = '\\';
            break;
        case 'n':
            c = '\n';
            break;
        case 'r':
            c = '\r';
            break;
        case '\'':
            c = '\'';
            break;
        case 'v':
            c = '\v';
            break;
        case 't':
            c = '\t';
            break;
        case 'f':
            c = '\f';
            break;
        default:
            // Invalid escape sequence
            c = '\0';
    }
    return c;
}

bool string_view_eq(const StringView* a, const StringView* b)
{
    return a->len == b->len && strncmp(a->value, b->value, a->len) == 0;
}
