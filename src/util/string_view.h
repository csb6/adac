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
#ifndef ADA_STRING_VIEW_H
#define ADA_STRING_VIEW_H

#include <stdbool.h>
#include <stdint.h>

typedef struct {
    const char* value;
    uint32_t len;
} StringView;

// Enables succinct printing of a StringView using printf("%.*s")
#define SV(sv) (sv).len, (sv).value

// Pre: length of buffer >= 2
void escape_char(char c, char* buffer);
char unescape_char(char second);
bool string_view_eq(const StringView* a, const StringView* b);

#endif /* ADA_STRING_VIEW_H */
