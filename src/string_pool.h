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
#ifndef ADA_STRING_POOL_H
#define ADA_STRING_POOL_H

#include <stdint.h>
#include "string_view.h"

typedef uint32_t StringToken;

void string_pool_init(void);
StringToken string_pool_to_token(StringView s);
const char* string_pool_to_str(StringToken token);

// Enables succinct printing of a StringToken using printf("%s")
#define ST(token) string_pool_to_str(token)

#endif /* ADA_STRING_POOL_H */
