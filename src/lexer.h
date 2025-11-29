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
#ifndef ADA_LEXER_H
#define ADA_LEXER_H

#include "token.h"

const char* lexer_parse_token(const char* input_start, const char* input_end, const char* curr, Token* token);
TokenKind lexer_lookahead(const char* input_start, const char* input_end, const char** curr);

#endif /*ADA_LEXER_H*/
