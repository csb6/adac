/*
lexer_gen - lexer generator
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
#ifndef LEXER_GEN_LEXER_H
#define LEXER_GEN_LEXER_H

#include <stdint.h>
#include "string_view.h"

typedef uint8_t TokenKind;
enum {
    // Basic tokens
    TOKEN_BAR, TOKEN_COLON, TOKEN_L_SQ_BRACKET, TOKEN_R_SQ_BRACKET, TOKEN_L_BRACE, TOKEN_R_BRACE,
    TOKEN_EQ, TOKEN_COMMA, TOKEN_SEMICOLON, TOKEN_DOUBLE_DOT,
    // Literals
    TOKEN_CHAR_LITERAL,
    // Identifiers
    TOKEN_IDENT,
    // Keywords,
    TOKEN_TYPE, TOKEN_IS, TOKEN_OPTION, TOKEN_RANGE, TOKEN_OTHERS,
    // Special tokens
    TOKEN_EOF,

    TOKEN_KIND_COUNT,
};

typedef struct {
    StringView text;
    TokenKind kind;
    uint32_t line_num;
} Token;

const char* lexer_parse_token(const char* input_end, const char* curr, Token* token);

const char* token_kind_to_str(TokenKind kind);

#endif /* LEXER_GEN_LEXER_H */
