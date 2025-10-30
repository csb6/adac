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
#ifndef ADA_TOKEN_H
#define ADA_TOKEN_H

#include <stdint.h>
#include "string_view.h"

typedef uint8_t TokenKind;
enum {
    // Basic lexical elements
    //  Single character elements
    TOKEN_AMP, TOKEN_L_PAREN, TOKEN_R_PAREN, TOKEN_PLUS, TOKEN_COMMA, TOKEN_SEMICOLON, TOKEN_BAR,
    TOKEN_SINGLE_QUOTE, TOKEN_MULT, TOKEN_MINUS, TOKEN_DOT, TOKEN_DIVIDE, TOKEN_COLON,
    TOKEN_LT, TOKEN_EQ, TOKEN_GT,
    //  Multi-character elements
    TOKEN_NEQ, TOKEN_GTE, TOKEN_LTE, TOKEN_ARROW, TOKEN_DOUBLE_DOT, TOKEN_EXP, TOKEN_ASSIGN, TOKEN_L_LABEL_BRACKET,
    TOKEN_R_LABEL_BRACKET, TOKEN_BOX,
    // Identifier
    TOKEN_IDENT,
    // Literals
    TOKEN_NUM_LITERAL, TOKEN_CHAR_LITERAL, TOKEN_STRING_LITERAL,
    // Reserved words
    TOKEN_ABORT, TOKEN_ABS, TOKEN_ACCEPT, TOKEN_ACCESS, TOKEN_ALL, TOKEN_AND, TOKEN_ARRAY,
    TOKEN_AT, TOKEN_BEGIN, TOKEN_BODY, TOKEN_CASE, TOKEN_CONSTANT, TOKEN_DECLARE, TOKEN_DELAY,
    TOKEN_DELTA, TOKEN_DIGITS, TOKEN_DO, TOKEN_ELSE, TOKEN_ELSIF, TOKEN_END, TOKEN_ENTRY,
    TOKEN_EXCEPTION, TOKEN_EXIT, TOKEN_FOR, TOKEN_FUNCTION, TOKEN_GENERIC, TOKEN_GOTO, TOKEN_IF,
    TOKEN_IN, TOKEN_IS, TOKEN_LIMITED, TOKEN_LOOP, TOKEN_MOD, TOKEN_NEW, TOKEN_NOT, TOKEN_NULL,
    TOKEN_OF, TOKEN_OR, TOKEN_OTHERS, TOKEN_OUT, TOKEN_PACKAGE, TOKEN_PRAGMA, TOKEN_PRIVATE,
    TOKEN_PROCEDURE, TOKEN_RAISE, TOKEN_RANGE, TOKEN_RECORD, TOKEN_REM, TOKEN_RENAMES, TOKEN_RETURN,
    TOKEN_REVERSE, TOKEN_SELECT, TOKEN_SEPARATE, TOKEN_SUBTYPE, TOKEN_TASK, TOKEN_TERMINATE,
    TOKEN_THEN, TOKEN_TYPE, TOKEN_USE, TOKEN_WHEN, TOKEN_WHILE, TOKEN_WITH, TOKEN_XOR,
    // EOF
    TOKEN_EOF,
    // Error
    TOKEN_ERROR,

    TOKEN_NUM_TOKEN_KINDS
};

typedef struct {
    uint32_t start;
    uint32_t len;
    TokenKind kind;
    union {
        uint8_t num_base;
    } extra;
} Token;

StringView token_to_str(const char* text_start, const Token* token);

#endif /* ADA_TOKEN_H */
