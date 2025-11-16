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
#include "token.h"
#include <string.h>
#include "static_assert.h"

static const char* const token_kind_strs[] = {
    // Basic lexical elements
    //  Single character elements
    [TOKEN_AMP] = "&",
    [TOKEN_L_PAREN] = "(",
    [TOKEN_R_PAREN] = ")",
    [TOKEN_PLUS] = "+",
    [TOKEN_COMMA] = ",",
    [TOKEN_SEMICOLON] = ";",
    [TOKEN_BAR] = "|",
    [TOKEN_SINGLE_QUOTE] = "'",
    [TOKEN_MULT] = "*",
    [TOKEN_MINUS] = "-",
    [TOKEN_DOT] = ".",
    [TOKEN_DIVIDE] = "/",
    [TOKEN_COLON] = ":",
    [TOKEN_LT] = "<",
    [TOKEN_EQ] = "=",
    [TOKEN_GT] = ">",
    //  Multi-character elements
    [TOKEN_NEQ] = "/=",
    [TOKEN_GTE] = ">=",
    [TOKEN_LTE] = "<=",
    [TOKEN_ARROW] = "=>",
    [TOKEN_DOUBLE_DOT] = "..",
    [TOKEN_EXP] = "**",
    [TOKEN_ASSIGN] = ":=",
    [TOKEN_L_LABEL_BRACKET] = "<<",
    [TOKEN_R_LABEL_BRACKET] = ">>",
    [TOKEN_BOX] = "<>",
    [TOKEN_NOT_IN] = "not in",
    [TOKEN_AND_THEN] = "and then",
    [TOKEN_OR_ELSE] = "or else",
    // Identifier
    [TOKEN_IDENT] = "identifier",
    // Literals
    [TOKEN_NUM_LITERAL] = "numeric literal",
    [TOKEN_CHAR_LITERAL] = "character literal",
    [TOKEN_STRING_LITERAL] = "string literal",
    // Reserved words
    [TOKEN_ABORT] = "abort",
    [TOKEN_ABS] = "abs",
    [TOKEN_ACCEPT] = "accept",
    [TOKEN_ACCESS] = "access",
    [TOKEN_ALL] = "all",
    [TOKEN_AND] = "and",
    [TOKEN_ARRAY] = "array",
    [TOKEN_AT] = "at",
    [TOKEN_BEGIN] = "begin",
    [TOKEN_BODY] = "body",
    [TOKEN_CASE] = "case",
    [TOKEN_CONSTANT] = "constant",
    [TOKEN_DECLARE] = "declare",
    [TOKEN_DELAY] = "delay",
    [TOKEN_DELTA] = "delta",
    [TOKEN_DIGITS] = "digits",
    [TOKEN_DO] = "do",
    [TOKEN_ELSE] = "else",
    [TOKEN_ELSIF] = "elsif",
    [TOKEN_END] = "end",
    [TOKEN_ENTRY] = "entry",
    [TOKEN_EXCEPTION] = "exception",
    [TOKEN_EXIT] = "exit",
    [TOKEN_FOR] = "for",
    [TOKEN_FUNCTION] = "function",
    [TOKEN_GENERIC] = "generic",
    [TOKEN_GOTO] = "goto",
    [TOKEN_IF] = "if",
    [TOKEN_IN] = "in",
    [TOKEN_IS] = "is",
    [TOKEN_LIMITED] = "limited",
    [TOKEN_LOOP] = "loop",
    [TOKEN_MOD] = "mod",
    [TOKEN_NEW] = "new",
    [TOKEN_NOT] = "not",
    [TOKEN_NULL] = "null",
    [TOKEN_OF] = "of",
    [TOKEN_OR] = "or",
    [TOKEN_OTHERS] = "others",
    [TOKEN_OUT] = "out",
    [TOKEN_IN_OUT] = "in_out",
    [TOKEN_PACKAGE] = "package",
    [TOKEN_PRAGMA] = "pragma",
    [TOKEN_PRIVATE] = "private",
    [TOKEN_PROCEDURE] = "procedure",
    [TOKEN_RAISE] = "raise",
    [TOKEN_RANGE] = "range",
    [TOKEN_RECORD] = "record",
    [TOKEN_REM] = "rem",
    [TOKEN_RENAMES] = "renames",
    [TOKEN_RETURN] = "return",
    [TOKEN_REVERSE] = "reverse",
    [TOKEN_SELECT] = "select",
    [TOKEN_SEPARATE] = "separate",
    [TOKEN_SUBTYPE] = "subtype",
    [TOKEN_TASK] = "task",
    [TOKEN_TERMINATE] = "terminate",
    [TOKEN_THEN] = "then",
    [TOKEN_TYPE] = "type",
    [TOKEN_USE] = "use",
    [TOKEN_WHEN] = "when",
    [TOKEN_WHILE] = "while",
    [TOKEN_WITH] = "with",
    [TOKEN_XOR] = "xor",
    // EOF
    [TOKEN_EOF] = "end-of-file",
    // Error
    [TOKEN_ERROR] = "error token",
};
STATIC_ASSERT(sizeof(token_kind_strs) / sizeof(token_kind_strs)[0] == TOKEN_NUM_TOKEN_KINDS);

StringView token_to_str(const Token* token)
{
    StringView token_name = token->text;
    if(token->kind > TOKEN_NUM_TOKEN_KINDS) {
        token_name.value = "Invalid token";
        token_name.len = strlen(token_name.value);
    }

    return token_name;
}

const char* token_kind_to_str(TokenKind kind)
{
    const char* str;
    if(kind > TOKEN_NUM_TOKEN_KINDS) {
        str = "Invalid token";
    } else {
        str = token_kind_strs[kind];
    }
    return str;
}
