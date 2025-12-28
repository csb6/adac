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
#include "lexer.h"
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include "error.h"

static const struct {
    const char * keyword;
    TokenKind token;
} keywords[] = {
    {"type",    TOKEN_TYPE},
    {"is",      TOKEN_IS},
    {"option",  TOKEN_OPTION},
    {"others",  TOKEN_OTHERS},
    {"range",   TOKEN_RANGE},
};

static
TokenKind find_keyword(StringView identifier)
{
    for(size_t i = 0; i < sizeof(keywords) / sizeof(keywords[0]); ++i) {
        if(strncmp(keywords[i].keyword, identifier.value, identifier.len) == 0) {
            return keywords[i].token;
        }
    }
    return TOKEN_KIND_COUNT;
}

static
void print_unexpected_char(uint32_t line_num, const char* curr)
{
    error_print(line_num, "Unexpected character: %c", *curr);
}

const char* lexer_parse_token(const char* input_end, const char* curr, Token* token)
{
    token->kind = TOKEN_EOF;
    bool done = false;
    while(curr != input_end && !done) {
        switch(*curr) {
            case '#':
                // Comments
                ++curr;
                while(curr != input_end) {
                    if(*curr == '\n') {
                        ++curr;
                        ++token->line_num;
                        break;
                    }
                    ++curr;
                }
                break;
            case '|':
                token->kind = TOKEN_BAR;
                token->text.value = curr;
                token->text.len = 1;
                ++curr;
                done = true;
                break;
            case ':':
                token->kind = TOKEN_COLON;
                token->text.value = curr;
                token->text.len = 1;
                ++curr;
                done = true;
                break;
            case '[':
                token->kind = TOKEN_L_SQ_BRACKET;
                token->text.value = curr;
                token->text.len = 1;
                ++curr;
                done = true;
                break;
            case ']':
                token->kind = TOKEN_R_SQ_BRACKET;
                token->text.value = curr;
                token->text.len = 1;
                ++curr;
                done = true;
                break;
            case '{':
                token->kind = TOKEN_L_BRACE;
                token->text.value = curr;
                token->text.len = 1;
                ++curr;
                done = true;
                break;
            case '}':
                token->kind = TOKEN_R_BRACE;
                token->text.value = curr;
                token->text.len = 1;
                ++curr;
                done = true;
                break;
            case '=':
                token->kind = TOKEN_EQ;
                token->text.value = curr;
                token->text.len = 1;
                ++curr;
                done = true;
                break;
            case ',':
                token->kind = TOKEN_COMMA;
                token->text.value = curr;
                token->text.len = 1;
                ++curr;
                done = true;
                break;
            case ';':
                token->kind = TOKEN_SEMICOLON;
                token->text.value = curr;
                token->text.len = 1;
                ++curr;
                done = true;
                break;
            case '.':
                ++curr;
                if(curr == input_end) {
                    error_print(token->line_num, "Unexpected end of input");
                    error_exit();
                } else if(*curr != '.') {
                    print_unexpected_char(token->line_num, curr);
                    error_exit();
                } else {
                    token->kind = TOKEN_DOUBLE_DOT;
                    token->text.value = curr - 1;
                    token->text.len = 2;
                    ++curr;
                }
                done = true;
                break;
            case '\'':
                token->kind = TOKEN_CHAR_LITERAL;
                ++curr;
                if(curr == input_end) {
                    error_print(token->line_num, "Unexpected end of input");
                    error_exit();
                }
                token->text.value = curr;
                if(*curr == '\\') {
                    ++curr;
                    if(curr == input_end) {
                        error_print(token->line_num, "Unexpected end of escape sequence");
                        error_exit();
                    }
                    token->text.len = 2;
                } else {
                    token->text.len = 1;
                }
                ++curr;
                if(curr == input_end || *curr != '\'') {
                    error_print(token->line_num, "Unexpected end of character literal");
                    error_exit();
                }
                ++curr; // Skip closing '\''
                done = true;
                break;
            case '\n':
                ++token->line_num;
                ++curr;
                break;
            default:
                if(isspace(*curr)) {
                    // Skip whitespace
                    ++curr;
                } else if(isalpha(*curr)) {
                    token->text.value = curr;
                    ++curr;
                    while(curr != input_end && (isalnum(*curr) || *curr == '_')) {
                        ++curr;
                    }
                    token->text.len = curr - token->text.value;
                    TokenKind keyword = find_keyword(token->text);
                    token->kind = (keyword == TOKEN_KIND_COUNT) ? TOKEN_IDENT : keyword;
                    done = true;
                } else {
                    print_unexpected_char(token->line_num, curr);
                    error_exit();
                }
                break;
        }
    }
    return curr;
}

static const char* token_kind_strs[] = {
    // Basic tokens
    "|",
    ":",
    "[",
    "]",
    "{",
    "}",
    "=",
    ",",
    ";",
    "..",
    // Literals
    "character literal",
    // Identifiers
    "identifier",
    // Keywords,
    "type",
    "is",
    "option",
    "subtype",
    "range",
    "others",
    // Special tokens
    "end-of-file",
};

const char* token_kind_to_str(TokenKind kind)
{
    if(kind >= TOKEN_KIND_COUNT) {
        return "Unknown token kind";
    }
    return token_kind_strs[kind];
}
