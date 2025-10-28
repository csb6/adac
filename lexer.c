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
#include "lexer.h"
#include <ctype.h>
#include <string.h>
#include "error.h"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#include "keywords.c"
#pragma GCC diagnostic pop

#define set_simple_token(token_kind) \
    token->kind = token_kind; \
    token->start = curr - input_start; \
    token->len = 1;

static
int is_graphic_character(char c)
{
    return c >= ' ' && c <= '~';
}

static
const char* lex_numeric_literal(const char* input_start, const char* input_end, const char* curr, Token* token)
{
    const char* token_start = curr;
    ++curr;
    while(curr != input_end && (isdigit(*curr) || *curr == '_')) {
        ++curr;
    }
    if(curr != input_end) {
        if(*curr == '#') {
            // Based literals
            // TODO: store base in token->extra.num_base
            // TODO: validate that base is within [1,16]
            // TODO: validate that digits A-F are appropriate for given base
            // TODO: allow substitution of '#' with '"' subject to rules in LRM 2.10
            ++curr;
            while(curr != input_end && (isalnum(*curr) || *curr == '_')) {
                ++curr;
            }
            if(curr == input_end || *curr != '#') {
                error_print("Unexpected end of based literal");
                return curr;
            }
            ++curr;
        } else if(*curr == '.') {
            // Decimal literals
            ++curr;
            if(curr == input_end) {
                error_print("Unexpected end of decimal literal");
                return curr;
            }
            while(curr != input_end && (isdigit(*curr) || *curr == '_')) {
                ++curr;
            }
        }

        if(curr != input_end && (*curr == 'e' || *curr == 'E')) {
            // Exponents
            ++curr;
            if(curr == input_end) {
                error_print("Unexpected end of exponent");
                return curr;
            }
            if(*curr == '+' || *curr == '-') {
                ++curr;
                if(curr == input_end) {
                    error_print("Unexpected end of exponent");
                    return curr;
                }
            }
            if(!isdigit(*curr)) {
                error_print("Unexpected end of exponent");
                return curr;
            }
            ++curr;
            while(curr != input_end && (isdigit(*curr) || *curr == '_')) {
                ++curr;
            }
        }
    }
    token->kind = TOKEN_NUM_LITERAL;
    token->start = token_start - input_start;
    token->len = curr - token_start;
    return curr;
}

static
const char* lex_string_literal(const char* input_start, const char* input_end, const char* curr, Token* token)
{
    ++curr; // Skip '"'
    if(curr == input_end) {
        error_print("Unexpected end of string literal");
        return curr;
    }
    const char* token_start = curr;
    int found_closing_quote = 0;
    while(curr != input_end && !found_closing_quote) {
        if(*curr == '"') {
            if(curr + 1 != input_end && curr[1] == '"') {
                // Two double quotes inside a string literal are treated as one double quote (cursed)
                curr += 2;
            } else {
                found_closing_quote = 1;
            }
        } else if(is_graphic_character(*curr)) {
            ++curr;
        } else {
            error_print("Unexpected character");
            return curr;
        }
    }
    if(!found_closing_quote) {
        error_print("Missing closing quote for string literal");
        return curr;
    }
    token->kind = TOKEN_STRING_LITERAL;
    token->start = token_start - input_start;
    token->len = curr - token_start;
    ++curr; // Skip final '"'
    return curr;
}

static
const char* lex_identifier_or_keyword(const char* input_start, const char* input_end, const char* curr, Token* token)
{
    const char* token_start = curr;
    token->start = curr - input_start;
    ++curr;
    while(curr != input_end && isalpha(*curr)) {
        ++curr;
    }
    const struct keyword_token* keyword = is_keyword(token_start, curr - token_start);
    if(keyword && (curr == input_end || !(isalnum(*curr) || *curr == '_'))) {
        token->kind = keyword->kind;
    } else {
        token->kind = TOKEN_IDENT;
        while(curr != input_end && (isalnum(*curr) || *curr == '_')) {
            ++curr;
        }
    }
    token->len = curr - token_start;
    return curr;
}

static
const char* skip_comment(const char* curr, const char* input_end)
{
    curr += 2; // Skip over '--'
    while(curr != input_end && *curr != '\n') {
        ++curr;
    }
    return curr;
}

const char* lexer_parse_token(const char* input_start, const char* input_end, const char* curr, Token* token)
{
    memset(token, 0, sizeof(*token));
    token->kind = TOKEN_ERROR;
    int done;
    do {
        done = 1;
        // Skip leading whitespace
        while(curr < input_end && isspace(*curr)) {
            ++curr;
        }

        if(curr == input_end) {
            set_simple_token(TOKEN_EOF)
            return curr;
        }

        switch(*curr) {
            case '&':
                set_simple_token(TOKEN_AMP)
                ++curr;
                break;
            case '(':
                set_simple_token(TOKEN_L_PAREN)
                ++curr;
                break;
            case ')':
                set_simple_token(TOKEN_R_PAREN)
                ++curr;
                break;
            case '+':
                set_simple_token(TOKEN_PLUS)
                ++curr;
                break;
            case ',':
                set_simple_token(TOKEN_COMMA)
                ++curr;
                break;
            case ';':
                set_simple_token(TOKEN_SEMICOLON)
                ++curr;
                break;
            case '!':
            case '|':
                set_simple_token(TOKEN_BAR)
                ++curr;
                break;
            case '\'':
                if(curr + 2 < input_end && curr[2] == '\'') {
                    ++curr;
                    if(is_graphic_character(*curr)) {
                        set_simple_token(TOKEN_CHAR_LITERAL)
                    } else {
                        error_print("Character literal must be a graphical character");
                    }
                    ++curr;
                } else {
                    set_simple_token(TOKEN_SINGLE_QUOTE)
                    token->len = 1;
                }
                ++curr;
                break;
            case '"':
                curr = lex_string_literal(input_start, input_end, curr, token);
                break;
            case '*':
                token->start = curr - input_start;
                if(curr + 1 != input_end && curr[1] == '*') {
                    token->kind = TOKEN_EXP;
                    token->len = 2;
                    ++curr;
                } else {
                    token->kind = TOKEN_MULT;
                    token->len = 1;
                }
                ++curr;
                break;
            case '-':
                if(curr + 1 != input_end && curr[1] == '-') {
                    curr = skip_comment(curr, input_end);
                    done = 0;
                } else {
                    set_simple_token(TOKEN_MINUS)
                    ++curr;
                }
                break;
            case '.':
                token->start = curr - input_start;
                if(curr + 1 != input_end && curr[1] == '.') {
                    token->kind = TOKEN_DOUBLE_DOT;
                    token->len = 2;
                    ++curr;
                } else {
                    token->kind = TOKEN_DOT;
                    token->len = 1;
                }
                ++curr;
                break;
            case '/':
                token->start = curr - input_start;
                if(curr + 1 != input_end && curr[1] == '=') {
                    token->kind = TOKEN_NEQ;
                    token->len = 2;
                    ++curr;
                } else {
                    token->kind = TOKEN_DIVIDE;
                    token->len = 1;
                }
                ++curr;
                break;
            case ':':
                token->start = curr - input_start;
                if(curr + 1 != input_end && curr[1] == '=') {
                    token->kind = TOKEN_ASSIGN;
                    token->len = 2;
                    ++curr;
                } else {
                    token->kind = TOKEN_COLON;
                    token->len = 1;
                }
                ++curr;
                break;
            case '<':
                token->start = curr - input_start;
                if(curr + 1 != input_end) {
                    if(curr[1] == '=') {
                        token->kind = TOKEN_LTE;
                        token->len = 2;
                        ++curr;
                    } else if(curr[1] == '<') {
                        token->kind = TOKEN_L_LABEL_BRACKET;
                        token->len = 2;
                        ++curr;
                    } else if(curr[1] == '>') {
                        token->kind = TOKEN_BOX;
                        token->len = 2;
                        ++curr;
                    }
                }
                if(token->len != 2) {
                    token->kind = TOKEN_LT;
                    token->len = 1;
                }
                ++curr;
                break;
            case '=':
                token->start = curr - input_start;
                if(curr + 1 != input_end && curr[1] == '<') {
                    token->kind = TOKEN_ARROW;
                    token->len = 2;
                    ++curr;
                } else {
                    token->kind = TOKEN_EQ;
                    token->len = 1;
                }
                ++curr;
                break;
            case '>':
                token->start = curr - input_start;
                if(curr + 1 != input_end) {
                    if(curr[1] == '=') {
                        token->kind = TOKEN_GTE;
                        token->len = 2;
                        ++curr;
                    } else if(curr[1] == '>') {
                        token->kind = TOKEN_R_LABEL_BRACKET;
                        token->len = 2;
                        ++curr;
                    }
                }
                if(token->len != 2) {
                    token->kind = TOKEN_GT;
                    token->len = 1;
                }
                ++curr;
                break;
            default:
                if(isalpha(*curr)) {
                    curr = lex_identifier_or_keyword(input_start, input_end, curr, token);
                } else if(isdigit(*curr)) {
                    curr = lex_numeric_literal(input_start, input_end, curr, token);
                } else {
                    error_print("Unexpected character");
                }
        }
    } while(!done);

    return curr;
}
