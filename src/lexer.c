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
#include "lexer.h"
#include "token.h"
#include "error.h"
#include "lexer_table.c"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#include "keywords.c"
#pragma GCC diagnostic pop

TokenKind lexer_lookahead(const char* input_end, const char** curr)
{
    Token next_token;
    *curr = lexer_parse_token(input_end, *curr, &next_token);
    return next_token.kind;
}

const char* lexer_parse_token(const char* input_end, const char* curr, Token* token)
{
    State state = STATE_START;
    // Skip whitespace and comments
    while(curr != input_end) {
        Class class_ = class_table[(uint8_t)*curr];
        state = skip_table[state - TOKEN_NUM_TOKEN_KINDS][class_];
        if(*curr == '\n') {
            ++token->line_num;
        }
        if(state < TOKEN_NUM_TOKEN_KINDS) {
            break;
        }
        ++curr;
    }
    // Since comments start with "--", state machine will consume first "-", then halt
    // if no second "-". Back up curr so we can parse the TOKEN_MINUS
    if(state == TOKEN_MINUS) {
        --curr;
    }

    state = STATE_START;
    const char* token_start = curr;
    while(curr != input_end && state >= TOKEN_NUM_TOKEN_KINDS) {
        Class class_ = class_table[(uint8_t)*curr];
        state = state_table[state - TOKEN_NUM_TOKEN_KINDS][class_];
        curr += move_table[state];
    }

    if(curr == input_end) {
        if(state == STATE_START) {
            token->kind = TOKEN_EOF;
        } else {
            error_print(token->line_num, "Unexpected end of file");
            token->kind = TOKEN_ERROR;
            error_exit();
        }
    } else {
        token->kind = (TokenKind)state;
        switch(token->kind) {
            case TOKEN_IDENT: {
                const struct keyword_token* keyword = is_keyword(token_start, curr - token_start);
                if(keyword) {
                    token->kind = keyword->kind;
                    const char* next;
                    switch(keyword->kind) {
                        case TOKEN_AND:
                            next = curr;
                            if(lexer_lookahead(input_end, &next) == TOKEN_THEN) {
                                token->kind = TOKEN_AND_THEN;
                                curr = next;
                            }
                            break;
                        case TOKEN_OR:
                            next = curr;
                            if(lexer_lookahead(input_end, &next) == TOKEN_ELSE) {
                                token->kind = TOKEN_OR_ELSE;
                                curr = next;
                            }
                            break;
                        case TOKEN_NOT:
                            next = curr;
                            if(lexer_lookahead(input_end, &next) == TOKEN_IN) {
                                token->kind = TOKEN_NOT_IN;
                                curr = next;
                            }
                            break;
                        case TOKEN_IN:
                            next = curr;
                            if(lexer_lookahead(input_end, &next) == TOKEN_OUT) {
                                token->kind = TOKEN_IN_OUT;
                                curr = next;
                            }
                            break;
                        default:
                            break;
                    }
                } else {
                    token->kind = TOKEN_IDENT;
                }
                token->text.value = token_start;
                token->text.len = curr - token_start;
                break;
            }
            case TOKEN_LABEL:
                token->text.value = token_start + 2;
                token->text.len = curr - token_start - 4;
                break;
            case TOKEN_SINGLE_QUOTE:
                token->text.value = token_start;
                token->text.len = 1;
                curr = token_start + 1;
                break;
            case TOKEN_CHAR_LITERAL:
            case TOKEN_STRING_LITERAL:
                token->text.value = token_start + 1;
                token->text.len = curr - token_start - 2;
                break;
            case TOKEN_ERROR:
                error_print(token->line_num, "Unexpected character: '%c'", *curr);
                error_exit();
            default:
                token->text.value = token_start;
                token->text.len = curr - token_start;
                break;
        }
    }
    return curr;
}
