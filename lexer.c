#include "lexer.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#include "keywords.c"
#pragma GCC diagnostic pop

#define set_simple_token(token_kind) \
    token->kind = token_kind; \
    token->start = curr - input_start; \
    token->len = 1;

static
const char* lex_numeric_literal(const char* input_start, const char* input_end, const char* curr, Token* token)
{
    const char* token_start = curr;
    ++curr;
    while(curr != input_end && (isdigit(*curr) || *curr == '_')) {
        ++curr;
    }
    if(curr != input_end) {
        if(*curr == '.') {
            ++curr;
            while(curr != input_end && (isdigit(*curr) || *curr == '_')) {
                ++curr;
            }
            if(curr == input_end) {
                fprintf(stderr, "Unexpected end of decimal literal\n");
                return curr;
            }
        }
        if(*curr == 'e' || *curr == 'E') {
            ++curr;
            if(curr == input_end) {
                fprintf(stderr, "Unexpected end of exponent\n");
                return curr;
            }
            if(*curr == '+' || *curr == '-') {
                ++curr;
                if(curr == input_end) {
                    fprintf(stderr, "Unexpected end of exponent\n");
                return curr;
                }
            }
            if(!isdigit(*curr)) {
                fprintf(stderr, "Unexpected end of exponent\n");
                return curr;
            }
            ++curr;
            while(curr != input_end && (isdigit(*curr) || *curr == '_')) {
                ++curr;
            }
        }
    }
    // TODO: based literals
    token->kind = TOKEN_NUM_LITERAL;
    token->start = token_start - input_start;
    token->len = curr - token_start;
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
    if(keyword && (curr == input_end || !(isalpha(*curr) || *curr == '_' || isdigit(*curr)))) {
        token->kind = keyword->kind;
    } else {
        token->kind = TOKEN_IDENT;
        while(curr != input_end && (isalpha(*curr) || *curr == '_' || isdigit(*curr))) {
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
                    if(isgraph(*curr) || *curr == ' ') {
                        set_simple_token(TOKEN_CHAR_LITERAL)
                    } else {
                        fprintf(stderr, "Character literal must be a graphical character\n");
                    }
                    ++curr;
                } else {
                    set_simple_token(TOKEN_SINGLE_QUOTE)
                    token->len = 1;
                }
                ++curr;
                break;
            case '"':
                // TODO: string literal
                set_simple_token(TOKEN_DOUBLE_QUOTE)
                ++curr;
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
                    fprintf(stderr, "Unexpected character: %c\n", *curr);
                }
        }
    } while(!done);

    return curr;
}
