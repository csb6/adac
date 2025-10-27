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

const char* lexer_parse_token(const char* input_start, const char* input_end, const char* curr, Token* token)
{
    typedef enum {
        STATE_START, STATE_END, STATE_COMMENT, STATE_STRING_LIT,
        STATE_NUM_LIT
    } State;

    memset(token, 0, sizeof(*token));
    token->kind = TOKEN_ERROR;

    State state = STATE_START;
    do {
        switch(state) {
            case STATE_START:
                // Skip leading whitespace
                while(curr < input_end && isspace(*curr)) {
                    ++curr;
                }
                if(curr == input_end) {
                    set_simple_token(TOKEN_EOF)
                    state = STATE_END;
                    continue;
                }
                switch(*curr) {
                    case '&':
                        set_simple_token(TOKEN_AMP)
                        state = STATE_END;
                        ++curr;
                        break;
                    case '(':
                        set_simple_token(TOKEN_L_PAREN)
                        state = STATE_END;
                        ++curr;
                        break;
                    case ')':
                        set_simple_token(TOKEN_R_PAREN)
                        state = STATE_END;
                        ++curr;
                        break;
                    case '+':
                        set_simple_token(TOKEN_PLUS)
                        state = STATE_END;
                        ++curr;
                        break;
                    case ',':
                        set_simple_token(TOKEN_COMMA)
                        state = STATE_END;
                        ++curr;
                        break;
                    case ';':
                        set_simple_token(TOKEN_SEMICOLON)
                        state = STATE_END;
                        ++curr;
                        break;
                    case '|':
                        set_simple_token(TOKEN_BAR)
                        state = STATE_END;
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
                        state = STATE_END;
                        ++curr;
                        break;
                    case '"':
                        // TODO: string literal
                        set_simple_token(TOKEN_DOUBLE_QUOTE)
                        state = STATE_END;
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
                        state = STATE_END;
                        ++curr;
                        break;
                    case '-':
                        if(curr + 1 != input_end && curr[1] == '-') {
                            state = STATE_COMMENT;
                            ++curr;
                        } else {
                            set_simple_token(TOKEN_MINUS)
                            state = STATE_END;
                        }
                        ++curr;
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
                        state = STATE_END;
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
                        state = STATE_END;
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
                        state = STATE_END;
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
                        state = STATE_END;
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
                        state = STATE_END;
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
                        state = STATE_END;
                        ++curr;
                        break;
                    default:
                        if(isalpha(*curr)) {
                            const char* token_start = curr;
                            token->start = curr - input_start;
                            ++curr;
                            while(curr != input_end && isalpha(*curr)) {
                                ++curr;
                            }
                            const struct keyword_token* keyword = is_keyword(token_start, curr - token_start);
                            if(keyword) {
                                token->kind = keyword->kind;
                            } else {
                                token->kind = TOKEN_IDENT;
                                while(curr != input_end && (isalpha(*curr) || *curr == '_' || isdigit(*curr))) {
                                    ++curr;
                                }
                            }
                            token->len = curr - token_start;
                        } else if(isdigit(*curr)) {
                            token->kind = TOKEN_NUM_LITERAL;
                            token->start = curr - input_start;
                            ++curr;
                            while(curr != input_end && isdigit(*curr)) {
                                ++curr;
                            }
                            // TODO: based literals
                            // TODO: decimal literals
                            token->len = (curr - input_start) - token->start;
                        } else {
                            fprintf(stderr, "Unexpected character: %c\n", *curr);
                        }
                        state = STATE_END;
                        break;
                }
                break;
            case STATE_STRING_LIT:
                break;
            case STATE_NUM_LIT:
                break;
            case STATE_COMMENT:
                curr += 2; // Skip over '--'
                while(curr != input_end && *curr != '\n') {
                    ++curr;
                }
                state = STATE_START;
                break;
            case STATE_END:
                break;
        }
    } while(state != STATE_END);

    return curr;
}
