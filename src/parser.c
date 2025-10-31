#include "parser.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "lexer.h"
#include "token.h"
#include "error.h"
#include "string_view.h"
#include "mini-gmp.h"

typedef struct {
    const char* curr;
    const char* input_start;
    const char* input_end;
    Token token;
} ParseContext;

static ParseContext ctx;

#define print_parse_error(...) error_print(ctx.input_start, ctx.curr, __VA_ARGS__)

static
void next_token(void)
{
    ctx.curr = lexer_parse_token(ctx.input_start, ctx.input_end, ctx.curr, &ctx.token);
}

static
void print_unexpected_token_error(void)
{
    StringView token_str = token_to_str(ctx.input_start, &ctx.token);
    print_parse_error("Unexpected token: '%.*s'", token_str.len, token_str.value);
}

bool expect_token(TokenKind kind)
{
    next_token();
    if(ctx.token.kind == TOKEN_ERROR) {
        return false;
    }
    if(ctx.token.kind != kind) {
        print_unexpected_token_error();
        return false;
    }
    return true;
}

static
bool prepare_num_str(const StringView* text, char* buffer, int buffer_sz)
{
    const char* text_end = text->value + text->len;
    const char* buffer_end = buffer + buffer_sz - 1; // Leave space for null terminator
    const char* c = text->value;
    char* b = buffer;
    while(c < text_end) {
        if(isalnum(*c)) {
            *b = *c;
            ++b;
            if(b >= buffer_end) {
                return false;
            }
        }
        ++c;
    }
    *b = '\0';
    return true;
}

static
bool parse_integer_type_definition(IntType* int_type)
{
    char num_buffer[128];

    // Lower bound
    // TODO: support more complex expressions
    if(!expect_token(TOKEN_NUM_LITERAL)) {
        return false;
    }
    num_buffer[0] = '\0';
    StringView num_str_view = token_to_str(ctx.input_start, &ctx.token);
    if(!prepare_num_str(&num_str_view, num_buffer, sizeof(num_buffer))) {
        print_parse_error("Numeric literal is too long to be processed (max is 127 characters in length)");
        return false;
    }
    if(mpz_init_set_str(int_type->range.lower_bound, num_buffer, (int)ctx.token.u.num_base) < 0) {
        print_parse_error("Invalid numeric literal: '%.*s' for base %u", num_str_view.len, num_str_view.value, ctx.token.u.num_base);
        return false;
    }

    if(!expect_token(TOKEN_DOUBLE_DOT)) {
        return false;
    }

    // Upper bound
    // TODO: support more complex expressions
    if(!expect_token(TOKEN_NUM_LITERAL)) {
        return false;
    }
    num_buffer[0] = '\0';
    num_str_view = token_to_str(ctx.input_start, &ctx.token);
    prepare_num_str(&num_str_view, num_buffer, sizeof(num_buffer));
    if(mpz_init_set_str(int_type->range.upper_bound, num_buffer, (int)ctx.token.u.num_base) < 0) {
        print_parse_error("Invalid numeric literal: '%.*s' for base %u", num_str_view.len, num_str_view.value, ctx.token.u.num_base);
        return false;
    }
    return true;
}

static
bool parse_full_type_declaration(TypeDecl* type_decl)
{
    next_token();
    switch(ctx.token.kind) {
        case TOKEN_IDENT:
            type_decl->name.value = ctx.input_start + ctx.token.start;
            type_decl->name.len = ctx.token.len;
            // TODO: discriminant_part
            if(!expect_token(TOKEN_IS)) {
                return false;
            }
            next_token();
            switch(ctx.token.kind) {
                case TOKEN_RANGE:
                    type_decl->type = calloc(1, sizeof(Type));
                    type_decl->type->kind = TYPE_INTEGER;
                    if(!parse_integer_type_definition(&type_decl->type->u.int_type)) {
                        return false;
                    }
                    break;
                case TOKEN_ERROR:
                    return false;
                default:
                    print_unexpected_token_error();
                    return false;
            }
            break;
        case TOKEN_ERROR:
            return false;
        default:
            print_unexpected_token_error();
            return false;
    }
    return expect_token(TOKEN_SEMICOLON);
}

Declaration* parse_basic_declaration(void)
{
    Declaration* decl = calloc(1, sizeof(Declaration));
    next_token();
    switch(ctx.token.kind) {
        case TOKEN_TYPE:
            decl->kind = DECL_FULL_TYPE;
            // TODO: incomplete and private type declarations
            if(!parse_full_type_declaration(&decl->u.type)) {
                return NULL;
            }
            break;
        case TOKEN_ERROR:
            return NULL;
        default:
            print_unexpected_token_error();
            return NULL;
    }
    return decl;
}

void parser_parse(const char* input_start, const char* input_end)
{
    memset(&ctx, 0, sizeof(ctx));
    ctx.curr = input_start;
    ctx.input_start = input_start;
    ctx.input_end = input_end;
    while(ctx.curr < input_end) {
        Declaration* decl = parse_basic_declaration();
        if(decl == NULL) {
            return;
        }
    }
}
