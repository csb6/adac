#include "parser.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
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

static
void print_unexpected_token_error(ParseContext* ctx)
{
    StringView token_str = token_to_str(ctx->input_start, &ctx->token);
    error_print(ctx->input_start, ctx->curr, "Unexpected token: '%.*s'", token_str.len, token_str.value);
}

bool expect_token(ParseContext* ctx, TokenKind kind)
{
    ctx->curr = lexer_parse_token(ctx->input_start, ctx->input_end, ctx->curr, &ctx->token);
    if(ctx->token.kind == TOKEN_ERROR) {
        return false;
    }
    if(ctx->token.kind != kind) {
        print_unexpected_token_error(ctx);
        return false;
    }
    return true;
}

static
void prepare_num_str(const StringView* text, char* buffer, int buffer_sz)
{
    const char* text_end = text->value + text->len;
    const char* buffer_end = buffer + buffer_sz - 1; // Leave space for null terminator
    const char* c = text->value;
    char* b = buffer;
    while(c < text_end && b < buffer_end) {
        if(isalnum(*c)) {
            *b = *c;
            ++b;
        }
        ++c;
    }
    *b = '\0';
}

static
bool parse_integer_type_definition(ParseContext* ctx, IntType* int_type)
{
    char num_buffer[128];

    // Lower bound
    // TODO: support more complex expressions
    if(!expect_token(ctx, TOKEN_NUM_LITERAL)) {
        return false;
    }
    num_buffer[0] = '\0';
    StringView num_str_view = token_to_str(ctx->input_start, &ctx->token);
    prepare_num_str(&num_str_view, num_buffer, sizeof(num_buffer));
    if(mpz_init_set_str(int_type->range.lower_bound, num_buffer, (int)ctx->token.u.num_base) < 0) {
        error_print(ctx->input_start, ctx->curr, "Invalid numeric literal: '%.*s' for base %u", num_str_view.len, num_str_view.value, ctx->token.u.num_base);
        return false;
    }

    if(!expect_token(ctx, TOKEN_DOUBLE_DOT)) {
        return false;
    }

    // Upper bound
    // TODO: support more complex expressions
    if(!expect_token(ctx, TOKEN_NUM_LITERAL)) {
        return false;
    }
    num_buffer[0] = '\0';
    num_str_view = token_to_str(ctx->input_start, &ctx->token);
    prepare_num_str(&num_str_view, num_buffer, sizeof(num_buffer));
    if(mpz_init_set_str(int_type->range.upper_bound, num_buffer, (int)ctx->token.u.num_base) < 0) {
        error_print(ctx->input_start, ctx->curr, "Invalid numeric literal: '%.*s' for base %u", num_str_view.len, num_str_view.value, ctx->token.u.num_base);
        return false;
    }
    return true;
}

static
bool parse_full_type_declaration(ParseContext* ctx, TypeDecl* type_decl)
{
    ctx->curr = lexer_parse_token(ctx->input_start, ctx->input_end, ctx->curr, &ctx->token);
    switch(ctx->token.kind) {
        case TOKEN_IDENT:
            type_decl->name.value = ctx->input_start + ctx->token.start;
            type_decl->name.len = ctx->token.len;
            // TODO: discriminant_part
            if(!expect_token(ctx, TOKEN_IS)) {
                return false;
            }
            ctx->curr = lexer_parse_token(ctx->input_start, ctx->input_end, ctx->curr, &ctx->token);
            switch(ctx->token.kind) {
                case TOKEN_RANGE:
                    type_decl->type = calloc(1, sizeof(Type));
                    type_decl->type->kind = TYPE_INTEGER;
                    if(!parse_integer_type_definition(ctx, &type_decl->type->u.int_type)) {
                        return false;
                    }
                    break;
                case TOKEN_ERROR:
                    return false;
                default:
                    print_unexpected_token_error(ctx);
                    return false;
            }
            break;
        default:
            print_unexpected_token_error(ctx);
            return false;
    }
    if(!expect_token(ctx, TOKEN_SEMICOLON)) {
        return false;
    }
    return true;
}

Declaration* parse_basic_declaration(ParseContext* ctx)
{
    Declaration* decl = calloc(1, sizeof(Declaration));
    ctx->curr = lexer_parse_token(ctx->input_start, ctx->input_end, ctx->curr, &ctx->token);
    switch(ctx->token.kind) {
        case TOKEN_TYPE:
            decl->kind = DECL_FULL_TYPE;
            // TODO: incomplete and private type declarations
            if(!parse_full_type_declaration(ctx, &decl->u.type)) {
                return NULL;
            }
            break;
        case TOKEN_ERROR:
            return NULL;
        default:
            print_unexpected_token_error(ctx);
            return NULL;
    }
    return decl;
}

void parser_parse(const char* input_start, const char* input_end)
{
    ParseContext ctx = {0};
    ctx.curr = input_start;
    ctx.input_start = input_start;
    ctx.input_end = input_end;
    while(ctx.curr < input_end) {
        Declaration* decl = parse_basic_declaration(&ctx);
        if(decl == NULL) {
            return;
        }
    }
}
