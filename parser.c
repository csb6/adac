#include "parser.h"
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
bool parse_integer_type_definition(ParseContext* ctx, IntType* int_type)
{
    // TODO: support more complex expressions
    if(!expect_token(ctx, TOKEN_NUM_LITERAL)) {
        return false;
    }
    // TODO: min
    mpz_init_set_ui(int_type->range.lower_bound, 0);
    if(!expect_token(ctx, TOKEN_DOUBLE_DOT)) {
        return false;
    }
    // TODO: support more complex expressions
    if(!expect_token(ctx, TOKEN_NUM_LITERAL)) {
        return false;
    }
    // TODO: max
    mpz_init_set_ui(int_type->range.upper_bound, 128);
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
