#include "parser.h"
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

_Bool expect_token(ParseContext* ctx, TokenKind kind)
{
    ctx->curr = lexer_parse_token(ctx->input_start, ctx->input_end, ctx->curr, &ctx->token);
    if(ctx->token.kind == TOKEN_ERROR) {
        return 0;
    }
    if(ctx->token.kind != kind) {
        error_print("Unexpected token");
        return 0;
    }
    return 1;
}

static
_Bool parse_integer_type_definition(ParseContext* ctx, IntType* int_type)
{
    // TODO: support more complex expressions
    if(!expect_token(ctx, TOKEN_NUM_LITERAL)) {
        return 0;
    }
    // TODO: min
    mpz_init_set_ui(int_type->range.lower_bound, 0);
    if(!expect_token(ctx, TOKEN_DOUBLE_DOT)) {
        return 0;
    }
    // TODO: support more complex expressions
    if(!expect_token(ctx, TOKEN_NUM_LITERAL)) {
        return 0;
    }
    // TODO: max
    mpz_init_set_ui(int_type->range.upper_bound, 128);
    return 1;
}

static
_Bool parse_full_type_declaration(ParseContext* ctx, TypeDecl* type_decl)
{
    ctx->curr = lexer_parse_token(ctx->input_start, ctx->input_end, ctx->curr, &ctx->token);
    switch(ctx->token.kind) {
        case TOKEN_IDENT:
            string_view_create(ctx->input_start, &ctx->token, &type_decl->name);
            // TODO: discriminant_part
            if(!expect_token(ctx, TOKEN_IS)) {
                return 0;
            }
            ctx->curr = lexer_parse_token(ctx->input_start, ctx->input_end, ctx->curr, &ctx->token);
            switch(ctx->token.kind) {
                case TOKEN_RANGE:
                    type_decl->type = calloc(1, sizeof(Type));
                    type_decl->type->kind = TYPE_INTEGER;
                    if(!parse_integer_type_definition(ctx, &type_decl->type->u.int_type)) {
                        return 0;
                    }
                    break;
                default:
                    error_print("Unexpected token");
                    return 0;
            }
            break;
        default:
            error_print("Unexpected token");
            return 0;
    }
    if(!expect_token(ctx, TOKEN_SEMICOLON)) {
        return 0;
    }
    return 1;
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
        default:
            error_print("Unexpected token");
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
