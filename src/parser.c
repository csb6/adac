#include "parser.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
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

Type universal_int_type = {.kind = TYPE_UNIV_INTEGER};

static ParseContext ctx;

/* PACKAGE */
static bool parse_package_spec(PackageSpec* package_spec);
/* DECLARATIONS */
static bool parse_basic_declaration(Declaration* decl);
static bool parse_object_declaration(ObjectDecl* obj_decl);
static bool parse_full_type_declaration(TypeDecl* type_decl);
static bool parse_integer_type_definition(IntType* int_type);
/* EXPRESSIONS */
static Expression* parse_expression(void);
static Expression* parse_numeric_literal(void);
/* UTILITIES */
#define print_parse_error(...) error_print(ctx.input_start, ctx.curr, __VA_ARGS__)
static void next_token(void);
static bool expect_token(TokenKind kind);
static void print_unexpected_token_error(void);
static bool prepare_num_str(const StringView* text, char* buffer, int buffer_sz);
static void print_declaration(const Declaration* decl);
static void print_type(const Type* type);
static void print_expression(const Expression* expr);

PackageSpec* parser_parse(const char* input_start, const char* input_end)
{
    memset(&ctx, 0, sizeof(ctx));
    PackageSpec* package_spec = calloc(1, sizeof(PackageSpec));
    ctx.curr = input_start;
    ctx.input_start = input_start;
    ctx.input_end = input_end;

    next_token();
    if(!parse_package_spec(package_spec)) {
        return NULL;
    }
    return package_spec;
}

static
bool parse_package_spec(PackageSpec* package_spec)
{
    if(!expect_token(TOKEN_PACKAGE)) {
        return false;
    }
    next_token();

    if(!expect_token(TOKEN_IDENT)) {
        return false;
    }
    package_spec->name = ctx.token.text;
    next_token();

    if(!expect_token(TOKEN_IS)) {
        return false;
    }
    next_token();

    Declaration* prev_decl = NULL;
    bool done = false;
    while(!done) {
        switch(ctx.token.kind) {
            case TOKEN_END:
                done = true;
                next_token();
                break;
            case TOKEN_ERROR:
                return false;
            default: {
                Declaration* decl = calloc(1, sizeof(Declaration));
                // TODO: parse representation_clause/use_clause here too
                if(!parse_basic_declaration(decl)) {
                    return false;
                }
                if(prev_decl) {
                    prev_decl->next = decl;
                    prev_decl = decl;
                } else {
                    package_spec->decls = decl;
                    prev_decl = decl;
                }
            }
        }
    }

    // TODO: support optional trailing name
    if(!expect_token(TOKEN_SEMICOLON)) {
        return false;
    }
    next_token();

    return expect_token(TOKEN_EOF);
}

static
bool parse_basic_declaration(Declaration* decl)
{
    switch(ctx.token.kind) {
        case TOKEN_IDENT:
            decl->kind = DECL_OBJECT;
            if(!parse_object_declaration(&decl->u.object)) {
                return NULL;
            }
            break;
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

static
bool parse_object_declaration(ObjectDecl* obj_decl)
{
    // TODO: support identifier_list
    obj_decl->identifier = ctx.token.text;
    next_token();
    if(!expect_token(TOKEN_COLON)) {
        return false;
    }

    next_token();
    if(ctx.token.kind == TOKEN_CONSTANT) {
        obj_decl->is_constant = true;
        next_token();
    }

    if(ctx.token.kind == TOKEN_IDENT) {
        // Type name
        // TODO: properly parse this as a subtype_indication or constrained_array_definition
        // TODO: use pool to reduce allocations for identical placeholders
        obj_decl->type = calloc(1, sizeof(Type));
        obj_decl->type->kind = TYPE_PLACEHOLDER;
        obj_decl->type->u.placeholder_name = ctx.token.text;
        next_token();
    } else if(obj_decl->is_constant && ctx.token.kind == TOKEN_ASSIGN) {
        // number_declaration
        obj_decl->type = &universal_int_type;
    } else {
        print_unexpected_token_error();
        return false;
    }

    if(ctx.token.kind == TOKEN_ASSIGN) {
        next_token();
        obj_decl->init_expr = parse_expression();
        if(!obj_decl->init_expr) {
            return false;
        }
    }

    if(!expect_token(TOKEN_SEMICOLON)) {
        return false;
    }
    next_token();
    return true;
}

static
bool parse_full_type_declaration(TypeDecl* type_decl)
{
    next_token(); // Skip 'type' keyword
    switch(ctx.token.kind) {
        case TOKEN_IDENT:
            type_decl->name = ctx.token.text;
            next_token();
            // TODO: discriminant_part
            if(!expect_token(TOKEN_IS)) {
                return false;
            }
            next_token();
            switch(ctx.token.kind) {
                case TOKEN_RANGE:
                    type_decl->type = calloc(1, sizeof(Type));
                    type_decl->type->kind = TYPE_INTEGER;
                    if(!parse_integer_type_definition(&type_decl->type->u.int_)) {
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
    if(!expect_token(TOKEN_SEMICOLON)) {
        return false;
    }
    next_token();
    return true;
}

static
bool parse_integer_type_definition(IntType* int_type)
{
    next_token(); // Skip 'range' keyword
    int_type->range.lower_bound = parse_expression();
    if(!int_type->range.lower_bound) {
        return false;
    }

    if(!expect_token(TOKEN_DOUBLE_DOT)) {
        return false;
    }
    next_token();

    int_type->range.upper_bound = parse_expression();
    return int_type->range.upper_bound != NULL;
}

static
Expression* parse_expression(void)
{
    switch(ctx.token.kind) {
        case TOKEN_NUM_LITERAL:
            return parse_numeric_literal();
        case TOKEN_ERROR:
            return NULL;
        default:
            print_unexpected_token_error();
            return NULL;
    }
}

// TODO: use pool since likely to reuse same numeric literals
static
Expression* parse_numeric_literal(void)
{
    char num_buffer[128];

    if(ctx.token.u.int_lit.has_fraction) {
        print_parse_error("TODO: support non-integer numeric literals");
        return NULL;
    }

    num_buffer[0] = '\0';
    if(!prepare_num_str(&ctx.token.text, num_buffer, sizeof(num_buffer))) {
        print_parse_error("Numeric literal is too long to be processed (max supported is 127 characters)");
        return NULL;
    }
    Expression* expr = calloc(1, sizeof(Expression));
    expr->kind = EXPR_INT_LIT;
    if(mpz_init_set_str(expr->u.int_lit.value, num_buffer, (int)ctx.token.u.int_lit.base) < 0) {
        print_parse_error("Invalid numeric literal: '%.*s' for base %u", ctx.token.text.len, ctx.token.text.value, ctx.token.u.int_lit.base);
        return NULL;
    }
    next_token();
    return expr;
}

static
void next_token(void)
{
    ctx.curr = lexer_parse_token(ctx.input_start, ctx.input_end, ctx.curr, &ctx.token);
}

static
bool expect_token(TokenKind kind)
{
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
void print_unexpected_token_error(void)
{
    StringView token_str = token_to_str(&ctx.token);
    print_parse_error("Unexpected token: '%.*s'", token_str.len, token_str.value);
}

static
bool prepare_num_str(const StringView* text, char* buffer, int buffer_sz)
{
    const char* text_end = text->value + text->len;
    const char* buffer_end = buffer + buffer_sz - 1; // Leave space for null terminator
    const char* c = text->value;
    char* b = buffer;
    while(c < text_end) {
        // TODO: handle exponent notation
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

void print_package_spec(const PackageSpec* package_spec)
{
    printf("Package: %.*s\n", package_spec->name.len, package_spec->name.value);
    printf("Declarations:\n");
    for(const Declaration* decl = package_spec->decls; decl != NULL; decl = decl->next) {
        printf("  ");
        print_declaration(decl);
        putchar('\n');
    }
}

static
void print_declaration(const Declaration* decl)
{
    switch(decl->kind) {
        case DECL_FULL_TYPE:
            printf("Type declaration (name: %.*s, type: ", decl->u.type.name.len, decl->u.type.name.value);
            print_type(decl->u.type.type);
            putchar(')');
            break;
        case DECL_OBJECT:
            printf("Object declaration (%.*s: ", decl->u.object.identifier.len, decl->u.object.identifier.value);
            if(decl->u.object.is_constant) {
                printf("constant ");
            }
            print_type(decl->u.object.type);
            if(decl->u.object.init_expr) {
                printf(" := ");
                print_expression(decl->u.object.init_expr);
            }
            putchar(')');
            break;
        default:
            printf("Unknown declaration");
    }
}

static
void print_type(const Type* type)
{
    switch(type->kind) {
        case TYPE_PLACEHOLDER:
            printf("%.*s (placeholder)", type->u.placeholder_name.len, type->u.placeholder_name.value);
            break;
        case TYPE_UNIV_INTEGER:
            printf("universal integer");
            break;
        case TYPE_INTEGER:
            printf("integer (range: [");
            print_expression(type->u.int_.range.lower_bound);
            printf(", ");
            print_expression(type->u.int_.range.upper_bound);
            printf("])");
            break;
        default:
            printf("Unhandled type");
    }
}

static
void print_expression(const Expression* expr)
{
    switch(expr->kind) {
        case EXPR_INT_LIT: {
            char* num_str = mpz_get_str(NULL, 10, expr->u.int_lit.value);
            printf("%s", num_str);
            free(num_str);
            break;
        }
        default:
            printf("Unhandled expression");
    }
}
