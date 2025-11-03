#include "parser.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "ast.h"
#include "lexer.h"
#include "token.h"
#include "error.h"
#include "string_view.h"
#include "mini-gmp.h"

typedef struct {
    Declaration* decl_stack[32]; // Each item is a linked-list of Declarations
    uint8_t curr_stack_idx;
    Declaration* top_decl; // Pointer to last node of top-most decl list in decl_stack
    const char* curr;
    const char* input_start;
    const char* input_end;
    Token token;
} ParseContext;

static const char universal_integer_str[] = "universal_integer";
TypeDecl universal_int_type = {
    .kind = TYPE_UNIV_INTEGER,
    .name = { .value = universal_integer_str, .len = sizeof(universal_integer_str) }
};

static ParseContext ctx;

/* PACKAGE */
static bool parse_package_spec(PackageSpec* package_spec);
/* DECLARATIONS */
static bool parse_basic_declaration(Declaration* decl);
static bool parse_object_declaration(ObjectDecl* obj_decl);
static bool parse_full_type_declaration(TypeDecl* type_decl);
static bool parse_integer_type_definition(IntType* int_type);
static bool parse_enum_type_definition(EnumType* enum_type);
/* EXPRESSIONS */
static Expression* parse_expression(void);
static Expression* parse_numeric_literal(Expression* expr);
/* VISIBILITY */
static bool push_declaration(Declaration* decl);
static Declaration* find_declaration_in_current_region(StringView name);
static TypeDecl* find_visible_type_declaration(StringView name);
/* UTILITIES */
#define print_parse_error(...) error_print(ctx.input_start, ctx.curr, __VA_ARGS__)
static void next_token(void);
static bool expect_token(TokenKind kind);
static bool count_enum_literals(uint32_t* literal_count);
static void print_unexpected_token_error(const Token* token);
static bool prepare_num_str(const StringView* text, char* buffer, int buffer_sz);

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
                if(!package_spec->decls) {
                    package_spec->decls = decl;
                }
                if(!push_declaration(decl)) {
                    return false;
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
        case TOKEN_SUBTYPE:
            decl->kind = DECL_TYPE;
            // TODO: incomplete and private type declarations
            if(!parse_full_type_declaration(&decl->u.type)) {
                return NULL;
            }
            break;
        case TOKEN_ERROR:
            return NULL;
        default:
            print_unexpected_token_error(&ctx.token);
            return NULL;
    }
    return decl;
}

static
bool parse_object_declaration(ObjectDecl* obj_decl)
{
    // TODO: support identifier_list
    obj_decl->name = ctx.token.text;
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
        TypeDecl* type_decl = find_visible_type_declaration(ctx.token.text);
        if(!type_decl) {
            print_parse_error("Unknown type: %.*s", ctx.token.text.len, ctx.token.text.value);
            return false;
        }
        obj_decl->type = type_decl;
        next_token();
    } else if(obj_decl->is_constant && ctx.token.kind == TOKEN_ASSIGN) {
        // number_declaration
        obj_decl->type = &universal_int_type;
    } else {
        print_unexpected_token_error(&ctx.token);
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
    bool is_subtype = ctx.token.kind == TOKEN_SUBTYPE;
    next_token(); // Skip 'type'/'subtype' keyword
    if(!expect_token(TOKEN_IDENT)) {
        return false;
    }
    type_decl->name = ctx.token.text;
    next_token();
    // TODO: discriminant_part
    if(!expect_token(TOKEN_IS)) {
        return false;
    }
    next_token();
    if(is_subtype) {
        // TODO: properly parse this as a subtype_indication
        if(!expect_token(TOKEN_IDENT)) {
            return false;
        }
        TypeDecl* base_type_decl = find_visible_type_declaration(ctx.token.text);
        if(!base_type_decl) {
            print_parse_error("Unknown base type: %.*s", ctx.token.text.len, ctx.token.text.value);
            return false;
        }
        type_decl->kind = TYPE_SUBTYPE;
        type_decl->u.subtype.base = base_type_decl;
        next_token();
    } else {
        switch(ctx.token.kind) {
            case TOKEN_RANGE:
                type_decl->kind = TYPE_INTEGER;
                if(!parse_integer_type_definition(&type_decl->u.int_)) {
                    return false;
                }
                break;
            case TOKEN_L_PAREN:
                type_decl->kind = TYPE_ENUM;
                if(!parse_enum_type_definition(&type_decl->u.enum_)) {
                    return false;
                }
                break;
            case TOKEN_ERROR:
                return false;
            default:
                print_unexpected_token_error(&ctx.token);
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
bool parse_enum_type_definition(EnumType* enum_type)
{
    next_token(); // Skip '('
    uint32_t literal_count = 0;
    if(!count_enum_literals(&literal_count)) {
        return false;
    }
    enum_type->literal_count = literal_count;
    enum_type->literals = calloc(enum_type->literal_count, sizeof(Expression));
    for(uint32_t i = 0; i < enum_type->literal_count; ++i) {
        switch(ctx.token.kind) {
            case TOKEN_IDENT:
                enum_type->literals[i].kind = EXPR_ENUM_LIT;
                enum_type->literals[i].u.enum_lit = ctx.token.text;
                break;
            case TOKEN_CHAR_LITERAL:
                enum_type->literals[i].kind = EXPR_CHAR_LIT;
                enum_type->literals[i].u.char_lit = ctx.token.text.value[0]; // len is always 1
                break;
            default:
                // Should be unreachable since count_enum_literals() succeeded
                print_unexpected_token_error(&ctx.token);
                return false;
        }
        next_token();
        if(i + 1 < enum_type->literal_count) {
            if(!expect_token(TOKEN_COMMA)) {
                return false;
            }
            next_token();
        }
    }
    if(!expect_token(TOKEN_R_PAREN)) {
        return false;
    }
    next_token();
    return true;
}

static
Expression* parse_expression(void)
{
    Expression* expr = calloc(1, sizeof(Expression));
    switch(ctx.token.kind) {
        case TOKEN_NUM_LITERAL:
            expr = parse_numeric_literal(expr);
            break;
        case TOKEN_CHAR_LITERAL:
            expr->kind = EXPR_CHAR_LIT;
            expr->u.char_lit = ctx.token.text.value[0];
            next_token();
            break;
        case TOKEN_IDENT:
            expr->kind = EXPR_ENUM_LIT;
            expr->u.enum_lit = ctx.token.text;
            next_token();
            break;
        case TOKEN_ERROR:
            return NULL;
        default:
            print_unexpected_token_error(&ctx.token);
            return NULL;
    }
    return expr;
}

// TODO: use pool since likely to reuse same numeric literals
static
Expression* parse_numeric_literal(Expression* expr)
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
    expr->kind = EXPR_INT_LIT;
    if(mpz_init_set_str(expr->u.int_lit, num_buffer, (int)ctx.token.u.int_lit.base) < 0) {
        print_parse_error("Invalid numeric literal: '%.*s' for base %u", ctx.token.text.len, ctx.token.text.value, ctx.token.u.int_lit.base);
        return NULL;
    }
    next_token();
    return expr;
}

static
bool push_declaration(Declaration* decl)
{
    switch(decl->kind) {
        case DECL_OBJECT:
            // TODO: print line number of previous definition
            if(find_declaration_in_current_region(decl->u.object.name) != NULL) {
                print_parse_error("Redefinition of '%.*s' within same declarative region", decl->u.object.name.len, decl->u.object.name.value);
                return false;
            }
            break;
        case DECL_TYPE:
            // TODO: print line number of previous definition
            if(find_declaration_in_current_region(decl->u.type.name) != NULL) {
                print_parse_error("Redefinition of '%.*s' within same declarative region", decl->u.type.name.len, decl->u.type.name.value);
                return false;
            }
            break;
        default:
            assert(false && "Unhandled declaration type");
    }

    if(ctx.top_decl) {
        ctx.top_decl->next = decl;
    } else {
        // First decl in the current region
        ctx.decl_stack[ctx.curr_stack_idx] = decl;
    }
    ctx.top_decl = decl;
    return true;
}

static
bool string_view_equal(const StringView* a, const StringView* b)
{
    if(a->len != b->len) {
        return false;
    }
    return memcmp(a->value, b->value, a->len) == 0;
}

static
TypeDecl* find_visible_type_declaration(StringView name)
{
    for(int stack_idx = ctx.curr_stack_idx; stack_idx >= 0; --stack_idx) {
        Declaration* decl_list = ctx.decl_stack[stack_idx];
        for(Declaration* decl = decl_list; decl != NULL; decl = decl->next) {
            if(decl->kind == DECL_TYPE && string_view_equal(&decl->u.type.name, &name)) {
                return &decl->u.type;
            }
        }
    }
    return NULL;
}

static
Declaration* find_declaration_in_current_region(StringView name)
{
    Declaration* decl_list = ctx.decl_stack[ctx.curr_stack_idx];
    for(Declaration* decl = decl_list; decl != NULL; decl = decl->next) {
        switch(decl->kind) {
            case DECL_TYPE:
                if(string_view_equal(&decl->u.type.name, &name)) {
                    return decl;
                }
                break;
            case DECL_OBJECT:
                if(string_view_equal(&decl->u.object.name, &name)) {
                    return decl;
                }
                break;
            default:
                assert(false && "Unhandled declaration type");
        }
    }
    return NULL;
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
        print_unexpected_token_error(&ctx.token);
        return false;
    }
    return true;
}

static
bool count_enum_literals(uint32_t* literal_count)
{
    Token token = ctx.token;
    const char* curr = ctx.curr;
    while(token.kind != TOKEN_R_PAREN) {
        switch(token.kind) {
            case TOKEN_IDENT:
            case TOKEN_CHAR_LITERAL:
                if(*literal_count == UINT32_MAX) {
                    error_print(ctx.input_start, curr, "Enumeration type has too many literals to be processed (max supported is 2**32-1 literals)");
                    return false;
                }
                ++*literal_count;
                break;
            case TOKEN_COMMA:
                break;
            case TOKEN_ERROR:
                return false;
            default:
                print_unexpected_token_error(&token);
                return false;
        }
        curr = lexer_parse_token(ctx.input_start, ctx.input_end, curr, &token);
    }
    return true;
}

static
void print_unexpected_token_error(const Token* token)
{
    StringView token_str = token_to_str(token);
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
