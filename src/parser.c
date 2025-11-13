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
#include "static_assert.h"
#include "string_view.h"
#include "mini-gmp.h"

typedef struct {
    Declaration* first; // linked list of decls
    Declaration* last; // Pointer to last node of decls
} DeclList;

typedef struct {
    DeclList region_stack[32];
    uint8_t curr_region_idx;
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
static void parse_package_spec(PackageSpec* package_spec);
/* DECLARATIONS */
static void parse_basic_declaration(Declaration* decl);
static void parse_object_declaration(Declaration* decl, bool is_param);
static void parse_type_declaration(Declaration* decl);
static void parse_integer_type_definition(IntType* int_type);
static void parse_enum_type_definition(EnumType* enum_type);
static void parse_subprogram_declaration(Declaration* decl);
static void parse_parameters(Declaration** params);
/* EXPRESSIONS */
static Expression* parse_expression(void);
static Expression* parse_expression_1(uint8_t min_precedence);
static Expression* parse_primary_expression(void);
static Expression* parse_numeric_literal(void);
/* VISIBILITY */
static void begin_region(void);
static void end_region(void);
static void push_declaration(Declaration* decl);
static Declaration* find_declaration_in_current_region(StringView name);
static TypeDecl* find_visible_type_declaration(StringView name);
/* UTILITIES */
#define curr_region(ctx) ctx.region_stack[ctx.curr_region_idx]
#define print_parse_error(...) error_print(ctx.input_start, ctx.curr, __VA_ARGS__)
#define cnt_of_array(arr) (sizeof(arr) / sizeof(arr[0]))
static void next_token(void);
static void expect_token(TokenKind kind);
static bool is_binary_op(TokenKind token);
static bool is_unary_op(TokenKind token);
static bool is_overloadable_op(TokenKind token);
static void check_op_arity(const Declaration* params, const Token* op_token);
static uint32_t count_enum_literals(void);
static void print_unexpected_token_error(const Token* token);
static bool prepare_num_str(const StringView* text, char* buffer, int buffer_sz);
static bool identifier_equal(const StringView* a, const StringView* b);
static void append_decl(DeclList* decl_list, Declaration* decl);

PackageSpec* parser_parse(const char* input_start, const char* input_end)
{
    memset(&ctx, 0, sizeof(ctx));
    PackageSpec* package_spec = calloc(1, sizeof(PackageSpec));
    ctx.curr = input_start;
    ctx.input_start = input_start;
    ctx.input_end = input_end;

    next_token();
    parse_package_spec(package_spec);
    return package_spec;
}

static
void parse_package_spec(PackageSpec* package_spec)
{
    expect_token(TOKEN_PACKAGE);
    next_token();

    expect_token(TOKEN_IDENT);
    package_spec->name = ctx.token.text;
    next_token();

    expect_token(TOKEN_IS);
    next_token();

    begin_region();
    while(ctx.token.kind != TOKEN_END) {
        Declaration* decl = calloc(1, sizeof(Declaration));
        // TODO: parse representation_clause/use_clause here too
        parse_basic_declaration(decl);
    }
    next_token(); // Skip 'end'

    if(ctx.token.kind == TOKEN_IDENT) {
        if(!identifier_equal(&package_spec->name, &ctx.token.text)) {
            print_parse_error("Closing identifier does not match package specification name (%.*s)", SV(package_spec->name));
            exit(1);
        }
        next_token();
    }

    expect_token(TOKEN_SEMICOLON);
    next_token();

    package_spec->decls = curr_region(ctx).first;
    end_region();

    expect_token(TOKEN_EOF);
}

static
void parse_basic_declaration(Declaration* decl)
{
    switch(ctx.token.kind) {
        case TOKEN_IDENT:
            parse_object_declaration(decl, /*is_param*/false);
            break;
        case TOKEN_TYPE:
        case TOKEN_SUBTYPE:
            // TODO: incomplete and private type declarations
            parse_type_declaration(decl);
            break;
        case TOKEN_PROCEDURE:
        case TOKEN_FUNCTION:
            parse_subprogram_declaration(decl);
            break;
        default:
            print_unexpected_token_error(&ctx.token); /* fall through */
        case TOKEN_ERROR:
            exit(1);
    }
    expect_token(TOKEN_SEMICOLON);
    next_token();

    // TODO: recursive functions need their declaration pushed before body is done being defined.
    //   Most other declarations cannot be self-referential and so should not push their names until
    //   the declaration is done being parsed.
    push_declaration(decl);
}

static
void parse_object_declaration(Declaration* decl, bool is_param)
{
    decl->kind = DECL_OBJECT;
    ObjectDecl* obj_decl = &decl->u.object;
    // TODO: support identifier_list
    obj_decl->name = ctx.token.text;
    // TODO: print line number of previous definition
    if(find_declaration_in_current_region(obj_decl->name) != NULL) {
        print_parse_error("Redefinition of '%.*s' within same declarative region", SV(obj_decl->name));
        exit(1);
    }
    next_token();

    expect_token(TOKEN_COLON);
    next_token();

    if(is_param) {
        switch(ctx.token.kind) {
            case TOKEN_IN:
                obj_decl->mode = PARAM_MODE_IN;
                next_token();
                break;
            case TOKEN_OUT:
                obj_decl->mode = PARAM_MODE_OUT;
                next_token();
                break;
            case TOKEN_IN_OUT:
                obj_decl->mode = PARAM_MODE_IN_OUT;
                next_token();
                break;
            case TOKEN_ERROR:
                exit(1);
            default:
                // No mode specified means 'in' mode
                obj_decl->mode = PARAM_MODE_IN;
        }
    } else if(ctx.token.kind == TOKEN_CONSTANT) {
        obj_decl->is_constant = true;
        next_token();
    }

    if(ctx.token.kind == TOKEN_IDENT) {
        // Type name
        // TODO: properly parse this as a subtype_indication or constrained_array_definition
        TypeDecl* type_decl = find_visible_type_declaration(ctx.token.text);
        if(!type_decl) {
            print_parse_error("Unknown type: %.*s", SV(ctx.token.text));
            exit(1);
        }
        obj_decl->type = type_decl;
        next_token();
    } else if(obj_decl->is_constant && ctx.token.kind == TOKEN_ASSIGN) {
        // number_declaration
        obj_decl->type = &universal_int_type;
    } else {
        print_unexpected_token_error(&ctx.token);
        exit(1);
    }

    if(ctx.token.kind == TOKEN_ASSIGN) {
        next_token();
        obj_decl->init_expr = parse_expression();
    }
}

static
void parse_type_declaration(Declaration* decl)
{
    decl->kind = DECL_TYPE;
    TypeDecl* type_decl = &decl->u.type;
    bool is_subtype = ctx.token.kind == TOKEN_SUBTYPE;
    next_token(); // Skip 'type'/'subtype' keyword

    expect_token(TOKEN_IDENT);
    type_decl->name = ctx.token.text;
    // TODO: print line number of previous definition
    if(find_declaration_in_current_region(type_decl->name) != NULL) {
        print_parse_error("Redefinition of '%.*s' within same declarative region", SV(type_decl->name));
        exit(1);
    }
    next_token();

    // TODO: discriminant_part
    expect_token(TOKEN_IS);
    next_token();

    if(is_subtype) {
        // TODO: properly parse this as a subtype_indication
        expect_token(TOKEN_IDENT);
        TypeDecl* base_type_decl = find_visible_type_declaration(ctx.token.text);
        if(!base_type_decl) {
            print_parse_error("Unknown base type: %.*s", SV(ctx.token.text));
            exit(1);
        }
        type_decl->kind = TYPE_SUBTYPE;
        type_decl->u.subtype.base = base_type_decl;
        next_token();
    } else {
        switch(ctx.token.kind) {
            case TOKEN_RANGE:
                type_decl->kind = TYPE_INTEGER;
                parse_integer_type_definition(&type_decl->u.int_);
                break;
            case TOKEN_L_PAREN:
                type_decl->kind = TYPE_ENUM;
                parse_enum_type_definition(&type_decl->u.enum_);
                break;
            case TOKEN_NEW:
                next_token();
                // TODO: properly parse this as a subtype_indication
                expect_token(TOKEN_IDENT);
                TypeDecl* base_type_decl = find_visible_type_declaration(ctx.token.text);
                if(!base_type_decl) {
                    print_parse_error("Unknown base type: %.*s", SV(ctx.token.text));
                    exit(1);
                }
                type_decl->kind = TYPE_DERIVED;
                type_decl->u.subtype.base = base_type_decl;
                next_token();
                break;
            default:
                print_unexpected_token_error(&ctx.token); /* fall through */
            case TOKEN_ERROR:
                exit(1);
        }
    }
}

static
void parse_integer_type_definition(IntType* int_type)
{
    next_token(); // Skip 'range' keyword
    int_type->range.lower_bound = parse_expression();

    expect_token(TOKEN_DOUBLE_DOT);
    next_token();

    int_type->range.upper_bound = parse_expression();
}

static
void parse_enum_type_definition(EnumType* enum_type)
{
    next_token(); // Skip '('
    uint32_t literal_count = count_enum_literals();
    enum_type->literal_count = literal_count;
    enum_type->literals = calloc(literal_count, sizeof(Expression));
    for(uint32_t i = 0; i < literal_count; ++i) {
        switch(ctx.token.kind) {
            case TOKEN_IDENT:
                enum_type->literals[i].kind = EXPR_NAME;
                enum_type->literals[i].u.name = ctx.token.text;
                break;
            case TOKEN_CHAR_LITERAL:
                enum_type->literals[i].kind = EXPR_CHAR_LIT;
                enum_type->literals[i].u.char_lit = ctx.token.text.value[0]; // len is always 1
                break;
            default:
                // Should be unreachable since count_enum_literals() succeeded
                print_unexpected_token_error(&ctx.token);
                exit(1);
        }
        next_token();
        if(i < literal_count - 1) {
            expect_token(TOKEN_COMMA);
            next_token();
        }
    }
    expect_token(TOKEN_R_PAREN);
    next_token();
}

static
void parse_subprogram_declaration(Declaration* decl)
{
    decl->kind = (ctx.token.kind == TOKEN_FUNCTION) ? DECL_FUNCTION : DECL_PROCEDURE;
    next_token();

    Token op_token = {0};
    if(ctx.token.kind == TOKEN_STRING_LITERAL) {
        if(decl->kind != DECL_FUNCTION) {
            print_parse_error("Overloaded operators must be functions");
            exit(1);
        }
        const char* token_end = lexer_parse_token(ctx.input_start, ctx.input_end, ctx.token.text.value, &op_token);
        if(token_end != ctx.token.text.value + ctx.token.text.len || !is_overloadable_op(op_token.kind)) {
            print_parse_error("'%.*s' is not an overloadable operator", SV(ctx.token.text));
            exit(1);
        }
        decl->u.subprogram.is_operator = true;
    } else {
        expect_token(TOKEN_IDENT);
    }
    // TODO: check if is an overload, if so if it is permissable
    decl->u.subprogram.name = ctx.token.text;
    next_token();

    begin_region();
    // TODO: pushing function decl to its own body's region will mess things up. When symbols in the body
    //  are pushed, they will be chained to the end of the decl (i.e. they will show up in the scope the function
    //  was declared in when the function as a whole is pushed). Maybe can temporarily push (so params see their
    //  function name, then push to outer scope?)
    push_declaration(decl);
    if(ctx.token.kind == TOKEN_L_PAREN) {
        parse_parameters(&decl->u.subprogram.params);
    }

    if(decl->u.subprogram.is_operator) {
        check_op_arity(decl->u.subprogram.params, &op_token);
    }

    if(decl->kind == DECL_FUNCTION) {
        expect_token(TOKEN_RETURN);
        next_token();
        // TODO: properly parse type_mark
        TypeDecl* return_type_decl = find_visible_type_declaration(ctx.token.text);
        if(!return_type_decl) {
            print_parse_error("Unknown type: %.*s", SV(ctx.token.text));
            exit(1);
        }
        decl->u.subprogram.return_type = return_type_decl;
        next_token();
    }
    end_region();
}

static
void parse_parameters(Declaration** params)
{
    next_token(); // Skip '('
    DeclList decl_list = {0};
    while(ctx.token.kind != TOKEN_R_PAREN) {
        Declaration* param = calloc(1, sizeof(Declaration));
        parse_object_declaration(param, /*is_param*/true);
        if(ctx.token.kind != TOKEN_R_PAREN) {
            expect_token(TOKEN_SEMICOLON);
            next_token();
        }
        append_decl(&decl_list, param);
        *params = decl_list.first;
    }
    next_token(); // Skip ')'
}

typedef uint8_t OperatorFlags;
enum {
    UNARY = 1,
    BINARY = 1 << 1,
    OVERLOADABLE = 1 << 2,
};

static const struct {
    OperatorFlags flags; // Default of 0 means token is not an operator
    // upper 3 bits: UnaryOperator (if any)
    // lower 5 bits: BinaryOperator (if any)
    uint8_t ops;
} op_token_info[TOKEN_NUM_TOKEN_KINDS] = {
    // Logical operators
    [TOKEN_AND]      = {BINARY | OVERLOADABLE, OP_AND},
    [TOKEN_AND_THEN] = {BINARY,                OP_AND_THEN},
    [TOKEN_OR]       = {BINARY | OVERLOADABLE, OP_OR},
    [TOKEN_OR_ELSE]  = {BINARY,                OP_OR_ELSE},
    [TOKEN_XOR]      = {BINARY | OVERLOADABLE, OP_XOR},
    // Relational operators
    [TOKEN_EQ]       = {BINARY | OVERLOADABLE, OP_EQ},
    [TOKEN_NEQ]      = {BINARY,                OP_NEQ},
    [TOKEN_LT]       = {BINARY | OVERLOADABLE, OP_LT},
    [TOKEN_LTE]      = {BINARY | OVERLOADABLE, OP_LTE},
    [TOKEN_GT]       = {BINARY | OVERLOADABLE, OP_GT},
    [TOKEN_GTE]      = {BINARY | OVERLOADABLE, OP_GTE},
    [TOKEN_IN]       = {BINARY,                OP_IN},
    [TOKEN_NOT_IN]   = {BINARY,                OP_NOT_IN},
    // Binary adding operators
    [TOKEN_PLUS]     = {BINARY | UNARY | OVERLOADABLE, (OP_UNARY_PLUS << 5) | OP_PLUS},
    [TOKEN_MINUS]    = {BINARY | UNARY | OVERLOADABLE, (OP_UNARY_MINUS << 5) | OP_MINUS},
    [TOKEN_AMP]      = {BINARY | OVERLOADABLE,         OP_AMP},
    // Multiplying operators
    [TOKEN_MULT]     = {BINARY | OVERLOADABLE, OP_MULT},
    [TOKEN_DIVIDE]   = {BINARY | OVERLOADABLE, OP_DIVIDE},
    [TOKEN_MOD]      = {BINARY | OVERLOADABLE, OP_MOD},
    [TOKEN_REM]      = {BINARY | OVERLOADABLE, OP_REM},
    // Highest precedence operator
    [TOKEN_EXP]      = {BINARY | OVERLOADABLE, OP_EXP},
    [TOKEN_NOT]      = {UNARY | OVERLOADABLE,  OP_NOT << 5},
    [TOKEN_ABS]      = {UNARY | OVERLOADABLE,  OP_ABS << 5},
};
// BinaryOperator no longer fits in 5 bits; update precedence table layout
STATIC_ASSERT(BINARY_OP_COUNT < 32);
// UnaryOperator no longer fits in 3 bits; update precedence table layout
STATIC_ASSERT(UNARY_OP_COUNT < 8);

static const uint8_t unary_prec[UNARY_OP_COUNT] = {
    // Unary adding operators
    [OP_UNARY_PLUS]  = 4,
    [OP_UNARY_MINUS] = 4,
    // Highest precedence operators
    [OP_ABS]         = 6,
    [OP_NOT]         = 6
};

static const uint8_t binary_prec[BINARY_OP_COUNT] = {
    // Logical operators
    [OP_AND]      = 1,
    [OP_AND_THEN] = 1,
    [OP_OR]       = 1,
    [OP_OR_ELSE]  = 1,
    [OP_XOR]      = 1,
    // Relational operators
    [OP_EQ]       = 2,
    [OP_NEQ]      = 2,
    [OP_LT]       = 2,
    [OP_LTE]      = 2,
    [OP_GT]       = 2,
    [OP_GTE]      = 2,
    [OP_IN]       = 2,
    [OP_NOT_IN]   = 2,
    // Binary adding operators
    [OP_PLUS]     = 3,
    [OP_MINUS]    = 3,
    [OP_AMP]      = 3,
    // Multiplying operators
    [OP_MULT]     = 5,
    [OP_DIVIDE]   = 5,
    [OP_MOD]      = 5,
    [OP_REM]      = 5,
    // Highest precedence operator
    [OP_EXP]      = 6
};

static
bool is_binary_op(TokenKind token)
{
    return token < cnt_of_array(op_token_info) && (op_token_info[token].flags & BINARY);
}

static
bool is_unary_op(TokenKind token)
{
    return token < cnt_of_array(op_token_info) && (op_token_info[token].flags & UNARY);
}

static
bool is_overloadable_op(TokenKind token)
{
    return token < cnt_of_array(op_token_info) && (op_token_info[token].flags & OVERLOADABLE);
}

// Assumes op_token->kind is either an unary or binary operator
static
void check_op_arity(const Declaration* params, const Token* op_token)
{
    if(is_unary_op(op_token->kind)) {
        if(is_binary_op(op_token->kind)) {
            if(!params || (params->next && params->next->next)) {
                print_parse_error("Overloaded operator '%.*s' must be an unary or binary function", SV(op_token->text));
                exit(1);
            }
        } else if(!params || params->next) {
            print_parse_error("Overloaded operator '%.*s' must be an unary function", SV(op_token->text));
            exit(1);
        }
    } else if(!params || !params->next || params->next->next) {
        print_parse_error("Overloaded operator '%.*s' must be a binary function", SV(op_token->text));
        exit(1);
    }
}

#define binary_op(token) (op_token_info[token].ops & 0x1F) // Bottom 5 bits
#define unary_op(token) (op_token_info[token].ops >> 5) // Top 3 bits

static
Expression* parse_expression(void)
{
    return parse_expression_1(0);
}

static
Expression* parse_expression_1(uint8_t min_precedence)
{
    Expression* left = parse_primary_expression();
    while(is_binary_op(ctx.token.kind) && binary_prec[binary_op(ctx.token.kind)] >= min_precedence) {
        BinaryOperator op = binary_op(ctx.token.kind);
        next_token();
        Expression* right = parse_expression_1(binary_prec[op] + 1);
        Expression* expr = calloc(1, sizeof(Expression));
        expr->kind = EXPR_BINARY;
        expr->u.binary.left = left;
        expr->u.binary.op = op;
        expr->u.binary.right = right;
        left = expr;
    }
    return left;
}

static
Expression* parse_primary_expression(void)
{
    Expression* expr = NULL;
    switch(ctx.token.kind) {
        case TOKEN_NUM_LITERAL:
            expr = parse_numeric_literal();
            break;
        case TOKEN_CHAR_LITERAL:
            expr = calloc(1, sizeof(Expression));
            expr->kind = EXPR_CHAR_LIT;
            expr->u.char_lit = ctx.token.text.value[0];
            next_token();
            break;
        case TOKEN_STRING_LITERAL:
            expr = calloc(1, sizeof(Expression));
            expr->kind = EXPR_STRING_LIT;
            expr->u.string_lit = ctx.token.text;
            next_token();
            break;
        case TOKEN_IDENT:
            expr = calloc(1, sizeof(Expression));
            expr->kind = EXPR_NAME;
            expr->u.name = ctx.token.text;
            next_token();
            break;
        case TOKEN_L_PAREN:
            next_token();
            expr = parse_expression();
            expect_token(TOKEN_R_PAREN);
            next_token();
            break;
        case TOKEN_ERROR:
            exit(1);
        default:
            if(is_unary_op(ctx.token.kind)) {
                UnaryOperator op = unary_op(ctx.token.kind);
                next_token();
                Expression* right = parse_expression_1(unary_prec[op]);
                if(right) {
                    expr = calloc(1, sizeof(Expression));
                    expr->kind = EXPR_UNARY;
                    expr->u.unary.op = op;
                    expr->u.unary.right = right;
                }
            } else {
                print_unexpected_token_error(&ctx.token);
                exit(1);
            }
    }
    return expr;
}

// TODO: use pool since likely to reuse same numeric literals
static
Expression* parse_numeric_literal(void)
{
    char num_buffer[128];

    if(memchr(ctx.token.text.value, '.', ctx.token.text.len) != NULL) {
        print_parse_error("TODO: support non-integer numeric literals");
        exit(1);
    }
    int base = 10;
    const char* hash_mark = memchr(ctx.token.text.value, '#', ctx.token.text.len);
    if(hash_mark) {
        base = 0;
        for(const char* c = ctx.token.text.value; c != hash_mark; ++c) {
            if(*c != '_') {
                base = base * 10 + (*c - '0');
            }
        }
        if(base < 1 || base > 16) {
            print_parse_error("Numeric literal has invalid base (%d). Bases must be in range [1, 16]", base);
            exit(1);
        }
    }

    num_buffer[0] = '\0';
    if(!prepare_num_str(&ctx.token.text, num_buffer, sizeof(num_buffer))) {
        print_parse_error("Numeric literal is too long to be processed (max supported is 127 characters)");
        exit(1);
    }
    Expression* expr = calloc(1, sizeof(Expression));
    expr->kind = EXPR_INT_LIT;
    if(mpz_init_set_str(expr->u.int_lit, num_buffer, base) < 0) {
        print_parse_error("Invalid numeric literal: '%.*s' for base %u", SV(ctx.token.text), base);
        exit(1);
    }
    next_token();
    return expr;
}

// TODO: have way to provide initial list of declarations (e.g. from a package spec or a function's param list)
static
void begin_region(void)
{
    if(ctx.curr_region_idx + 1 >= cnt_of_array(ctx.region_stack)) {
        print_parse_error("Too many nested regions (maximum is %u nested regions)", cnt_of_array(ctx.region_stack));
        exit(1);
    }
    ++ctx.curr_region_idx;
    memset(&curr_region(ctx), 0, sizeof(ctx.region_stack[0]));
}

static
void end_region(void)
{
    if(ctx.curr_region_idx == 0) {
        print_parse_error("Attempted to exit top-level region");
        exit(1);
    }
    --ctx.curr_region_idx;
}

static
void push_declaration(Declaration* decl)
{
    append_decl(&curr_region(ctx), decl);
}

// TODO: intern strings to make this faster
static
bool identifier_equal(const StringView* a, const StringView* b)
{
    if(a->len != b->len) {
        return false;
    }
    for(uint32_t i = 0; i < a->len; ++i) {
        if(tolower(a->value[i]) != tolower(b->value[i])) {
            return false;
        }
    }
    return true;
}

static
void append_decl(DeclList* decl_list, Declaration* decl)
{
    if(decl_list->last) {
        decl_list->last->next = decl;
    } else {
        decl_list->first = decl;
    }
    decl_list->last = decl;
}

static
TypeDecl* find_visible_type_declaration(StringView name)
{
    for(int stack_idx = ctx.curr_region_idx; stack_idx >= 0; --stack_idx) {
        DeclList* region = &ctx.region_stack[stack_idx];
        for(Declaration* decl = region->first; decl != NULL; decl = decl->next) {
            if(decl->kind == DECL_TYPE && identifier_equal(&decl->u.type.name, &name)) {
                return &decl->u.type;
            }
        }
    }
    return NULL;
}

static
Declaration* find_declaration(Declaration* decl_list, StringView name)
{
    for(Declaration* decl = decl_list; decl != NULL; decl = decl->next) {
        switch(decl->kind) {
            case DECL_TYPE:
                if(identifier_equal(&decl->u.type.name, &name)) {
                    return decl;
                }
                break;
            case DECL_OBJECT:
                if(identifier_equal(&decl->u.object.name, &name)) {
                    return decl;
                }
                break;
            case DECL_PROCEDURE:
            case DECL_FUNCTION: {
                if(identifier_equal(&decl->u.subprogram.name, &name)) {
                    return decl;
                }
                Declaration* match = find_declaration(decl->u.subprogram.params, name);
                if(match) {
                    return match;
                }
                break;
            }
            default:
                assert(false && "Unhandled declaration type");
        }
    }
    return NULL;
}

static
Declaration* find_declaration_in_current_region(StringView name)
{
    DeclList* region = &ctx.region_stack[ctx.curr_region_idx];
    return find_declaration(region->first, name);
}

static
void next_token(void)
{
    ctx.curr = lexer_parse_token(ctx.input_start, ctx.input_end, ctx.curr, &ctx.token);
}

static
void expect_token(TokenKind kind)
{
    if(ctx.token.kind == TOKEN_ERROR) {
        exit(1);
    }
    if(ctx.token.kind != kind) {
        print_unexpected_token_error(&ctx.token);
        exit(1);
    }
}

static
uint32_t count_enum_literals(void)
{
    uint32_t literal_count = 0;
    Token token = ctx.token;
    const char* curr = ctx.curr;
    while(token.kind != TOKEN_R_PAREN) {
        switch(token.kind) {
            case TOKEN_IDENT:
            case TOKEN_CHAR_LITERAL:
                if(literal_count == UINT32_MAX) {
                    error_print(ctx.input_start, curr, "Enumeration type has too many literals to be processed (max supported is 2**32-1 literals)");
                    exit(1);
                }
                ++literal_count;
                break;
            case TOKEN_COMMA:
                break;
            default:
                print_unexpected_token_error(&token); /* fall through */
            case TOKEN_ERROR:
                exit(1);
        }
        curr = lexer_parse_token(ctx.input_start, ctx.input_end, curr, &token);
    }
    return literal_count;
}

static
void print_unexpected_token_error(const Token* token)
{
    StringView token_str = token_to_str(token);
    print_parse_error("Unexpected token: '%.*s'", SV(token_str));
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
