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
#include "string_pool.h"
#include "string_view.h"
#include "mini-gmp.h"

#define DEFINE_LIST_TYPE(list_type, value_type, name) \
    typedef struct { \
        value_type* first; \
        value_type* last; \
    } list_type; \
    static void append_##name(list_type* list, value_type* value) \
    { \
        if(list->last) { \
            list->last->next = value; \
        } else { \
            list->first = value; \
        } \
        list->last = value; \
    }

DEFINE_LIST_TYPE(DeclList, Declaration, decl)
DEFINE_LIST_TYPE(StmtList, Statement, stmt)
DEFINE_LIST_TYPE(CaseList, Case, case)

typedef struct {
    DeclList region_stack[32];
    uint8_t curr_region_idx;
    const char* curr;
    const char* input_end;
    Token token;
} ParseContext;

static const char universal_integer_str[] = "universal_integer";
TypeDecl universal_int_type = {
    .kind = TYPE_UNIV_INTEGER,
    .name = 0 // This gets set in parser_parse
};

static ParseContext ctx;

/* PACKAGE */
static void parse_package_spec(PackageSpec* package_spec);
/* DECLARATIONS */
static void parse_declaration(void);
static void parse_object_declaration(bool is_param);
static void parse_type_declaration(void);
static void parse_integer_type_definition(IntType* int_type);
static void parse_enum_type_definition(EnumType* enum_type);
static void parse_subprogram_declaration(SubprogramDecl* decl);
static uint8_t parse_parameters(void);
static void parse_subprogram_body(SubprogramDecl* decl);
static void parse_choice(Choice* choice);
/* STATEMENTS */
static Statement* parse_statement(void);
static void parse_statement_labels(Statement* stmt);
static void parse_assign_statement(Statement* stmt, StringToken name);
static void parse_procedure_call_statement(Statement* stmt, StringToken name);
static void parse_block_statement(Statement* stmt);
static void parse_if_statement(Statement* stmt);
static void parse_case_statement(Statement* stmt);
static void parse_loop_statement(Statement* stmt);
static void parse_goto_statement(Statement* stmt);
/* EXPRESSIONS */
static Expression* parse_expression(void);
static Expression* parse_expression_1(uint8_t min_precedence);
static Expression* parse_primary_expression(void);
static Expression* parse_numeric_literal(void);
/* VISIBILITY */
static void begin_region(void);
static void end_region(void);
static void push_declaration(Declaration* decl);
static Declaration* find_declaration(Declaration* decl_list, StringToken name);
static Declaration* find_declaration_in_current_region(StringToken name);
static Declaration* find_visible_declaration(StringToken name, DeclKind kind);
/* UTILITIES */
#define curr_region(ctx) ctx.region_stack[ctx.curr_region_idx]
#define print_parse_error(...) error_print(ctx.token.line_num, __VA_ARGS__)
#define cnt_of_array(arr) (sizeof(arr) / sizeof(arr[0]))
static void next_token(void);
static void expect_token(TokenKind kind);
static bool is_binary_op(TokenKind token);
static bool is_unary_op(TokenKind token);
static bool is_overloadable_op(TokenKind token);
static void check_op_arity(const Token* op_token, uint8_t param_count);
static uint32_t count_enum_literals(void);
static uint8_t count_alternatives(void);
static void print_unexpected_token_error(const Token* token);
static bool prepare_num_str(const StringView* text, char* buffer, int buffer_sz);

void parser_init(void)
{
    StringView universal_int_str_view = { .value = universal_integer_str, .len = sizeof(universal_integer_str) };
    universal_int_type.name = string_pool_to_token(universal_int_str_view);
}

CompilationUnit* parser_parse(const char* input_start, const char* input_end)
{
    memset(&ctx, 0, sizeof(ctx));
    CompilationUnit* unit = calloc(1, sizeof(CompilationUnit));

    ctx.curr = input_start;
    ctx.input_end = input_end;
    ctx.token.line_num = 1;
    next_token(); // Read first token
    switch(ctx.token.kind) {
        case TOKEN_PACKAGE: {
            const char* next = ctx.curr;
            if(lexer_lookahead(ctx.input_end, &next) == TOKEN_BODY) {
                unit->kind = COMP_UNIT_PACKAGE_BODY;
                // TODO
            } else {
                unit->kind = COMP_UNIT_PACKAGE_SPEC;
                parse_package_spec(&unit->u.package_spec);
            }
            break;
        }
        case TOKEN_PROCEDURE:
            unit->kind = COMP_UNIT_SUBPROGRAM;
            parse_subprogram_declaration(&unit->u.subprogram_decl);
            break;
        default:
            print_unexpected_token_error(&ctx.token);
            error_exit();
    }
    return unit;
}

static
void parse_package_spec(PackageSpec* package_spec)
{
    expect_token(TOKEN_PACKAGE);
    next_token();

    expect_token(TOKEN_IDENT);
    package_spec->name = string_pool_to_token(ctx.token.text);
    next_token();

    expect_token(TOKEN_IS);
    next_token();

    begin_region();
    while(ctx.token.kind != TOKEN_END) {
        parse_declaration();
    }
    next_token(); // Skip 'end'

    if(ctx.token.kind == TOKEN_IDENT) {
        StringToken closing_name = string_pool_to_token(ctx.token.text);
        if(package_spec->name != closing_name) {
            print_parse_error("Closing identifier does not match package specification name (%s)", ST(package_spec->name));
            error_exit();
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
void parse_declaration(void)
{
    // TODO: parse representation_clause/use_clause here too
    switch(ctx.token.kind) {
        case TOKEN_IDENT:
            parse_object_declaration(/*is_param*/false);
            break;
        case TOKEN_TYPE:
        case TOKEN_SUBTYPE:
            // TODO: incomplete and private type declarations
            parse_type_declaration();
            break;
        case TOKEN_PROCEDURE:
        case TOKEN_FUNCTION: {
            SubprogramDecl* decl = calloc(1, sizeof(SubprogramDecl));
            parse_subprogram_declaration(decl);
            break;
        }
        default:
            print_unexpected_token_error(&ctx.token);
            error_exit();
    }
    expect_token(TOKEN_SEMICOLON);
    next_token();
}

static
void parse_object_declaration(bool is_param)
{
    ObjectDecl* decl = calloc(1, sizeof(ObjectDecl));
    decl->base.kind = DECL_OBJECT;
    decl->base.line_num = ctx.token.line_num;
    // TODO: support identifier_list
    decl->name = string_pool_to_token(ctx.token.text);
    Declaration* prev_decl = find_declaration_in_current_region(decl->name);
    if(prev_decl) {
        print_parse_error("Redefinition of '%.*s' within same declarative region", SV(ctx.token.text));
        error_print(prev_decl->line_num, "Previous definition here");
        error_exit();
    }
    next_token();

    expect_token(TOKEN_COLON);
    next_token();

    if(is_param) {
        switch(ctx.token.kind) {
            case TOKEN_IN:
                decl->mode = PARAM_MODE_IN;
                next_token();
                break;
            case TOKEN_OUT:
                decl->mode = PARAM_MODE_OUT;
                next_token();
                break;
            case TOKEN_IN_OUT:
                decl->mode = PARAM_MODE_IN_OUT;
                next_token();
                break;
            default:
                // No mode specified means 'in' mode
                decl->mode = PARAM_MODE_IN;
        }
    } else if(ctx.token.kind == TOKEN_CONSTANT) {
        decl->is_constant = true;
        next_token();
    }

    if(ctx.token.kind == TOKEN_IDENT) {
        // Type name
        // TODO: properly parse this as a subtype_indication or constrained_array_definition
        StringToken type_name = string_pool_to_token(ctx.token.text);
        TypeDecl* type_decl = (TypeDecl*)find_visible_declaration(type_name, DECL_TYPE);
        if(!type_decl) {
            print_parse_error("Unknown type: %.*s", SV(ctx.token.text));
            error_exit();
        }
        decl->type = type_decl;
        next_token();
    } else if(decl->is_constant && ctx.token.kind == TOKEN_ASSIGN) {
        // number_declaration
        decl->type = &universal_int_type;
    } else {
        print_unexpected_token_error(&ctx.token);
        error_exit();
    }

    if(ctx.token.kind == TOKEN_ASSIGN) {
        next_token();
        decl->init_expr = parse_expression();
    }
    push_declaration(&decl->base);
}

static
void parse_type_declaration(void)
{
    TypeDecl* decl = calloc(1, sizeof(TypeDecl));
    decl->base.kind = DECL_TYPE;
    decl->base.line_num = ctx.token.line_num;
    bool is_subtype = ctx.token.kind == TOKEN_SUBTYPE;
    next_token(); // Skip 'type'/'subtype' keyword

    expect_token(TOKEN_IDENT);
    decl->name = string_pool_to_token(ctx.token.text);
    Declaration* prev_decl = find_declaration_in_current_region(decl->name);
    if(prev_decl) {
        print_parse_error("Redefinition of '%s' within same declarative region", ST(decl->name));
        error_print(prev_decl->line_num, "Previous definition here");
        error_exit();
    }
    next_token();

    // TODO: discriminant_part
    expect_token(TOKEN_IS);
    next_token();

    if(is_subtype) {
        // TODO: properly parse this as a subtype_indication
        expect_token(TOKEN_IDENT);
        StringToken base_type_name = string_pool_to_token(ctx.token.text);
        TypeDecl* base_type_decl = (TypeDecl*)find_visible_declaration(base_type_name, DECL_TYPE);
        if(!base_type_decl) {
            print_parse_error("Unknown base type: %.*s", SV(ctx.token.text));
            error_exit();
        }
        decl->kind = TYPE_SUBTYPE;
        decl->u.subtype.base = base_type_decl;
        next_token();
    } else {
        switch(ctx.token.kind) {
            case TOKEN_RANGE:
                decl->kind = TYPE_INTEGER;
                parse_integer_type_definition(&decl->u.int_);
                break;
            case TOKEN_L_PAREN:
                decl->kind = TYPE_ENUM;
                parse_enum_type_definition(&decl->u.enum_);
                break;
            case TOKEN_NEW:
                next_token();
                // TODO: properly parse this as a subtype_indication
                expect_token(TOKEN_IDENT);
                StringToken base_type_name = string_pool_to_token(ctx.token.text);
                TypeDecl* base_type_decl = (TypeDecl*)find_visible_declaration(base_type_name, DECL_TYPE);
                if(!base_type_decl) {
                    print_parse_error("Unknown base type: %.*s", SV(ctx.token.text));
                    error_exit();
                }
                decl->kind = TYPE_DERIVED;
                decl->u.subtype.base = base_type_decl;
                next_token();
                break;
            default:
                print_unexpected_token_error(&ctx.token);
                error_exit();
        }
    }
    push_declaration((Declaration*)decl);
}

static
void parse_integer_type_definition(IntType* int_type)
{
    next_token(); // Skip 'range' keyword
    int_type->range = parse_expression();
    if(int_type->range->kind != EXPR_BINARY || int_type->range->u.binary.op != OP_RANGE) {
        print_parse_error("Unexpected expression: integer type must be defined by a range");
        error_exit();
    }
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
                error_exit();
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
void parse_subprogram_declaration(SubprogramDecl* decl)
{
    decl->base.kind = DECL_SUBPROGRAM;
    decl->base.line_num = ctx.token.line_num;
    bool is_function = ctx.token.kind == TOKEN_FUNCTION;
    next_token();

    Token op_token = ctx.token;
    if(ctx.token.kind == TOKEN_STRING_LITERAL) {
        if(!is_function) {
            print_parse_error("Overloaded operators must be functions");
            error_exit();
        }
        const char* token_end = lexer_parse_token(ctx.input_end, ctx.token.text.value, &op_token);
        if(token_end != ctx.token.text.value + ctx.token.text.len || !is_overloadable_op(op_token.kind)) {
            print_parse_error("'%.*s' is not an overloadable operator", SV(ctx.token.text));
            error_exit();
        }
        decl->is_operator = true;
    } else {
        expect_token(TOKEN_IDENT);
    }
    // TODO: check if is an overload, if so if it is permissable
    decl->name = string_pool_to_token(ctx.token.text);
    push_declaration((Declaration*)decl);
    next_token();

    begin_region();
    if(ctx.token.kind == TOKEN_L_PAREN) {
        decl->param_count = parse_parameters();
        decl->decls = curr_region(ctx).first;
    }

    if(decl->is_operator) {
        check_op_arity(&op_token, decl->param_count);
    }

    if(is_function) {
        expect_token(TOKEN_RETURN);
        next_token();
        // TODO: properly parse type_mark
        StringToken return_type_name = string_pool_to_token(ctx.token.text);
        TypeDecl* return_type_decl = (TypeDecl*)find_visible_declaration(return_type_name, DECL_TYPE);
        if(!return_type_decl) {
            print_parse_error("Unknown type: %.*s", SV(ctx.token.text));
            error_exit();
        }
        decl->return_type = return_type_decl;
        next_token();
    }

    if(ctx.token.kind == TOKEN_IS) {
        parse_subprogram_body(decl);
    }
    decl->decls = curr_region(ctx).first;
    end_region();
}

static
void parse_subprogram_body(SubprogramDecl* decl)
{
    next_token(); // Skip 'is'

    // Declarative part
    // TODO: in semantic analysis phase, ensure no basic_declarative_items come after the first later_declarative_item
    while(ctx.token.kind != TOKEN_BEGIN) {
        parse_declaration();
    }
    if(decl->param_count == 0) {
        // Params and decls are in same list, so if no params then the first element will be the first decl (if any)
        decl->decls = curr_region(ctx).first;
    }

    next_token(); // Skip 'begin'
    StmtList stmt_list = {0};
    while(ctx.token.kind != TOKEN_END && ctx.token.kind != TOKEN_EXCEPTION) {
        append_stmt(&stmt_list, parse_statement());
    }
    decl->stmts = stmt_list.first;

    // TODO: exception handlers

    expect_token(TOKEN_END);
    next_token();

    if(ctx.token.kind == TOKEN_IDENT) {
        StringToken closing_name = string_pool_to_token(ctx.token.text);
        if(closing_name != decl->name) {
            print_parse_error("Closing identifier does not match subprogram name (%.*s)", SV(ctx.token.text));
            error_exit();
        }
        next_token();
    }
}

static
uint8_t parse_parameters(void)
{
    next_token(); // Skip '('
    uint8_t param_count = 0;
    while(ctx.token.kind != TOKEN_R_PAREN) {
        parse_object_declaration(/*is_param*/true);
        if(ctx.token.kind != TOKEN_R_PAREN) {
            expect_token(TOKEN_SEMICOLON);
            next_token();
        }
        ++param_count;
    }
    next_token(); // Skip ')'
    return param_count;
}

static
Statement* parse_statement(void)
{
    Statement* stmt = calloc(1, sizeof(Statement));
    parse_statement_labels(stmt);
    stmt->line_num = ctx.token.line_num;
    switch(ctx.token.kind) {
        case TOKEN_NULL:
            stmt->kind = STMT_NULL;
            next_token();
            break;
        case TOKEN_RETURN:
            // TODO: check if this function returns a value (maybe do in semantic analysis phase instead?)
            stmt->kind = STMT_RETURN;
            next_token();
            if(ctx.token.kind != TOKEN_SEMICOLON) {
                stmt->u.return_.expr = parse_expression();
            }
            break;
        case TOKEN_EXIT:
            stmt->kind = STMT_EXIT;
            next_token();
            // TODO: support optional loop name
            if(ctx.token.kind == TOKEN_WHEN) {
                next_token();
                stmt->u.exit.condition = parse_expression();
            }
            break;
        case TOKEN_IDENT: {
            // TODO: properly parse array/record components (and maybe .all?) See LRM chapter 4.1
            StringToken name = string_pool_to_token(ctx.token.text);
            next_token();
            switch(ctx.token.kind) {
                case TOKEN_ASSIGN:
                    parse_assign_statement(stmt, name);
                    break;
                case TOKEN_L_PAREN:
                case TOKEN_SEMICOLON:
                    parse_procedure_call_statement(stmt, name);
                    break;
                default:
                    print_unexpected_token_error(&ctx.token);
                    error_exit();
            }
            break;
        }
        case TOKEN_BEGIN:
        case TOKEN_DECLARE:
            parse_block_statement(stmt);
            break;
        case TOKEN_IF:
            parse_if_statement(stmt);
            break;
        case TOKEN_CASE:
            parse_case_statement(stmt);
            break;
        case TOKEN_FOR:
        case TOKEN_WHILE:
        case TOKEN_LOOP:
            parse_loop_statement(stmt);
            break;
        case TOKEN_GOTO:
            parse_goto_statement(stmt);
            break;
        default:
            print_unexpected_token_error(&ctx.token);
            error_exit();
    }
    expect_token(TOKEN_SEMICOLON);
    next_token();
    return stmt;
}

static
void parse_statement_labels(Statement* stmt)
{
    while(ctx.token.kind == TOKEN_LABEL) {
        StringToken label_name = string_pool_to_token(ctx.token.text);
        next_token();
        Declaration* existing_decl = find_declaration_in_current_region(label_name);
        if(existing_decl) {
            if(existing_decl->kind != DECL_LABEL || ((LabelDecl*)existing_decl)->target) {
                print_parse_error("Redefinition of '%.*s' within same declarative region", SV(ctx.token.text));
                error_print(existing_decl->line_num, "Previous definition here");
                error_exit();
            }
            // There is a placeholder label defined - fill it in instead of creating a new label
            ((LabelDecl*)existing_decl)->target = stmt;
        } else {
            LabelDecl* label = calloc(1, sizeof(LabelDecl));
            label->base.kind = DECL_LABEL;
            label->name = label_name;
            label->target = stmt;
            push_declaration((Declaration*)label);
        }
    }
}

static
void parse_assign_statement(Statement* stmt, StringToken name)
{
    next_token(); // Skip ':='
    stmt->kind = STMT_ASSIGN;
    ObjectDecl* dest = (ObjectDecl*)find_visible_declaration(name, DECL_OBJECT);
    if(!dest) {
        print_parse_error("Unknown variable '%s'", ST(name));
        error_exit();
    }
    stmt->u.assign.dest = dest;
    stmt->u.assign.expr = parse_expression();
}

static
void parse_procedure_call_statement(Statement* stmt, StringToken name)
{
    SubprogramDecl* subprogram_decl = (SubprogramDecl*)find_visible_declaration(name, DECL_SUBPROGRAM);
    if(!subprogram_decl) {
        print_parse_error("Unknown procedure '%s'", ST(name));
        error_exit();
    }
    if(subprogram_decl->return_type) {
        print_parse_error("A function call cannot be a standalone statement (only procedure calls can)");
        error_exit();
    }

    Expression** args = NULL;
    switch(ctx.token.kind) {
        case TOKEN_SEMICOLON:
            // Must be a subprogram called with no arguments
            if(subprogram_decl->param_count != 0) {
                print_parse_error("Subprogram '%s' is called with zero arguments, but it requires %u argument(s)", ST(name), subprogram_decl->param_count);
                error_exit();
            }
            break;
        case TOKEN_L_PAREN: {
            // TODO: account for default arguments and named arguments
            if(subprogram_decl->param_count == 0) {
                print_parse_error("Subprogram '%s' is called with %u arguments, but it requires 0 arguments", ST(name), subprogram_decl->param_count);
                error_exit();
            }
            next_token();
            args = calloc(subprogram_decl->param_count, sizeof(Expression*));
            uint8_t i = 0;
            while(ctx.token.kind != TOKEN_R_PAREN) {
                if(i >= subprogram_decl->param_count) {
                    print_parse_error("Subprogram '%s' is called with too many arguments (requires %u argument(s))", ST(name), subprogram_decl->param_count);
                    error_exit();
                }
                args[i] = parse_expression();
                if(i != subprogram_decl->param_count - 1) {
                    expect_token(TOKEN_COMMA);
                    next_token();
                }
                ++i;
            }
            next_token(); // Skip ')'
            if(i != subprogram_decl->param_count) {
                print_parse_error("Subprogram '%s' is called with %u arguments, but it requires %u argument(s)", ST(name), i, subprogram_decl->param_count);
                error_exit();
            }
            break;
        }
        default:
            print_unexpected_token_error(&ctx.token);
            error_exit();
    }

    stmt->kind = STMT_CALL;
    stmt->u.call.subprogram = subprogram_decl;
    stmt->u.call.args = args;
}

static
void parse_block_statement(Statement* stmt)
{
    stmt->kind = STMT_BLOCK;
    begin_region();
    if(ctx.token.kind == TOKEN_DECLARE) {
        next_token();
        while(ctx.token.kind != TOKEN_BEGIN) {
           parse_declaration();
        }
        stmt->u.block.decls = curr_region(ctx).first;
    }

    expect_token(TOKEN_BEGIN);
    next_token();
    StmtList stmt_list = {0};
    while(ctx.token.kind != TOKEN_END && ctx.token.kind != TOKEN_EXCEPTION) {
        append_stmt(&stmt_list, parse_statement());
    }
    stmt->u.block.stmts = stmt_list.first;

    // TODO: exception handlers

    expect_token(TOKEN_END);
    next_token();

    stmt->u.block.decls = curr_region(ctx).first;
    end_region();
}

static
void parse_if_statement(Statement* stmt)
{
    bool is_if = ctx.token.kind == TOKEN_IF;
    next_token(); // Skip 'if'/'elsif'
    stmt->kind = STMT_IF;
    stmt->u.if_.condition = parse_expression();
    expect_token(TOKEN_THEN);
    next_token();

    StmtList stmt_list = {0};
    while(ctx.token.kind != TOKEN_END && ctx.token.kind != TOKEN_ELSIF && ctx.token.kind != TOKEN_ELSE) {
        append_stmt(&stmt_list, parse_statement());
    }
    stmt->u.if_.stmts = stmt_list.first;
    switch(ctx.token.kind) {
        case TOKEN_ELSIF:
            stmt->u.if_.else_ = calloc(1, sizeof(Statement));
            parse_if_statement(stmt->u.if_.else_);
            break;
        case TOKEN_ELSE:
            next_token();
            stmt->u.if_.else_ = calloc(1, sizeof(Statement));
            stmt->u.if_.else_->kind = STMT_BLOCK;
            memset(&stmt_list, 0, sizeof(stmt_list));
            while(ctx.token.kind != TOKEN_END) {
                append_stmt(&stmt_list, parse_statement());
            }
            stmt->u.if_.else_->u.block.stmts = stmt_list.first;
            break;
        case TOKEN_END:
            break;
        default:
            print_unexpected_token_error(&ctx.token);
            error_exit();
    }

    if(is_if) {
        expect_token(TOKEN_END);
        next_token();
        expect_token(TOKEN_IF);
        next_token();
    }
}

static
void parse_case_statement(Statement* stmt)
{
    next_token(); // Skip 'case'
    stmt->kind = STMT_CASE;
    stmt->u.case_.expr = parse_expression();
    expect_token(TOKEN_IS);
    next_token();

    CaseList choice_list = {0};
    while(ctx.token.kind != TOKEN_END) {
        expect_token(TOKEN_WHEN);
        next_token();
        Case* case_ = calloc(1, sizeof(Case));
        parse_choice(&case_->choice);

        expect_token(TOKEN_ARROW);
        next_token();

        StmtList stmt_list = {0};
        while(ctx.token.kind != TOKEN_WHEN && ctx.token.kind != TOKEN_END) {
            append_stmt(&stmt_list, parse_statement());
        }
        case_->stmts = stmt_list.first;
        append_case(&choice_list, case_);
    }
    stmt->u.case_.cases = choice_list.first;
    next_token();

    expect_token(TOKEN_CASE);
    next_token();
}

static
void parse_loop_statement(Statement* stmt)
{
    // TODO: loop_simple_name (also required at end if provided at start)
    stmt->kind = STMT_LOOP;
    stmt->u.loop.kind = LOOP_WHILE;
    if(ctx.token.kind == TOKEN_WHILE) {
        next_token();
        stmt->u.loop.u.while_.condition = parse_expression();
    } else if(ctx.token.kind == TOKEN_FOR) {
        stmt->u.loop.kind = LOOP_FOR;
        next_token();
        expect_token(TOKEN_IDENT);
        StringToken var_name = string_pool_to_token(ctx.token.text);
        Declaration* prev_decl = find_declaration_in_current_region(var_name);
        if(prev_decl) {
            print_parse_error("Redefinition of '%.*s' within same declarative region", SV(ctx.token.text));
            error_print(prev_decl->line_num, "Previous definition here");
            error_exit();
        }
        stmt->u.loop.u.for_.var = calloc(1, sizeof(ObjectDecl));
        stmt->u.loop.u.for_.var->name = var_name;
        stmt->u.loop.u.for_.var->type = &universal_int_type; // TODO: will need to fill in this type later to be range's type
        next_token();

        expect_token(TOKEN_IN);
        next_token();
        if(ctx.token.kind == TOKEN_REVERSE) {
            stmt->u.loop.reverse = true;
            next_token();
        }
        Expression* range = parse_expression();
        if(range->kind != EXPR_BINARY || range->u.binary.op != OP_RANGE) {
            print_parse_error("Unexpected expression: for loop must have a discrete range");
            error_exit();
        }
        stmt->u.loop.u.for_.range = range;
    } else {
        stmt->u.loop.u.while_.condition = calloc(1, sizeof(Expression));
        // TODO: set to boolean literal
        stmt->u.loop.u.while_.condition->kind = EXPR_INT_LIT;
        mpz_init_set_ui(stmt->u.loop.u.while_.condition->u.int_lit, 1);
    }

    expect_token(TOKEN_LOOP);
    next_token();
    StmtList stmt_list = {0};
    while(ctx.token.kind != TOKEN_END) {
        append_stmt(&stmt_list, parse_statement());
    }
    stmt->u.loop.stmts = stmt_list.first;

    expect_token(TOKEN_END);
    next_token();
    expect_token(TOKEN_LOOP);
    next_token();
}

static
void parse_goto_statement(Statement* stmt)
{
    next_token(); // Skip 'goto'
    stmt->kind = STMT_GOTO;
    expect_token(TOKEN_IDENT);
    StringToken label_name = string_pool_to_token(ctx.token.text);
    LabelDecl* label = (LabelDecl*)find_visible_declaration(label_name, DECL_LABEL);
    if(label) {
        // Label is defined prior to the goto statement
        stmt->u.goto_.label = label;
    } else {
        // Label is not defined yet
        Declaration* existing_decl = find_declaration_in_current_region(label_name);
        if(existing_decl) {
            // When this label is defined, it will conflict with an existing declaration's name
            print_parse_error("Redefinition of '%.*s' within same declarative region", SV(ctx.token.text));
            error_print(existing_decl->line_num, "Previous definition here");
            error_exit();
        }
        // Define a placeholder label
        // TODO: in semantic analysis, verify that all placeholder labels are filled in
        LabelDecl* label = calloc(1, sizeof(LabelDecl));
        label->base.kind = DECL_LABEL;
        label->name = label_name;
        stmt->u.goto_.label = label;
        push_declaration((Declaration*)label);
    }
    next_token();
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
    // Pseudo-operator
    [TOKEN_DOUBLE_DOT] = {BINARY, OP_RANGE},
};
// BinaryOperator no longer fits in 5 bits; update precedence table layout
STATIC_ASSERT(BINARY_OP_COUNT < 32);
// UnaryOperator no longer fits in 3 bits; update precedence table layout
STATIC_ASSERT(UNARY_OP_COUNT < 8);

static const uint8_t unary_prec[UNARY_OP_COUNT] = {
    // Unary adding operators
    [OP_UNARY_PLUS]  = 5,
    [OP_UNARY_MINUS] = 5,
    // Highest precedence operators
    [OP_ABS]         = 7,
    [OP_NOT]         = 7
};

static const uint8_t binary_prec[BINARY_OP_COUNT] = {
    // Pseudo-operators
    [OP_RANGE]    = 1,
    // Logical operators
    [OP_AND]      = 2,
    [OP_AND_THEN] = 2,
    [OP_OR]       = 2,
    [OP_OR_ELSE]  = 2,
    [OP_XOR]      = 2,
    // Relational operators
    [OP_EQ]       = 3,
    [OP_NEQ]      = 3,
    [OP_LT]       = 3,
    [OP_LTE]      = 3,
    [OP_GT]       = 3,
    [OP_GTE]      = 3,
    [OP_IN]       = 3,
    [OP_NOT_IN]   = 3,
    // Binary adding operators
    [OP_PLUS]     = 4,
    [OP_MINUS]    = 4,
    [OP_AMP]      = 4,
    // Multiplying operators
    [OP_MULT]     = 6,
    [OP_DIVIDE]   = 6,
    [OP_MOD]      = 6,
    [OP_REM]      = 6,
    // Highest precedence operator
    [OP_EXP]      = 7,
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
void check_op_arity(const Token* op_token, uint8_t param_count)
{
    if(is_unary_op(op_token->kind)) {
        if(is_binary_op(op_token->kind)) {
            if(param_count != 1 && param_count != 2) {
                print_parse_error("Overloaded operator '%.*s' must be an unary or binary function", SV(op_token->text));
                error_exit();
            }
        } else if(param_count != 1) {
            print_parse_error("Overloaded operator '%.*s' must be an unary function", SV(op_token->text));
            error_exit();
        }
    } else if(param_count != 2) {
        print_parse_error("Overloaded operator '%.*s' must be a binary function", SV(op_token->text));
        error_exit();
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
        uint32_t line_num = ctx.token.line_num;
        next_token();
        Expression* right = parse_expression_1(binary_prec[op] + 1);
        Expression* expr = calloc(1, sizeof(Expression));
        expr->kind = EXPR_BINARY;
        expr->line_num = line_num;
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
            expr->line_num = ctx.token.line_num;
            expr->u.char_lit = ctx.token.text.value[0];
            next_token();
            break;
        case TOKEN_STRING_LITERAL:
            expr = calloc(1, sizeof(Expression));
            expr->kind = EXPR_STRING_LIT;
            expr->line_num = ctx.token.line_num;
            expr->u.string_lit = ctx.token.text;
            next_token();
            break;
        case TOKEN_IDENT:
            expr = calloc(1, sizeof(Expression));
            expr->kind = EXPR_NAME;
            expr->line_num = ctx.token.line_num;
            expr->u.name = ctx.token.text;
            next_token();
            break;
        case TOKEN_L_PAREN:
            next_token();
            expr = parse_expression();
            expect_token(TOKEN_R_PAREN);
            next_token();
            break;
        default:
            if(is_unary_op(ctx.token.kind)) {
                UnaryOperator op = unary_op(ctx.token.kind);
                uint32_t line_num = ctx.token.line_num;
                next_token();
                Expression* right = parse_expression_1(unary_prec[op]);
                expr = calloc(1, sizeof(Expression));
                expr->kind = EXPR_UNARY;
                expr->line_num = line_num;
                expr->u.unary.op = op;
                expr->u.unary.right = right;
            } else {
                print_unexpected_token_error(&ctx.token);
                error_exit();
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
        error_exit();
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
            error_exit();
        }
    }

    num_buffer[0] = '\0';
    if(!prepare_num_str(&ctx.token.text, num_buffer, sizeof(num_buffer))) {
        print_parse_error("Numeric literal is too long to be processed (max supported is 127 characters)");
        error_exit();
    }
    Expression* expr = calloc(1, sizeof(Expression));
    expr->kind = EXPR_INT_LIT;
    expr->line_num = ctx.token.line_num;
    if(mpz_init_set_str(expr->u.int_lit, num_buffer, base) < 0) {
        print_parse_error("Invalid numeric literal: '%.*s' for base %u", SV(ctx.token.text), base);
        error_exit();
    }
    next_token();
    return expr;
}

static
void parse_choice(Choice* choice)
{
    choice->count = count_alternatives();
    choice->alternatives = calloc(choice->count, sizeof(Alternative));
    for(uint8_t i = 0; i < choice->count; ++i) {
        if(ctx.token.kind == TOKEN_OTHERS) {
            choice->alternatives[i].kind = ALT_OTHERS;
            next_token();
        } else {
            choice->alternatives[i].u.expr = parse_expression();
        }
        if(i != choice->count - 1) {
            expect_token(TOKEN_BAR);
            next_token();
        }
    }
}

// TODO: have way to provide initial list of declarations (e.g. from a package spec or a function's param list)
static
void begin_region(void)
{
    if(ctx.curr_region_idx + 1 >= cnt_of_array(ctx.region_stack)) {
        print_parse_error("Too many nested regions (maximum is %u nested regions)", cnt_of_array(ctx.region_stack));
        error_exit();
    }
    ++ctx.curr_region_idx;
    memset(&curr_region(ctx), 0, sizeof(ctx.region_stack[0]));
}

static
void end_region(void)
{
    if(ctx.curr_region_idx == 0) {
        print_parse_error("Attempted to exit top-level region");
        error_exit();
    }
    --ctx.curr_region_idx;
}

static
void push_declaration(Declaration* decl)
{
    append_decl(&curr_region(ctx), decl);
}

static
StringToken get_decl_name(const Declaration* decl)
{
    StringToken name = 0;
    switch(decl->kind) {
        case DECL_TYPE:
            name = ((TypeDecl*)decl)->name;
            break;
        case DECL_OBJECT:
            name = ((ObjectDecl*)decl)->name;
            break;
        case DECL_SUBPROGRAM:
            name = ((SubprogramDecl*)decl)->name;
            break;
        case DECL_LABEL:
            name = ((LabelDecl*)decl)->name;
            break;
        default:
            assert(false && "Unhandled declaration type");
    }
    return name;
}

static
Declaration* find_visible_declaration(StringToken name, DeclKind kind)
{
    for(int stack_idx = ctx.curr_region_idx; stack_idx >= 0; --stack_idx) {
        DeclList* region = &ctx.region_stack[stack_idx];
        for(Declaration* decl = region->first; decl != NULL; decl = decl->next) {
            if(decl->kind == kind) {
                StringToken decl_name = get_decl_name(decl);
                if(decl_name == name) {
                    return decl;
                }
            }
        }
    }
    return NULL;
}

static
Declaration* find_declaration(Declaration* decl_list, StringToken name)
{
    for(Declaration* decl = decl_list; decl != NULL; decl = decl->next) {
        StringToken decl_name = get_decl_name(decl);
        if(decl_name == name) {
            return decl;
        }
    }
    return NULL;
}

static
Declaration* find_declaration_in_current_region(StringToken name)
{
    DeclList* region = &ctx.region_stack[ctx.curr_region_idx];
    return find_declaration(region->first, name);
}

static
void next_token(void)
{
    ctx.curr = lexer_parse_token(ctx.input_end, ctx.curr, &ctx.token);
}

static
void expect_token(TokenKind kind)
{
    if(ctx.token.kind != kind) {
        print_unexpected_token_error(&ctx.token);
        print_parse_error("Expected token: '%s'", token_kind_to_str(kind));
        error_exit();
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
                    error_print(token.line_num, "Enumeration type has too many literals to be processed (max supported is 2**32-1 literals)");
                    error_exit();
                }
                ++literal_count;
                break;
            case TOKEN_COMMA:
                break;
            default:
                print_unexpected_token_error(&token);
                error_exit();
        }
        curr = lexer_parse_token(ctx.input_end, curr, &token);
    }
    return literal_count;
}

static
uint8_t count_alternatives(void)
{
    uint8_t alt_count = 1;
    Token token = ctx.token;
    const char* curr = ctx.curr;
    while(token.kind != TOKEN_ARROW) {
        switch(token.kind) {
            case TOKEN_BAR:
                if(alt_count == UINT8_MAX) {
                    error_print(token.line_num, "Case has too many alternatives to be processed (max supported is 255 alternatives)");
                    error_exit();
                }
                ++alt_count;
                break;
            default:
                break;
        }
        curr = lexer_parse_token(ctx.input_end, curr, &token);
    }
    return alt_count;
}

static
void print_unexpected_token_error(const Token* token)
{
    StringView token_str = token_to_str(token);
    error_print(token->line_num, "Unexpected token: '%.*s'", SV(token_str));
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
