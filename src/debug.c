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
#include "debug.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "ast.h"

static void print_declaration(const Declaration* decl, uint8_t indent_level);
static void print_subprogram_decl(const Declaration* decl, uint8_t indent_level);
static void print_type_decl(const TypeDecl* type);
static void print_statement(const Statement* stmt, uint8_t indent_level);
static void print_if_statement(const IfStmt* stmt, uint8_t indent_level);
static void print_case_statement(const CaseStmt* stmt, uint8_t indent_level);
static void print_expression(const Expression* expr);
static void print_unary_operator(UnaryOperator op);
static void print_binary_operator(BinaryOperator op);
static void print_params(const Declaration* params, uint8_t param_count);
static const char* param_mode_str(ParamMode mode);
static void print_indent(uint8_t size);

void print_package_spec(const PackageSpec* package_spec)
{
    printf("package %.*s is\n", SV(package_spec->name));
    for(const Declaration* decl = package_spec->decls; decl != NULL; decl = decl->next) {
        print_declaration(decl, 1);
    }
    printf("end %.*s;\n", SV(package_spec->name));
}

static
void print_declaration(const Declaration* decl, uint8_t indent_level)
{
    switch(decl->kind) {
        case DECL_TYPE:
            print_indent(indent_level);
            print_type_decl(&decl->u.type);
            break;
        case DECL_OBJECT:
            print_indent(indent_level);
            printf("%.*s : ", SV(decl->u.object.name));
            if(decl->u.object.is_constant) {
                printf("constant ");
            }
            printf("%.*s", SV(decl->u.object.type->name));
            if(decl->u.object.init_expr) {
                printf(" := ");
                print_expression(decl->u.object.init_expr);
            }
            break;
        case DECL_FUNCTION:
        case DECL_PROCEDURE:
            print_subprogram_decl(decl, indent_level);
            break;
        default:
            printf("Unknown declaration");
    }
    printf(";\n");
}

static
void print_subprogram_decl(const Declaration* decl, uint8_t indent_level)
{
    print_indent(indent_level);
    printf("%s %.*s", decl->kind == DECL_FUNCTION ? "function" : "procedure", SV(decl->u.subprogram.name));
    print_params(decl->u.subprogram.decls, decl->u.subprogram.param_count);
    if(decl->kind == DECL_FUNCTION) {
        printf(" return %.*s", SV(decl->u.subprogram.return_type->name));
    }

    Declaration* inner_decl = decl->u.subprogram.decls;
    for(uint8_t i = 0; i < decl->u.subprogram.param_count; ++i) {
        inner_decl = inner_decl->next;
    }
    // Only print body if it exists
    if(inner_decl || decl->u.subprogram.stmts) {
        printf(" is\n");
        while(inner_decl) {
            print_declaration(inner_decl, indent_level+1);
            inner_decl = inner_decl->next;
        }
        print_indent(indent_level);
        printf("begin\n");
        for(const Statement* stmt = decl->u.subprogram.stmts; stmt != NULL; stmt = stmt->next) {
            print_statement(stmt, indent_level+1);
        }
        print_indent(indent_level);
        printf("end %.*s", SV(decl->u.subprogram.name));
    }
}

static
void print_params(const Declaration* params, uint8_t param_count)
{
    putchar('(');
    const Declaration* decl = params;
    for(uint8_t i = 0; i < param_count; ++i) {
        assert(decl->kind == DECL_OBJECT);
        const ObjectDecl* param = &decl->u.object;
        printf("%.*s : %s %.*s", SV(param->name), param_mode_str(param->mode), SV(param->type->name));
        if(param->init_expr) {
            printf(" := ");
            print_expression(param->init_expr);
        }
        printf("; ");
        decl = decl->next;
    }
    putchar(')');
}

static
const char* param_mode_str(ParamMode mode)
{
    const char* mode_str = "No_mode";
    switch(mode) {
        case PARAM_MODE_IN:
            mode_str = "in";
            break;
        case PARAM_MODE_OUT:
            mode_str = "out";
            break;
        case PARAM_MODE_IN_OUT:
            mode_str = "in out";
            break;
        default:
            break;
    }
    return mode_str;
}

static
void print_indent(uint8_t size)
{
    for(uint8_t i = 0; i < size; ++i) {
        printf("  ");
    }
}

static
void print_type_decl(const TypeDecl* type_decl)
{
    printf("%s %.*s is ", type_decl->kind == TYPE_SUBTYPE ? "subtype" : "type", SV(type_decl->name));
    switch(type_decl->kind) {
        case TYPE_UNIV_INTEGER:
            printf("universal integer");
            break;
        case TYPE_INTEGER:
            printf("range ");
            print_expression(type_decl->u.int_.range.lower_bound);
            printf(" .. ");
            print_expression(type_decl->u.int_.range.upper_bound);
            break;
        case TYPE_ENUM:
            putchar('(');
            for(uint32_t i = 0; i < type_decl->u.enum_.literal_count; ++i) {
                print_expression(type_decl->u.enum_.literals + i);
                putchar(' ');
            }
            putchar(')');
            break;
        case TYPE_SUBTYPE:
            printf("%.*s", SV(type_decl->u.subtype.base->name));
            break;
        case TYPE_DERIVED:
            printf("new %.*s", SV(type_decl->u.subtype.base->name));
            break;
        default:
            printf("Unhandled type");
    }
}

static
void print_statement(const Statement* stmt, uint8_t indent_level)
{
    print_indent(indent_level);
    switch(stmt->kind) {
        case STMT_NULL:
            printf("null");
            break;
        case STMT_ASSIGN:
            printf("%.*s := ", SV(stmt->u.assign.dest->name));
            print_expression(stmt->u.assign.expr);
            break;
        case STMT_RETURN:
            printf("return");
            if(stmt->u.return_.expr) {
                putchar(' ');
                print_expression(stmt->u.return_.expr);
            }
            break;
        case STMT_CALL:
            printf("%.*s", SV(stmt->u.call.subprogram->name));
            if(stmt->u.call.subprogram->param_count > 0) {
                putchar('(');
                for(uint8_t i = 0; i < stmt->u.call.subprogram->param_count; ++i) {
                    print_expression(stmt->u.call.args[i]);
                    printf(", ");
                }
                putchar(')');
            }
            break;
        case STMT_BLOCK:
            if(stmt->u.block.decls) {
                printf("declare\n");
                for(const Declaration* decl = stmt->u.block.decls; decl != NULL; decl = decl->next) {
                    print_declaration(decl, indent_level+1);
                }
                print_indent(indent_level);
            }
            printf("begin\n");
            for(const Statement* s = stmt->u.block.stmts; s != NULL; s = s->next) {
                print_statement(s, indent_level+1);
            }
            print_indent(indent_level);
            printf("end");
            break;
        case STMT_IF:
            print_if_statement(&stmt->u.if_, indent_level);
            break;
        case STMT_CASE:
            print_case_statement(&stmt->u.case_, indent_level);
            break;
        default:
            printf("Unhandled statement");
    }
    printf(";\n");
}

static
void print_if_statement(const IfStmt* stmt, uint8_t indent_level)
{
    printf("if ");
    print_expression(stmt->condition);
    printf(" then\n");
    for(const Statement* s = stmt->stmts; s != NULL; s = s->next) {
        print_statement(s, indent_level+1);
    }
    const Statement* block = stmt->else_;
    while(block) {
        if(block->kind == STMT_IF) {
            print_indent(indent_level);
            printf("elsif ");
            print_expression(block->u.if_.condition);
            printf(" then\n");
            for(const Statement* s = block->u.if_.stmts; s != NULL; s = s->next) {
                print_statement(s, indent_level+1);
            }
            block = block->u.if_.else_;
        } else {
            print_indent(indent_level);
            assert(block->kind == STMT_BLOCK);
            printf("else\n");
            for(const Statement* s = block->u.block.stmts; s != NULL; s = s->next) {
                print_statement(s, indent_level+1);
            }
            block = NULL;
        }
    }
    print_indent(indent_level);
    printf("end if");
}

static
void print_case_statement(const CaseStmt* stmt, uint8_t indent_level)
{
    printf("case ");
    print_expression(stmt->expr);
    printf(" is\n");
    for(const Case* case_ = stmt->cases; case_ != NULL; case_ = case_->next) {
        print_indent(indent_level+1);
        printf("when ");
        print_expression(case_->choice);
        printf(" =>\n");
        for(const Statement* s = case_->stmts; s != NULL; s = s->next) {
            print_statement(s, indent_level+2);
        }
    }
    print_indent(indent_level);
    printf("end case");
}

static
void print_expression(const Expression* expr)
{
    switch(expr->kind) {
        case EXPR_INT_LIT: {
            char* num_str = mpz_get_str(NULL, 10, expr->u.int_lit);
            printf("%s", num_str);
            free(num_str);
            break;
        }
        case EXPR_CHAR_LIT:
            printf("'%c'", expr->u.char_lit);
            break;
        case EXPR_NAME:
            printf("%.*s", SV(expr->u.name));
            break;
        case EXPR_STRING_LIT:
            printf("\"%.*s\"", SV(expr->u.string_lit));
            break;
        case EXPR_UNARY:
            putchar('(');
            print_unary_operator(expr->u.unary.op);
            print_expression(expr->u.unary.right);
            putchar(')');
            break;
        case EXPR_BINARY:
            putchar('(');
            print_expression(expr->u.binary.left);
            print_binary_operator(expr->u.binary.op);
            print_expression(expr->u.binary.right);
            putchar(')');
            break;
        default:
            printf("Unhandled expression");
    }
}

static const char* const unary_op_str[UNARY_OP_COUNT] = {
    [OP_UNARY_PLUS] = "+",
    [OP_UNARY_MINUS] = "-",
    [OP_ABS] = "abs",
    [OP_NOT] = "not"
};

static const char* const binary_op_str[BINARY_OP_COUNT] = {
    // Logical operators
    [OP_AND]      = "and",
    [OP_AND_THEN] = "and then",
    [OP_OR]       = "or",
    [OP_OR_ELSE]  = "or else",
    [OP_XOR]      = "xor",
    // Relational operators
    [OP_EQ]       = "=",
    [OP_NEQ]      = "/=",
    [OP_LT]       = "<",
    [OP_LTE]      = "<=",
    [OP_GT]       = ">",
    [OP_GTE]      = ">=",
    [OP_IN]       = "in",
    [OP_NOT_IN]   = "not in",
    // Binary adding operators
    [OP_PLUS]     = "+",
    [OP_MINUS]    = "-",
    [OP_AMP]      = "&",
    // Multiplying operators
    [OP_MULT]     = "*",
    [OP_DIVIDE]   = "/",
    [OP_MOD]      = "mod",
    [OP_REM]      = "rem",
    // Highest precedence operator
    [OP_EXP]      = "**"
};

static
void print_binary_operator(BinaryOperator op)
{
    if(op >= BINARY_OP_COUNT) {
        printf(" unknown_op ");
    } else {
        printf(" %s ", binary_op_str[op]);
    }
}

static
void print_unary_operator(UnaryOperator op)
{
    if(op >= UNARY_OP_COUNT) {
        printf("unknown_op ");
    } else {
        printf("%s ", unary_op_str[op]);
    }
}
