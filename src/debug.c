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
#include <stdio.h>
#include <stdlib.h>
#include "ast.h"

static void print_declaration(const Declaration* decl);
static void print_type_decl(const TypeDecl* type);
static void print_expression(const Expression* expr);
static void print_unary_operator(UnaryOperator op);
static void print_binary_operator(BinaryOperator op);

void print_package_spec(const PackageSpec* package_spec)
{
    printf("Package: %.*s\n", SV(package_spec->name));
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
        case DECL_TYPE:
            printf("Type declaration (name: %.*s, type: ", SV(decl->u.type.name));
            print_type_decl(&decl->u.type);
            putchar(')');
            break;
        case DECL_OBJECT:
            printf("Object declaration (%.*s: ", SV(decl->u.object.name));
            if(decl->u.object.is_constant) {
                printf("constant ");
            }
            printf("%.*s", SV(decl->u.object.type->name));
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
void print_type_decl(const TypeDecl* type_decl)
{
    switch(type_decl->kind) {
        case TYPE_PLACEHOLDER:
            printf("%.*s (placeholder)", SV(type_decl->u.placeholder_name));
            break;
        case TYPE_UNIV_INTEGER:
            printf("universal integer");
            break;
        case TYPE_INTEGER:
            printf("integer (range: [");
            print_expression(type_decl->u.int_.range.lower_bound);
            printf(", ");
            print_expression(type_decl->u.int_.range.upper_bound);
            printf("])");
            break;
        case TYPE_ENUM:
            printf("enum type (");
            for(uint32_t i = 0; i < type_decl->u.enum_.literal_count; ++i) {
                print_expression(type_decl->u.enum_.literals + i);
                putchar(' ');
            }
            putchar(')');
            break;
        case TYPE_SUBTYPE:
            printf("subtype (base: %.*s)", SV(type_decl->u.subtype.base->name));
            break;
        case TYPE_DERIVED:
            printf("derived (base: %.*s)", SV(type_decl->u.subtype.base->name));
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
            char* num_str = mpz_get_str(NULL, 10, expr->u.int_lit);
            printf("%s", num_str);
            free(num_str);
            break;
        }
        case EXPR_CHAR_LIT:
            printf("'%c'", expr->u.char_lit);
            break;
        case EXPR_ENUM_LIT:
            printf("%.*s", SV(expr->u.enum_lit));
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
