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
#ifndef ADA_AST_H
#define ADA_AST_H

#include <stdbool.h>
#include <stdint.h>
#include "mini-gmp.h"
#include "string_view.h"

struct Declaration_;
struct TypeDecl_;
struct Type_;
struct Expression_;

/* PACKAGES */

// 7.1: Package Structure
typedef struct PackageSpec_ {
    StringView name;
    struct Declaration_* decls; // TODO: support representation_clause/use_clause in this list
} PackageSpec;

/* TYPES */

typedef uint8_t TypeKind;
enum {
    // 3.2: Objects and Named Numbers
    TYPE_UNIV_INTEGER, TYPE_UNIV_FLOAT_PT, TYPE_UNIV_FIXED_PT,
    // 3.3: Types and Subtypes
    TYPE_SUBTYPE,
    // 3.4: Derived Types
    TYPE_DERIVED,
    // 3.5: Scalar types
    TYPE_ENUM, TYPE_INTEGER, TYPE_FLOAT_PT, TYPE_FIXED_PT,
    // 3.6: Array types
    TYPE_ARRAY,
    // 3.7: Record types
    TYPE_RECORD,
    // 3.8: Access types
    TYPE_ACCESS, TYPE_INCOMPLETE,

    TYPE_PLACEHOLDER,
    TYPE_KIND_COUNT
};

// 3.3: Types and Subtypes
// 3.4: Derived Types (these are just subtypes that do not implicitly convert to other subtypes with same base type)
typedef struct {
    struct TypeDecl_* base;
    // TODO: constraints
} SubType;

// 3.5.1: Enumeration Types
typedef struct {
    struct Expression_* literals;
    uint32_t literal_count;
} EnumType;

typedef struct {
    struct Expression_* lower_bound;
    struct Expression_* upper_bound; // inclusive
} IntRange;

// 3.5.4: Integer Types
typedef struct {
    IntRange range;
} IntType;

// 3.6: Array types
typedef struct {
    struct TypeDecl_* index_type;
    struct TypeDecl_* component_type;
    // TODO: constraints
    // TODO: multidimensional arrays
} ArrayType;

// 3.7: Record types
typedef struct {
    int placeholder;
    // TODO: variant
    // TODO: fields
    // TODO: discriminant (doesn't always mean there's a variant)
} RecordType;

// 3.8: Access types
typedef struct {
    struct TypeDecl_* inner_type;
} AccessType;

extern struct TypeDecl_ universal_int_type;

/* DECLARATIONS */

typedef uint8_t DeclKind;
enum {
    DECL_OBJECT, DECL_TYPE, DECL_FUNCTION, DECL_PROCEDURE
};

// 6.2: Formal Parameter Modes
typedef uint8_t ParamMode;
enum {
    PARAM_MODE_NONE, PARAM_MODE_IN, PARAM_MODE_OUT, PARAM_MODE_IN_OUT
};

// 3.3.1: Type Declarations
typedef struct TypeDecl_ {
    StringView name;
    // TODO: discriminant_part
    TypeKind kind;
    union {
        EnumType enum_;
        IntType int_;
        SubType subtype;
        ArrayType array;
        RecordType record;
        AccessType access;
        StringView placeholder_name;
    } u;
} TypeDecl;

// 3.2: Objects and Named Numbers
typedef struct ObjectDecl_ {
    StringView name;
    TypeDecl* type;
    struct Expression_* init_expr;
    bool is_constant;
    ParamMode mode; // Only used if ObjectDecl is a formal parameter
} ObjectDecl;

typedef struct SubprogramDecl_ {
    StringView name;
    TypeDecl* return_type; // NULL for procedures
    struct Declaration_* params; // all are ObjectDecls
    bool is_operator;
} SubprogramDecl;

typedef struct Declaration_ {
    DeclKind kind;
    union {
        ObjectDecl object;
        TypeDecl type;
        SubprogramDecl subprogram;
    } u;
    struct Declaration_* next;
} Declaration;

/* EXPRESSIONS */

typedef uint8_t ExprKind;
enum {
    EXPR_INT_LIT, EXPR_CHAR_LIT, EXPR_ENUM_LIT, EXPR_STRING_LIT,
    EXPR_UNARY, EXPR_BINARY
};

typedef uint8_t UnaryOperator;
enum {
    OP_UNARY_PLUS, OP_UNARY_MINUS, OP_ABS, OP_NOT,

    UNARY_OP_COUNT
};

typedef uint8_t BinaryOperator;
enum {
    // Logical operators
    OP_AND, OP_AND_THEN, OP_OR, OP_OR_ELSE, OP_XOR,
    // Relational operators
    OP_EQ, OP_NEQ, OP_LT, OP_LTE, OP_GT, OP_GTE, OP_IN, OP_NOT_IN,
    // Binary adding operators
    OP_PLUS, OP_MINUS, OP_AMP,
    // Multiplying operators
    OP_MULT, OP_DIVIDE, OP_MOD, OP_REM,
    // Highest precedence operator
    OP_EXP,

    BINARY_OP_COUNT
};

typedef struct UnaryExpr_ {
    struct Expression_* right;
    UnaryOperator op;
} UnaryExpr;

typedef struct BinaryExpr_ {
    struct Expression_* left;
    struct Expression_* right;
    BinaryOperator op;
} BinaryExpr;

typedef struct Expression_ {
    ExprKind kind;
    union {
        // 4.2: Literals
        mpz_t int_lit;
        char char_lit;
        StringView enum_lit;
        StringView string_lit;
        UnaryExpr unary;
        BinaryExpr binary;
    } u;
} Expression;

#endif /* ADA_AST_H */
