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
#ifndef ADA_AST_H
#define ADA_AST_H

#include <stdbool.h>
#include <stdint.h>
#include "error.h"
#include "mini-gmp.h"
#include "string_view.h"
#include "string_pool.h"

struct Declaration_;
struct TypeDecl_;
struct Statement_;
struct Expression_;
struct EnumLiteral_;

/* TYPES */

typedef uint8_t TypeKind;
enum {
    TYPE_UNIV_INTEGER, TYPE_UNIV_FLOAT_PT, TYPE_UNIV_FIXED_PT,
    TYPE_SUBTYPE,
    TYPE_DERIVED,
    TYPE_ENUM, TYPE_INTEGER, TYPE_FLOAT_PT, TYPE_FIXED_PT,
    TYPE_ARRAY,
    TYPE_RECORD,
    TYPE_ACCESS, TYPE_INCOMPLETE,

    TYPE_KIND_COUNT
};

typedef struct {
    struct TypeDecl_* base;
    // TODO: constraints
} SubType;

typedef struct {
    struct EnumLiteral_* literals;
    uint32_t literal_count;
} EnumType;

typedef struct {
    struct Expression_* range; // Must be a BinaryExpr with OP_RANGE
} IntType;

typedef struct {
    struct TypeDecl_* index_type;
    struct TypeDecl_* component_type;
    // TODO: constraints
    // TODO: multidimensional arrays
} ArrayType;

typedef struct {
    int placeholder;
    // TODO: variant
    // TODO: fields
    // TODO: discriminant (doesn't always mean there's a variant)
} RecordType;

typedef struct {
    struct TypeDecl_* inner_type;
} AccessType;

extern struct TypeDecl_ universal_int_type;

/* EXPRESSIONS */

typedef uint8_t ExprKind;
enum {
    EXPR_INT_LIT, EXPR_CHAR_LIT, EXPR_ENUM_LIT, EXPR_NAME, EXPR_STRING_LIT,
    EXPR_UNARY, EXPR_BINARY, EXPR_OBJECT, EXPR_CALL, EXPR_QUALIFIED,
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
    // Note: not truly operators, but for convenience when parsing we act as if they are
    OP_RANGE,

    BINARY_OP_COUNT
};

typedef struct {
    mpz_t value;
    struct TypeDecl_* type; // Initially NULL, set in checker based on context
} IntLiteral;

typedef struct {
    struct Expression_* right;
    UnaryOperator op;
} UnaryExpr;

typedef struct {
    struct Expression_* left;
    struct Expression_* right;
    BinaryOperator op;
} BinaryExpr;

typedef struct {
    struct Expression_** args; // array of Expression*
    uint32_t arg_count;
    // Bitset representing which of up to 32 available overloads are viable
    // candidates
    // Note: assumes overloads won't be added/removed when bitset is in use
    uint32_t overload_candidates;
    StringToken name;
} NameExpr;

#define MAX_OVERLOAD_CANDIDATES 32u

typedef struct {
    struct SubprogramDecl_* subprogram;
    struct Expression_** args; // array of Expression*
} CallExpr;

typedef struct {
    struct Expression_* expr;
    struct TypeDecl_* type;
} QualifiedExpr;

typedef struct Expression_ {
    SourceLocation loc;
    ExprKind kind;
    union {
        IntLiteral int_lit;
        char char_lit;
        StringView string_lit;
        UnaryExpr unary;
        BinaryExpr binary;
        NameExpr name;
        struct EnumLiteral_* enum_lit;
        struct ObjectDecl_* object;
        CallExpr call;
        QualifiedExpr qualified;
    } u;
} Expression;

/* DECLARATIONS */

typedef uint8_t DeclKind;
enum {
    DECL_OBJECT, DECL_ENUM_LIT, DECL_TYPE, DECL_SUBPROGRAM, DECL_LABEL, DECL_PKG_SPEC,
    DECL_PKG_BODY, DECL_USE, DECL_RENAME,
};

typedef struct Declaration_ {
    struct Declaration_* next; // Next decl in region
    struct Declaration_* next_overload; // Next declaration with same name
    SourceLocation loc;
    DeclKind kind;
} Declaration;

typedef uint8_t ParamMode;
enum {
    PARAM_MODE_NONE, PARAM_MODE_IN, PARAM_MODE_OUT, PARAM_MODE_IN_OUT
};

typedef struct TypeDecl_ {
    Declaration base;
    StringToken name;
    // TODO: discriminant_part
    TypeKind kind;
    union {
        EnumType enum_;
        IntType int_;
        SubType subtype;
        ArrayType array;
        RecordType record;
        AccessType access;
    } u;
} TypeDecl;

typedef struct ObjectDecl_ {
    Declaration base;
    TypeDecl* type;
    Expression* init_expr;
    StringToken name;
    bool is_constant;
    ParamMode mode; // Only used if ObjectDecl is a formal parameter
} ObjectDecl;

// Represents a single enum value within an enum type
typedef struct EnumLiteral_ {
    Declaration base;
    TypeDecl* type; // Must be an EnumType
    StringToken name;
    bool is_char_lit;
} EnumLiteral;

typedef struct SubprogramDecl_ {
    Declaration base;
    TypeDecl* return_type; // NULL for procedures
    struct Declaration_* decls; // Parameters are the first param_count decls
    struct Statement_* stmts;
    StringToken name;
    uint8_t param_count;
    bool is_operator;
} SubprogramDecl;

typedef struct {
    Declaration base;
    struct Statement_* target;
    StringToken name;
    bool is_placeholder;
} LabelDecl;

typedef struct PackageSpec_ {
    Declaration base;
    StringToken name;
    Declaration* decls;
} PackageSpec;

typedef struct {
    Declaration base;
    StringToken name;
    Declaration* decls;
    struct Statement_* stmts;
} PackageBody;

typedef struct {
    Declaration base;
    PackageSpec* package_spec;
} UseClause;

typedef struct {
    Declaration base;
    Expression target; // Must be a NameExpr
    StringToken name;
} RenameDecl;

/* STATEMENTS */

typedef uint8_t StmtKind;
enum {
    STMT_NULL, STMT_ASSIGN, STMT_EXPR, STMT_EXIT, STMT_RETURN, STMT_GOTO,
    STMT_BLOCK, STMT_IF, STMT_CASE, STMT_WHILE, STMT_FOR
};

typedef struct {
    Expression dest; // Must be a NameExpr
    Expression* expr;
} AssignStmt;

typedef struct {
    Expression* expr;
} ReturnStmt;

typedef struct {
    Expression* condition; // Can be NULL
} ExitStmt;

typedef struct {
    struct Declaration_* decls;
    struct Statement_* stmts;
} BlockStmt;

typedef struct IfStmt_ {
    Expression* condition;
    struct Statement_* stmts;
    struct Statement_* else_; // Either an IfStmt (for elsif block) or a linked list of Statements (for else block)
} IfStmt;

typedef uint8_t ChoiceKind;
enum {
    // TODO: component_simple_name
    CHOICE_EXPR, CHOICE_OTHERS
};

typedef struct {
    ChoiceKind kind;
    union {
        Expression* expr;
    } u;
} Choice;

typedef struct {
    Choice* choices;
    uint8_t count;
} Choices;

typedef struct Alternative_ {
    Choices choices;
    struct Statement_* stmts;
    struct Alternative_* next;
} Alternative;

typedef struct {
    Expression* expr;
    Alternative* cases;
} CaseStmt;

typedef struct {
    Expression* condition; // Must be boolean
    struct Statement_* stmts;
} WhileLoop;

typedef struct {
    ObjectDecl* var;
    Expression* range;
    struct Statement_* stmts;
    bool reverse;
} ForLoop;

typedef struct {
    LabelDecl* label;
} GotoStmt;

typedef struct Statement_ {
    SourceLocation loc;
    StmtKind kind;
    union {
        AssignStmt assign;
        Expression expr; // Must be a NameExpr or a CallExpr
        ReturnStmt return_;
        ExitStmt exit;
        BlockStmt block;
        IfStmt if_;
        CaseStmt case_;
        WhileLoop while_;
        ForLoop for_;
        GotoStmt goto_;
    } u;
    struct Statement_* next;
} Statement;

/* COMPILATION UNITS */

typedef uint8_t CompilationUnitKind;
enum {
    COMP_UNIT_SUBPROGRAM,
    COMP_UNIT_PACKAGE_SPEC,
    COMP_UNIT_PACKAGE_BODY,
};

typedef struct CompilationUnit_ {
    CompilationUnitKind kind;
    union {
        SubprogramDecl* subprogram_decl;
        PackageSpec* package_spec;
        PackageBody* package_body;
        // TODO: secondary_unit, generics
    } u;
} CompilationUnit;

#endif /* ADA_AST_H */
