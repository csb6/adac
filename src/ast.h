#ifndef ADA_AST_H
#define ADA_AST_H

#include <stdbool.h>
#include <stdint.h>
#include "mini-gmp.h"
#include "string_view.h"

struct Declaration_;
struct Type_;
struct Expression_;

/* PACKAGES */

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
    TYPE_DERIVED_TYPE,
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
    struct Type_* inner_type;
    // TODO: constraints
} SubType;

// 3.5: Scalar Types
typedef struct {
    struct Expression_* lower_bound;
    struct Expression_* upper_bound; // inclusive
} IntRange;

typedef struct {
    IntRange range;
} IntType;

// 3.6: Array types
typedef struct {
    struct Type_* index_type;
    struct Type_* component_type;
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
    struct Type_* inner_type;
} AccessType;

typedef struct Type_ {
    TypeKind kind;
    union {
        IntType int_;
        SubType subtype;
        ArrayType array;
        RecordType record;
        AccessType access;
        StringView placeholder_name;
    } u;
} Type;

extern Type universal_int_type;

/* DECLARATIONS */

typedef uint8_t DeclKind;
enum {
    // 3.2: Objects and Named Numbers
    DECL_OBJECT,
    // 3.3: Types and Subtypes
    DECL_FULL_TYPE,
};

// 3.2: Objects and Named Numbers
typedef struct {
    StringView identifier;
    Type* type;
    struct Expression_* init_expr;
    bool is_constant;
} ObjectDecl;

// 3.3.1: Type Declarations
typedef struct {
    StringView name;
    Type* type;
    // TODO: discriminant_part
} TypeDecl;

typedef struct Declaration_ {
    DeclKind kind;
    union {
        ObjectDecl object;
        TypeDecl type;
    } u;
    struct Declaration_* next;
} Declaration;

/* EXPRESSIONS */

typedef uint8_t ExprKind;
enum {
    EXPR_INT_LIT
};

typedef struct {
    mpz_t value;
} IntLiteral;

typedef struct Expression_ {
    ExprKind kind;
    union {
        IntLiteral int_lit;
    } u;
} Expression;

#endif /* ADA_AST_H */
