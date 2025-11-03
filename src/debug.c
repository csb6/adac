#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include "ast.h"

static void print_declaration(const Declaration* decl);
static void print_type_decl(const TypeDecl* type);
static void print_expression(const Expression* expr);

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
        case DECL_TYPE:
            printf("Type declaration (name: %.*s, type: ", decl->u.type.name.len, decl->u.type.name.value);
            print_type_decl(&decl->u.type);
            putchar(')');
            break;
        case DECL_OBJECT:
            printf("Object declaration (%.*s: ", decl->u.object.name.len, decl->u.object.name.value);
            if(decl->u.object.is_constant) {
                printf("constant ");
            }
            printf("%.*s", decl->u.object.type->name.len, decl->u.object.type->name.value);
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
            printf("%.*s (placeholder)", type_decl->u.placeholder_name.len, type_decl->u.placeholder_name.value);
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
            printf("subtype (base: %.*s)", type_decl->u.subtype.base->name.len, type_decl->u.subtype.base->name.value);
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
            printf("%.*s", expr->u.enum_lit.len, expr->u.enum_lit.value);
            break;
        default:
            printf("Unhandled expression");
    }
}
