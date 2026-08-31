/******* A YACC grammar for Ada 9X *********************************/
/* Copyright (C) Intermetrics, Inc. 1994 Cambridge, MA  USA        */
/* Copying permitted if accompanied by this statement.             */
/* Derivative works are permitted if accompanied by this statement.*/
/* This grammar is thought to be correct as of May 1, 1994         */
/* but as usual there is *no warranty* to that effect.             */
/*                                                                 */
/* Ada 83 grammar (adapted from the Ada 9X grammar)                */
/* Copyright 2026 Cole Blakley                                     */
/*******************************************************************/

/* Print descriptive error messages */
%define parse.error detailed
/* Improve syntax errors by doing exploratory parsing before
   running semantic actions */
%define parse.lac full
/* Don't use globals */
%define api.pure true
%define api.header.include {"parser.h"}
%locations
%define api.location.type {SourceLocation}
/* Add extra parameter to yyparse() and yylex() */
%param {void* scanner}
%parse-param {ParseContext* context}

// Emitted in the header file before the definition of YYSTYPE.
%code requires {
    #include <stdint.h>
    #include <stdbool.h>
    #include <ctype.h>
    #include "array.h"
    #include "linked_list.h"
    #include "ast.h"
    #include "comp_manager.h"

    DEFINE_ARRAY_TYPE(StringToken)

    typedef Declaration Decl;
    DEFINE_LINKED_LIST_TYPE(Decl)

    #define YYLLOC_DEFAULT(Cur, Rhs, N) \
        do { \
            if(N > 0) { \
                (Cur) = YYRHSLOC(Rhs, 1); \
            } else { \
                (Cur) = YYRHSLOC(Rhs, 0); \
            } \
        } while (0);

    typedef struct ParseContext_ {
        CompilationManager* comp_manager;
        CompilationUnit* comp_unit;
        uint16_t file_id;
        uint8_t curr_scope_idx;
    } ParseContext;

    Declaration* find_bucket(ParseContext* context, StringToken name);

    ObjectDecl* find_object_decl(ParseContext* context, StringToken name);
}

// Emitted in the header file after the definition of YYSTYPE.
%code provides {
    void yyerror(YYLTYPE* yyloc, void* scanner, ParseContext* parse_ctx, const char* msg);
}

// Emitted in the implementation file
%code {
    #include <assert.h>
    #include <stdlib.h>
    #include <stdbool.h>
    #include "error.h"
    #include "string_pool.h"
    #include "string_view.h"
    #include "lexer.h"

    DEFINE_ARRAY_OPS(StringToken)
    DEFINE_LINKED_LIST_OPS(Decl)
}

%union {
    StringToken str_token;
    char c;
    StringView str; // Note: this StringView owns its allocated data
    StringTokenArray str_token_array;
}

/* Terminals */
%type <c> char_lit;
%type <str_token> identifier goto_label identifier_opt
%type <str> char_string numeric_lit

%type <str_token_array> def_id_s

/* Multi-character operators */
%token DOT_DOT BOX LT_EQ EXPON NE GE IS_ASSIGNED RIGHT_SHAFT
/* Keywords */
%token ABORT ABS ACCEPT ACCESS ALL AND ARRAY AT BEGiN BODY CASE CONSTANT DECLARE DELAY DELTA DIGITS DO
       ELSE ELSIF END ENTRY EXCEPTION EXIT FOR FUNCTION GENERIC GOTO IF IN IS LIMITED LOOP MOD NEW NOT
       NuLL OF OR OTHERS OUT PACKAGE PRAGMA PRIVATE PROCEDURE RAISE RANGE RECORD REM RENAMES RETURN
       REVERSE SELECT SEPARATE SUBTYPE TASK TERMINATE THEN TYPE USE WHEN WHILE WITH XOR
/* Tokens using yylval */
%token char_lit identifier char_string numeric_lit goto_label

%initial-action {
    @$.file_id = context->file_id;
    @$.line_num = 1;
    // Silences annoying compiler warning
    (void)yynerrs;
}

%%

goal_symbol : comp_unit
    ;

pragma :
    PRAGMA identifier ';'
  | PRAGMA identifier '(' pragma_arg_s ')' ';'
    ;

pragma_arg_s :
    pragma_arg
  | pragma_arg_s ',' pragma_arg
    ;

pragma_arg :
    expression
  | identifier RIGHT_SHAFT expression
    ;

pragma_s :
      %empty
    | pragma_s pragma
    ;

decl :
    object_decl
  | number_decl
  | type_decl
  | subtype_decl
  | subprog_decl
  | pkg_decl
  | exception_decl
  | rename_decl
  | generic_decl
  | body_stub
  | error ';'
    ;

object_decl :
    def_id_s ':' object_qualifier_opt object_subtype_def init_opt ';'
    ;

number_decl :
    def_id_s ':' CONSTANT IS_ASSIGNED expression ';'
    ;

def_id_s :
    identifier {
        StringTokenArray_init(&$$);
        StringTokenArray_append(&$$, $identifier);
    }
  | def_id_s ',' identifier {
        $$ = $1;
        StringTokenArray_append(&$$, $identifier);
    };

// boolean attribute indicates whether object is a constant or not
object_qualifier_opt :
    %empty
  | CONSTANT
    ;

object_subtype_def :
    subtype_ind
  | array_type
    ;

init_opt :
    %empty
  | IS_ASSIGNED expression
    ;

type_decl :
    TYPE identifier discrim_part_opt type_completion ';'
    ;

discrim_part_opt :
    %empty
  | discrim_part
  | '(' BOX ')'
    ;

// TODO: incomplete types (i.e. case 1)
type_completion :
    %empty
  | IS type_def
    ;

type_def :
    enumeration_type
  | integer_type
  | real_type
  | array_type
  | record_type
  | access_type
  | derived_type
  | private_type
    ;

subtype_decl :
    SUBTYPE identifier IS subtype_ind ';'
    ;

// TODO: support other name variants (e.g. indexed, compound)
subtype_ind :
    name constraint
  | name
  ;

constraint :
    range_constraint
  | decimal_digits_constraint
    ;

decimal_digits_constraint :
    DIGITS expression range_constr_opt
    ;

derived_type :
    NEW subtype_ind
    ;

range_constraint :
    RANGE range
    ;

range_constr_opt :
    %empty
  | range_constraint
    ;

range :
    simple_expression[left] DOT_DOT simple_expression[right]
  | name '\'' RANGE
  | name '\'' RANGE '(' expression ')'
    ;

enumeration_type :
    '(' enum_id_s ')'
    ;

enum_id_s :
    enum_id
  | enum_id_s[left] ',' enum_id
  ;

enum_id :
    identifier
  | char_lit
  ;

integer_type :
    range_constraint
  | MOD expression
    ;

real_type :
    float_type
  | fixed_type
    ;

float_type :
    DIGITS expression range_constr_opt
    ;

fixed_type :
    DELTA expression range_constraint
  | DELTA expression DIGITS expression range_constr_opt
    ;

array_type :
    unconstr_array_type
  | constr_array_type
    ;

unconstr_array_type :
    ARRAY '(' index_s ')' OF component_subtype_def
    ;

constr_array_type :
    ARRAY iter_index_constraint OF component_subtype_def
    ;

component_subtype_def :
    subtype_ind
    ;

index_s :
    index
  | index_s ',' index
    ;

index :
    name RANGE BOX
    ;

iter_index_constraint :
    '(' iter_discrete_range_s ')'
    ;

iter_discrete_range_s :
    discrete_range
  | iter_discrete_range_s ',' discrete_range
    ;

discrete_range :
    name range_constr_opt
  | range
    ;

record_type :
    limited_opt record_def
    ;

record_def :
    RECORD pragma_s comp_list END RECORD
  | NuLL RECORD
    ;

comp_list :
    comp_decl_s variant_part_opt
  | variant_part pragma_s
  | NuLL ';' pragma_s
    ;

comp_decl_s :
    comp_decl
  | comp_decl_s pragma_s comp_decl
    ;

variant_part_opt :
    pragma_s
  | pragma_s variant_part pragma_s
    ;

comp_decl :
    def_id_s ':' component_subtype_def init_opt ';'
  | error ';'
    ;

discrim_part :
    '(' discrim_spec_s ')'
    ;

discrim_spec_s :
    discrim_spec
  | discrim_spec_s ';' discrim_spec
    ;

discrim_spec :
    def_id_s ':' access_opt mark init_opt
  | error
    ;

access_opt :
    %empty
  | ACCESS
    ;

variant_part :
    CASE identifier IS pragma_s variant_s END CASE ';'
    ;

variant_s :
    variant
  | variant_s variant
    ;

variant :
    WHEN choice_s RIGHT_SHAFT pragma_s comp_list
    ;

choice_s :
    choice
  | choice_s[left] '|' choice
  ;

choice :
    expression
  | discrete_with_range
  | OTHERS
  ;

discrete_with_range :
    name range_constraint
  | range
    ;

access_type :
    ACCESS subtype_ind
  | ACCESS CONSTANT subtype_ind
    ;

decl_part :
    %empty
  | decl_item_or_body_s1
    ;

decl_item_s :
    %empty
  | decl_item_s1
    ;

decl_item_s1 :
    decl_item
  | decl_item_s1 decl_item
  ;

decl_item :
    decl
  | use_clause
  | rep_spec
  | pragma
    ;

decl_item_or_body_s1 :
    decl_item_or_body
  | decl_item_or_body_s1 decl_item_or_body
  ;

decl_item_or_body :
    body
  | decl_item
    ;

body :
    subprog_body
  | pkg_body
    ;

name :
    identifier
  | indexed_comp
  | selected_comp
  | attribute
  | operator_symbol
  ;

mark :
    identifier
  | mark '\'' attribute_id
  | mark '.' identifier
    ;

used_char :
    char_lit
    ;

operator_symbol :
    char_string
    ;

indexed_comp :
    name '(' value_s ')'
    ;

value_s :
    value
  | value_s ',' value
    ;

value :
    expression
  | comp_assoc
  | discrete_with_range
  | error
    ;

selected_comp :
    name '.' identifier
  | name '.' used_char
  | name '.' operator_symbol
  | name '.' ALL
    ;

attribute :
    name '\'' attribute_id
    ;

attribute_id :
    identifier
  | DIGITS
  | DELTA
  | ACCESS
    ;

literal :
    numeric_lit
  | used_char
  | NuLL
    ;

aggregate :
    '(' comp_assoc ')'
  | '(' value_s_2 ')'
  | '(' expression WITH value_s ')'
  | '(' expression WITH NuLL RECORD ')'
  | '(' NuLL RECORD ')'
    ;

value_s_2 :
    value ',' value
  | value_s_2 ',' value
    ;

comp_assoc :
    choice_s RIGHT_SHAFT expression
    ;

expression :
    relation
  | expression[left] logical[op] relation[right]
  | expression[left] short_circuit[op] relation[right]
    ;

logical :
    AND
  | OR
  | XOR
    ;

short_circuit :
      AND THEN
    | OR ELSE
    ;

// TODO: constant folding of literals
relation :
    simple_expression
  | simple_expression[left] relational[op] simple_expression[right]
  | simple_expression[left] membership[op] range[right]
  | simple_expression[left] membership[op] name
  ;

relational :
    '='
  | NE
  | '<'
  | LT_EQ
  | '>'
  | GE
    ;

membership :
    IN
  | NOT IN
    ;

simple_expression :
    term
  | unary[op] term
  | simple_expression[left] adding[op] term[right]
    ;

unary :
    '+'
  | '-'
    ;

adding :
    '+'
  | '-'
  | '&'
    ;

term :
    factor
  | term[left] multiplying[op] factor[right]
    ;

multiplying :
    '*'
  | '/'
  | MOD
  | REM
    ;

factor :
    primary
  | NOT primary
  | ABS primary
  | primary[left] EXPON primary[right]
    ;

primary :
    literal
  | name
  | allocator
  | qualified
  | parenthesized_primary
    ;

parenthesized_primary :
    aggregate
  | '(' expression ')'
    ;

qualified :
    name '\'' parenthesized_primary[expr]
    ;

allocator :
    NEW name
  | NEW qualified
    ;

statement_s :
    statement
  | statement_s[left] statement
  ;

statement :
    unlabeled
  | goto_label statement
  ;

unlabeled :
    simple_stmt
  | compound_stmt
  | pragma
    ;

simple_stmt :
    null_stmt
  | assign_stmt
  | exit_stmt
  | return_stmt
  | goto_stmt
  | procedure_call
  | raise_stmt
  | code_stmt
  | error ';'
    ;

compound_stmt :
    if_stmt
  | case_stmt
  | loop_stmt
  | block
    ;

null_stmt :
    NuLL ';'
    ;

assign_stmt :
    name IS_ASSIGNED expression ';'
    ;

if_stmt :
    IF cond_clause_s else_opt END IF ';'
    ;

cond_clause_s :
    cond_clause
  | cond_clause_s[if] ELSIF cond_clause[elsif]
  ;

cond_clause :
    condition THEN statement_s
    ;

condition :
    expression
    ;

else_opt :
    %empty
  | ELSE statement_s
    ;

case_stmt :
    case_hdr pragma_s alternative_s END CASE ';'
    ;

case_hdr :
    CASE expression IS
    ;

alternative_s :
    %empty
  | alternative_s[left] alternative
  ;

alternative :
    WHEN choice_s RIGHT_SHAFT statement_s
    ;

// TODO: label_opt and id_opt
loop_stmt :
    label_opt loop_content id_opt ';'
    ;

label_opt :
    %empty
  | identifier ':'
    ;

loop_content :
    basic_loop
  | WHILE condition basic_loop
  | FOR identifier IN reverse_opt discrete_range basic_loop
  ;

reverse_opt :
    %empty
  | REVERSE
    ;

basic_loop :
    LOOP statement_s END LOOP
    ;

id_opt :
    %empty
  | designator
    ;

// TODO: label
block :
    label_opt block_decl block_body END id_opt ';'
    ;

block_decl :
    %empty
  | DECLARE decl_part
  ;

block_body :
    BEGiN handled_stmt_s
    ;

// TODO: exception handler
handled_stmt_s :
    statement_s except_handler_part_opt
    ;

except_handler_part_opt :
    %empty
  | except_handler_part
    ;

exit_stmt :
    EXIT name_opt when_opt ';'
    ;

name_opt :
    %empty
  | name
    ;

when_opt :
    %empty
  | WHEN condition
    ;

return_stmt :
    RETURN ';'
  | RETURN expression ';'
  ;

goto_stmt :
    GOTO identifier ';'
    ;

subprog_decl :
    subprog_spec ';'
  | generic_subp_inst ';'
    ;

// TODO: process formal_part_opt
subprog_spec :
    PROCEDURE identifier formal_part_opt
  | FUNCTION designator formal_part_opt RETURN name
  | FUNCTION designator  /* for generic inst and generic rename */
    ;

designator :
    identifier
  | char_string
    ;

formal_part_opt :
    %empty
  | formal_part
    ;

formal_part :
    '(' param_s ')'
    ;

param_s :
    param
  | param_s ';' param
    ;

param :
    def_id_s ':' mode mark init_opt
  | error
    ;

mode :
    %empty
  | IN
  | OUT
  | IN OUT
    ;

subprog_spec_is_push :
    subprog_spec IS
    ;

// TODO: params will be pushed twice (one in forward decl, if any, and again in subprog_body)
//  Need to somehow check if a forward decl was already made; if so, don't push params again
subprog_body :
    subprog_spec_is_push decl_part block_body END id_opt ';'
    ;

procedure_call :
    name ';'
    ;

pkg_decl :
    pkg_spec ';'
  | generic_pkg_inst ';'
    ;

pkg_spec :
    PACKAGE identifier IS decl_item_s private_part END identifier_opt;

private_part :
    %empty
  | PRIVATE decl_item_s
    ;

identifier_opt :
    %empty
  | identifier
    ;

pkg_body :
    PACKAGE BODY identifier IS decl_part body_opt END identifier_opt ';'
    ;

body_opt :
    %empty
  | block_body
    ;

private_type :
    limited_opt PRIVATE
    ;

limited_opt :
    %empty
  | LIMITED
    ;

use_name_s :
    identifier
  | selected_comp
  | use_name_s ',' identifier
  | use_name_s ',' selected_comp
  ;

use_clause :
    USE use_name_s ';'
    ;

// Note: def_id_s is used instead of identifier to avoid shift/reduce conflict
rename_decl :
    def_id_s ':' object_qualifier_opt subtype_ind RENAMES name ';'
  | def_id_s ':' EXCEPTION RENAMES name ';'
  | rename_unit
    ;

rename_unit :
    PACKAGE identifier RENAMES name ';'
  | subprog_spec RENAMES name ';'
  | generic_formal_part PACKAGE identifier RENAMES name ';'
  | generic_formal_part subprog_spec RENAMES name ';'
    ;

comp_unit :
    pragma_s context_spec unit pragma_s
  | pragma_s unit pragma_s
    ;

context_spec :
    with_clause use_clause_opt
  | context_spec with_clause use_clause_opt
  | context_spec pragma
    ;

with_clause :
    WITH def_id_s ';' {
        uint32_t package_count = StringTokenArray_size(&$def_id_s);
        for(uint32_t i = 0; i < package_count; ++i) {
            const char* package_name = string_pool_to_str($def_id_s.data[i]);
            comp_manager_parse_spec(context->comp_manager, package_name, &@$);
        }
    };

use_clause_opt :
    %empty
  | use_clause_opt use_clause
    ;

unit :
    pkg_decl
  | pkg_body
  | subprog_decl
  | subprog_body
  | subunit
  | generic_decl
  | rename_unit
    ;

subunit :
    SEPARATE '(' identifier ')' subunit_body
    ;

subunit_body :
    subprog_body
  | pkg_body
    ;

body_stub :
    PACKAGE BODY identifier IS SEPARATE ';'
  | subprog_spec IS SEPARATE ';'
    ;

exception_decl :
    def_id_s ':' EXCEPTION ';'
    ;

except_handler_part :
    EXCEPTION exception_handler
  | except_handler_part exception_handler
    ;

exception_handler :
    WHEN except_choice_s RIGHT_SHAFT statement_s
  | WHEN identifier ':' except_choice_s RIGHT_SHAFT statement_s
    ;

except_choice_s :
    except_choice
  | except_choice_s '|' except_choice
    ;

except_choice :
    name
  | OTHERS
    ;

raise_stmt :
    RAISE name_opt ';'
    ;

generic_decl :
    generic_formal_part subprog_spec ';'
  | generic_formal_part pkg_spec ';'
    ;

generic_formal_part :
    GENERIC
  | generic_formal_part generic_formal
    ;

generic_formal :
    param ';'
  | TYPE identifier generic_discrim_part_opt IS generic_type_def ';'
  | WITH PROCEDURE identifier formal_part_opt subp_default ';'
  | WITH FUNCTION designator formal_part_opt RETURN name subp_default ';'
  | WITH PACKAGE identifier IS NEW name '(' BOX ')' ';'
  | WITH PACKAGE identifier IS NEW name ';'
  | use_clause
    ;

generic_discrim_part_opt :
    %empty
  | discrim_part
  | '(' BOX ')'
    ;

subp_default :
    %empty
  | IS name
  | IS BOX
    ;

generic_type_def :
    '(' BOX ')'
  | RANGE BOX
  | MOD BOX
  | DELTA BOX
  | DELTA BOX DIGITS BOX
  | DIGITS BOX
  | array_type
  | access_type
  | private_type
  | generic_derived_type
    ;

generic_derived_type :
    NEW subtype_ind
  | NEW subtype_ind WITH PRIVATE
    ;

generic_subp_inst :
    subprog_spec IS generic_inst
    ;

generic_pkg_inst :
    PACKAGE identifier IS generic_inst
    ;

generic_inst :
    NEW name
    ;

rep_spec :
    attrib_def
  | record_type_spec
  | address_spec
    ;

attrib_def :
    FOR mark USE expression ';'
    ;

record_type_spec :
    FOR mark USE RECORD align_opt comp_loc_s END RECORD ';'
    ;

align_opt :
    %empty
  | AT MOD expression ';'
    ;

comp_loc_s :
    %empty
  | comp_loc_s mark AT expression RANGE range ';'
    ;

address_spec :
    FOR mark USE AT expression ';'
    ;

code_stmt :
    qualified ';'
    ;

%%

void yyerror(YYLTYPE* yyloc, yyscan_t scanner, ParseContext* parse_ctx, const char* msg)
{
    (void)scanner;
    (void)parse_ctx;
    error_print(*yyloc, msg);
    error_exit();
}
