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

    DEFINE_ARRAY_TYPE(EnumLiteral)
    DEFINE_ARRAY_TYPE(StringToken)
    DEFINE_ARRAY_TYPE(Choice)

    typedef Declaration Decl;
    DEFINE_LINKED_LIST_TYPE(Decl)
    typedef Statement Stmt;
    DEFINE_LINKED_LIST_TYPE(Stmt)
    typedef Alternative Alt;
    DEFINE_LINKED_LIST_TYPE(Alt)

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
        DeclList scope_stack[32];
        void* symbol_table;
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

    #define NAME symbol_map
    #define KEY_TY StringToken
    #define VAL_TY Declaration*
    #define HASH_FN vt_hash_integer
    #define CMPR_FN vt_cmpr_integer
    #include "verstable.h"

    DEFINE_ARRAY_OPS(EnumLiteral)
    DEFINE_ARRAY_OPS(StringToken)
    DEFINE_ARRAY_OPS(Choice)
    DEFINE_LINKED_LIST_OPS(Decl)
    DEFINE_LINKED_LIST_OPS(Stmt)
    DEFINE_LINKED_LIST_OPS(Alt)

    TypeDecl universal_int_type = {
        .base.kind = DECL_TYPE,
        .kind = TYPE_UNIV_INTEGER,
        .name = 0 // Note: this is set the first time the parser is called (see initial-action)
    };

    TypeDecl boolean_type = {
        .base.kind = DECL_TYPE,
        .kind = TYPE_ENUM,
        .name = 0, // Note: this is set the first time the parser is called (see initial-action)
    };

    #define curr_scope(context) ((context)->scope_stack + (context)->curr_scope_idx)

    static
    void begin_scope(ParseContext* context, SourceLocation loc);

    static
    void end_scope(ParseContext* context, SourceLocation loc);

    static
    void push_declaration(ParseContext* context, Declaration* decl);

    static
    void add_decl_to_symbol_table(ParseContext* context, Declaration* decl);

    static
    void remove_decl_from_symbol_table(ParseContext* context, Declaration* decl);

    static
    Declaration* find_decl_in_scope(DeclList* scope, StringToken name);

    static
    TypeDecl* find_type_decl(ParseContext* context, StringToken name);

    static
    PackageSpec* find_package_spec(ParseContext* context, StringToken name);

    static
    UseClause* find_use_clause(ParseContext* context, StringToken package_name);

    static
    LabelDecl* find_label(ParseContext* context, StringToken name);

    #define cnt_of_array(arr) (sizeof(arr) / sizeof(arr[0]))

    #define clr_struct(s) memset(s, 0, sizeof(*(s)))

    static
    void check_for_redefinition(ParseContext* context, StringToken name, SourceLocation loc);

    static
    Expression* create_expr(ExprKind kind, SourceLocation loc);

    static
    Expression* create_binary_expr(Expression* left, BinaryOperator op, Expression* right);

    static
    Expression* create_unary_expr(UnaryOperator op, Expression* right);

    static
    Statement* create_stmt(StmtKind kind, SourceLocation loc);

    static
    TypeDecl* create_type_decl(TypeKind kind);

    static
    ObjectDecl* create_object_decl(StringToken name, SourceLocation loc);

    static
    SubprogramDecl* create_subprogram_decl(StringToken name, SourceLocation loc);

    static
    LabelDecl* create_label(StringToken name, SourceLocation loc);

    static
    CompilationUnit* create_comp_unit(CompilationUnitKind kind);

    static
    int get_base(StringView num_str, SourceLocation loc);

    static
    bool prepare_num_str(StringView num_str, char* buffer, int buffer_sz);

    static
    StringToken get_decl_name(const Declaration* decl);
}

%union {
    UnaryOperator unary_op;
    BinaryOperator binary_op;
    Expression* expr;
    Statement* stmt;
    StmtList stmt_list;
    AltList case_list;
    Choice choice;
    ChoiceArray choice_array;
    Alternative* case_;
    TypeDecl* type_decl;
    SubprogramDecl* subprogram_decl;
    PackageSpec* pkg_spec;
    PackageBody* pkg_body;
    Declaration* decl;
    CompilationUnit* comp_unit;
    bool bool_;
    ParamMode param_mode;
    StringToken str_token;
    char c;
    StringView str; // Note: this StringView owns its allocated data
    EnumLiteral enum_literal;
    EnumLiteralArray enum_literals;
    StringTokenArray str_token_array;
    NameExpr name;
}

/* Terminals */
%type <c> char_lit;
%type <str_token> identifier goto_label identifier_opt
%type <str> char_string numeric_lit
/* Nonterminals */
%type <unary_op> unary adding multiplying membership relational logical short_circuit
%type <expr> used_char literal simple_expression relation primary term factor expression
             parenthesized_primary condition when_opt range range_constraint range_constr_opt
             discrete_range init_opt qualified
%type <stmt> statement simple_stmt null_stmt assign_stmt return_stmt exit_stmt basic_loop loop_content
             loop_stmt goto_stmt unlabeled compound_stmt procedure_call handled_stmt_s
             block_body block cond_clause cond_clause_s else_opt if_stmt case_hdr case_stmt
%type <stmt_list> statement_s
%type <case_> alternative
%type <decl> body decl_item_s decl_part type_decl subtype_decl block_decl decl decl_item_s1
             decl_item_or_body_s1 decl_item object_decl number_decl decl_item_or_body use_clause
             rename_decl
%type <case_list> alternative_s
%type <choice> choice
%type <choice_array> choice_s
%type <type_decl> type_completion type_def enumeration_type integer_type derived_type
%type <subprogram_decl> subprog_decl subprog_spec subprog_spec_is_push subprog_body
%type <pkg_spec> pkg_spec pkg_decl
%type <pkg_body> pkg_body
%type <bool_> reverse_opt object_qualifier_opt
%type <param_mode> mode
%type <str_token> subtype_ind object_subtype_def designator operator_symbol
%type <str_token_array> def_id_s use_name_s
%type <enum_literal> enum_id
%type <enum_literals> enum_id_s
%type <name> name
%type <comp_unit> comp_unit unit

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
    context->symbol_table = calloc(1, sizeof(symbol_map));
    symbol_map_init(context->symbol_table);
    if(!universal_int_type.name) {
        universal_int_type.name = string_pool_c_str_to_token("universal_integer");
    }
    if(!boolean_type.name) {
        boolean_type.name = string_pool_c_str_to_token("Boolean");
        EnumLiteral* literals = calloc(2, sizeof(EnumLiteral));
        literals[false].base.kind = DECL_ENUM_LIT;
        literals[false].name = string_pool_c_str_to_token("False");
        literals[true] = literals[false];
        literals[true].name = string_pool_c_str_to_token("True");
        boolean_type.u.enum_.literals = literals;
        boolean_type.u.enum_.literal_count = 2;
    }
    push_declaration(context, &boolean_type.base);
    add_decl_to_symbol_table(context, &boolean_type.u.enum_.literals[false].base);
    add_decl_to_symbol_table(context, &boolean_type.u.enum_.literals[true].base);
    // Silences annoying compiler warning
    (void)yynerrs;
}

%%

goal_symbol : comp_unit { context->comp_unit = $comp_unit; }
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
    def_id_s ':' object_qualifier_opt object_subtype_def init_opt ';' {
        TypeDecl* type_decl = find_type_decl(context, $object_subtype_def);
        if(!type_decl) {
            error_print(@$, "Unknown type: %s", ST($object_subtype_def));
            error_exit();
        }

        $$ = NULL;
        uint32_t name_count = StringTokenArray_size(&$def_id_s);
        for(uint32_t i = 0; i < name_count; ++i) {
            ObjectDecl* decl = create_object_decl($def_id_s.data[i], @$);
            check_for_redefinition(context, decl->name, @$);
            decl->is_constant = $object_qualifier_opt;
            decl->type = type_decl;
            decl->init_expr = $init_opt;
            // TODO: handle deferred constants, which do not have initial expressions
            if(decl->is_constant && !decl->init_expr) {
                error_print(@$, "Constant declaration '%s' is not initialized", ST(decl->name));
                error_exit();
            }
            push_declaration(context, &decl->base);
            if(!$$) {
                $$ = &decl->base;
            }
        }
    };

number_decl :
    def_id_s ':' CONSTANT IS_ASSIGNED expression ';' {
        $$ = NULL;
        uint32_t name_count = StringTokenArray_size(&$def_id_s);
        for(uint32_t i = 0; i < name_count; ++i) {
            ObjectDecl* decl = create_object_decl($def_id_s.data[i], @$);
            check_for_redefinition(context, decl->name, @$);
            decl->is_constant = true;
            decl->type = &universal_int_type;
            decl->init_expr = $expression;
            push_declaration(context, &decl->base);
            if(!$$) {
                $$ = &decl->base;
            }
        }
    };

def_id_s :
    identifier              {
        StringTokenArray_init(&$$);
        StringTokenArray_append(&$$, $identifier);
    }
  | def_id_s ',' identifier {
        $$ = $1;
        StringTokenArray_append(&$$, $identifier);
    };

// boolean attribute indicates whether object is a constant or not
object_qualifier_opt :
    %empty   { $$ = false; }
  | CONSTANT { $$ = true; }
    ;

object_subtype_def :
    subtype_ind
  | array_type
    ;

init_opt :
    %empty                 { $$ = NULL; }
  | IS_ASSIGNED expression { $$ = $expression; }
    ;

type_decl :
    TYPE identifier discrim_part_opt type_completion ';' {
        // TODO: discriminant
        TypeDecl* decl = $type_completion;
        // Note: decl->base.kind is set by the specific type_completion
        decl->base.loc = @$;
        decl->name = $identifier;
        check_for_redefinition(context, decl->name, @$);
        push_declaration(context, &decl->base);
        $$ = &decl->base;
    };

discrim_part_opt :
    %empty
  | discrim_part
  | '(' BOX ')'
    ;

// TODO: incomplete types (i.e. case 1)
type_completion :
    %empty
  | IS type_def { $$ = $type_def; }
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
    SUBTYPE identifier IS subtype_ind ';' {
        TypeDecl* decl = create_type_decl(TYPE_SUBTYPE);
        decl->base.loc = @$;
        decl->name = $identifier;
        check_for_redefinition(context, decl->name, @$);
        TypeDecl* base_type = find_type_decl(context, $subtype_ind);
        if(!base_type) {
            error_print(@$, "Unknown base type: %s", ST($subtype_ind));
            error_exit();
        }
        decl->u.subtype.base = base_type;
        push_declaration(context, &decl->base);
        $$ = &decl->base;
    };

// TODO: support other name variants (e.g. indexed, compound)
subtype_ind :
    name constraint {
        // TODO: propagate constraint somehow
        $$ = $name.name;
    }
  | name { $$ = $name.name; }
  ;

constraint :
    range_constraint
  | decimal_digits_constraint
    ;

decimal_digits_constraint :
    DIGITS expression range_constr_opt
    ;

derived_type :
    NEW subtype_ind {
        $$ = create_type_decl(TYPE_DERIVED);
        TypeDecl* base_type = find_type_decl(context, $subtype_ind);
        if(!base_type) {
            error_print(@$, "Unknown base type: %s", ST($subtype_ind));
            error_exit();
        }
        $$->u.subtype.base = base_type;
    };

range_constraint :
    RANGE range { $$ = $range; }
    ;

range_constr_opt :
    %empty { $$ = NULL; }
  | range_constraint
    ;

range :
    simple_expression[left] DOT_DOT simple_expression[right] { $$ = create_binary_expr($left, OP_RANGE, $right); }
  | name '\'' RANGE
  | name '\'' RANGE '(' expression ')'
    ;

enumeration_type :
    '(' enum_id_s ')' {
        $$ = create_type_decl(TYPE_ENUM);
        $$->u.enum_.literals = $enum_id_s.data;
        $$->u.enum_.literal_count = EnumLiteralArray_size(&$enum_id_s);
    };

enum_id_s :
    enum_id {
        EnumLiteralArray_init(&$$);
        EnumLiteralArray_append(&$$, $enum_id);
    }
  | enum_id_s[left] ',' enum_id {
        $$ = $left;
        EnumLiteralArray_append(&$$, $enum_id);
    };

enum_id :
    identifier {
        clr_struct(&$$);
        $$.base.kind = DECL_ENUM_LIT;
        $$.base.loc = @$;
        $$.name = $identifier;
        $$.is_char_lit = false;
    }
  | char_lit {
        clr_struct(&$$);
        $$.base.kind = DECL_ENUM_LIT;
        $$.base.loc = @$;
        char buffer[3] = {0};
        buffer[0] = '\'';
        buffer[1] = $char_lit;
        buffer[2] = '\'';
        StringView literal_text = { .value = buffer, .len = sizeof(buffer) };
        $$.name = string_pool_to_token(literal_text);
        $$.is_char_lit = true;
    };

integer_type :
    range_constraint {
        $$ = create_type_decl(TYPE_INTEGER);
        $$->u.int_.range = $range_constraint;
    }
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
    choice              {
        ChoiceArray_init(&$$);
        ChoiceArray_append(&$$, $choice);
    }
  | choice_s[left] '|' choice {
        $$ = $left;
        ChoiceArray_append(&$$, $choice);
    };

choice :
    expression           {
        $$.kind = CHOICE_EXPR;
        $$.u.expr = $expression;
    }
  | discrete_with_range
  | OTHERS               { $$.kind = CHOICE_OTHERS; }
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
    %empty               { $$ = NULL; }
  | decl_item_or_body_s1
    ;

decl_item_s :
    %empty       { $$ = NULL; }
  | decl_item_s1
    ;

decl_item_s1 :
    decl_item
  | decl_item_s1 decl_item { $$ = $1; };

decl_item :
    decl
  | use_clause
  | rep_spec
  | pragma
    ;

decl_item_or_body_s1 :
    decl_item_or_body
  | decl_item_or_body_s1 decl_item_or_body { $$ = $1; };

decl_item_or_body :
    body
  | decl_item
    ;

body :
    subprog_body { $$ = &$subprog_body->base; }
  | pkg_body     { $$ = &$pkg_body->base; }
    ;

// TODO: replace most usages of NameExpr with a new Name type that is not an expression
//   Names are used in a lot of places where expressions are not needed and just make things
//   more confusing/bloated
name :
    identifier {
        clr_struct(&$$);
        $$.name = $identifier;
    }
  | indexed_comp
  | selected_comp
  | attribute
  | operator_symbol {
        clr_struct(&$$);
        $$.name = $operator_symbol;
        //TODO: lookup operator, determine its arity, and allocate args array
    };

mark :
    identifier
  | mark '\'' attribute_id
  | mark '.' identifier
    ;

used_char :
    char_lit {
        $$ = create_expr(EXPR_CHAR_LIT, @$);
        $$->u.char_lit = $char_lit;
    };

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
    numeric_lit {
        // TODO: support non-integer numeric literals
        int base = get_base($numeric_lit, @$);

        char num_buffer[128];
        num_buffer[0] = '\0';
        if(!prepare_num_str($numeric_lit, num_buffer, sizeof(num_buffer))) {
            error_print(@$, "Numeric literal is too long to be processed (max supported is 127 characters)");
            error_exit();
        }

        // Note: don't overwrite $$ here since we are still using its value
        Expression* expr = create_expr(EXPR_INT_LIT, @$);
        if(mpz_init_set_str(expr->u.int_lit.value, num_buffer, base) < 0) {
            error_print(@$, "Invalid numeric literal: '%.*s' for base %u", SV($numeric_lit), base);
            error_exit();
        }
        $$ = expr;
    }
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
  | expression[left] logical[op] relation[right]       { $$ = create_binary_expr($left, $op, $right); }
  | expression[left] short_circuit[op] relation[right] { $$ = create_binary_expr($left, $op, $right); }
    ;

logical :
    AND { $$ = OP_AND; }
  | OR  { $$ = OP_OR; }
  | XOR { $$ = OP_XOR; }
    ;

short_circuit :
      AND THEN { $$ = OP_AND_THEN; }
    | OR ELSE  { $$ = OP_OR_ELSE; }
    ;

// TODO: constant folding of literals
relation :
    simple_expression
  | simple_expression[left] relational[op] simple_expression[right] { $$ = create_binary_expr($left, $op, $right); }
  | simple_expression[left] membership[op] range[right]             { $$ = create_binary_expr($left, $op, $right); }
  | simple_expression[left] membership[op] name                     {
        Expression* right = create_expr(EXPR_NAME, @3);
        right->u.name = $name;
        $$ = create_binary_expr($left, $op, right);
    };

relational :
    '='   { $$ = OP_EQ; }
  | NE    { $$ = OP_NEQ; }
  | '<'   { $$ = OP_LT; }
  | LT_EQ { $$ = OP_LTE; }
  | '>'   { $$ = OP_GT; }
  | GE    { $$ = OP_GTE; }
    ;

membership :
    IN     { $$ = OP_IN; }
  | NOT IN { $$ = OP_NOT_IN; }
    ;

simple_expression :
    term
  | unary[op] term                                 { $$ = create_unary_expr($op, $term); }
  | simple_expression[left] adding[op] term[right] { $$ = create_binary_expr($left, $op, $right); }
    ;

unary :
    '+' { $$ = OP_UNARY_PLUS; }
  | '-' { $$ = OP_UNARY_MINUS; }
    ;

adding :
    '+' { $$ = OP_PLUS; }
  | '-' { $$ = OP_MINUS; }
  | '&' { $$ = OP_AMP; }
    ;

term :
    factor
  | term[left] multiplying[op] factor[right] { $$ = create_binary_expr($left, $op, $right); }
    ;

multiplying :
    '*' { $$ = OP_MULT; }
  | '/' { $$ = OP_DIVIDE; }
  | MOD { $$ = OP_MOD; }
  | REM { $$ = OP_REM; }
    ;

factor :
    primary
  | NOT primary                        { $$ = create_unary_expr(OP_NOT, $primary); }
  | ABS primary                        { $$ = create_unary_expr(OP_ABS, $primary); }
  | primary[left] EXPON primary[right] { $$ = create_binary_expr($left, OP_EXP, $right); }
    ;

primary :
    literal
  | name {
        $$ = create_expr(EXPR_NAME, @$);
        $$->u.name = $name;
    }
  | allocator
  | qualified
  | parenthesized_primary
    ;

parenthesized_primary :
    aggregate
  | '(' expression ')' { $$ = $expression; }
    ;

qualified :
    name '\'' parenthesized_primary[expr] {
        // TODO: support other kinds of names
        assert($name.arg_count == 0);
        TypeDecl* type_decl = find_type_decl(context, $name.name);
        if(!type_decl) {
            error_print(@$, "Unknown type: %s", ST($name.name));
            error_exit();
        }
        $$ = create_expr(EXPR_QUALIFIED, @$);
        $$->u.qualified.type = type_decl;
        $$->u.qualified.expr = $expr;
    };

allocator :
    NEW name
  | NEW qualified
    ;

statement_s :
    statement             {
        clr_struct(&$$);
        StmtList_append(&$$, $statement);
    }
  | statement_s[left] statement {
        StmtList_append(&$left, $statement);
        $$ = $left;
    };

statement :
    unlabeled
  | goto_label statement {
        LabelDecl* label = find_label(context, $goto_label);
        if(label) {
            if(label->is_placeholder) {
                // Fill in the placeholder
                label->is_placeholder = false;
                label->base.loc = @goto_label;
            } else {
                error_print(@goto_label, "Redefinition of label '%s'", ST($goto_label));
                error_print(label->base.loc, "Previous definition here");
                error_exit();
            }
        } else {
            check_for_redefinition(context, $goto_label, @goto_label);
            label = create_label($goto_label, @goto_label);
            push_declaration(context, (Declaration*)label);
        }
        $$ = $2;
    };

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
    NuLL ';' { $$ = create_stmt(STMT_NULL, @$); }
    ;

assign_stmt :
    name IS_ASSIGNED expression ';' {
        $$ = create_stmt(STMT_ASSIGN, @$);
        $$->u.assign.dest.kind = EXPR_NAME;
        $$->u.assign.dest.loc = @$;
        $$->u.assign.dest.u.name = $name;
        $$->u.assign.expr = $expression;
    };

if_stmt :
    IF cond_clause_s else_opt END IF ';' {
        $$ = $cond_clause_s;
        Statement* branch = $cond_clause_s;
        while(branch->u.if_.else_) {
            branch = branch->u.if_.else_;
            assert(branch->kind == STMT_IF);
        }
        branch->u.if_.else_ = $else_opt;
    };

cond_clause_s :
    cond_clause
  | cond_clause_s[if] ELSIF cond_clause[elsif] {
        $$ = $if;
        $$->u.if_.else_ = $elsif;
    };

cond_clause :
    condition THEN statement_s {
        $$ = create_stmt(STMT_IF, @$);
        $$->u.if_.condition = $condition;
        $$->u.if_.stmts = $statement_s.first;
    };

condition :
    expression
    ;

else_opt :
    %empty           { $$ = NULL; }
  | ELSE statement_s { $$ = $statement_s.first; }
    ;

case_stmt :
    case_hdr pragma_s alternative_s END CASE ';' {
        $$ = $case_hdr;
        // TODO: pragmas
        $$->u.case_.cases = $alternative_s.first;
    };

case_hdr :
    CASE expression IS {
        $$ = create_stmt(STMT_CASE, @$);
        $$->u.case_.expr = $expression;
    };

alternative_s :
    %empty                    { clr_struct(&$$); }
  | alternative_s[left] alternative {
        $$ = $left;
        AltList_append(&$$, $alternative);
    };

alternative :
    WHEN choice_s RIGHT_SHAFT statement_s {
        $$ = calloc(1, sizeof(Alternative));
        $$->choices.choices = $choice_s.data;
        $$->choices.count = ChoiceArray_size(&$choice_s);
        $$->stmts = $statement_s.first;
    };

// TODO: label_opt and id_opt
loop_stmt :
    label_opt loop_content id_opt ';' { $$ = $loop_content; }
    ;

label_opt :
    %empty
  | identifier ':'
    ;

loop_content :
    basic_loop {
        $$ = create_stmt(STMT_WHILE, @$);
        // Create condition so this becomes a 'while True' loop
        Expression* condition = create_expr(EXPR_ENUM_LIT, @$);
        condition->u.enum_lit = &boolean_type.u.enum_.literals[true];
        $$->u.while_.condition = condition;
        $$->u.while_.stmts = $basic_loop;
    }
  | WHILE condition basic_loop {
        $$ = create_stmt(STMT_WHILE, @$);
        $$->u.while_.condition = $condition;
        $$->u.while_.stmts = $basic_loop;
    }
  | FOR identifier IN reverse_opt discrete_range basic_loop {
        $$ = create_stmt(STMT_FOR, @$);
        $$->u.for_.var = create_object_decl($identifier, @identifier);
        $$->u.for_.reverse = $reverse_opt;
        $$->u.for_.range = $discrete_range;
        $$->u.for_.stmts = $basic_loop;
    };

reverse_opt :
    %empty  { $$ = false; }
  | REVERSE { $$ = true; }
    ;

basic_loop :
    LOOP statement_s END LOOP { $$ = $statement_s.first; }
    ;

id_opt :
    %empty
  | designator
    ;

// TODO: label
block :
    label_opt block_decl block_body END id_opt ';' {
        $$ = create_stmt(STMT_BLOCK, @$);
        $$->u.block.decls = $block_decl;
        $$->u.block.stmts = $block_body;
        // Close scope if there was a declaration section
        if($2) {
            end_scope(context, @END);
        }
    };

block_decl :
    %empty                                                { $$ = NULL; }
  | DECLARE { begin_scope(context, @DECLARE); } decl_part {
        $$ = $decl_part;
        // Close scope if no declaration section
        if(!$$) {
            end_scope(context, @DECLARE);
        }
    };

block_body :
    BEGiN handled_stmt_s { $$ = $handled_stmt_s; }
    ;

// TODO: exception handler
handled_stmt_s :
    statement_s except_handler_part_opt { $$ = $statement_s.first; }
    ;

except_handler_part_opt :
    %empty
  | except_handler_part
    ;

exit_stmt :
    EXIT name_opt when_opt ';' {
        $$ = create_stmt(STMT_EXIT, @$);
        // TODO: name_opt
        $$->u.exit.condition = $when_opt;
    };

name_opt :
    %empty
  | name
    ;

when_opt :
    %empty         { $$ = NULL; }
  | WHEN condition { $$ = $condition; }
    ;

return_stmt :
    RETURN ';'            { $$ = create_stmt(STMT_RETURN, @$); }
  | RETURN expression ';' {
        $$ = create_stmt(STMT_RETURN, @$);
        $$->u.return_.expr = $expression;
    };

goto_stmt :
    GOTO identifier ';' {
        StringToken label_name = $identifier;

        $$ = create_stmt(STMT_GOTO, @$);
        LabelDecl* label = find_label(context, label_name);
        if(label) {
            // Label is defined prior to the goto statement
            $$->u.goto_.label = label;
        } else {
            // Label is not defined yet
            check_for_redefinition(context, label_name, @identifier);
            // Define a placeholder label
            // TODO: in semantic analysis, verify that all placeholder labels are filled in
            LabelDecl* label = create_label(label_name, @identifier);
            label->is_placeholder = true;
            $$->u.goto_.label = label;
            push_declaration(context, (Declaration*)label);
        }
    };

subprog_decl :
    subprog_spec ';'      {
        $$ = $subprog_spec;
        end_scope(context, @2);
    }
  | generic_subp_inst ';'
    ;

// TODO: process formal_part_opt
subprog_spec :
    PROCEDURE identifier <subprogram_decl>{
        // TODO: check for name conflict
        $<subprogram_decl>$ = create_subprogram_decl($identifier, @identifier);
        push_declaration(context, &$<subprogram_decl>$->base);
        begin_scope(context, @identifier);
    }
    formal_part_opt             { $$ = $3; }
  | FUNCTION designator <subprogram_decl>{
        // TODO: check for name conflict
        $<subprogram_decl>$ = create_subprogram_decl($designator, @designator);
        push_declaration(context, &$<subprogram_decl>$->base);
        begin_scope(context, @designator);
    }
    formal_part_opt RETURN name { $$ = $3; }
  | FUNCTION designator  /* for generic inst and generic rename */
    ;

designator :
    identifier
  | char_string { $$ = string_pool_to_token($char_string); }
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
    %empty { $$ = PARAM_MODE_IN; }
  | IN     { $$ = PARAM_MODE_IN; }
  | OUT    { $$ = PARAM_MODE_OUT; }
  | IN OUT { $$ = PARAM_MODE_IN_OUT; }
    ;

subprog_spec_is_push :
    subprog_spec IS { $$ = $subprog_spec; }
    ;

// TODO: params will be pushed twice (one in forward decl, if any, and again in subprog_body)
//  Need to somehow check if a forward decl was already made; if so, don't push params again
subprog_body :
    subprog_spec_is_push decl_part block_body END id_opt ';' {
        $$ = $subprog_spec_is_push;
        $$->decls = $decl_part;
        $$->stmts = $block_body;
        // Close scope opened in subprog_spec
        end_scope(context, @END);
    };

procedure_call :
    name ';' {
        $$ = create_stmt(STMT_EXPR, @$);
        $$->u.expr.kind = EXPR_NAME;
        $$->u.expr.loc = @$;
        $$->u.expr.u.name = $name;
    };

pkg_decl :
    pkg_spec ';'         { $$ = $pkg_spec; }
  | generic_pkg_inst ';'
    ;

pkg_spec :
    PACKAGE identifier IS <pkg_spec>{
        begin_scope(context, @IS);
        $<pkg_spec>$ = calloc(1, sizeof(PackageSpec));
        $<pkg_spec>$->base.kind = DECL_PKG_SPEC;
        $<pkg_spec>$->base.loc = @$;
        $<pkg_spec>$->name = $identifier;
    }
    decl_item_s private_part END identifier_opt {
        $$ = $4;
        $$->decls = $decl_item_s;
        // TODO: private part
        end_scope(context, @END);
        if($identifier_opt && $$->name != $identifier_opt) {
            error_print(@identifier_opt,
                "End label '%s' does not match package name ('%s')", ST($identifier_opt), ST($$->name));
            error_exit();
        }
        push_declaration(context, &$$->base);
    };

private_part :
    %empty
  | PRIVATE decl_item_s
    ;

identifier_opt :
    %empty     { $$ = 0; }
  | identifier
    ;

pkg_body :
    PACKAGE BODY identifier IS <pkg_body>{
        begin_scope(context, @IS);
        $<pkg_body>$ = calloc(1, sizeof(PackageBody));
        $<pkg_body>$->base.kind = DECL_PKG_BODY;
        $<pkg_body>$->base.loc = @$;
        $<pkg_body>$->name = $identifier;
    }
    decl_part body_opt END identifier_opt ';' {
        $$ = $5;
        $$->decls = $decl_part;
        // TODO: body_opt
        end_scope(context, @END);
        if($identifier_opt && $$->name != $identifier_opt) {
            error_print(@identifier_opt,
                "End label '%s' does not match package name ('%s')", ST($identifier_opt), ST($$->name));
            error_exit();
        }
        push_declaration(context, &$$->base);
    };

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

// TODO: support complex names (e.g. nested packages)
use_name_s :
    identifier                   {
        StringTokenArray_init(&$$);
        StringTokenArray_append(&$$, $identifier);
    }
  | selected_comp
  | use_name_s ',' identifier    {
        $$ = $1;
        StringTokenArray_append(&$$, $identifier);
    }
  | use_name_s ',' selected_comp
  ;

use_clause :
    USE use_name_s ';' {
        $$ = NULL;
        uint32_t package_count = StringTokenArray_size(&$use_name_s);
        for(uint32_t i = 0; i < package_count; ++i) {
            StringToken package_name = $use_name_s.data[i];
            PackageSpec* package_spec = find_package_spec(context, package_name);
            if(!package_spec) {
                error_print(@use_name_s, "Unknown package name '%s'", ST(package_name));
                error_exit();
            }
            UseClause* use_clause = find_use_clause(context, package_name);
            if(use_clause) {
                // Duplicate use clause - ignore
                // TODO: warning?
                continue;
            }
            // TODO: mark potential ambiguities that require name qualifications
            //  somehow
            use_clause = calloc(1, sizeof(UseClause));
            use_clause->base.kind = DECL_USE;
            use_clause->base.loc = @$;
            use_clause->package_spec = package_spec;
            push_declaration(context, &use_clause->base);
            if(!$$) {
                $$ = &use_clause->base;
            }
        }
    };

// Note: def_id_s is used instead of identifier to avoid shift/reduce conflict
rename_decl :
    def_id_s ':' object_qualifier_opt subtype_ind RENAMES name ';' {
        uint32_t ident_count = StringTokenArray_size(&$def_id_s);
        if(ident_count != 1) {
            error_print(@def_id_s,
                "Renames declarations must have exactly one identifier on the left-hand side of the 'renames' keyword");
            error_exit();
        }
        RenameDecl* rename_decl = calloc(1, sizeof(RenameDecl));
        rename_decl->base.kind = DECL_RENAME;
        rename_decl->base.loc = @$;
        rename_decl->name = $def_id_s.data[0];
        rename_decl->target.kind = EXPR_NAME;
        rename_decl->target.loc = @$;
        rename_decl->target.u.name = $name;
        // TODO: handle object_qualifier_opt
        // TODO: handle subtype_ind
        // TODO: check that the target is an object (or some kind of slice/expression that yields an object)
        push_declaration(context, &rename_decl->base);
    }
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
    context_spec unit pragma_s { $$ = $unit; }
  | unit pragma_s              { $$ = $unit; }
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
            CompilationUnit* unit = comp_manager_parse_spec(context->comp_manager, package_name, &@$);
            assert(unit->kind == COMP_UNIT_PACKAGE_SPEC);
            push_declaration(context, &unit->u.package_spec->base);
        }
    };

use_clause_opt :
    %empty
  | use_clause_opt use_clause
    ;

unit :
    pkg_decl     {
        $$ = create_comp_unit(COMP_UNIT_PACKAGE_SPEC);
        $$->u.package_spec = $pkg_decl;
    }
  | pkg_body     {
        $$ = create_comp_unit(COMP_UNIT_PACKAGE_BODY);
        $$->u.package_body = $pkg_body;
    }
  | subprog_decl {
        $$ = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        $$->u.subprogram_decl = $subprog_decl;
    }
  | subprog_body {
        $$ = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        $$->u.subprogram_decl = $subprog_body;
    }
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

static
void begin_scope(ParseContext* context, SourceLocation loc)
{
    if(context->curr_scope_idx + 1u >= cnt_of_array(context->scope_stack)) {
        error_print(loc, "Too many nested scopes (maximum is %u nested scopes)", cnt_of_array(context->scope_stack));
        error_exit();
    }
    ++context->curr_scope_idx;
}

static
void end_scope(ParseContext* context, SourceLocation loc)
{
    if(context->curr_scope_idx == 0) {
        error_print(loc, "Attempted to exit top-level region");
        error_exit();
    }
    // Remove all named declarations from the symbol table
    for(Declaration* decl = curr_scope(context)->first; decl; decl = decl->next) {
        remove_decl_from_symbol_table(context, decl);
    }
    clr_struct(curr_scope(context));
    --context->curr_scope_idx;
}

static
void push_declaration(ParseContext* context, Declaration* decl)
{
    DeclList_append(curr_scope(context), decl);
    add_decl_to_symbol_table(context, decl);
}

static
void add_decl_to_symbol_table(ParseContext* context, Declaration* decl)
{
    StringToken name = get_decl_name(decl);
    if(!name) {
        return;
    }

    if(decl->kind == DECL_USE) {
        UseClause* use_clause = (UseClause*)decl;
        // Add the declarations pulled in by the use clause
        for(Declaration* inner_decl = use_clause->package_spec->decls; inner_decl; inner_decl = inner_decl->next) {
            add_decl_to_symbol_table(context, inner_decl);
        }
        return;
    }

    // TODO: also handle types derived from enum types
    if(decl->kind == DECL_TYPE && ((TypeDecl*)decl)->kind == TYPE_ENUM) {
        // Add the enum literals to the symbol table
        EnumType* enum_type = &((TypeDecl*)decl)->u.enum_;
        uint32_t literal_count = enum_type->literal_count;
        for(uint32_t i = 0; i < literal_count; ++i) {
            add_decl_to_symbol_table(context, &enum_type->literals[i].base);
        }
    }

    // Add named declarations to the symbol table
    symbol_map_itr it = symbol_map_get(context->symbol_table, name);
    if(!symbol_map_is_end(it)) {
        // Prepend new declaration to the bucket
        decl->next_overload = it.data->val;
    }
    symbol_map_insert(context->symbol_table, name, decl);
}

static
void remove_decl_from_symbol_table(ParseContext* context, Declaration* decl)
{
    StringToken name = get_decl_name(decl);
    if(!name) {
        return;
    }

    if(decl->kind == DECL_USE) {
        UseClause* use_clause = (UseClause*)decl;
        // Remove the declarations pulled in by the use clause
        for(Declaration* inner_decl = use_clause->package_spec->decls; inner_decl; inner_decl = inner_decl->next) {
            remove_decl_from_symbol_table(context, inner_decl);
        }
        return;
    }

    // TODO: also handle types derived from enum types
    if(decl->kind == DECL_TYPE && ((TypeDecl*)decl)->kind == TYPE_ENUM) {
        // Remove the enum literals from the symbol table
        EnumType* enum_type = &((TypeDecl*)decl)->u.enum_;
        uint32_t literal_count = enum_type->literal_count;
        for(uint32_t i = 0; i < literal_count; ++i) {
            remove_decl_from_symbol_table(context, &enum_type->literals[i].base);
        }
    }

    symbol_map_itr it = symbol_map_get(context->symbol_table, name);
    assert(!symbol_map_is_end(it));
    Declaration* next_overload = it.data->val->next_overload;
    if(next_overload) {
        symbol_map_insert(context->symbol_table, name, next_overload);
    } else {
        symbol_map_erase_itr(context->symbol_table, it);
    }
}

static
Declaration* find_decl_in_scope(DeclList* scope, StringToken name)
{
    for(Declaration* decl = scope->first; decl; decl = decl->next) {
        if(get_decl_name(decl) == name) {
            return decl;
        }
    }
    return NULL;
}

static
TypeDecl* find_type_decl(ParseContext* context, StringToken name)
{
    Declaration* bucket = find_bucket(context, name);
    if(bucket) {
        for(Declaration* decl = bucket; decl; decl = decl->next_overload) {
            if(decl->kind == DECL_TYPE) {
                return (TypeDecl*)decl;
            }
        }
    }
    return NULL;
}

ObjectDecl* find_object_decl(ParseContext* context, StringToken name)
{
    Declaration* bucket = find_bucket(context, name);
    if(bucket) {
        for(Declaration* decl = bucket; decl; decl = decl->next_overload) {
            if(decl->kind == DECL_OBJECT) {
                return (ObjectDecl*)decl;
            }
        }
    }
    return NULL;
}

static
PackageSpec* find_package_spec(ParseContext* context, StringToken name)
{
    Declaration* bucket = find_bucket(context, name);
    if(bucket) {
        for(Declaration* decl = bucket; decl; decl = decl->next_overload) {
            if(decl->kind == DECL_PKG_SPEC) {
                return (PackageSpec*)decl;
            }
        }
    }
    return NULL;
}

static
UseClause* find_use_clause(ParseContext* context, StringToken package_name)
{
    Declaration* bucket = find_bucket(context, package_name);
    if(bucket) {
        for(Declaration* decl = bucket; decl; decl = decl->next_overload) {
            if(decl->kind == DECL_USE) {
                return (UseClause*)decl;
            }
        }
    }
    return NULL;
}

static
LabelDecl* find_label(ParseContext* context, StringToken name)
{
    Declaration* bucket = find_bucket(context, name);
    if(bucket) {
        for(Declaration* decl = bucket; decl; decl = decl->next_overload) {
            if(decl->kind == DECL_LABEL) {
                return (LabelDecl*)decl;
            }
        }
    }
    return NULL;
}

Declaration* find_bucket(ParseContext* context, StringToken name)
{
    symbol_map_itr it = symbol_map_get(context->symbol_table, name);
    if(symbol_map_is_end(it)) {
        return NULL;
    }
    return it.data->val;
}

static
void check_for_redefinition(ParseContext* context, StringToken name, SourceLocation loc)
{
    Declaration* existing_decl = find_decl_in_scope(curr_scope(context), name);
    if(existing_decl) {
        error_print(loc, "Redefinition of '%s' within same declarative region", ST(name));
        error_print(existing_decl->loc, "Previous definition here");
        error_exit();
    }
}

static
Expression* create_expr(ExprKind kind, SourceLocation loc)
{
    Expression* expr = calloc(1, sizeof(Expression));
    expr->kind = kind;
    expr->loc = loc;
    return expr;
}

static
Expression* create_binary_expr(Expression* left, BinaryOperator op, Expression* right)
{
    Expression* expr = create_expr(EXPR_BINARY, left->loc);
    expr->u.binary.left = left;
    expr->u.binary.op = op;
    expr->u.binary.right = right;
    return expr;
}

static
Expression* create_unary_expr(UnaryOperator op, Expression* right)
{
    Expression* expr = create_expr(EXPR_UNARY, right->loc);
    expr->u.unary.op = op;
    expr->u.unary.right = right;
    return expr;
}

static
Statement* create_stmt(StmtKind kind, SourceLocation loc)
{
    Statement* stmt = calloc(1, sizeof(Statement));
    stmt->kind = kind;
    stmt->loc = loc;
    return stmt;
}

static
TypeDecl* create_type_decl(TypeKind kind)
{
    TypeDecl* decl = calloc(1, sizeof(TypeDecl));
    decl->base.kind = DECL_TYPE;
    decl->kind = kind;
    return decl;
}

static
ObjectDecl* create_object_decl(StringToken name, SourceLocation loc)
{
    ObjectDecl* decl = calloc(1, sizeof(ObjectDecl));
    decl->base.kind = DECL_OBJECT;
    decl->base.loc = loc;
    decl->name = name;
    return decl;
}

static
SubprogramDecl* create_subprogram_decl(StringToken name, SourceLocation loc)
{
    SubprogramDecl* decl = calloc(1, sizeof(SubprogramDecl));
    decl->base.kind = DECL_SUBPROGRAM;
    decl->base.loc = loc;
    decl->name = name;
    return decl;
}

static
LabelDecl* create_label(StringToken name, SourceLocation loc)
{
    LabelDecl* label = calloc(1, sizeof(LabelDecl));
    label->base.kind = DECL_LABEL;
    label->base.loc = loc;
    label->name = name;
    return label;
}

static
CompilationUnit* create_comp_unit(CompilationUnitKind kind)
{
    CompilationUnit* comp_unit = calloc(1, sizeof(CompilationUnit));
    comp_unit->kind = kind;
    return comp_unit;
}

static
int get_base(StringView num_str, SourceLocation loc)
{
    int base = 10;
    const char* hash_mark = memchr(num_str.value, '#', num_str.len);
    if(hash_mark) {
        base = 0;
        for(const char* c = num_str.value; c != hash_mark; ++c) {
            if(*c != '_') {
                base = base * 10 + (*c - '0');
            }
        }
        if(base < 1 || base > 16) {
            error_print(loc, "Numeric literal has invalid base (%d). Bases must be in range [1, 16]", base);
            error_exit();
        }
    }
    return base;
}

static
bool prepare_num_str(StringView num_str, char* buffer, int buffer_sz)
{
    const char* num_str_end = num_str.value + num_str.len;
    const char* buffer_end = buffer + buffer_sz - 1; // Leave space for null terminator
    const char* c = num_str.value;
    char* b = buffer;
    while(c < num_str_end) {
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
        case DECL_ENUM_LIT:
            name = ((EnumLiteral*)decl)->name;
            break;
        case DECL_SUBPROGRAM:
            name = ((SubprogramDecl*)decl)->name;
            break;
        case DECL_LABEL:
            name = ((LabelDecl*)decl)->name;
            break;
        case DECL_PKG_SPEC:
            name = ((PackageSpec*)decl)->name;
            break;
        case DECL_USE:
            name = ((UseClause*)decl)->package_spec->name;
            break;
        case DECL_RENAME:
            name = ((RenameDecl*)decl)->name;
            break;
        default:
            // This kind of declaration has no associated name
            break;
    }
    return name;
}

void yyerror(YYLTYPE* yyloc, yyscan_t scanner, ParseContext* parse_ctx, const char* msg)
{
    (void)scanner;
    (void)parse_ctx;
    error_print(*yyloc, msg);
    error_exit();
}
