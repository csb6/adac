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
    #include "ast.h"

    typedef uint32_t SourceLocation;

    typedef Expression* ExprPtr;
    DEFINE_ARRAY_TYPE(ExprPtr)

    DEFINE_ARRAY_TYPE(StringToken)

    #define YYLLOC_DEFAULT(Cur, Rhs, N) \
        do { \
            if(N > 0) { \
                (Cur) = YYRHSLOC(Rhs, 1); \
            } else { \
                (Cur) = YYRHSLOC(Rhs, 0); \
            } \
        } while (0);

    typedef struct {
        Declaration* first;
        Declaration* last;
    } DeclList;

    typedef struct {
        DeclList scope_stack[32];
        Declaration** symbol_table; // array of Declaration*
        uint32_t symbol_table_capacity;
        uint32_t symbol_table_size;
        uint8_t curr_scope_idx;
    } ParseContext;
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

    #define TABLE_GROWTH_FACTOR 2

    DEFINE_ARRAY_OPS(ExprPtr)
    DEFINE_ARRAY_OPS(StringToken)

    static const char universal_integer_str[] = "universal_integer";
    TypeDecl universal_int_type = {
        .kind = TYPE_UNIV_INTEGER,
        .name = 0 // Note: this is set the first time the parser is called (see initial-action)
    };

    static
    Expression* make_binary_expr(Expression* left, BinaryOperator op, Expression* right);

    static
    Expression* make_unary_expr(UnaryOperator op, Expression* right);

    #define curr_scope (context->scope_stack + context->curr_scope_idx)

    static
    void begin_scope(ParseContext* context, uint32_t line_num);

    static
    void end_scope(ParseContext* context, uint32_t line_num);

    static
    void push_declaration(ParseContext* context, Declaration* decl);

    static
    Declaration* find_decl_in_scope(DeclList* scope, StringToken name);

    static
    TypeDecl* find_type_decl(ParseContext* context, StringToken name);

    static
    LabelDecl* find_label(ParseContext* context, StringToken name);

    static
    void append_decl(DeclList* decl_list, Declaration* decl);

    static
    Declaration** find_bucket(ParseContext* context, StringToken name);

    #define cnt_of_array(arr) (sizeof(arr) / sizeof(arr[0]))

    static
    void check_for_redefinition(ParseContext* context, StringToken name, uint32_t line_num);

    static
    Expression* create_expr(ExprKind kind, uint32_t line_num);

    static
    Statement* create_stmt(StmtKind kind, uint32_t line_num);

    static
    TypeDecl* create_type_decl(TypeKind kind);

    static
    ObjectDecl* create_object_decl(StringToken name, uint32_t line_num);

    static
    SubprogramDecl* create_subprogram_decl(StringToken name, uint32_t line_num);

    static
    LabelDecl* create_label(StringToken name, uint32_t line_num);

    static
    int get_base(StringView num_str, uint32_t line_num);

    static
    bool prepare_num_str(StringView num_str, char* buffer, int buffer_sz);

    static
    uint32_t hash_fnv(StringToken token);

    static
    void grow_table(ParseContext* context);

    static
    StringToken get_decl_name(const Declaration* decl);
}

%union {
    UnaryOperator unary_op;
    BinaryOperator binary_op;
    Expression* expr;
    Statement* stmt;
    TypeDecl* type_decl;
    SubprogramDecl* subprogram_decl;
    bool bool_;
    ParamMode param_mode;
    StringToken str_token;
    char c;
    StringView str; // Note: this StringView owns its allocated data
    Array_ExprPtr expr_array;
    Array_StringToken str_token_array;
    NameExpr name;
}

/* Terminals */
%type <c> char_lit;
%type <str_token> identifier goto_label
%type <str> char_string numeric_lit
/* Nonterminals */
%type <unary_op> unary adding multiplying membership relational logical short_circuit
%type <expr> used_char literal simple_expression relation primary term factor expression
             parenthesized_primary condition cond_part when_opt range range_constraint range_constr_opt
             init_opt enum_id
%type <stmt> statement simple_stmt null_stmt assign_stmt return_stmt exit_stmt basic_loop loop_content
             loop_stmt goto_stmt statement_s unlabeled compound_stmt procedure_call handled_stmt_s
             block_body block
%type <type_decl> type_completion type_def enumeration_type integer_type derived_type
%type <bool_> reverse_opt object_qualifier_opt block_decl
%type <param_mode> mode
%type <str_token> subtype_ind simple_name object_subtype_def designator operator_symbol
%type <str_token_array> def_id_s
%type <expr_array> enum_id_s
%type <name> name

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
    @$ = 1;
    memset(context, 0, sizeof(*context));
    context->symbol_table = calloc(64, sizeof(Declaration*));
    context->symbol_table_capacity = 64;
    context->symbol_table_size = 0;
    if(!universal_int_type.name) {
        StringView universal_int_str_view = { .value = universal_integer_str, .len = sizeof(universal_integer_str) };
        universal_int_type.name = string_pool_to_token(universal_int_str_view);
    }
}

%%

goal_symbol : compilation
    ;

pragma :
    PRAGMA identifier ';'
  | PRAGMA simple_name '(' pragma_arg_s ')' ';'
    ;

pragma_arg_s :
    pragma_arg
  | pragma_arg_s ',' pragma_arg
    ;

pragma_arg :
    expression
  | simple_name RIGHT_SHAFT expression
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
        TypeDecl* type_decl = find_type_decl(context, $4);
        if(!type_decl) {
            error_print(@$, "Unknown type: %s", ST($4));
            error_exit();
        }

        uint32_t name_count = array_StringToken_size(&$1);
        for(uint32_t i = 0; i < name_count; ++i) {
            ObjectDecl* decl = create_object_decl($1.data[i], @$);
            check_for_redefinition(context, decl->name, @$);
            decl->is_constant = $3;
            decl->type = type_decl;
            decl->init_expr = $5;
            // TODO: handle deferred constants, which do not have initial expressions
            if(decl->is_constant && !decl->init_expr) {
                error_print(@$, "Constant declaration '%s' is not initialized", ST(decl->name));
                error_exit();
            }
            push_declaration(context, &decl->base);
        }
    };

number_decl :
    def_id_s ':' CONSTANT IS_ASSIGNED expression ';' {
        uint32_t name_count = array_StringToken_size(&$1);
        for(uint32_t i = 0; i < name_count; ++i) {
            ObjectDecl* decl = create_object_decl($1.data[i], @$);
            check_for_redefinition(context, decl->name, @$);
            decl->is_constant = true;
            decl->type = &universal_int_type;
            decl->init_expr = $5;
            push_declaration(context, &decl->base);
        }
    };

def_id_s :
    identifier {
        array_StringToken_init(&$$);
        array_StringToken_append(&$$, $1);
    }
  | def_id_s ',' identifier { array_StringToken_append(&$$, $3); }
    ;

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
  | IS_ASSIGNED expression { $$ = $2; }
    ;

type_decl :
    TYPE identifier discrim_part_opt type_completion ';' {
        // TODO: discriminant
        TypeDecl* decl = $4;
        // Note: decl->base.kind is set by the specific type_completion
        decl->base.line_num = @$;
        decl->name = $2;
        check_for_redefinition(context, decl->name, @$);
        push_declaration(context, &decl->base);
    };

discrim_part_opt :
    %empty
  | discrim_part
  | '(' BOX ')'
    ;

// TODO: incomplete types (i.e. case 1)
type_completion :
    %empty
  | IS type_def { $$ = $2; }
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
        decl->base.line_num = @$;
        decl->name = $2;
        check_for_redefinition(context, decl->name, @$);
        TypeDecl* base_type = find_type_decl(context, $4);
        if(!base_type) {
            error_print(@$, "Unknown base type: %s", ST($4));
            error_exit();
        }
        decl->u.subtype.base = base_type;
        push_declaration(context, &decl->base);
    };

// TODO: support other name variants (e.g. indexed, compound)
subtype_ind :
    name constraint {
        // TODO: propagate constraint somehow
        $$ = $1.name;
    }
  | name { $$ = $1.name; }
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
        TypeDecl* base_type = find_type_decl(context, $2);
        if(!base_type) {
            error_print(@$, "Unknown base type: %s", ST($2));
            error_exit();
        }
        $$->u.subtype.base = base_type;
    };

range_constraint :
    RANGE range { $$ = $2; }
    ;

range_constr_opt :
    %empty { $$ = NULL; }
  | range_constraint
    ;

range :
    simple_expression DOT_DOT simple_expression { $$ = make_binary_expr($1, OP_RANGE, $3); }
  | name '\'' RANGE
  | name '\'' RANGE '(' expression ')'
    ;

enumeration_type :
    '(' enum_id_s ')' {
        $$ = create_type_decl(TYPE_ENUM);
        $$->u.enum_.literals = $2.data;
        $$->u.enum_.literal_count = array_ExprPtr_size(&$2);
    };

enum_id_s :
    enum_id {
        array_ExprPtr_init(&$$);
        array_ExprPtr_append(&$$, $1);
    }
  | enum_id_s ',' enum_id { array_ExprPtr_append(&$$, $3); }
    ;

enum_id :
    identifier {
        $$ = create_expr(EXPR_NAME, @$);
        $$->u.name.name = $1;
    }
  | char_lit {
        $$ = create_expr(EXPR_CHAR_LIT, @$);
        $$->u.char_lit = $1;
    };

integer_type :
    range_constraint {
        $$ = create_type_decl(TYPE_INTEGER);
        $$->u.int_.range = $1;
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
    CASE simple_name IS pragma_s variant_s END CASE ';'
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
  | choice_s '|' choice
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
    simple_name {
        memset(&$$, 0, sizeof($$));
        $$.name = $1;
    }
  | indexed_comp
  | selected_comp
  | attribute
  | operator_symbol {
        memset(&$$, 0, sizeof($$));
        $$.name = $1;
        //TODO: lookup operator, determine its arity, and allocate args array
    };

mark :
    simple_name
  | mark '\'' attribute_id
  | mark '.' simple_name
    ;

simple_name :
    identifier
    ;

compound_name :
    simple_name
  | compound_name '.' simple_name
    ;

c_name_list :
    compound_name
  | c_name_list ',' compound_name
    ;

used_char :
    char_lit {
        $$ = create_expr(EXPR_CHAR_LIT, @$);
        $$->u.char_lit = $1;
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
    name '.' simple_name
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
        int base = get_base($1, @$);

        char num_buffer[128];
        num_buffer[0] = '\0';
        if(!prepare_num_str($1, num_buffer, sizeof(num_buffer))) {
            error_print(@$, "Numeric literal is too long to be processed (max supported is 127 characters)");
            error_exit();
        }

        // Note: don't overwrite $$ here since we are still using its value
        Expression* expr = create_expr(EXPR_INT_LIT, @$);
        if(mpz_init_set_str(expr->u.int_lit.value, num_buffer, base) < 0) {
            error_print(@$, "Invalid numeric literal: '%.*s' for base %u", SV($1), base);
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
  | expression logical relation       { $$ = make_binary_expr($1, $2, $3); }
  | expression short_circuit relation { $$ = make_binary_expr($1, $2, $3); }
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
  | simple_expression relational simple_expression { $$ = make_binary_expr($1, $2, $3); }
  | simple_expression membership range             { $$ = make_binary_expr($1, $2, $3); }
  | simple_expression membership name              {
        Expression* right = create_expr(EXPR_NAME, @3);
        right->u.name = $3;
        $$ = make_binary_expr($1, $2, right);
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
  | unary term                    { $$ = make_unary_expr($1, $2); }
  | simple_expression adding term { $$ = make_binary_expr($1, $2, $3); }
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
  | term multiplying factor { $$ = make_binary_expr($1, $2, $3); }
    ;

multiplying :
    '*' { $$ = OP_MULT; }
  | '/' { $$ = OP_DIVIDE; }
  | MOD { $$ = OP_MOD; }
  | REM { $$ = OP_REM; }
    ;

factor :
    primary
  | NOT primary           { $$ = make_unary_expr(OP_NOT, $2); }
  | ABS primary           { $$ = make_unary_expr(OP_ABS, $2); }
  | primary EXPON primary { $$ = make_binary_expr($1, OP_EXP, $3); }
    ;

primary :
    literal
  | name {
        $$ = create_expr(EXPR_NAME, @$);
        $$->u.name = $1;
    }
  | allocator
  | qualified
  | parenthesized_primary
    ;

parenthesized_primary :
    aggregate
  | '(' expression ')' { $$ = $2; }
    ;

qualified :
    name '\'' parenthesized_primary
    ;

allocator :
    NEW name
  | NEW qualified
    ;

statement_s :
    statement
  | statement_s statement {
        $$ = $1;
        $$->next = $2;
        $$ = $2;
    };

statement :
    unlabeled
  | goto_label statement {
        check_for_redefinition(context, $1, @1);
        LabelDecl* label = create_label($1, @1);
        push_declaration(context, (Declaration*)label);
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

// TODO: name
assign_stmt :
    name IS_ASSIGNED expression ';' {
        $$ = create_stmt(STMT_ASSIGN, @$);
        $$->u.assign.expr = $3;
    };

if_stmt :
    IF cond_clause_s else_opt END IF ';'
    ;

cond_clause_s :
    cond_clause
  | cond_clause_s ELSIF cond_clause
    ;

cond_clause :
    cond_part statement_s
    ;

cond_part :
    condition THEN { $$ = $1; }
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
  | alternative_s alternative
    ;

alternative :
    WHEN choice_s RIGHT_SHAFT statement_s
    ;

// TODO: label_opt and id_opt
loop_stmt :
    label_opt loop_content id_opt ';' { $$ = $2; }
    ;

label_opt :
    %empty
  | identifier ':'
    ;

loop_content :
    basic_loop {
        $$ = create_stmt(STMT_LOOP, @$);
        $$->u.loop.kind = LOOP_WHILE;
        $$->u.loop.stmts = $1;
        // Create condition so this becomes a 'while True' loop
        // TODO: should be a boolean literal
        Expression* condition = create_expr(EXPR_INT_LIT, @$);
        mpz_init_set_ui(condition->u.int_lit.value, 1);
        $$->u.loop.u.while_.condition = condition;
    }
  | WHILE condition basic_loop {
        $$ = create_stmt(STMT_LOOP, @$);
        $$->u.loop.kind = LOOP_WHILE;
        $$->u.loop.stmts = $3;
        $$->u.loop.u.while_.condition = $2;
    }
  | iter_part reverse_opt discrete_range basic_loop {
        // TODO: identifier
        $$ = create_stmt(STMT_LOOP, @$);
        $$->u.loop.kind = LOOP_FOR;
        $$->u.loop.reverse = $2;
        $$->u.loop.stmts = $4;
    };

iter_part :
    FOR identifier IN
    ;

reverse_opt :
    %empty  { $$ = false; }
  | REVERSE { $$ = true; }
    ;

basic_loop :
    LOOP statement_s END LOOP { $$ = $2; }
    ;

id_opt :
    %empty
  | designator
    ;

// TODO: label
block :
    label_opt block_decl block_body END id_opt ';' {
        $$ = create_stmt(STMT_BLOCK, @$);
        $$->u.block.stmts = $3;
        // Close scope if needed (i.e. if there was a declaration section)
        if($2) {
            end_scope(context, @4);
        }
    };

block_decl :
    %empty  { $$ = false; }
  | DECLARE { begin_scope(context, @1); } decl_part { $$ = true; }
    ;

block_body :
    BEGiN handled_stmt_s { $$ = $2; }
    ;

// TODO: exception handler
handled_stmt_s :
    statement_s except_handler_part_opt { $$ = $1; }
    ;

except_handler_part_opt :
    %empty
  | except_handler_part
    ;

exit_stmt :
    EXIT name_opt when_opt ';' {
        $$ = create_stmt(STMT_EXIT, @$);
        // TODO: name_opt
        $$->u.exit.condition = $3;
    };

name_opt :
    %empty
  | name
    ;

when_opt :
    %empty         { $$ = NULL; }
  | WHEN condition { $$ = $2; }
    ;

return_stmt :
    RETURN ';'    { $$ = create_stmt(STMT_RETURN, @$); }
  | RETURN expression ';' {
        $$ = create_stmt(STMT_RETURN, @$);
        $$->u.return_.expr = $2;
    };

goto_stmt :
    GOTO name ';' {
        if($2.arg_count != 0) {
            error_print(@2, "Invalid label name (must be a simple name)");
            error_exit();
        }
        StringToken label_name = $2.name;

        $$ = create_stmt(STMT_GOTO, @$);
        LabelDecl* label = find_label(context, label_name);
        if(label) {
            // Label is defined prior to the goto statement
            $$->u.goto_.label = label;
        } else {
            // Label is not defined yet
            check_for_redefinition(context, label_name, @2);
            // Define a placeholder label
            // TODO: in semantic analysis, verify that all placeholder labels are filled in
            LabelDecl* label = create_label(label_name, @2);
            $$->u.goto_.label = label;
            push_declaration(context, (Declaration*)label);
        }
    };

subprog_decl :
    subprog_spec ';'
  | generic_subp_inst ';'
    ;

subprog_spec :
    PROCEDURE simple_name <subprogram_decl>{
        begin_scope(context, @2);
        // TODO: check for name conflict
        $<subprogram_decl>$ = create_subprogram_decl($2, @2);
    }
    formal_part_opt
  | FUNCTION designator <subprogram_decl>{
        begin_scope(context, @2);
        // TODO: check for name conflict
        $<subprogram_decl>$ = create_subprogram_decl($2, @2);
    }
    formal_part_opt RETURN name
  | FUNCTION designator  /* for generic inst and generic rename */
    ;

designator :
    simple_name
  | char_string { $$ = string_pool_to_token($1); }
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
    subprog_spec IS
    ;

subprog_body :
    subprog_spec_is_push decl_part block_body END id_opt ';'
    ;

procedure_call :
    name ';' {
        $$ = create_stmt(STMT_NAME, @$);
        $$->u.name = $1;
        $$->u.name.is_function = false;
        $$->u.name.is_subprogram = true;
    };

pkg_decl :
    pkg_spec ';'
  | generic_pkg_inst ';'
    ;

pkg_spec :
    PACKAGE compound_name IS decl_item_s private_part END c_id_opt
    ;

private_part :
    %empty
  | PRIVATE decl_item_s
    ;

c_id_opt :
    %empty
  | compound_name
    ;

pkg_body :
    PACKAGE BODY compound_name IS decl_part body_opt END c_id_opt ';'
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

use_clause :
    USE name_s ';'
    ;

name_s :
    name
  | name_s ',' name
    ;

rename_decl :
    def_id_s ':' object_qualifier_opt subtype_ind renames ';'
  | def_id_s ':' EXCEPTION renames ';'
  | rename_unit
    ;

rename_unit :
    PACKAGE compound_name renames ';'
  | subprog_spec renames ';'
  | generic_formal_part PACKAGE compound_name renames ';'
  | generic_formal_part subprog_spec renames ';'
    ;

renames :
    RENAMES name
    ;

compilation :
    %empty
  | compilation comp_unit
  | pragma pragma_s
    ;

comp_unit :
    context_spec private_opt unit pragma_s
  | private_opt unit pragma_s
    ;

private_opt :
    %empty
  | PRIVATE
    ;

context_spec :
    with_clause use_clause_opt
  | context_spec with_clause use_clause_opt
  | context_spec pragma
    ;

with_clause :
    WITH c_name_list ';'
    ;

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
    SEPARATE '(' compound_name ')' subunit_body
    ;

subunit_body :
    subprog_body
  | pkg_body
    ;

body_stub :
    PACKAGE BODY compound_name IS SEPARATE ';'
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
  | TYPE simple_name generic_discrim_part_opt IS generic_type_def ';'
  | WITH PROCEDURE simple_name formal_part_opt subp_default ';'
  | WITH FUNCTION designator formal_part_opt RETURN name subp_default ';'
  | WITH PACKAGE simple_name IS NEW name '(' BOX ')' ';'
  | WITH PACKAGE simple_name IS NEW name ';'
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
    PACKAGE compound_name IS generic_inst
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
Expression* make_binary_expr(Expression* left, BinaryOperator op, Expression* right)
{
    Expression* expr = create_expr(EXPR_BINARY, left->line_num);
    expr->u.binary.left = left;
    expr->u.binary.op = op;
    expr->u.binary.right = right;
    return expr;
}

static
Expression* make_unary_expr(UnaryOperator op, Expression* right)
{
    Expression* expr = create_expr(EXPR_UNARY, right->line_num);
    expr->kind = EXPR_UNARY;
    expr->u.unary.op = op;
    expr->u.unary.right = right;
    return expr;
}

static
void begin_scope(ParseContext* context, uint32_t line_num)
{
    if(context->curr_scope_idx + 1u >= cnt_of_array(context->scope_stack)) {
        error_print(line_num, "Too many nested scopes (maximum is %u nested scopes)", cnt_of_array(context->scope_stack));
        error_exit();
    }
    ++context->curr_scope_idx;
}

static
void end_scope(ParseContext* context, uint32_t line_num)
{
    if(context->curr_scope_idx == 0) {
        error_print(line_num, "Attempted to exit top-level region");
        error_exit();
    }
    --context->curr_scope_idx;
    // TODO: go through all decls in curr_scope, remove named ones from
    // symbol table
}

static
void push_declaration(ParseContext* context, Declaration* decl)
{
    append_decl(curr_scope, decl);
    StringToken name = get_decl_name(decl);
    // Add named declarations to the symbol table
    if(name) {
        if(context->symbol_table_size * 7 >= context->symbol_table_capacity) {
            // Grow if table is at least 70% full
            grow_table(context);
        }
        Declaration** first_overload = find_bucket(context, name);
        // Prepend new declaration to the bucket
        decl->next_overload = *first_overload;
        *first_overload = decl;
        ++context->symbol_table_size;
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
    Declaration** bucket = find_bucket(context, name);
    if(bucket) {
        for(Declaration* decl = *bucket; decl; decl = decl->next_overload) {
            if(decl->kind == DECL_TYPE) {
                return (TypeDecl*)decl;
            }
        }
    }
    return NULL;
}

static
LabelDecl* find_label(ParseContext* context, StringToken name)
{
    Declaration** bucket = find_bucket(context, name);
    if(bucket) {
        for(Declaration* decl = *bucket; decl; decl = decl->next_overload) {
            if(decl->kind == DECL_LABEL) {
                return (LabelDecl*)decl;
            }
        }
    }
    return NULL;
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
Declaration** find_bucket(ParseContext* context, StringToken name)
{
    uint32_t hash = hash_fnv(name);
    uint32_t capacity = context->symbol_table_capacity;
    uint32_t idx = hash % capacity;
    Declaration** bucket = context->symbol_table + idx;
    // Linear probing to resolve conflicts (stop when we find an empty
    // bucket or a bucket with the target name)
    while(*bucket && get_decl_name(*bucket) != name) {
        ++idx;
        idx %= capacity;
        bucket = context->symbol_table + idx;
    }
    return bucket;
}

static
void check_for_redefinition(ParseContext* context, StringToken name, uint32_t line_num)
{
    Declaration* existing_decl = find_decl_in_scope(curr_scope, name);
    if(existing_decl) {
        error_print(line_num, "Redefinition of '%s' within same declarative region", ST(name));
        error_print(existing_decl->line_num, "Previous definition here");
        error_exit();
    }
}

static
Expression* create_expr(ExprKind kind, uint32_t line_num)
{
    Expression* expr = calloc(1, sizeof(Expression));
    expr->kind = kind;
    expr->line_num = line_num;
    return expr;
}

static
Statement* create_stmt(StmtKind kind, uint32_t line_num)
{
    Statement* stmt = calloc(1, sizeof(Statement));
    stmt->kind = kind;
    stmt->line_num = line_num;
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
ObjectDecl* create_object_decl(StringToken name, uint32_t line_num)
{
    ObjectDecl* decl = calloc(1, sizeof(ObjectDecl));
    decl->base.kind = DECL_OBJECT;
    decl->base.line_num = line_num;
    decl->name = name;
    return decl;
}

static
SubprogramDecl* create_subprogram_decl(StringToken name, uint32_t line_num)
{
    SubprogramDecl* decl = calloc(1, sizeof(SubprogramDecl));
    decl->base.kind = DECL_SUBPROGRAM;
    decl->base.line_num = line_num;
    decl->name = name;
    return decl;
}

static
LabelDecl* create_label(StringToken name, uint32_t line_num)
{
    LabelDecl* label = calloc(1, sizeof(LabelDecl));
    label->base.kind = DECL_LABEL;
    label->base.line_num = line_num;
    label->name = name;
    return label;
}

static
int get_base(StringView num_str, uint32_t line_num)
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
            error_print(line_num, "Numeric literal has invalid base (%d). Bases must be in range [1, 16]", base);
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

// FNV-1 hash (32-bit variant)
static
uint32_t hash_fnv(StringToken token)
{
    uint32_t hash = 2166136261;
    const char* bytes = (const char*)&token;
    for(int i = 0; i < 4; ++i) {
        hash *= 16777619;
        hash ^= bytes[i];
    }
    return hash;
}

static
void grow_table(ParseContext* context)
{
    uint32_t old_capacity = context->symbol_table_capacity;
    Declaration** old_buckets = context->symbol_table;

    context->symbol_table_capacity *= TABLE_GROWTH_FACTOR;
    context->symbol_table = calloc(context->symbol_table_capacity, sizeof(DeclList));
    for(uint32_t i = 0; i < old_capacity; ++i) {
        // Skip over empty buckets (no need to copy them to new table)
        if(old_buckets[i]) {
            StringToken name = get_decl_name(old_buckets[i]);
            assert(name);
            Declaration** new_bucket = find_bucket(context, name);
            *new_bucket = old_buckets[i];
        }
    }
    free(old_buckets);
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
            // This kind of declaration has no associated name
            break;
    }
    return name;
}
