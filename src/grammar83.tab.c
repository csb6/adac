/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1





# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "parser.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_DOT_DOT = 3,                    /* DOT_DOT  */
  YYSYMBOL_BOX = 4,                        /* BOX  */
  YYSYMBOL_LT_EQ = 5,                      /* LT_EQ  */
  YYSYMBOL_EXPON = 6,                      /* EXPON  */
  YYSYMBOL_NE = 7,                         /* NE  */
  YYSYMBOL_GE = 8,                         /* GE  */
  YYSYMBOL_IS_ASSIGNED = 9,                /* IS_ASSIGNED  */
  YYSYMBOL_RIGHT_SHAFT = 10,               /* RIGHT_SHAFT  */
  YYSYMBOL_ABORT = 11,                     /* ABORT  */
  YYSYMBOL_ABS = 12,                       /* ABS  */
  YYSYMBOL_ACCEPT = 13,                    /* ACCEPT  */
  YYSYMBOL_ACCESS = 14,                    /* ACCESS  */
  YYSYMBOL_ALL = 15,                       /* ALL  */
  YYSYMBOL_AND = 16,                       /* AND  */
  YYSYMBOL_ARRAY = 17,                     /* ARRAY  */
  YYSYMBOL_AT = 18,                        /* AT  */
  YYSYMBOL_BEGiN = 19,                     /* BEGiN  */
  YYSYMBOL_BODY = 20,                      /* BODY  */
  YYSYMBOL_CASE = 21,                      /* CASE  */
  YYSYMBOL_CONSTANT = 22,                  /* CONSTANT  */
  YYSYMBOL_DECLARE = 23,                   /* DECLARE  */
  YYSYMBOL_DELAY = 24,                     /* DELAY  */
  YYSYMBOL_DELTA = 25,                     /* DELTA  */
  YYSYMBOL_DIGITS = 26,                    /* DIGITS  */
  YYSYMBOL_DO = 27,                        /* DO  */
  YYSYMBOL_ELSE = 28,                      /* ELSE  */
  YYSYMBOL_ELSIF = 29,                     /* ELSIF  */
  YYSYMBOL_END = 30,                       /* END  */
  YYSYMBOL_ENTRY = 31,                     /* ENTRY  */
  YYSYMBOL_EXCEPTION = 32,                 /* EXCEPTION  */
  YYSYMBOL_EXIT = 33,                      /* EXIT  */
  YYSYMBOL_FOR = 34,                       /* FOR  */
  YYSYMBOL_FUNCTION = 35,                  /* FUNCTION  */
  YYSYMBOL_GENERIC = 36,                   /* GENERIC  */
  YYSYMBOL_GOTO = 37,                      /* GOTO  */
  YYSYMBOL_IF = 38,                        /* IF  */
  YYSYMBOL_IN = 39,                        /* IN  */
  YYSYMBOL_IS = 40,                        /* IS  */
  YYSYMBOL_LIMITED = 41,                   /* LIMITED  */
  YYSYMBOL_LOOP = 42,                      /* LOOP  */
  YYSYMBOL_MOD = 43,                       /* MOD  */
  YYSYMBOL_NEW = 44,                       /* NEW  */
  YYSYMBOL_NOT = 45,                       /* NOT  */
  YYSYMBOL_NuLL = 46,                      /* NuLL  */
  YYSYMBOL_OF = 47,                        /* OF  */
  YYSYMBOL_OR = 48,                        /* OR  */
  YYSYMBOL_OTHERS = 49,                    /* OTHERS  */
  YYSYMBOL_OUT = 50,                       /* OUT  */
  YYSYMBOL_PACKAGE = 51,                   /* PACKAGE  */
  YYSYMBOL_PRAGMA = 52,                    /* PRAGMA  */
  YYSYMBOL_PRIVATE = 53,                   /* PRIVATE  */
  YYSYMBOL_PROCEDURE = 54,                 /* PROCEDURE  */
  YYSYMBOL_RAISE = 55,                     /* RAISE  */
  YYSYMBOL_RANGE = 56,                     /* RANGE  */
  YYSYMBOL_RECORD = 57,                    /* RECORD  */
  YYSYMBOL_REM = 58,                       /* REM  */
  YYSYMBOL_RENAMES = 59,                   /* RENAMES  */
  YYSYMBOL_RETURN = 60,                    /* RETURN  */
  YYSYMBOL_REVERSE = 61,                   /* REVERSE  */
  YYSYMBOL_SELECT = 62,                    /* SELECT  */
  YYSYMBOL_SEPARATE = 63,                  /* SEPARATE  */
  YYSYMBOL_SUBTYPE = 64,                   /* SUBTYPE  */
  YYSYMBOL_TASK = 65,                      /* TASK  */
  YYSYMBOL_TERMINATE = 66,                 /* TERMINATE  */
  YYSYMBOL_THEN = 67,                      /* THEN  */
  YYSYMBOL_TYPE = 68,                      /* TYPE  */
  YYSYMBOL_USE = 69,                       /* USE  */
  YYSYMBOL_WHEN = 70,                      /* WHEN  */
  YYSYMBOL_WHILE = 71,                     /* WHILE  */
  YYSYMBOL_WITH = 72,                      /* WITH  */
  YYSYMBOL_XOR = 73,                       /* XOR  */
  YYSYMBOL_char_lit = 74,                  /* char_lit  */
  YYSYMBOL_identifier = 75,                /* identifier  */
  YYSYMBOL_char_string = 76,               /* char_string  */
  YYSYMBOL_numeric_lit = 77,               /* numeric_lit  */
  YYSYMBOL_goto_label = 78,                /* goto_label  */
  YYSYMBOL_79_ = 79,                       /* ';'  */
  YYSYMBOL_80_ = 80,                       /* '('  */
  YYSYMBOL_81_ = 81,                       /* ')'  */
  YYSYMBOL_82_ = 82,                       /* ','  */
  YYSYMBOL_83_ = 83,                       /* ':'  */
  YYSYMBOL_84_ = 84,                       /* '\''  */
  YYSYMBOL_85_ = 85,                       /* '|'  */
  YYSYMBOL_86_ = 86,                       /* '.'  */
  YYSYMBOL_87_ = 87,                       /* '='  */
  YYSYMBOL_88_ = 88,                       /* '<'  */
  YYSYMBOL_89_ = 89,                       /* '>'  */
  YYSYMBOL_90_ = 90,                       /* '+'  */
  YYSYMBOL_91_ = 91,                       /* '-'  */
  YYSYMBOL_92_ = 92,                       /* '&'  */
  YYSYMBOL_93_ = 93,                       /* '*'  */
  YYSYMBOL_94_ = 94,                       /* '/'  */
  YYSYMBOL_YYACCEPT = 95,                  /* $accept  */
  YYSYMBOL_goal_symbol = 96,               /* goal_symbol  */
  YYSYMBOL_pragma = 97,                    /* pragma  */
  YYSYMBOL_pragma_arg_s = 98,              /* pragma_arg_s  */
  YYSYMBOL_pragma_arg = 99,                /* pragma_arg  */
  YYSYMBOL_pragma_s = 100,                 /* pragma_s  */
  YYSYMBOL_decl = 101,                     /* decl  */
  YYSYMBOL_object_decl = 102,              /* object_decl  */
  YYSYMBOL_number_decl = 103,              /* number_decl  */
  YYSYMBOL_def_id_s = 104,                 /* def_id_s  */
  YYSYMBOL_object_qualifier_opt = 105,     /* object_qualifier_opt  */
  YYSYMBOL_object_subtype_def = 106,       /* object_subtype_def  */
  YYSYMBOL_init_opt = 107,                 /* init_opt  */
  YYSYMBOL_type_decl = 108,                /* type_decl  */
  YYSYMBOL_discrim_part_opt = 109,         /* discrim_part_opt  */
  YYSYMBOL_type_completion = 110,          /* type_completion  */
  YYSYMBOL_type_def = 111,                 /* type_def  */
  YYSYMBOL_subtype_decl = 112,             /* subtype_decl  */
  YYSYMBOL_subtype_ind = 113,              /* subtype_ind  */
  YYSYMBOL_constraint = 114,               /* constraint  */
  YYSYMBOL_decimal_digits_constraint = 115, /* decimal_digits_constraint  */
  YYSYMBOL_derived_type = 116,             /* derived_type  */
  YYSYMBOL_range_constraint = 117,         /* range_constraint  */
  YYSYMBOL_range_constr_opt = 118,         /* range_constr_opt  */
  YYSYMBOL_range = 119,                    /* range  */
  YYSYMBOL_enumeration_type = 120,         /* enumeration_type  */
  YYSYMBOL_enum_id_s = 121,                /* enum_id_s  */
  YYSYMBOL_enum_id = 122,                  /* enum_id  */
  YYSYMBOL_integer_type = 123,             /* integer_type  */
  YYSYMBOL_real_type = 124,                /* real_type  */
  YYSYMBOL_float_type = 125,               /* float_type  */
  YYSYMBOL_fixed_type = 126,               /* fixed_type  */
  YYSYMBOL_array_type = 127,               /* array_type  */
  YYSYMBOL_unconstr_array_type = 128,      /* unconstr_array_type  */
  YYSYMBOL_constr_array_type = 129,        /* constr_array_type  */
  YYSYMBOL_component_subtype_def = 130,    /* component_subtype_def  */
  YYSYMBOL_index_s = 131,                  /* index_s  */
  YYSYMBOL_index = 132,                    /* index  */
  YYSYMBOL_iter_index_constraint = 133,    /* iter_index_constraint  */
  YYSYMBOL_iter_discrete_range_s = 134,    /* iter_discrete_range_s  */
  YYSYMBOL_discrete_range = 135,           /* discrete_range  */
  YYSYMBOL_record_type = 136,              /* record_type  */
  YYSYMBOL_record_def = 137,               /* record_def  */
  YYSYMBOL_comp_list = 138,                /* comp_list  */
  YYSYMBOL_comp_decl_s = 139,              /* comp_decl_s  */
  YYSYMBOL_variant_part_opt = 140,         /* variant_part_opt  */
  YYSYMBOL_comp_decl = 141,                /* comp_decl  */
  YYSYMBOL_discrim_part = 142,             /* discrim_part  */
  YYSYMBOL_discrim_spec_s = 143,           /* discrim_spec_s  */
  YYSYMBOL_discrim_spec = 144,             /* discrim_spec  */
  YYSYMBOL_access_opt = 145,               /* access_opt  */
  YYSYMBOL_variant_part = 146,             /* variant_part  */
  YYSYMBOL_variant_s = 147,                /* variant_s  */
  YYSYMBOL_variant = 148,                  /* variant  */
  YYSYMBOL_choice_s = 149,                 /* choice_s  */
  YYSYMBOL_choice = 150,                   /* choice  */
  YYSYMBOL_discrete_with_range = 151,      /* discrete_with_range  */
  YYSYMBOL_access_type = 152,              /* access_type  */
  YYSYMBOL_decl_part = 153,                /* decl_part  */
  YYSYMBOL_decl_item_s = 154,              /* decl_item_s  */
  YYSYMBOL_decl_item_s1 = 155,             /* decl_item_s1  */
  YYSYMBOL_decl_item = 156,                /* decl_item  */
  YYSYMBOL_decl_item_or_body_s1 = 157,     /* decl_item_or_body_s1  */
  YYSYMBOL_decl_item_or_body = 158,        /* decl_item_or_body  */
  YYSYMBOL_body = 159,                     /* body  */
  YYSYMBOL_name = 160,                     /* name  */
  YYSYMBOL_mark = 161,                     /* mark  */
  YYSYMBOL_simple_name = 162,              /* simple_name  */
  YYSYMBOL_simple_name_list = 163,         /* simple_name_list  */
  YYSYMBOL_used_char = 164,                /* used_char  */
  YYSYMBOL_operator_symbol = 165,          /* operator_symbol  */
  YYSYMBOL_indexed_comp = 166,             /* indexed_comp  */
  YYSYMBOL_value_s = 167,                  /* value_s  */
  YYSYMBOL_value = 168,                    /* value  */
  YYSYMBOL_selected_comp = 169,            /* selected_comp  */
  YYSYMBOL_attribute = 170,                /* attribute  */
  YYSYMBOL_attribute_id = 171,             /* attribute_id  */
  YYSYMBOL_literal = 172,                  /* literal  */
  YYSYMBOL_aggregate = 173,                /* aggregate  */
  YYSYMBOL_value_s_2 = 174,                /* value_s_2  */
  YYSYMBOL_comp_assoc = 175,               /* comp_assoc  */
  YYSYMBOL_expression = 176,               /* expression  */
  YYSYMBOL_logical = 177,                  /* logical  */
  YYSYMBOL_short_circuit = 178,            /* short_circuit  */
  YYSYMBOL_relation = 179,                 /* relation  */
  YYSYMBOL_relational = 180,               /* relational  */
  YYSYMBOL_membership = 181,               /* membership  */
  YYSYMBOL_simple_expression = 182,        /* simple_expression  */
  YYSYMBOL_unary = 183,                    /* unary  */
  YYSYMBOL_adding = 184,                   /* adding  */
  YYSYMBOL_term = 185,                     /* term  */
  YYSYMBOL_multiplying = 186,              /* multiplying  */
  YYSYMBOL_factor = 187,                   /* factor  */
  YYSYMBOL_primary = 188,                  /* primary  */
  YYSYMBOL_parenthesized_primary = 189,    /* parenthesized_primary  */
  YYSYMBOL_qualified = 190,                /* qualified  */
  YYSYMBOL_allocator = 191,                /* allocator  */
  YYSYMBOL_statement_s = 192,              /* statement_s  */
  YYSYMBOL_statement = 193,                /* statement  */
  YYSYMBOL_unlabeled = 194,                /* unlabeled  */
  YYSYMBOL_simple_stmt = 195,              /* simple_stmt  */
  YYSYMBOL_compound_stmt = 196,            /* compound_stmt  */
  YYSYMBOL_null_stmt = 197,                /* null_stmt  */
  YYSYMBOL_assign_stmt = 198,              /* assign_stmt  */
  YYSYMBOL_if_stmt = 199,                  /* if_stmt  */
  YYSYMBOL_cond_clause_s = 200,            /* cond_clause_s  */
  YYSYMBOL_cond_clause = 201,              /* cond_clause  */
  YYSYMBOL_cond_part = 202,                /* cond_part  */
  YYSYMBOL_condition = 203,                /* condition  */
  YYSYMBOL_else_opt = 204,                 /* else_opt  */
  YYSYMBOL_case_stmt = 205,                /* case_stmt  */
  YYSYMBOL_case_hdr = 206,                 /* case_hdr  */
  YYSYMBOL_alternative_s = 207,            /* alternative_s  */
  YYSYMBOL_alternative = 208,              /* alternative  */
  YYSYMBOL_loop_stmt = 209,                /* loop_stmt  */
  YYSYMBOL_label_opt = 210,                /* label_opt  */
  YYSYMBOL_loop_content = 211,             /* loop_content  */
  YYSYMBOL_iter_part = 212,                /* iter_part  */
  YYSYMBOL_reverse_opt = 213,              /* reverse_opt  */
  YYSYMBOL_basic_loop = 214,               /* basic_loop  */
  YYSYMBOL_id_opt = 215,                   /* id_opt  */
  YYSYMBOL_block = 216,                    /* block  */
  YYSYMBOL_block_decl = 217,               /* block_decl  */
  YYSYMBOL_218_1 = 218,                    /* $@1  */
  YYSYMBOL_block_body = 219,               /* block_body  */
  YYSYMBOL_handled_stmt_s = 220,           /* handled_stmt_s  */
  YYSYMBOL_except_handler_part_opt = 221,  /* except_handler_part_opt  */
  YYSYMBOL_exit_stmt = 222,                /* exit_stmt  */
  YYSYMBOL_name_opt = 223,                 /* name_opt  */
  YYSYMBOL_when_opt = 224,                 /* when_opt  */
  YYSYMBOL_return_stmt = 225,              /* return_stmt  */
  YYSYMBOL_goto_stmt = 226,                /* goto_stmt  */
  YYSYMBOL_subprog_decl = 227,             /* subprog_decl  */
  YYSYMBOL_subprog_spec = 228,             /* subprog_spec  */
  YYSYMBOL_229_2 = 229,                    /* @2  */
  YYSYMBOL_230_3 = 230,                    /* @3  */
  YYSYMBOL_designator = 231,               /* designator  */
  YYSYMBOL_formal_part_opt = 232,          /* formal_part_opt  */
  YYSYMBOL_formal_part = 233,              /* formal_part  */
  YYSYMBOL_param_s = 234,                  /* param_s  */
  YYSYMBOL_param = 235,                    /* param  */
  YYSYMBOL_mode = 236,                     /* mode  */
  YYSYMBOL_subprog_spec_is_push = 237,     /* subprog_spec_is_push  */
  YYSYMBOL_subprog_body = 238,             /* subprog_body  */
  YYSYMBOL_procedure_call = 239,           /* procedure_call  */
  YYSYMBOL_pkg_decl = 240,                 /* pkg_decl  */
  YYSYMBOL_pkg_spec = 241,                 /* pkg_spec  */
  YYSYMBOL_private_part = 242,             /* private_part  */
  YYSYMBOL_simple_name_opt = 243,          /* simple_name_opt  */
  YYSYMBOL_pkg_body = 244,                 /* pkg_body  */
  YYSYMBOL_body_opt = 245,                 /* body_opt  */
  YYSYMBOL_private_type = 246,             /* private_type  */
  YYSYMBOL_limited_opt = 247,              /* limited_opt  */
  YYSYMBOL_use_clause = 248,               /* use_clause  */
  YYSYMBOL_name_s = 249,                   /* name_s  */
  YYSYMBOL_rename_decl = 250,              /* rename_decl  */
  YYSYMBOL_rename_unit = 251,              /* rename_unit  */
  YYSYMBOL_renames = 252,                  /* renames  */
  YYSYMBOL_comp_unit = 253,                /* comp_unit  */
  YYSYMBOL_context_spec = 254,             /* context_spec  */
  YYSYMBOL_with_clause = 255,              /* with_clause  */
  YYSYMBOL_use_clause_opt = 256,           /* use_clause_opt  */
  YYSYMBOL_unit = 257,                     /* unit  */
  YYSYMBOL_subunit = 258,                  /* subunit  */
  YYSYMBOL_subunit_body = 259,             /* subunit_body  */
  YYSYMBOL_body_stub = 260,                /* body_stub  */
  YYSYMBOL_exception_decl = 261,           /* exception_decl  */
  YYSYMBOL_except_handler_part = 262,      /* except_handler_part  */
  YYSYMBOL_exception_handler = 263,        /* exception_handler  */
  YYSYMBOL_except_choice_s = 264,          /* except_choice_s  */
  YYSYMBOL_except_choice = 265,            /* except_choice  */
  YYSYMBOL_raise_stmt = 266,               /* raise_stmt  */
  YYSYMBOL_generic_decl = 267,             /* generic_decl  */
  YYSYMBOL_generic_formal_part = 268,      /* generic_formal_part  */
  YYSYMBOL_generic_formal = 269,           /* generic_formal  */
  YYSYMBOL_generic_discrim_part_opt = 270, /* generic_discrim_part_opt  */
  YYSYMBOL_subp_default = 271,             /* subp_default  */
  YYSYMBOL_generic_type_def = 272,         /* generic_type_def  */
  YYSYMBOL_generic_derived_type = 273,     /* generic_derived_type  */
  YYSYMBOL_generic_subp_inst = 274,        /* generic_subp_inst  */
  YYSYMBOL_generic_pkg_inst = 275,         /* generic_pkg_inst  */
  YYSYMBOL_generic_inst = 276,             /* generic_inst  */
  YYSYMBOL_rep_spec = 277,                 /* rep_spec  */
  YYSYMBOL_attrib_def = 278,               /* attrib_def  */
  YYSYMBOL_record_type_spec = 279,         /* record_type_spec  */
  YYSYMBOL_align_opt = 280,                /* align_opt  */
  YYSYMBOL_comp_loc_s = 281,               /* comp_loc_s  */
  YYSYMBOL_address_spec = 282,             /* address_spec  */
  YYSYMBOL_code_stmt = 283                 /* code_stmt  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;



/* Unqualified %code blocks.  */
#line 78 "grammar83.y"

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
    DEFINE_ARRAY_OPS(Choice)
    DEFINE_LINKED_LIST_OPS(Decl)
    DEFINE_LINKED_LIST_OPS(Stmt)
    DEFINE_LINKED_LIST_OPS(Alt)

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
    CompilationUnit* create_comp_unit(CompilationUnitKind kind);

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

#line 479 "grammar83.tab.c"

#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
# define YYCOPY_NEEDED 1
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  35
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1373

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  95
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  189
/* YYNRULES -- Number of rules.  */
#define YYNRULES  402
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  718

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   333


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    92,    84,
      80,    81,    93,    90,    82,    91,    86,    94,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    83,    79,
      88,    87,    89,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    85,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   250,   250,   254,   255,   259,   260,   264,   265,   269,
     270,   274,   275,   276,   280,   284,   288,   289,   290,   291,
     292,   293,   297,   323,   338,   342,   347,   348,   352,   353,
     357,   358,   362,   374,   375,   376,   381,   382,   386,   387,
     388,   389,   390,   391,   392,   393,   397,   414,   418,   422,
     423,   427,   431,   442,   446,   447,   451,   452,   453,   457,
     465,   469,   475,   479,   485,   489,   493,   494,   498,   502,
     503,   507,   508,   512,   516,   520,   524,   525,   529,   533,
     537,   538,   542,   543,   547,   551,   552,   556,   557,   558,
     562,   563,   567,   568,   572,   573,   577,   581,   582,   586,
     587,   591,   592,   596,   600,   601,   605,   609,   613,   619,
     623,   624,   628,   629,   633,   634,   638,   639,   643,   644,
     648,   649,   655,   656,   657,   658,   662,   663,   669,   673,
     677,   678,   682,   686,   687,   688,   689,   696,   697,   698,
     702,   706,   707,   711,   717,   721,   725,   726,   730,   731,
     732,   733,   737,   738,   739,   740,   744,   748,   749,   750,
     751,   755,   774,   775,   779,   780,   781,   782,   783,   787,
     788,   792,   796,   797,   798,   802,   803,   804,   808,   809,
     814,   815,   816,   817,   824,   825,   826,   827,   828,   829,
     833,   834,   838,   839,   840,   844,   845,   849,   850,   851,
     855,   856,   860,   861,   862,   863,   867,   868,   869,   870,
     874,   875,   879,   880,   881,   885,   886,   890,   894,   895,
     899,   903,   909,   910,   918,   919,   920,   924,   925,   926,
     927,   928,   929,   930,   931,   932,   936,   937,   938,   939,
     943,   947,   956,   967,   968,   974,   981,   985,   989,   990,
     994,  1001,  1007,  1008,  1014,  1023,  1027,  1028,  1032,  1042,
    1048,  1058,  1066,  1067,  1071,  1075,  1076,  1081,  1092,  1093,
    1093,  1097,  1102,  1106,  1107,  1111,  1118,  1119,  1123,  1124,
    1128,  1129,  1135,  1159,  1160,  1165,  1165,  1171,  1171,  1177,
    1181,  1182,  1186,  1187,  1191,  1195,  1196,  1200,  1201,  1205,
    1206,  1207,  1208,  1212,  1216,  1223,  1232,  1233,  1237,  1246,
    1247,  1251,  1252,  1256,  1265,  1266,  1270,  1274,  1275,  1279,
    1283,  1284,  1288,  1289,  1290,  1294,  1295,  1296,  1297,  1301,
    1305,  1306,  1310,  1311,  1312,  1316,  1320,  1321,  1325,  1326,
    1327,  1331,  1335,  1336,  1337,  1341,  1345,  1346,  1350,  1351,
    1355,  1359,  1360,  1364,  1365,  1369,  1370,  1374,  1375,  1379,
    1383,  1384,  1388,  1389,  1393,  1394,  1395,  1396,  1397,  1398,
    1399,  1403,  1404,  1405,  1409,  1410,  1411,  1415,  1416,  1417,
    1418,  1419,  1420,  1421,  1422,  1423,  1424,  1428,  1429,  1433,
    1437,  1441,  1445,  1446,  1447,  1451,  1455,  1459,  1460,  1464,
    1465,  1469,  1473
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  static const char *const yy_sname[] =
  {
  "end of file", "error", "invalid token", "DOT_DOT", "BOX", "LT_EQ",
  "EXPON", "NE", "GE", "IS_ASSIGNED", "RIGHT_SHAFT", "ABORT", "ABS",
  "ACCEPT", "ACCESS", "ALL", "AND", "ARRAY", "AT", "BEGiN", "BODY", "CASE",
  "CONSTANT", "DECLARE", "DELAY", "DELTA", "DIGITS", "DO", "ELSE", "ELSIF",
  "END", "ENTRY", "EXCEPTION", "EXIT", "FOR", "FUNCTION", "GENERIC",
  "GOTO", "IF", "IN", "IS", "LIMITED", "LOOP", "MOD", "NEW", "NOT", "NuLL",
  "OF", "OR", "OTHERS", "OUT", "PACKAGE", "PRAGMA", "PRIVATE", "PROCEDURE",
  "RAISE", "RANGE", "RECORD", "REM", "RENAMES", "RETURN", "REVERSE",
  "SELECT", "SEPARATE", "SUBTYPE", "TASK", "TERMINATE", "THEN", "TYPE",
  "USE", "WHEN", "WHILE", "WITH", "XOR", "char_lit", "identifier",
  "char_string", "numeric_lit", "goto_label", "';'", "'('", "')'", "','",
  "':'", "'\\''", "'|'", "'.'", "'='", "'<'", "'>'", "'+'", "'-'", "'&'",
  "'*'", "'/'", "$accept", "goal_symbol", "pragma", "pragma_arg_s",
  "pragma_arg", "pragma_s", "decl", "object_decl", "number_decl",
  "def_id_s", "object_qualifier_opt", "object_subtype_def", "init_opt",
  "type_decl", "discrim_part_opt", "type_completion", "type_def",
  "subtype_decl", "subtype_ind", "constraint", "decimal_digits_constraint",
  "derived_type", "range_constraint", "range_constr_opt", "range",
  "enumeration_type", "enum_id_s", "enum_id", "integer_type", "real_type",
  "float_type", "fixed_type", "array_type", "unconstr_array_type",
  "constr_array_type", "component_subtype_def", "index_s", "index",
  "iter_index_constraint", "iter_discrete_range_s", "discrete_range",
  "record_type", "record_def", "comp_list", "comp_decl_s",
  "variant_part_opt", "comp_decl", "discrim_part", "discrim_spec_s",
  "discrim_spec", "access_opt", "variant_part", "variant_s", "variant",
  "choice_s", "choice", "discrete_with_range", "access_type", "decl_part",
  "decl_item_s", "decl_item_s1", "decl_item", "decl_item_or_body_s1",
  "decl_item_or_body", "body", "name", "mark", "simple_name",
  "simple_name_list", "used_char", "operator_symbol", "indexed_comp",
  "value_s", "value", "selected_comp", "attribute", "attribute_id",
  "literal", "aggregate", "value_s_2", "comp_assoc", "expression",
  "logical", "short_circuit", "relation", "relational", "membership",
  "simple_expression", "unary", "adding", "term", "multiplying", "factor",
  "primary", "parenthesized_primary", "qualified", "allocator",
  "statement_s", "statement", "unlabeled", "simple_stmt", "compound_stmt",
  "null_stmt", "assign_stmt", "if_stmt", "cond_clause_s", "cond_clause",
  "cond_part", "condition", "else_opt", "case_stmt", "case_hdr",
  "alternative_s", "alternative", "loop_stmt", "label_opt", "loop_content",
  "iter_part", "reverse_opt", "basic_loop", "id_opt", "block",
  "block_decl", "$@1", "block_body", "handled_stmt_s",
  "except_handler_part_opt", "exit_stmt", "name_opt", "when_opt",
  "return_stmt", "goto_stmt", "subprog_decl", "subprog_spec", "@2", "@3",
  "designator", "formal_part_opt", "formal_part", "param_s", "param",
  "mode", "subprog_spec_is_push", "subprog_body", "procedure_call",
  "pkg_decl", "pkg_spec", "private_part", "simple_name_opt", "pkg_body",
  "body_opt", "private_type", "limited_opt", "use_clause", "name_s",
  "rename_decl", "rename_unit", "renames", "comp_unit", "context_spec",
  "with_clause", "use_clause_opt", "unit", "subunit", "subunit_body",
  "body_stub", "exception_decl", "except_handler_part",
  "exception_handler", "except_choice_s", "except_choice", "raise_stmt",
  "generic_decl", "generic_formal_part", "generic_formal",
  "generic_discrim_part_opt", "subp_default", "generic_type_def",
  "generic_derived_type", "generic_subp_inst", "generic_pkg_inst",
  "generic_inst", "rep_spec", "attrib_def", "record_type_spec",
  "align_opt", "comp_loc_s", "address_spec", "code_stmt", YY_NULLPTR
  };
  return yy_sname[yysymbol];
}
#endif

#define YYPACT_NINF (-593)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-355)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     796,   239,  -593,    20,     4,     3,     4,   174,  -593,    17,
    1273,  -593,  -593,   120,  -593,  -593,  -593,   551,  -593,  -593,
    -593,  -593,   474,   131,   199,  -593,  -593,  -593,     8,     4,
     249,  -593,     4,  -593,    -2,  -593,   246,   403,  -593,   231,
     257,     4,    35,   279,   284,   289,   403,  -593,  -593,  -593,
    -593,  -593,   420,  -593,  -593,   348,  -593,  1171,  -593,  -593,
    -593,   143,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,
    -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,   316,
     341,  -593,     4,     4,    27,   536,   155,   319,   346,  -593,
    -593,  -593,  -593,   362,   409,  1117,   375,   362,   392,  -593,
       4,   403,  -593,  -593,   716,  -593,  -593,  -593,  -593,  -593,
    -593,  -593,   471,  -593,     4,   405,   411,   467,   442,   716,
     477,   455,   482,  1102,   511,  -593,   180,   316,   341,  -593,
    -593,   394,   472,   239,     4,     4,   288,  -593,   468,  -593,
    -593,    45,   501,  -593,  1215,    51,   512,  1207,  -593,   351,
    -593,  -593,  -593,   321,  -593,   716,   332,   172,   342,   567,
     172,     4,   535,  -593,   729,   403,    96,   548,  -593,  -593,
     403,  -593,   584,   187,   184,   521,   729,   403,   403,   729,
     569,   403,   642,   544,  1102,  -593,    28,   572,   841,  -593,
    -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,   429,
    -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,   239,   576,
    1251,   580,    98,  -593,   605,   362,   638,   362,   630,  -593,
       4,  -593,   485,  -593,   403,   348,     4,  1298,   655,  -593,
     180,   669,   658,  -593,  -593,  -593,  -593,   862,   403,   862,
    -593,  -593,  -593,  -593,   436,  -593,  -593,  -593,    40,  -593,
     156,   494,  -593,   556,  -593,  -593,  -593,  -593,   764,  -593,
     623,   691,    11,  -593,   696,  -593,  -593,  -593,  -593,  -593,
    -593,  -593,  -593,  -593,  -593,  -593,  -593,   729,   689,   730,
     261,   406,  -593,  -593,  1163,   591,  -593,   699,   163,   641,
     285,  -593,   646,   550,   502,  -593,   421,   652,   716,   729,
    -593,   659,   667,   725,   713,  -593,  -593,  -593,  -593,   653,
     716,   707,   620,   163,   654,  -593,  1102,   714,  -593,   705,
    -593,   301,  -593,  -593,   729,  -593,   225,  -593,   719,  -593,
    -593,   719,   341,  -593,   718,  1102,   729,   239,   740,  -593,
     348,   732,  -593,  -593,  -593,   727,   735,   768,   780,   790,
    -593,    47,    45,  -593,   716,  -593,   803,   798,  -593,     4,
    -593,  -593,   730,  -593,  -593,   784,   767,   648,   770,   347,
     729,   679,   729,   304,  -593,  -593,   332,   800,   829,  -593,
     729,   729,   729,  -593,  -593,  -593,  -593,   824,  -593,  -593,
    -593,  -593,  -593,  -593,   729,   729,   691,    11,  -593,  -593,
    -593,  -593,   691,   862,   489,   822,  -593,  -593,   791,   793,
     729,   729,  -593,   729,  -593,  -593,  -593,  -593,   855,    85,
    -593,   365,   729,   729,  -593,   729,   403,   676,  -593,  -593,
    -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,
     514,  -593,   490,  -593,   729,   833,   729,   797,   809,  -593,
     729,   811,  -593,  1102,   729,   861,   806,  -593,  -593,  -593,
     528,  -593,   276,  -593,  -593,    43,  1273,   853,  1039,   852,
     816,  -593,   729,   867,  -593,  -593,   894,   896,   898,   403,
     900,   901,  -593,  -593,  -593,   857,   834,  -593,   403,   403,
     112,   835,  -593,  -593,     4,   860,  -593,  -593,   839,   332,
    -593,   332,  -593,   443,  -593,   163,  -593,  -593,   163,  -593,
     737,    38,   844,  -593,  -593,  -593,  -593,  -593,   575,  -593,
     575,  -593,   310,    11,  -593,  -593,  -593,   729,    23,  -593,
    -593,  -593,   163,   160,  -593,     4,  -593,   403,  -593,   410,
     160,   163,  -593,  -593,  -593,   682,  -593,   858,  -593,  -593,
    -593,  -593,  -593,   717,  -593,   772,  -593,   847,   403,   163,
    -593,  -593,  -593,  -593,  1067,  -593,   897,  -593,  -593,   849,
     716,    50,  -593,   920,   679,  -593,  -593,  -593,   902,  -593,
    -593,   852,   706,   239,   919,  -593,  -593,   877,  -593,   869,
    -593,   595,   660,  -593,   716,  -593,   872,  -593,  -593,  -593,
     895,   774,   729,   542,   899,   217,  -593,  -593,    47,  -593,
     729,  -593,  -593,  -593,   676,  -593,   227,   906,   403,  -593,
     729,   208,  -593,  -593,  -593,   875,   356,  1102,   356,   876,
      53,  -593,  -593,   878,   956,   908,  -593,   883,  -593,   181,
    -593,   887,  -593,   264,  -593,   891,   729,  -593,   160,  -593,
     892,     4,   893,   675,   943,  -593,  -593,  -593,   403,  -593,
     566,  -593,  -593,  -593,    54,   888,  -593,  -593,  1102,  -593,
    -593,  -593,  -593,   904,  -593,  -593,  -593,   561,  -593,  -593,
     935,  -593,   403,   921,   102,  -593,   341,  -593,   973,  1102,
     946,   907,   729,  -593,   341,   725,  -593,  -593,  -593,  1004,
    -593,   910,   234,   911,   341,  -593,   679,    52,  -593,  -593,
      55,   960,  -593,  -593,   912,   227,  -593,  -593
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     0,   362,     0,     0,     0,     0,     0,   340,     0,
       0,   341,   338,     0,   339,   344,     2,     0,   336,     9,
     342,   343,     0,     0,     0,   140,   291,   290,   289,     0,
       0,   285,     0,   141,     0,     1,   303,     0,   283,     0,
       0,     0,     0,     0,     0,     0,     0,    24,   125,   122,
      11,    12,     0,    13,    14,     0,   129,     0,   126,   128,
      15,     0,   130,    16,   131,   123,    18,   324,    20,    17,
      19,   124,   392,   393,   394,   306,   334,   336,     9,   332,
     331,   298,     0,     0,     0,     0,     0,     0,     0,   370,
     363,   284,   307,   292,     0,     0,     0,   292,     0,   335,
       0,     0,   389,   144,   329,   132,   136,   133,   134,   135,
     326,    21,     0,   137,     0,   140,     0,     0,    33,   320,
       0,     0,    26,     0,     0,   127,   303,   333,   330,   337,
      10,     0,   371,     0,     0,     0,   299,   360,     0,   364,
     361,     0,     0,   293,     0,     0,   309,     0,   120,     0,
     390,   325,   286,     0,   142,   391,     0,     0,     0,     0,
       0,     0,     0,     3,     0,     0,     0,    36,    34,   319,
       0,    25,    27,     0,     0,     0,     0,   276,     0,     0,
       0,   276,     0,   140,     0,   226,     0,     0,     0,   220,
     222,   224,   225,   227,   228,   236,   237,     9,   238,   268,
     239,   271,   229,   230,   231,   232,   233,   234,   265,     0,
       0,     0,     0,   372,     0,   292,     0,   292,   300,   301,
       0,   328,     0,   295,     0,   314,     0,     0,     0,   121,
       0,     0,     0,   346,   347,   345,   151,     0,     0,     0,
     163,   111,   143,   161,     0,   195,   196,   113,     0,   107,
     110,   211,   162,     0,   146,   210,   215,   149,   109,   172,
     180,     0,   192,   200,   206,   214,   213,   212,   160,   159,
     158,   157,   156,   155,   152,   153,   154,     0,   397,   211,
       0,   180,   138,   139,     0,     0,     5,   132,     7,     0,
      48,   100,     0,     0,     0,    97,   317,     0,   321,     0,
     350,     0,     0,    30,    28,    29,    71,    72,   235,     0,
     277,   278,     0,   247,   248,   243,     0,     0,   240,     0,
     280,     0,   257,   223,     0,   305,     0,   402,     0,   221,
     272,   274,   252,   269,     0,     0,     0,   265,   262,   258,
       0,     0,   266,   349,   327,     0,   317,     0,     0,   374,
     302,    30,     0,   294,   288,   315,     0,     0,   310,   311,
     303,   208,   218,   219,   207,   163,     0,     0,   149,   109,
       0,     0,     0,     0,   112,   145,     0,   175,   176,   177,
       0,     0,     0,   187,   185,   189,   190,     0,   184,   186,
     188,   197,   198,   199,     0,     0,     0,   193,   204,   205,
     202,   203,     0,     0,     0,     0,   399,   395,     0,     0,
       0,     0,    46,     0,    47,    50,    49,    35,   101,     0,
      96,     0,     0,     0,   318,     0,     0,     0,    37,    44,
      64,    38,    39,    40,    66,    67,    41,    42,    43,    45,
       0,    32,     0,   323,     0,     0,     0,     0,     0,   251,
       0,     0,   282,     0,     0,     0,     0,   246,   359,   281,
       0,   217,     0,   351,   352,     0,     0,     0,     0,     0,
       0,   263,     0,     0,   304,   373,     0,     0,     0,     0,
       0,     0,   383,   384,   385,     0,     0,   386,     0,     0,
       0,     0,   297,   296,   311,     0,   312,   308,     0,     0,
     165,     0,   164,     0,   216,   171,   108,   110,   109,    53,
     211,     0,    57,   147,   178,   179,   173,   174,    56,   191,
     181,   182,   183,   194,   201,   209,   401,     0,     0,   348,
       4,     6,     8,    54,   102,     0,    98,     0,   114,     0,
      54,    65,    52,    63,    62,     0,    60,     0,   316,     9,
      84,    23,    83,     0,    76,     0,    80,   211,     0,    31,
      22,   322,   279,   275,     0,   244,     0,   241,   358,   140,
     357,     0,   355,     0,     0,   253,   270,   261,     0,   259,
     255,     0,   211,   265,   380,   382,   379,   387,   378,     0,
     365,   374,     0,   376,   375,   366,     0,   168,   169,   170,
     163,     0,     0,     0,     0,     0,    55,    51,    30,   115,
       0,    69,    68,    59,     0,    86,     0,     0,     0,    79,
       0,     0,    82,    75,    74,     0,     0,     0,     0,     0,
       0,   264,   260,     0,     0,     0,   377,     0,   369,     0,
     313,     0,   166,     0,   398,     0,     0,    99,    54,    61,
       0,     0,     0,     0,     0,     9,    90,     9,     0,    77,
       0,    81,    78,   242,     0,     0,   356,   250,     0,   267,
     381,   388,   367,     0,   167,    58,   396,     0,    70,    95,
       0,     9,     0,     0,     0,    87,    88,    73,     0,     0,
       0,     0,     0,     9,    89,    30,    85,    91,     9,     0,
     368,     0,     0,     0,    93,   400,     0,     0,   104,    94,
       0,     0,   105,     9,     0,     0,   103,   106
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -593,  -593,   -10,  -593,   577,   -73,  -593,  -593,  -593,   -20,
    -593,  -593,  -340,  -593,  -593,  -593,  -593,  -593,  -151,  -593,
    -593,  -593,  -245,  -506,  -357,  -593,  -593,   379,  -593,  -593,
    -593,  -593,  -235,  -593,  -593,  -592,  -593,   376,  -593,  -593,
    -440,  -593,  -593,   280,  -593,  -593,   312,   865,  -593,   581,
    -593,   315,  -593,   295,  -544,   632,  -367,   661,    -9,   777,
    -593,   -46,  -593,   951,  -593,   -29,  -204,   162,  -593,   851,
     854,  -593,   507,  -232,  -593,  -593,   859,  -593,  -593,  -593,
     769,   372,  -593,  -593,   505,  -593,  -593,  -153,  -593,  -593,
    -240,  -593,   609,  -213,  -267,   -95,  -593,  -306,  -175,  -593,
    -593,  -593,  -593,  -593,  -593,  -593,   560,  -593,  -300,  -593,
    -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -411,
    -318,  -593,  -593,  -593,  -182,  -593,  -593,  -593,   845,  -593,
    -593,  -593,    67,    22,  -593,  -593,    34,   -64,  -593,  -593,
    -121,  -593,  -593,    25,  -593,    75,   993,  -593,   526,    31,
    -593,   683,   684,    30,  -593,  -593,   189,   -12,  -593,  -593,
    1011,   954,  1015,  -593,  -593,  -593,  -593,  -593,   702,   413,
     407,  -593,   196,  -593,  -593,  -593,   445,  -593,  -593,  -593,
    -593,   948,  -593,  -593,  -593,  -593,  -593,  -593,  -593
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     7,   185,   285,   286,    80,    49,    50,    51,    52,
     174,   303,   447,    53,   167,   297,   428,    54,   623,   414,
     415,   429,   606,   622,   247,   431,   545,   546,   432,   433,
     434,   435,   305,   306,   307,   624,   553,   554,   445,   555,
     556,   437,   550,   654,   655,   685,   656,   168,   294,   295,
     535,   657,   707,   708,   248,   249,   250,   438,   225,   146,
     147,    56,    57,    58,    59,   279,   112,   105,    34,   252,
     106,   107,   253,   254,   108,   109,   272,   255,   256,   367,
     257,   258,   380,   381,   259,   394,   395,   281,   261,   396,
     262,   402,   263,   264,   265,   266,   267,   188,   189,   190,
     191,   192,   193,   194,   195,   314,   315,   316,   317,   455,
     196,   197,   465,   575,   198,   199,   337,   338,   472,   339,
     341,   200,   340,   466,   124,   201,   330,   202,   311,   451,
     203,   204,    60,    61,    97,    93,   342,   142,   143,   222,
      87,   220,    10,    62,   205,    63,    13,   228,   497,    64,
     356,   439,   440,    65,   120,    66,    67,    39,    16,    17,
      18,    79,    19,    20,   235,    68,    69,   331,   463,   571,
     572,   206,    70,    22,    90,   214,   491,   486,   487,    23,
      24,   102,    71,    72,    73,   406,   528,    74,   207
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      48,    55,    85,   260,   507,   128,   374,    76,   104,   323,
     456,   492,   366,   329,   289,   509,   351,   119,    96,   470,
     223,   397,     9,   304,   361,    11,   364,   607,   187,   468,
     630,    14,   581,   152,   612,    28,   469,   324,   521,     9,
      29,   382,    11,   355,    86,   416,    81,    48,    14,   148,
     370,   430,    89,   604,   398,   114,   446,    36,   579,   461,
     627,   436,   133,   668,   689,   713,   687,     8,  -287,   399,
     130,   226,   155,   573,   138,    12,    37,    99,   134,    25,
     100,   135,   711,    32,     8,    48,   291,   552,  -287,   187,
     695,   260,    12,   187,   186,    25,    38,   291,    25,   291,
     292,   229,   345,   650,   400,   401,   461,   325,   156,   129,
      25,   482,   326,   574,   158,   552,   593,   149,   130,   211,
      47,    85,   706,   651,   332,   371,    25,   251,   391,   392,
     393,   160,   -92,   161,    48,   628,   290,    48,   371,   628,
     371,   298,   678,   363,   513,   290,   293,   564,   310,   312,
     562,   347,   310,   349,    43,   186,   523,   129,   473,   186,
      47,   301,   710,    27,   148,    30,    31,   215,    33,   149,
     632,    47,   -92,    47,    35,   232,   377,    47,   233,   377,
     661,   148,   236,   126,   234,   673,   268,    25,   103,    15,
     525,    94,   293,   237,    98,   354,    21,   269,   270,    75,
      48,   302,    37,   113,    30,   116,    15,   507,   378,   362,
      91,   378,   662,    21,    37,   251,   372,    48,   260,   511,
     237,   187,    38,   260,   101,   238,   239,   240,   650,   518,
     241,   493,   149,   379,   137,   646,   379,  -150,  -150,   268,
     187,   520,   511,   209,   131,   132,    37,   271,   651,   149,
     269,   270,   238,   239,   240,   242,    25,   103,   243,    25,
     103,   244,   154,   552,   509,   633,   300,   598,   647,   599,
     538,   245,   246,   652,    48,   542,   162,   377,    92,    43,
     377,   329,   242,    25,   103,   243,    43,   186,   244,    95,
     101,   511,   448,   329,   611,    27,   216,   217,   245,   246,
     271,   160,    47,   161,   706,   244,   186,    30,    37,   378,
     110,   413,   378,  -211,    25,    26,  -211,   377,   268,   511,
     274,   665,   130,   283,   605,   568,   287,   218,   587,   269,
     270,   608,    85,   236,   379,   701,   111,   379,   219,   507,
     407,   372,   251,   510,   237,   675,   260,   251,   260,   378,
     260,   569,   103,  -211,   115,   703,     1,   273,   187,   117,
     512,   187,   690,   377,   118,   156,   522,   123,  -211,   157,
      27,   158,   231,   187,   379,     4,   238,   239,   240,   271,
     459,   241,   113,   699,   244,    46,   609,   537,   357,   329,
     156,   230,   290,    43,   373,   378,   158,   290,   139,   293,
    -211,  -211,  -211,  -211,  -211,   568,   242,    25,   103,   243,
      37,   383,   244,   384,   385,   557,   242,    25,   103,   503,
     379,   260,   245,   246,   186,   140,   377,   186,   504,  -148,
      38,    25,   103,   570,   210,   421,   610,   236,   302,   186,
      25,   103,   141,   582,   236,   386,   422,   423,   237,   144,
     290,   387,   333,    37,   151,   237,    48,   576,   378,   591,
     592,   594,   424,   334,   425,   426,   372,   511,   511,   187,
     251,   335,   251,   153,   251,    81,   616,   372,    25,   103,
     238,   239,   365,   379,   163,   241,   260,   238,   239,   600,
     329,   164,   241,   388,   389,   390,   391,   392,   393,    27,
     336,   427,   121,   122,   172,   377,   377,   165,   290,     1,
     242,    25,   103,   243,   173,   329,   244,   242,    25,   103,
     243,   496,   166,   244,   329,    82,   245,   246,     4,   290,
     171,   280,   187,   245,   246,   186,   288,   378,   378,   511,
     159,   208,    83,    46,   377,   251,    84,   221,   309,    47,
     372,   313,   212,   260,   321,   160,   169,   161,   377,   170,
     547,   224,   379,   379,   352,   227,   353,   548,   526,   551,
     187,   549,   287,   187,   156,   284,   378,   377,   373,   237,
     158,   419,   684,   420,   686,   277,     1,     2,   296,   660,
     378,   582,   510,   299,   187,   187,   653,   570,   186,   570,
     308,   379,     3,    43,   187,     4,   130,   567,   694,   378,
     251,   238,   239,   240,     5,   379,   369,   692,   121,   136,
     702,   644,   688,     6,   278,   704,   382,   322,   383,   290,
     384,   385,   121,   418,   379,   490,   186,   375,   376,   186,
     715,   242,    25,   103,   243,   346,   156,   244,   318,   404,
     157,   327,   158,   290,   237,   343,   496,   245,   246,   344,
     186,   186,   386,   510,   653,   391,   392,   393,   387,   377,
     186,   442,   409,   410,   130,   156,   130,   251,   348,   157,
     350,   158,   453,   454,   130,   359,   238,   239,   240,    29,
     113,   237,   130,   449,   130,   653,   460,   113,   360,   452,
     156,   378,   403,   237,   157,   130,   158,   405,   313,   411,
     388,   389,   390,   391,   392,   393,   242,    25,   103,   243,
     412,   320,   244,   238,   239,   240,   379,   417,   241,   500,
     501,   441,   245,   246,   446,   238,   239,   240,   443,   638,
     639,   237,   505,   508,   157,    27,   158,   444,   -54,   421,
     543,   544,   302,   242,    25,   103,   243,   121,   682,   244,
     476,   477,   372,   613,   614,   242,    25,   103,   243,   245,
     246,   244,    37,   238,   239,   240,   424,   450,   478,   479,
     377,   457,   288,   532,   458,   533,   156,   -54,   -54,   462,
     373,   480,   158,   467,   539,   540,   156,   541,   617,   618,
     157,   471,   158,   242,    25,   103,   243,   175,   475,   244,
     156,   474,   378,   680,   326,   481,   158,   156,   559,   245,
     246,   373,   313,   158,   489,  -256,   313,   176,   488,  -256,
     490,     1,     2,   494,  -245,  -245,  -245,   379,   495,   177,
    -256,   498,   175,   178,   179,  -148,  -148,     3,  -256,   499,
       4,   502,   180,   619,   620,   642,   376,   515,    43,     5,
    -256,   181,   176,   519,  -256,   527,   182,   514,     6,   534,
     529,  -273,   530,   328,   177,  -256,   560,  -256,   178,   179,
     558,   183,   103,  -256,   184,   516,   517,   180,   561,   175,
     563,   566,   577,    43,   335,   580,   181,   583,   584,   603,
     585,   182,   586,   621,   588,   589,   238,  -256,   240,   176,
     548,  -256,  -256,   590,   595,   615,   183,   103,  -353,   184,
     597,   177,  -256,   408,   602,   178,   179,   156,   -54,   -54,
    -256,   373,   626,   158,   180,   625,   242,    25,   103,   243,
      43,   629,   244,   181,   631,   634,   508,   175,   182,   635,
     636,   640,   641,   658,   663,   667,   645,   669,  -353,  -256,
     670,   671,   672,   183,   103,  -256,   184,   176,   674,  -256,
     676,   679,   681,   683,   643,   693,  -254,   662,   696,   177,
    -256,   714,   648,   178,   179,   691,   700,   531,  -256,   705,
     709,   716,   180,   649,   659,   717,   697,   213,    43,   698,
     536,   181,   712,   506,   358,   175,   182,   483,   125,   275,
     601,   524,   276,   368,   565,    88,  -254,  -256,   677,   282,
     596,   183,   103,  -256,   184,   176,   319,  -256,    77,   484,
     485,   127,    78,   464,  -354,   666,   637,   177,  -256,   664,
     175,   178,   179,   150,     0,     0,  -256,     0,     0,     0,
     180,     0,     0,     0,     0,     0,    43,     0,  -256,   181,
     176,     0,  -256,     0,   182,     0,     0,     0,   175,   578,
       0,     0,   177,  -256,  -354,  -256,   178,   179,   508,   183,
     103,  -256,   184,     0,     0,   180,  -256,     0,   176,     0,
    -256,    43,     0,     0,   181,     0,     0,  -249,     0,   182,
     177,  -256,     0,   175,   178,   179,     0,     0,     0,  -256,
    -256,     0,     0,   180,   183,   103,     0,   184,    40,    43,
       0,  -256,   181,   176,     0,  -256,     0,   182,     0,     0,
       0,     0,     0,     0,     0,   177,  -256,     0,  -256,   178,
     179,     0,   183,   103,  -256,   184,     0,  -118,   180,     0,
       0,    41,     1,     2,    43,     0,     0,   181,     0,     0,
       0,   101,   182,     0,    40,     0,     0,     0,   145,    43,
    -118,     4,    40,  -256,     0,     0,     0,   183,   103,     0,
     184,    44,  -116,     0,     0,    45,    46,     0,     0,     0,
    -117,     0,    47,  -116,     0,     0,     0,    41,     1,     2,
       0,  -117,     0,     0,     0,    41,     1,     2,    40,     0,
       0,     0,     0,     0,    42,    43,    40,     4,     0,     0,
       0,     0,    42,    43,     0,     4,   408,    44,     0,     0,
       0,    45,    46,     0,  -116,    44,     0,  -119,    47,    45,
      46,    41,     1,     2,     0,  -116,    47,     0,     0,    41,
       1,     2,    40,     0,     0,     0,     0,     0,   145,    43,
    -119,     4,     0,     0,     0,     0,    42,    43,     0,     4,
       0,    44,     0,     0,    40,    45,    46,     0,     0,    44,
       0,  -118,    47,    45,    46,    41,     1,     2,     0,     0,
      47,     0,  -116,     0,     0,     0,     0,     0,     0,    40,
       0,     0,   145,    43,  -118,     4,     0,    41,     1,     2,
       0,     0,     0,     0,     0,    44,     0,     0,     0,    45,
      46,     0,     0,     0,    42,    43,    47,     4,  -118,     0,
       0,     0,    41,     1,     2,     0,     0,    44,     0,     0,
       0,    45,    46,     0,     0,     0,     0,     0,    47,   145,
      43,     0,     4,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    44,     0,     0,     0,    45,    46,     0,     0,
       0,     0,     0,    47
};

static const yytype_int16 yycheck[] =
{
      10,    10,    22,   156,   371,    78,   251,    17,    37,   184,
     316,   351,   244,   188,   165,   372,   220,    46,    30,   337,
     141,   261,     0,   174,   237,     0,   239,   533,   123,   335,
     574,     0,   472,    97,   540,     1,   336,     9,   395,    17,
      20,     3,    17,   225,    22,   290,     1,    57,    17,    95,
      10,   296,    22,    30,    43,    20,     9,    40,   469,   326,
      10,   296,    35,    10,    10,    10,   658,     0,    60,    58,
      80,    20,   101,    30,    86,     0,    59,    79,    51,    75,
      82,    54,    30,    80,    17,    95,     1,   444,    80,   184,
     682,   244,    17,   188,   123,    75,    79,     1,    75,     1,
       4,   147,     4,     1,    93,    94,   373,    79,    80,    79,
      75,   346,    84,    70,    86,   472,     4,    95,   128,   131,
      75,   141,    70,    21,   197,    85,    75,   156,    90,    91,
      92,    84,    30,    86,   144,    85,   165,   147,    85,    85,
      85,   170,   648,   238,   376,   174,   166,   453,   177,   178,
     450,   215,   181,   217,    52,   184,   396,   127,   340,   188,
      75,   173,   706,     1,   210,     3,     4,   133,     6,   147,
     581,    75,    70,    75,     0,   153,    16,    75,   153,    16,
     620,   227,     1,    40,   153,     4,    14,    75,    76,     0,
     403,    29,   212,    12,    32,   224,     0,    25,    26,    79,
     210,    17,    59,    41,    42,    43,    17,   574,    48,   238,
      79,    48,     4,    17,    59,   244,    56,   227,   371,   372,
      12,   316,    79,   376,    44,    44,    45,    46,     1,   382,
      49,   352,   210,    73,    79,    18,    73,    81,    82,    14,
     335,   394,   395,    63,    82,    83,    59,    75,    21,   227,
      25,    26,    44,    45,    46,    74,    75,    76,    77,    75,
      76,    80,   100,   620,   621,   583,    79,   499,   608,   501,
     421,    90,    91,    46,   284,   426,   114,    16,    79,    52,
      16,   456,    74,    75,    76,    77,    52,   316,    80,    40,
      44,   444,   304,   468,   539,   133,   134,   135,    90,    91,
      75,    84,    75,    86,    70,    80,   335,   145,    59,    48,
      79,    26,    48,     3,    75,    76,     6,    16,    14,   472,
     158,   627,   332,   161,   528,    49,   164,    39,   479,    25,
      26,   535,   352,     1,    73,   692,    79,    73,    50,   706,
      79,    56,   371,   372,    12,    81,   499,   376,   501,    48,
     503,    75,    76,    43,    75,   695,    35,    15,   453,    75,
      56,   456,   668,    16,    75,    80,   395,    19,    58,    84,
     208,    86,    51,   468,    73,    54,    44,    45,    46,    75,
      79,    49,   220,   689,    80,    69,   537,    22,   226,   564,
      80,    40,   421,    52,    84,    48,    86,   426,    79,   419,
      90,    91,    92,    93,    94,    49,    74,    75,    76,    77,
      59,     5,    80,     7,     8,   444,    74,    75,    76,    72,
      73,   574,    90,    91,   453,    79,    16,   456,    81,    82,
      79,    75,    76,   462,    40,    14,    26,     1,    17,   468,
      75,    76,    80,   472,     1,    39,    25,    26,    12,    40,
     479,    45,    23,    59,    79,    12,   466,   466,    48,   488,
     489,   490,    41,    34,    43,    44,    56,   620,   621,   564,
     499,    42,   501,    81,   503,     1,   549,    56,    75,    76,
      44,    45,    46,    73,    79,    49,   639,    44,    45,    46,
     665,    80,    49,    87,    88,    89,    90,    91,    92,   337,
      71,    80,    82,    83,    22,    16,    16,    40,   537,    35,
      74,    75,    76,    77,    32,   690,    80,    74,    75,    76,
      77,   359,    80,    80,   699,    51,    90,    91,    54,   558,
      75,   159,   627,    90,    91,   564,   164,    48,    48,   692,
      69,    30,    68,    69,    16,   574,    72,    79,   176,    75,
      56,   179,    80,   706,   182,    84,    79,    86,    16,    82,
      46,    60,    73,    73,    79,    53,    81,    53,    79,    79,
     665,    57,   410,   668,    80,    40,    48,    16,    84,    12,
      86,    79,   655,    81,   657,    18,    35,    36,    40,   618,
      48,   620,   621,     9,   689,   690,   616,   626,   627,   628,
      79,    73,    51,    52,   699,    54,   616,    79,   681,    48,
     639,    44,    45,    46,    63,    73,   244,    56,    82,    83,
     693,    79,    56,    72,    57,   698,     3,    83,     5,   658,
       7,     8,    82,    83,    73,    40,   665,    81,    82,   668,
     713,    74,    75,    76,    77,    40,    80,    80,    79,   277,
      84,    79,    86,   682,    12,    79,   494,    90,    91,    79,
     689,   690,    39,   692,   684,    90,    91,    92,    45,    16,
     699,   299,    81,    82,   684,    80,   686,   706,    40,    84,
      50,    86,    28,    29,   694,    30,    44,    45,    46,    20,
     528,    12,   702,    40,   704,   715,   324,   535,    40,    79,
      80,    48,     6,    12,    84,   715,    86,    18,   336,    10,
      87,    88,    89,    90,    91,    92,    74,    75,    76,    77,
      79,    79,    80,    44,    45,    46,    73,    81,    49,    81,
      82,    79,    90,    91,     9,    44,    45,    46,    79,    79,
      80,    12,   370,   371,    84,   583,    86,    80,    42,    14,
      74,    75,    17,    74,    75,    76,    77,    82,    83,    80,
      25,    26,    56,    81,    82,    74,    75,    76,    77,    90,
      91,    80,    59,    44,    45,    46,    41,    70,    43,    44,
      16,    67,   410,   411,    79,   413,    80,    81,    82,    70,
      84,    56,    86,    75,   422,   423,    80,   425,    81,    82,
      84,    61,    86,    74,    75,    76,    77,     1,    81,    80,
      80,    79,    48,   651,    84,    80,    86,    80,   446,    90,
      91,    84,   450,    86,    44,    19,   454,    21,    60,    23,
      40,    35,    36,    30,    28,    29,    30,    73,    40,    33,
      34,    57,     1,    37,    38,    81,    82,    51,    42,    82,
      54,    81,    46,    81,    82,    81,    82,    28,    52,    63,
      19,    55,    21,    39,    23,    43,    60,    67,    72,    14,
      79,    30,    79,    32,    33,    34,    79,    71,    37,    38,
      47,    75,    76,    42,    78,   380,   381,    46,    79,     1,
      79,    30,    39,    52,    42,    79,    55,    30,     4,   527,
       4,    60,     4,    56,     4,     4,    44,    19,    46,    21,
      53,    23,    71,    79,    79,    57,    75,    76,    30,    78,
      81,    33,    34,    63,    80,    37,    38,    80,    81,    82,
      42,    84,    83,    86,    46,    38,    74,    75,    76,    77,
      52,    21,    80,    55,    42,    26,   574,     1,    60,    72,
      81,    79,    57,    47,    79,    79,    57,    79,    70,    71,
       4,    53,    79,    75,    76,    19,    78,    21,    81,    23,
      79,    79,    79,    30,   602,    40,    30,     4,    57,    33,
      34,    21,   610,    37,    38,    81,    79,   410,    42,    79,
      79,    79,    46,   614,   618,   715,   684,   132,    52,   684,
     419,    55,   707,   371,   227,     1,    60,   346,    57,   158,
     503,   402,   158,   244,   454,    22,    70,    71,   646,   160,
     494,    75,    76,    19,    78,    21,   181,    23,    17,   346,
     346,    77,    17,   331,    30,   628,   591,    33,    34,   626,
       1,    37,    38,    95,    -1,    -1,    42,    -1,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    52,    -1,    19,    55,
      21,    -1,    23,    -1,    60,    -1,    -1,    -1,     1,    30,
      -1,    -1,    33,    34,    70,    71,    37,    38,   706,    75,
      76,    42,    78,    -1,    -1,    46,    19,    -1,    21,    -1,
      23,    52,    -1,    -1,    55,    -1,    -1,    30,    -1,    60,
      33,    34,    -1,     1,    37,    38,    -1,    -1,    -1,    42,
      71,    -1,    -1,    46,    75,    76,    -1,    78,     1,    52,
      -1,    19,    55,    21,    -1,    23,    -1,    60,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    33,    34,    -1,    71,    37,
      38,    -1,    75,    76,    42,    78,    -1,    30,    46,    -1,
      -1,    34,    35,    36,    52,    -1,    -1,    55,    -1,    -1,
      -1,    44,    60,    -1,     1,    -1,    -1,    -1,    51,    52,
      53,    54,     1,    71,    -1,    -1,    -1,    75,    76,    -1,
      78,    64,    19,    -1,    -1,    68,    69,    -1,    -1,    -1,
      19,    -1,    75,    30,    -1,    -1,    -1,    34,    35,    36,
      -1,    30,    -1,    -1,    -1,    34,    35,    36,     1,    -1,
      -1,    -1,    -1,    -1,    51,    52,     1,    54,    -1,    -1,
      -1,    -1,    51,    52,    -1,    54,    63,    64,    -1,    -1,
      -1,    68,    69,    -1,    19,    64,    -1,    30,    75,    68,
      69,    34,    35,    36,    -1,    30,    75,    -1,    -1,    34,
      35,    36,     1,    -1,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,    -1,    -1,    -1,    -1,    51,    52,    -1,    54,
      -1,    64,    -1,    -1,     1,    68,    69,    -1,    -1,    64,
      -1,    30,    75,    68,    69,    34,    35,    36,    -1,    -1,
      75,    -1,    19,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    51,    52,    53,    54,    -1,    34,    35,    36,
      -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    68,
      69,    -1,    -1,    -1,    51,    52,    75,    54,    30,    -1,
      -1,    -1,    34,    35,    36,    -1,    -1,    64,    -1,    -1,
      -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    75,    51,
      52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    64,    -1,    -1,    -1,    68,    69,    -1,    -1,
      -1,    -1,    -1,    75
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    35,    36,    51,    54,    63,    72,    96,   227,   228,
     237,   238,   240,   241,   244,   251,   253,   254,   255,   257,
     258,   267,   268,   274,   275,    75,    76,   162,   231,    20,
     162,   162,    80,   162,   163,     0,    40,    59,    79,   252,
       1,    34,    51,    52,    64,    68,    69,    75,    97,   101,
     102,   103,   104,   108,   112,   153,   156,   157,   158,   159,
     227,   228,   238,   240,   244,   248,   250,   251,   260,   261,
     267,   277,   278,   279,   282,    79,    97,   255,   257,   256,
     100,     1,    51,    68,    72,   104,   228,   235,   241,   248,
     269,    79,    79,   230,   162,    40,   252,   229,   162,    79,
      82,    44,   276,    76,   160,   162,   165,   166,   169,   170,
      79,    79,   161,   162,    20,    75,   162,    75,    75,   160,
     249,    82,    83,    19,   219,   158,    40,   256,   100,   248,
      97,   162,   162,    35,    51,    54,    83,    79,   252,    79,
      79,    80,   232,   233,    40,    51,   154,   155,   156,   228,
     276,    79,   232,    81,   162,   160,    80,    84,    86,    69,
      84,    86,   162,    79,    80,    40,    80,   109,   142,    79,
      82,    75,    22,    32,   105,     1,    21,    33,    37,    38,
      46,    55,    60,    75,    78,    97,   160,   190,   192,   193,
     194,   195,   196,   197,   198,   199,   205,   206,   209,   210,
     216,   220,   222,   225,   226,   239,   266,   283,    30,    63,
      40,   252,    80,   142,   270,   231,   162,   162,    39,    50,
     236,    79,   234,   235,    60,   153,    20,    53,   242,   156,
      40,    51,   228,   238,   244,   259,     1,    12,    44,    45,
      46,    49,    74,    77,    80,    90,    91,   119,   149,   150,
     151,   160,   164,   167,   168,   172,   173,   175,   176,   179,
     182,   183,   185,   187,   188,   189,   190,   191,    14,    25,
      26,    75,   171,    15,   162,   164,   165,    18,    57,   160,
     176,   182,   171,   162,    40,    98,    99,   162,   176,   113,
     160,     1,     4,   104,   143,   144,    40,   110,   160,     9,
      79,   252,    17,   106,   113,   127,   128,   129,    79,   176,
     160,   223,   160,   176,   200,   201,   202,   203,    79,   223,
      79,   176,    83,   193,     9,    79,    84,    79,    32,   193,
     221,   262,   100,    23,    34,    42,    71,   211,   212,   214,
     217,   215,   231,    79,    79,     4,    40,   232,    40,   232,
      50,   161,    79,    81,   160,   219,   245,   162,   154,    30,
      40,   188,   160,   190,   188,    46,   168,   174,   175,   176,
      10,    85,    56,    84,   117,    81,    82,    16,    48,    73,
     177,   178,     3,     5,     7,     8,    39,    45,    87,    88,
      89,    90,    91,    92,   180,   181,   184,   185,    43,    58,
      93,    94,   186,     6,   176,    18,   280,    79,    63,    81,
      82,    10,    79,    26,   114,   115,   117,    81,    83,    79,
      81,    14,    25,    26,    41,    43,    44,    80,   111,   116,
     117,   120,   123,   124,   125,   126,   127,   136,   152,   246,
     247,    79,   176,    79,    80,   133,     9,   107,   252,    40,
      70,   224,    79,    28,    29,   204,   192,    67,    79,    79,
     176,   189,    70,   263,   263,   207,   218,    75,   192,   203,
     215,    61,   213,   219,    79,    81,    25,    26,    43,    44,
      56,    80,   127,   152,   246,   247,   272,   273,    60,    44,
      40,   271,   107,   235,    30,    40,   162,   243,    57,    82,
      81,    82,    81,    72,    81,   176,   150,   151,   176,   119,
     160,   182,    56,   168,    67,    28,   179,   179,   182,    39,
     182,   119,   160,   185,   187,   188,    79,    43,   281,    79,
      79,    99,   176,   176,    14,   145,   144,    22,   113,   176,
     176,   176,   113,    74,    75,   121,   122,    46,    53,    57,
     137,    79,   119,   131,   132,   134,   135,   160,    47,   176,
      79,    79,   203,    79,   192,   201,    30,    79,    49,    75,
     160,   264,   265,    30,    70,   208,   153,    39,    30,   214,
      79,   135,   160,    30,     4,     4,     4,   113,     4,     4,
      79,   160,   160,     4,   160,    79,   243,    81,   168,   168,
      46,   167,    80,   176,    30,   161,   117,   118,   161,   113,
      26,   117,   118,    81,    82,    57,   100,    81,    82,    81,
      82,    56,   118,   113,   130,    38,    83,    10,    85,    21,
     149,    42,   214,   215,    26,    72,    81,   271,    79,    80,
      79,    57,    81,   176,    79,    57,    18,   107,   176,   122,
       1,    21,    46,   104,   138,   139,   141,   146,    47,   132,
     160,   135,     4,    79,   264,   192,   265,    79,    10,    79,
       4,    53,    79,     4,    81,    81,    79,   176,   118,    79,
     162,    79,    83,    30,   100,   140,   100,   130,    56,    10,
     192,    81,    56,    40,   100,   130,    57,   141,   146,   192,
      79,   119,   100,   107,   100,    79,    70,   147,   148,    79,
     149,    30,   148,    10,    21,   100,    79,   138
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,    95,    96,    97,    97,    98,    98,    99,    99,   100,
     100,   101,   101,   101,   101,   101,   101,   101,   101,   101,
     101,   101,   102,   103,   104,   104,   105,   105,   106,   106,
     107,   107,   108,   109,   109,   109,   110,   110,   111,   111,
     111,   111,   111,   111,   111,   111,   112,   113,   113,   114,
     114,   115,   116,   117,   118,   118,   119,   119,   119,   120,
     121,   121,   122,   122,   123,   123,   124,   124,   125,   126,
     126,   127,   127,   128,   129,   130,   131,   131,   132,   133,
     134,   134,   135,   135,   136,   137,   137,   138,   138,   138,
     139,   139,   140,   140,   141,   141,   142,   143,   143,   144,
     144,   145,   145,   146,   147,   147,   148,   149,   149,   150,
     150,   150,   151,   151,   152,   152,   153,   153,   154,   154,
     155,   155,   156,   156,   156,   156,   157,   157,   158,   158,
     159,   159,   160,   160,   160,   160,   160,   161,   161,   161,
     162,   163,   163,   164,   165,   166,   167,   167,   168,   168,
     168,   168,   169,   169,   169,   169,   170,   171,   171,   171,
     171,   172,   172,   172,   173,   173,   173,   173,   173,   174,
     174,   175,   176,   176,   176,   177,   177,   177,   178,   178,
     179,   179,   179,   179,   180,   180,   180,   180,   180,   180,
     181,   181,   182,   182,   182,   183,   183,   184,   184,   184,
     185,   185,   186,   186,   186,   186,   187,   187,   187,   187,
     188,   188,   188,   188,   188,   189,   189,   190,   191,   191,
     192,   192,   193,   193,   194,   194,   194,   195,   195,   195,
     195,   195,   195,   195,   195,   195,   196,   196,   196,   196,
     197,   198,   199,   200,   200,   201,   202,   203,   204,   204,
     205,   206,   207,   207,   208,   209,   210,   210,   211,   211,
     211,   212,   213,   213,   214,   215,   215,   216,   217,   218,
     217,   219,   220,   221,   221,   222,   223,   223,   224,   224,
     225,   225,   226,   227,   227,   229,   228,   230,   228,   228,
     231,   231,   232,   232,   233,   234,   234,   235,   235,   236,
     236,   236,   236,   237,   238,   239,   240,   240,   241,   242,
     242,   243,   243,   244,   245,   245,   246,   247,   247,   248,
     249,   249,   250,   250,   250,   251,   251,   251,   251,   252,
     253,   253,   254,   254,   254,   255,   256,   256,   257,   257,
     257,   257,   257,   257,   257,   258,   259,   259,   260,   260,
     261,   262,   262,   263,   263,   264,   264,   265,   265,   266,
     267,   267,   268,   268,   269,   269,   269,   269,   269,   269,
     269,   270,   270,   270,   271,   271,   271,   272,   272,   272,
     272,   272,   272,   272,   272,   272,   272,   273,   273,   274,
     275,   276,   277,   277,   277,   278,   279,   280,   280,   281,
     281,   282,   283
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     3,     6,     1,     3,     1,     3,     0,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     6,     6,     1,     3,     0,     1,     1,     1,
       0,     2,     5,     0,     1,     3,     0,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     5,     2,     1,     1,
       1,     3,     2,     2,     0,     1,     3,     3,     6,     3,
       1,     3,     1,     1,     1,     2,     1,     1,     3,     3,
       5,     1,     1,     6,     4,     1,     1,     3,     3,     3,
       1,     3,     2,     1,     2,     5,     2,     2,     2,     3,
       1,     3,     1,     3,     5,     2,     3,     1,     3,     5,
       1,     0,     1,     8,     1,     2,     5,     1,     3,     1,
       1,     1,     2,     1,     2,     3,     0,     1,     0,     1,
       1,     2,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     3,
       1,     1,     3,     1,     1,     4,     1,     3,     1,     1,
       1,     1,     3,     3,     3,     3,     3,     1,     1,     1,
       1,     1,     1,     1,     3,     3,     5,     6,     4,     3,
       3,     3,     1,     3,     3,     1,     1,     1,     2,     2,
       1,     3,     3,     3,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     2,     3,     1,     1,     1,     1,     1,
       1,     3,     1,     1,     1,     1,     1,     2,     2,     3,
       1,     1,     1,     1,     1,     1,     3,     3,     2,     2,
       1,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     1,     1,
       2,     4,     6,     1,     3,     2,     2,     1,     0,     2,
       6,     3,     0,     2,     4,     4,     0,     2,     1,     3,
       4,     3,     0,     1,     4,     0,     1,     6,     0,     0,
       3,     2,     2,     0,     1,     4,     0,     1,     0,     2,
       2,     3,     3,     2,     2,     0,     4,     0,     6,     2,
       1,     1,     0,     1,     3,     1,     3,     5,     1,     0,
       1,     1,     2,     2,     6,     2,     2,     2,     7,     0,
       2,     0,     1,     9,     0,     1,     2,     0,     1,     3,
       1,     3,     6,     5,     1,     4,     3,     5,     4,     2,
       3,     2,     2,     3,     2,     3,     0,     2,     1,     1,
       1,     1,     1,     1,     1,     5,     1,     1,     6,     4,
       4,     2,     2,     4,     6,     1,     3,     1,     1,     3,
       3,     3,     1,     2,     2,     6,     6,     8,    10,     7,
       1,     0,     1,     3,     0,     2,     2,     3,     2,     2,
       2,     4,     2,     1,     1,     1,     1,     2,     4,     3,
       4,     2,     1,     1,     1,     5,     9,     0,     4,     0,
       7,     6,     2
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        YY_LAC_DISCARD ("YYBACKUP");                              \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (&yylloc, scanner, context, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location, scanner, context); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, void* scanner, ParseContext* context)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  YY_USE (scanner);
  YY_USE (context);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, void* scanner, ParseContext* context)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp, scanner, context);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule, void* scanner, ParseContext* context)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]), scanner, context);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, scanner, context); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


/* Given a state stack such that *YYBOTTOM is its bottom, such that
   *YYTOP is either its top or is YYTOP_EMPTY to indicate an empty
   stack, and such that *YYCAPACITY is the maximum number of elements it
   can hold without a reallocation, make sure there is enough room to
   store YYADD more elements.  If not, allocate a new stack using
   YYSTACK_ALLOC, copy the existing elements, and adjust *YYBOTTOM,
   *YYTOP, and *YYCAPACITY to reflect the new capacity and memory
   location.  If *YYBOTTOM != YYBOTTOM_NO_FREE, then free the old stack
   using YYSTACK_FREE.  Return 0 if successful or if no reallocation is
   required.  Return YYENOMEM if memory is exhausted.  */
static int
yy_lac_stack_realloc (YYPTRDIFF_T *yycapacity, YYPTRDIFF_T yyadd,
#if YYDEBUG
                      char const *yydebug_prefix,
                      char const *yydebug_suffix,
#endif
                      yy_state_t **yybottom,
                      yy_state_t *yybottom_no_free,
                      yy_state_t **yytop, yy_state_t *yytop_empty)
{
  YYPTRDIFF_T yysize_old =
    *yytop == yytop_empty ? 0 : *yytop - *yybottom + 1;
  YYPTRDIFF_T yysize_new = yysize_old + yyadd;
  if (*yycapacity < yysize_new)
    {
      YYPTRDIFF_T yyalloc = 2 * yysize_new;
      yy_state_t *yybottom_new;
      /* Use YYMAXDEPTH for maximum stack size given that the stack
         should never need to grow larger than the main state stack
         needs to grow without LAC.  */
      if (YYMAXDEPTH < yysize_new)
        {
          YYDPRINTF ((stderr, "%smax size exceeded%s", yydebug_prefix,
                      yydebug_suffix));
          return YYENOMEM;
        }
      if (YYMAXDEPTH < yyalloc)
        yyalloc = YYMAXDEPTH;
      yybottom_new =
        YY_CAST (yy_state_t *,
                 YYSTACK_ALLOC (YY_CAST (YYSIZE_T,
                                         yyalloc * YYSIZEOF (*yybottom_new))));
      if (!yybottom_new)
        {
          YYDPRINTF ((stderr, "%srealloc failed%s", yydebug_prefix,
                      yydebug_suffix));
          return YYENOMEM;
        }
      if (*yytop != yytop_empty)
        {
          YYCOPY (yybottom_new, *yybottom, yysize_old);
          *yytop = yybottom_new + (yysize_old - 1);
        }
      if (*yybottom != yybottom_no_free)
        YYSTACK_FREE (*yybottom);
      *yybottom = yybottom_new;
      *yycapacity = yyalloc;
    }
  return 0;
}

/* Establish the initial context for the current lookahead if no initial
   context is currently established.

   We define a context as a snapshot of the parser stacks.  We define
   the initial context for a lookahead as the context in which the
   parser initially examines that lookahead in order to select a
   syntactic action.  Thus, if the lookahead eventually proves
   syntactically unacceptable (possibly in a later context reached via a
   series of reductions), the initial context can be used to determine
   the exact set of tokens that would be syntactically acceptable in the
   lookahead's place.  Moreover, it is the context after which any
   further semantic actions would be erroneous because they would be
   determined by a syntactically unacceptable token.

   YY_LAC_ESTABLISH should be invoked when a reduction is about to be
   performed in an inconsistent state (which, for the purposes of LAC,
   includes consistent states that don't know they're consistent because
   their default reductions have been disabled).  Iff there is a
   lookahead token, it should also be invoked before reporting a syntax
   error.  This latter case is for the sake of the debugging output.

   For parse.lac=full, the implementation of YY_LAC_ESTABLISH is as
   follows.  If no initial context is currently established for the
   current lookahead, then check if that lookahead can eventually be
   shifted if syntactic actions continue from the current context.
   Report a syntax error if it cannot.  */
#define YY_LAC_ESTABLISH                                                \
do {                                                                    \
  if (!yy_lac_established)                                              \
    {                                                                   \
      YYDPRINTF ((stderr,                                               \
                  "LAC: initial context established for %s\n",          \
                  yysymbol_name (yytoken)));                            \
      yy_lac_established = 1;                                           \
      switch (yy_lac (yyesa, &yyes, &yyes_capacity, yyssp, yytoken))    \
        {                                                               \
        case YYENOMEM:                                                  \
          YYNOMEM;                                                      \
        case 1:                                                         \
          goto yyerrlab;                                                \
        }                                                               \
    }                                                                   \
} while (0)

/* Discard any previous initial lookahead context because of Event,
   which may be a lookahead change or an invalidation of the currently
   established initial context for the current lookahead.

   The most common example of a lookahead change is a shift.  An example
   of both cases is syntax error recovery.  That is, a syntax error
   occurs when the lookahead is syntactically erroneous for the
   currently established initial context, so error recovery manipulates
   the parser stacks to try to find a new initial context in which the
   current lookahead is syntactically acceptable.  If it fails to find
   such a context, it discards the lookahead.  */
#if YYDEBUG
# define YY_LAC_DISCARD(Event)                                           \
do {                                                                     \
  if (yy_lac_established)                                                \
    {                                                                    \
      YYDPRINTF ((stderr, "LAC: initial context discarded due to "       \
                  Event "\n"));                                          \
      yy_lac_established = 0;                                            \
    }                                                                    \
} while (0)
#else
# define YY_LAC_DISCARD(Event) yy_lac_established = 0
#endif

/* Given the stack whose top is *YYSSP, return 0 iff YYTOKEN can
   eventually (after perhaps some reductions) be shifted, return 1 if
   not, or return YYENOMEM if memory is exhausted.  As preconditions and
   postconditions: *YYES_CAPACITY is the allocated size of the array to
   which *YYES points, and either *YYES = YYESA or *YYES points to an
   array allocated with YYSTACK_ALLOC.  yy_lac may overwrite the
   contents of either array, alter *YYES and *YYES_CAPACITY, and free
   any old *YYES other than YYESA.  */
static int
yy_lac (yy_state_t *yyesa, yy_state_t **yyes,
        YYPTRDIFF_T *yyes_capacity, yy_state_t *yyssp, yysymbol_kind_t yytoken)
{
  yy_state_t *yyes_prev = yyssp;
  yy_state_t *yyesp = yyes_prev;
  /* Reduce until we encounter a shift and thereby accept the token.  */
  YYDPRINTF ((stderr, "LAC: checking lookahead %s:", yysymbol_name (yytoken)));
  if (yytoken == YYSYMBOL_YYUNDEF)
    {
      YYDPRINTF ((stderr, " Always Err\n"));
      return 1;
    }
  while (1)
    {
      int yyrule = yypact[+*yyesp];
      if (yypact_value_is_default (yyrule)
          || (yyrule += yytoken) < 0 || YYLAST < yyrule
          || yycheck[yyrule] != yytoken)
        {
          /* Use the default action.  */
          yyrule = yydefact[+*yyesp];
          if (yyrule == 0)
            {
              YYDPRINTF ((stderr, " Err\n"));
              return 1;
            }
        }
      else
        {
          /* Use the action from yytable.  */
          yyrule = yytable[yyrule];
          if (yytable_value_is_error (yyrule))
            {
              YYDPRINTF ((stderr, " Err\n"));
              return 1;
            }
          if (0 < yyrule)
            {
              YYDPRINTF ((stderr, " S%d\n", yyrule));
              return 0;
            }
          yyrule = -yyrule;
        }
      /* By now we know we have to simulate a reduce.  */
      YYDPRINTF ((stderr, " R%d", yyrule - 1));
      {
        /* Pop the corresponding number of values from the stack.  */
        YYPTRDIFF_T yylen = yyr2[yyrule];
        /* First pop from the LAC stack as many tokens as possible.  */
        if (yyesp != yyes_prev)
          {
            YYPTRDIFF_T yysize = yyesp - *yyes + 1;
            if (yylen < yysize)
              {
                yyesp -= yylen;
                yylen = 0;
              }
            else
              {
                yyesp = yyes_prev;
                yylen -= yysize;
              }
          }
        /* Only afterwards look at the main stack.  */
        if (yylen)
          yyesp = yyes_prev -= yylen;
      }
      /* Push the resulting state of the reduction.  */
      {
        yy_state_fast_t yystate;
        {
          const int yylhs = yyr1[yyrule] - YYNTOKENS;
          const int yyi = yypgoto[yylhs] + *yyesp;
          yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyesp
                     ? yytable[yyi]
                     : yydefgoto[yylhs]);
        }
        if (yyesp == yyes_prev)
          {
            yyesp = *yyes;
            YY_IGNORE_USELESS_CAST_BEGIN
            *yyesp = YY_CAST (yy_state_t, yystate);
            YY_IGNORE_USELESS_CAST_END
          }
        else
          {
            if (yy_lac_stack_realloc (yyes_capacity, 1,
#if YYDEBUG
                                      " (", ")",
#endif
                                      yyes, yyesa, &yyesp, yyes_prev))
              {
                YYDPRINTF ((stderr, "\n"));
                return YYENOMEM;
              }
            YY_IGNORE_USELESS_CAST_BEGIN
            *++yyesp = YY_CAST (yy_state_t, yystate);
            YY_IGNORE_USELESS_CAST_END
          }
        YYDPRINTF ((stderr, " G%d", yystate));
      }
    }
}

/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yy_state_t *yyesa;
  yy_state_t **yyes;
  YYPTRDIFF_T *yyes_capacity;
  yysymbol_kind_t yytoken;
  YYLTYPE *yylloc;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;

  int yyx;
  for (yyx = 0; yyx < YYNTOKENS; ++yyx)
    {
      yysymbol_kind_t yysym = YY_CAST (yysymbol_kind_t, yyx);
      if (yysym != YYSYMBOL_YYerror && yysym != YYSYMBOL_YYUNDEF)
        switch (yy_lac (yyctx->yyesa, yyctx->yyes, yyctx->yyes_capacity, yyctx->yyssp, yysym))
          {
          case YYENOMEM:
            return YYENOMEM;
          case 1:
            continue;
          default:
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = yysym;
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif



static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
       In the first two cases, it might appear that the current syntax
       error should have been detected in the previous state when yy_lac
       was invoked.  However, at that time, there might have been a
       different syntax error that discarded a different initial context
       during error recovery, leaving behind the current lookahead.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      YYDPRINTF ((stderr, "Constructing syntax error message\n"));
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else if (yyn == 0)
        YYDPRINTF ((stderr, "No expected tokens.\n"));
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.  In order to see if a particular token T is a
   valid looakhead, invoke yy_lac (YYESA, YYES, YYES_CAPACITY, YYSSP, T).

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store or if
   yy_lac returned YYENOMEM.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yystrlen (yysymbol_name (yyarg[yyi]));
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp = yystpcpy (yyp, yysymbol_name (yyarg[yyi++]));
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, void* scanner, ParseContext* context)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  YY_USE (scanner);
  YY_USE (context);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}






/*----------.
| yyparse.  |
`----------*/

int
yyparse (void* scanner, ParseContext* context)
{
/* Lookahead token kind.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs = 0;

    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

    yy_state_t yyesa[20];
    yy_state_t *yyes = yyesa;
    YYPTRDIFF_T yyes_capacity = 20 < YYMAXDEPTH ? 20 : YYMAXDEPTH;

  /* Whether LAC context is established.  A Boolean.  */
  int yy_lac_established = 0;
  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */


/* User initialization code.  */
#line 236 "grammar83.y"
{
    yylloc = 1;
    memset(context, 0, sizeof(*context));
    context->symbol_table = calloc(64, sizeof(Declaration*));
    context->symbol_table_capacity = 64;
    context->symbol_table_size = 0;
    if(!universal_int_type.name) {
        StringView universal_int_str_view = { .value = universal_integer_str, .len = sizeof(universal_integer_str) };
        universal_int_type.name = string_pool_to_token(universal_int_str_view);
    }
}

#line 2504 "grammar83.tab.c"

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex (&yylval, &yylloc, scanner);
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    {
      YY_LAC_ESTABLISH;
      goto yydefault;
    }
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      YY_LAC_ESTABLISH;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  YY_LAC_DISCARD ("shift");
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  {
    int yychar_backup = yychar;
    switch (yyn)
      {
  case 2: /* goal_symbol: comp_unit  */
#line 250 "grammar83.y"
                        { context->comp_unit = (yyvsp[0].comp_unit); }
#line 2724 "grammar83.tab.c"
    break;

  case 13: /* decl: type_decl  */
#line 276 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), (yyvsp[0].decl));
    }
#line 2733 "grammar83.tab.c"
    break;

  case 14: /* decl: subtype_decl  */
#line 280 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), (yyvsp[0].decl));
    }
#line 2742 "grammar83.tab.c"
    break;

  case 15: /* decl: subprog_decl  */
#line 284 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), &(yyvsp[0].subprogram_decl)->base);
    }
#line 2751 "grammar83.tab.c"
    break;

  case 22: /* object_decl: def_id_s ':' object_qualifier_opt object_subtype_def init_opt ';'  */
#line 297 "grammar83.y"
                                                                      {
        TypeDecl* type_decl = find_type_decl(context, (yyvsp[-2].str_token));
        if(!type_decl) {
            error_print((yyloc), "Unknown type: %s", ST((yyvsp[-2].str_token)));
            error_exit();
        }

        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        uint32_t name_count = StringTokenArray_size(&(yyvsp[-5].str_token_array));
        for(uint32_t i = 0; i < name_count; ++i) {
            ObjectDecl* decl = create_object_decl((yyvsp[-5].str_token_array).data[i], (yyloc));
            check_for_redefinition(context, decl->name, (yyloc));
            decl->is_constant = (yyvsp[-3].bool_);
            decl->type = type_decl;
            decl->init_expr = (yyvsp[-1].expr);
            // TODO: handle deferred constants, which do not have initial expressions
            if(decl->is_constant && !decl->init_expr) {
                error_print((yyloc), "Constant declaration '%s' is not initialized", ST(decl->name));
                error_exit();
            }
            DeclList_append(&(yyval.decl_list), &decl->base);
            push_declaration(context, &decl->base);
        }
    }
#line 2780 "grammar83.tab.c"
    break;

  case 23: /* number_decl: def_id_s ':' CONSTANT IS_ASSIGNED expression ';'  */
#line 323 "grammar83.y"
                                                     {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        uint32_t name_count = StringTokenArray_size(&(yyvsp[-5].str_token_array));
        for(uint32_t i = 0; i < name_count; ++i) {
            ObjectDecl* decl = create_object_decl((yyvsp[-5].str_token_array).data[i], (yyloc));
            check_for_redefinition(context, decl->name, (yyloc));
            decl->is_constant = true;
            decl->type = &universal_int_type;
            decl->init_expr = (yyvsp[-1].expr);
            DeclList_append(&(yyval.decl_list), &decl->base);
            push_declaration(context, &decl->base);
        }
    }
#line 2798 "grammar83.tab.c"
    break;

  case 24: /* def_id_s: identifier  */
#line 338 "grammar83.y"
               {
        StringTokenArray_init(&(yyval.str_token_array));
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2807 "grammar83.tab.c"
    break;

  case 25: /* def_id_s: def_id_s ',' identifier  */
#line 342 "grammar83.y"
                            { StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token)); }
#line 2813 "grammar83.tab.c"
    break;

  case 26: /* object_qualifier_opt: %empty  */
#line 347 "grammar83.y"
             { (yyval.bool_) = false; }
#line 2819 "grammar83.tab.c"
    break;

  case 27: /* object_qualifier_opt: CONSTANT  */
#line 348 "grammar83.y"
             { (yyval.bool_) = true; }
#line 2825 "grammar83.tab.c"
    break;

  case 30: /* init_opt: %empty  */
#line 357 "grammar83.y"
                           { (yyval.expr) = NULL; }
#line 2831 "grammar83.tab.c"
    break;

  case 31: /* init_opt: IS_ASSIGNED expression  */
#line 358 "grammar83.y"
                           { (yyval.expr) = (yyvsp[0].expr); }
#line 2837 "grammar83.tab.c"
    break;

  case 32: /* type_decl: TYPE identifier discrim_part_opt type_completion ';'  */
#line 362 "grammar83.y"
                                                         {
        // TODO: discriminant
        TypeDecl* decl = (yyvsp[-1].type_decl);
        // Note: decl->base.kind is set by the specific type_completion
        decl->base.line_num = (yyloc);
        decl->name = (yyvsp[-3].str_token);
        check_for_redefinition(context, decl->name, (yyloc));
        push_declaration(context, &decl->base);
        (yyval.decl) = &decl->base;
    }
#line 2852 "grammar83.tab.c"
    break;

  case 37: /* type_completion: IS type_def  */
#line 382 "grammar83.y"
                { (yyval.type_decl) = (yyvsp[0].type_decl); }
#line 2858 "grammar83.tab.c"
    break;

  case 46: /* subtype_decl: SUBTYPE identifier IS subtype_ind ';'  */
#line 397 "grammar83.y"
                                          {
        TypeDecl* decl = create_type_decl(TYPE_SUBTYPE);
        decl->base.line_num = (yyloc);
        decl->name = (yyvsp[-3].str_token);
        check_for_redefinition(context, decl->name, (yyloc));
        TypeDecl* base_type = find_type_decl(context, (yyvsp[-1].str_token));
        if(!base_type) {
            error_print((yyloc), "Unknown base type: %s", ST((yyvsp[-1].str_token)));
            error_exit();
        }
        decl->u.subtype.base = base_type;
        push_declaration(context, &decl->base);
        (yyval.decl) = &decl->base;
    }
#line 2877 "grammar83.tab.c"
    break;

  case 47: /* subtype_ind: name constraint  */
#line 414 "grammar83.y"
                    {
        // TODO: propagate constraint somehow
        (yyval.str_token) = (yyvsp[-1].name).name;
    }
#line 2886 "grammar83.tab.c"
    break;

  case 48: /* subtype_ind: name  */
#line 418 "grammar83.y"
         { (yyval.str_token) = (yyvsp[0].name).name; }
#line 2892 "grammar83.tab.c"
    break;

  case 52: /* derived_type: NEW subtype_ind  */
#line 431 "grammar83.y"
                    {
        (yyval.type_decl) = create_type_decl(TYPE_DERIVED);
        TypeDecl* base_type = find_type_decl(context, (yyvsp[0].str_token));
        if(!base_type) {
            error_print((yyloc), "Unknown base type: %s", ST((yyvsp[0].str_token)));
            error_exit();
        }
        (yyval.type_decl)->u.subtype.base = base_type;
    }
#line 2906 "grammar83.tab.c"
    break;

  case 53: /* range_constraint: RANGE range  */
#line 442 "grammar83.y"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 2912 "grammar83.tab.c"
    break;

  case 54: /* range_constr_opt: %empty  */
#line 446 "grammar83.y"
           { (yyval.expr) = NULL; }
#line 2918 "grammar83.tab.c"
    break;

  case 56: /* range: simple_expression DOT_DOT simple_expression  */
#line 451 "grammar83.y"
                                                { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_RANGE, (yyvsp[0].expr)); }
#line 2924 "grammar83.tab.c"
    break;

  case 59: /* enumeration_type: '(' enum_id_s ')'  */
#line 457 "grammar83.y"
                      {
        (yyval.type_decl) = create_type_decl(TYPE_ENUM);
        (yyval.type_decl)->u.enum_.literals = (yyvsp[-1].expr_array).data;
        (yyval.type_decl)->u.enum_.literal_count = ExprPtrArray_size(&(yyvsp[-1].expr_array));
        // TODO: add all enum literals into symbol table scope
    }
#line 2935 "grammar83.tab.c"
    break;

  case 60: /* enum_id_s: enum_id  */
#line 465 "grammar83.y"
            {
        ExprPtrArray_init(&(yyval.expr_array));
        ExprPtrArray_append(&(yyval.expr_array), (yyvsp[0].expr));
    }
#line 2944 "grammar83.tab.c"
    break;

  case 61: /* enum_id_s: enum_id_s ',' enum_id  */
#line 469 "grammar83.y"
                          {
        (yyval.expr_array) = (yyvsp[-2].expr_array);
        ExprPtrArray_append(&(yyval.expr_array), (yyvsp[0].expr));
    }
#line 2953 "grammar83.tab.c"
    break;

  case 62: /* enum_id: identifier  */
#line 475 "grammar83.y"
               {
        (yyval.expr) = create_expr(EXPR_NAME, (yyloc));
        (yyval.expr)->u.name.name = (yyvsp[0].str_token);
    }
#line 2962 "grammar83.tab.c"
    break;

  case 63: /* enum_id: char_lit  */
#line 479 "grammar83.y"
             {
        (yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
        (yyval.expr)->u.char_lit = (yyvsp[0].c);
    }
#line 2971 "grammar83.tab.c"
    break;

  case 64: /* integer_type: range_constraint  */
#line 485 "grammar83.y"
                     {
        (yyval.type_decl) = create_type_decl(TYPE_INTEGER);
        (yyval.type_decl)->u.int_.range = (yyvsp[0].expr);
    }
#line 2980 "grammar83.tab.c"
    break;

  case 107: /* choice_s: choice  */
#line 609 "grammar83.y"
                        {
        ChoiceArray_init(&(yyval.choice_array));
        ChoiceArray_append(&(yyval.choice_array), (yyvsp[0].choice));
    }
#line 2989 "grammar83.tab.c"
    break;

  case 108: /* choice_s: choice_s '|' choice  */
#line 613 "grammar83.y"
                        {
        (yyval.choice_array) = (yyvsp[-2].choice_array);
        ChoiceArray_append(&(yyval.choice_array), (yyvsp[0].choice));
    }
#line 2998 "grammar83.tab.c"
    break;

  case 109: /* choice: expression  */
#line 619 "grammar83.y"
                         {
        (yyval.choice).kind = CHOICE_EXPR;
        (yyval.choice).u.expr = (yyvsp[0].expr);
    }
#line 3007 "grammar83.tab.c"
    break;

  case 111: /* choice: OTHERS  */
#line 624 "grammar83.y"
                         { (yyval.choice).kind = CHOICE_OTHERS; }
#line 3013 "grammar83.tab.c"
    break;

  case 116: /* decl_part: %empty  */
#line 638 "grammar83.y"
                         { (yyval.decl) = NULL; }
#line 3019 "grammar83.tab.c"
    break;

  case 117: /* decl_part: decl_item_or_body_s1  */
#line 639 "grammar83.y"
                         { (yyval.decl) = (yyvsp[0].decl_list).first; }
#line 3025 "grammar83.tab.c"
    break;

  case 118: /* decl_item_s: %empty  */
#line 643 "grammar83.y"
                 { (yyval.decl) = NULL; }
#line 3031 "grammar83.tab.c"
    break;

  case 119: /* decl_item_s: decl_item_s1  */
#line 644 "grammar83.y"
                 { (yyval.decl) = (yyvsp[0].decl_list).first; }
#line 3037 "grammar83.tab.c"
    break;

  case 121: /* decl_item_s1: decl_item_s1 decl_item  */
#line 649 "grammar83.y"
                           {
        DeclList_splice(&(yyvsp[-1].decl_list), &(yyvsp[0].decl_list));
        (yyval.decl_list) = (yyvsp[-1].decl_list);
    }
#line 3046 "grammar83.tab.c"
    break;

  case 127: /* decl_item_or_body_s1: decl_item_or_body_s1 decl_item_or_body  */
#line 663 "grammar83.y"
                                           {
        DeclList_splice(&(yyvsp[-1].decl_list), &(yyvsp[0].decl_list));
        (yyval.decl_list) = (yyvsp[-1].decl_list);
    }
#line 3055 "grammar83.tab.c"
    break;

  case 128: /* decl_item_or_body: body  */
#line 669 "grammar83.y"
              {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), (yyvsp[0].decl));
    }
#line 3064 "grammar83.tab.c"
    break;

  case 130: /* body: subprog_body  */
#line 677 "grammar83.y"
                 { (yyval.decl) = &(yyvsp[0].subprogram_decl)->base; }
#line 3070 "grammar83.tab.c"
    break;

  case 132: /* name: simple_name  */
#line 682 "grammar83.y"
                {
        memset(&(yyval.name), 0, sizeof((yyval.name)));
        (yyval.name).name = (yyvsp[0].str_token);
    }
#line 3079 "grammar83.tab.c"
    break;

  case 136: /* name: operator_symbol  */
#line 689 "grammar83.y"
                    {
        memset(&(yyval.name), 0, sizeof((yyval.name)));
        (yyval.name).name = (yyvsp[0].str_token);
        //TODO: lookup operator, determine its arity, and allocate args array
    }
#line 3089 "grammar83.tab.c"
    break;

  case 143: /* used_char: char_lit  */
#line 711 "grammar83.y"
             {
        (yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
        (yyval.expr)->u.char_lit = (yyvsp[0].c);
    }
#line 3098 "grammar83.tab.c"
    break;

  case 161: /* literal: numeric_lit  */
#line 755 "grammar83.y"
                {
        // TODO: support non-integer numeric literals
        int base = get_base((yyvsp[0].str), (yyloc));

        char num_buffer[128];
        num_buffer[0] = '\0';
        if(!prepare_num_str((yyvsp[0].str), num_buffer, sizeof(num_buffer))) {
            error_print((yyloc), "Numeric literal is too long to be processed (max supported is 127 characters)");
            error_exit();
        }

        // Note: don't overwrite $$ here since we are still using its value
        Expression* expr = create_expr(EXPR_INT_LIT, (yyloc));
        if(mpz_init_set_str(expr->u.int_lit.value, num_buffer, base) < 0) {
            error_print((yyloc), "Invalid numeric literal: '%.*s' for base %u", SV((yyvsp[0].str)), base);
            error_exit();
        }
        (yyval.expr) = expr;
    }
#line 3122 "grammar83.tab.c"
    break;

  case 173: /* expression: expression logical relation  */
#line 797 "grammar83.y"
                                      { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3128 "grammar83.tab.c"
    break;

  case 174: /* expression: expression short_circuit relation  */
#line 798 "grammar83.y"
                                      { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3134 "grammar83.tab.c"
    break;

  case 175: /* logical: AND  */
#line 802 "grammar83.y"
        { (yyval.unary_op) = OP_AND; }
#line 3140 "grammar83.tab.c"
    break;

  case 176: /* logical: OR  */
#line 803 "grammar83.y"
        { (yyval.unary_op) = OP_OR; }
#line 3146 "grammar83.tab.c"
    break;

  case 177: /* logical: XOR  */
#line 804 "grammar83.y"
        { (yyval.unary_op) = OP_XOR; }
#line 3152 "grammar83.tab.c"
    break;

  case 178: /* short_circuit: AND THEN  */
#line 808 "grammar83.y"
               { (yyval.unary_op) = OP_AND_THEN; }
#line 3158 "grammar83.tab.c"
    break;

  case 179: /* short_circuit: OR ELSE  */
#line 809 "grammar83.y"
               { (yyval.unary_op) = OP_OR_ELSE; }
#line 3164 "grammar83.tab.c"
    break;

  case 181: /* relation: simple_expression relational simple_expression  */
#line 815 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3170 "grammar83.tab.c"
    break;

  case 182: /* relation: simple_expression membership range  */
#line 816 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3176 "grammar83.tab.c"
    break;

  case 183: /* relation: simple_expression membership name  */
#line 817 "grammar83.y"
                                                   {
        Expression* right = create_expr(EXPR_NAME, (yylsp[0]));
        right->u.name = (yyvsp[0].name);
        (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), right);
    }
#line 3186 "grammar83.tab.c"
    break;

  case 184: /* relational: '='  */
#line 824 "grammar83.y"
          { (yyval.unary_op) = OP_EQ; }
#line 3192 "grammar83.tab.c"
    break;

  case 185: /* relational: NE  */
#line 825 "grammar83.y"
          { (yyval.unary_op) = OP_NEQ; }
#line 3198 "grammar83.tab.c"
    break;

  case 186: /* relational: '<'  */
#line 826 "grammar83.y"
          { (yyval.unary_op) = OP_LT; }
#line 3204 "grammar83.tab.c"
    break;

  case 187: /* relational: LT_EQ  */
#line 827 "grammar83.y"
          { (yyval.unary_op) = OP_LTE; }
#line 3210 "grammar83.tab.c"
    break;

  case 188: /* relational: '>'  */
#line 828 "grammar83.y"
          { (yyval.unary_op) = OP_GT; }
#line 3216 "grammar83.tab.c"
    break;

  case 189: /* relational: GE  */
#line 829 "grammar83.y"
          { (yyval.unary_op) = OP_GTE; }
#line 3222 "grammar83.tab.c"
    break;

  case 190: /* membership: IN  */
#line 833 "grammar83.y"
           { (yyval.unary_op) = OP_IN; }
#line 3228 "grammar83.tab.c"
    break;

  case 191: /* membership: NOT IN  */
#line 834 "grammar83.y"
           { (yyval.unary_op) = OP_NOT_IN; }
#line 3234 "grammar83.tab.c"
    break;

  case 193: /* simple_expression: unary term  */
#line 839 "grammar83.y"
                                  { (yyval.expr) = make_unary_expr((yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3240 "grammar83.tab.c"
    break;

  case 194: /* simple_expression: simple_expression adding term  */
#line 840 "grammar83.y"
                                  { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3246 "grammar83.tab.c"
    break;

  case 195: /* unary: '+'  */
#line 844 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_PLUS; }
#line 3252 "grammar83.tab.c"
    break;

  case 196: /* unary: '-'  */
#line 845 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_MINUS; }
#line 3258 "grammar83.tab.c"
    break;

  case 197: /* adding: '+'  */
#line 849 "grammar83.y"
        { (yyval.unary_op) = OP_PLUS; }
#line 3264 "grammar83.tab.c"
    break;

  case 198: /* adding: '-'  */
#line 850 "grammar83.y"
        { (yyval.unary_op) = OP_MINUS; }
#line 3270 "grammar83.tab.c"
    break;

  case 199: /* adding: '&'  */
#line 851 "grammar83.y"
        { (yyval.unary_op) = OP_AMP; }
#line 3276 "grammar83.tab.c"
    break;

  case 201: /* term: term multiplying factor  */
#line 856 "grammar83.y"
                            { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3282 "grammar83.tab.c"
    break;

  case 202: /* multiplying: '*'  */
#line 860 "grammar83.y"
        { (yyval.unary_op) = OP_MULT; }
#line 3288 "grammar83.tab.c"
    break;

  case 203: /* multiplying: '/'  */
#line 861 "grammar83.y"
        { (yyval.unary_op) = OP_DIVIDE; }
#line 3294 "grammar83.tab.c"
    break;

  case 204: /* multiplying: MOD  */
#line 862 "grammar83.y"
        { (yyval.unary_op) = OP_MOD; }
#line 3300 "grammar83.tab.c"
    break;

  case 205: /* multiplying: REM  */
#line 863 "grammar83.y"
        { (yyval.unary_op) = OP_REM; }
#line 3306 "grammar83.tab.c"
    break;

  case 207: /* factor: NOT primary  */
#line 868 "grammar83.y"
                          { (yyval.expr) = make_unary_expr(OP_NOT, (yyvsp[0].expr)); }
#line 3312 "grammar83.tab.c"
    break;

  case 208: /* factor: ABS primary  */
#line 869 "grammar83.y"
                          { (yyval.expr) = make_unary_expr(OP_ABS, (yyvsp[0].expr)); }
#line 3318 "grammar83.tab.c"
    break;

  case 209: /* factor: primary EXPON primary  */
#line 870 "grammar83.y"
                          { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_EXP, (yyvsp[0].expr)); }
#line 3324 "grammar83.tab.c"
    break;

  case 211: /* primary: name  */
#line 875 "grammar83.y"
         {
        (yyval.expr) = create_expr(EXPR_NAME, (yyloc));
        (yyval.expr)->u.name = (yyvsp[0].name);
    }
#line 3333 "grammar83.tab.c"
    break;

  case 216: /* parenthesized_primary: '(' expression ')'  */
#line 886 "grammar83.y"
                       { (yyval.expr) = (yyvsp[-1].expr); }
#line 3339 "grammar83.tab.c"
    break;

  case 220: /* statement_s: statement  */
#line 899 "grammar83.y"
                          {
        memset(&(yyval.stmt_list), 0, sizeof((yyval.stmt_list)));
        StmtList_append(&(yyval.stmt_list), (yyvsp[0].stmt));
    }
#line 3348 "grammar83.tab.c"
    break;

  case 221: /* statement_s: statement_s statement  */
#line 903 "grammar83.y"
                          {
        StmtList_append(&(yyvsp[-1].stmt_list), (yyvsp[0].stmt));
        (yyval.stmt_list) = (yyvsp[-1].stmt_list);
    }
#line 3357 "grammar83.tab.c"
    break;

  case 223: /* statement: goto_label statement  */
#line 910 "grammar83.y"
                         {
        check_for_redefinition(context, (yyvsp[-1].str_token), (yylsp[-1]));
        LabelDecl* label = create_label((yyvsp[-1].str_token), (yylsp[-1]));
        push_declaration(context, (Declaration*)label);
        (yyval.stmt) = (yyvsp[0].stmt);
    }
#line 3368 "grammar83.tab.c"
    break;

  case 240: /* null_stmt: NuLL ';'  */
#line 943 "grammar83.y"
             { (yyval.stmt) = create_stmt(STMT_NULL, (yyloc)); }
#line 3374 "grammar83.tab.c"
    break;

  case 241: /* assign_stmt: name IS_ASSIGNED expression ';'  */
#line 947 "grammar83.y"
                                    {
        (yyval.stmt) = create_stmt(STMT_ASSIGN, (yyloc));
        (yyval.stmt)->u.assign.dest.kind = EXPR_NAME;
        (yyval.stmt)->u.assign.dest.line_num = (yyloc);
        (yyval.stmt)->u.assign.dest.u.name = (yyvsp[-3].name);
        (yyval.stmt)->u.assign.expr = (yyvsp[-1].expr);
    }
#line 3386 "grammar83.tab.c"
    break;

  case 242: /* if_stmt: IF cond_clause_s else_opt END IF ';'  */
#line 956 "grammar83.y"
                                         {
        (yyval.stmt) = (yyvsp[-4].stmt);
        Statement* branch = (yyvsp[-4].stmt);
        while(branch->u.if_.else_) {
            branch = branch->u.if_.else_;
            assert(branch->kind == STMT_IF);
        }
        branch->u.if_.else_ = (yyvsp[-3].stmt);
    }
#line 3400 "grammar83.tab.c"
    break;

  case 244: /* cond_clause_s: cond_clause_s ELSIF cond_clause  */
#line 968 "grammar83.y"
                                    {
        (yyval.stmt) = (yyvsp[-2].stmt);
        (yyval.stmt)->u.if_.else_ = (yyvsp[0].stmt);
    }
#line 3409 "grammar83.tab.c"
    break;

  case 245: /* cond_clause: cond_part statement_s  */
#line 974 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_IF, (yyloc));
        (yyval.stmt)->u.if_.condition = (yyvsp[-1].expr);
        (yyval.stmt)->u.if_.stmts = (yyvsp[0].stmt_list).first;
    }
#line 3419 "grammar83.tab.c"
    break;

  case 246: /* cond_part: condition THEN  */
#line 981 "grammar83.y"
                   { (yyval.expr) = (yyvsp[-1].expr); }
#line 3425 "grammar83.tab.c"
    break;

  case 248: /* else_opt: %empty  */
#line 989 "grammar83.y"
                     { (yyval.stmt) = NULL; }
#line 3431 "grammar83.tab.c"
    break;

  case 249: /* else_opt: ELSE statement_s  */
#line 990 "grammar83.y"
                     { (yyval.stmt) = (yyvsp[0].stmt_list).first; }
#line 3437 "grammar83.tab.c"
    break;

  case 250: /* case_stmt: case_hdr pragma_s alternative_s END CASE ';'  */
#line 994 "grammar83.y"
                                                 {
        (yyval.stmt) = (yyvsp[-5].stmt);
        // TODO: pragmas
        (yyval.stmt)->u.case_.cases = (yyvsp[-3].case_list).first;
    }
#line 3447 "grammar83.tab.c"
    break;

  case 251: /* case_hdr: CASE expression IS  */
#line 1001 "grammar83.y"
                       {
        (yyval.stmt) = create_stmt(STMT_CASE, (yyloc));
        (yyval.stmt)->u.case_.expr = (yyvsp[-1].expr);
    }
#line 3456 "grammar83.tab.c"
    break;

  case 252: /* alternative_s: %empty  */
#line 1007 "grammar83.y"
                              { memset(&(yyval.case_list), 0, sizeof((yyval.case_list))); }
#line 3462 "grammar83.tab.c"
    break;

  case 253: /* alternative_s: alternative_s alternative  */
#line 1008 "grammar83.y"
                              {
        (yyval.case_list) = (yyvsp[-1].case_list);
        AltList_append(&(yyval.case_list), (yyvsp[0].case_));
    }
#line 3471 "grammar83.tab.c"
    break;

  case 254: /* alternative: WHEN choice_s RIGHT_SHAFT statement_s  */
#line 1014 "grammar83.y"
                                          {
        (yyval.case_) = calloc(1, sizeof(Alternative));
        (yyval.case_)->choices.choices = (yyvsp[-2].choice_array).data;
        (yyval.case_)->choices.count = ChoiceArray_size(&(yyvsp[-2].choice_array));
        (yyval.case_)->stmts = (yyvsp[0].stmt_list).first;
    }
#line 3482 "grammar83.tab.c"
    break;

  case 255: /* loop_stmt: label_opt loop_content id_opt ';'  */
#line 1023 "grammar83.y"
                                      { (yyval.stmt) = (yyvsp[-2].stmt); }
#line 3488 "grammar83.tab.c"
    break;

  case 258: /* loop_content: basic_loop  */
#line 1032 "grammar83.y"
               {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_WHILE;
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
        // Create condition so this becomes a 'while True' loop
        // TODO: should be a boolean literal
        Expression* condition = create_expr(EXPR_INT_LIT, (yyloc));
        mpz_init_set_ui(condition->u.int_lit.value, 1);
        (yyval.stmt)->u.loop.u.while_.condition = condition;
    }
#line 3503 "grammar83.tab.c"
    break;

  case 259: /* loop_content: WHILE condition basic_loop  */
#line 1042 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_WHILE;
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
        (yyval.stmt)->u.loop.u.while_.condition = (yyvsp[-1].expr);
    }
#line 3514 "grammar83.tab.c"
    break;

  case 260: /* loop_content: iter_part reverse_opt discrete_range basic_loop  */
#line 1048 "grammar83.y"
                                                    {
        // TODO: range
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_FOR;
        (yyval.stmt)->u.loop.reverse = (yyvsp[-2].bool_);
        (yyval.stmt)->u.loop.u.for_.var = (yyvsp[-3].object_decl);
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
    }
#line 3527 "grammar83.tab.c"
    break;

  case 261: /* iter_part: FOR identifier IN  */
#line 1058 "grammar83.y"
                      {
        memset(&(yyval.object_decl), 0, sizeof((yyval.object_decl)));
        (yyval.object_decl).base.kind = DECL_OBJECT;
        (yyval.object_decl).base.line_num = (yyloc);
        (yyval.object_decl).name = (yyvsp[-1].str_token);
    }
#line 3538 "grammar83.tab.c"
    break;

  case 262: /* reverse_opt: %empty  */
#line 1066 "grammar83.y"
            { (yyval.bool_) = false; }
#line 3544 "grammar83.tab.c"
    break;

  case 263: /* reverse_opt: REVERSE  */
#line 1067 "grammar83.y"
            { (yyval.bool_) = true; }
#line 3550 "grammar83.tab.c"
    break;

  case 264: /* basic_loop: LOOP statement_s END LOOP  */
#line 1071 "grammar83.y"
                              { (yyval.stmt) = (yyvsp[-2].stmt_list).first; }
#line 3556 "grammar83.tab.c"
    break;

  case 267: /* block: label_opt block_decl block_body END id_opt ';'  */
#line 1081 "grammar83.y"
                                                   {
        (yyval.stmt) = create_stmt(STMT_BLOCK, (yyloc));
        (yyval.stmt)->u.block.decls = (yyvsp[-4].decl);
        (yyval.stmt)->u.block.stmts = (yyvsp[-3].stmt);
        // Close scope if needed (i.e. if there was a declaration section)
        if((yyvsp[-4].decl)) {
            end_scope(context, (yylsp[-2]));
        }
    }
#line 3570 "grammar83.tab.c"
    break;

  case 268: /* block_decl: %empty  */
#line 1092 "grammar83.y"
                                                    { (yyval.decl) = NULL; }
#line 3576 "grammar83.tab.c"
    break;

  case 269: /* $@1: %empty  */
#line 1093 "grammar83.y"
            { begin_scope(context, (yylsp[0])); }
#line 3582 "grammar83.tab.c"
    break;

  case 270: /* block_decl: DECLARE $@1 decl_part  */
#line 1093 "grammar83.y"
                                                    { (yyval.decl) = (yyvsp[0].decl); }
#line 3588 "grammar83.tab.c"
    break;

  case 271: /* block_body: BEGiN handled_stmt_s  */
#line 1097 "grammar83.y"
                         { (yyval.stmt) = (yyvsp[0].stmt); }
#line 3594 "grammar83.tab.c"
    break;

  case 272: /* handled_stmt_s: statement_s except_handler_part_opt  */
#line 1102 "grammar83.y"
                                        { (yyval.stmt) = (yyvsp[-1].stmt_list).first; }
#line 3600 "grammar83.tab.c"
    break;

  case 275: /* exit_stmt: EXIT name_opt when_opt ';'  */
#line 1111 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_EXIT, (yyloc));
        // TODO: name_opt
        (yyval.stmt)->u.exit.condition = (yyvsp[-1].expr);
    }
#line 3610 "grammar83.tab.c"
    break;

  case 278: /* when_opt: %empty  */
#line 1123 "grammar83.y"
                   { (yyval.expr) = NULL; }
#line 3616 "grammar83.tab.c"
    break;

  case 279: /* when_opt: WHEN condition  */
#line 1124 "grammar83.y"
                   { (yyval.expr) = (yyvsp[0].expr); }
#line 3622 "grammar83.tab.c"
    break;

  case 280: /* return_stmt: RETURN ';'  */
#line 1128 "grammar83.y"
                  { (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc)); }
#line 3628 "grammar83.tab.c"
    break;

  case 281: /* return_stmt: RETURN expression ';'  */
#line 1129 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc));
        (yyval.stmt)->u.return_.expr = (yyvsp[-1].expr);
    }
#line 3637 "grammar83.tab.c"
    break;

  case 282: /* goto_stmt: GOTO name ';'  */
#line 1135 "grammar83.y"
                  {
        if((yyvsp[-1].name).arg_count != 0) {
            error_print((yylsp[-1]), "Invalid label name (must be a simple name)");
            error_exit();
        }
        StringToken label_name = (yyvsp[-1].name).name;

        (yyval.stmt) = create_stmt(STMT_GOTO, (yyloc));
        LabelDecl* label = find_label(context, label_name);
        if(label) {
            // Label is defined prior to the goto statement
            (yyval.stmt)->u.goto_.label = label;
        } else {
            // Label is not defined yet
            check_for_redefinition(context, label_name, (yylsp[-1]));
            // Define a placeholder label
            // TODO: in semantic analysis, verify that all placeholder labels are filled in
            LabelDecl* label = create_label(label_name, (yylsp[-1]));
            (yyval.stmt)->u.goto_.label = label;
            push_declaration(context, (Declaration*)label);
        }
    }
#line 3664 "grammar83.tab.c"
    break;

  case 283: /* subprog_decl: subprog_spec ';'  */
#line 1159 "grammar83.y"
                          { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3670 "grammar83.tab.c"
    break;

  case 285: /* @2: %empty  */
#line 1165 "grammar83.y"
                                           {
        begin_scope(context, (yylsp[0]));
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
    }
#line 3680 "grammar83.tab.c"
    break;

  case 286: /* subprog_spec: PROCEDURE simple_name @2 formal_part_opt  */
#line 1170 "grammar83.y"
                                { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3686 "grammar83.tab.c"
    break;

  case 287: /* @3: %empty  */
#line 1171 "grammar83.y"
                                         {
        begin_scope(context, (yylsp[0]));
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
    }
#line 3696 "grammar83.tab.c"
    break;

  case 288: /* subprog_spec: FUNCTION designator @3 formal_part_opt RETURN name  */
#line 1176 "grammar83.y"
                                { (yyval.subprogram_decl) = (yyvsp[-3].subprogram_decl); }
#line 3702 "grammar83.tab.c"
    break;

  case 291: /* designator: char_string  */
#line 1182 "grammar83.y"
                { (yyval.str_token) = string_pool_to_token((yyvsp[0].str)); }
#line 3708 "grammar83.tab.c"
    break;

  case 299: /* mode: %empty  */
#line 1205 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3714 "grammar83.tab.c"
    break;

  case 300: /* mode: IN  */
#line 1206 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3720 "grammar83.tab.c"
    break;

  case 301: /* mode: OUT  */
#line 1207 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_OUT; }
#line 3726 "grammar83.tab.c"
    break;

  case 302: /* mode: IN OUT  */
#line 1208 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN_OUT; }
#line 3732 "grammar83.tab.c"
    break;

  case 303: /* subprog_spec_is_push: subprog_spec IS  */
#line 1212 "grammar83.y"
                    { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3738 "grammar83.tab.c"
    break;

  case 304: /* subprog_body: subprog_spec_is_push decl_part block_body END id_opt ';'  */
#line 1216 "grammar83.y"
                                                             {
        (yyval.subprogram_decl) = (yyvsp[-5].subprogram_decl);
        (yyval.subprogram_decl)->decls = (yyvsp[-4].decl);
        (yyval.subprogram_decl)->stmts = (yyvsp[-3].stmt);
    }
#line 3748 "grammar83.tab.c"
    break;

  case 305: /* procedure_call: name ';'  */
#line 1223 "grammar83.y"
             {
        (yyval.stmt) = create_stmt(STMT_EXPR, (yyloc));
        (yyval.stmt)->u.expr.kind = EXPR_NAME;
        (yyval.stmt)->u.expr.line_num = (yyloc);
        (yyval.stmt)->u.expr.u.name = (yyvsp[-1].name);
    }
#line 3759 "grammar83.tab.c"
    break;

  case 306: /* pkg_decl: pkg_spec ';'  */
#line 1232 "grammar83.y"
                         { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3765 "grammar83.tab.c"
    break;

  case 308: /* pkg_spec: PACKAGE simple_name IS decl_item_s private_part END simple_name_opt  */
#line 1237 "grammar83.y"
                                                                        {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_PACKAGE_SPEC);
        (yyval.comp_unit)->u.package_spec.name = (yyvsp[-5].str_token);
        (yyval.comp_unit)->u.package_spec.decls = (yyvsp[-3].decl);
        // TODO: private part
        // TODO: check simple_name_opt matches
    }
#line 3777 "grammar83.tab.c"
    break;

  case 313: /* pkg_body: PACKAGE BODY simple_name IS decl_part body_opt END simple_name_opt ';'  */
#line 1256 "grammar83.y"
                                                                           {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_PACKAGE_BODY);
        (yyval.comp_unit)->u.package_body.name = (yyvsp[-6].str_token);
        // TODO: decl_part
        // TODO: body_opt
        // TODO: check simple_name_opt matches
    }
#line 3789 "grammar83.tab.c"
    break;

  case 330: /* comp_unit: context_spec unit pragma_s  */
#line 1305 "grammar83.y"
                               { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3795 "grammar83.tab.c"
    break;

  case 331: /* comp_unit: unit pragma_s  */
#line 1306 "grammar83.y"
                               { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3801 "grammar83.tab.c"
    break;

  case 340: /* unit: subprog_decl  */
#line 1327 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        (yyval.comp_unit)->u.subprogram_decl = (yyvsp[0].subprogram_decl);
    }
#line 3810 "grammar83.tab.c"
    break;

  case 341: /* unit: subprog_body  */
#line 1331 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        (yyval.comp_unit)->u.subprogram_decl = (yyvsp[0].subprogram_decl);
    }
#line 3819 "grammar83.tab.c"
    break;


#line 3823 "grammar83.tab.c"

        default: break;
      }
    if (yychar_backup != yychar)
      YY_LAC_DISCARD ("yychar change");
  }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yyesa, &yyes, &yyes_capacity, yytoken, &yylloc};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        if (yychar != YYEMPTY)
          YY_LAC_ESTABLISH;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (&yylloc, scanner, context, yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc, scanner, context);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp, scanner, context);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  /* If the stack popping above didn't lose the initial context for the
     current lookahead token, the shift below will for sure.  */
  YY_LAC_DISCARD ("error recovery");

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, scanner, context, YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, scanner, context);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp, scanner, context);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yyes != yyesa)
    YYSTACK_FREE (yyes);
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 1476 "grammar83.y"


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
    DeclList_append(curr_scope, decl);
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

ObjectDecl* find_object_decl(ParseContext* context, StringToken name)
{
    Declaration** bucket = find_bucket(context, name);
    if(bucket) {
        for(Declaration* decl = *bucket; decl; decl = decl->next_overload) {
            if(decl->kind == DECL_OBJECT) {
                return (ObjectDecl*)decl;
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
CompilationUnit* create_comp_unit(CompilationUnitKind kind)
{
    CompilationUnit* comp_unit = calloc(1, sizeof(CompilationUnit));
    comp_unit->kind = kind;
    return comp_unit;
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
