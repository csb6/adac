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
  YYSYMBOL_compound_name = 163,            /* compound_name  */
  YYSYMBOL_c_name_list = 164,              /* c_name_list  */
  YYSYMBOL_used_char = 165,                /* used_char  */
  YYSYMBOL_operator_symbol = 166,          /* operator_symbol  */
  YYSYMBOL_indexed_comp = 167,             /* indexed_comp  */
  YYSYMBOL_value_s = 168,                  /* value_s  */
  YYSYMBOL_value = 169,                    /* value  */
  YYSYMBOL_selected_comp = 170,            /* selected_comp  */
  YYSYMBOL_attribute = 171,                /* attribute  */
  YYSYMBOL_attribute_id = 172,             /* attribute_id  */
  YYSYMBOL_literal = 173,                  /* literal  */
  YYSYMBOL_aggregate = 174,                /* aggregate  */
  YYSYMBOL_value_s_2 = 175,                /* value_s_2  */
  YYSYMBOL_comp_assoc = 176,               /* comp_assoc  */
  YYSYMBOL_expression = 177,               /* expression  */
  YYSYMBOL_logical = 178,                  /* logical  */
  YYSYMBOL_short_circuit = 179,            /* short_circuit  */
  YYSYMBOL_relation = 180,                 /* relation  */
  YYSYMBOL_relational = 181,               /* relational  */
  YYSYMBOL_membership = 182,               /* membership  */
  YYSYMBOL_simple_expression = 183,        /* simple_expression  */
  YYSYMBOL_unary = 184,                    /* unary  */
  YYSYMBOL_adding = 185,                   /* adding  */
  YYSYMBOL_term = 186,                     /* term  */
  YYSYMBOL_multiplying = 187,              /* multiplying  */
  YYSYMBOL_factor = 188,                   /* factor  */
  YYSYMBOL_primary = 189,                  /* primary  */
  YYSYMBOL_parenthesized_primary = 190,    /* parenthesized_primary  */
  YYSYMBOL_qualified = 191,                /* qualified  */
  YYSYMBOL_allocator = 192,                /* allocator  */
  YYSYMBOL_statement_s = 193,              /* statement_s  */
  YYSYMBOL_statement = 194,                /* statement  */
  YYSYMBOL_unlabeled = 195,                /* unlabeled  */
  YYSYMBOL_simple_stmt = 196,              /* simple_stmt  */
  YYSYMBOL_compound_stmt = 197,            /* compound_stmt  */
  YYSYMBOL_null_stmt = 198,                /* null_stmt  */
  YYSYMBOL_assign_stmt = 199,              /* assign_stmt  */
  YYSYMBOL_if_stmt = 200,                  /* if_stmt  */
  YYSYMBOL_cond_clause_s = 201,            /* cond_clause_s  */
  YYSYMBOL_cond_clause = 202,              /* cond_clause  */
  YYSYMBOL_cond_part = 203,                /* cond_part  */
  YYSYMBOL_condition = 204,                /* condition  */
  YYSYMBOL_else_opt = 205,                 /* else_opt  */
  YYSYMBOL_case_stmt = 206,                /* case_stmt  */
  YYSYMBOL_case_hdr = 207,                 /* case_hdr  */
  YYSYMBOL_alternative_s = 208,            /* alternative_s  */
  YYSYMBOL_alternative = 209,              /* alternative  */
  YYSYMBOL_loop_stmt = 210,                /* loop_stmt  */
  YYSYMBOL_label_opt = 211,                /* label_opt  */
  YYSYMBOL_loop_content = 212,             /* loop_content  */
  YYSYMBOL_iter_part = 213,                /* iter_part  */
  YYSYMBOL_reverse_opt = 214,              /* reverse_opt  */
  YYSYMBOL_basic_loop = 215,               /* basic_loop  */
  YYSYMBOL_id_opt = 216,                   /* id_opt  */
  YYSYMBOL_block = 217,                    /* block  */
  YYSYMBOL_block_decl = 218,               /* block_decl  */
  YYSYMBOL_219_1 = 219,                    /* $@1  */
  YYSYMBOL_block_body = 220,               /* block_body  */
  YYSYMBOL_handled_stmt_s = 221,           /* handled_stmt_s  */
  YYSYMBOL_except_handler_part_opt = 222,  /* except_handler_part_opt  */
  YYSYMBOL_exit_stmt = 223,                /* exit_stmt  */
  YYSYMBOL_name_opt = 224,                 /* name_opt  */
  YYSYMBOL_when_opt = 225,                 /* when_opt  */
  YYSYMBOL_return_stmt = 226,              /* return_stmt  */
  YYSYMBOL_goto_stmt = 227,                /* goto_stmt  */
  YYSYMBOL_subprog_decl = 228,             /* subprog_decl  */
  YYSYMBOL_subprog_spec = 229,             /* subprog_spec  */
  YYSYMBOL_230_2 = 230,                    /* @2  */
  YYSYMBOL_231_3 = 231,                    /* @3  */
  YYSYMBOL_designator = 232,               /* designator  */
  YYSYMBOL_formal_part_opt = 233,          /* formal_part_opt  */
  YYSYMBOL_formal_part = 234,              /* formal_part  */
  YYSYMBOL_param_s = 235,                  /* param_s  */
  YYSYMBOL_param = 236,                    /* param  */
  YYSYMBOL_mode = 237,                     /* mode  */
  YYSYMBOL_subprog_spec_is_push = 238,     /* subprog_spec_is_push  */
  YYSYMBOL_subprog_body = 239,             /* subprog_body  */
  YYSYMBOL_procedure_call = 240,           /* procedure_call  */
  YYSYMBOL_pkg_decl = 241,                 /* pkg_decl  */
  YYSYMBOL_pkg_spec = 242,                 /* pkg_spec  */
  YYSYMBOL_private_part = 243,             /* private_part  */
  YYSYMBOL_c_id_opt = 244,                 /* c_id_opt  */
  YYSYMBOL_pkg_body = 245,                 /* pkg_body  */
  YYSYMBOL_body_opt = 246,                 /* body_opt  */
  YYSYMBOL_private_type = 247,             /* private_type  */
  YYSYMBOL_limited_opt = 248,              /* limited_opt  */
  YYSYMBOL_use_clause = 249,               /* use_clause  */
  YYSYMBOL_name_s = 250,                   /* name_s  */
  YYSYMBOL_rename_decl = 251,              /* rename_decl  */
  YYSYMBOL_rename_unit = 252,              /* rename_unit  */
  YYSYMBOL_renames = 253,                  /* renames  */
  YYSYMBOL_comp_unit = 254,                /* comp_unit  */
  YYSYMBOL_context_spec = 255,             /* context_spec  */
  YYSYMBOL_with_clause = 256,              /* with_clause  */
  YYSYMBOL_use_clause_opt = 257,           /* use_clause_opt  */
  YYSYMBOL_unit = 258,                     /* unit  */
  YYSYMBOL_subunit = 259,                  /* subunit  */
  YYSYMBOL_subunit_body = 260,             /* subunit_body  */
  YYSYMBOL_body_stub = 261,                /* body_stub  */
  YYSYMBOL_exception_decl = 262,           /* exception_decl  */
  YYSYMBOL_except_handler_part = 263,      /* except_handler_part  */
  YYSYMBOL_exception_handler = 264,        /* exception_handler  */
  YYSYMBOL_except_choice_s = 265,          /* except_choice_s  */
  YYSYMBOL_except_choice = 266,            /* except_choice  */
  YYSYMBOL_raise_stmt = 267,               /* raise_stmt  */
  YYSYMBOL_generic_decl = 268,             /* generic_decl  */
  YYSYMBOL_generic_formal_part = 269,      /* generic_formal_part  */
  YYSYMBOL_generic_formal = 270,           /* generic_formal  */
  YYSYMBOL_generic_discrim_part_opt = 271, /* generic_discrim_part_opt  */
  YYSYMBOL_subp_default = 272,             /* subp_default  */
  YYSYMBOL_generic_type_def = 273,         /* generic_type_def  */
  YYSYMBOL_generic_derived_type = 274,     /* generic_derived_type  */
  YYSYMBOL_generic_subp_inst = 275,        /* generic_subp_inst  */
  YYSYMBOL_generic_pkg_inst = 276,         /* generic_pkg_inst  */
  YYSYMBOL_generic_inst = 277,             /* generic_inst  */
  YYSYMBOL_rep_spec = 278,                 /* rep_spec  */
  YYSYMBOL_attrib_def = 279,               /* attrib_def  */
  YYSYMBOL_record_type_spec = 280,         /* record_type_spec  */
  YYSYMBOL_align_opt = 281,                /* align_opt  */
  YYSYMBOL_comp_loc_s = 282,               /* comp_loc_s  */
  YYSYMBOL_address_spec = 283,             /* address_spec  */
  YYSYMBOL_code_stmt = 284                 /* code_stmt  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;



/* Unqualified %code blocks.  */
#line 74 "grammar83.y"

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

#line 476 "grammar83.tab.c"

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
#define YYFINAL  36
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1346

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  95
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  190
/* YYNRULES -- Number of rules.  */
#define YYNRULES  404
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  721

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
       0,   223,   223,   227,   228,   232,   233,   237,   238,   242,
     243,   247,   248,   249,   250,   251,   252,   253,   254,   255,
     256,   257,   261,   285,   298,   302,   307,   308,   312,   313,
     317,   318,   322,   333,   334,   335,   340,   341,   345,   346,
     347,   348,   349,   350,   351,   352,   356,   372,   376,   380,
     381,   385,   389,   400,   404,   405,   409,   410,   411,   415,
     423,   427,   431,   435,   441,   445,   449,   450,   454,   458,
     459,   463,   464,   468,   472,   476,   480,   481,   485,   489,
     493,   494,   498,   499,   503,   507,   508,   512,   513,   514,
     518,   519,   523,   524,   528,   529,   533,   537,   538,   542,
     543,   547,   548,   552,   556,   557,   561,   565,   566,   570,
     571,   572,   576,   577,   581,   582,   586,   587,   591,   592,
     596,   597,   601,   602,   603,   604,   608,   609,   613,   614,
     618,   619,   623,   627,   628,   629,   630,   637,   638,   639,
     643,   647,   648,   652,   653,   657,   663,   667,   671,   672,
     676,   677,   678,   679,   683,   684,   685,   686,   690,   694,
     695,   696,   697,   701,   720,   721,   725,   726,   727,   728,
     729,   733,   734,   738,   742,   743,   744,   748,   749,   750,
     754,   755,   760,   761,   762,   763,   770,   771,   772,   773,
     774,   775,   779,   780,   784,   785,   786,   790,   791,   795,
     796,   797,   801,   802,   806,   807,   808,   809,   813,   814,
     815,   816,   820,   821,   825,   826,   827,   831,   832,   836,
     840,   841,   845,   846,   853,   854,   862,   863,   864,   868,
     869,   870,   871,   872,   873,   874,   875,   876,   880,   881,
     882,   883,   887,   892,   898,   904,   905,   909,   916,   920,
     924,   925,   929,   933,   937,   938,   942,   947,   951,   952,
     956,   966,   972,   981,   985,   986,   990,   994,   995,  1000,
    1010,  1011,  1011,  1015,  1020,  1024,  1025,  1029,  1036,  1037,
    1041,  1042,  1046,  1047,  1053,  1077,  1078,  1082,  1082,  1088,
    1088,  1094,  1098,  1099,  1103,  1104,  1108,  1112,  1113,  1117,
    1118,  1122,  1123,  1124,  1125,  1129,  1133,  1137,  1145,  1146,
    1150,  1154,  1155,  1159,  1160,  1164,  1168,  1169,  1173,  1177,
    1178,  1182,  1186,  1187,  1191,  1192,  1193,  1197,  1198,  1199,
    1200,  1204,  1208,  1209,  1213,  1214,  1215,  1219,  1223,  1224,
    1228,  1229,  1230,  1231,  1232,  1233,  1234,  1238,  1242,  1243,
    1247,  1248,  1252,  1256,  1257,  1261,  1262,  1266,  1267,  1271,
    1272,  1276,  1280,  1281,  1285,  1286,  1290,  1291,  1292,  1293,
    1294,  1295,  1296,  1300,  1301,  1302,  1306,  1307,  1308,  1312,
    1313,  1314,  1315,  1316,  1317,  1318,  1319,  1320,  1321,  1325,
    1326,  1330,  1334,  1338,  1342,  1343,  1344,  1348,  1352,  1356,
    1357,  1361,  1362,  1366,  1370
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
  "compound_name", "c_name_list", "used_char", "operator_symbol",
  "indexed_comp", "value_s", "value", "selected_comp", "attribute",
  "attribute_id", "literal", "aggregate", "value_s_2", "comp_assoc",
  "expression", "logical", "short_circuit", "relation", "relational",
  "membership", "simple_expression", "unary", "adding", "term",
  "multiplying", "factor", "primary", "parenthesized_primary", "qualified",
  "allocator", "statement_s", "statement", "unlabeled", "simple_stmt",
  "compound_stmt", "null_stmt", "assign_stmt", "if_stmt", "cond_clause_s",
  "cond_clause", "cond_part", "condition", "else_opt", "case_stmt",
  "case_hdr", "alternative_s", "alternative", "loop_stmt", "label_opt",
  "loop_content", "iter_part", "reverse_opt", "basic_loop", "id_opt",
  "block", "block_decl", "$@1", "block_body", "handled_stmt_s",
  "except_handler_part_opt", "exit_stmt", "name_opt", "when_opt",
  "return_stmt", "goto_stmt", "subprog_decl", "subprog_spec", "@2", "@3",
  "designator", "formal_part_opt", "formal_part", "param_s", "param",
  "mode", "subprog_spec_is_push", "subprog_body", "procedure_call",
  "pkg_decl", "pkg_spec", "private_part", "c_id_opt", "pkg_body",
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

#define YYPACT_NINF (-563)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-357)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     794,   252,  -563,   154,    12,    13,    12,   230,  -563,   300,
    1238,  -563,  -563,   137,  -563,  -563,  -563,   904,  -563,  -563,
    -563,  -563,   236,   196,   204,  -563,  -563,  -563,    29,    12,
    -563,   322,  -563,    12,   305,   433,  -563,   359,   292,  -563,
     362,   385,    12,   174,   346,   423,   426,   292,  -563,  -563,
    -563,  -563,  -563,   126,  -563,  -563,   510,  -563,  1136,  -563,
    -563,  -563,   525,  -563,  -563,  -563,  -563,  -563,  -563,  -563,
    -563,  -563,  -563,  -563,  -563,  -563,  -563,  -563,  -563,  -563,
     410,   484,  -563,    12,    12,   686,   404,   142,   467,   474,
    -563,  -563,  -563,  -563,   475,   133,  1082,    12,   504,   475,
     312,  -563,    12,   292,  -563,  -563,   559,  -563,  -563,  -563,
    -563,  -563,  -563,  -563,     6,  -563,    12,   508,   477,   523,
     513,   559,   488,   515,   192,  1067,   568,  -563,   370,   410,
     484,  -563,  -563,   386,   520,   252,    12,    12,   287,  -563,
     528,  -563,  -563,    42,   550,  -563,  1180,   178,   591,  1172,
    -563,   549,  -563,  -563,  -563,  -563,   786,   305,   559,   629,
      71,   342,   729,    71,    12,   223,  -563,   748,   292,    63,
     628,  -563,  -563,   292,  -563,   641,   313,    52,   593,   748,
     292,   292,   748,   635,   292,   458,   606,  1067,  -563,   216,
     643,   848,  -563,  -563,  -563,  -563,  -563,  -563,  -563,  -563,
    -563,  -563,    39,  -563,  -563,  -563,  -563,  -563,  -563,  -563,
    -563,   252,   646,  1216,   650,   104,  -563,   667,   475,   711,
     475,   718,  -563,    12,  -563,   367,  -563,   292,   510,    12,
    1263,   740,  -563,   370,   756,   739,  -563,  -563,  -563,  -563,
    1266,   292,  1266,  -563,  -563,  -563,  -563,   636,  -563,  -563,
    -563,    34,  -563,   460,   444,  -563,   535,  -563,  -563,  -563,
    -563,   478,  -563,   566,  1259,   227,  -563,   777,  -563,  -563,
    -563,  -563,  -563,  -563,  -563,  -563,  -563,  -563,  -563,  -563,
     748,   769,   644,   328,   710,  -563,  -563,  1128,   565,  -563,
     780,   220,   712,   217,  -563,   714,   619,   446,  -563,   369,
     717,   559,   748,  -563,   738,   727,   806,   759,  -563,  -563,
    -563,  -563,   290,   559,   761,   556,   220,   632,  -563,  1067,
     766,  -563,   765,  -563,   332,  -563,  -563,   748,  -563,   209,
    -563,   782,  -563,  -563,   782,   484,  -563,   760,  1067,   748,
     252,   792,  -563,   510,   775,  -563,  -563,  -563,   779,   728,
     796,   814,   821,  -563,    56,    42,  -563,   559,  -563,   832,
     272,  -563,    12,  -563,  -563,   644,  -563,  -563,   807,   781,
     653,   789,   250,   748,   736,   748,   329,  -563,  -563,   629,
     805,   846,  -563,   748,   748,   748,  -563,  -563,  -563,  -563,
     836,  -563,  -563,  -563,  -563,  -563,  -563,   748,   748,  1259,
     227,  -563,  -563,  -563,  -563,  1259,  1266,   358,   833,  -563,
    -563,   800,   804,   748,   748,  -563,   748,  -563,  -563,  -563,
    -563,   873,    54,  -563,   324,   748,   748,  -563,   748,   292,
     442,  -563,  -563,  -563,  -563,  -563,  -563,  -563,  -563,  -563,
    -563,  -563,  -563,   435,  -563,   441,  -563,   748,   845,   748,
     817,   819,  -563,   748,   822,  -563,  1067,   748,   863,   813,
    -563,  -563,  -563,   502,  -563,    28,  -563,  -563,   170,  1238,
     865,  1004,   860,   826,  -563,   748,   877,  -563,  -563,   907,
     912,   913,   292,   916,   917,  -563,  -563,  -563,   862,   850,
    -563,   292,   292,   129,   854,  -563,  -563,    12,   864,   305,
    -563,   844,   629,  -563,   629,  -563,   687,  -563,   220,  -563,
    -563,   220,  -563,   666,    95,   855,  -563,  -563,  -563,  -563,
    -563,   487,  -563,   487,  -563,   164,   227,  -563,  -563,  -563,
     748,   158,  -563,  -563,  -563,   220,   576,  -563,    12,  -563,
     292,  -563,   540,   576,   220,  -563,  -563,  -563,   657,  -563,
     880,  -563,  -563,  -563,  -563,  -563,   662,  -563,   675,  -563,
     610,   292,   220,  -563,  -563,  -563,  -563,  1032,  -563,   900,
    -563,  -563,   859,   559,    58,  -563,   922,   736,  -563,  -563,
    -563,   908,  -563,  -563,   860,  1179,   252,   933,  -563,  -563,
     888,  -563,   881,  -563,   459,   585,  -563,   559,  -563,   882,
    -563,  -563,  -563,   915,   677,   748,   524,   918,   106,  -563,
    -563,    56,  -563,   748,  -563,  -563,  -563,   442,  -563,   261,
     921,   292,  -563,   748,   378,  -563,  -563,  -563,   885,   243,
    1067,   243,   886,    60,  -563,  -563,   890,   970,   920,  -563,
     898,  -563,   431,  -563,   897,  -563,   199,  -563,   901,   748,
    -563,   576,  -563,   905,    12,   906,   683,   949,  -563,  -563,
    -563,   292,  -563,   545,  -563,  -563,  -563,    69,   876,  -563,
    -563,  1067,  -563,  -563,  -563,  -563,   902,  -563,  -563,  -563,
     578,  -563,  -563,   951,  -563,   292,   936,    36,  -563,   484,
    -563,   990,  1067,   911,   919,   748,  -563,   484,   806,  -563,
    -563,  -563,   969,  -563,   925,   281,   929,   484,  -563,   736,
     181,  -563,  -563,    81,   974,  -563,  -563,   930,   261,  -563,
    -563
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     0,   364,     0,     0,     0,     0,     0,   342,     0,
       0,   343,   340,     0,   341,   346,     2,     0,   338,     9,
     344,   345,     0,     0,     0,   140,   293,   292,   291,     0,
     141,     0,   287,     0,   143,     0,     1,   305,     0,   285,
       0,     0,     0,     0,     0,     0,     0,     0,    24,   125,
     122,    11,    12,     0,    13,    14,     0,   129,     0,   126,
     128,    15,     0,   130,    16,   131,   123,    18,   326,    20,
      17,    19,   124,   394,   395,   396,   308,   336,   338,     9,
     334,   333,   300,     0,     0,     0,     0,     0,     0,     0,
     372,   365,   286,   309,   294,     0,     0,     0,     0,   294,
       0,   337,     0,     0,   391,   146,   331,   132,   136,   133,
     134,   135,   328,    21,     0,   137,     0,   140,     0,     0,
      33,   322,     0,     0,    26,     0,     0,   127,   305,   335,
     332,   339,    10,     0,   373,     0,     0,     0,   301,   362,
       0,   366,   363,     0,     0,   295,     0,     0,   311,     0,
     120,     0,   392,   142,   327,   288,     0,   144,   393,     0,
       0,     0,     0,     0,     0,     0,     3,     0,     0,     0,
      36,    34,   321,     0,    25,    27,     0,     0,     0,     0,
     278,     0,     0,     0,   278,     0,   140,     0,   228,     0,
       0,     0,   222,   224,   226,   227,   229,   230,   238,   239,
       9,   240,   270,   241,   273,   231,   232,   233,   234,   235,
     236,   267,     0,     0,     0,     0,   374,     0,   294,     0,
     294,   302,   303,     0,   330,     0,   297,     0,   316,     0,
       0,     0,   121,     0,     0,     0,   348,   349,   347,   153,
       0,     0,     0,   165,   111,   145,   163,     0,   197,   198,
     113,     0,   107,   110,   213,   164,     0,   148,   212,   217,
     151,   109,   174,   182,     0,   194,   202,   208,   216,   215,
     214,   162,   161,   160,   159,   158,   157,   154,   155,   156,
       0,   399,   213,     0,   182,   138,   139,     0,     0,     5,
     132,     7,     0,    48,   100,     0,     0,     0,    97,   319,
       0,   323,     0,   352,     0,     0,    30,    28,    29,    71,
      72,   237,     0,   279,   280,     0,   249,   250,   245,     0,
       0,   242,     0,   282,     0,   259,   225,     0,   307,     0,
     404,     0,   223,   274,   276,   254,   271,     0,     0,     0,
     267,   264,   260,     0,     0,   268,   351,   329,     0,   319,
       0,     0,   376,   304,    30,     0,   296,   290,   317,     0,
       0,   312,   313,   305,   210,   220,   221,   209,   165,     0,
       0,   151,   109,     0,     0,     0,     0,   112,   147,     0,
     177,   178,   179,     0,     0,     0,   189,   187,   191,   192,
       0,   186,   188,   190,   199,   200,   201,     0,     0,     0,
     195,   206,   207,   204,   205,     0,     0,     0,     0,   401,
     397,     0,     0,     0,     0,    46,     0,    47,    50,    49,
      35,   101,     0,    96,     0,     0,     0,   320,     0,     0,
       0,    37,    44,    64,    38,    39,    40,    66,    67,    41,
      42,    43,    45,     0,    32,     0,   325,     0,     0,     0,
       0,     0,   253,     0,     0,   284,     0,     0,     0,     0,
     248,   361,   283,     0,   219,     0,   353,   354,     0,     0,
       0,     0,     0,     0,   265,     0,     0,   306,   375,     0,
       0,     0,     0,     0,     0,   385,   386,   387,     0,     0,
     388,     0,     0,     0,     0,   299,   298,   313,     0,   314,
     310,     0,     0,   167,     0,   166,     0,   218,   173,   108,
     110,   109,    53,   213,     0,    57,   149,   180,   181,   175,
     176,    56,   193,   183,   184,   185,   196,   203,   211,   403,
       0,     0,   350,     4,     6,     8,    54,   102,     0,    98,
       0,   114,     0,    54,    65,    52,    63,    62,     0,    60,
       0,   318,     9,    84,    23,    83,     0,    76,     0,    80,
     213,     0,    31,    22,   324,   281,   277,     0,   246,     0,
     243,   360,   140,   359,     0,   357,     0,     0,   255,   272,
     263,     0,   261,   257,     0,   213,   267,   382,   384,   381,
     389,   380,     0,   367,   376,     0,   378,   377,   368,     0,
     170,   171,   172,   165,     0,     0,     0,     0,     0,    55,
      51,    30,   115,     0,    69,    68,    59,     0,    86,     0,
       0,     0,    79,     0,     0,    82,    75,    74,     0,     0,
       0,     0,     0,     0,   266,   262,     0,     0,     0,   379,
       0,   371,     0,   315,     0,   168,     0,   400,     0,     0,
      99,    54,    61,     0,     0,     0,     0,     0,     9,    90,
       9,     0,    77,     0,    81,    78,   244,     0,     0,   358,
     252,     0,   269,   383,   390,   369,     0,   169,    58,   398,
       0,    70,    95,     0,     9,     0,     0,     0,    87,    88,
      73,     0,     0,     0,     0,     0,     9,    89,    30,    85,
      91,     9,     0,   370,     0,     0,     0,    93,   402,     0,
       0,   104,    94,     0,     0,   105,     9,     0,     0,   103,
     106
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -563,  -563,   -10,  -563,   583,   -78,  -563,  -563,  -563,   -20,
    -563,  -563,  -351,  -563,  -563,  -563,  -563,  -563,  -143,  -563,
    -563,  -563,  -243,  -496,  -345,  -563,  -563,   380,  -563,  -563,
    -563,  -563,  -223,  -563,  -563,  -554,  -563,   379,  -563,  -563,
    -454,  -563,  -563,   283,  -563,  -563,   323,   878,  -563,   592,
    -563,   326,  -563,   306,  -562,   645,  -364,   668,    -4,   788,
    -563,   -24,  -563,   962,  -563,    -9,  -214,    16,    -1,  -563,
     861,   867,  -563,   526,  -235,  -563,  -563,   868,  -563,  -563,
    -563,   783,    14,  -563,  -563,   405,  -563,  -563,  -133,  -563,
    -563,  -237,  -563,   621,  -209,  -255,  -107,  -563,  -296,  -183,
    -563,  -563,  -563,  -563,  -563,  -563,  -563,   579,  -563,  -288,
    -563,  -563,  -563,  -563,  -563,  -563,  -563,  -563,  -563,  -563,
    -436,  -327,  -563,  -563,  -563,  -187,  -563,  -563,  -563,   851,
    -563,  -563,  -563,   360,    61,  -563,  -563,    23,   -83,  -563,
    -563,  -129,  -563,  -563,    35,  -563,   419,  1021,  -563,   551,
      46,  -563,   700,   703,    32,  -563,  -563,   434,     8,  -563,
    -563,  1037,   979,  1041,  -563,  -563,  -563,  -563,  -563,   726,
     432,   436,  -563,   461,  -563,  -563,  -563,   469,  -563,  -563,
    -563,  -563,   975,  -563,  -563,  -563,  -563,  -563,  -563,  -563
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     7,   188,   288,   289,    81,    50,    51,    52,    53,
     177,   306,   450,    54,   170,   300,   431,    55,   626,   417,
     418,   432,   609,   625,   250,   434,   548,   549,   435,   436,
     437,   438,   308,   309,   310,   627,   556,   557,   448,   558,
     559,   440,   553,   657,   658,   688,   659,   171,   297,   298,
     538,   660,   710,   711,   251,   252,   253,   441,   228,   148,
     149,    57,    58,    59,    60,   282,   114,   107,    31,    35,
     255,   108,   109,   256,   257,   110,   111,   275,   258,   259,
     370,   260,   261,   383,   384,   262,   397,   398,   284,   264,
     399,   265,   405,   266,   267,   268,   269,   270,   191,   192,
     193,   194,   195,   196,   197,   198,   317,   318,   319,   320,
     458,   199,   200,   468,   578,   201,   202,   340,   341,   475,
     342,   344,   203,   343,   469,   126,   204,   333,   205,   314,
     454,   206,   207,    61,    62,    99,    94,   345,   144,   145,
     225,    88,   223,    10,    63,   208,    64,    13,   231,   500,
      65,   359,   442,   443,    66,   122,    67,    68,    40,    16,
      17,    18,    80,    19,    20,   238,    69,    70,   334,   466,
     574,   575,   209,    71,    22,    91,   217,   494,   489,   490,
      23,    24,   104,    72,    73,    74,   409,   531,    75,   210
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      49,   130,    86,   495,   326,    34,    56,    77,   332,   354,
     510,   377,   369,   473,   226,   633,   155,    27,   190,    30,
      32,   584,    30,   459,    28,   292,   263,   400,    95,   106,
     512,   364,   100,   367,   307,    11,   582,   653,   121,    98,
     610,   358,   471,    82,   373,    30,    14,   615,    49,    30,
     419,   472,    11,   524,    90,   294,   433,   654,   115,    30,
     118,     9,   336,    14,   294,   449,   -92,   295,   630,   305,
     671,   132,   150,   337,   464,   162,   439,   571,     9,   692,
     190,   338,   133,    87,   190,   271,    49,    25,    44,  -289,
     163,   716,   164,    33,   158,   140,   272,   273,   385,    30,
     134,   157,   555,   572,   105,   294,   -92,   690,   348,  -289,
     339,    48,   131,   153,   263,   165,   189,    48,    30,   374,
     132,   464,   335,    86,   649,   232,   485,    25,   105,    48,
     555,   698,    30,   596,   366,   350,    49,   352,    48,    49,
     163,   214,   164,   631,   516,   374,   274,   713,   635,   296,
     254,    27,   219,   220,   631,   681,   476,   151,   218,   293,
     567,   131,   526,    30,   301,   565,   374,  -213,   293,   664,
    -213,   313,   315,   146,    29,   313,   283,   277,   189,    48,
     286,   291,   189,   290,   304,   394,   395,   396,   607,   150,
     163,   236,   164,   312,   116,   296,   316,   528,   229,   324,
     576,    38,   237,    49,    25,   105,   150,  -213,   123,   124,
     151,   714,   190,   510,   175,   380,    76,   235,   357,    97,
      49,   139,  -213,   271,   176,   327,   496,    27,   360,    25,
      36,   190,   365,    25,   272,   273,   380,    82,   254,   115,
     577,   263,   514,   416,   159,    30,   263,   381,   376,    25,
     161,   709,   521,    25,  -213,  -213,  -213,  -213,  -213,   636,
     650,   372,   653,   287,   523,   514,   380,   601,   381,   602,
     401,     1,   382,   375,   151,    92,   332,    49,   555,   512,
     678,   541,   654,    93,   274,   402,   545,    83,   332,   247,
       4,   151,   571,   382,   407,   328,   159,   159,   381,   614,
     329,   160,   161,   161,    84,    47,   380,   655,    85,    97,
     189,    48,   498,    44,   514,   451,   445,   608,    25,   105,
     403,   404,   506,   382,   611,   132,   221,    25,    26,   189,
     452,   507,  -150,    44,   668,    86,    48,   222,   381,   590,
      37,   463,   514,   271,   380,   510,   540,   706,   380,   190,
     704,   709,   190,   316,   272,   273,    27,   276,    97,    38,
       8,   499,    96,   382,   190,   254,   513,    25,   105,   263,
     254,   263,    38,   263,   380,   693,   381,     8,    30,    39,
     381,    38,   665,   424,   332,   515,   305,   508,   511,   525,
     240,    97,   303,   156,   425,   426,   702,   612,    97,    25,
     105,   382,   296,   103,   274,   382,   381,   410,    97,   247,
     427,   462,   428,   429,   103,   293,   245,    25,   105,    12,
     293,   117,   241,   242,   243,   375,   213,   291,   535,   290,
     536,   382,   239,   212,    15,   676,    12,   529,   560,   542,
     543,   112,   544,   240,   263,    38,   355,   189,   356,   430,
     189,    15,   245,    25,   105,   246,   573,   380,   247,    49,
     190,    21,   189,   562,   113,   579,   585,   316,   248,   249,
     240,   316,    97,   293,   619,   241,   242,   243,    21,    47,
     244,   550,   594,   595,   597,   332,   123,   138,   551,   381,
     514,   514,   552,   254,   380,   254,   499,   254,   119,   493,
     375,   120,   241,   242,   243,   245,    25,   105,   246,   263,
     332,   247,   101,    30,   382,   102,   546,   547,   380,   332,
     554,   248,   249,   190,   159,   422,   381,   423,   376,   125,
     161,   293,   245,    25,   105,   246,    44,   323,   247,   159,
     380,  -152,  -152,   160,   606,   161,   141,   115,   248,   249,
     381,   382,   293,   142,   115,   143,   380,   167,   189,  -150,
    -150,   190,   514,   168,   190,   128,   613,   172,   254,   385,
     173,   386,   381,   387,   388,   382,   263,   394,   395,   396,
     687,   570,   689,   154,    38,   190,   190,   166,   381,   233,
     174,   511,   380,   169,   380,   190,   375,   382,   211,   656,
     215,   691,    27,   647,    39,   389,   697,   224,    38,   132,
     227,   390,   663,   382,   585,   513,   378,   379,   705,   646,
     573,   189,   573,   707,   381,   159,   381,   651,    39,   160,
     239,   161,   375,   254,   695,   455,   159,   239,   718,   159,
     160,   240,   161,   160,   230,   161,   412,   413,   240,   382,
     302,   382,   293,   391,   392,   393,   394,   395,   396,   189,
     456,   457,   189,   680,   641,   642,   624,   656,   299,   160,
     683,   161,   311,   241,   242,   243,   293,   132,   244,   132,
     241,   242,   368,   189,   189,   244,   513,   132,   239,   325,
     159,   -54,   -54,   189,   376,   132,   161,   132,   656,   240,
     254,   123,   421,   245,    25,   105,   246,   349,   132,   247,
     245,    25,   105,   246,   321,   386,   247,   387,   388,   248,
     249,   135,   330,   511,   159,   346,   248,   249,   329,   347,
     161,   241,   242,   603,   503,   504,   244,   136,   616,   617,
     137,   240,   424,   620,   621,   305,   159,   280,   240,   389,
     376,   351,   161,   479,   480,   390,   622,   623,   645,   379,
     240,   245,    25,   105,   246,   123,   685,   247,   353,   427,
     362,   481,   482,   241,   242,   243,    29,   248,   249,   363,
     241,   242,   243,   406,   483,   244,   281,   408,   519,   520,
     414,   415,   241,   242,   243,   420,   444,   391,   392,   393,
     394,   395,   396,   245,    25,   105,   246,   447,   484,   247,
     245,    25,   105,   246,   178,   449,   247,   446,    38,   248,
     249,     1,   245,    25,   105,   246,   248,   249,   247,     1,
       2,   453,  -258,   460,   179,   470,  -258,   234,   248,   249,
       4,  -247,  -247,  -247,   461,     3,   180,  -258,     4,   178,
     181,   182,   465,   474,   477,  -258,   491,     5,   492,   183,
     478,   493,   497,   502,   501,    44,     6,  -258,   184,   179,
     505,  -258,   517,   185,   518,   522,   530,   178,  -275,   532,
     331,   180,  -258,   533,  -258,   181,   182,   537,   186,   105,
    -258,   187,   561,   569,   183,  -258,   563,   179,   564,  -258,
      44,   566,   338,   184,   580,   583,  -355,   586,   185,   180,
    -258,   587,   178,   181,   182,   551,   588,   589,  -258,  -258,
     591,   592,   183,   186,   105,   600,   187,   411,    44,   593,
    -258,   184,   179,   598,  -258,   605,   185,   618,   628,     1,
       2,  -256,   629,   632,   180,  -258,  -355,  -258,   181,   182,
     634,   186,   105,  -258,   187,     3,    44,   183,     4,   637,
     638,   643,   639,    44,   666,   670,   184,     5,   661,   672,
     178,   185,   644,   674,   673,   648,     6,   675,   677,   686,
     679,  -256,  -258,   694,   682,   684,   186,   105,  -258,   187,
     179,   696,  -258,   699,   665,   717,   534,   652,   703,  -356,
     662,   720,   180,  -258,   708,   178,   181,   182,   712,   719,
     700,  -258,   216,   701,   539,   183,   715,   486,   361,   509,
     127,    44,   278,  -258,   184,   179,   527,  -258,   279,   185,
     371,   285,   604,   178,   581,   322,   568,   180,  -258,  -356,
    -258,   181,   182,    89,   186,   105,  -258,   187,   599,   487,
     183,  -258,   488,   179,    78,  -258,    44,   129,    79,   184,
     467,   667,  -251,   640,   185,   180,  -258,   669,   178,   181,
     182,   152,     0,     0,  -258,  -258,     0,     0,   183,   186,
     105,     0,   187,    41,    44,     0,  -258,   184,   179,     0,
    -258,     0,   185,     0,     0,     0,     0,     0,     0,     0,
     180,  -258,     0,  -258,   181,   182,     0,   186,   105,  -258,
     187,     0,  -118,   183,     0,     0,    42,     1,     2,    44,
       0,     0,   184,     0,     0,     0,   103,   185,     0,    41,
       0,     0,     0,   147,    44,  -118,     4,    41,  -258,     0,
       0,     0,   186,   105,     0,   187,    45,  -116,     0,     0,
      46,    47,     0,     0,     0,  -117,     0,    48,  -116,     0,
       0,     0,    42,     1,     2,     0,  -117,     0,     0,     0,
      42,     1,     2,    41,     0,     0,     0,     0,     0,    43,
      44,    41,     4,     0,     0,     0,     0,    43,    44,     0,
       4,   411,    45,     0,     0,     0,    46,    47,     0,  -116,
      45,     0,  -119,    48,    46,    47,    42,     1,     2,     0,
    -116,    48,     0,     0,    42,     1,     2,    41,     0,     0,
       0,   -54,     0,   147,    44,  -119,     4,     0,     0,     0,
       0,    43,    44,     0,     4,   375,    45,     0,     0,    41,
      46,    47,     0,     0,    45,     0,  -118,    48,    46,    47,
      42,     1,     2,     0,     0,    48,     0,  -116,     0,   159,
     -54,   -54,     0,   376,    41,   161,     0,   147,    44,  -118,
       4,   240,    42,     1,     2,     0,     0,     0,     0,     0,
      45,     0,     0,     0,    46,    47,     0,     0,     0,    43,
      44,    48,     4,  -118,     0,     0,     0,    42,     1,     2,
       0,     0,    45,   241,   242,   243,    46,    47,     0,     0,
     241,     0,   243,    48,   147,    44,     0,     4,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    45,     0,     0,
       0,    46,    47,   245,    25,   105,   246,     0,    48,   247,
     245,    25,   105,   246,     0,     0,   247
};

static const yytype_int16 yycheck[] =
{
      10,    79,    22,   354,   187,     6,    10,    17,   191,   223,
     374,   254,   247,   340,   143,   577,    99,     1,   125,     3,
       4,   475,     6,   319,     1,   168,   159,   264,    29,    38,
     375,   240,    33,   242,   177,     0,   472,     1,    47,    31,
     536,   228,   338,     1,    10,    29,     0,   543,    58,    33,
     293,   339,    17,   398,    22,     1,   299,    21,    42,    43,
      44,     0,    23,    17,     1,     9,    30,     4,    10,    17,
      10,    81,    96,    34,   329,    69,   299,    49,    17,    10,
     187,    42,    83,    22,   191,    14,    96,    75,    52,    60,
      84,    10,    86,    80,   103,    87,    25,    26,     3,    83,
      84,   102,   447,    75,    76,     1,    70,   661,     4,    80,
      71,    75,    80,    97,   247,   116,   125,    75,   102,    85,
     130,   376,   200,   143,    18,   149,   349,    75,    76,    75,
     475,   685,   116,     4,   241,   218,   146,   220,    75,   149,
      84,   133,    86,    85,   379,    85,    75,   709,   584,   169,
     159,   135,   136,   137,    85,   651,   343,    96,   135,   168,
     456,   129,   399,   147,   173,   453,    85,     3,   177,   623,
       6,   180,   181,    40,    20,   184,   162,   161,   187,    75,
     164,   167,   191,   167,   176,    90,    91,    92,    30,   213,
      84,   156,    86,   179,    20,   215,   182,   406,    20,   185,
      30,    59,   156,   213,    75,    76,   230,    43,    82,    83,
     149,    30,   319,   577,    22,    16,    79,   156,   227,    86,
     230,    79,    58,    14,    32,     9,   355,   211,   229,    75,
       0,   338,   241,    75,    25,    26,    16,     1,   247,   223,
      70,   374,   375,    26,    80,   229,   379,    48,    84,    75,
      86,    70,   385,    75,    90,    91,    92,    93,    94,   586,
     611,   247,     1,    40,   397,   398,    16,   502,    48,   504,
      43,    35,    73,    56,   213,    79,   459,   287,   623,   624,
      81,   424,    21,    79,    75,    58,   429,    51,   471,    80,
      54,   230,    49,    73,   280,    79,    80,    80,    48,   542,
      84,    84,    86,    86,    68,    69,    16,    46,    72,    86,
     319,    75,    40,    52,   447,   307,   302,   531,    75,    76,
      93,    94,    72,    73,   538,   335,    39,    75,    76,   338,
      40,    81,    82,    52,   630,   355,    75,    50,    48,   482,
      40,   327,   475,    14,    16,   709,    22,   698,    16,   456,
     695,    70,   459,   339,    25,    26,   340,    15,    86,    59,
       0,   362,    40,    73,   471,   374,   375,    75,    76,   502,
     379,   504,    59,   506,    16,   671,    48,    17,   362,    79,
      48,    59,     4,    14,   567,    56,    17,   373,   374,   398,
      12,    86,    79,    81,    25,    26,   692,   540,    86,    75,
      76,    73,   422,    44,    75,    73,    48,    79,    86,    80,
      41,    79,    43,    44,    44,   424,    74,    75,    76,     0,
     429,    75,    44,    45,    46,    56,    40,   413,   414,   413,
     416,    73,     1,    63,     0,     4,    17,    79,   447,   425,
     426,    79,   428,    12,   577,    59,    79,   456,    81,    80,
     459,    17,    74,    75,    76,    77,   465,    16,    80,   469,
     567,     0,   471,   449,    79,   469,   475,   453,    90,    91,
      12,   457,    86,   482,   552,    44,    45,    46,    17,    69,
      49,    46,   491,   492,   493,   668,    82,    83,    53,    48,
     623,   624,    57,   502,    16,   504,   497,   506,    75,    40,
      56,    75,    44,    45,    46,    74,    75,    76,    77,   642,
     693,    80,    79,   497,    73,    82,    74,    75,    16,   702,
      79,    90,    91,   630,    80,    79,    48,    81,    84,    19,
      86,   540,    74,    75,    76,    77,    52,    79,    80,    80,
      16,    81,    82,    84,   530,    86,    79,   531,    90,    91,
      48,    73,   561,    79,   538,    80,    16,    80,   567,    81,
      82,   668,   695,    40,   671,    40,    26,    79,   577,     3,
      82,     5,    48,     7,     8,    73,   709,    90,    91,    92,
     658,    79,   660,    79,    59,   692,   693,    79,    48,    40,
      75,   577,    16,    80,    16,   702,    56,    73,    30,   619,
      80,    56,   586,    79,    79,    39,   684,    79,    59,   619,
      60,    45,   621,    73,   623,   624,    81,    82,   696,   605,
     629,   630,   631,   701,    48,    80,    48,   613,    79,    84,
       1,    86,    56,   642,    56,    79,    80,     1,   716,    80,
      84,    12,    86,    84,    53,    86,    81,    82,    12,    73,
       9,    73,   661,    87,    88,    89,    90,    91,    92,   668,
      28,    29,   671,   649,    79,    80,    56,   687,    40,    84,
     654,    86,    79,    44,    45,    46,   685,   687,    49,   689,
      44,    45,    46,   692,   693,    49,   695,   697,     1,    83,
      80,    81,    82,   702,    84,   705,    86,   707,   718,    12,
     709,    82,    83,    74,    75,    76,    77,    40,   718,    80,
      74,    75,    76,    77,    79,     5,    80,     7,     8,    90,
      91,    35,    79,   709,    80,    79,    90,    91,    84,    79,
      86,    44,    45,    46,    81,    82,    49,    51,    81,    82,
      54,    12,    14,    81,    82,    17,    80,    18,    12,    39,
      84,    40,    86,    25,    26,    45,    81,    82,    81,    82,
      12,    74,    75,    76,    77,    82,    83,    80,    50,    41,
      30,    43,    44,    44,    45,    46,    20,    90,    91,    40,
      44,    45,    46,     6,    56,    49,    57,    18,   383,   384,
      10,    79,    44,    45,    46,    81,    79,    87,    88,    89,
      90,    91,    92,    74,    75,    76,    77,    80,    80,    80,
      74,    75,    76,    77,     1,     9,    80,    79,    59,    90,
      91,    35,    74,    75,    76,    77,    90,    91,    80,    35,
      36,    70,    19,    67,    21,    75,    23,    51,    90,    91,
      54,    28,    29,    30,    79,    51,    33,    34,    54,     1,
      37,    38,    70,    61,    79,    42,    60,    63,    44,    46,
      81,    40,    30,    82,    57,    52,    72,    19,    55,    21,
      81,    23,    67,    60,    28,    39,    43,     1,    30,    79,
      32,    33,    34,    79,    71,    37,    38,    14,    75,    76,
      42,    78,    47,    30,    46,    19,    79,    21,    79,    23,
      52,    79,    42,    55,    39,    79,    30,    30,    60,    33,
      34,     4,     1,    37,    38,    53,     4,     4,    42,    71,
       4,     4,    46,    75,    76,    81,    78,    63,    52,    79,
      19,    55,    21,    79,    23,    80,    60,    57,    38,    35,
      36,    30,    83,    21,    33,    34,    70,    71,    37,    38,
      42,    75,    76,    42,    78,    51,    52,    46,    54,    26,
      72,    79,    81,    52,    79,    79,    55,    63,    47,    79,
       1,    60,    57,    53,     4,    57,    72,    79,    81,    30,
      79,    70,    71,    81,    79,    79,    75,    76,    19,    78,
      21,    40,    23,    57,     4,    21,   413,   617,    79,    30,
     621,   718,    33,    34,    79,     1,    37,    38,    79,    79,
     687,    42,   134,   687,   422,    46,   710,   349,   230,   374,
      58,    52,   161,    19,    55,    21,   405,    23,   161,    60,
     247,   163,   506,     1,    30,   184,   457,    33,    34,    70,
      71,    37,    38,    22,    75,    76,    42,    78,   497,   349,
      46,    19,   349,    21,    17,    23,    52,    78,    17,    55,
     334,   629,    30,   594,    60,    33,    34,   631,     1,    37,
      38,    96,    -1,    -1,    42,    71,    -1,    -1,    46,    75,
      76,    -1,    78,     1,    52,    -1,    19,    55,    21,    -1,
      23,    -1,    60,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      33,    34,    -1,    71,    37,    38,    -1,    75,    76,    42,
      78,    -1,    30,    46,    -1,    -1,    34,    35,    36,    52,
      -1,    -1,    55,    -1,    -1,    -1,    44,    60,    -1,     1,
      -1,    -1,    -1,    51,    52,    53,    54,     1,    71,    -1,
      -1,    -1,    75,    76,    -1,    78,    64,    19,    -1,    -1,
      68,    69,    -1,    -1,    -1,    19,    -1,    75,    30,    -1,
      -1,    -1,    34,    35,    36,    -1,    30,    -1,    -1,    -1,
      34,    35,    36,     1,    -1,    -1,    -1,    -1,    -1,    51,
      52,     1,    54,    -1,    -1,    -1,    -1,    51,    52,    -1,
      54,    63,    64,    -1,    -1,    -1,    68,    69,    -1,    19,
      64,    -1,    30,    75,    68,    69,    34,    35,    36,    -1,
      30,    75,    -1,    -1,    34,    35,    36,     1,    -1,    -1,
      -1,    42,    -1,    51,    52,    53,    54,    -1,    -1,    -1,
      -1,    51,    52,    -1,    54,    56,    64,    -1,    -1,     1,
      68,    69,    -1,    -1,    64,    -1,    30,    75,    68,    69,
      34,    35,    36,    -1,    -1,    75,    -1,    19,    -1,    80,
      81,    82,    -1,    84,     1,    86,    -1,    51,    52,    53,
      54,    12,    34,    35,    36,    -1,    -1,    -1,    -1,    -1,
      64,    -1,    -1,    -1,    68,    69,    -1,    -1,    -1,    51,
      52,    75,    54,    30,    -1,    -1,    -1,    34,    35,    36,
      -1,    -1,    64,    44,    45,    46,    68,    69,    -1,    -1,
      44,    -1,    46,    75,    51,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,
      -1,    68,    69,    74,    75,    76,    77,    -1,    75,    80,
      74,    75,    76,    77,    -1,    -1,    80
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    35,    36,    51,    54,    63,    72,    96,   228,   229,
     238,   239,   241,   242,   245,   252,   254,   255,   256,   258,
     259,   268,   269,   275,   276,    75,    76,   162,   232,    20,
     162,   163,   162,    80,   163,   164,     0,    40,    59,    79,
     253,     1,    34,    51,    52,    64,    68,    69,    75,    97,
     101,   102,   103,   104,   108,   112,   153,   156,   157,   158,
     159,   228,   229,   239,   241,   245,   249,   251,   252,   261,
     262,   268,   278,   279,   280,   283,    79,    97,   256,   258,
     257,   100,     1,    51,    68,    72,   104,   229,   236,   242,
     249,   270,    79,    79,   231,   163,    40,    86,   253,   230,
     163,    79,    82,    44,   277,    76,   160,   162,   166,   167,
     170,   171,    79,    79,   161,   162,    20,    75,   162,    75,
      75,   160,   250,    82,    83,    19,   220,   158,    40,   257,
     100,   249,    97,   163,   162,    35,    51,    54,    83,    79,
     253,    79,    79,    80,   233,   234,    40,    51,   154,   155,
     156,   229,   277,   162,    79,   233,    81,   163,   160,    80,
      84,    86,    69,    84,    86,   163,    79,    80,    40,    80,
     109,   142,    79,    82,    75,    22,    32,   105,     1,    21,
      33,    37,    38,    46,    55,    60,    75,    78,    97,   160,
     191,   193,   194,   195,   196,   197,   198,   199,   200,   206,
     207,   210,   211,   217,   221,   223,   226,   227,   240,   267,
     284,    30,    63,    40,   253,    80,   142,   271,   232,   162,
     162,    39,    50,   237,    79,   235,   236,    60,   153,    20,
      53,   243,   156,    40,    51,   229,   239,   245,   260,     1,
      12,    44,    45,    46,    49,    74,    77,    80,    90,    91,
     119,   149,   150,   151,   160,   165,   168,   169,   173,   174,
     176,   177,   180,   183,   184,   186,   188,   189,   190,   191,
     192,    14,    25,    26,    75,   172,    15,   162,   165,   166,
      18,    57,   160,   177,   183,   172,   162,    40,    98,    99,
     162,   177,   113,   160,     1,     4,   104,   143,   144,    40,
     110,   160,     9,    79,   253,    17,   106,   113,   127,   128,
     129,    79,   177,   160,   224,   160,   177,   201,   202,   203,
     204,    79,   224,    79,   177,    83,   194,     9,    79,    84,
      79,    32,   194,   222,   263,   100,    23,    34,    42,    71,
     212,   213,   215,   218,   216,   232,    79,    79,     4,    40,
     233,    40,   233,    50,   161,    79,    81,   160,   220,   246,
     163,   154,    30,    40,   189,   160,   191,   189,    46,   169,
     175,   176,   177,    10,    85,    56,    84,   117,    81,    82,
      16,    48,    73,   178,   179,     3,     5,     7,     8,    39,
      45,    87,    88,    89,    90,    91,    92,   181,   182,   185,
     186,    43,    58,    93,    94,   187,     6,   177,    18,   281,
      79,    63,    81,    82,    10,    79,    26,   114,   115,   117,
      81,    83,    79,    81,    14,    25,    26,    41,    43,    44,
      80,   111,   116,   117,   120,   123,   124,   125,   126,   127,
     136,   152,   247,   248,    79,   177,    79,    80,   133,     9,
     107,   253,    40,    70,   225,    79,    28,    29,   205,   193,
      67,    79,    79,   177,   190,    70,   264,   264,   208,   219,
      75,   193,   204,   216,    61,   214,   220,    79,    81,    25,
      26,    43,    44,    56,    80,   127,   152,   247,   248,   273,
     274,    60,    44,    40,   272,   107,   236,    30,    40,   163,
     244,    57,    82,    81,    82,    81,    72,    81,   177,   150,
     151,   177,   119,   160,   183,    56,   169,    67,    28,   180,
     180,   183,    39,   183,   119,   160,   186,   188,   189,    79,
      43,   282,    79,    79,    99,   177,   177,    14,   145,   144,
      22,   113,   177,   177,   177,   113,    74,    75,   121,   122,
      46,    53,    57,   137,    79,   119,   131,   132,   134,   135,
     160,    47,   177,    79,    79,   204,    79,   193,   202,    30,
      79,    49,    75,   160,   265,   266,    30,    70,   209,   153,
      39,    30,   215,    79,   135,   160,    30,     4,     4,     4,
     113,     4,     4,    79,   160,   160,     4,   160,    79,   244,
      81,   169,   169,    46,   168,    80,   177,    30,   161,   117,
     118,   161,   113,    26,   117,   118,    81,    82,    57,   100,
      81,    82,    81,    82,    56,   118,   113,   130,    38,    83,
      10,    85,    21,   149,    42,   215,   216,    26,    72,    81,
     272,    79,    80,    79,    57,    81,   177,    79,    57,    18,
     107,   177,   122,     1,    21,    46,   104,   138,   139,   141,
     146,    47,   132,   160,   135,     4,    79,   265,   193,   266,
      79,    10,    79,     4,    53,    79,     4,    81,    81,    79,
     177,   118,    79,   162,    79,    83,    30,   100,   140,   100,
     130,    56,    10,   193,    81,    56,    40,   100,   130,    57,
     141,   146,   193,    79,   119,   100,   107,   100,    79,    70,
     147,   148,    79,   149,    30,   148,    10,    21,   100,    79,
     138
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
     162,   163,   163,   164,   164,   165,   166,   167,   168,   168,
     169,   169,   169,   169,   170,   170,   170,   170,   171,   172,
     172,   172,   172,   173,   173,   173,   174,   174,   174,   174,
     174,   175,   175,   176,   177,   177,   177,   178,   178,   178,
     179,   179,   180,   180,   180,   180,   181,   181,   181,   181,
     181,   181,   182,   182,   183,   183,   183,   184,   184,   185,
     185,   185,   186,   186,   187,   187,   187,   187,   188,   188,
     188,   188,   189,   189,   189,   189,   189,   190,   190,   191,
     192,   192,   193,   193,   194,   194,   195,   195,   195,   196,
     196,   196,   196,   196,   196,   196,   196,   196,   197,   197,
     197,   197,   198,   199,   200,   201,   201,   202,   203,   204,
     205,   205,   206,   207,   208,   208,   209,   210,   211,   211,
     212,   212,   212,   213,   214,   214,   215,   216,   216,   217,
     218,   219,   218,   220,   221,   222,   222,   223,   224,   224,
     225,   225,   226,   226,   227,   228,   228,   230,   229,   231,
     229,   229,   232,   232,   233,   233,   234,   235,   235,   236,
     236,   237,   237,   237,   237,   238,   239,   240,   241,   241,
     242,   243,   243,   244,   244,   245,   246,   246,   247,   248,
     248,   249,   250,   250,   251,   251,   251,   252,   252,   252,
     252,   253,   254,   254,   255,   255,   255,   256,   257,   257,
     258,   258,   258,   258,   258,   258,   258,   259,   260,   260,
     261,   261,   262,   263,   263,   264,   264,   265,   265,   266,
     266,   267,   268,   268,   269,   269,   270,   270,   270,   270,
     270,   270,   270,   271,   271,   271,   272,   272,   272,   273,
     273,   273,   273,   273,   273,   273,   273,   273,   273,   274,
     274,   275,   276,   277,   278,   278,   278,   279,   280,   281,
     281,   282,   282,   283,   284
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
       1,     1,     3,     1,     3,     1,     1,     4,     1,     3,
       1,     1,     1,     1,     3,     3,     3,     3,     3,     1,
       1,     1,     1,     1,     1,     1,     3,     3,     5,     6,
       4,     3,     3,     3,     1,     3,     3,     1,     1,     1,
       2,     2,     1,     3,     3,     3,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     2,     3,     1,     1,     1,
       1,     1,     1,     3,     1,     1,     1,     1,     1,     2,
       2,     3,     1,     1,     1,     1,     1,     1,     3,     3,
       2,     2,     1,     2,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     2,     4,     6,     1,     3,     2,     2,     1,
       0,     2,     6,     3,     0,     2,     4,     4,     0,     2,
       1,     3,     4,     3,     0,     1,     4,     0,     1,     6,
       0,     0,     3,     2,     2,     0,     1,     4,     0,     1,
       0,     2,     2,     3,     3,     2,     2,     0,     4,     0,
       6,     2,     1,     1,     0,     1,     3,     1,     3,     5,
       1,     0,     1,     1,     2,     2,     6,     2,     2,     2,
       7,     0,     2,     0,     1,     9,     0,     1,     2,     0,
       1,     3,     1,     3,     6,     5,     1,     4,     3,     5,
       4,     2,     3,     2,     2,     3,     2,     3,     0,     2,
       1,     1,     1,     1,     1,     1,     1,     5,     1,     1,
       6,     4,     4,     2,     2,     4,     6,     1,     3,     1,
       1,     3,     3,     3,     1,     2,     2,     6,     6,     8,
      10,     7,     1,     0,     1,     3,     0,     2,     2,     3,
       2,     2,     2,     4,     2,     1,     1,     1,     1,     2,
       4,     3,     4,     2,     1,     1,     1,     5,     9,     0,
       4,     0,     7,     6,     2
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
#line 209 "grammar83.y"
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

#line 2498 "grammar83.tab.c"

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
  case 22: /* object_decl: def_id_s ':' object_qualifier_opt object_subtype_def init_opt ';'  */
#line 261 "grammar83.y"
                                                                      {
        TypeDecl* type_decl = find_type_decl(context, (yyvsp[-2].str_token));
        if(!type_decl) {
            error_print((yyloc), "Unknown type: %s", ST((yyvsp[-2].str_token)));
            error_exit();
        }

        uint32_t name_count = array_StringToken_size(&(yyvsp[-5].str_token_array));
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
            push_declaration(context, &decl->base);
        }
    }
#line 2739 "grammar83.tab.c"
    break;

  case 23: /* number_decl: def_id_s ':' CONSTANT IS_ASSIGNED expression ';'  */
#line 285 "grammar83.y"
                                                     {
        uint32_t name_count = array_StringToken_size(&(yyvsp[-5].str_token_array));
        for(uint32_t i = 0; i < name_count; ++i) {
            ObjectDecl* decl = create_object_decl((yyvsp[-5].str_token_array).data[i], (yyloc));
            check_for_redefinition(context, decl->name, (yyloc));
            decl->is_constant = true;
            decl->type = &universal_int_type;
            decl->init_expr = (yyvsp[-1].expr);
            push_declaration(context, &decl->base);
        }
    }
#line 2755 "grammar83.tab.c"
    break;

  case 24: /* def_id_s: identifier  */
#line 298 "grammar83.y"
               {
        array_StringToken_init(&(yyval.str_token_array));
        array_StringToken_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2764 "grammar83.tab.c"
    break;

  case 25: /* def_id_s: def_id_s ',' identifier  */
#line 302 "grammar83.y"
                            { array_StringToken_append(&(yyval.str_token_array), (yyvsp[0].str_token)); }
#line 2770 "grammar83.tab.c"
    break;

  case 26: /* object_qualifier_opt: %empty  */
#line 307 "grammar83.y"
             { (yyval.bool_) = false; }
#line 2776 "grammar83.tab.c"
    break;

  case 27: /* object_qualifier_opt: CONSTANT  */
#line 308 "grammar83.y"
             { (yyval.bool_) = true; }
#line 2782 "grammar83.tab.c"
    break;

  case 30: /* init_opt: %empty  */
#line 317 "grammar83.y"
                           { (yyval.expr) = NULL; }
#line 2788 "grammar83.tab.c"
    break;

  case 31: /* init_opt: IS_ASSIGNED expression  */
#line 318 "grammar83.y"
                           { (yyval.expr) = (yyvsp[0].expr); }
#line 2794 "grammar83.tab.c"
    break;

  case 32: /* type_decl: TYPE identifier discrim_part_opt type_completion ';'  */
#line 322 "grammar83.y"
                                                         {
        // TODO: discriminant
        TypeDecl* decl = (yyvsp[-1].type_decl);
        // Note: decl->base.kind is set by the specific type_completion
        decl->base.line_num = (yyloc);
        decl->name = (yyvsp[-3].str_token);
        check_for_redefinition(context, decl->name, (yyloc));
        push_declaration(context, &decl->base);
    }
#line 2808 "grammar83.tab.c"
    break;

  case 37: /* type_completion: IS type_def  */
#line 341 "grammar83.y"
                { (yyval.type_decl) = (yyvsp[0].type_decl); }
#line 2814 "grammar83.tab.c"
    break;

  case 46: /* subtype_decl: SUBTYPE identifier IS subtype_ind ';'  */
#line 356 "grammar83.y"
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
    }
#line 2832 "grammar83.tab.c"
    break;

  case 47: /* subtype_ind: name constraint  */
#line 372 "grammar83.y"
                    {
        // TODO: propagate constraint somehow
        (yyval.str_token) = (yyvsp[-1].name).name;
    }
#line 2841 "grammar83.tab.c"
    break;

  case 48: /* subtype_ind: name  */
#line 376 "grammar83.y"
         { (yyval.str_token) = (yyvsp[0].name).name; }
#line 2847 "grammar83.tab.c"
    break;

  case 52: /* derived_type: NEW subtype_ind  */
#line 389 "grammar83.y"
                    {
        (yyval.type_decl) = create_type_decl(TYPE_DERIVED);
        TypeDecl* base_type = find_type_decl(context, (yyvsp[0].str_token));
        if(!base_type) {
            error_print((yyloc), "Unknown base type: %s", ST((yyvsp[0].str_token)));
            error_exit();
        }
        (yyval.type_decl)->u.subtype.base = base_type;
    }
#line 2861 "grammar83.tab.c"
    break;

  case 53: /* range_constraint: RANGE range  */
#line 400 "grammar83.y"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 2867 "grammar83.tab.c"
    break;

  case 54: /* range_constr_opt: %empty  */
#line 404 "grammar83.y"
           { (yyval.expr) = NULL; }
#line 2873 "grammar83.tab.c"
    break;

  case 56: /* range: simple_expression DOT_DOT simple_expression  */
#line 409 "grammar83.y"
                                                { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_RANGE, (yyvsp[0].expr)); }
#line 2879 "grammar83.tab.c"
    break;

  case 59: /* enumeration_type: '(' enum_id_s ')'  */
#line 415 "grammar83.y"
                      {
        (yyval.type_decl) = create_type_decl(TYPE_ENUM);
        (yyval.type_decl)->u.enum_.literals = (yyvsp[-1].expr_array).data;
        (yyval.type_decl)->u.enum_.literal_count = array_ExprPtr_size(&(yyvsp[-1].expr_array));
        // TODO: add all enum literals into symbol table scope
    }
#line 2890 "grammar83.tab.c"
    break;

  case 60: /* enum_id_s: enum_id  */
#line 423 "grammar83.y"
            {
        array_ExprPtr_init(&(yyval.expr_array));
        array_ExprPtr_append(&(yyval.expr_array), (yyvsp[0].expr));
    }
#line 2899 "grammar83.tab.c"
    break;

  case 61: /* enum_id_s: enum_id_s ',' enum_id  */
#line 427 "grammar83.y"
                          { array_ExprPtr_append(&(yyval.expr_array), (yyvsp[0].expr)); }
#line 2905 "grammar83.tab.c"
    break;

  case 62: /* enum_id: identifier  */
#line 431 "grammar83.y"
               {
        (yyval.expr) = create_expr(EXPR_NAME, (yyloc));
        (yyval.expr)->u.name.name = (yyvsp[0].str_token);
    }
#line 2914 "grammar83.tab.c"
    break;

  case 63: /* enum_id: char_lit  */
#line 435 "grammar83.y"
             {
        (yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
        (yyval.expr)->u.char_lit = (yyvsp[0].c);
    }
#line 2923 "grammar83.tab.c"
    break;

  case 64: /* integer_type: range_constraint  */
#line 441 "grammar83.y"
                     {
        (yyval.type_decl) = create_type_decl(TYPE_INTEGER);
        (yyval.type_decl)->u.int_.range = (yyvsp[0].expr);
    }
#line 2932 "grammar83.tab.c"
    break;

  case 132: /* name: simple_name  */
#line 623 "grammar83.y"
                {
        memset(&(yyval.name), 0, sizeof((yyval.name)));
        (yyval.name).name = (yyvsp[0].str_token);
    }
#line 2941 "grammar83.tab.c"
    break;

  case 136: /* name: operator_symbol  */
#line 630 "grammar83.y"
                    {
        memset(&(yyval.name), 0, sizeof((yyval.name)));
        (yyval.name).name = (yyvsp[0].str_token);
        //TODO: lookup operator, determine its arity, and allocate args array
    }
#line 2951 "grammar83.tab.c"
    break;

  case 145: /* used_char: char_lit  */
#line 657 "grammar83.y"
             {
        (yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
        (yyval.expr)->u.char_lit = (yyvsp[0].c);
    }
#line 2960 "grammar83.tab.c"
    break;

  case 163: /* literal: numeric_lit  */
#line 701 "grammar83.y"
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
#line 2984 "grammar83.tab.c"
    break;

  case 175: /* expression: expression logical relation  */
#line 743 "grammar83.y"
                                      { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 2990 "grammar83.tab.c"
    break;

  case 176: /* expression: expression short_circuit relation  */
#line 744 "grammar83.y"
                                      { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 2996 "grammar83.tab.c"
    break;

  case 177: /* logical: AND  */
#line 748 "grammar83.y"
        { (yyval.unary_op) = OP_AND; }
#line 3002 "grammar83.tab.c"
    break;

  case 178: /* logical: OR  */
#line 749 "grammar83.y"
        { (yyval.unary_op) = OP_OR; }
#line 3008 "grammar83.tab.c"
    break;

  case 179: /* logical: XOR  */
#line 750 "grammar83.y"
        { (yyval.unary_op) = OP_XOR; }
#line 3014 "grammar83.tab.c"
    break;

  case 180: /* short_circuit: AND THEN  */
#line 754 "grammar83.y"
               { (yyval.unary_op) = OP_AND_THEN; }
#line 3020 "grammar83.tab.c"
    break;

  case 181: /* short_circuit: OR ELSE  */
#line 755 "grammar83.y"
               { (yyval.unary_op) = OP_OR_ELSE; }
#line 3026 "grammar83.tab.c"
    break;

  case 183: /* relation: simple_expression relational simple_expression  */
#line 761 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3032 "grammar83.tab.c"
    break;

  case 184: /* relation: simple_expression membership range  */
#line 762 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3038 "grammar83.tab.c"
    break;

  case 185: /* relation: simple_expression membership name  */
#line 763 "grammar83.y"
                                                   {
        Expression* right = create_expr(EXPR_NAME, (yylsp[0]));
        right->u.name = (yyvsp[0].name);
        (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), right);
    }
#line 3048 "grammar83.tab.c"
    break;

  case 186: /* relational: '='  */
#line 770 "grammar83.y"
          { (yyval.unary_op) = OP_EQ; }
#line 3054 "grammar83.tab.c"
    break;

  case 187: /* relational: NE  */
#line 771 "grammar83.y"
          { (yyval.unary_op) = OP_NEQ; }
#line 3060 "grammar83.tab.c"
    break;

  case 188: /* relational: '<'  */
#line 772 "grammar83.y"
          { (yyval.unary_op) = OP_LT; }
#line 3066 "grammar83.tab.c"
    break;

  case 189: /* relational: LT_EQ  */
#line 773 "grammar83.y"
          { (yyval.unary_op) = OP_LTE; }
#line 3072 "grammar83.tab.c"
    break;

  case 190: /* relational: '>'  */
#line 774 "grammar83.y"
          { (yyval.unary_op) = OP_GT; }
#line 3078 "grammar83.tab.c"
    break;

  case 191: /* relational: GE  */
#line 775 "grammar83.y"
          { (yyval.unary_op) = OP_GTE; }
#line 3084 "grammar83.tab.c"
    break;

  case 192: /* membership: IN  */
#line 779 "grammar83.y"
           { (yyval.unary_op) = OP_IN; }
#line 3090 "grammar83.tab.c"
    break;

  case 193: /* membership: NOT IN  */
#line 780 "grammar83.y"
           { (yyval.unary_op) = OP_NOT_IN; }
#line 3096 "grammar83.tab.c"
    break;

  case 195: /* simple_expression: unary term  */
#line 785 "grammar83.y"
                                  { (yyval.expr) = make_unary_expr((yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3102 "grammar83.tab.c"
    break;

  case 196: /* simple_expression: simple_expression adding term  */
#line 786 "grammar83.y"
                                  { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3108 "grammar83.tab.c"
    break;

  case 197: /* unary: '+'  */
#line 790 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_PLUS; }
#line 3114 "grammar83.tab.c"
    break;

  case 198: /* unary: '-'  */
#line 791 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_MINUS; }
#line 3120 "grammar83.tab.c"
    break;

  case 199: /* adding: '+'  */
#line 795 "grammar83.y"
        { (yyval.unary_op) = OP_PLUS; }
#line 3126 "grammar83.tab.c"
    break;

  case 200: /* adding: '-'  */
#line 796 "grammar83.y"
        { (yyval.unary_op) = OP_MINUS; }
#line 3132 "grammar83.tab.c"
    break;

  case 201: /* adding: '&'  */
#line 797 "grammar83.y"
        { (yyval.unary_op) = OP_AMP; }
#line 3138 "grammar83.tab.c"
    break;

  case 203: /* term: term multiplying factor  */
#line 802 "grammar83.y"
                            { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3144 "grammar83.tab.c"
    break;

  case 204: /* multiplying: '*'  */
#line 806 "grammar83.y"
        { (yyval.unary_op) = OP_MULT; }
#line 3150 "grammar83.tab.c"
    break;

  case 205: /* multiplying: '/'  */
#line 807 "grammar83.y"
        { (yyval.unary_op) = OP_DIVIDE; }
#line 3156 "grammar83.tab.c"
    break;

  case 206: /* multiplying: MOD  */
#line 808 "grammar83.y"
        { (yyval.unary_op) = OP_MOD; }
#line 3162 "grammar83.tab.c"
    break;

  case 207: /* multiplying: REM  */
#line 809 "grammar83.y"
        { (yyval.unary_op) = OP_REM; }
#line 3168 "grammar83.tab.c"
    break;

  case 209: /* factor: NOT primary  */
#line 814 "grammar83.y"
                          { (yyval.expr) = make_unary_expr(OP_NOT, (yyvsp[0].expr)); }
#line 3174 "grammar83.tab.c"
    break;

  case 210: /* factor: ABS primary  */
#line 815 "grammar83.y"
                          { (yyval.expr) = make_unary_expr(OP_ABS, (yyvsp[0].expr)); }
#line 3180 "grammar83.tab.c"
    break;

  case 211: /* factor: primary EXPON primary  */
#line 816 "grammar83.y"
                          { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_EXP, (yyvsp[0].expr)); }
#line 3186 "grammar83.tab.c"
    break;

  case 213: /* primary: name  */
#line 821 "grammar83.y"
         {
        (yyval.expr) = create_expr(EXPR_NAME, (yyloc));
        (yyval.expr)->u.name = (yyvsp[0].name);
    }
#line 3195 "grammar83.tab.c"
    break;

  case 218: /* parenthesized_primary: '(' expression ')'  */
#line 832 "grammar83.y"
                       { (yyval.expr) = (yyvsp[-1].expr); }
#line 3201 "grammar83.tab.c"
    break;

  case 223: /* statement_s: statement_s statement  */
#line 846 "grammar83.y"
                          {
        (yyval.stmt) = (yyvsp[-1].stmt);
        (yyval.stmt)->next = (yyvsp[0].stmt);
        (yyval.stmt) = (yyvsp[0].stmt);
    }
#line 3211 "grammar83.tab.c"
    break;

  case 225: /* statement: goto_label statement  */
#line 854 "grammar83.y"
                         {
        check_for_redefinition(context, (yyvsp[-1].str_token), (yylsp[-1]));
        LabelDecl* label = create_label((yyvsp[-1].str_token), (yylsp[-1]));
        push_declaration(context, (Declaration*)label);
        (yyval.stmt) = (yyvsp[0].stmt);
    }
#line 3222 "grammar83.tab.c"
    break;

  case 242: /* null_stmt: NuLL ';'  */
#line 887 "grammar83.y"
             { (yyval.stmt) = create_stmt(STMT_NULL, (yyloc)); }
#line 3228 "grammar83.tab.c"
    break;

  case 243: /* assign_stmt: name IS_ASSIGNED expression ';'  */
#line 892 "grammar83.y"
                                    {
        (yyval.stmt) = create_stmt(STMT_ASSIGN, (yyloc));
        (yyval.stmt)->u.assign.expr = (yyvsp[-1].expr);
    }
#line 3237 "grammar83.tab.c"
    break;

  case 244: /* if_stmt: IF cond_clause_s else_opt END IF ';'  */
#line 898 "grammar83.y"
                                         {
        (yyval.stmt) = (yyvsp[-4].stmt);
        (yyval.stmt)->u.if_.else_ = (yyvsp[-3].stmt);
    }
#line 3246 "grammar83.tab.c"
    break;

  case 246: /* cond_clause_s: cond_clause_s ELSIF cond_clause  */
#line 905 "grammar83.y"
                                    { (yyvsp[-2].stmt)->u.if_.else_ = (yyvsp[0].stmt); }
#line 3252 "grammar83.tab.c"
    break;

  case 247: /* cond_clause: cond_part statement_s  */
#line 909 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_IF, (yyloc));
        (yyval.stmt)->u.if_.condition = (yyvsp[-1].expr);
        (yyval.stmt)->u.if_.stmts = (yyvsp[0].stmt);
    }
#line 3262 "grammar83.tab.c"
    break;

  case 248: /* cond_part: condition THEN  */
#line 916 "grammar83.y"
                   { (yyval.expr) = (yyvsp[-1].expr); }
#line 3268 "grammar83.tab.c"
    break;

  case 250: /* else_opt: %empty  */
#line 924 "grammar83.y"
                     { (yyval.stmt) = NULL; }
#line 3274 "grammar83.tab.c"
    break;

  case 251: /* else_opt: ELSE statement_s  */
#line 925 "grammar83.y"
                     { (yyval.stmt) = (yyvsp[0].stmt); }
#line 3280 "grammar83.tab.c"
    break;

  case 257: /* loop_stmt: label_opt loop_content id_opt ';'  */
#line 947 "grammar83.y"
                                      { (yyval.stmt) = (yyvsp[-2].stmt); }
#line 3286 "grammar83.tab.c"
    break;

  case 260: /* loop_content: basic_loop  */
#line 956 "grammar83.y"
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
#line 3301 "grammar83.tab.c"
    break;

  case 261: /* loop_content: WHILE condition basic_loop  */
#line 966 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_WHILE;
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
        (yyval.stmt)->u.loop.u.while_.condition = (yyvsp[-1].expr);
    }
#line 3312 "grammar83.tab.c"
    break;

  case 262: /* loop_content: iter_part reverse_opt discrete_range basic_loop  */
#line 972 "grammar83.y"
                                                    {
        // TODO: identifier
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_FOR;
        (yyval.stmt)->u.loop.reverse = (yyvsp[-2].bool_);
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
    }
#line 3324 "grammar83.tab.c"
    break;

  case 264: /* reverse_opt: %empty  */
#line 985 "grammar83.y"
            { (yyval.bool_) = false; }
#line 3330 "grammar83.tab.c"
    break;

  case 265: /* reverse_opt: REVERSE  */
#line 986 "grammar83.y"
            { (yyval.bool_) = true; }
#line 3336 "grammar83.tab.c"
    break;

  case 266: /* basic_loop: LOOP statement_s END LOOP  */
#line 990 "grammar83.y"
                              { (yyval.stmt) = (yyvsp[-2].stmt); }
#line 3342 "grammar83.tab.c"
    break;

  case 269: /* block: label_opt block_decl block_body END id_opt ';'  */
#line 1000 "grammar83.y"
                                                   {
        (yyval.stmt) = create_stmt(STMT_BLOCK, (yyloc));
        (yyval.stmt)->u.block.stmts = (yyvsp[-3].stmt);
        // Close scope if needed (i.e. if there was a declaration section)
        if((yyvsp[-4].bool_)) {
            end_scope(context, (yylsp[-2]));
        }
    }
#line 3355 "grammar83.tab.c"
    break;

  case 270: /* block_decl: %empty  */
#line 1010 "grammar83.y"
            { (yyval.bool_) = false; }
#line 3361 "grammar83.tab.c"
    break;

  case 271: /* $@1: %empty  */
#line 1011 "grammar83.y"
            { begin_scope(context, (yylsp[0])); }
#line 3367 "grammar83.tab.c"
    break;

  case 272: /* block_decl: DECLARE $@1 decl_part  */
#line 1011 "grammar83.y"
                                                    { (yyval.bool_) = true; }
#line 3373 "grammar83.tab.c"
    break;

  case 273: /* block_body: BEGiN handled_stmt_s  */
#line 1015 "grammar83.y"
                         { (yyval.stmt) = (yyvsp[0].stmt); }
#line 3379 "grammar83.tab.c"
    break;

  case 274: /* handled_stmt_s: statement_s except_handler_part_opt  */
#line 1020 "grammar83.y"
                                        { (yyval.stmt) = (yyvsp[-1].stmt); }
#line 3385 "grammar83.tab.c"
    break;

  case 277: /* exit_stmt: EXIT name_opt when_opt ';'  */
#line 1029 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_EXIT, (yyloc));
        // TODO: name_opt
        (yyval.stmt)->u.exit.condition = (yyvsp[-1].expr);
    }
#line 3395 "grammar83.tab.c"
    break;

  case 280: /* when_opt: %empty  */
#line 1041 "grammar83.y"
                   { (yyval.expr) = NULL; }
#line 3401 "grammar83.tab.c"
    break;

  case 281: /* when_opt: WHEN condition  */
#line 1042 "grammar83.y"
                   { (yyval.expr) = (yyvsp[0].expr); }
#line 3407 "grammar83.tab.c"
    break;

  case 282: /* return_stmt: RETURN ';'  */
#line 1046 "grammar83.y"
                  { (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc)); }
#line 3413 "grammar83.tab.c"
    break;

  case 283: /* return_stmt: RETURN expression ';'  */
#line 1047 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc));
        (yyval.stmt)->u.return_.expr = (yyvsp[-1].expr);
    }
#line 3422 "grammar83.tab.c"
    break;

  case 284: /* goto_stmt: GOTO name ';'  */
#line 1053 "grammar83.y"
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
#line 3449 "grammar83.tab.c"
    break;

  case 287: /* @2: %empty  */
#line 1082 "grammar83.y"
                                           {
        begin_scope(context, (yylsp[0]));
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
    }
#line 3459 "grammar83.tab.c"
    break;

  case 289: /* @3: %empty  */
#line 1088 "grammar83.y"
                                         {
        begin_scope(context, (yylsp[0]));
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
    }
#line 3469 "grammar83.tab.c"
    break;

  case 293: /* designator: char_string  */
#line 1099 "grammar83.y"
                { (yyval.str_token) = string_pool_to_token((yyvsp[0].str)); }
#line 3475 "grammar83.tab.c"
    break;

  case 301: /* mode: %empty  */
#line 1122 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3481 "grammar83.tab.c"
    break;

  case 302: /* mode: IN  */
#line 1123 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3487 "grammar83.tab.c"
    break;

  case 303: /* mode: OUT  */
#line 1124 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_OUT; }
#line 3493 "grammar83.tab.c"
    break;

  case 304: /* mode: IN OUT  */
#line 1125 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN_OUT; }
#line 3499 "grammar83.tab.c"
    break;

  case 307: /* procedure_call: name ';'  */
#line 1137 "grammar83.y"
             {
        (yyval.stmt) = create_stmt(STMT_EXPR, (yyloc));
        (yyval.stmt)->u.expr.kind = EXPR_NAME;
        (yyval.stmt)->u.expr.line_num = (yyloc);
        (yyval.stmt)->u.expr.u.name = (yyvsp[-1].name);
    }
#line 3510 "grammar83.tab.c"
    break;


#line 3514 "grammar83.tab.c"

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

#line 1373 "grammar83.y"


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
