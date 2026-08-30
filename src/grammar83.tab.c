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
  YYSYMBOL_used_char = 162,                /* used_char  */
  YYSYMBOL_operator_symbol = 163,          /* operator_symbol  */
  YYSYMBOL_indexed_comp = 164,             /* indexed_comp  */
  YYSYMBOL_value_s = 165,                  /* value_s  */
  YYSYMBOL_value = 166,                    /* value  */
  YYSYMBOL_selected_comp = 167,            /* selected_comp  */
  YYSYMBOL_attribute = 168,                /* attribute  */
  YYSYMBOL_attribute_id = 169,             /* attribute_id  */
  YYSYMBOL_literal = 170,                  /* literal  */
  YYSYMBOL_aggregate = 171,                /* aggregate  */
  YYSYMBOL_value_s_2 = 172,                /* value_s_2  */
  YYSYMBOL_comp_assoc = 173,               /* comp_assoc  */
  YYSYMBOL_expression = 174,               /* expression  */
  YYSYMBOL_logical = 175,                  /* logical  */
  YYSYMBOL_short_circuit = 176,            /* short_circuit  */
  YYSYMBOL_relation = 177,                 /* relation  */
  YYSYMBOL_relational = 178,               /* relational  */
  YYSYMBOL_membership = 179,               /* membership  */
  YYSYMBOL_simple_expression = 180,        /* simple_expression  */
  YYSYMBOL_unary = 181,                    /* unary  */
  YYSYMBOL_adding = 182,                   /* adding  */
  YYSYMBOL_term = 183,                     /* term  */
  YYSYMBOL_multiplying = 184,              /* multiplying  */
  YYSYMBOL_factor = 185,                   /* factor  */
  YYSYMBOL_primary = 186,                  /* primary  */
  YYSYMBOL_parenthesized_primary = 187,    /* parenthesized_primary  */
  YYSYMBOL_qualified = 188,                /* qualified  */
  YYSYMBOL_allocator = 189,                /* allocator  */
  YYSYMBOL_statement_s = 190,              /* statement_s  */
  YYSYMBOL_statement = 191,                /* statement  */
  YYSYMBOL_unlabeled = 192,                /* unlabeled  */
  YYSYMBOL_simple_stmt = 193,              /* simple_stmt  */
  YYSYMBOL_compound_stmt = 194,            /* compound_stmt  */
  YYSYMBOL_null_stmt = 195,                /* null_stmt  */
  YYSYMBOL_assign_stmt = 196,              /* assign_stmt  */
  YYSYMBOL_if_stmt = 197,                  /* if_stmt  */
  YYSYMBOL_cond_clause_s = 198,            /* cond_clause_s  */
  YYSYMBOL_cond_clause = 199,              /* cond_clause  */
  YYSYMBOL_cond_part = 200,                /* cond_part  */
  YYSYMBOL_condition = 201,                /* condition  */
  YYSYMBOL_else_opt = 202,                 /* else_opt  */
  YYSYMBOL_case_stmt = 203,                /* case_stmt  */
  YYSYMBOL_case_hdr = 204,                 /* case_hdr  */
  YYSYMBOL_alternative_s = 205,            /* alternative_s  */
  YYSYMBOL_alternative = 206,              /* alternative  */
  YYSYMBOL_loop_stmt = 207,                /* loop_stmt  */
  YYSYMBOL_label_opt = 208,                /* label_opt  */
  YYSYMBOL_loop_content = 209,             /* loop_content  */
  YYSYMBOL_iter_part = 210,                /* iter_part  */
  YYSYMBOL_reverse_opt = 211,              /* reverse_opt  */
  YYSYMBOL_basic_loop = 212,               /* basic_loop  */
  YYSYMBOL_id_opt = 213,                   /* id_opt  */
  YYSYMBOL_block = 214,                    /* block  */
  YYSYMBOL_block_decl = 215,               /* block_decl  */
  YYSYMBOL_block_body = 216,               /* block_body  */
  YYSYMBOL_handled_stmt_s = 217,           /* handled_stmt_s  */
  YYSYMBOL_except_handler_part_opt = 218,  /* except_handler_part_opt  */
  YYSYMBOL_exit_stmt = 219,                /* exit_stmt  */
  YYSYMBOL_name_opt = 220,                 /* name_opt  */
  YYSYMBOL_when_opt = 221,                 /* when_opt  */
  YYSYMBOL_return_stmt = 222,              /* return_stmt  */
  YYSYMBOL_goto_stmt = 223,                /* goto_stmt  */
  YYSYMBOL_subprog_decl = 224,             /* subprog_decl  */
  YYSYMBOL_subprog_spec = 225,             /* subprog_spec  */
  YYSYMBOL_designator = 226,               /* designator  */
  YYSYMBOL_formal_part_opt = 227,          /* formal_part_opt  */
  YYSYMBOL_formal_part = 228,              /* formal_part  */
  YYSYMBOL_param_s = 229,                  /* param_s  */
  YYSYMBOL_param = 230,                    /* param  */
  YYSYMBOL_mode = 231,                     /* mode  */
  YYSYMBOL_subprog_spec_is_push = 232,     /* subprog_spec_is_push  */
  YYSYMBOL_subprog_body = 233,             /* subprog_body  */
  YYSYMBOL_procedure_call = 234,           /* procedure_call  */
  YYSYMBOL_pkg_decl = 235,                 /* pkg_decl  */
  YYSYMBOL_pkg_spec = 236,                 /* pkg_spec  */
  YYSYMBOL_private_part = 237,             /* private_part  */
  YYSYMBOL_identifier_opt = 238,           /* identifier_opt  */
  YYSYMBOL_pkg_body = 239,                 /* pkg_body  */
  YYSYMBOL_body_opt = 240,                 /* body_opt  */
  YYSYMBOL_private_type = 241,             /* private_type  */
  YYSYMBOL_limited_opt = 242,              /* limited_opt  */
  YYSYMBOL_use_name_s = 243,               /* use_name_s  */
  YYSYMBOL_use_clause = 244,               /* use_clause  */
  YYSYMBOL_rename_decl = 245,              /* rename_decl  */
  YYSYMBOL_rename_unit = 246,              /* rename_unit  */
  YYSYMBOL_renames = 247,                  /* renames  */
  YYSYMBOL_comp_unit = 248,                /* comp_unit  */
  YYSYMBOL_context_spec = 249,             /* context_spec  */
  YYSYMBOL_with_clause = 250,              /* with_clause  */
  YYSYMBOL_use_clause_opt = 251,           /* use_clause_opt  */
  YYSYMBOL_unit = 252,                     /* unit  */
  YYSYMBOL_subunit = 253,                  /* subunit  */
  YYSYMBOL_subunit_body = 254,             /* subunit_body  */
  YYSYMBOL_body_stub = 255,                /* body_stub  */
  YYSYMBOL_exception_decl = 256,           /* exception_decl  */
  YYSYMBOL_except_handler_part = 257,      /* except_handler_part  */
  YYSYMBOL_exception_handler = 258,        /* exception_handler  */
  YYSYMBOL_except_choice_s = 259,          /* except_choice_s  */
  YYSYMBOL_except_choice = 260,            /* except_choice  */
  YYSYMBOL_raise_stmt = 261,               /* raise_stmt  */
  YYSYMBOL_generic_decl = 262,             /* generic_decl  */
  YYSYMBOL_generic_formal_part = 263,      /* generic_formal_part  */
  YYSYMBOL_generic_formal = 264,           /* generic_formal  */
  YYSYMBOL_generic_discrim_part_opt = 265, /* generic_discrim_part_opt  */
  YYSYMBOL_subp_default = 266,             /* subp_default  */
  YYSYMBOL_generic_type_def = 267,         /* generic_type_def  */
  YYSYMBOL_generic_derived_type = 268,     /* generic_derived_type  */
  YYSYMBOL_generic_subp_inst = 269,        /* generic_subp_inst  */
  YYSYMBOL_generic_pkg_inst = 270,         /* generic_pkg_inst  */
  YYSYMBOL_generic_inst = 271,             /* generic_inst  */
  YYSYMBOL_rep_spec = 272,                 /* rep_spec  */
  YYSYMBOL_attrib_def = 273,               /* attrib_def  */
  YYSYMBOL_record_type_spec = 274,         /* record_type_spec  */
  YYSYMBOL_align_opt = 275,                /* align_opt  */
  YYSYMBOL_comp_loc_s = 276,               /* comp_loc_s  */
  YYSYMBOL_address_spec = 277,             /* address_spec  */
  YYSYMBOL_code_stmt = 278                 /* code_stmt  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;



/* Unqualified %code blocks.  */
#line 68 "grammar83.y"

    #include <assert.h>
    #include <stdlib.h>
    #include <stdbool.h>
    #include "error.h"
    #include "string_pool.h"
    #include "string_view.h"
    #include "lexer.h"

    DEFINE_ARRAY_OPS(StringToken)
    DEFINE_LINKED_LIST_OPS(Decl)

#line 395 "grammar83.tab.c"

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
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1331

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  95
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  184
/* YYNRULES -- Number of rules.  */
#define YYNRULES  398
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  714

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
       0,   114,   114,   118,   119,   123,   124,   128,   129,   133,
     134,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   152,   156,   160,   164,   171,   172,   176,   177,
     181,   182,   186,   190,   191,   192,   197,   198,   202,   203,
     204,   205,   206,   207,   208,   209,   213,   218,   219,   223,
     224,   228,   232,   236,   240,   241,   245,   246,   247,   251,
     255,   256,   260,   261,   265,   266,   270,   271,   275,   279,
     280,   284,   285,   289,   293,   297,   301,   302,   306,   310,
     314,   315,   319,   320,   324,   328,   329,   333,   334,   335,
     339,   340,   344,   345,   349,   350,   354,   358,   359,   363,
     364,   368,   369,   373,   377,   378,   382,   386,   387,   391,
     392,   393,   397,   398,   402,   403,   407,   408,   412,   413,
     417,   418,   422,   423,   424,   425,   429,   430,   434,   435,
     439,   440,   444,   445,   446,   447,   448,   452,   453,   454,
     458,   462,   466,   470,   471,   475,   476,   477,   478,   482,
     483,   484,   485,   489,   493,   494,   495,   496,   500,   501,
     502,   506,   507,   508,   509,   510,   514,   515,   519,   523,
     524,   525,   529,   530,   531,   535,   536,   541,   542,   543,
     544,   548,   549,   550,   551,   552,   553,   557,   558,   562,
     563,   564,   568,   569,   573,   574,   575,   579,   580,   584,
     585,   586,   587,   591,   592,   593,   594,   598,   599,   600,
     601,   602,   606,   607,   611,   615,   616,   620,   621,   625,
     626,   630,   631,   632,   636,   637,   638,   639,   640,   641,
     642,   643,   644,   648,   649,   650,   651,   655,   659,   663,
     667,   668,   672,   676,   680,   684,   685,   689,   693,   697,
     698,   702,   707,   711,   712,   716,   717,   718,   722,   726,
     727,   731,   735,   736,   741,   745,   746,   750,   755,   759,
     760,   764,   768,   769,   773,   774,   778,   779,   783,   787,
     788,   793,   794,   795,   799,   800,   804,   805,   809,   813,
     814,   818,   819,   823,   824,   825,   826,   830,   836,   840,
     844,   845,   849,   852,   853,   857,   858,   862,   866,   867,
     871,   875,   876,   880,   881,   882,   883,   887,   892,   893,
     894,   898,   899,   900,   901,   905,   909,   910,   914,   915,
     916,   920,   929,   930,   934,   935,   936,   937,   938,   939,
     940,   944,   948,   949,   953,   954,   958,   962,   963,   967,
     968,   972,   973,   977,   978,   982,   986,   987,   991,   992,
     996,   997,   998,   999,  1000,  1001,  1002,  1006,  1007,  1008,
    1012,  1013,  1014,  1018,  1019,  1020,  1021,  1022,  1023,  1024,
    1025,  1026,  1027,  1031,  1032,  1036,  1040,  1044,  1048,  1049,
    1050,  1054,  1058,  1062,  1063,  1067,  1068,  1072,  1076
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
  "decl_item_or_body", "body", "name", "mark", "used_char",
  "operator_symbol", "indexed_comp", "value_s", "value", "selected_comp",
  "attribute", "attribute_id", "literal", "aggregate", "value_s_2",
  "comp_assoc", "expression", "logical", "short_circuit", "relation",
  "relational", "membership", "simple_expression", "unary", "adding",
  "term", "multiplying", "factor", "primary", "parenthesized_primary",
  "qualified", "allocator", "statement_s", "statement", "unlabeled",
  "simple_stmt", "compound_stmt", "null_stmt", "assign_stmt", "if_stmt",
  "cond_clause_s", "cond_clause", "cond_part", "condition", "else_opt",
  "case_stmt", "case_hdr", "alternative_s", "alternative", "loop_stmt",
  "label_opt", "loop_content", "iter_part", "reverse_opt", "basic_loop",
  "id_opt", "block", "block_decl", "block_body", "handled_stmt_s",
  "except_handler_part_opt", "exit_stmt", "name_opt", "when_opt",
  "return_stmt", "goto_stmt", "subprog_decl", "subprog_spec", "designator",
  "formal_part_opt", "formal_part", "param_s", "param", "mode",
  "subprog_spec_is_push", "subprog_body", "procedure_call", "pkg_decl",
  "pkg_spec", "private_part", "identifier_opt", "pkg_body", "body_opt",
  "private_type", "limited_opt", "use_name_s", "use_clause", "rename_decl",
  "rename_unit", "renames", "comp_unit", "context_spec", "with_clause",
  "use_clause_opt", "unit", "subunit", "subunit_body", "body_stub",
  "exception_decl", "except_handler_part", "exception_handler",
  "except_choice_s", "except_choice", "raise_stmt", "generic_decl",
  "generic_formal_part", "generic_formal", "generic_discrim_part_opt",
  "subp_default", "generic_type_def", "generic_derived_type",
  "generic_subp_inst", "generic_pkg_inst", "generic_inst", "rep_spec",
  "attrib_def", "record_type_spec", "align_opt", "comp_loc_s",
  "address_spec", "code_stmt", YY_NULLPTR
  };
  return yy_sname[yysymbol];
}
#endif

#define YYPACT_NINF (-590)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-351)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -590,    93,   574,  -590,  -590,   247,  -590,    72,   -14,     8,
      22,    33,  -590,  -590,   268,  1231,  -590,  -590,    87,  -590,
    -590,   574,  -590,  -590,  -590,  -590,   211,   108,   153,  -590,
    -590,    10,   122,   151,   304,   215,   226,  -590,     9,   322,
     336,  -590,   309,   316,   256,    85,   328,   380,   563,  -590,
    -590,  -590,  -590,   562,  -590,  -590,   448,  -590,   484,  -590,
    -590,  -590,   409,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
    -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
     407,   449,  -590,   433,   440,   423,   584,    18,   442,   467,
    -590,  -590,  -590,  -590,    47,   468,  -590,   514,  1097,   479,
    -590,   746,  -590,   486,  -590,   501,   336,  -590,  -590,  -590,
     597,  -590,  -590,  -590,  -590,  -590,  -590,  -590,   136,   507,
     547,   511,   132,   597,   323,   399,   339,  1082,   569,  -590,
     165,   407,   449,  -590,   213,   540,   247,   530,   549,   287,
    -590,   548,  -590,  -590,   414,  -590,   336,  1151,   111,   587,
    1187,  -590,   425,  -590,  -590,   936,   336,   936,  -590,  -590,
     661,  -590,   488,  -590,  -590,   612,  -590,   606,  -590,  -590,
    -590,   227,  -590,   252,   333,   508,  -590,   649,  -590,  -590,
    -590,   557,  -590,   597,   573,    81,   118,   679,    81,   593,
     659,   336,    59,   668,  -590,  -590,   638,   678,   116,   318,
     632,   764,   336,   610,   764,   636,   336,   698,   637,  1082,
    -590,    71,   643,   863,  -590,  -590,  -590,  -590,  -590,  -590,
    -590,  -590,  -590,  -590,   391,  -590,  -590,  -590,  -590,  -590,
    -590,  -590,  -590,   247,   666,  1195,   703,    77,  -590,   690,
     215,   755,   215,   736,  -590,   256,  -590,    47,  -590,   597,
     448,   741,  1256,   777,  -590,   165,  -590,   606,  -590,  -590,
     764,  -590,   754,  -590,  -590,    52,  -590,   645,   504,   735,
     647,   737,   375,   347,   740,   746,   307,   757,   797,  -590,
     764,   764,  -590,  -590,  -590,  -590,   788,  -590,  -590,  -590,
    -590,  -590,  -590,   764,   764,   333,   508,  -590,  -590,  -590,
    -590,   333,   936,   808,   791,  -590,  -590,  -590,   657,  -590,
    -590,   513,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
    -590,   764,   816,    37,  -590,  -590,  1143,   756,   320,  -590,
     761,   658,   542,  -590,   789,   767,   463,   490,   764,  -590,
     769,   763,   841,   793,  -590,  -590,  -590,  -590,   156,   597,
     783,   780,   227,   732,  -590,  1082,   800,  -590,   794,  -590,
     208,  -590,  -590,   764,  -590,  -590,   798,  -590,  -590,   798,
     449,  1231,   801,  1082,   764,   247,   814,  -590,   448,   799,
    -590,  -590,  -590,   796,   846,   819,   837,   845,  -590,    43,
    -590,  -590,   861,   854,  -590,   823,   227,   826,   764,   722,
     764,   427,  -590,   573,  -590,   573,  -590,   672,  -590,   764,
    -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,   523,  -590,
     212,    48,   508,  -590,  -590,  -590,  -590,   573,   301,   865,
    -590,  -590,   832,  -590,   764,  -590,  -590,  -590,  -590,   899,
      53,  -590,   244,   764,   764,  -590,   764,   336,   676,  -590,
    -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
    -590,   550,  -590,   438,  -590,   764,   869,   764,   838,   840,
    -590,   764,   843,  -590,  1082,   764,   890,   828,  -590,  -590,
    -590,   476,    24,  -590,  -590,   147,  -590,   892,  1019,   888,
     853,  -590,   764,   905,  -590,  -590,   932,   938,   940,   336,
     944,   946,  -590,  -590,  -590,   887,   873,  -590,   336,   336,
     105,   874,  -590,   823,   894,  -590,  -590,  -590,   227,  -590,
    -590,   227,  -590,   620,   875,  -590,  -590,   897,   683,   523,
    -590,  -590,   764,    83,  -590,   450,  -590,   256,  -590,   336,
    -590,   424,   450,   227,  -590,  -590,  -590,   702,  -590,   901,
    -590,  -590,  -590,  -590,  -590,   712,  -590,   719,  -590,   651,
     336,   227,  -590,  -590,  -590,  -590,  1047,  -590,   927,  -590,
    -590,   893,   597,    57,  -590,   949,   722,  -590,  -590,   929,
    -590,  -590,   888,   389,   247,   947,  -590,  -590,   902,  -590,
     896,  -590,   335,   572,  -590,   597,  -590,   900,   764,   903,
    -590,   527,   918,    40,  -590,  -590,    43,  -590,   764,  -590,
    -590,  -590,   676,  -590,   263,   941,   336,  -590,   764,   585,
    -590,  -590,  -590,   904,   464,  1082,   464,   908,    58,  -590,
    -590,   910,   986,   939,  -590,   912,  -590,   352,  -590,    39,
    -590,  -590,   914,   764,  -590,   450,  -590,   915,   920,   919,
     697,   969,  -590,  -590,  -590,   336,  -590,   701,  -590,  -590,
    -590,    61,   891,  -590,  -590,  1082,  -590,  -590,  -590,  -590,
     925,  -590,  -590,   525,  -590,  -590,   960,  -590,   336,   951,
     169,  -590,   449,  -590,  1005,  1082,   926,   945,   764,  -590,
     449,   841,  -590,  -590,  -590,   984,  -590,   948,    34,   950,
     449,  -590,   722,   168,  -590,  -590,    64,   994,  -590,  -590,
     952,   263,  -590,  -590
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       9,     0,     0,     2,     1,     0,   358,     0,     0,     0,
       0,     0,    10,   336,     0,     0,   337,   334,     0,   335,
     340,     0,   332,     9,   338,   339,     0,     0,     0,   284,
     285,   283,     0,     0,     0,   286,     0,    24,     0,   297,
       0,   279,     0,     0,     0,     0,     0,     0,     0,   125,
     122,    11,    12,     0,    13,    14,     0,   129,     0,   126,
     128,    15,     0,   130,    16,   131,   123,    18,   320,    20,
      17,    19,   124,   388,   389,   390,   300,   330,   332,     9,
     328,   327,   292,     0,     0,     0,     0,     0,     0,     0,
     366,   359,   280,   301,     0,     0,   287,     0,     0,     0,
       3,     0,   281,     0,   331,     0,     0,   385,   132,   141,
     325,   136,   133,   134,   135,   322,    21,   137,     0,     0,
       0,    33,   132,     0,   134,     0,    26,     0,     0,   127,
     297,   329,   326,   333,     0,   367,     0,     0,     0,   293,
     356,     0,   360,   357,     0,   289,     0,     0,     0,   303,
       0,   120,     0,   386,   321,     0,     0,     0,   160,   140,
     132,   158,     0,   192,   193,     0,     5,   208,   159,   207,
     212,     7,   169,   177,     0,   189,   197,   203,   211,   210,
     209,     0,    25,   387,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    36,    34,   317,     0,    27,     0,     0,
       0,     0,   272,     0,     0,     0,   272,     0,   132,     0,
     223,     0,     0,     0,   217,   219,   221,   222,   224,   225,
     233,   234,     9,   235,   265,   236,   267,   226,   227,   228,
     229,   230,   231,   262,     0,     0,     0,     0,   368,     0,
     286,     0,   286,   294,   295,     0,   324,     0,   288,   282,
     308,     0,     0,     0,   121,     0,   205,   215,   216,   204,
       0,   148,   160,   111,   113,     0,   107,   110,   208,     0,
       0,   146,   109,   177,     0,     0,     0,   172,   173,   174,
       0,     0,   184,   182,   186,   187,     0,   181,   183,   185,
     194,   195,   196,     0,     0,     0,   190,   201,   202,   199,
     200,     0,     0,     0,     0,   342,   343,   341,     0,   143,
     146,   109,   157,   156,   155,   154,   153,   152,   149,   150,
     151,     0,   393,     0,   138,   139,     0,     0,    48,   100,
       0,     0,     0,    97,   311,     0,   132,   134,     0,   346,
       0,     0,    30,    28,    29,    71,    72,   232,     0,   273,
     274,     0,   244,   245,   240,     0,     0,   237,     0,   276,
       0,   254,   220,     0,   299,   398,     0,   218,   268,   270,
     249,     0,     0,     0,     0,   262,   259,   255,     0,     0,
     263,   345,   323,     0,   311,     0,     0,   370,   296,    30,
     290,   309,     0,     0,   304,   305,     8,     0,     0,     0,
       0,     0,   112,     0,   162,     0,   161,     0,   213,     0,
       4,     6,   214,   175,   176,   170,   171,   188,   178,   179,
     180,     0,   191,   198,   206,   297,   142,     0,     0,     0,
     395,   391,     0,    46,     0,    47,    50,    49,    35,   101,
       0,    96,     0,     0,     0,   312,     0,     0,     0,    37,
      44,    64,    38,    39,    40,    66,    67,    41,    42,    43,
      45,     0,    32,     0,   319,     0,     0,     0,     0,     0,
     248,     0,     0,   278,     0,     0,     0,     0,   243,   355,
     277,     0,     0,   347,   348,     0,   266,     0,     0,     0,
       0,   260,     0,     0,   298,   369,     0,     0,     0,     0,
       0,     0,   379,   380,   381,     0,     0,   382,     0,     0,
       0,     0,   291,   305,     0,   306,   302,   165,   168,   108,
     110,   109,    53,   208,    57,   166,   167,   160,     0,    56,
     144,   397,     0,     0,   344,    54,   102,     0,    98,     0,
     114,     0,    54,    65,    52,    63,    62,     0,    60,     0,
     310,     9,    84,    23,    83,     0,    76,     0,    80,   208,
       0,    31,    22,   318,   275,   271,     0,   241,     0,   238,
     354,   132,   353,     0,   351,     0,     0,   250,   258,     0,
     256,   252,     0,   208,   262,   376,   378,   375,   383,   374,
       0,   361,   370,     0,   372,   371,   362,     0,     0,     0,
     163,     0,     0,     0,    55,    51,    30,   115,     0,    69,
      68,    59,     0,    86,     0,     0,     0,    79,     0,     0,
      82,    75,    74,     0,     0,     0,     0,     0,     0,   261,
     257,     0,     0,     0,   373,     0,   365,     0,   307,     0,
     164,   394,     0,     0,    99,    54,    61,     0,     0,     0,
       0,     0,     9,    90,     9,     0,    77,     0,    81,    78,
     239,     0,     0,   352,   247,     0,   264,   377,   384,   363,
       0,    58,   392,     0,    70,    95,     0,     9,     0,     0,
       0,    87,    88,    73,     0,     0,     0,     0,     0,     9,
      89,    30,    85,    91,     9,     0,   364,     0,     0,     0,
      93,   396,     0,     0,   104,    94,     0,     0,   105,     9,
       0,     0,   103,   106
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -590,  -590,    -2,  -590,   744,   -20,  -590,  -590,  -590,   -10,
    -590,  -590,  -381,  -590,  -590,  -590,  -590,  -590,  -174,  -590,
    -590,  -590,  -259,  -492,  -289,  -590,  -590,   411,  -590,  -590,
    -590,  -590,  -270,  -590,  -590,  -589,  -590,   412,  -590,  -590,
    -457,  -590,  -590,   314,  -590,  -590,   353,   906,  -590,   592,
    -590,   354,  -590,   332,  -534,   644,  -388,   653,    -8,   795,
    -590,   -68,  -590,   987,  -590,    17,  -243,   860,   864,  -590,
     656,  -142,   -25,  -590,   870,  -590,  -590,  -590,   889,   -86,
    -590,  -590,   524,  -590,  -590,  -158,  -590,  -590,  -136,  -590,
     766,  -124,  -239,  -115,  -590,  -349,  -199,  -590,  -590,  -590,
    -590,  -590,  -590,  -590,   589,  -590,  -330,  -590,  -590,  -590,
    -590,  -590,  -590,  -590,  -590,  -590,  -590,  -450,  -353,  -590,
    -590,  -214,  -590,  -590,  -590,   866,  -590,  -590,  -590,   246,
      19,    29,    -6,  -590,  -590,   -62,  -590,  -590,    25,  -590,
     272,  1043,  -590,   560,    26,  -590,   691,   692,  -590,    23,
    -590,   297,   -15,  -590,  -590,  1057,  1004,  1065,  -590,  -590,
    -590,  -590,  -590,   718,   472,   462,  -590,   349,  -590,  -590,
    -590,   499,  -590,  -590,  -590,  -590,  1002,  -590,  -590,  -590,
    -590,  -590,  -590,  -590
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     1,   210,   165,   166,     2,    50,    51,    52,    53,
     199,   342,   468,    54,   193,   335,   449,    55,   621,   435,
     436,   450,   604,   620,   264,   452,   547,   548,   453,   454,
     455,   456,   344,   345,   346,   622,   555,   556,   466,   557,
     558,   458,   552,   651,   652,   681,   653,   194,   332,   333,
     537,   654,   703,   704,   265,   266,   267,   459,   250,   149,
     150,    57,    58,    59,    60,   167,   118,   168,   111,   112,
     308,   309,   113,   114,   316,   169,   170,   270,   310,   311,
     280,   281,   172,   293,   294,   173,   174,   295,   175,   301,
     176,   177,   178,   179,   180,   213,   214,   215,   216,   217,
     218,   219,   220,   353,   354,   355,   356,   476,   221,   222,
     485,   577,   223,   224,   375,   376,   492,   377,   379,   225,
     378,   128,   226,   368,   227,   350,   472,   228,   229,    61,
      62,   380,    95,    96,   144,    88,   245,    15,    63,   230,
      64,    18,   253,   516,    65,   392,   460,   461,   125,    66,
      67,    68,    42,     3,    21,    22,    80,    23,    24,   307,
      69,    70,   369,   483,   573,   574,   231,    71,    26,    91,
     239,   511,   506,   507,    27,    28,   107,    72,    73,    74,
     430,   533,    75,   232
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      12,    38,   389,    81,   273,   419,   477,    56,   512,   402,
     362,   520,   212,    49,   367,   171,    86,   327,    99,    77,
     269,    14,   490,   124,   488,   343,   273,    16,    19,   102,
     151,   256,   145,   259,    31,   582,   391,   412,   296,   580,
      14,   258,   628,   605,   489,    87,    16,    19,    82,    90,
     610,   409,   467,   277,   329,   277,    49,   110,   643,   132,
     329,    34,   398,   330,   457,   123,   683,   625,   665,   437,
    -286,   685,   141,   570,   709,   451,   272,    40,   329,    12,
     363,   383,   254,    35,    86,   278,     8,   278,   104,   691,
      94,   105,    32,     4,   212,   312,    49,   140,   212,   571,
     109,   323,    36,   133,   702,   119,   313,   314,    37,   594,
     279,   522,   279,   602,   502,   348,   431,   152,   352,   236,
     671,   360,    37,   183,   188,   566,   189,   188,    37,   189,
      12,   251,   630,   317,    37,   418,   421,   399,   290,   291,
     292,   564,   626,   399,   211,    49,   626,    33,    49,   399,
     364,   184,    37,   674,   133,   276,   315,   186,   117,   422,
      33,   658,   412,   249,   493,   240,    76,   151,   706,   152,
     647,   337,   277,   257,   396,    40,   554,   575,   424,   268,
     108,   109,   331,   340,   151,   390,    33,    92,   520,   171,
     648,    98,   159,   318,   109,   339,   470,    97,   707,   -92,
     304,   268,   370,   554,   278,   187,   305,   306,   328,   106,
      40,  -313,    82,   123,  -313,  -208,   328,   576,  -208,   349,
     188,     8,   189,   349,   277,   644,   211,   331,   234,   279,
     211,   631,    93,    49,   385,   428,   387,    86,   702,   -92,
     212,   273,   421,   277,    37,   273,     5,   273,    13,   273,
      49,   529,   463,   235,   152,  -208,   278,   282,   212,   283,
     284,   525,    83,   526,   647,     9,   539,    13,   540,   273,
    -208,   152,    40,   544,    17,   278,   662,   481,   367,    84,
      48,   279,   609,    85,   648,   530,    37,   480,   352,   367,
     603,   285,   184,    17,   606,    94,   401,   286,   186,    20,
     279,   103,  -208,  -208,  -208,  -208,  -208,   421,    39,   649,
     699,   420,   518,   521,   520,     8,   686,   277,    20,   108,
     109,   312,    29,    30,    49,   588,   243,    40,   469,   554,
     522,   117,   313,   314,   421,   341,   695,   244,    37,   287,
     288,   289,   290,   291,   292,   155,   434,    41,   535,   278,
     409,    25,   282,   261,   283,   284,   670,   541,   542,   212,
     543,   197,   212,   486,   155,   607,   106,   367,    12,    49,
      25,   198,   211,   212,   279,   510,   400,   156,   157,   158,
     531,   561,   315,   100,   101,   352,   285,   162,   115,   352,
     211,   277,   286,   108,   109,   116,   156,   157,   158,   697,
     184,   263,  -314,   120,   185,  -314,   186,   159,   108,   109,
     161,   108,   109,   162,   371,   184,   268,   523,   273,   185,
     268,   186,   268,   278,   268,   372,   159,   108,   109,   161,
     331,   -54,   162,   373,   287,   288,   289,   290,   291,   292,
     277,   312,   163,   164,   268,   400,   601,   407,   279,   130,
     608,   212,   313,   314,   277,   121,   408,  -145,   136,   328,
     421,   421,   374,   367,   328,   255,   277,   127,    40,   184,
     -54,   -54,   278,   401,   137,   186,    48,   138,   195,   273,
     400,   196,   559,   524,    40,    43,   278,   367,    41,   261,
     521,   211,   277,   247,   211,   248,   367,   279,   278,   572,
     155,     8,   315,  -117,    41,   211,   400,   162,   134,   583,
     212,   279,   639,   570,  -117,   135,   328,   553,    44,     5,
       6,   142,   645,   279,   278,   592,   593,   595,   146,   277,
     421,   614,   156,   157,   262,    45,     8,   263,     9,   108,
     109,   277,  -315,   277,   273,  -315,   143,   212,    46,   279,
     212,   297,    47,    48,   147,   569,   328,   673,   154,    37,
     400,   278,   159,   108,   109,   161,   298,   181,   162,  -316,
     212,   212,  -316,   278,   261,   278,   182,   328,   163,   164,
     212,   688,   190,   211,   184,   155,   279,   191,   401,   659,
     186,   192,     5,   268,  -145,  -145,   549,   155,   279,   233,
     279,   299,   300,   550,   650,   241,   641,   551,   303,     5,
       6,     9,    12,   290,   291,   292,   521,   156,   157,   158,
     237,   440,   263,   441,   242,     7,     8,   246,     9,   156,
     157,   158,   680,   657,   682,   583,   523,    10,   122,   109,
     252,   572,   211,   572,   105,   126,    11,   159,   108,   109,
     161,   636,   637,   162,   268,   302,   185,   690,   186,   159,
     108,   109,   161,   163,   164,   162,   105,   139,   325,   698,
     650,   260,   328,   261,   700,   163,   164,   184,    12,   211,
      12,   185,   211,   186,   155,   351,   184,   338,    12,   711,
     276,   155,   186,   274,   275,   328,    12,   321,    12,   326,
     184,   650,   211,   211,   401,   523,   186,   619,   334,    12,
     155,   347,   211,   336,   109,   357,   156,   157,   527,   268,
     361,   263,   365,   156,   157,   158,  -147,  -147,   404,   405,
     384,   184,   -54,   -54,   155,   401,   322,   186,   426,   427,
     105,   439,   156,   157,   158,   381,   159,   108,   109,   161,
     545,   546,   162,   159,   108,   109,   161,   684,   155,   162,
     474,   475,   163,   164,   600,   427,   156,   157,   158,   163,
     164,   263,   159,   108,   109,   161,   155,   359,   162,   105,
     678,   184,   382,   611,   612,   185,   388,   186,   163,   164,
     156,   157,   158,   615,   616,   386,   159,   108,   109,   161,
     617,   618,   162,   442,   415,   416,   341,   395,   156,   157,
     158,   397,   163,   164,   443,   444,   393,   403,   406,   410,
     159,   160,   109,   161,   413,   414,   162,   417,    32,   200,
     445,   425,   446,   447,   429,   433,   163,   164,   159,   108,
     109,   161,   438,   465,   162,   400,   462,  -253,   464,   201,
     467,  -253,    40,   471,   163,   164,  -242,  -242,  -242,   473,
     442,   202,  -253,   341,   200,   203,   204,   478,   482,   448,
    -253,   496,   497,   479,   205,   491,   487,   495,   494,   508,
       8,   509,  -253,   206,   201,   510,  -253,   445,   207,   498,
     499,   513,   200,  -269,   514,   366,   202,  -253,   515,  -253,
     203,   204,   500,   208,   109,  -253,   209,   517,   532,   205,
    -253,   534,   201,   536,  -253,     8,   560,   562,   206,   563,
     568,  -349,   565,   207,   202,  -253,   501,   200,   203,   204,
     373,   578,   581,  -253,  -253,   584,   585,   205,   208,   109,
     550,   209,   586,     8,   587,  -253,   206,   201,   589,  -253,
     590,   207,   591,   596,   599,   598,  -251,   432,   613,   202,
    -253,  -349,  -253,   203,   204,   623,   208,   109,  -253,   209,
     627,   629,   205,   632,   633,   642,   624,   634,     8,   638,
     156,   206,   158,   660,   640,   200,   207,   664,   655,   666,
     667,   669,   668,   672,   675,   676,  -251,  -253,   677,   679,
     689,   208,   109,  -253,   209,   201,   687,  -253,   692,   659,
     159,   108,   109,   161,  -350,   710,   162,   202,  -253,   411,
     200,   203,   204,   646,   696,   713,  -253,   701,   656,   705,
     205,   712,   538,   693,   694,   708,     8,   503,  -253,   206,
     201,   238,  -253,   519,   207,   129,   319,   394,   200,   579,
     320,   271,   202,  -253,  -350,  -253,   203,   204,   324,   208,
     109,  -253,   209,   528,   567,   205,  -253,   423,   201,    89,
    -253,     8,   358,   597,   206,   504,   505,  -246,    78,   207,
     202,  -253,   131,   200,   203,   204,    79,   484,   663,  -253,
    -253,   635,     0,   205,   208,   109,   661,   209,    43,     8,
     153,  -253,   206,   201,     0,  -253,     0,   207,     0,     0,
       0,     0,     0,     0,     0,   202,  -253,     0,  -253,   203,
     204,     0,   208,   109,  -253,   209,     0,  -118,   205,     0,
       0,    44,     5,     6,     8,     0,     0,   206,     0,     0,
       0,   106,   207,     0,    43,     0,     0,     0,   148,     8,
    -118,     9,    43,  -253,     0,     0,     0,   208,   109,     0,
     209,    46,  -116,     0,     0,    47,    48,     0,     0,     0,
    -116,     0,    37,  -116,     0,     0,     0,    44,     5,     6,
       0,  -116,     0,     0,     0,    44,     5,     6,    43,     0,
       0,     0,     0,     0,    45,     8,    43,     9,     0,     0,
       0,     0,    45,     8,     0,     9,   432,    46,     0,     0,
       0,    47,    48,     0,     0,    46,     0,  -119,    37,    47,
      48,    44,     5,     6,     0,  -118,    37,     0,     0,    44,
       5,     6,    43,     0,     0,     0,     0,     0,   148,     8,
    -119,     9,     0,     0,     0,     0,   148,     8,  -118,     9,
    -116,    46,     0,     0,     0,    47,    48,    43,     0,    46,
       0,     0,    37,    47,    48,    44,     5,     6,     0,     0,
      37,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    45,     8,     0,     9,  -118,     0,     0,     0,
      44,     5,     6,     0,     0,    46,     0,     0,     0,    47,
      48,     0,     0,     0,     0,     0,    37,   148,     8,     0,
       9,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      46,     0,     0,     0,    47,    48,     0,     0,     0,     0,
       0,    37
};

static const yytype_int16 yycheck[] =
{
       2,    11,   245,    23,   162,   294,   355,    15,   389,   268,
     209,   399,   127,    15,   213,   101,    26,   191,    33,    21,
     162,     2,   375,    48,   373,   199,   184,     2,     2,    35,
      98,   155,    94,   157,     5,   492,   250,   276,   174,   489,
      21,   156,   576,   535,   374,    26,    21,    21,     1,    26,
     542,     3,     9,    16,     1,    16,    58,    40,    18,    79,
       1,    75,    10,     4,   334,    48,   655,    10,    10,   328,
      60,    10,    87,    49,    10,   334,   162,    59,     1,    81,
       9,     4,   150,    75,    94,    48,    52,    48,    79,   678,
      80,    82,    20,     0,   209,    14,    98,    79,   213,    75,
      76,   187,    80,    80,    70,    20,    25,    26,    75,     4,
      73,   400,    73,    30,   384,   201,    79,    98,   204,   134,
      81,   207,    75,   106,    84,   474,    86,    84,    75,    86,
     132,    20,   582,    15,    75,   293,   294,    85,    90,    91,
      92,   471,    85,    85,   127,   147,    85,    75,   150,    85,
      79,    80,    75,   645,   131,    84,    75,    86,    75,   295,
      75,   618,   401,   146,   378,   136,    79,   235,   702,   150,
       1,   196,    16,   156,   260,    59,   465,    30,   302,   162,
      75,    76,   192,   198,   252,   247,    75,    79,   576,   275,
      21,    40,    74,    75,    76,    79,    40,    75,    30,    30,
     181,   184,   222,   492,    48,    69,   181,   181,   191,    44,
      59,    79,     1,   196,    82,     3,   199,    70,     6,   202,
      84,    52,    86,   206,    16,   606,   209,   237,    63,    73,
     213,   584,    79,   235,   240,   321,   242,   247,    70,    70,
     355,   399,   400,    16,    75,   403,    35,   405,     2,   407,
     252,   409,   338,    40,   235,    43,    48,     5,   373,     7,
       8,   403,    51,   405,     1,    54,    22,    21,   442,   427,
      58,   252,    59,   447,     2,    48,   625,   363,   477,    68,
      69,    73,   541,    72,    21,   427,    75,    79,   374,   488,
     533,    39,    80,    21,   537,    80,    84,    45,    86,     2,
      73,    75,    90,    91,    92,    93,    94,   465,    40,    46,
     691,   294,   398,   399,   702,    52,   665,    16,    21,    75,
      76,    14,    75,    76,   326,   499,    39,    59,   343,   618,
     619,    75,    25,    26,   492,    17,   685,    50,    75,    87,
      88,    89,    90,    91,    92,    12,    26,    79,   434,    48,
       3,     2,     5,     1,     7,     8,     4,   443,   444,   474,
     446,    22,   477,   371,    12,   539,    44,   566,   370,   371,
      21,    32,   355,   488,    73,    40,    56,    44,    45,    46,
      79,   467,    75,    79,    80,   471,    39,    80,    79,   475,
     373,    16,    45,    75,    76,    79,    44,    45,    46,   688,
      80,    49,    79,    75,    84,    82,    86,    74,    75,    76,
      77,    75,    76,    80,    23,    80,   399,   400,   576,    84,
     403,    86,   405,    48,   407,    34,    74,    75,    76,    77,
     440,    42,    80,    42,    87,    88,    89,    90,    91,    92,
      16,    14,    90,    91,   427,    56,   532,    72,    73,    40,
      26,   566,    25,    26,    16,    75,    81,    82,    35,   442,
     618,   619,    71,   662,   447,    40,    16,    19,    59,    80,
      81,    82,    48,    84,    51,    86,    69,    54,    79,   637,
      56,    82,   465,    56,    59,     1,    48,   686,    79,     1,
     576,   474,    16,    79,   477,    81,   695,    73,    48,   482,
      12,    52,    75,    19,    79,   488,    56,    80,    75,   492,
     625,    73,   598,    49,    30,    75,   499,    79,    34,    35,
      36,    79,   608,    73,    48,   508,   509,   510,    60,    16,
     688,   551,    44,    45,    46,    51,    52,    49,    54,    75,
      76,    16,    79,    16,   702,    82,    79,   662,    64,    73,
     665,    43,    68,    69,    40,    79,   539,   643,    79,    75,
      56,    48,    74,    75,    76,    77,    58,    81,    80,    79,
     685,   686,    82,    48,     1,    48,    75,   560,    90,    91,
     695,    56,    75,   566,    80,    12,    73,    40,    84,     4,
      86,    80,    35,   576,    81,    82,    46,    12,    73,    30,
      73,    93,    94,    53,   614,    75,    79,    57,    51,    35,
      36,    54,   614,    90,    91,    92,   702,    44,    45,    46,
      80,    79,    49,    81,    75,    51,    52,    79,    54,    44,
      45,    46,   652,   616,   654,   618,   619,    63,    75,    76,
      53,   624,   625,   626,    82,    83,    72,    74,    75,    76,
      77,    79,    80,    80,   637,     6,    84,   677,    86,    74,
      75,    76,    77,    90,    91,    80,    82,    83,    75,   689,
     680,    10,   655,     1,   694,    90,    91,    80,   680,   662,
     682,    84,   665,    86,    12,    75,    80,     9,   690,   709,
      84,    12,    86,    81,    82,   678,   698,    18,   700,    40,
      80,   711,   685,   686,    84,   688,    86,    56,    40,   711,
      12,    79,   695,    75,    76,    79,    44,    45,    46,   702,
      83,    49,    79,    44,    45,    46,    81,    82,    81,    82,
      40,    80,    81,    82,    12,    84,    57,    86,    81,    82,
      82,    83,    44,    45,    46,    79,    74,    75,    76,    77,
      74,    75,    80,    74,    75,    76,    77,    56,    12,    80,
      28,    29,    90,    91,    81,    82,    44,    45,    46,    90,
      91,    49,    74,    75,    76,    77,    12,    79,    80,    82,
      83,    80,    79,    81,    82,    84,    50,    86,    90,    91,
      44,    45,    46,    81,    82,    40,    74,    75,    76,    77,
      81,    82,    80,    14,   280,   281,    17,    30,    44,    45,
      46,    57,    90,    91,    25,    26,    75,    82,    81,    79,
      74,    75,    76,    77,    67,    28,    80,    39,    20,     1,
      41,    40,    43,    44,    18,    79,    90,    91,    74,    75,
      76,    77,    81,    80,    80,    56,    79,    19,    79,    21,
       9,    23,    59,    70,    90,    91,    28,    29,    30,    79,
      14,    33,    34,    17,     1,    37,    38,    67,    70,    80,
      42,    25,    26,    79,    46,    61,    75,    81,    79,    60,
      52,    44,    19,    55,    21,    40,    23,    41,    60,    43,
      44,    30,     1,    30,    40,    32,    33,    34,    75,    71,
      37,    38,    56,    75,    76,    42,    78,    81,    43,    46,
      19,    79,    21,    14,    23,    52,    47,    79,    55,    79,
      30,    30,    79,    60,    33,    34,    80,     1,    37,    38,
      42,    39,    79,    42,    71,    30,     4,    46,    75,    76,
      53,    78,     4,    52,     4,    19,    55,    21,     4,    23,
       4,    60,    79,    79,    57,    80,    30,    63,    57,    33,
      34,    70,    71,    37,    38,    38,    75,    76,    42,    78,
      21,    42,    46,    26,    72,    57,    83,    81,    52,    79,
      44,    55,    46,    79,    81,     1,    60,    79,    47,    79,
       4,    79,    53,    79,    79,    75,    70,    71,    79,    30,
      40,    75,    76,    19,    78,    21,    81,    23,    57,     4,
      74,    75,    76,    77,    30,    21,    80,    33,    34,   275,
       1,    37,    38,   612,    79,   711,    42,    79,   616,    79,
      46,    79,   440,   680,   680,   703,    52,   384,    19,    55,
      21,   135,    23,   399,    60,    58,   186,   252,     1,    30,
     186,   162,    33,    34,    70,    71,    37,    38,   188,    75,
      76,    42,    78,   407,   475,    46,    19,   301,    21,    26,
      23,    52,   206,   513,    55,   384,   384,    30,    21,    60,
      33,    34,    78,     1,    37,    38,    21,   369,   626,    42,
      71,   592,    -1,    46,    75,    76,   624,    78,     1,    52,
      98,    19,    55,    21,    -1,    23,    -1,    60,    -1,    -1,
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
      -1,    68,    69,    -1,    -1,    64,    -1,    30,    75,    68,
      69,    34,    35,    36,    -1,    30,    75,    -1,    -1,    34,
      35,    36,     1,    -1,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
      19,    64,    -1,    -1,    -1,    68,    69,     1,    -1,    64,
      -1,    -1,    75,    68,    69,    34,    35,    36,    -1,    -1,
      75,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    52,    -1,    54,    30,    -1,    -1,    -1,
      34,    35,    36,    -1,    -1,    64,    -1,    -1,    -1,    68,
      69,    -1,    -1,    -1,    -1,    -1,    75,    51,    52,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      64,    -1,    -1,    -1,    68,    69,    -1,    -1,    -1,    -1,
      -1,    75
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    96,   100,   248,     0,    35,    36,    51,    52,    54,
      63,    72,    97,   224,   225,   232,   233,   235,   236,   239,
     246,   249,   250,   252,   253,   262,   263,   269,   270,    75,
      76,   226,    20,    75,    75,    75,    80,    75,   104,    40,
      59,    79,   247,     1,    34,    51,    64,    68,    69,    97,
     101,   102,   103,   104,   108,   112,   153,   156,   157,   158,
     159,   224,   225,   233,   235,   239,   244,   245,   246,   255,
     256,   262,   272,   273,   274,   277,    79,    97,   250,   252,
     251,   100,     1,    51,    68,    72,   104,   225,   230,   236,
     244,   264,    79,    79,    80,   227,   228,    75,    40,   247,
      79,    80,   227,    75,    79,    82,    44,   271,    75,    76,
     160,   163,   164,   167,   168,    79,    79,    75,   161,    20,
      75,    75,    75,   160,   167,   243,    83,    19,   216,   158,
      40,   251,   100,   244,    75,    75,    35,    51,    54,    83,
      79,   247,    79,    79,   229,   230,    60,    40,    51,   154,
     155,   156,   225,   271,    79,    12,    44,    45,    46,    74,
      75,    77,    80,    90,    91,    98,    99,   160,   162,   170,
     171,   174,   177,   180,   181,   183,   185,   186,   187,   188,
     189,    81,    75,   160,    80,    84,    86,    69,    84,    86,
      75,    40,    80,   109,   142,    79,    82,    22,    32,   105,
       1,    21,    33,    37,    38,    46,    55,    60,    75,    78,
      97,   160,   188,   190,   191,   192,   193,   194,   195,   196,
     197,   203,   204,   207,   208,   214,   217,   219,   222,   223,
     234,   261,   278,    30,    63,    40,   247,    80,   142,   265,
     226,    75,    75,    39,    50,   231,    79,    79,    81,   160,
     153,    20,    53,   237,   156,    40,   186,   160,   188,   186,
      10,     1,    46,    49,   119,   149,   150,   151,   160,   166,
     172,   173,   174,   180,    81,    82,    84,    16,    48,    73,
     175,   176,     5,     7,     8,    39,    45,    87,    88,    89,
      90,    91,    92,   178,   179,   182,   183,    43,    58,    93,
      94,   184,     6,    51,   225,   233,   239,   254,   165,   166,
     173,   174,    14,    25,    26,    75,   169,    15,    75,   162,
     163,    18,    57,   174,   169,    75,    40,   113,   160,     1,
       4,   104,   143,   144,    40,   110,    75,   167,     9,    79,
     247,    17,   106,   113,   127,   128,   129,    79,   174,   160,
     220,    75,   174,   198,   199,   200,   201,    79,   220,    79,
     174,    83,   191,     9,    79,    79,    32,   191,   218,   257,
     100,    23,    34,    42,    71,   209,   210,   212,   215,   213,
     226,    79,    79,     4,    40,   227,    40,   227,    50,   161,
     230,   216,   240,    75,   154,    30,   174,    57,    10,    85,
      56,    84,   117,    82,    81,    82,    81,    72,    81,     3,
      79,    99,   187,    67,    28,   177,   177,    39,   180,   119,
     160,   180,   183,   185,   186,    40,    81,    82,   174,    18,
     275,    79,    63,    79,    26,   114,   115,   117,    81,    83,
      79,    81,    14,    25,    26,    41,    43,    44,    80,   111,
     116,   117,   120,   123,   124,   125,   126,   127,   136,   152,
     241,   242,    79,   174,    79,    80,   133,     9,   107,   247,
      40,    70,   221,    79,    28,    29,   202,   190,    67,    79,
      79,   174,    70,   258,   258,   205,   153,    75,   190,   201,
     213,    61,   211,   216,    79,    81,    25,    26,    43,    44,
      56,    80,   127,   152,   241,   242,   267,   268,    60,    44,
      40,   266,   107,    30,    40,    75,   238,    81,   174,   150,
     151,   174,   119,   160,    56,   166,   166,    46,   165,   180,
     166,    79,    43,   276,    79,   174,    14,   145,   144,    22,
     113,   174,   174,   174,   113,    74,    75,   121,   122,    46,
      53,    57,   137,    79,   119,   131,   132,   134,   135,   160,
      47,   174,    79,    79,   201,    79,   190,   199,    30,    79,
      49,    75,   160,   259,   260,    30,    70,   206,    39,    30,
     212,    79,   135,   160,    30,     4,     4,     4,   113,     4,
       4,    79,   160,   160,     4,   160,    79,   238,    80,    57,
      81,   174,    30,   161,   117,   118,   161,   113,    26,   117,
     118,    81,    82,    57,   100,    81,    82,    81,    82,    56,
     118,   113,   130,    38,    83,    10,    85,    21,   149,    42,
     212,   213,    26,    72,    81,   266,    79,    80,    79,   174,
      81,    79,    57,    18,   107,   174,   122,     1,    21,    46,
     104,   138,   139,   141,   146,    47,   132,   160,   135,     4,
      79,   259,   190,   260,    79,    10,    79,     4,    53,    79,
       4,    81,    79,   174,   118,    79,    75,    79,    83,    30,
     100,   140,   100,   130,    56,    10,   190,    81,    56,    40,
     100,   130,    57,   141,   146,   190,    79,   119,   100,   107,
     100,    79,    70,   147,   148,    79,   149,    30,   148,    10,
      21,   100,    79,   138
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
     162,   163,   164,   165,   165,   166,   166,   166,   166,   167,
     167,   167,   167,   168,   169,   169,   169,   169,   170,   170,
     170,   171,   171,   171,   171,   171,   172,   172,   173,   174,
     174,   174,   175,   175,   175,   176,   176,   177,   177,   177,
     177,   178,   178,   178,   178,   178,   178,   179,   179,   180,
     180,   180,   181,   181,   182,   182,   182,   183,   183,   184,
     184,   184,   184,   185,   185,   185,   185,   186,   186,   186,
     186,   186,   187,   187,   188,   189,   189,   190,   190,   191,
     191,   192,   192,   192,   193,   193,   193,   193,   193,   193,
     193,   193,   193,   194,   194,   194,   194,   195,   196,   197,
     198,   198,   199,   200,   201,   202,   202,   203,   204,   205,
     205,   206,   207,   208,   208,   209,   209,   209,   210,   211,
     211,   212,   213,   213,   214,   215,   215,   216,   217,   218,
     218,   219,   220,   220,   221,   221,   222,   222,   223,   224,
     224,   225,   225,   225,   226,   226,   227,   227,   228,   229,
     229,   230,   230,   231,   231,   231,   231,   232,   233,   234,
     235,   235,   236,   237,   237,   238,   238,   239,   240,   240,
     241,   242,   242,   243,   243,   243,   243,   244,   245,   245,
     245,   246,   246,   246,   246,   247,   248,   248,   249,   249,
     249,   250,   251,   251,   252,   252,   252,   252,   252,   252,
     252,   253,   254,   254,   255,   255,   256,   257,   257,   258,
     258,   259,   259,   260,   260,   261,   262,   262,   263,   263,
     264,   264,   264,   264,   264,   264,   264,   265,   265,   265,
     266,   266,   266,   267,   267,   267,   267,   267,   267,   267,
     267,   267,   267,   268,   268,   269,   270,   271,   272,   272,
     272,   273,   274,   275,   275,   276,   276,   277,   278
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
       1,     1,     4,     1,     3,     1,     1,     1,     1,     3,
       3,     3,     3,     3,     1,     1,     1,     1,     1,     1,
       1,     3,     3,     5,     6,     4,     3,     3,     3,     1,
       3,     3,     1,     1,     1,     2,     2,     1,     3,     3,
       3,     1,     1,     1,     1,     1,     1,     1,     2,     1,
       2,     3,     1,     1,     1,     1,     1,     1,     3,     1,
       1,     1,     1,     1,     2,     2,     3,     1,     1,     1,
       1,     1,     1,     3,     3,     2,     2,     1,     2,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     2,     4,     6,
       1,     3,     2,     2,     1,     0,     2,     6,     3,     0,
       2,     4,     4,     0,     2,     1,     3,     4,     3,     0,
       1,     4,     0,     1,     6,     0,     2,     2,     2,     0,
       1,     4,     0,     1,     0,     2,     2,     3,     3,     2,
       2,     3,     5,     2,     1,     1,     0,     1,     3,     1,
       3,     5,     1,     0,     1,     1,     2,     2,     6,     2,
       2,     2,     7,     0,     2,     0,     1,     9,     0,     1,
       2,     0,     1,     1,     1,     3,     3,     3,     6,     5,
       1,     4,     3,     5,     4,     2,     4,     3,     2,     3,
       2,     3,     0,     2,     1,     1,     1,     1,     1,     1,
       1,     5,     1,     1,     6,     4,     4,     2,     2,     4,
       6,     1,     3,     1,     1,     3,     3,     3,     1,     2,
       2,     6,     6,     8,    10,     7,     1,     0,     1,     3,
       0,     2,     2,     3,     2,     2,     2,     4,     2,     1,
       1,     1,     1,     2,     4,     3,     4,     2,     1,     1,
       1,     5,     9,     0,     4,     0,     7,     6,     2
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
#line 105 "grammar83.y"
{
    yylloc.file_id = context->file_id;
    yylloc.line_num = 1;
    // Silences annoying compiler warning
    (void)yynerrs;
}

#line 2403 "grammar83.tab.c"

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
  case 24: /* def_id_s: identifier  */
#line 160 "grammar83.y"
               {
        StringTokenArray_init(&(yyval.str_token_array));
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2626 "grammar83.tab.c"
    break;

  case 25: /* def_id_s: def_id_s ',' identifier  */
#line 164 "grammar83.y"
                            {
        (yyval.str_token_array) = (yyvsp[-2].str_token_array);
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2635 "grammar83.tab.c"
    break;

  case 331: /* with_clause: WITH def_id_s ';'  */
#line 920 "grammar83.y"
                      {
        uint32_t package_count = StringTokenArray_size(&(yyvsp[-1].str_token_array));
        for(uint32_t i = 0; i < package_count; ++i) {
            const char* package_name = string_pool_to_str((yyvsp[-1].str_token_array).data[i]);
            comp_manager_parse_spec(context->comp_manager, package_name, &(yyloc));
        }
    }
#line 2647 "grammar83.tab.c"
    break;


#line 2651 "grammar83.tab.c"

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

#line 1079 "grammar83.y"


void yyerror(YYLTYPE* yyloc, yyscan_t scanner, ParseContext* parse_ctx, const char* msg)
{
    (void)scanner;
    (void)parse_ctx;
    error_print(*yyloc, msg);
    error_exit();
}
