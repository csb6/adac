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
  YYSYMBOL_condition = 200,                /* condition  */
  YYSYMBOL_else_opt = 201,                 /* else_opt  */
  YYSYMBOL_case_stmt = 202,                /* case_stmt  */
  YYSYMBOL_case_hdr = 203,                 /* case_hdr  */
  YYSYMBOL_alternative_s = 204,            /* alternative_s  */
  YYSYMBOL_alternative = 205,              /* alternative  */
  YYSYMBOL_loop_stmt = 206,                /* loop_stmt  */
  YYSYMBOL_label_opt = 207,                /* label_opt  */
  YYSYMBOL_loop_content = 208,             /* loop_content  */
  YYSYMBOL_reverse_opt = 209,              /* reverse_opt  */
  YYSYMBOL_basic_loop = 210,               /* basic_loop  */
  YYSYMBOL_id_opt = 211,                   /* id_opt  */
  YYSYMBOL_block = 212,                    /* block  */
  YYSYMBOL_block_decl = 213,               /* block_decl  */
  YYSYMBOL_block_body = 214,               /* block_body  */
  YYSYMBOL_handled_stmt_s = 215,           /* handled_stmt_s  */
  YYSYMBOL_except_handler_part_opt = 216,  /* except_handler_part_opt  */
  YYSYMBOL_exit_stmt = 217,                /* exit_stmt  */
  YYSYMBOL_name_opt = 218,                 /* name_opt  */
  YYSYMBOL_when_opt = 219,                 /* when_opt  */
  YYSYMBOL_return_stmt = 220,              /* return_stmt  */
  YYSYMBOL_goto_stmt = 221,                /* goto_stmt  */
  YYSYMBOL_subprog_decl = 222,             /* subprog_decl  */
  YYSYMBOL_subprog_spec = 223,             /* subprog_spec  */
  YYSYMBOL_designator = 224,               /* designator  */
  YYSYMBOL_formal_part_opt = 225,          /* formal_part_opt  */
  YYSYMBOL_formal_part = 226,              /* formal_part  */
  YYSYMBOL_param_s = 227,                  /* param_s  */
  YYSYMBOL_param = 228,                    /* param  */
  YYSYMBOL_mode = 229,                     /* mode  */
  YYSYMBOL_subprog_spec_is_push = 230,     /* subprog_spec_is_push  */
  YYSYMBOL_subprog_body = 231,             /* subprog_body  */
  YYSYMBOL_procedure_call = 232,           /* procedure_call  */
  YYSYMBOL_pkg_decl = 233,                 /* pkg_decl  */
  YYSYMBOL_pkg_spec = 234,                 /* pkg_spec  */
  YYSYMBOL_private_part = 235,             /* private_part  */
  YYSYMBOL_identifier_opt = 236,           /* identifier_opt  */
  YYSYMBOL_pkg_body = 237,                 /* pkg_body  */
  YYSYMBOL_body_opt = 238,                 /* body_opt  */
  YYSYMBOL_private_type = 239,             /* private_type  */
  YYSYMBOL_limited_opt = 240,              /* limited_opt  */
  YYSYMBOL_use_name_s = 241,               /* use_name_s  */
  YYSYMBOL_use_clause = 242,               /* use_clause  */
  YYSYMBOL_rename_decl = 243,              /* rename_decl  */
  YYSYMBOL_rename_unit = 244,              /* rename_unit  */
  YYSYMBOL_comp_unit = 245,                /* comp_unit  */
  YYSYMBOL_context_spec = 246,             /* context_spec  */
  YYSYMBOL_with_clause = 247,              /* with_clause  */
  YYSYMBOL_use_clause_opt = 248,           /* use_clause_opt  */
  YYSYMBOL_unit = 249,                     /* unit  */
  YYSYMBOL_subunit = 250,                  /* subunit  */
  YYSYMBOL_subunit_body = 251,             /* subunit_body  */
  YYSYMBOL_body_stub = 252,                /* body_stub  */
  YYSYMBOL_exception_decl = 253,           /* exception_decl  */
  YYSYMBOL_except_handler_part = 254,      /* except_handler_part  */
  YYSYMBOL_exception_handler = 255,        /* exception_handler  */
  YYSYMBOL_except_choice_s = 256,          /* except_choice_s  */
  YYSYMBOL_except_choice = 257,            /* except_choice  */
  YYSYMBOL_raise_stmt = 258,               /* raise_stmt  */
  YYSYMBOL_generic_decl = 259,             /* generic_decl  */
  YYSYMBOL_generic_formal_part = 260,      /* generic_formal_part  */
  YYSYMBOL_generic_formal = 261,           /* generic_formal  */
  YYSYMBOL_generic_discrim_part_opt = 262, /* generic_discrim_part_opt  */
  YYSYMBOL_subp_default = 263,             /* subp_default  */
  YYSYMBOL_generic_type_def = 264,         /* generic_type_def  */
  YYSYMBOL_generic_derived_type = 265,     /* generic_derived_type  */
  YYSYMBOL_generic_subp_inst = 266,        /* generic_subp_inst  */
  YYSYMBOL_generic_pkg_inst = 267,         /* generic_pkg_inst  */
  YYSYMBOL_generic_inst = 268,             /* generic_inst  */
  YYSYMBOL_rep_spec = 269,                 /* rep_spec  */
  YYSYMBOL_attrib_def = 270,               /* attrib_def  */
  YYSYMBOL_record_type_spec = 271,         /* record_type_spec  */
  YYSYMBOL_align_opt = 272,                /* align_opt  */
  YYSYMBOL_comp_loc_s = 273,               /* comp_loc_s  */
  YYSYMBOL_address_spec = 274,             /* address_spec  */
  YYSYMBOL_code_stmt = 275                 /* code_stmt  */
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

#line 392 "grammar83.tab.c"

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
#define YYLAST   1327

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  95
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  181
/* YYNRULES -- Number of rules.  */
#define YYNRULES  395
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  716

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
     667,   668,   672,   676,   680,   681,   685,   689,   693,   694,
     698,   703,   707,   708,   712,   713,   714,   718,   719,   723,
     727,   728,   733,   737,   738,   742,   747,   751,   752,   756,
     760,   761,   765,   766,   770,   771,   775,   779,   780,   785,
     786,   787,   791,   792,   796,   797,   801,   805,   806,   810,
     811,   815,   816,   817,   818,   822,   828,   832,   836,   837,
     841,   844,   845,   849,   850,   854,   858,   859,   863,   867,
     868,   872,   873,   874,   875,   879,   884,   885,   886,   890,
     891,   892,   893,   897,   898,   902,   903,   904,   908,   917,
     918,   922,   923,   924,   925,   926,   927,   928,   932,   936,
     937,   941,   942,   946,   950,   951,   955,   956,   960,   961,
     965,   966,   970,   974,   975,   979,   980,   984,   985,   986,
     987,   988,   989,   990,   994,   995,   996,  1000,  1001,  1002,
    1006,  1007,  1008,  1009,  1010,  1011,  1012,  1013,  1014,  1015,
    1019,  1020,  1024,  1028,  1032,  1036,  1037,  1038,  1042,  1046,
    1050,  1051,  1055,  1056,  1060,  1064
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
  "cond_clause_s", "cond_clause", "condition", "else_opt", "case_stmt",
  "case_hdr", "alternative_s", "alternative", "loop_stmt", "label_opt",
  "loop_content", "reverse_opt", "basic_loop", "id_opt", "block",
  "block_decl", "block_body", "handled_stmt_s", "except_handler_part_opt",
  "exit_stmt", "name_opt", "when_opt", "return_stmt", "goto_stmt",
  "subprog_decl", "subprog_spec", "designator", "formal_part_opt",
  "formal_part", "param_s", "param", "mode", "subprog_spec_is_push",
  "subprog_body", "procedure_call", "pkg_decl", "pkg_spec", "private_part",
  "identifier_opt", "pkg_body", "body_opt", "private_type", "limited_opt",
  "use_name_s", "use_clause", "rename_decl", "rename_unit", "comp_unit",
  "context_spec", "with_clause", "use_clause_opt", "unit", "subunit",
  "subunit_body", "body_stub", "exception_decl", "except_handler_part",
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

#define YYPACT_NINF (-551)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-348)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -551,    69,   634,  -551,  -551,   277,  -551,    34,    -9,    15,
      -7,   156,  -551,  -551,   258,  1227,  -551,  -551,   103,  -551,
    -551,   634,  -551,  -551,  -551,  -551,   359,   154,   163,  -551,
    -551,    16,   171,    18,   339,   168,   180,  -551,   -18,   224,
     457,  -551,   215,   195,    94,   226,   237,   465,  -551,  -551,
    -551,  -551,   483,  -551,  -551,   302,  -551,   589,  -551,  -551,
    -551,   291,  -551,  -551,  -551,  -551,  -551,  -551,  -551,  -551,
    -551,  -551,  -551,  -551,  -551,  -551,  -551,  -551,  -551,   259,
     284,  -551,   283,   316,   354,   544,   107,   323,   327,  -551,
    -551,  -551,  -551,    45,   366,  -551,   415,  1093,   457,  -551,
     737,  -551,   390,  -551,   365,   457,  -551,  -551,  -551,   687,
    -551,  -551,  -551,  -551,  -551,  -551,   529,   443,   471,   441,
     262,   558,   381,   399,   313,  1078,   500,  -551,   162,   259,
     284,  -551,   275,   468,   277,   448,   478,   134,   457,  -551,
    -551,  -551,   115,  -551,   457,  1147,   133,   519,  1183,  -551,
     356,  -551,   736,   932,   457,   932,  -551,  -551,   576,  -551,
     376,  -551,  -551,   568,  -551,   746,  -551,  -551,  -551,    46,
    -551,   126,  1194,   389,  -551,   583,  -551,  -551,  -551,   645,
    -551,   558,  -551,   505,    74,   230,   671,    74,   516,   553,
     457,    80,   560,  -551,  -551,   598,   593,   132,    76,   525,
     760,   457,   542,   760,   549,   457,   712,   556,  1078,  -551,
      32,   557,   859,  -551,  -551,  -551,  -551,  -551,  -551,  -551,
    -551,  -551,  -551,   375,  -551,  -551,  -551,  -551,  -551,  -551,
    -551,  -551,   277,   567,  1191,   457,    81,  -551,   611,   168,
     616,   168,   618,  -551,   195,   762,    45,  -551,   558,   302,
     591,  1252,   648,  -551,   162,  -551,  -551,   746,  -551,  -551,
     760,  -551,   625,  -551,  -551,    38,  -551,   613,   436,   605,
     638,   610,   414,   469,   623,   737,   271,   647,   679,  -551,
     760,   760,  -551,  -551,  -551,  -551,   666,  -551,  -551,  -551,
    -551,  -551,  -551,   760,   760,  1194,   389,  -551,  -551,  -551,
    -551,  1194,   932,   692,   683,  -551,  -551,  -551,   640,  -551,
    -551,   208,  -551,  -551,  -551,  -551,  -551,  -551,  -551,  -551,
    -551,   760,   709,   391,  -551,  -551,  1139,   651,   536,  -551,
     656,   650,   128,  -551,   604,   663,   409,   419,   760,   457,
    -551,   697,   770,   731,  -551,  -551,  -551,  -551,   270,   558,
     730,   722,    46,   706,  -551,   754,  -551,   744,  -551,   396,
    -551,  -551,   760,  -551,  -551,   759,  -551,  -551,   759,   284,
    1227,   749,  1078,   760,   277,  -551,   302,   752,  -551,  -551,
     785,   757,   842,   773,   795,   804,  -551,    40,  -551,  -551,
    -551,   819,   815,  -551,   788,    46,   791,   760,   719,   760,
     213,  -551,   505,  -551,   505,  -551,   664,  -551,   760,  -551,
    -551,  -551,  -551,  -551,  -551,  -551,  -551,   520,  -551,   289,
      48,   389,  -551,  -551,  -551,  -551,   505,   400,   830,  -551,
    -551,   796,  -551,   760,  -551,  -551,  -551,  -551,   860,    55,
    -551,   317,   760,   760,  -551,   760,   457,   678,  -551,  -551,
    -551,  -551,  -551,  -551,  -551,  -551,  -551,  -551,  -551,  -551,
     490,  -551,   420,   852,   760,   834,   760,   798,   457,  -551,
     760,   808,  -551,  1078,   760,   864,  1078,  -551,  -551,   515,
     348,  -551,  -551,    33,  -551,   851,  1015,   862,   833,   883,
    -551,  -551,  -551,   911,   912,   914,   457,   923,   924,  -551,
    -551,  -551,   873,   861,  -551,   457,   457,   100,   865,  -551,
     788,   885,  -551,  -551,  -551,    46,  -551,  -551,    46,  -551,
     823,   866,  -551,  -551,   892,   688,   520,  -551,  -551,   760,
     120,  -551,   204,  -551,   195,  -551,   457,  -551,    49,   204,
      46,  -551,  -551,  -551,   703,  -551,   893,  -551,  -551,  -551,
    -551,  -551,  -551,   716,  -551,   726,  -551,   694,   457,    46,
    -551,   900,  -551,  -551,  1043,  -551,   913,   824,  -551,  -551,
     870,   558,    42,  -551,   933,   719,  -551,   905,   919,  -551,
    -551,   277,   941,  -551,  -551,   897,  -551,   889,  -551,   239,
     910,  -551,   558,  -551,   894,   760,   890,  -551,   526,   915,
      22,  -551,  -551,    40,  -551,   760,  -551,  -551,  -551,   678,
    -551,   189,   928,   457,  -551,   760,   493,  -551,  -551,  -551,
    -551,   904,   451,  1078,   451,   906,    57,  -551,   760,  -551,
     908,   984,   938,  -551,   916,  -551,   310,  -551,    44,  -551,
    -551,   925,   760,  -551,   204,  -551,   926,   927,   936,   677,
     981,  -551,  -551,  -551,   457,  -551,   551,  -551,  1233,  -551,
    -551,    60,   887,  -551,  -551,  1078,   862,  -551,  -551,  -551,
    -551,   939,  -551,  -551,   292,  -551,  -551,   979,  -551,   457,
     964,   191,  -551,   284,  -551,  1019,  1078,   922,  -551,   945,
     760,  -551,   284,   770,  -551,  -551,  -551,   980,  -551,   946,
     201,   948,   284,  -551,   719,   131,  -551,  -551,    62,  1007,
    -551,  -551,   950,   189,  -551,  -551
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       9,     0,     0,     2,     1,     0,   355,     0,     0,     0,
       0,     0,    10,   333,     0,     0,   334,   331,     0,   332,
     337,     0,   329,     9,   335,   336,     0,     0,     0,   282,
     283,   281,     0,     0,     0,   284,     0,    24,     0,   295,
       0,   277,     0,     0,     0,     0,     0,     0,   125,   122,
      11,    12,     0,    13,    14,     0,   129,     0,   126,   128,
      15,     0,   130,    16,   131,   123,    18,   318,    20,    17,
      19,   124,   385,   386,   387,   298,   327,   329,     9,   325,
     324,   290,     0,     0,     0,     0,     0,     0,     0,   363,
     356,   278,   299,     0,     0,   285,     0,     0,     0,     3,
       0,   279,     0,   328,     0,     0,   382,   132,   141,     0,
     136,   133,   134,   135,    21,   137,     0,     0,     0,    33,
     132,     0,   134,     0,    26,     0,     0,   127,   295,   326,
     323,   330,     0,   364,     0,     0,     0,   291,     0,   353,
     357,   354,     0,   287,     0,     0,     0,   301,     0,   120,
       0,   383,     0,     0,     0,     0,   160,   140,   132,   158,
       0,   192,   193,     0,     5,   208,   159,   207,   212,     7,
     169,   177,     0,   189,   197,   203,   211,   210,   209,     0,
      25,   384,   320,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    36,    34,   315,     0,    27,     0,     0,     0,
       0,   270,     0,     0,     0,   270,     0,   132,     0,   223,
       0,     0,     0,   217,   219,   221,   222,   224,   225,   233,
     234,     9,   235,   263,   236,   265,   226,   227,   228,   229,
     230,   231,   260,     0,     0,     0,     0,   365,     0,   284,
       0,   284,   292,   293,     0,     0,     0,   286,   280,   306,
       0,     0,     0,   121,     0,   319,   205,   215,   216,   204,
       0,   148,   160,   111,   113,     0,   107,   110,   208,     0,
       0,   146,   109,   177,     0,     0,     0,   172,   173,   174,
       0,     0,   184,   182,   186,   187,     0,   181,   183,   185,
     194,   195,   196,     0,     0,     0,   190,   201,   202,   199,
     200,     0,     0,     0,     0,   339,   340,   338,     0,   143,
     146,   109,   157,   156,   155,   154,   153,   152,   149,   150,
     151,     0,   390,     0,   138,   139,     0,     0,    48,   100,
       0,     0,     0,    97,   309,     0,   132,   134,     0,     0,
     343,     0,    30,    28,    29,    71,    72,   232,     0,   271,
     272,     0,   243,   244,   240,     0,   237,     0,   274,     0,
     253,   220,     0,   297,   395,     0,   218,   266,   268,   248,
       0,     0,     0,     0,   260,   254,     0,     0,   261,   342,
       0,     0,   309,     0,     0,   367,   294,    30,   322,   288,
     307,     0,     0,   302,   303,     8,     0,     0,     0,     0,
       0,   112,     0,   162,     0,   161,     0,   213,     0,     4,
       6,   214,   175,   176,   170,   171,   188,   178,   179,   180,
       0,   191,   198,   206,   295,   142,     0,     0,     0,   392,
     388,     0,    46,     0,    47,    50,    49,    35,   101,     0,
      96,     0,     0,     0,   310,     0,     0,     0,    37,    44,
      64,    38,    39,    40,    66,    67,    41,    42,    43,    45,
       0,    32,     0,     0,     0,     0,     0,     0,     0,   247,
       0,     0,   276,     0,     0,     0,     0,   352,   275,     0,
       0,   344,   345,     0,   264,     0,     0,     0,     0,     0,
     296,   321,   366,     0,     0,     0,     0,     0,     0,   376,
     377,   378,     0,     0,   379,     0,     0,     0,     0,   289,
     303,     0,   304,   300,   165,   168,   108,   110,   109,    53,
     208,    57,   166,   167,   160,     0,    56,   144,   394,     0,
       0,   341,    54,   102,     0,    98,     0,   114,     0,    54,
      65,    52,    63,    62,     0,    60,     0,   308,     9,    84,
      23,   317,    83,     0,    76,     0,    80,   208,     0,    31,
      22,     0,   273,   269,     0,   241,     0,     0,   238,   351,
     132,   350,     0,   348,     0,     0,   249,   257,     0,   255,
     251,   260,   373,   375,   372,   380,   371,     0,   358,   367,
       0,   369,   368,   359,     0,     0,     0,   163,     0,     0,
       0,    55,    51,    30,   115,     0,    69,    68,    59,     0,
      86,     0,     0,     0,    79,     0,     0,    82,    75,    74,
     316,     0,     0,     0,     0,     0,     0,   258,     0,   259,
       0,     0,     0,   370,     0,   362,     0,   305,     0,   164,
     391,     0,     0,    99,    54,    61,     0,     0,     0,     0,
       0,     9,    90,     9,     0,    77,     0,    81,   208,    78,
     239,     0,     0,   349,   246,     0,     0,   262,   374,   381,
     360,     0,    58,   389,     0,    70,    95,     0,     9,     0,
       0,     0,    87,    88,    73,     0,     0,     0,   256,     0,
       0,     9,    89,    30,    85,    91,     9,     0,   361,     0,
       0,     0,    93,   393,     0,     0,   104,    94,     0,     0,
     105,     9,     0,     0,   103,   106
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -551,  -551,    -2,  -551,   755,   -19,  -551,  -551,  -551,   -10,
    -551,  -551,  -380,  -551,  -551,  -551,  -551,  -551,  -187,  -551,
    -551,  -551,  -254,  -496,  -286,  -551,  -551,   422,  -551,  -551,
    -551,  -551,  -281,  -551,  -551,  -491,  -551,   424,  -551,  -551,
    -430,  -551,  -551,   320,  -551,  -551,   358,   909,  -551,   602,
    -551,   362,  -551,   341,  -550,   649,  -388,   672,   -13,   809,
    -551,   -77,  -551,  1002,  -551,    39,  -227,   878,   880,  -551,
     662,  -139,   -25,  -551,   882,  -551,  -551,  -551,   918,   -71,
    -551,  -551,   538,  -551,  -551,  -126,  -551,  -551,  -135,  -551,
     771,  -123,  -241,  -110,  -551,  -366,  -203,  -551,  -551,  -551,
    -551,  -551,  -551,  -551,   597,  -334,  -551,  -551,  -551,  -551,
    -551,  -551,  -551,  -551,  -551,  -469,  -362,  -551,  -551,  -218,
    -551,  -551,  -551,   869,  -551,  -551,  -551,    66,    24,    28,
     -11,  -551,  -551,   -66,  -551,  -551,    21,  -551,   412,  1056,
    -551,   573,    26,  -551,   702,   705,  -551,    12,  -551,   440,
    -551,  -551,  1067,  1023,  1071,  -551,  -551,  -551,  -551,  -551,
     728,   480,   481,  -551,   507,  -551,  -551,  -551,   517,  -551,
    -551,  -551,  -551,  1010,  -551,  -551,  -551,  -551,  -551,  -551,
    -551
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     1,   209,   163,   164,     2,    49,    50,    51,    52,
     198,   342,   467,    53,   192,   335,   448,    54,   618,   434,
     435,   449,   601,   617,   264,   451,   544,   545,   452,   453,
     454,   455,   344,   345,   346,   619,   553,   554,   465,   555,
     556,   457,   549,   650,   651,   682,   652,   193,   332,   333,
     534,   653,   705,   706,   265,   266,   267,   458,   249,   147,
     148,    56,    57,    58,    59,   165,   116,   166,   110,   111,
     308,   309,   112,   113,   316,   167,   168,   270,   310,   311,
     280,   281,   170,   293,   294,   171,   172,   295,   173,   301,
     174,   175,   176,   177,   178,   212,   213,   214,   215,   216,
     217,   218,   219,   353,   354,   355,   475,   220,   221,   483,
     576,   222,   223,   374,   628,   375,   377,   224,   376,   126,
     225,   367,   226,   350,   471,   227,   228,    60,    61,   378,
      94,    95,   142,    87,   244,    15,    62,   229,    63,    18,
     252,   513,    64,   391,   459,   460,   123,    65,    66,    67,
       3,    21,    22,    79,    23,    24,   307,    68,    69,   368,
     481,   572,   573,   230,    70,    26,    90,   238,   508,   503,
     504,    27,    28,   106,    71,    72,    73,   429,   530,    74,
     231
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      12,    38,    55,   327,    80,   361,   486,   509,   418,   366,
     517,   343,   488,    48,   401,   211,    85,   387,   579,    76,
     149,   269,   122,    16,   101,   626,    14,   143,    19,   169,
     256,   390,   259,    31,   273,   411,   602,   296,    89,   487,
     642,   362,    16,   607,   258,    14,    81,    19,   397,   466,
      86,   408,   623,   456,    32,    48,   329,   273,    97,   130,
     277,   103,   277,   574,   104,   277,    34,   665,    13,     4,
     686,   253,   711,    36,   436,   605,  -284,    98,    12,   109,
     450,   329,   329,    85,   330,   381,   121,    13,   312,   272,
      35,   131,   278,   341,   278,    48,    93,   278,   211,   313,
     314,   499,   211,   575,   591,   399,   187,   564,   188,    33,
     567,   363,   183,   519,   117,   323,   276,   279,   185,   279,
      37,   150,   279,   398,   187,   672,   188,   624,    12,   348,
      37,   282,   352,   283,   284,   359,   562,   152,   290,   291,
     292,   131,   398,    48,   181,   624,    48,   398,   675,   315,
     599,   107,   108,   250,   708,    37,    37,   149,   489,   411,
     421,   709,   239,   684,   210,   285,   138,   417,   420,    33,
     337,   286,   150,   242,   149,   107,   108,   245,   552,   423,
     389,   331,    75,   248,   243,   657,   139,   517,   693,   395,
     646,   339,   646,   257,   246,   115,   247,   688,   666,   268,
     305,   704,   369,   304,   169,   306,   105,   439,    33,   440,
     647,   340,   647,   287,   288,   289,   290,   291,   292,   630,
     277,   -92,   268,   643,   277,   233,   331,   312,   383,   328,
     385,    37,    48,    91,   121,   648,    85,   328,   313,   314,
     349,     8,    92,     8,   349,   317,    96,   210,    93,    48,
     427,   210,   278,     8,   537,   102,   278,   662,   150,   541,
     399,   -92,   211,   522,    37,   523,    37,   462,   105,   521,
     115,   704,   273,   420,   380,   150,   273,   279,   273,   507,
     273,   279,   526,   366,   606,   312,   277,   527,   315,  -145,
    -145,   479,  -208,   160,   114,  -208,   313,   314,    39,   687,
     273,   118,   352,   600,   157,   318,   108,   603,   277,   585,
     469,   261,   119,   701,   671,   234,   517,    40,   278,   183,
     697,   125,   153,   184,    48,   185,   515,   518,    47,   552,
     519,   128,  -208,   419,   235,   196,     8,    41,   420,   536,
     278,  -311,   552,   279,  -311,   197,   315,  -208,   690,   604,
      40,   160,    29,    30,   154,   155,   156,   484,   132,   263,
      81,   366,   532,   211,   366,   279,   211,    12,    48,   183,
      41,   538,   539,   400,   540,   185,   211,   261,   463,  -208,
    -208,  -208,  -208,  -208,   157,   107,   108,   159,   153,   134,
     160,   133,   107,   108,     5,   559,   254,   569,   370,   352,
     161,   162,   140,   352,   699,   135,   141,   277,   136,   371,
      82,   210,   277,     9,    17,    40,   277,   372,    99,   100,
     154,   155,   262,   570,   108,   263,   144,    83,    47,   331,
     277,    84,   297,    17,    37,    41,   277,   268,   520,   278,
     180,   268,    20,   268,   278,   268,   373,   298,   278,   273,
     157,   107,   108,   159,   211,   145,   160,   211,   598,   366,
    -312,    20,   278,  -312,   279,   268,   161,   162,   278,   279,
     430,   179,   408,   279,   282,   478,   283,   284,   194,   528,
     328,   195,   299,   300,   366,   328,   406,   279,  -313,   420,
     420,  -313,   399,   279,   366,   407,  -145,   659,  -314,   550,
     569,  -314,   420,   557,   518,   153,   261,   561,   285,    25,
     273,   190,   210,   211,   286,   210,   183,   153,   189,   571,
     400,   191,   185,   240,   638,   210,   107,   108,    25,   611,
     232,   277,   107,   108,   644,   328,   546,   154,   155,   156,
     120,   108,   277,   547,   589,   590,   592,   548,   236,   154,
     155,   156,   211,   241,   263,   211,   287,   288,   289,   290,
     291,   292,   433,   278,   420,   104,   124,   157,   107,   108,
     159,   674,   251,   160,   278,   328,   211,   211,   273,   157,
     107,   108,   159,   161,   162,   160,   260,   211,   279,   302,
      42,   325,   399,   326,   568,   161,   162,   328,   186,   279,
     334,   649,   338,   210,   347,   640,   210,   685,  -117,    12,
     290,   291,   292,   187,   268,   188,   183,   351,   441,  -117,
     184,   341,   185,    43,     5,     6,   104,   137,   356,   442,
     443,   183,   681,   518,   683,   184,   364,   185,   183,   360,
      44,     8,   184,     9,   185,   444,   379,   445,   446,   274,
     275,   382,   656,    45,   658,   520,   384,    46,    47,   692,
     399,   571,   210,   571,    37,   261,   392,   658,   386,     5,
       6,   649,   700,   336,   108,   268,   153,   702,   394,    12,
       5,    12,   396,   153,   447,     7,     8,   402,     9,   321,
      12,   405,   713,   328,  -147,  -147,   303,    10,    12,     9,
      12,   210,   409,   649,   210,   416,    11,   413,   154,   155,
     524,    12,    32,   263,   412,   154,   155,   156,   328,   403,
     404,   425,   426,   424,   153,   210,   210,   428,   322,   520,
     432,   153,   104,   438,   473,   474,   210,   437,   157,   107,
     108,   159,   461,   268,   160,   157,   107,   108,   159,   153,
     616,   160,   542,   543,   161,   162,   154,   155,   156,   104,
     679,   161,   162,   154,   155,   156,   182,   183,   263,   597,
     426,   184,   153,   185,   183,   -54,   -54,   464,   400,   466,
     185,   154,   155,   156,   608,   609,   157,   107,   108,   159,
     468,   358,   160,   157,   107,   108,   159,   612,   613,   160,
     470,   472,   161,   162,   154,   155,   156,   614,   615,   161,
     162,   157,   158,   108,   159,   255,   183,   160,   414,   415,
     184,   476,   185,   477,   485,   199,   183,   161,   162,   480,
     276,   490,   185,   505,   157,   107,   108,   159,   492,   506,
     160,   388,   183,  -252,   507,   200,   184,  -252,   185,   510,
     161,   162,  -242,  -242,  -242,   511,   441,   201,  -252,   341,
     199,   202,   203,   512,   491,   183,  -252,   493,   494,   184,
     204,   185,   514,   529,   533,   531,     8,   560,  -252,   205,
     200,   558,  -252,   444,   206,   495,   496,   563,   199,  -267,
     577,   365,   201,  -252,   566,  -252,   202,   203,   497,   207,
     108,  -252,   208,   183,   372,   204,  -252,   400,   200,   185,
    -252,     8,   580,   581,   205,   582,   583,  -346,   584,   206,
     201,  -252,   498,   199,   202,   203,   547,   586,   587,  -252,
    -252,   551,   183,   204,   207,   108,   184,   208,   185,     8,
     588,  -252,   205,   200,   593,  -252,   595,   206,   431,   596,
     610,   621,  -250,   622,   625,   201,  -252,  -346,  -252,   202,
     203,   629,   207,   108,  -252,   208,   627,   631,   204,   632,
     633,   639,   641,   637,     8,   654,   154,   205,   156,   620,
     183,   199,   206,   660,   184,   664,   185,   667,   668,   635,
     636,   669,  -250,  -252,   184,   670,   185,   207,   108,  -252,
     208,   200,   677,  -252,   673,   676,   157,   107,   108,   159,
    -347,   680,   160,   201,  -252,   678,   199,   202,   203,   691,
     689,   694,  -252,   659,   698,   703,   204,   707,   712,   714,
     410,   645,     8,   715,  -252,   205,   200,   655,  -252,   695,
     206,   535,   237,   696,   199,   578,   710,   516,   201,  -252,
    -347,  -252,   202,   203,   500,   207,   108,  -252,   208,   127,
     393,   204,  -252,   319,   200,   320,  -252,     8,   525,   324,
     205,   565,   422,  -245,   357,   206,   201,  -252,   271,   199,
     202,   203,    88,   594,   501,  -252,  -252,   502,    77,   204,
     207,   108,    78,   208,    42,     8,   482,  -252,   205,   200,
     129,  -252,   661,   206,     0,   663,   634,   151,     0,     0,
       0,   201,  -252,     0,  -252,   202,   203,     0,   207,   108,
    -252,   208,     0,  -118,   204,     0,     0,    43,     5,     6,
       8,     0,     0,   205,     0,     0,     0,   105,   206,     0,
      42,     0,     0,     0,   146,     8,  -118,     9,    42,  -252,
       0,     0,     0,   207,   108,     0,   208,    45,  -116,     0,
       0,    46,    47,     0,     0,     0,  -116,     0,    37,  -116,
       0,     0,     0,    43,     5,     6,     0,  -116,     0,     0,
       0,    43,     5,     6,    42,     0,     0,     0,     0,     0,
      44,     8,    42,     9,     0,     0,     0,     0,    44,     8,
       0,     9,   431,    45,     0,     0,   153,    46,    47,     0,
       0,    45,     0,  -119,    37,    46,    47,    43,     5,     6,
       0,  -118,    37,     0,     0,    43,     5,     6,    42,     0,
       0,     0,     0,     0,   146,     8,  -119,     9,   154,   155,
     156,     0,   146,     8,  -118,     9,  -116,    45,     0,     0,
       0,    46,    47,    42,     0,    45,     0,     0,    37,    46,
      47,    43,     5,     6,     0,     0,    37,     0,   157,   107,
     108,   159,     0,     0,   160,   -54,     0,     0,    44,     8,
       0,     9,  -118,     0,     0,     0,    43,     5,     6,   399,
       0,    45,     0,     0,     0,    46,    47,     0,     0,     0,
       0,     0,    37,   146,     8,     0,     9,     0,     0,     0,
       0,     0,     0,   183,   -54,   -54,    45,   400,     0,   185,
      46,    47,     0,     0,     0,     0,     0,    37
};

static const yytype_int16 yycheck[] =
{
       2,    11,    15,   190,    23,   208,   372,   387,   294,   212,
     398,   198,   374,    15,   268,   125,    26,   244,   487,    21,
      97,   160,    47,     2,    35,   575,     2,    93,     2,   100,
     153,   249,   155,     5,   160,   276,   532,   172,    26,   373,
      18,     9,    21,   539,   154,    21,     1,    21,    10,     9,
      26,     3,    10,   334,    20,    57,     1,   183,    40,    78,
      16,    79,    16,    30,    82,    16,    75,    10,     2,     0,
      10,   148,    10,    80,   328,    26,    60,    59,    80,    40,
     334,     1,     1,    93,     4,     4,    47,    21,    14,   160,
      75,    79,    48,    17,    48,    97,    80,    48,   208,    25,
      26,   382,   212,    70,     4,    56,    84,   473,    86,    75,
     476,    79,    80,   399,    20,   186,    84,    73,    86,    73,
      75,    97,    73,    85,    84,    81,    86,    85,   130,   200,
      75,     5,   203,     7,     8,   206,   470,    98,    90,    91,
      92,   129,    85,   145,   105,    85,   148,    85,   644,    75,
      30,    75,    76,    20,   704,    75,    75,   234,   376,   400,
     295,    30,   134,   654,   125,    39,    59,   293,   294,    75,
     195,    45,   148,    39,   251,    75,    76,   138,   464,   302,
     246,   191,    79,   144,    50,   615,    79,   575,   679,   260,
       1,    59,     1,   154,    79,    75,    81,   666,   628,   160,
     179,    70,   221,   179,   275,   179,    44,    79,    75,    81,
      21,    79,    21,    87,    88,    89,    90,    91,    92,   581,
      16,    30,   183,   603,    16,    63,   236,    14,   239,   190,
     241,    75,   234,    79,   195,    46,   246,   198,    25,    26,
     201,    52,    79,    52,   205,    15,    75,   208,    80,   251,
     321,   212,    48,    52,   441,    75,    48,   623,   234,   446,
      56,    70,   372,   402,    75,   404,    75,   338,    44,    56,
      75,    70,   398,   399,   235,   251,   402,    73,   404,    40,
     406,    73,   408,   486,   538,    14,    16,   426,    75,    81,
      82,   362,     3,    80,    79,     6,    25,    26,    40,   665,
     426,    75,   373,   530,    74,    75,    76,   534,    16,   496,
      40,     1,    75,   693,     4,    40,   704,    59,    48,    80,
     686,    19,    12,    84,   326,    86,   397,   398,    69,   615,
     616,    40,    43,   294,    59,    22,    52,    79,   464,    22,
      48,    79,   628,    73,    82,    32,    75,    58,    56,   536,
      59,    80,    75,    76,    44,    45,    46,   370,    75,    49,
       1,   564,   433,   473,   567,    73,   476,   369,   370,    80,
      79,   442,   443,    84,   445,    86,   486,     1,   339,    90,
      91,    92,    93,    94,    74,    75,    76,    77,    12,    35,
      80,    75,    75,    76,    35,   466,    40,    49,    23,   470,
      90,    91,    79,   474,   690,    51,    79,    16,    54,    34,
      51,   372,    16,    54,     2,    59,    16,    42,    79,    80,
      44,    45,    46,    75,    76,    49,    60,    68,    69,   439,
      16,    72,    43,    21,    75,    79,    16,   398,   399,    48,
      75,   402,     2,   404,    48,   406,    71,    58,    48,   575,
      74,    75,    76,    77,   564,    40,    80,   567,   529,   662,
      79,    21,    48,    82,    73,   426,    90,    91,    48,    73,
      79,    81,     3,    73,     5,    79,     7,     8,    79,    79,
     441,    82,    93,    94,   687,   446,    72,    73,    79,   615,
     616,    82,    56,    73,   697,    81,    82,     4,    79,    79,
      49,    82,   628,   464,   575,    12,     1,   468,    39,     2,
     636,    40,   473,   623,    45,   476,    80,    12,    75,   480,
      84,    80,    86,    75,   595,   486,    75,    76,    21,   548,
      30,    16,    75,    76,   605,   496,    46,    44,    45,    46,
      75,    76,    16,    53,   505,   506,   507,    57,    80,    44,
      45,    46,   662,    75,    49,   665,    87,    88,    89,    90,
      91,    92,    26,    48,   690,    82,    83,    74,    75,    76,
      77,   642,    53,    80,    48,   536,   686,   687,   704,    74,
      75,    76,    77,    90,    91,    80,    10,   697,    73,     6,
       1,    75,    56,    40,    79,    90,    91,   558,    69,    73,
      40,   611,     9,   564,    79,    79,   567,    56,    19,   611,
      90,    91,    92,    84,   575,    86,    80,    75,    14,    30,
      84,    17,    86,    34,    35,    36,    82,    83,    79,    25,
      26,    80,   651,   704,   653,    84,    79,    86,    80,    83,
      51,    52,    84,    54,    86,    41,    79,    43,    44,    81,
      82,    40,   613,    64,   615,   616,    40,    68,    69,   678,
      56,   622,   623,   624,    75,     1,    75,   628,    50,    35,
      36,   681,   691,    75,    76,   636,    12,   696,    30,   681,
      35,   683,    57,    12,    80,    51,    52,    82,    54,    18,
     692,    81,   711,   654,    81,    82,    51,    63,   700,    54,
     702,   662,    79,   713,   665,    39,    72,    28,    44,    45,
      46,   713,    20,    49,    67,    44,    45,    46,   679,    81,
      82,    81,    82,    40,    12,   686,   687,    18,    57,   690,
      79,    12,    82,    83,    28,    29,   697,    81,    74,    75,
      76,    77,    79,   704,    80,    74,    75,    76,    77,    12,
      56,    80,    74,    75,    90,    91,    44,    45,    46,    82,
      83,    90,    91,    44,    45,    46,    79,    80,    49,    81,
      82,    84,    12,    86,    80,    81,    82,    80,    84,     9,
      86,    44,    45,    46,    81,    82,    74,    75,    76,    77,
      59,    79,    80,    74,    75,    76,    77,    81,    82,    80,
      70,    79,    90,    91,    44,    45,    46,    81,    82,    90,
      91,    74,    75,    76,    77,    79,    80,    80,   280,   281,
      84,    67,    86,    79,    75,     1,    80,    90,    91,    70,
      84,    79,    86,    60,    74,    75,    76,    77,    81,    44,
      80,    79,    80,    19,    40,    21,    84,    23,    86,    30,
      90,    91,    28,    29,    30,    40,    14,    33,    34,    17,
       1,    37,    38,    75,    79,    80,    42,    25,    26,    84,
      46,    86,    81,    43,    14,    79,    52,    79,    19,    55,
      21,    47,    23,    41,    60,    43,    44,    79,     1,    30,
      39,    32,    33,    34,    30,    71,    37,    38,    56,    75,
      76,    42,    78,    80,    42,    46,    19,    84,    21,    86,
      23,    52,    79,    30,    55,     4,     4,    30,     4,    60,
      33,    34,    80,     1,    37,    38,    53,     4,     4,    42,
      71,    79,    80,    46,    75,    76,    84,    78,    86,    52,
      79,    19,    55,    21,    79,    23,    80,    60,    63,    57,
      57,    38,    30,    83,    21,    33,    34,    70,    71,    37,
      38,    42,    75,    76,    42,    78,    61,    26,    46,    72,
      81,    81,    57,    79,    52,    47,    44,    55,    46,    79,
      80,     1,    60,    79,    84,    79,    86,    79,     4,    79,
      80,    53,    70,    71,    84,    79,    86,    75,    76,    19,
      78,    21,    75,    23,    79,    79,    74,    75,    76,    77,
      30,    30,    80,    33,    34,    79,     1,    37,    38,    40,
      81,    57,    42,     4,    79,    79,    46,    79,    21,    79,
     275,   609,    52,   713,    19,    55,    21,   613,    23,   681,
      60,   439,   133,   681,     1,    30,   705,   398,    33,    34,
      70,    71,    37,    38,   382,    75,    76,    42,    78,    57,
     251,    46,    19,   185,    21,   185,    23,    52,   406,   187,
      55,   474,   301,    30,   205,    60,    33,    34,   160,     1,
      37,    38,    26,   510,   382,    42,    71,   382,    21,    46,
      75,    76,    21,    78,     1,    52,   368,    19,    55,    21,
      77,    23,   622,    60,    -1,   624,   589,    97,    -1,    -1,
      -1,    33,    34,    -1,    71,    37,    38,    -1,    75,    76,
      42,    78,    -1,    30,    46,    -1,    -1,    34,    35,    36,
      52,    -1,    -1,    55,    -1,    -1,    -1,    44,    60,    -1,
       1,    -1,    -1,    -1,    51,    52,    53,    54,     1,    71,
      -1,    -1,    -1,    75,    76,    -1,    78,    64,    19,    -1,
      -1,    68,    69,    -1,    -1,    -1,    19,    -1,    75,    30,
      -1,    -1,    -1,    34,    35,    36,    -1,    30,    -1,    -1,
      -1,    34,    35,    36,     1,    -1,    -1,    -1,    -1,    -1,
      51,    52,     1,    54,    -1,    -1,    -1,    -1,    51,    52,
      -1,    54,    63,    64,    -1,    -1,    12,    68,    69,    -1,
      -1,    64,    -1,    30,    75,    68,    69,    34,    35,    36,
      -1,    30,    75,    -1,    -1,    34,    35,    36,     1,    -1,
      -1,    -1,    -1,    -1,    51,    52,    53,    54,    44,    45,
      46,    -1,    51,    52,    53,    54,    19,    64,    -1,    -1,
      -1,    68,    69,     1,    -1,    64,    -1,    -1,    75,    68,
      69,    34,    35,    36,    -1,    -1,    75,    -1,    74,    75,
      76,    77,    -1,    -1,    80,    42,    -1,    -1,    51,    52,
      -1,    54,    30,    -1,    -1,    -1,    34,    35,    36,    56,
      -1,    64,    -1,    -1,    -1,    68,    69,    -1,    -1,    -1,
      -1,    -1,    75,    51,    52,    -1,    54,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    81,    82,    64,    84,    -1,    86,
      68,    69,    -1,    -1,    -1,    -1,    -1,    75
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    96,   100,   245,     0,    35,    36,    51,    52,    54,
      63,    72,    97,   222,   223,   230,   231,   233,   234,   237,
     244,   246,   247,   249,   250,   259,   260,   266,   267,    75,
      76,   224,    20,    75,    75,    75,    80,    75,   104,    40,
      59,    79,     1,    34,    51,    64,    68,    69,    97,   101,
     102,   103,   104,   108,   112,   153,   156,   157,   158,   159,
     222,   223,   231,   233,   237,   242,   243,   244,   252,   253,
     259,   269,   270,   271,   274,    79,    97,   247,   249,   248,
     100,     1,    51,    68,    72,   104,   223,   228,   234,   242,
     261,    79,    79,    80,   225,   226,    75,    40,    59,    79,
      80,   225,    75,    79,    82,    44,   268,    75,    76,   160,
     163,   164,   167,   168,    79,    75,   161,    20,    75,    75,
      75,   160,   167,   241,    83,    19,   214,   158,    40,   248,
     100,   242,    75,    75,    35,    51,    54,    83,    59,    79,
      79,    79,   227,   228,    60,    40,    51,   154,   155,   156,
     223,   268,   160,    12,    44,    45,    46,    74,    75,    77,
      80,    90,    91,    98,    99,   160,   162,   170,   171,   174,
     177,   180,   181,   183,   185,   186,   187,   188,   189,    81,
      75,   160,    79,    80,    84,    86,    69,    84,    86,    75,
      40,    80,   109,   142,    79,    82,    22,    32,   105,     1,
      21,    33,    37,    38,    46,    55,    60,    75,    78,    97,
     160,   188,   190,   191,   192,   193,   194,   195,   196,   197,
     202,   203,   206,   207,   212,   215,   217,   220,   221,   232,
     258,   275,    30,    63,    40,    59,    80,   142,   262,   224,
      75,    75,    39,    50,   229,   160,    79,    81,   160,   153,
      20,    53,   235,   156,    40,    79,   186,   160,   188,   186,
      10,     1,    46,    49,   119,   149,   150,   151,   160,   166,
     172,   173,   174,   180,    81,    82,    84,    16,    48,    73,
     175,   176,     5,     7,     8,    39,    45,    87,    88,    89,
      90,    91,    92,   178,   179,   182,   183,    43,    58,    93,
      94,   184,     6,    51,   223,   231,   237,   251,   165,   166,
     173,   174,    14,    25,    26,    75,   169,    15,    75,   162,
     163,    18,    57,   174,   169,    75,    40,   113,   160,     1,
       4,   104,   143,   144,    40,   110,    75,   167,     9,    59,
      79,    17,   106,   113,   127,   128,   129,    79,   174,   160,
     218,    75,   174,   198,   199,   200,    79,   218,    79,   174,
      83,   191,     9,    79,    79,    32,   191,   216,   254,   100,
      23,    34,    42,    71,   208,   210,   213,   211,   224,    79,
     160,     4,    40,   225,    40,   225,    50,   161,    79,   228,
     214,   238,    75,   154,    30,   174,    57,    10,    85,    56,
      84,   117,    82,    81,    82,    81,    72,    81,     3,    79,
      99,   187,    67,    28,   177,   177,    39,   180,   119,   160,
     180,   183,   185,   186,    40,    81,    82,   174,    18,   272,
      79,    63,    79,    26,   114,   115,   117,    81,    83,    79,
      81,    14,    25,    26,    41,    43,    44,    80,   111,   116,
     117,   120,   123,   124,   125,   126,   127,   136,   152,   239,
     240,    79,   174,   160,    80,   133,     9,   107,    59,    40,
      70,   219,    79,    28,    29,   201,    67,    79,    79,   174,
      70,   255,   255,   204,   153,    75,   190,   200,   211,   214,
      79,    79,    81,    25,    26,    43,    44,    56,    80,   127,
     152,   239,   240,   264,   265,    60,    44,    40,   263,   107,
      30,    40,    75,   236,    81,   174,   150,   151,   174,   119,
     160,    56,   166,   166,    46,   165,   180,   166,    79,    43,
     273,    79,   174,    14,   145,   144,    22,   113,   174,   174,
     174,   113,    74,    75,   121,   122,    46,    53,    57,   137,
      79,    79,   119,   131,   132,   134,   135,   160,    47,   174,
      79,   160,   200,    79,   190,   199,    30,   190,    79,    49,
      75,   160,   256,   257,    30,    70,   205,    39,    30,   210,
      79,    30,     4,     4,     4,   113,     4,     4,    79,   160,
     160,     4,   160,    79,   236,    80,    57,    81,   174,    30,
     161,   117,   118,   161,   113,    26,   117,   118,    81,    82,
      57,   100,    81,    82,    81,    82,    56,   118,   113,   130,
      79,    38,    83,    10,    85,    21,   149,    61,   209,    42,
     211,    26,    72,    81,   263,    79,    80,    79,   174,    81,
      79,    57,    18,   107,   174,   122,     1,    21,    46,   104,
     138,   139,   141,   146,    47,   132,   160,   135,   160,     4,
      79,   256,   190,   257,    79,    10,   135,    79,     4,    53,
      79,     4,    81,    79,   174,   118,    79,    75,    79,    83,
      30,   100,   140,   100,   130,    56,    10,   190,   210,    81,
      56,    40,   100,   130,    57,   141,   146,   190,    79,   119,
     100,   107,   100,    79,    70,   147,   148,    79,   149,    30,
     148,    10,    21,   100,    79,   138
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
     198,   198,   199,   200,   201,   201,   202,   203,   204,   204,
     205,   206,   207,   207,   208,   208,   208,   209,   209,   210,
     211,   211,   212,   213,   213,   214,   215,   216,   216,   217,
     218,   218,   219,   219,   220,   220,   221,   222,   222,   223,
     223,   223,   224,   224,   225,   225,   226,   227,   227,   228,
     228,   229,   229,   229,   229,   230,   231,   232,   233,   233,
     234,   235,   235,   236,   236,   237,   238,   238,   239,   240,
     240,   241,   241,   241,   241,   242,   243,   243,   243,   244,
     244,   244,   244,   245,   245,   246,   246,   246,   247,   248,
     248,   249,   249,   249,   249,   249,   249,   249,   250,   251,
     251,   252,   252,   253,   254,   254,   255,   255,   256,   256,
     257,   257,   258,   259,   259,   260,   260,   261,   261,   261,
     261,   261,   261,   261,   262,   262,   262,   263,   263,   263,
     264,   264,   264,   264,   264,   264,   264,   264,   264,   264,
     265,   265,   266,   267,   268,   269,   269,   269,   270,   271,
     272,   272,   273,   273,   274,   275
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
       1,     3,     3,     1,     0,     2,     6,     3,     0,     2,
       4,     4,     0,     2,     1,     3,     6,     0,     1,     4,
       0,     1,     6,     0,     2,     2,     2,     0,     1,     4,
       0,     1,     0,     2,     2,     3,     3,     2,     2,     3,
       5,     2,     1,     1,     0,     1,     3,     1,     3,     5,
       1,     0,     1,     1,     2,     2,     6,     2,     2,     2,
       7,     0,     2,     0,     1,     9,     0,     1,     2,     0,
       1,     1,     1,     3,     3,     3,     7,     6,     1,     5,
       4,     6,     5,     4,     3,     2,     3,     2,     3,     0,
       2,     1,     1,     1,     1,     1,     1,     1,     5,     1,
       1,     6,     4,     4,     2,     2,     4,     6,     1,     3,
       1,     1,     3,     3,     3,     1,     2,     2,     6,     6,
       8,    10,     7,     1,     0,     1,     3,     0,     2,     2,
       3,     2,     2,     2,     4,     2,     1,     1,     1,     1,
       2,     4,     3,     4,     2,     1,     1,     1,     5,     9,
       0,     4,     0,     7,     6,     2
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

#line 2397 "grammar83.tab.c"

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
#line 2620 "grammar83.tab.c"
    break;

  case 25: /* def_id_s: def_id_s ',' identifier  */
#line 164 "grammar83.y"
                            {
        (yyval.str_token_array) = (yyvsp[-2].str_token_array);
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2629 "grammar83.tab.c"
    break;

  case 328: /* with_clause: WITH def_id_s ';'  */
#line 908 "grammar83.y"
                      {
        uint32_t package_count = StringTokenArray_size(&(yyvsp[-1].str_token_array));
        for(uint32_t i = 0; i < package_count; ++i) {
            const char* package_name = string_pool_to_str((yyvsp[-1].str_token_array).data[i]);
            comp_manager_parse_spec(context->comp_manager, package_name, &(yyloc));
        }
    }
#line 2641 "grammar83.tab.c"
    break;


#line 2645 "grammar83.tab.c"

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

#line 1067 "grammar83.y"


void yyerror(YYLTYPE* yyloc, yyscan_t scanner, ParseContext* parse_ctx, const char* msg)
{
    (void)scanner;
    (void)parse_ctx;
    error_print(*yyloc, msg);
    error_exit();
}
