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
  YYSYMBOL_reverse_opt = 210,              /* reverse_opt  */
  YYSYMBOL_basic_loop = 211,               /* basic_loop  */
  YYSYMBOL_id_opt = 212,                   /* id_opt  */
  YYSYMBOL_block = 213,                    /* block  */
  YYSYMBOL_block_decl = 214,               /* block_decl  */
  YYSYMBOL_block_body = 215,               /* block_body  */
  YYSYMBOL_handled_stmt_s = 216,           /* handled_stmt_s  */
  YYSYMBOL_except_handler_part_opt = 217,  /* except_handler_part_opt  */
  YYSYMBOL_exit_stmt = 218,                /* exit_stmt  */
  YYSYMBOL_name_opt = 219,                 /* name_opt  */
  YYSYMBOL_when_opt = 220,                 /* when_opt  */
  YYSYMBOL_return_stmt = 221,              /* return_stmt  */
  YYSYMBOL_goto_stmt = 222,                /* goto_stmt  */
  YYSYMBOL_subprog_decl = 223,             /* subprog_decl  */
  YYSYMBOL_subprog_spec = 224,             /* subprog_spec  */
  YYSYMBOL_designator = 225,               /* designator  */
  YYSYMBOL_formal_part_opt = 226,          /* formal_part_opt  */
  YYSYMBOL_formal_part = 227,              /* formal_part  */
  YYSYMBOL_param_s = 228,                  /* param_s  */
  YYSYMBOL_param = 229,                    /* param  */
  YYSYMBOL_mode = 230,                     /* mode  */
  YYSYMBOL_subprog_spec_is_push = 231,     /* subprog_spec_is_push  */
  YYSYMBOL_subprog_body = 232,             /* subprog_body  */
  YYSYMBOL_procedure_call = 233,           /* procedure_call  */
  YYSYMBOL_pkg_decl = 234,                 /* pkg_decl  */
  YYSYMBOL_pkg_spec = 235,                 /* pkg_spec  */
  YYSYMBOL_private_part = 236,             /* private_part  */
  YYSYMBOL_identifier_opt = 237,           /* identifier_opt  */
  YYSYMBOL_pkg_body = 238,                 /* pkg_body  */
  YYSYMBOL_body_opt = 239,                 /* body_opt  */
  YYSYMBOL_private_type = 240,             /* private_type  */
  YYSYMBOL_limited_opt = 241,              /* limited_opt  */
  YYSYMBOL_use_name_s = 242,               /* use_name_s  */
  YYSYMBOL_use_clause = 243,               /* use_clause  */
  YYSYMBOL_rename_decl = 244,              /* rename_decl  */
  YYSYMBOL_rename_unit = 245,              /* rename_unit  */
  YYSYMBOL_comp_unit = 246,                /* comp_unit  */
  YYSYMBOL_context_spec = 247,             /* context_spec  */
  YYSYMBOL_with_clause = 248,              /* with_clause  */
  YYSYMBOL_use_clause_opt = 249,           /* use_clause_opt  */
  YYSYMBOL_unit = 250,                     /* unit  */
  YYSYMBOL_subunit = 251,                  /* subunit  */
  YYSYMBOL_subunit_body = 252,             /* subunit_body  */
  YYSYMBOL_body_stub = 253,                /* body_stub  */
  YYSYMBOL_exception_decl = 254,           /* exception_decl  */
  YYSYMBOL_except_handler_part = 255,      /* except_handler_part  */
  YYSYMBOL_exception_handler = 256,        /* exception_handler  */
  YYSYMBOL_except_choice_s = 257,          /* except_choice_s  */
  YYSYMBOL_except_choice = 258,            /* except_choice  */
  YYSYMBOL_raise_stmt = 259,               /* raise_stmt  */
  YYSYMBOL_generic_decl = 260,             /* generic_decl  */
  YYSYMBOL_generic_formal_part = 261,      /* generic_formal_part  */
  YYSYMBOL_generic_formal = 262,           /* generic_formal  */
  YYSYMBOL_generic_discrim_part_opt = 263, /* generic_discrim_part_opt  */
  YYSYMBOL_subp_default = 264,             /* subp_default  */
  YYSYMBOL_generic_type_def = 265,         /* generic_type_def  */
  YYSYMBOL_generic_derived_type = 266,     /* generic_derived_type  */
  YYSYMBOL_generic_subp_inst = 267,        /* generic_subp_inst  */
  YYSYMBOL_generic_pkg_inst = 268,         /* generic_pkg_inst  */
  YYSYMBOL_generic_inst = 269,             /* generic_inst  */
  YYSYMBOL_rep_spec = 270,                 /* rep_spec  */
  YYSYMBOL_attrib_def = 271,               /* attrib_def  */
  YYSYMBOL_record_type_spec = 272,         /* record_type_spec  */
  YYSYMBOL_align_opt = 273,                /* align_opt  */
  YYSYMBOL_comp_loc_s = 274,               /* comp_loc_s  */
  YYSYMBOL_address_spec = 275,             /* address_spec  */
  YYSYMBOL_code_stmt = 276                 /* code_stmt  */
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

#line 393 "grammar83.tab.c"

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
#define YYNNTS  182
/* YYNRULES -- Number of rules.  */
#define YYNRULES  396
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  717

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
     698,   702,   707,   711,   712,   716,   717,   718,   722,   723,
     727,   731,   732,   737,   741,   742,   746,   751,   755,   756,
     760,   764,   765,   769,   770,   774,   775,   779,   783,   784,
     789,   790,   791,   795,   796,   800,   801,   805,   809,   810,
     814,   815,   819,   820,   821,   822,   826,   832,   836,   840,
     841,   845,   848,   849,   853,   854,   858,   862,   863,   867,
     871,   872,   876,   877,   878,   879,   883,   888,   889,   890,
     894,   895,   896,   897,   901,   902,   906,   907,   908,   912,
     921,   922,   926,   927,   928,   929,   930,   931,   932,   936,
     940,   941,   945,   946,   950,   954,   955,   959,   960,   964,
     965,   969,   970,   974,   978,   979,   983,   984,   988,   989,
     990,   991,   992,   993,   994,   998,   999,  1000,  1004,  1005,
    1006,  1010,  1011,  1012,  1013,  1014,  1015,  1016,  1017,  1018,
    1019,  1023,  1024,  1028,  1032,  1036,  1040,  1041,  1042,  1046,
    1050,  1054,  1055,  1059,  1060,  1064,  1068
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
  "label_opt", "loop_content", "reverse_opt", "basic_loop", "id_opt",
  "block", "block_decl", "block_body", "handled_stmt_s",
  "except_handler_part_opt", "exit_stmt", "name_opt", "when_opt",
  "return_stmt", "goto_stmt", "subprog_decl", "subprog_spec", "designator",
  "formal_part_opt", "formal_part", "param_s", "param", "mode",
  "subprog_spec_is_push", "subprog_body", "procedure_call", "pkg_decl",
  "pkg_spec", "private_part", "identifier_opt", "pkg_body", "body_opt",
  "private_type", "limited_opt", "use_name_s", "use_clause", "rename_decl",
  "rename_unit", "comp_unit", "context_spec", "with_clause",
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

#define YYPACT_NINF (-584)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-349)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -584,    62,   634,  -584,  -584,    79,  -584,    34,    91,   117,
       8,   121,  -584,  -584,   131,  1247,  -584,  -584,   151,  -584,
    -584,   634,  -584,  -584,  -584,  -584,   267,   176,   199,  -584,
    -584,    13,   173,   205,    36,   214,   224,  -584,   109,   242,
     129,  -584,   228,   236,    46,   263,   276,   163,  -584,  -584,
    -584,  -584,   429,  -584,  -584,   284,  -584,  1145,  -584,  -584,
    -584,   373,  -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,
    -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,   286,
     315,  -584,   302,   307,   241,   435,    11,   316,   347,  -584,
    -584,  -584,  -584,    59,   402,  -584,   442,  1091,   129,  -584,
     738,  -584,   311,  -584,   431,   129,  -584,  -584,  -584,   595,
    -584,  -584,  -584,  -584,  -584,  -584,   476,   477,   456,   449,
     211,   446,   454,   493,   165,  1076,   541,  -584,   254,   286,
     315,  -584,   249,   486,    79,   507,   513,    63,   129,  -584,
    -584,  -584,   341,  -584,   129,  1189,    74,   547,  1181,  -584,
     420,  -584,   685,   930,   129,   930,  -584,  -584,   584,  -584,
     479,  -584,  -584,   536,  -584,   458,  -584,  -584,  -584,   293,
    -584,   644,  1251,   294,  -584,   603,  -584,  -584,  -584,   382,
    -584,   446,  -584,   567,    78,   132,   672,    78,   539,   587,
     129,    81,   591,  -584,  -584,   549,   630,   101,    32,   569,
     756,   129,   575,   756,   580,   129,   713,   571,  1076,  -584,
     139,   582,   857,  -584,  -584,  -584,  -584,  -584,  -584,  -584,
    -584,  -584,  -584,   356,  -584,  -584,  -584,  -584,  -584,  -584,
    -584,  -584,    79,   586,  1225,   129,    83,  -584,   627,   214,
     631,   214,   637,  -584,   236,   758,    59,  -584,   446,   284,
     617,   295,   668,  -584,   254,  -584,  -584,   458,  -584,  -584,
     756,  -584,   643,  -584,  -584,    40,  -584,   564,   711,   621,
     614,   632,   328,   124,   636,   738,   195,   657,   700,  -584,
     756,   756,  -584,  -584,  -584,  -584,   699,  -584,  -584,  -584,
    -584,  -584,  -584,   756,   756,  1251,   294,  -584,  -584,  -584,
    -584,  1251,   930,   746,   730,  -584,  -584,  -584,   626,  -584,
    -584,   368,  -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,
    -584,   756,   733,   253,  -584,  -584,  1137,   695,   509,  -584,
     735,   638,   559,  -584,   459,   702,   501,   505,   756,   129,
    -584,   714,   808,   760,  -584,  -584,  -584,  -584,   503,   446,
     755,   748,   293,   694,  -584,  1076,   767,  -584,   761,  -584,
     392,  -584,  -584,   756,  -584,  -584,   765,  -584,  -584,   765,
     315,  1247,   764,  1076,   756,    79,  -584,   284,   769,  -584,
    -584,   783,   768,   840,   793,   817,   830,  -584,    38,  -584,
    -584,  -584,   841,   832,  -584,   798,   293,   794,   756,   731,
     756,    43,  -584,   567,  -584,   567,  -584,   665,  -584,   756,
    -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,   516,  -584,
     313,    50,   294,  -584,  -584,  -584,  -584,   567,   399,   836,
    -584,  -584,   806,  -584,   756,  -584,  -584,  -584,  -584,   874,
      70,  -584,   206,   756,   756,  -584,   756,   129,   679,  -584,
    -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,
    -584,   521,  -584,   415,   850,   756,   845,   756,   823,   129,
    -584,   756,   826,  -584,  1076,   756,   871,   822,  -584,  -584,
    -584,   419,   119,  -584,  -584,    26,  -584,   868,  1013,   869,
     831,   883,  -584,  -584,  -584,   910,   912,   921,   129,   922,
     934,  -584,  -584,  -584,   889,   865,  -584,   129,   129,    97,
     867,  -584,   798,   861,  -584,  -584,  -584,   293,  -584,  -584,
     293,  -584,   517,   872,  -584,  -584,   890,   691,   516,  -584,
    -584,   756,    37,  -584,   441,  -584,   236,  -584,   129,  -584,
     395,   441,   293,  -584,  -584,  -584,   697,  -584,   891,  -584,
    -584,  -584,  -584,  -584,  -584,   704,  -584,   717,  -584,   548,
     129,   293,  -584,   898,  -584,  -584,  1041,  -584,   911,  -584,
    -584,   876,   446,    41,  -584,   943,   731,  -584,   904,   909,
    -584,  -584,    79,   941,  -584,  -584,   896,  -584,   888,  -584,
     421,   908,  -584,   446,  -584,   892,   756,   900,  -584,   461,
     913,   157,  -584,  -584,    38,  -584,   756,  -584,  -584,  -584,
     679,  -584,   245,   926,   129,  -584,   756,   546,  -584,  -584,
    -584,  -584,   906,   230,  1076,   230,   907,    48,  -584,   756,
    -584,   914,   979,   936,  -584,   923,  -584,   379,  -584,   169,
    -584,  -584,   924,   756,  -584,   441,  -584,   938,   925,   939,
     678,   983,  -584,  -584,  -584,   129,  -584,   740,  -584,  1188,
    -584,  -584,    51,   885,  -584,  -584,  1076,   869,  -584,  -584,
    -584,  -584,   928,  -584,  -584,   525,  -584,  -584,   981,  -584,
     129,   962,   344,  -584,   315,  -584,  1018,  1076,   920,  -584,
     944,   756,  -584,   315,   808,  -584,  -584,  -584,   978,  -584,
     946,    28,   947,   315,  -584,   731,    35,  -584,  -584,    53,
    1006,  -584,  -584,   949,   245,  -584,  -584
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       9,     0,     0,     2,     1,     0,   356,     0,     0,     0,
       0,     0,    10,   334,     0,     0,   335,   332,     0,   333,
     338,     0,   330,     9,   336,   337,     0,     0,     0,   283,
     284,   282,     0,     0,     0,   285,     0,    24,     0,   296,
       0,   278,     0,     0,     0,     0,     0,     0,   125,   122,
      11,    12,     0,    13,    14,     0,   129,     0,   126,   128,
      15,     0,   130,    16,   131,   123,    18,   319,    20,    17,
      19,   124,   386,   387,   388,   299,   328,   330,     9,   326,
     325,   291,     0,     0,     0,     0,     0,     0,     0,   364,
     357,   279,   300,     0,     0,   286,     0,     0,     0,     3,
       0,   280,     0,   329,     0,     0,   383,   132,   141,     0,
     136,   133,   134,   135,    21,   137,     0,     0,     0,    33,
     132,     0,   134,     0,    26,     0,     0,   127,   296,   327,
     324,   331,     0,   365,     0,     0,     0,   292,     0,   354,
     358,   355,     0,   288,     0,     0,     0,   302,     0,   120,
       0,   384,     0,     0,     0,     0,   160,   140,   132,   158,
       0,   192,   193,     0,     5,   208,   159,   207,   212,     7,
     169,   177,     0,   189,   197,   203,   211,   210,   209,     0,
      25,   385,   321,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    36,    34,   316,     0,    27,     0,     0,     0,
       0,   271,     0,     0,     0,   271,     0,   132,     0,   223,
       0,     0,     0,   217,   219,   221,   222,   224,   225,   233,
     234,     9,   235,   264,   236,   266,   226,   227,   228,   229,
     230,   231,   261,     0,     0,     0,     0,   366,     0,   285,
       0,   285,   293,   294,     0,     0,     0,   287,   281,   307,
       0,     0,     0,   121,     0,   320,   205,   215,   216,   204,
       0,   148,   160,   111,   113,     0,   107,   110,   208,     0,
       0,   146,   109,   177,     0,     0,     0,   172,   173,   174,
       0,     0,   184,   182,   186,   187,     0,   181,   183,   185,
     194,   195,   196,     0,     0,     0,   190,   201,   202,   199,
     200,     0,     0,     0,     0,   340,   341,   339,     0,   143,
     146,   109,   157,   156,   155,   154,   153,   152,   149,   150,
     151,     0,   391,     0,   138,   139,     0,     0,    48,   100,
       0,     0,     0,    97,   310,     0,   132,   134,     0,     0,
     344,     0,    30,    28,    29,    71,    72,   232,     0,   272,
     273,     0,   244,   245,   240,     0,     0,   237,     0,   275,
       0,   254,   220,     0,   298,   396,     0,   218,   267,   269,
     249,     0,     0,     0,     0,   261,   255,     0,     0,   262,
     343,     0,     0,   310,     0,     0,   368,   295,    30,   323,
     289,   308,     0,     0,   303,   304,     8,     0,     0,     0,
       0,     0,   112,     0,   162,     0,   161,     0,   213,     0,
       4,     6,   214,   175,   176,   170,   171,   188,   178,   179,
     180,     0,   191,   198,   206,   296,   142,     0,     0,     0,
     393,   389,     0,    46,     0,    47,    50,    49,    35,   101,
       0,    96,     0,     0,     0,   311,     0,     0,     0,    37,
      44,    64,    38,    39,    40,    66,    67,    41,    42,    43,
      45,     0,    32,     0,     0,     0,     0,     0,     0,     0,
     248,     0,     0,   277,     0,     0,     0,     0,   243,   353,
     276,     0,     0,   345,   346,     0,   265,     0,     0,     0,
       0,     0,   297,   322,   367,     0,     0,     0,     0,     0,
       0,   377,   378,   379,     0,     0,   380,     0,     0,     0,
       0,   290,   304,     0,   305,   301,   165,   168,   108,   110,
     109,    53,   208,    57,   166,   167,   160,     0,    56,   144,
     395,     0,     0,   342,    54,   102,     0,    98,     0,   114,
       0,    54,    65,    52,    63,    62,     0,    60,     0,   309,
       9,    84,    23,   318,    83,     0,    76,     0,    80,   208,
       0,    31,    22,     0,   274,   270,     0,   241,     0,   238,
     352,   132,   351,     0,   349,     0,     0,   250,   258,     0,
     256,   252,   261,   374,   376,   373,   381,   372,     0,   359,
     368,     0,   370,   369,   360,     0,     0,     0,   163,     0,
       0,     0,    55,    51,    30,   115,     0,    69,    68,    59,
       0,    86,     0,     0,     0,    79,     0,     0,    82,    75,
      74,   317,     0,     0,     0,     0,     0,     0,   259,     0,
     260,     0,     0,     0,   371,     0,   363,     0,   306,     0,
     164,   392,     0,     0,    99,    54,    61,     0,     0,     0,
       0,     0,     9,    90,     9,     0,    77,     0,    81,   208,
      78,   239,     0,     0,   350,   247,     0,     0,   263,   375,
     382,   361,     0,    58,   390,     0,    70,    95,     0,     9,
       0,     0,     0,    87,    88,    73,     0,     0,     0,   257,
       0,     0,     9,    89,    30,    85,    91,     9,     0,   362,
       0,     0,     0,    93,   394,     0,     0,   104,    94,     0,
       0,   105,     9,     0,     0,   103,   106
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -584,  -584,    -2,  -584,   754,   -19,  -584,  -584,  -584,   -10,
    -584,  -584,  -380,  -584,  -584,  -584,  -584,  -584,  -188,  -584,
    -584,  -584,  -253,  -495,  -289,  -584,  -584,   425,  -584,  -584,
    -584,  -584,  -292,  -584,  -584,  -583,  -584,   417,  -584,  -584,
    -552,  -584,  -584,   323,  -584,  -584,   357,   919,  -584,   600,
    -584,   359,  -584,   338,  -548,   646,  -390,   674,    -9,   807,
    -584,   -72,  -584,  1004,  -584,    39,  -232,   878,   881,  -584,
     660,  -143,   -17,  -584,   882,  -584,  -584,  -584,   916,   -86,
    -584,  -584,   529,  -584,  -584,  -142,  -584,  -584,  -128,  -584,
     771,  -120,  -242,  -102,  -584,  -344,  -205,  -584,  -584,  -584,
    -584,  -584,  -584,  -584,   605,  -584,  -336,  -584,  -584,  -584,
    -584,  -584,  -584,  -584,  -584,  -584,  -467,  -355,  -584,  -584,
    -212,  -584,  -584,  -584,   877,  -584,  -584,  -584,   332,    22,
      27,    -8,  -584,  -584,   -62,  -584,  -584,    19,  -584,   352,
    1044,  -584,   573,    24,  -584,   698,   703,  -584,    10,  -584,
     416,  -584,  -584,  1069,  1017,  1077,  -584,  -584,  -584,  -584,
    -584,   734,   481,   475,  -584,   427,  -584,  -584,  -584,   512,
    -584,  -584,  -584,  -584,  1008,  -584,  -584,  -584,  -584,  -584,
    -584,  -584
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     1,   209,   163,   164,     2,    49,    50,    51,    52,
     198,   342,   468,    53,   192,   335,   449,    54,   619,   435,
     436,   450,   602,   618,   264,   452,   546,   547,   453,   454,
     455,   456,   344,   345,   346,   620,   555,   556,   466,   557,
     558,   458,   551,   651,   652,   683,   653,   193,   332,   333,
     536,   654,   706,   707,   265,   266,   267,   459,   249,   147,
     148,    56,    57,    58,    59,   165,   116,   166,   110,   111,
     308,   309,   112,   113,   316,   167,   168,   270,   310,   311,
     280,   281,   170,   293,   294,   171,   172,   295,   173,   301,
     174,   175,   176,   177,   178,   212,   213,   214,   215,   216,
     217,   218,   219,   353,   354,   355,   356,   476,   220,   221,
     485,   577,   222,   223,   375,   629,   376,   378,   224,   377,
     126,   225,   368,   226,   350,   472,   227,   228,    60,    61,
     379,    94,    95,   142,    87,   244,    15,    62,   229,    63,
      18,   252,   515,    64,   392,   460,   461,   123,    65,    66,
      67,     3,    21,    22,    79,    23,    24,   307,    68,    69,
     369,   483,   573,   574,   230,    70,    26,    90,   238,   510,
     505,   506,    27,    28,   106,    71,    72,    73,   430,   532,
      74,   231
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      12,    38,   327,   362,    80,   419,    55,   367,   511,   519,
     343,   477,   388,    48,   169,   402,    85,   269,   273,    76,
     490,    16,   580,   211,    14,   149,    19,   101,   627,   488,
     122,   143,    31,   256,   412,   259,    89,   391,   489,   603,
      16,   273,   457,    14,   296,    19,   608,   467,    86,   341,
     398,   624,   258,   409,    32,    48,   575,   312,   666,   130,
      81,   687,     4,   712,   658,   710,   117,   600,   313,   314,
     138,   329,   685,  -285,   272,   437,   253,   667,    12,   109,
       8,   451,   329,    85,   329,   330,   121,   382,    36,   131,
     139,   501,   312,    93,   250,    48,   576,   694,   705,   523,
     323,   592,   242,   313,   314,   705,   211,   107,   108,    33,
     211,   521,   115,   243,   348,    99,   100,   352,   315,   150,
     360,    33,   187,   160,   188,   399,   625,   409,    12,   282,
     566,   283,   284,   399,    37,   564,   625,   152,   399,   131,
     290,   291,   292,    48,   181,    37,    48,   317,   363,    33,
     676,   418,   421,   315,    29,    30,    37,   709,    37,   412,
     339,   239,   149,   285,   210,   491,    34,   422,   570,   286,
     150,    39,   107,   108,   396,   643,   554,   245,   337,   149,
     340,   331,   424,   248,   390,   277,   519,   196,   103,   169,
      40,   104,    35,   257,   571,   108,    37,   197,   305,   268,
     689,   304,   370,   306,   107,   108,   157,   318,   108,   312,
      41,   287,   288,   289,   290,   291,   292,   278,   364,   183,
     313,   314,   268,   276,   644,   185,   331,   631,   538,   328,
      75,   384,    48,   386,   121,   428,    85,   328,   120,   108,
     349,   187,   279,   188,   349,    97,   647,   210,    96,    48,
     673,   210,   463,   211,   539,    91,   150,   273,   421,   543,
     524,   273,   525,   273,    98,   273,   648,   528,    81,   277,
     315,   211,   367,   150,   381,   160,   134,   481,    92,   570,
     663,   107,   108,   367,   529,   273,   105,   607,   352,   234,
    -312,   649,   135,  -312,    93,   136,    42,     8,   105,   102,
     601,   278,     5,   125,   604,   107,   108,   114,   235,   277,
     586,   115,   517,   520,   702,   519,  -208,   233,    82,  -208,
      37,     9,   688,   421,    48,  -118,   279,   554,   521,    43,
       5,     6,   431,   420,    13,    83,    47,   297,   118,    84,
     554,   278,    37,   698,   277,   647,   146,     8,   534,     9,
     605,   119,   298,    13,    17,    47,  -208,   540,   541,    45,
     542,   367,   486,    46,    47,   648,   279,     8,    12,    48,
      37,  -208,   211,    17,   -92,   211,   278,   132,   464,   371,
     261,   561,   133,   672,   277,   352,   211,   299,   300,   352,
     372,   153,   179,   183,   210,   140,     8,   401,   373,   185,
     407,   279,   700,  -208,  -208,  -208,  -208,  -208,   277,   408,
    -145,   277,   210,   128,   -92,   277,   278,     5,    20,    37,
     246,   606,   247,   154,   155,   156,   141,   374,   263,    25,
     331,   277,    40,   303,   273,   277,     9,    20,   268,   522,
     278,   279,   268,   278,   268,   599,   268,   278,    25,  -145,
    -145,   400,    41,   157,   107,   108,   159,   277,   367,   160,
     254,   509,   144,   278,   211,   279,   268,   278,   279,   161,
     162,   480,   279,   442,   421,   421,   341,   277,   530,    40,
     261,   328,   145,   367,   443,   444,   328,   421,   279,   278,
     520,   153,   279,   367,   552,   273,   190,   400,   569,    41,
     445,   183,   446,   447,   559,   184,   180,   185,   563,   278,
     639,   104,   124,   210,   279,   400,   210,   104,   137,   277,
     645,   572,   211,   154,   155,   262,   183,   210,   263,   191,
     184,   612,   185,  -313,   279,   434,  -313,   328,   183,   448,
     641,   277,   276,   470,   185,   186,   590,   591,   593,   421,
     660,   278,   189,   157,   107,   108,   159,   675,   153,   160,
     187,   211,   188,   273,   211,   400,   236,   548,   261,   161,
     162,   232,   194,   278,   549,   195,   279,   328,   550,   153,
    -314,   691,   240,  -314,  -315,   211,   211,  -315,   241,   183,
     154,   155,   156,   184,   260,   185,   211,   183,   279,   328,
     251,   401,   650,   185,   617,   210,   290,   291,   292,   302,
      12,   154,   155,   156,   325,   268,   263,   274,   275,   520,
     157,   107,   108,   159,   336,   108,   160,   326,   183,   -54,
     -54,   334,   401,   682,   185,   684,   161,   162,   440,   338,
     441,   157,   107,   108,   159,  -147,  -147,   160,   347,   282,
     351,   283,   284,   657,   361,   659,   522,   161,   162,   357,
     693,   365,   572,   210,   572,   380,   261,   383,   659,     5,
       6,   385,   650,   701,   182,   183,   268,   153,   703,   184,
      12,   185,    12,   285,   153,     7,     8,   387,     9,   286,
     321,    12,   393,   714,   328,   404,   405,    10,   395,    12,
     397,    12,   210,   403,   650,   210,    11,   426,   427,   154,
     155,   526,    12,   406,   263,   410,   154,   155,   156,   328,
     104,   439,   474,   475,   413,   153,   210,   210,   414,   322,
     522,   287,   288,   289,   290,   291,   292,   210,   417,   157,
     107,   108,   159,   153,   268,   160,   157,   107,   108,   159,
     153,   429,   160,   544,   545,   161,   162,   154,   155,   156,
     104,   680,   161,   162,   255,   183,    32,   400,   153,   184,
     425,   185,   598,   427,   433,   154,   155,   156,   609,   610,
     263,   462,   154,   155,   156,   613,   614,   157,   107,   108,
     159,   183,   359,   160,   465,   401,   686,   185,   615,   616,
     154,   155,   156,   161,   162,   157,   107,   108,   159,   415,
     416,   160,   157,   158,   108,   159,   438,   467,   160,   469,
     183,   161,   162,   199,   184,   471,   185,   473,   161,   162,
     157,   107,   108,   159,   478,   482,   160,   389,   183,   487,
     479,  -253,   184,   200,   185,  -253,   161,   162,   492,   494,
    -242,  -242,  -242,   507,   442,   201,  -253,   341,   199,   202,
     203,   508,   493,   183,  -253,   495,   496,   184,   204,   185,
     509,   512,   513,   514,     8,   516,  -253,   205,   200,   531,
    -253,   445,   206,   497,   498,   533,   199,  -268,   535,   366,
     201,  -253,   560,  -253,   202,   203,   499,   207,   108,  -253,
     208,   568,   562,   204,  -253,   565,   200,   578,  -253,     8,
     581,   373,   205,   582,   583,  -347,   584,   206,   201,  -253,
     500,   199,   202,   203,   432,   585,   587,  -253,  -253,   553,
     183,   204,   207,   108,   184,   208,   185,     8,   588,  -253,
     205,   200,   549,  -253,   589,   206,   594,   597,   611,   622,
    -251,   630,   596,   201,  -253,  -347,  -253,   202,   203,   623,
     207,   108,  -253,   208,   626,   628,   204,   632,   633,   634,
     642,   638,     8,   655,   154,   205,   156,   621,   183,   199,
     206,   640,   184,   669,   185,   661,   665,   636,   637,   670,
    -251,  -253,   184,   668,   185,   207,   108,  -253,   208,   200,
     678,  -253,   671,   674,   157,   107,   108,   159,  -348,   690,
     160,   201,  -253,   681,   199,   202,   203,   677,   679,   695,
    -253,   692,   660,   699,   204,   704,   708,   713,   715,   411,
       8,   656,  -253,   205,   200,   646,  -253,   716,   206,   696,
     537,   697,   199,   579,   711,   518,   201,  -253,  -348,  -253,
     202,   203,   237,   207,   108,  -253,   208,   502,   394,   204,
    -253,   127,   200,   319,  -253,     8,   320,   527,   205,   324,
      88,  -246,   423,   206,   201,  -253,   271,   199,   202,   203,
     567,   503,   358,  -253,  -253,   595,   504,   204,   207,   108,
      77,   208,    42,     8,   129,  -253,   205,   200,    78,  -253,
     664,   206,   635,   484,   662,   151,     0,     0,     0,   201,
    -253,     0,  -253,   202,   203,     0,   207,   108,  -253,   208,
       0,  -118,   204,     0,     0,    43,     5,     6,     8,     0,
       0,   205,     0,     0,     0,   105,   206,     0,    42,     0,
       0,     0,   146,     8,  -118,     9,    42,  -253,     0,     0,
       0,   207,   108,     0,   208,    45,  -116,     0,     0,    46,
      47,     0,     0,     0,  -117,     0,    37,  -116,     0,     0,
       0,    43,     5,     6,     0,  -117,     0,     0,     0,    43,
       5,     6,    42,     0,     0,     0,     0,     0,    44,     8,
      42,     9,     0,     0,     0,     0,    44,     8,     0,     9,
     432,    45,     0,     0,     0,    46,    47,     0,  -116,    45,
       0,  -119,    37,    46,    47,    43,     5,     6,     0,  -116,
      37,     0,     0,    43,     5,     6,    42,     0,     0,     0,
     -54,     0,   146,     8,  -119,     9,     0,     0,     0,     0,
      44,     8,     0,     9,   400,    45,     0,     0,    42,    46,
      47,     0,     0,    45,     0,  -118,    37,    46,    47,    43,
       5,     6,     0,   153,    37,     0,  -116,     0,   183,   -54,
     -54,     0,   401,     0,   185,     0,   146,     8,  -118,     9,
       0,    43,     5,     6,     0,     0,     0,     0,     0,    45,
       0,     0,     0,    46,    47,   154,   155,   156,    44,     8,
      37,     9,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    45,     0,     0,     0,    46,    47,     0,     0,     0,
       0,     0,    37,     0,     0,   157,   107,   108,   159,     0,
       0,   160
};

static const yytype_int16 yycheck[] =
{
       2,    11,   190,   208,    23,   294,    15,   212,   388,   399,
     198,   355,   244,    15,   100,   268,    26,   160,   160,    21,
     375,     2,   489,   125,     2,    97,     2,    35,   576,   373,
      47,    93,     5,   153,   276,   155,    26,   249,   374,   534,
      21,   183,   334,    21,   172,    21,   541,     9,    26,    17,
      10,    10,   154,     3,    20,    57,    30,    14,    10,    78,
       1,    10,     0,    10,   616,    30,    20,    30,    25,    26,
      59,     1,   655,    60,   160,   328,   148,   629,    80,    40,
      52,   334,     1,    93,     1,     4,    47,     4,    80,    79,
      79,   383,    14,    80,    20,    97,    70,   680,    70,    56,
     186,     4,    39,    25,    26,    70,   208,    75,    76,    75,
     212,   400,    75,    50,   200,    79,    80,   203,    75,    97,
     206,    75,    84,    80,    86,    85,    85,     3,   130,     5,
     474,     7,     8,    85,    75,   471,    85,    98,    85,   129,
      90,    91,    92,   145,   105,    75,   148,    15,     9,    75,
     645,   293,   294,    75,    75,    76,    75,   705,    75,   401,
      59,   134,   234,    39,   125,   377,    75,   295,    49,    45,
     148,    40,    75,    76,   260,    18,   465,   138,   195,   251,
      79,   191,   302,   144,   246,    16,   576,    22,    79,   275,
      59,    82,    75,   154,    75,    76,    75,    32,   179,   160,
     667,   179,   221,   179,    75,    76,    74,    75,    76,    14,
      79,    87,    88,    89,    90,    91,    92,    48,    79,    80,
      25,    26,   183,    84,   604,    86,   236,   582,    22,   190,
      79,   239,   234,   241,   195,   321,   246,   198,    75,    76,
     201,    84,    73,    86,   205,    40,     1,   208,    75,   251,
      81,   212,   338,   355,   442,    79,   234,   399,   400,   447,
     403,   403,   405,   405,    59,   407,    21,   409,     1,    16,
      75,   373,   477,   251,   235,    80,    35,   363,    79,    49,
     624,    75,    76,   488,   427,   427,    44,   540,   374,    40,
      79,    46,    51,    82,    80,    54,     1,    52,    44,    75,
     532,    48,    35,    19,   536,    75,    76,    79,    59,    16,
     498,    75,   398,   399,   694,   705,     3,    63,    51,     6,
      75,    54,   666,   465,   326,    30,    73,   616,   617,    34,
      35,    36,    79,   294,     2,    68,    69,    43,    75,    72,
     629,    48,    75,   687,    16,     1,    51,    52,   434,    54,
     538,    75,    58,    21,     2,    69,    43,   443,   444,    64,
     446,   566,   371,    68,    69,    21,    73,    52,   370,   371,
      75,    58,   474,    21,    30,   477,    48,    75,   339,    23,
       1,   467,    75,     4,    16,   471,   488,    93,    94,   475,
      34,    12,    81,    80,   355,    79,    52,    84,    42,    86,
      72,    73,   691,    90,    91,    92,    93,    94,    16,    81,
      82,    16,   373,    40,    70,    16,    48,    35,     2,    75,
      79,    26,    81,    44,    45,    46,    79,    71,    49,     2,
     440,    16,    59,    51,   576,    16,    54,    21,   399,   400,
      48,    73,   403,    48,   405,   531,   407,    48,    21,    81,
      82,    56,    79,    74,    75,    76,    77,    16,   663,    80,
      40,    40,    60,    48,   566,    73,   427,    48,    73,    90,
      91,    79,    73,    14,   616,   617,    17,    16,    79,    59,
       1,   442,    40,   688,    25,    26,   447,   629,    73,    48,
     576,    12,    73,   698,    79,   637,    40,    56,    79,    79,
      41,    80,    43,    44,   465,    84,    75,    86,   469,    48,
     596,    82,    83,   474,    73,    56,   477,    82,    83,    16,
     606,   482,   624,    44,    45,    46,    80,   488,    49,    80,
      84,   550,    86,    79,    73,    26,    82,   498,    80,    80,
      79,    16,    84,    40,    86,    69,   507,   508,   509,   691,
       4,    48,    75,    74,    75,    76,    77,   643,    12,    80,
      84,   663,    86,   705,   666,    56,    80,    46,     1,    90,
      91,    30,    79,    48,    53,    82,    73,   538,    57,    12,
      79,    56,    75,    82,    79,   687,   688,    82,    75,    80,
      44,    45,    46,    84,    10,    86,   698,    80,    73,   560,
      53,    84,   612,    86,    56,   566,    90,    91,    92,     6,
     612,    44,    45,    46,    75,   576,    49,    81,    82,   705,
      74,    75,    76,    77,    75,    76,    80,    40,    80,    81,
      82,    40,    84,   652,    86,   654,    90,    91,    79,     9,
      81,    74,    75,    76,    77,    81,    82,    80,    79,     5,
      75,     7,     8,   614,    83,   616,   617,    90,    91,    79,
     679,    79,   623,   624,   625,    79,     1,    40,   629,    35,
      36,    40,   682,   692,    79,    80,   637,    12,   697,    84,
     682,    86,   684,    39,    12,    51,    52,    50,    54,    45,
      18,   693,    75,   712,   655,    81,    82,    63,    30,   701,
      57,   703,   663,    82,   714,   666,    72,    81,    82,    44,
      45,    46,   714,    81,    49,    79,    44,    45,    46,   680,
      82,    83,    28,    29,    67,    12,   687,   688,    28,    57,
     691,    87,    88,    89,    90,    91,    92,   698,    39,    74,
      75,    76,    77,    12,   705,    80,    74,    75,    76,    77,
      12,    18,    80,    74,    75,    90,    91,    44,    45,    46,
      82,    83,    90,    91,    79,    80,    20,    56,    12,    84,
      40,    86,    81,    82,    79,    44,    45,    46,    81,    82,
      49,    79,    44,    45,    46,    81,    82,    74,    75,    76,
      77,    80,    79,    80,    80,    84,    56,    86,    81,    82,
      44,    45,    46,    90,    91,    74,    75,    76,    77,   280,
     281,    80,    74,    75,    76,    77,    81,     9,    80,    59,
      80,    90,    91,     1,    84,    70,    86,    79,    90,    91,
      74,    75,    76,    77,    67,    70,    80,    79,    80,    75,
      79,    19,    84,    21,    86,    23,    90,    91,    79,    81,
      28,    29,    30,    60,    14,    33,    34,    17,     1,    37,
      38,    44,    79,    80,    42,    25,    26,    84,    46,    86,
      40,    30,    40,    75,    52,    81,    19,    55,    21,    43,
      23,    41,    60,    43,    44,    79,     1,    30,    14,    32,
      33,    34,    47,    71,    37,    38,    56,    75,    76,    42,
      78,    30,    79,    46,    19,    79,    21,    39,    23,    52,
      79,    42,    55,    30,     4,    30,     4,    60,    33,    34,
      80,     1,    37,    38,    63,     4,     4,    42,    71,    79,
      80,    46,    75,    76,    84,    78,    86,    52,     4,    19,
      55,    21,    53,    23,    79,    60,    79,    57,    57,    38,
      30,    42,    80,    33,    34,    70,    71,    37,    38,    83,
      75,    76,    42,    78,    21,    61,    46,    26,    72,    81,
      57,    79,    52,    47,    44,    55,    46,    79,    80,     1,
      60,    81,    84,     4,    86,    79,    79,    79,    80,    53,
      70,    71,    84,    79,    86,    75,    76,    19,    78,    21,
      75,    23,    79,    79,    74,    75,    76,    77,    30,    81,
      80,    33,    34,    30,     1,    37,    38,    79,    79,    57,
      42,    40,     4,    79,    46,    79,    79,    21,    79,   275,
      52,   614,    19,    55,    21,   610,    23,   714,    60,   682,
     440,   682,     1,    30,   706,   399,    33,    34,    70,    71,
      37,    38,   133,    75,    76,    42,    78,   383,   251,    46,
      19,    57,    21,   185,    23,    52,   185,   407,    55,   187,
      26,    30,   301,    60,    33,    34,   160,     1,    37,    38,
     475,   383,   205,    42,    71,   512,   383,    46,    75,    76,
      21,    78,     1,    52,    77,    19,    55,    21,    21,    23,
     625,    60,   590,   369,   623,    97,    -1,    -1,    -1,    33,
      34,    -1,    71,    37,    38,    -1,    75,    76,    42,    78,
      -1,    30,    46,    -1,    -1,    34,    35,    36,    52,    -1,
      -1,    55,    -1,    -1,    -1,    44,    60,    -1,     1,    -1,
      -1,    -1,    51,    52,    53,    54,     1,    71,    -1,    -1,
      -1,    75,    76,    -1,    78,    64,    19,    -1,    -1,    68,
      69,    -1,    -1,    -1,    19,    -1,    75,    30,    -1,    -1,
      -1,    34,    35,    36,    -1,    30,    -1,    -1,    -1,    34,
      35,    36,     1,    -1,    -1,    -1,    -1,    -1,    51,    52,
       1,    54,    -1,    -1,    -1,    -1,    51,    52,    -1,    54,
      63,    64,    -1,    -1,    -1,    68,    69,    -1,    19,    64,
      -1,    30,    75,    68,    69,    34,    35,    36,    -1,    30,
      75,    -1,    -1,    34,    35,    36,     1,    -1,    -1,    -1,
      42,    -1,    51,    52,    53,    54,    -1,    -1,    -1,    -1,
      51,    52,    -1,    54,    56,    64,    -1,    -1,     1,    68,
      69,    -1,    -1,    64,    -1,    30,    75,    68,    69,    34,
      35,    36,    -1,    12,    75,    -1,    19,    -1,    80,    81,
      82,    -1,    84,    -1,    86,    -1,    51,    52,    53,    54,
      -1,    34,    35,    36,    -1,    -1,    -1,    -1,    -1,    64,
      -1,    -1,    -1,    68,    69,    44,    45,    46,    51,    52,
      75,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    64,    -1,    -1,    -1,    68,    69,    -1,    -1,    -1,
      -1,    -1,    75,    -1,    -1,    74,    75,    76,    77,    -1,
      -1,    80
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    96,   100,   246,     0,    35,    36,    51,    52,    54,
      63,    72,    97,   223,   224,   231,   232,   234,   235,   238,
     245,   247,   248,   250,   251,   260,   261,   267,   268,    75,
      76,   225,    20,    75,    75,    75,    80,    75,   104,    40,
      59,    79,     1,    34,    51,    64,    68,    69,    97,   101,
     102,   103,   104,   108,   112,   153,   156,   157,   158,   159,
     223,   224,   232,   234,   238,   243,   244,   245,   253,   254,
     260,   270,   271,   272,   275,    79,    97,   248,   250,   249,
     100,     1,    51,    68,    72,   104,   224,   229,   235,   243,
     262,    79,    79,    80,   226,   227,    75,    40,    59,    79,
      80,   226,    75,    79,    82,    44,   269,    75,    76,   160,
     163,   164,   167,   168,    79,    75,   161,    20,    75,    75,
      75,   160,   167,   242,    83,    19,   215,   158,    40,   249,
     100,   243,    75,    75,    35,    51,    54,    83,    59,    79,
      79,    79,   228,   229,    60,    40,    51,   154,   155,   156,
     224,   269,   160,    12,    44,    45,    46,    74,    75,    77,
      80,    90,    91,    98,    99,   160,   162,   170,   171,   174,
     177,   180,   181,   183,   185,   186,   187,   188,   189,    81,
      75,   160,    79,    80,    84,    86,    69,    84,    86,    75,
      40,    80,   109,   142,    79,    82,    22,    32,   105,     1,
      21,    33,    37,    38,    46,    55,    60,    75,    78,    97,
     160,   188,   190,   191,   192,   193,   194,   195,   196,   197,
     203,   204,   207,   208,   213,   216,   218,   221,   222,   233,
     259,   276,    30,    63,    40,    59,    80,   142,   263,   225,
      75,    75,    39,    50,   230,   160,    79,    81,   160,   153,
      20,    53,   236,   156,    40,    79,   186,   160,   188,   186,
      10,     1,    46,    49,   119,   149,   150,   151,   160,   166,
     172,   173,   174,   180,    81,    82,    84,    16,    48,    73,
     175,   176,     5,     7,     8,    39,    45,    87,    88,    89,
      90,    91,    92,   178,   179,   182,   183,    43,    58,    93,
      94,   184,     6,    51,   224,   232,   238,   252,   165,   166,
     173,   174,    14,    25,    26,    75,   169,    15,    75,   162,
     163,    18,    57,   174,   169,    75,    40,   113,   160,     1,
       4,   104,   143,   144,    40,   110,    75,   167,     9,    59,
      79,    17,   106,   113,   127,   128,   129,    79,   174,   160,
     219,    75,   174,   198,   199,   200,   201,    79,   219,    79,
     174,    83,   191,     9,    79,    79,    32,   191,   217,   255,
     100,    23,    34,    42,    71,   209,   211,   214,   212,   225,
      79,   160,     4,    40,   226,    40,   226,    50,   161,    79,
     229,   215,   239,    75,   154,    30,   174,    57,    10,    85,
      56,    84,   117,    82,    81,    82,    81,    72,    81,     3,
      79,    99,   187,    67,    28,   177,   177,    39,   180,   119,
     160,   180,   183,   185,   186,    40,    81,    82,   174,    18,
     273,    79,    63,    79,    26,   114,   115,   117,    81,    83,
      79,    81,    14,    25,    26,    41,    43,    44,    80,   111,
     116,   117,   120,   123,   124,   125,   126,   127,   136,   152,
     240,   241,    79,   174,   160,    80,   133,     9,   107,    59,
      40,    70,   220,    79,    28,    29,   202,   190,    67,    79,
      79,   174,    70,   256,   256,   205,   153,    75,   190,   201,
     212,   215,    79,    79,    81,    25,    26,    43,    44,    56,
      80,   127,   152,   240,   241,   265,   266,    60,    44,    40,
     264,   107,    30,    40,    75,   237,    81,   174,   150,   151,
     174,   119,   160,    56,   166,   166,    46,   165,   180,   166,
      79,    43,   274,    79,   174,    14,   145,   144,    22,   113,
     174,   174,   174,   113,    74,    75,   121,   122,    46,    53,
      57,   137,    79,    79,   119,   131,   132,   134,   135,   160,
      47,   174,    79,   160,   201,    79,   190,   199,    30,    79,
      49,    75,   160,   257,   258,    30,    70,   206,    39,    30,
     211,    79,    30,     4,     4,     4,   113,     4,     4,    79,
     160,   160,     4,   160,    79,   237,    80,    57,    81,   174,
      30,   161,   117,   118,   161,   113,    26,   117,   118,    81,
      82,    57,   100,    81,    82,    81,    82,    56,   118,   113,
     130,    79,    38,    83,    10,    85,    21,   149,    61,   210,
      42,   212,    26,    72,    81,   264,    79,    80,    79,   174,
      81,    79,    57,    18,   107,   174,   122,     1,    21,    46,
     104,   138,   139,   141,   146,    47,   132,   160,   135,   160,
       4,    79,   257,   190,   258,    79,    10,   135,    79,     4,
      53,    79,     4,    81,    79,   174,   118,    79,    75,    79,
      83,    30,   100,   140,   100,   130,    56,    10,   190,   211,
      81,    56,    40,   100,   130,    57,   141,   146,   190,    79,
     119,   100,   107,   100,    79,    70,   147,   148,    79,   149,
      30,   148,    10,    21,   100,    79,   138
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
     205,   206,   207,   208,   208,   209,   209,   209,   210,   210,
     211,   212,   212,   213,   214,   214,   215,   216,   217,   217,
     218,   219,   219,   220,   220,   221,   221,   222,   223,   223,
     224,   224,   224,   225,   225,   226,   226,   227,   228,   228,
     229,   229,   230,   230,   230,   230,   231,   232,   233,   234,
     234,   235,   236,   236,   237,   237,   238,   239,   239,   240,
     241,   241,   242,   242,   242,   242,   243,   244,   244,   244,
     245,   245,   245,   245,   246,   246,   247,   247,   247,   248,
     249,   249,   250,   250,   250,   250,   250,   250,   250,   251,
     252,   252,   253,   253,   254,   255,   255,   256,   256,   257,
     257,   258,   258,   259,   260,   260,   261,   261,   262,   262,
     262,   262,   262,   262,   262,   263,   263,   263,   264,   264,
     264,   265,   265,   265,   265,   265,   265,   265,   265,   265,
     265,   266,   266,   267,   268,   269,   270,   270,   270,   271,
     272,   273,   273,   274,   274,   275,   276
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
       2,     4,     4,     0,     2,     1,     3,     6,     0,     1,
       4,     0,     1,     6,     0,     2,     2,     2,     0,     1,
       4,     0,     1,     0,     2,     2,     3,     3,     2,     2,
       3,     5,     2,     1,     1,     0,     1,     3,     1,     3,
       5,     1,     0,     1,     1,     2,     2,     6,     2,     2,
       2,     7,     0,     2,     0,     1,     9,     0,     1,     2,
       0,     1,     1,     1,     3,     3,     3,     7,     6,     1,
       5,     4,     6,     5,     4,     3,     2,     3,     2,     3,
       0,     2,     1,     1,     1,     1,     1,     1,     1,     5,
       1,     1,     6,     4,     4,     2,     2,     4,     6,     1,
       3,     1,     1,     3,     3,     3,     1,     2,     2,     6,
       6,     8,    10,     7,     1,     0,     1,     3,     0,     2,
       2,     3,     2,     2,     2,     4,     2,     1,     1,     1,
       1,     2,     4,     3,     4,     2,     1,     1,     1,     5,
       9,     0,     4,     0,     7,     6,     2
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

#line 2401 "grammar83.tab.c"

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
#line 2624 "grammar83.tab.c"
    break;

  case 25: /* def_id_s: def_id_s ',' identifier  */
#line 164 "grammar83.y"
                            {
        (yyval.str_token_array) = (yyvsp[-2].str_token_array);
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2633 "grammar83.tab.c"
    break;

  case 329: /* with_clause: WITH def_id_s ';'  */
#line 912 "grammar83.y"
                      {
        uint32_t package_count = StringTokenArray_size(&(yyvsp[-1].str_token_array));
        for(uint32_t i = 0; i < package_count; ++i) {
            const char* package_name = string_pool_to_str((yyvsp[-1].str_token_array).data[i]);
            comp_manager_parse_spec(context->comp_manager, package_name, &(yyloc));
        }
    }
#line 2645 "grammar83.tab.c"
    break;


#line 2649 "grammar83.tab.c"

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

#line 1071 "grammar83.y"


void yyerror(YYLTYPE* yyloc, yyscan_t scanner, ParseContext* parse_ctx, const char* msg)
{
    (void)scanner;
    (void)parse_ctx;
    error_print(*yyloc, msg);
    error_exit();
}
