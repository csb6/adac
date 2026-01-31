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
  YYSYMBOL_def_id = 105,                   /* def_id  */
  YYSYMBOL_object_qualifier_opt = 106,     /* object_qualifier_opt  */
  YYSYMBOL_object_subtype_def = 107,       /* object_subtype_def  */
  YYSYMBOL_init_opt = 108,                 /* init_opt  */
  YYSYMBOL_type_decl = 109,                /* type_decl  */
  YYSYMBOL_discrim_part_opt = 110,         /* discrim_part_opt  */
  YYSYMBOL_type_completion = 111,          /* type_completion  */
  YYSYMBOL_type_def = 112,                 /* type_def  */
  YYSYMBOL_subtype_decl = 113,             /* subtype_decl  */
  YYSYMBOL_subtype_ind = 114,              /* subtype_ind  */
  YYSYMBOL_constraint = 115,               /* constraint  */
  YYSYMBOL_decimal_digits_constraint = 116, /* decimal_digits_constraint  */
  YYSYMBOL_derived_type = 117,             /* derived_type  */
  YYSYMBOL_range_constraint = 118,         /* range_constraint  */
  YYSYMBOL_range_constr_opt = 119,         /* range_constr_opt  */
  YYSYMBOL_range = 120,                    /* range  */
  YYSYMBOL_enumeration_type = 121,         /* enumeration_type  */
  YYSYMBOL_enum_id_s = 122,                /* enum_id_s  */
  YYSYMBOL_enum_id = 123,                  /* enum_id  */
  YYSYMBOL_integer_type = 124,             /* integer_type  */
  YYSYMBOL_real_type = 125,                /* real_type  */
  YYSYMBOL_float_type = 126,               /* float_type  */
  YYSYMBOL_fixed_type = 127,               /* fixed_type  */
  YYSYMBOL_array_type = 128,               /* array_type  */
  YYSYMBOL_unconstr_array_type = 129,      /* unconstr_array_type  */
  YYSYMBOL_constr_array_type = 130,        /* constr_array_type  */
  YYSYMBOL_component_subtype_def = 131,    /* component_subtype_def  */
  YYSYMBOL_index_s = 132,                  /* index_s  */
  YYSYMBOL_index = 133,                    /* index  */
  YYSYMBOL_iter_index_constraint = 134,    /* iter_index_constraint  */
  YYSYMBOL_iter_discrete_range_s = 135,    /* iter_discrete_range_s  */
  YYSYMBOL_discrete_range = 136,           /* discrete_range  */
  YYSYMBOL_record_type = 137,              /* record_type  */
  YYSYMBOL_record_def = 138,               /* record_def  */
  YYSYMBOL_comp_list = 139,                /* comp_list  */
  YYSYMBOL_comp_decl_s = 140,              /* comp_decl_s  */
  YYSYMBOL_variant_part_opt = 141,         /* variant_part_opt  */
  YYSYMBOL_comp_decl = 142,                /* comp_decl  */
  YYSYMBOL_discrim_part = 143,             /* discrim_part  */
  YYSYMBOL_discrim_spec_s = 144,           /* discrim_spec_s  */
  YYSYMBOL_discrim_spec = 145,             /* discrim_spec  */
  YYSYMBOL_access_opt = 146,               /* access_opt  */
  YYSYMBOL_variant_part = 147,             /* variant_part  */
  YYSYMBOL_variant_s = 148,                /* variant_s  */
  YYSYMBOL_variant = 149,                  /* variant  */
  YYSYMBOL_choice_s = 150,                 /* choice_s  */
  YYSYMBOL_choice = 151,                   /* choice  */
  YYSYMBOL_discrete_with_range = 152,      /* discrete_with_range  */
  YYSYMBOL_access_type = 153,              /* access_type  */
  YYSYMBOL_decl_part = 154,                /* decl_part  */
  YYSYMBOL_decl_item_s = 155,              /* decl_item_s  */
  YYSYMBOL_decl_item_s1 = 156,             /* decl_item_s1  */
  YYSYMBOL_decl_item = 157,                /* decl_item  */
  YYSYMBOL_decl_item_or_body_s1 = 158,     /* decl_item_or_body_s1  */
  YYSYMBOL_decl_item_or_body = 159,        /* decl_item_or_body  */
  YYSYMBOL_body = 160,                     /* body  */
  YYSYMBOL_name = 161,                     /* name  */
  YYSYMBOL_mark = 162,                     /* mark  */
  YYSYMBOL_simple_name = 163,              /* simple_name  */
  YYSYMBOL_compound_name = 164,            /* compound_name  */
  YYSYMBOL_c_name_list = 165,              /* c_name_list  */
  YYSYMBOL_used_char = 166,                /* used_char  */
  YYSYMBOL_operator_symbol = 167,          /* operator_symbol  */
  YYSYMBOL_indexed_comp = 168,             /* indexed_comp  */
  YYSYMBOL_value_s = 169,                  /* value_s  */
  YYSYMBOL_value = 170,                    /* value  */
  YYSYMBOL_selected_comp = 171,            /* selected_comp  */
  YYSYMBOL_attribute = 172,                /* attribute  */
  YYSYMBOL_attribute_id = 173,             /* attribute_id  */
  YYSYMBOL_literal = 174,                  /* literal  */
  YYSYMBOL_aggregate = 175,                /* aggregate  */
  YYSYMBOL_value_s_2 = 176,                /* value_s_2  */
  YYSYMBOL_comp_assoc = 177,               /* comp_assoc  */
  YYSYMBOL_expression = 178,               /* expression  */
  YYSYMBOL_logical = 179,                  /* logical  */
  YYSYMBOL_short_circuit = 180,            /* short_circuit  */
  YYSYMBOL_relation = 181,                 /* relation  */
  YYSYMBOL_relational = 182,               /* relational  */
  YYSYMBOL_membership = 183,               /* membership  */
  YYSYMBOL_simple_expression = 184,        /* simple_expression  */
  YYSYMBOL_unary = 185,                    /* unary  */
  YYSYMBOL_adding = 186,                   /* adding  */
  YYSYMBOL_term = 187,                     /* term  */
  YYSYMBOL_multiplying = 188,              /* multiplying  */
  YYSYMBOL_factor = 189,                   /* factor  */
  YYSYMBOL_primary = 190,                  /* primary  */
  YYSYMBOL_parenthesized_primary = 191,    /* parenthesized_primary  */
  YYSYMBOL_qualified = 192,                /* qualified  */
  YYSYMBOL_allocator = 193,                /* allocator  */
  YYSYMBOL_statement_s = 194,              /* statement_s  */
  YYSYMBOL_statement = 195,                /* statement  */
  YYSYMBOL_unlabeled = 196,                /* unlabeled  */
  YYSYMBOL_simple_stmt = 197,              /* simple_stmt  */
  YYSYMBOL_compound_stmt = 198,            /* compound_stmt  */
  YYSYMBOL_null_stmt = 199,                /* null_stmt  */
  YYSYMBOL_assign_stmt = 200,              /* assign_stmt  */
  YYSYMBOL_if_stmt = 201,                  /* if_stmt  */
  YYSYMBOL_cond_clause_s = 202,            /* cond_clause_s  */
  YYSYMBOL_cond_clause = 203,              /* cond_clause  */
  YYSYMBOL_cond_part = 204,                /* cond_part  */
  YYSYMBOL_condition = 205,                /* condition  */
  YYSYMBOL_else_opt = 206,                 /* else_opt  */
  YYSYMBOL_case_stmt = 207,                /* case_stmt  */
  YYSYMBOL_case_hdr = 208,                 /* case_hdr  */
  YYSYMBOL_alternative_s = 209,            /* alternative_s  */
  YYSYMBOL_alternative = 210,              /* alternative  */
  YYSYMBOL_loop_stmt = 211,                /* loop_stmt  */
  YYSYMBOL_label_opt = 212,                /* label_opt  */
  YYSYMBOL_loop_content = 213,             /* loop_content  */
  YYSYMBOL_iter_part = 214,                /* iter_part  */
  YYSYMBOL_reverse_opt = 215,              /* reverse_opt  */
  YYSYMBOL_basic_loop = 216,               /* basic_loop  */
  YYSYMBOL_id_opt = 217,                   /* id_opt  */
  YYSYMBOL_block = 218,                    /* block  */
  YYSYMBOL_block_decl = 219,               /* block_decl  */
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
  YYSYMBOL_designator = 230,               /* designator  */
  YYSYMBOL_formal_part_opt = 231,          /* formal_part_opt  */
  YYSYMBOL_formal_part = 232,              /* formal_part  */
  YYSYMBOL_param_s = 233,                  /* param_s  */
  YYSYMBOL_param = 234,                    /* param  */
  YYSYMBOL_mode = 235,                     /* mode  */
  YYSYMBOL_subprog_spec_is_push = 236,     /* subprog_spec_is_push  */
  YYSYMBOL_subprog_body = 237,             /* subprog_body  */
  YYSYMBOL_procedure_call = 238,           /* procedure_call  */
  YYSYMBOL_pkg_decl = 239,                 /* pkg_decl  */
  YYSYMBOL_pkg_spec = 240,                 /* pkg_spec  */
  YYSYMBOL_private_part = 241,             /* private_part  */
  YYSYMBOL_c_id_opt = 242,                 /* c_id_opt  */
  YYSYMBOL_pkg_body = 243,                 /* pkg_body  */
  YYSYMBOL_body_opt = 244,                 /* body_opt  */
  YYSYMBOL_private_type = 245,             /* private_type  */
  YYSYMBOL_limited_opt = 246,              /* limited_opt  */
  YYSYMBOL_use_clause = 247,               /* use_clause  */
  YYSYMBOL_name_s = 248,                   /* name_s  */
  YYSYMBOL_rename_decl = 249,              /* rename_decl  */
  YYSYMBOL_rename_unit = 250,              /* rename_unit  */
  YYSYMBOL_renames = 251,                  /* renames  */
  YYSYMBOL_compilation = 252,              /* compilation  */
  YYSYMBOL_comp_unit = 253,                /* comp_unit  */
  YYSYMBOL_private_opt = 254,              /* private_opt  */
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
#line 64 "grammar83.y"

	#include <assert.h>
	#include <stdlib.h>
	#include <stdbool.h>
	#include "error.h"
	#include "string_pool.h"
	#include "string_view.h"
	#include "lexer.h"

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
	Declaration* find_first_overload(ParseContext* context, StringToken name);

	static
	void append_decl(DeclList* decl_list, Declaration* decl);

	static
	DeclList* find_bucket(ParseContext* context, StringToken name);

	#define cnt_of_array(arr) (sizeof(arr) / sizeof(arr[0]))

	static
	Expression* create_expr(ExprKind kind, uint32_t line_num);

	static
	Statement* create_stmt(StmtKind kind, uint32_t line_num);

	static
	TypeDecl* create_type_decl(TypeKind kind);

	static
	ObjectDecl* create_object_decl(StringToken name, uint32_t line_num);

	static
	int get_base(StringView num_str, uint32_t line_num);

	static
	bool prepare_num_str(StringView num_str, char* buffer, int buffer_sz);

	static
	uint32_t hash_fnv(StringToken token);

	static
	StringToken get_decl_name(const Declaration* decl);

#line 462 "grammar83.tab.c"

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
#define YYFINAL  7
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1339

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  95
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  190
/* YYNRULES -- Number of rules.  */
#define YYNRULES  407
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  725

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
       0,   193,   193,   196,   197,   200,   201,   204,   205,   208,
     209,   212,   213,   214,   215,   216,   217,   218,   219,   220,
     221,   222,   225,   251,   267,   268,   271,   274,   275,   278,
     279,   282,   283,   286,   302,   303,   304,   308,   309,   312,
     313,   314,   315,   316,   317,   318,   319,   322,   343,   344,
     347,   348,   351,   354,   364,   367,   368,   371,   372,   373,
     376,   381,   382,   385,   389,   394,   398,   401,   402,   405,
     408,   409,   412,   413,   416,   419,   422,   425,   426,   429,
     432,   435,   436,   439,   440,   443,   446,   447,   450,   451,
     452,   455,   456,   459,   460,   463,   464,   467,   470,   471,
     474,   475,   478,   479,   482,   485,   486,   489,   492,   493,
     496,   497,   498,   501,   502,   505,   506,   509,   510,   513,
     514,   517,   518,   521,   522,   523,   524,   527,   528,   531,
     532,   535,   536,   539,   540,   541,   542,   543,   546,   547,
     548,   551,   554,   555,   558,   559,   562,   567,   570,   573,
     574,   577,   578,   579,   580,   583,   584,   585,   586,   589,
     592,   593,   594,   595,   598,   617,   618,   621,   622,   623,
     624,   625,   628,   629,   632,   635,   636,   637,   640,   641,
     642,   645,   646,   650,   651,   652,   653,   656,   657,   658,
     659,   660,   661,   664,   665,   668,   669,   670,   673,   674,
     677,   678,   679,   682,   683,   686,   687,   688,   689,   692,
     693,   694,   695,   698,   699,   700,   701,   702,   705,   706,
     709,   712,   713,   716,   717,   720,   721,   724,   725,   726,
     729,   730,   731,   732,   733,   734,   735,   736,   737,   740,
     741,   742,   743,   746,   750,   755,   758,   759,   762,   765,
     768,   771,   772,   775,   778,   781,   782,   785,   789,   792,
     793,   796,   806,   812,   820,   823,   824,   827,   830,   831,
     834,   837,   838,   841,   844,   847,   848,   851,   857,   858,
     861,   862,   865,   866,   872,   876,   877,   880,   881,   882,
     885,   886,   889,   890,   893,   896,   897,   900,   901,   904,
     905,   906,   907,   910,   913,   917,   920,   921,   924,   928,
     929,   932,   933,   936,   940,   941,   944,   947,   948,   951,
     954,   955,   958,   959,   960,   963,   964,   965,   966,   969,
     972,   973,   974,   977,   978,   981,   982,   985,   986,   987,
     990,   993,   994,   997,   998,   999,  1000,  1001,  1002,  1003,
    1006,  1010,  1011,  1014,  1015,  1018,  1021,  1022,  1025,  1026,
    1029,  1030,  1033,  1034,  1037,  1040,  1041,  1044,  1045,  1048,
    1049,  1050,  1052,  1054,  1055,  1056,  1059,  1060,  1061,  1064,
    1065,  1066,  1069,  1070,  1071,  1072,  1073,  1074,  1075,  1076,
    1077,  1078,  1081,  1082,  1085,  1088,  1091,  1094,  1095,  1096,
    1099,  1102,  1105,  1106,  1109,  1110,  1113,  1116
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
  "def_id_s", "def_id", "object_qualifier_opt", "object_subtype_def",
  "init_opt", "type_decl", "discrim_part_opt", "type_completion",
  "type_def", "subtype_decl", "subtype_ind", "constraint",
  "decimal_digits_constraint", "derived_type", "range_constraint",
  "range_constr_opt", "range", "enumeration_type", "enum_id_s", "enum_id",
  "integer_type", "real_type", "float_type", "fixed_type", "array_type",
  "unconstr_array_type", "constr_array_type", "component_subtype_def",
  "index_s", "index", "iter_index_constraint", "iter_discrete_range_s",
  "discrete_range", "record_type", "record_def", "comp_list",
  "comp_decl_s", "variant_part_opt", "comp_decl", "discrim_part",
  "discrim_spec_s", "discrim_spec", "access_opt", "variant_part",
  "variant_s", "variant", "choice_s", "choice", "discrete_with_range",
  "access_type", "decl_part", "decl_item_s", "decl_item_s1", "decl_item",
  "decl_item_or_body_s1", "decl_item_or_body", "body", "name", "mark",
  "simple_name", "compound_name", "c_name_list", "used_char",
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
  "pkg_spec", "private_part", "c_id_opt", "pkg_body", "body_opt",
  "private_type", "limited_opt", "use_clause", "name_s", "rename_decl",
  "rename_unit", "renames", "compilation", "comp_unit", "private_opt",
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

#define YYPACT_NINF (-603)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-360)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      21,     1,   112,  -603,    91,   115,    94,  -603,    21,  -603,
     146,  -603,   580,   553,  -603,  -603,   743,  -603,  -603,  -603,
     167,    -2,   241,  -603,    39,   146,   163,  -603,   365,  1228,
    -603,  -603,   246,  -603,  -603,  -603,  -603,  -603,   180,   268,
     272,  -603,   580,  -603,   190,   919,   295,   919,  -603,  -603,
    -603,  -603,   300,  -603,  -603,   430,  -603,   673,   351,  -603,
    -603,  -603,  -603,  -603,  -603,  -603,   222,  -603,   713,   506,
     373,  -603,   378,  -603,  -603,  -603,   146,  -603,   146,  -603,
    -603,    14,   146,   279,   277,   146,   356,   295,  -603,   328,
     338,   146,    81,   400,   408,   295,  -603,  -603,  -603,  -603,
    -603,   538,  -603,  -603,  -603,   451,  -603,  1150,  -603,  -603,
    -603,   442,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,  -603,  -603,  -603,  -603,    21,  -603,   146,   146,
      32,   581,    19,   420,   425,  -603,  -603,  -603,  -603,  -603,
     190,  -603,  -603,  -603,   673,  -603,  -603,  -603,   470,  -603,
    -603,    37,  -603,   545,   487,   455,   606,   449,   694,   347,
     466,   743,   448,   240,   230,   743,   482,   527,  -603,   743,
     743,  -603,  -603,  -603,  -603,   525,  -603,  -603,  -603,  -603,
    -603,  -603,   743,   743,   506,   373,  -603,  -603,  -603,  -603,
     506,   919,  -603,   167,    31,   512,  -603,   138,  1124,   515,
    -603,   109,   295,  -603,   744,  -603,  -603,   490,  -603,   146,
     551,   522,   744,   383,   542,    70,  1095,   609,  -603,   293,
     410,   556,   241,   146,   146,   507,  -603,   565,  -603,  -603,
      21,   568,   743,   688,   743,   243,  -603,   448,  -603,   448,
    -603,   670,  -603,   743,  -603,  -603,   691,  -603,  -603,   315,
    -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,
     222,  -603,  -603,  -603,  -603,  -603,   679,  -603,   329,    61,
     373,  -603,  -603,   260,  -603,   295,  1181,   157,   620,  1206,
    -603,   475,  -603,  -603,   407,   744,   188,   736,   188,   146,
     156,   295,    80,   639,  -603,  -603,   295,  -603,   683,   113,
     326,   618,   743,   295,   295,   743,   626,   295,    85,   624,
    1095,  -603,   100,   644,   855,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,  -603,  -603,  -603,   406,  -603,  -603,  -603,  -603,
    -603,  -603,  -603,  -603,   241,   649,  1237,   652,    95,  -603,
     696,   277,   699,   277,   693,  -603,   146,  -603,  -603,   222,
    -603,  -603,   222,  -603,   752,   669,  -603,  -603,   717,   727,
     679,  -603,   448,    31,  -603,   744,   451,   146,  1264,   721,
    -603,   293,   764,   716,  -603,  -603,  -603,   743,   773,   185,
    -603,  -603,   578,   720,   751,  -603,   725,   712,   597,  -603,
     488,   746,   744,   743,  -603,   750,   742,   831,   783,  -603,
    -603,  -603,  -603,   635,   744,   782,   706,   222,   786,  -603,
    1095,   788,  -603,   780,  -603,   294,  -603,  -603,   743,  -603,
    -603,   794,  -603,  -603,   794,    21,  1228,   798,  1095,   743,
     241,   816,  -603,   451,   800,  -603,  -603,  -603,   805,   544,
     830,   850,   859,  -603,    53,   743,   819,  -603,  -603,  -603,
    -603,   874,   211,  -603,   146,  -603,   310,   862,  -603,  -603,
     827,  -603,   743,  -603,  -603,  -603,  -603,   895,    48,  -603,
      29,   743,   743,  -603,   743,   295,   770,  -603,  -603,  -603,
    -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,   660,
    -603,   395,  -603,   743,   864,   743,   834,   835,  -603,   743,
     837,  -603,  1095,   743,   887,   820,  -603,  -603,  -603,   462,
     379,  -603,  -603,    33,  -603,   879,  1032,   877,   843,  -603,
     743,   894,  -603,  -603,   923,   924,   925,   295,   933,   939,
    -603,  -603,  -603,   892,   867,  -603,   295,   295,   107,   868,
    -603,   312,  -603,   146,   888,   167,  -603,  -603,   743,   278,
    -603,   447,  -603,   146,  -603,   295,  -603,   886,   447,   222,
    -603,  -603,  -603,   765,  -603,   893,  -603,  -603,  -603,  -603,
    -603,   787,  -603,   789,  -603,  1140,   295,   222,  -603,  -603,
    -603,  -603,  1060,  -603,   911,  -603,  -603,   869,   744,    42,
    -603,   932,   688,  -603,  -603,   913,  -603,  -603,   877,  1227,
     241,   935,  -603,  -603,   896,  -603,   883,  -603,   476,   781,
    -603,   744,  -603,  -603,   890,   469,   909,   123,  -603,  -603,
      53,  -603,   743,  -603,  -603,  -603,   770,  -603,   358,   929,
     295,  -603,   743,   650,  -603,  -603,  -603,   903,   396,  1095,
     396,   905,    43,  -603,  -603,   906,   977,   934,  -603,   907,
    -603,    44,  -603,  -603,   912,   743,  -603,   447,  -603,   921,
     146,   922,   799,   958,  -603,  -603,  -603,   295,  -603,   509,
    -603,  -603,  -603,    47,   902,  -603,  -603,  1095,  -603,  -603,
    -603,  -603,   928,  -603,   656,  -603,  -603,   950,  -603,   295,
     945,   259,  -603,    21,  -603,   999,  1095,   937,   926,   743,
    -603,    21,   831,  -603,  -603,  -603,   997,  -603,   927,   133,
     931,    21,  -603,   688,   141,  -603,  -603,    60,   990,  -603,
    -603,   938,   358,  -603,  -603
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
     330,     0,     0,     9,   335,   141,     0,     1,   332,   336,
       0,   331,     0,   335,   341,     3,     0,    10,   141,   142,
     144,     0,     0,   367,     0,     0,     0,   345,     0,     0,
     346,   343,     0,   344,   349,     9,   347,   348,     0,     0,
       0,   339,     0,   341,   337,     0,     0,     0,   166,   146,
     147,   164,     0,   198,   199,     0,     5,   214,   133,   165,
     137,   134,   135,   136,   213,   218,     7,   175,   183,     0,
     195,   203,   209,   217,   216,   215,     0,   340,     0,   291,
     290,   289,     0,     0,   292,     0,   303,     0,   285,     0,
       0,     0,     0,     0,     0,     0,    26,   126,   123,    11,
      12,     0,    24,    13,    14,     0,   130,     0,   127,   129,
      15,     0,   131,    16,   132,   124,    18,   324,    20,    17,
      19,   125,   397,   398,   399,   306,   334,   298,     0,     0,
       0,     0,     0,     0,     0,   375,   368,   286,   307,     9,
     338,   342,   133,   211,   221,   222,   210,   154,   166,   112,
     114,     0,   108,   111,   214,     0,     0,   152,   110,   183,
       0,     0,     0,     0,     0,     0,   178,   179,   180,     0,
       0,   190,   188,   192,   193,     0,   187,   189,   191,   200,
     201,   202,     0,     0,     0,   196,   207,   208,   205,   206,
       0,     0,   143,   145,     0,     0,   293,     0,     0,     0,
     287,     0,     0,   394,   329,   326,    21,     0,   138,     0,
       0,    34,   320,     0,     0,    27,     0,     0,   128,   303,
       0,   376,     0,     0,     0,   299,   365,     0,   369,   366,
     333,     0,     0,     0,     0,     0,   113,     0,   168,     0,
     167,     0,   219,     0,     4,     6,     0,   149,   152,   110,
     163,   162,   161,   160,   159,   220,   158,   155,   156,   157,
       8,   181,   182,   176,   177,   194,   184,   185,   186,     0,
     197,   204,   212,     0,   295,     0,     0,     0,   309,     0,
     121,     0,   395,   325,     0,   396,     0,     0,     0,     0,
       0,     0,     0,    37,    35,   319,     0,    25,    28,     0,
       0,     0,     0,   278,     0,     0,     0,   278,     0,   141,
       0,   229,     0,     0,     0,   223,   225,   227,   228,   230,
     231,   239,   240,     9,   241,   271,   242,   273,   232,   233,
     234,   235,   236,   237,   268,     0,     0,     0,     0,   377,
       0,   292,     0,   292,   300,   301,     0,   328,   171,   174,
     109,   111,   110,    54,   214,    58,   172,   173,   166,     0,
      57,   148,     0,     0,   294,   288,   314,     0,     0,     0,
     122,     0,     0,     0,   351,   352,   350,     0,   402,     0,
     139,   140,     0,     0,    49,   101,     0,     0,     0,    98,
     317,     0,   321,     0,   355,     0,     0,    31,    29,    30,
      72,    73,   238,     0,   279,   280,     0,   250,   251,   246,
       0,     0,   243,     0,   282,     0,   260,   226,     0,   305,
     407,     0,   224,   274,   276,   255,     0,     0,     0,     0,
     268,   265,   261,     0,     0,   269,   354,   327,     0,   317,
       0,     0,   379,   302,    31,     0,     0,   169,   150,   296,
     315,     0,     0,   310,   311,   303,     0,     0,   404,   400,
       0,    47,     0,    48,    51,    50,    36,   102,     0,    97,
       0,     0,     0,   318,     0,     0,     0,    38,    45,    65,
      39,    40,    41,    67,    68,    42,    43,    44,    46,     0,
      33,     0,   323,     0,     0,     0,     0,     0,   254,     0,
       0,   284,     0,     0,     0,     0,   249,   364,   283,     0,
       0,   356,   357,     0,   272,     0,     0,     0,     0,   266,
       0,     0,   304,   378,     0,     0,     0,     0,     0,     0,
     388,   389,   390,     0,     0,   391,     0,     0,     0,     0,
     297,     0,   170,   311,     0,   312,   308,   406,     0,     0,
     353,    55,   103,     0,    99,     0,   115,     0,    55,    66,
      53,    64,    63,     0,    61,     0,   316,     9,    85,    23,
      84,     0,    77,     0,    81,   214,     0,    32,    22,   322,
     281,   277,     0,   247,     0,   244,   363,   141,   362,     0,
     360,     0,     0,   256,   264,     0,   262,   258,     0,   214,
     268,   385,   387,   384,   392,   383,     0,   370,   379,     0,
     381,   380,   371,    59,     0,     0,     0,     0,    56,    52,
      31,   116,     0,    70,    69,    60,     0,    87,     0,     0,
       0,    80,     0,     0,    83,    76,    75,     0,     0,     0,
       0,     0,     0,   267,   263,     0,     0,     0,   382,     0,
     374,     0,   313,   403,     0,     0,   100,    55,    62,     0,
       0,     0,     0,     0,     9,    91,     9,     0,    78,     0,
      82,    79,   245,     0,     0,   361,   253,     0,   270,   386,
     393,   372,     0,   401,     0,    71,    96,     0,     9,     0,
       0,     0,    88,    89,    74,     0,     0,     0,     0,     0,
       9,    90,    31,    86,    92,     9,     0,   373,     0,     0,
       0,    94,   405,     0,     0,   105,    95,     0,     0,   106,
       9,     0,     0,   104,   107
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -603,  -603,     0,  -603,   853,   -31,  -603,  -603,  -603,   -36,
     807,  -603,  -603,  -429,  -603,  -603,  -603,  -603,  -603,  -231,
    -603,  -603,  -603,  -149,  -508,  -180,  -603,  -603,   393,  -603,
    -603,  -603,  -603,  -329,  -603,  -603,  -602,  -603,   392,  -603,
    -603,  -492,  -603,  -603,   301,  -603,  -603,   333,   804,  -603,
     558,  -603,   341,  -603,   322,  -566,   796,  -223,   598,   -20,
     672,  -603,  -179,  -603,   940,  -603,   -29,  -313,   574,    -3,
    -603,   878,   880,  -603,   797,   -40,  -603,  -603,   753,  -603,
    -603,  -603,   993,   -15,  -603,  -603,   714,  -603,  -603,   -14,
    -603,  -603,   -42,  -603,   856,   -25,   -68,   -16,  -603,  -389,
    -279,  -603,  -603,  -603,  -603,  -603,  -603,  -603,   547,  -603,
    -383,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,
    -603,  -481,  -412,  -603,  -603,  -295,  -603,  -603,  -603,   741,
    -603,  -603,  -603,    73,    30,    -6,   -59,  -603,  -603,  -170,
    -603,  -603,    -1,  -603,   235,  1016,  -603,   513,     2,  -603,
     619,   621,    -4,  -603,  -603,   249,   -77,  -603,  -603,  1046,
    -603,  1050,  1021,  1029,  -603,  -603,  -603,  -603,  -603,   653,
     438,   440,  -603,   288,  -603,  -603,  -603,   474,  -603,  -603,
    -603,  -603,   891,  -603,  -603,  -603,  -603,  -603,  -603,  -603
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     2,   311,    55,    56,     8,    98,    99,   100,   101,
     102,   300,   397,   496,   103,   293,   391,   477,   104,   635,
     463,   464,   478,   618,   634,   150,   480,   563,   564,   481,
     482,   483,   484,   399,   400,   401,   636,   571,   572,   494,
     573,   574,   486,   568,   663,   664,   692,   665,   294,   388,
     389,   553,   666,   714,   715,   151,   152,   153,   487,   366,
     278,   279,   106,   107,   108,   109,    57,   207,   142,    83,
      21,    59,    60,    61,   246,   247,    62,    63,   254,    64,
      65,   156,   248,   249,   169,   170,    67,   182,   183,    68,
      69,   184,    70,   190,    71,    72,    73,    74,    75,   314,
     315,   316,   317,   318,   319,   320,   321,   408,   409,   410,
     411,   504,   322,   323,   513,   593,   324,   325,   430,   431,
     520,   432,   434,   326,   433,   217,   327,   423,   328,   405,
     500,   329,   330,   110,   111,   435,   195,   196,   273,   133,
     346,    29,   112,   331,   113,    32,   369,   546,   114,   451,
     488,   489,   115,   213,   116,   117,    89,     4,    11,    12,
      13,    14,    44,    35,    36,   376,   118,   119,   424,   511,
     589,   590,   332,   120,    38,   136,   340,   539,   534,   535,
      39,    40,   203,   121,   122,   123,   458,   549,   124,   333
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
       3,    66,   131,   267,   126,   236,   199,    20,    17,   105,
     351,    30,   155,    41,    33,   540,    81,   144,   518,   280,
     143,   505,   146,   154,   274,   200,   642,   185,   598,    97,
     145,   417,   127,   444,   135,   422,   596,   158,   159,   516,
     141,    30,    28,   619,    33,   147,   517,   232,   682,   385,
     624,   555,   639,   677,   353,   227,    45,   696,   204,    82,
     383,   485,   495,   591,   243,   694,   212,   222,   132,   398,
     720,   450,    28,     1,  -292,   193,     5,    77,    87,   197,
      78,   385,   201,   223,   386,    27,   224,   702,    46,    47,
      48,    -2,   298,   149,   194,   255,   385,    45,   226,   438,
     370,   209,   299,   592,    18,    50,    96,    97,   230,   418,
     530,   610,     7,   582,    18,    27,   580,   644,    49,    18,
      50,    51,   233,    96,    52,   220,    17,   640,   233,    46,
      47,    48,   640,   154,    53,    54,   141,   288,   521,   289,
     670,   655,   270,   337,     9,   233,    66,   717,   159,   685,
     260,   179,   180,   181,   268,    96,    18,   280,   131,    49,
      18,    50,    51,    10,   414,    52,   272,   255,   266,   269,
      96,   718,    87,   285,    16,    53,    54,   367,   276,   419,
     162,   127,    18,    50,   163,     1,   164,   312,   645,   280,
     284,   656,   394,   449,    15,    76,   382,   356,    97,   357,
     313,   166,   250,   713,   154,   354,   290,   288,   154,   289,
     154,   713,   154,   251,   252,    22,   341,   349,   352,   159,
     269,    18,   395,   159,    76,   159,   422,   159,   281,   360,
      17,   128,    18,   167,    25,   465,   617,   422,   166,   556,
     620,   479,    76,    85,   560,   256,   365,    31,   129,    95,
     674,   544,   130,    76,   250,    96,   387,   250,   168,    95,
     659,    34,   384,   253,   459,   251,   252,   392,   251,   252,
     167,   384,   379,   710,   404,   406,    97,    31,   404,    97,
     660,   312,   440,   374,   442,   312,   375,   403,   697,   -93,
     407,    34,   425,   415,   313,   168,   604,    76,   313,   355,
      37,   147,   387,   422,    49,    18,    50,   706,   616,   281,
     166,     1,    45,   570,   373,   253,    18,    79,   253,   198,
      52,   497,   448,    52,   621,   125,   166,   131,   166,   -93,
      37,   166,  -214,   154,    96,  -214,    97,   202,    87,   363,
     570,   364,   167,   396,    46,    47,   148,   137,   159,   149,
     243,   138,   171,    18,   172,   173,   335,   194,   167,   659,
     167,   165,   456,   167,   452,    76,   281,   168,    97,   351,
      18,    50,  -214,   508,    49,    18,    50,    51,   491,   660,
      52,   312,    97,   168,   191,   168,   174,  -214,   168,   547,
      53,    54,   175,   613,   313,   422,  -151,  -151,   281,   312,
     202,    18,    50,   509,   661,    86,   514,   205,   623,   162,
       1,   166,   313,   235,   407,   164,   186,   206,   422,  -214,
    -214,  -214,  -214,  -214,    87,    17,    97,   422,   586,   426,
     541,   187,   387,    96,   176,   177,   178,   179,   180,   181,
     427,   384,    22,   167,    88,   586,   384,   551,   428,   147,
     336,   545,   570,   353,   587,    50,   557,   558,   372,   559,
      45,    25,   295,   166,   575,   296,   188,   189,   168,    87,
     216,    18,    50,   312,   569,   210,   312,   429,   166,   269,
     577,   588,   219,   211,   407,   166,   313,   312,   407,   313,
     351,   599,    46,    47,    48,   167,    76,   149,   384,   228,
     313,    87,   470,   234,   229,   396,   269,   608,   609,   611,
     167,   160,   161,   471,   472,   371,   538,   167,    45,   708,
     168,    88,    49,    18,    50,    51,   384,   231,    52,   473,
     240,   474,   475,   615,    87,   168,   628,   237,    53,    54,
     545,   585,   168,   234,   234,   244,   344,   384,   653,   261,
      46,    47,    48,   312,    88,   262,   162,   345,   470,   287,
     286,   396,   164,   154,   265,   695,   313,   162,   476,   524,
     525,   235,   275,   164,   288,     6,   289,   352,   159,    90,
      49,    18,    50,    51,    19,   473,    52,   526,   527,   162,
      58,   291,   662,   286,   283,   164,    80,  -117,    19,    84,
     528,   669,   292,   599,   354,     1,     9,   657,  -117,   588,
     312,   588,    91,    22,    23,    22,    23,    96,   269,   269,
     214,   215,   154,   313,   529,    10,  -153,  -153,    17,    92,
       1,    24,    25,   691,    25,   693,   338,   159,   384,   334,
     684,   460,    93,    26,   347,   312,    94,    95,   312,   348,
     192,   166,    19,    96,   671,   662,    19,   701,   313,    19,
     384,   313,    45,   214,   225,   208,    19,   312,   312,   709,
     354,   147,   166,   368,   711,   498,   468,   312,   469,   390,
     313,   313,    45,   167,   154,   269,   662,   238,   239,   722,
     313,    17,   393,    17,    46,    47,    48,   402,   352,   159,
      45,    17,    19,   221,   167,   412,   565,   416,   168,    17,
     166,    17,   699,   566,    46,    47,   358,   567,   171,   149,
     172,   173,    17,   420,    49,    18,    50,    51,   436,   168,
      52,   437,    46,    47,    48,    58,   439,   149,   257,   441,
      53,    54,   167,   443,    49,    18,    50,    51,    45,   445,
      52,   454,   174,   162,   377,    45,   455,   163,   175,   164,
      53,    54,    49,    18,    50,    51,   241,   168,    52,   179,
     180,   181,   361,   362,   446,   242,  -151,   462,    53,    54,
      46,    47,    48,    19,    82,   501,   162,    46,    47,    48,
     286,   457,   164,   378,   214,   467,    80,   342,   343,   461,
     176,   177,   178,   179,   180,   181,   466,   234,   447,   362,
      49,    18,    50,    51,   502,   503,    52,    49,    18,    50,
      51,   301,   493,    52,   162,   490,    53,    54,   286,   492,
     164,   162,   162,    53,    54,   286,   235,   164,   164,  -259,
     495,   302,    87,  -259,   561,   562,   625,   626,  -248,  -248,
    -248,    19,   499,   303,  -259,   506,   301,   304,   305,   507,
     650,   651,  -259,   381,   510,   286,   306,   164,   629,   630,
     631,   632,     1,   515,  -259,   307,   302,   519,  -259,   522,
     308,   214,   689,   263,   264,  -275,   523,   421,   303,  -259,
     536,  -259,   304,   305,   537,   309,    50,  -259,   310,   538,
     542,   306,   166,   301,   543,   548,   550,     1,    80,   552,
     307,   576,   622,   578,   579,   308,   581,   584,   594,   428,
     208,  -259,   597,   302,   600,  -259,  -259,   601,   602,   603,
     309,    50,  -358,   310,   167,   303,  -259,   605,   301,   304,
     305,    19,   234,   606,  -259,   566,   607,   612,   306,   637,
     627,   460,   638,   641,     1,   643,  -259,   307,   302,   168,
    -259,   646,   308,    46,   648,    48,   654,  -257,   647,   652,
     303,  -259,  -358,  -259,   304,   305,   667,   309,    50,  -259,
     310,   679,   672,   306,   676,   678,   681,   680,   690,     1,
     700,   683,   307,    49,    18,    50,    51,   308,   301,    52,
     686,   688,   703,   671,    80,   707,   712,  -257,  -259,   698,
     716,   721,   309,    50,   245,   310,  -259,   723,   302,   658,
    -259,   297,   668,   724,   704,   339,   554,  -359,    19,   350,
     303,  -259,   705,   301,   304,   305,   719,   531,   359,  -259,
     453,   380,   258,   306,   259,   157,   271,   218,   413,     1,
     583,  -259,   307,   302,   134,  -259,   614,   308,   532,    42,
     533,   301,   595,    43,   140,   303,  -259,  -359,  -259,   304,
     305,   139,   309,    50,  -259,   310,   673,   512,   306,  -259,
     675,   302,   649,  -259,     1,     0,     0,   307,     0,   282,
    -252,     0,   308,   303,  -259,     0,   301,   304,   305,     0,
       0,     0,  -259,  -259,     0,     0,   306,   309,    50,     0,
     310,     0,     1,     0,  -259,   307,   302,    19,  -259,     0,
     308,     0,     0,   208,     0,    90,     0,   208,   303,  -259,
       0,  -259,   304,   305,     0,   309,    50,  -259,   310,     0,
       0,   306,     0,     0,     0,     0,     0,     1,     0,     0,
     307,    90,     0,     0,  -119,   308,     0,     0,    91,    22,
      23,     0,     0,     0,     0,     0,  -259,     0,   202,  -118,
     309,    50,     0,   310,    80,   277,     1,  -119,    25,     0,
    -118,     0,    90,     0,    91,    22,    23,     0,    93,     0,
       0,     0,    94,    95,     0,     0,   633,     0,     0,    96,
    -117,    92,     1,     0,    25,     0,     0,    90,     0,     0,
       0,  -117,     0,     0,    93,    91,    22,    23,    94,    95,
     162,   -55,   -55,     0,   235,    96,   164,     0,     0,    90,
       0,     0,    92,     1,   687,    25,  -120,     0,    90,     0,
      91,    22,    23,     0,     0,    93,     0,  -117,     0,    94,
      95,     0,     0,     0,     0,     0,    96,   277,     1,  -120,
      25,     0,    91,    22,    23,    90,     0,  -119,     0,   -55,
      93,    91,    22,    23,    94,    95,     0,     0,     0,    92,
       1,    96,    25,   234,     0,     0,     0,     0,   277,     1,
    -119,    25,    93,     0,  -119,     0,    94,    95,    91,    22,
      23,    93,     0,    96,     0,    94,    95,   162,   -55,   -55,
       0,   235,    96,   164,     0,   277,     1,     0,    25,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    93,     0,
       0,     0,    94,    95,     0,     0,     0,     0,     0,    96
};

static const yytype_int16 yycheck[] =
{
       0,    16,    38,   183,    35,   154,    83,    10,     8,    29,
     233,    12,    52,    13,    12,   444,    22,    46,   430,   198,
      45,   410,    47,    52,   194,    84,   592,    69,   520,    29,
      46,   310,     1,   346,    38,   314,   517,    52,    52,   428,
      44,    42,    12,   551,    42,     1,   429,    10,     4,     1,
     558,    22,    10,    10,   234,   132,    12,    10,    87,    20,
     291,   390,     9,    30,     3,   667,    95,    35,    38,   300,
      10,   366,    42,    52,    60,    78,    75,    79,    59,    82,
      82,     1,    85,    51,     4,    12,    54,   689,    44,    45,
      46,     0,    22,    49,    80,   163,     1,    12,    79,     4,
     279,    20,    32,    70,    75,    76,    75,   107,   139,     9,
     439,     4,     0,   502,    75,    42,   499,   598,    74,    75,
      76,    77,    85,    75,    80,   128,   126,    85,    85,    44,
      45,    46,    85,   162,    90,    91,   140,    84,   433,    86,
     632,    18,   184,   220,    53,    85,   161,   713,   162,   657,
     165,    90,    91,    92,   183,    75,    75,   336,   194,    74,
      75,    76,    77,    72,    79,    80,   191,   235,   182,   183,
      75,    30,    59,   202,    80,    90,    91,    20,    40,    79,
      80,     1,    75,    76,    84,    52,    86,   216,   600,   368,
      81,   620,    79,   363,    79,    86,    40,   237,   198,   239,
     216,    16,    14,    70,   233,   234,   209,    84,   237,    86,
     239,    70,   241,    25,    26,    35,   222,   232,   233,   233,
     234,    75,   299,   237,    86,   239,   505,   241,   198,   243,
     230,    51,    75,    48,    54,   384,   549,   516,    16,   470,
     553,   390,    86,    80,   475,    15,   275,    12,    68,    69,
     639,    40,    72,    86,    14,    75,   292,    14,    73,    69,
       1,    12,   291,    75,    79,    25,    26,   296,    25,    26,
      48,   300,   287,   702,   303,   304,   276,    42,   307,   279,
      21,   310,   341,   284,   343,   314,   284,   302,   677,    30,
     305,    42,   323,   308,   310,    73,   527,    86,   314,    56,
      12,     1,   338,   582,    74,    75,    76,   696,    30,   279,
      16,    52,    12,   493,   284,    75,    75,    76,    75,    40,
      80,   398,   362,    80,   555,    79,    16,   363,    16,    70,
      42,    16,     3,   362,    75,     6,   336,    44,    59,    79,
     520,    81,    48,    17,    44,    45,    46,    79,   362,    49,
       3,    79,     5,    75,     7,     8,    63,    80,    48,     1,
      48,    10,   377,    48,   367,    86,   336,    73,   368,   592,
      75,    76,    43,    79,    74,    75,    76,    77,   393,    21,
      80,   410,   382,    73,     6,    73,    39,    58,    73,    79,
      90,    91,    45,    81,   410,   674,    81,    82,   368,   428,
      44,    75,    76,   418,    46,    40,   426,    79,   557,    80,
      52,    16,   428,    84,   429,    86,    43,    79,   697,    90,
      91,    92,    93,    94,    59,   425,   426,   706,    49,    23,
     445,    58,   468,    75,    87,    88,    89,    90,    91,    92,
      34,   470,    35,    48,    79,    49,   475,   462,    42,     1,
      40,   454,   632,   633,    75,    76,   471,   472,    51,   474,
      12,    54,    79,    16,   493,    82,    93,    94,    73,    59,
      19,    75,    76,   502,    79,    75,   505,    71,    16,   493,
     495,   510,    40,    75,   499,    16,   502,   516,   503,   505,
     713,   520,    44,    45,    46,    48,    86,    49,   527,    79,
     516,    59,    14,    56,    79,    17,   520,   536,   537,   538,
      48,    81,    82,    25,    26,    40,    40,    48,    12,   699,
      73,    79,    74,    75,    76,    77,   555,    57,    80,    41,
      81,    43,    44,   548,    59,    73,   567,    82,    90,    91,
     543,    79,    73,    56,    56,    79,    39,   576,    79,    67,
      44,    45,    46,   582,    79,    28,    80,    50,    14,    69,
      84,    17,    86,   592,    39,    56,   582,    80,    80,    25,
      26,    84,    60,    86,    84,     1,    86,   592,   592,     1,
      74,    75,    76,    77,    10,    41,    80,    43,    44,    80,
      16,    40,   628,    84,    79,    86,    22,    19,    24,    25,
      56,   630,    80,   632,   633,    52,    53,   622,    30,   638,
     639,   640,    34,    35,    36,    35,    36,    75,   632,   633,
      82,    83,   651,   639,    80,    72,    81,    82,   628,    51,
      52,    51,    54,   664,    54,   666,    80,   651,   667,    30,
     655,    63,    64,    63,    79,   674,    68,    69,   677,    81,
      76,    16,    78,    75,     4,   691,    82,   688,   674,    85,
     689,   677,    12,    82,    83,    91,    92,   696,   697,   700,
     699,     1,    16,    53,   705,    40,    79,   706,    81,    40,
     696,   697,    12,    48,   713,   699,   722,    81,    82,   720,
     706,   691,     9,   693,    44,    45,    46,    79,   713,   713,
      12,   701,   128,   129,    48,    79,    46,    83,    73,   709,
      16,   711,    56,    53,    44,    45,    46,    57,     5,    49,
       7,     8,   722,    79,    74,    75,    76,    77,    79,    73,
      80,    79,    44,    45,    46,   161,    40,    49,   164,    40,
      90,    91,    48,    50,    74,    75,    76,    77,    12,    80,
      80,    30,    39,    80,    18,    12,    40,    84,    45,    86,
      90,    91,    74,    75,    76,    77,    72,    73,    80,    90,
      91,    92,    81,    82,    57,    81,    82,    26,    90,    91,
      44,    45,    46,   209,    20,    79,    80,    44,    45,    46,
      84,    18,    86,    57,    82,    83,   222,   223,   224,    79,
      87,    88,    89,    90,    91,    92,    81,    56,    81,    82,
      74,    75,    76,    77,    28,    29,    80,    74,    75,    76,
      77,     1,    80,    80,    80,    79,    90,    91,    84,    79,
      86,    80,    80,    90,    91,    84,    84,    86,    86,    19,
       9,    21,    59,    23,    74,    75,    81,    82,    28,    29,
      30,   277,    70,    33,    34,    67,     1,    37,    38,    79,
      79,    80,    42,   289,    70,    84,    46,    86,    81,    82,
      81,    82,    52,    75,    19,    55,    21,    61,    23,    79,
      60,    82,    83,   169,   170,    30,    81,    32,    33,    34,
      60,    71,    37,    38,    44,    75,    76,    42,    78,    40,
      81,    46,    16,     1,    30,    43,    79,    52,   334,    14,
      55,    47,    26,    79,    79,    60,    79,    30,    39,    42,
     346,    19,    79,    21,    30,    23,    71,     4,     4,     4,
      75,    76,    30,    78,    48,    33,    34,     4,     1,    37,
      38,   367,    56,     4,    42,    53,    79,    79,    46,    38,
      57,    63,    83,    21,    52,    42,    19,    55,    21,    73,
      23,    26,    60,    44,    81,    46,    57,    30,    72,    79,
      33,    34,    70,    71,    37,    38,    47,    75,    76,    42,
      78,     4,    79,    46,    79,    79,    79,    53,    30,    52,
      40,    79,    55,    74,    75,    76,    77,    60,     1,    80,
      79,    79,    57,     4,   430,    79,    79,    70,    71,    81,
      79,    21,    75,    76,   161,    78,    19,    79,    21,   626,
      23,   214,   630,   722,   691,   221,   468,    30,   454,   233,
      33,    34,   691,     1,    37,    38,   714,   439,   241,    42,
     368,   288,   164,    46,   164,    52,   190,   107,   307,    52,
     503,    19,    55,    21,    38,    23,   543,    60,   439,    13,
     439,     1,    30,    13,    43,    33,    34,    70,    71,    37,
      38,    42,    75,    76,    42,    78,   638,   424,    46,    19,
     640,    21,   608,    23,    52,    -1,    -1,    55,    -1,   198,
      30,    -1,    60,    33,    34,    -1,     1,    37,    38,    -1,
      -1,    -1,    42,    71,    -1,    -1,    46,    75,    76,    -1,
      78,    -1,    52,    -1,    19,    55,    21,   543,    23,    -1,
      60,    -1,    -1,   549,    -1,     1,    -1,   553,    33,    34,
      -1,    71,    37,    38,    -1,    75,    76,    42,    78,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    -1,    52,    -1,    -1,
      55,     1,    -1,    -1,    30,    60,    -1,    -1,    34,    35,
      36,    -1,    -1,    -1,    -1,    -1,    71,    -1,    44,    19,
      75,    76,    -1,    78,   600,    51,    52,    53,    54,    -1,
      30,    -1,     1,    -1,    34,    35,    36,    -1,    64,    -1,
      -1,    -1,    68,    69,    -1,    -1,    56,    -1,    -1,    75,
      19,    51,    52,    -1,    54,    -1,    -1,     1,    -1,    -1,
      -1,    30,    -1,    -1,    64,    34,    35,    36,    68,    69,
      80,    81,    82,    -1,    84,    75,    86,    -1,    -1,     1,
      -1,    -1,    51,    52,   660,    54,    30,    -1,     1,    -1,
      34,    35,    36,    -1,    -1,    64,    -1,    19,    -1,    68,
      69,    -1,    -1,    -1,    -1,    -1,    75,    51,    52,    53,
      54,    -1,    34,    35,    36,     1,    -1,    30,    -1,    42,
      64,    34,    35,    36,    68,    69,    -1,    -1,    -1,    51,
      52,    75,    54,    56,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,    64,    -1,    30,    -1,    68,    69,    34,    35,
      36,    64,    -1,    75,    -1,    68,    69,    80,    81,    82,
      -1,    84,    75,    86,    -1,    51,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    64,    -1,
      -1,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    75
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    52,    96,    97,   252,    75,   163,     0,   100,    53,
      72,   253,   254,   255,   256,    79,    80,    97,    75,   163,
     164,   165,    35,    36,    51,    54,    63,   228,   229,   236,
     237,   239,   240,   243,   250,   258,   259,   268,   269,   275,
     276,    97,   254,   256,   257,    12,    44,    45,    46,    74,
      76,    77,    80,    90,    91,    98,    99,   161,   163,   166,
     167,   168,   171,   172,   174,   175,   178,   181,   184,   185,
     187,   189,   190,   191,   192,   193,    86,    79,    82,    76,
     163,   230,    20,   164,   163,    80,    40,    59,    79,   251,
       1,    34,    51,    64,    68,    69,    75,    97,   101,   102,
     103,   104,   105,   109,   113,   154,   157,   158,   159,   160,
     228,   229,   237,   239,   243,   247,   249,   250,   261,   262,
     268,   278,   279,   280,   283,    79,   100,     1,    51,    68,
      72,   104,   229,   234,   240,   247,   270,    79,    79,   258,
     257,   247,   163,   190,   161,   192,   190,     1,    46,    49,
     120,   150,   151,   152,   161,   170,   176,   177,   178,   184,
      81,    82,    80,    84,    86,    10,    16,    48,    73,   179,
     180,     5,     7,     8,    39,    45,    87,    88,    89,    90,
      91,    92,   182,   183,   186,   187,    43,    58,    93,    94,
     188,     6,   163,   164,    80,   231,   232,   164,    40,   251,
     231,   164,    44,   277,   161,    79,    79,   162,   163,    20,
      75,    75,   161,   248,    82,    83,    19,   220,   159,    40,
     164,   163,    35,    51,    54,    83,    79,   251,    79,    79,
     100,    57,    10,    85,    56,    84,   118,    82,    81,    82,
      81,    72,    81,     3,    79,    99,   169,   170,   177,   178,
      14,    25,    26,    75,   173,   191,    15,   163,   166,   167,
     178,    67,    28,   181,   181,    39,   184,   120,   161,   184,
     187,   189,   190,   233,   234,    60,    40,    51,   155,   156,
     157,   229,   277,    79,    81,   161,    84,    69,    84,    86,
     164,    40,    80,   110,   143,    79,    82,   105,    22,    32,
     106,     1,    21,    33,    37,    38,    46,    55,    60,    75,
      78,    97,   161,   192,   194,   195,   196,   197,   198,   199,
     200,   201,   207,   208,   211,   212,   218,   221,   223,   226,
     227,   238,   267,   284,    30,    63,    40,   251,    80,   143,
     271,   230,   163,   163,    39,    50,   235,    79,    81,   178,
     151,   152,   178,   120,   161,    56,   170,   170,    46,   169,
     184,    81,    82,    79,    81,   161,   154,    20,    53,   241,
     157,    40,    51,   229,   237,   243,   260,    18,    57,   178,
     173,   163,    40,   114,   161,     1,     4,   104,   144,   145,
      40,   111,   161,     9,    79,   251,    17,   107,   114,   128,
     129,   130,    79,   178,   161,   224,   161,   178,   202,   203,
     204,   205,    79,   224,    79,   178,    83,   195,     9,    79,
      79,    32,   195,   222,   263,   100,    23,    34,    42,    71,
     213,   214,   216,   219,   217,   230,    79,    79,     4,    40,
     231,    40,   231,    50,   162,    80,    57,    81,   170,   234,
     220,   244,   164,   155,    30,    40,   178,    18,   281,    79,
      63,    79,    26,   115,   116,   118,    81,    83,    79,    81,
      14,    25,    26,    41,    43,    44,    80,   112,   117,   118,
     121,   124,   125,   126,   127,   128,   137,   153,   245,   246,
      79,   178,    79,    80,   134,     9,   108,   251,    40,    70,
     225,    79,    28,    29,   206,   194,    67,    79,    79,   178,
      70,   264,   264,   209,   154,    75,   194,   205,   217,    61,
     215,   220,    79,    81,    25,    26,    43,    44,    56,    80,
     128,   153,   245,   246,   273,   274,    60,    44,    40,   272,
     108,   178,    81,    30,    40,   164,   242,    79,    43,   282,
      79,   178,    14,   146,   145,    22,   114,   178,   178,   178,
     114,    74,    75,   122,   123,    46,    53,    57,   138,    79,
     120,   132,   133,   135,   136,   161,    47,   178,    79,    79,
     205,    79,   194,   203,    30,    79,    49,    75,   161,   265,
     266,    30,    70,   210,    39,    30,   216,    79,   136,   161,
      30,     4,     4,     4,   114,     4,     4,    79,   161,   161,
       4,   161,    79,    81,   242,   178,    30,   162,   118,   119,
     162,   114,    26,   118,   119,    81,    82,    57,   100,    81,
      82,    81,    82,    56,   119,   114,   131,    38,    83,    10,
      85,    21,   150,    42,   216,   217,    26,    72,    81,   272,
      79,    80,    79,    79,    57,    18,   108,   178,   123,     1,
      21,    46,   104,   139,   140,   142,   147,    47,   133,   161,
     136,     4,    79,   265,   194,   266,    79,    10,    79,     4,
      53,    79,     4,    79,   178,   119,    79,   163,    79,    83,
      30,   100,   141,   100,   131,    56,    10,   194,    81,    56,
      40,   100,   131,    57,   142,   147,   194,    79,   120,   100,
     108,   100,    79,    70,   148,   149,    79,   150,    30,   149,
      10,    21,   100,    79,   139
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,    95,    96,    97,    97,    98,    98,    99,    99,   100,
     100,   101,   101,   101,   101,   101,   101,   101,   101,   101,
     101,   101,   102,   103,   104,   104,   105,   106,   106,   107,
     107,   108,   108,   109,   110,   110,   110,   111,   111,   112,
     112,   112,   112,   112,   112,   112,   112,   113,   114,   114,
     115,   115,   116,   117,   118,   119,   119,   120,   120,   120,
     121,   122,   122,   123,   123,   124,   124,   125,   125,   126,
     127,   127,   128,   128,   129,   130,   131,   132,   132,   133,
     134,   135,   135,   136,   136,   137,   138,   138,   139,   139,
     139,   140,   140,   141,   141,   142,   142,   143,   144,   144,
     145,   145,   146,   146,   147,   148,   148,   149,   150,   150,
     151,   151,   151,   152,   152,   153,   153,   154,   154,   155,
     155,   156,   156,   157,   157,   157,   157,   158,   158,   159,
     159,   160,   160,   161,   161,   161,   161,   161,   162,   162,
     162,   163,   164,   164,   165,   165,   166,   167,   168,   169,
     169,   170,   170,   170,   170,   171,   171,   171,   171,   172,
     173,   173,   173,   173,   174,   174,   174,   175,   175,   175,
     175,   175,   176,   176,   177,   178,   178,   178,   179,   179,
     179,   180,   180,   181,   181,   181,   181,   182,   182,   182,
     182,   182,   182,   183,   183,   184,   184,   184,   185,   185,
     186,   186,   186,   187,   187,   188,   188,   188,   188,   189,
     189,   189,   189,   190,   190,   190,   190,   190,   191,   191,
     192,   193,   193,   194,   194,   195,   195,   196,   196,   196,
     197,   197,   197,   197,   197,   197,   197,   197,   197,   198,
     198,   198,   198,   199,   200,   201,   202,   202,   203,   204,
     205,   206,   206,   207,   208,   209,   209,   210,   211,   212,
     212,   213,   213,   213,   214,   215,   215,   216,   217,   217,
     218,   219,   219,   220,   221,   222,   222,   223,   224,   224,
     225,   225,   226,   226,   227,   228,   228,   229,   229,   229,
     230,   230,   231,   231,   232,   233,   233,   234,   234,   235,
     235,   235,   235,   236,   237,   238,   239,   239,   240,   241,
     241,   242,   242,   243,   244,   244,   245,   246,   246,   247,
     248,   248,   249,   249,   249,   250,   250,   250,   250,   251,
     252,   252,   252,   253,   253,   254,   254,   255,   255,   255,
     256,   257,   257,   258,   258,   258,   258,   258,   258,   258,
     259,   260,   260,   261,   261,   262,   263,   263,   264,   264,
     265,   265,   266,   266,   267,   268,   268,   269,   269,   270,
     270,   270,   270,   270,   270,   270,   271,   271,   271,   272,
     272,   272,   273,   273,   273,   273,   273,   273,   273,   273,
     273,   273,   274,   274,   275,   276,   277,   278,   278,   278,
     279,   280,   281,   281,   282,   282,   283,   284
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     3,     6,     1,     3,     1,     3,     0,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     6,     6,     1,     3,     1,     0,     1,     1,
       1,     0,     2,     5,     0,     1,     3,     0,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     5,     2,     1,
       1,     1,     3,     2,     2,     0,     1,     3,     3,     6,
       3,     1,     3,     1,     1,     1,     2,     1,     1,     3,
       3,     5,     1,     1,     6,     4,     1,     1,     3,     3,
       3,     1,     3,     2,     1,     2,     5,     2,     2,     2,
       3,     1,     3,     1,     3,     5,     2,     3,     1,     3,
       5,     1,     0,     1,     8,     1,     2,     5,     1,     3,
       1,     1,     1,     2,     1,     2,     3,     0,     1,     0,
       1,     1,     2,     1,     1,     1,     1,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       3,     1,     1,     3,     1,     3,     1,     1,     4,     1,
       3,     1,     1,     1,     1,     3,     3,     3,     3,     3,
       1,     1,     1,     1,     1,     1,     1,     3,     3,     5,
       6,     4,     3,     3,     3,     1,     3,     3,     1,     1,
       1,     2,     2,     1,     3,     3,     3,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     2,     3,     1,     1,
       1,     1,     1,     1,     3,     1,     1,     1,     1,     1,
       2,     2,     3,     1,     1,     1,     1,     1,     1,     3,
       3,     2,     2,     1,     2,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     1,
       1,     1,     1,     2,     4,     6,     1,     3,     2,     2,
       1,     0,     2,     6,     3,     0,     2,     4,     4,     0,
       2,     1,     3,     4,     3,     0,     1,     4,     0,     1,
       6,     0,     2,     2,     2,     0,     1,     4,     0,     1,
       0,     2,     2,     3,     3,     2,     2,     3,     5,     2,
       1,     1,     0,     1,     3,     1,     3,     5,     1,     0,
       1,     1,     2,     2,     6,     2,     2,     2,     7,     0,
       2,     0,     1,     9,     0,     1,     2,     0,     1,     3,
       1,     3,     6,     5,     1,     4,     3,     5,     4,     2,
       0,     2,     2,     4,     3,     0,     1,     2,     3,     2,
       3,     0,     2,     1,     1,     1,     1,     1,     1,     1,
       5,     1,     1,     6,     4,     4,     2,     2,     4,     6,
       1,     3,     1,     1,     3,     3,     3,     1,     2,     2,
       6,     6,     8,    10,     7,     1,     0,     1,     3,     0,
       2,     2,     3,     2,     2,     2,     4,     2,     1,     1,
       1,     1,     2,     4,     3,     4,     2,     1,     1,     1,
       5,     9,     0,     4,     0,     7,     6,     2
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
#line 179 "grammar83.y"
{
	yylloc = 1;
	memset(context, 0, sizeof(*context));
	context->symbol_table = calloc(64, sizeof(DeclList));
	context->symbol_table_capacity = 64;
	context->symbol_table_size = 0;
	if(!universal_int_type.name) {
		StringView universal_int_str_view = { .value = universal_integer_str, .len = sizeof(universal_integer_str) };
		universal_int_type.name = string_pool_to_token(universal_int_str_view);
	}
}

#line 2482 "grammar83.tab.c"

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
#line 225 "grammar83.y"
                                                                                {
	// TODO: support id list
	ObjectDecl* decl = create_object_decl((yyvsp[-5].str_token), (yyloc));
	Declaration* prev_decl = find_decl_in_scope(curr_scope, decl->name);
	if(prev_decl) {
		error_print((yyloc), "Redefinition of '%s' within same declarative region", ST(decl->name));
        error_print(prev_decl->line_num, "Previous definition here");
        error_exit();
	}
	decl->is_constant = (yyvsp[-3].bool_);
	TypeDecl* type_decl = find_type_decl(context, (yyvsp[-2].str_token));
	if(!type_decl) {
		error_print((yyloc), "Unknown type: %s", ST((yyvsp[-2].str_token)));
		error_exit();
	}
	decl->type = type_decl;
	decl->init_expr = (yyvsp[-1].expr);
	// TODO: handle deferred constants, which are legal and do not have initial expressions
	if(decl->is_constant && !decl->init_expr) {
		error_print((yyloc), "Constant declaration '%s' is not initialized", ST(decl->name));
        error_exit();
	}
	push_declaration(context, &decl->base);
	(yyval.decl) = &decl->base;
}
#line 2726 "grammar83.tab.c"
    break;

  case 23: /* number_decl: def_id_s ':' CONSTANT IS_ASSIGNED expression ';'  */
#line 251 "grammar83.y"
                                                               {
	// TODO: support id list
	ObjectDecl* decl = create_object_decl((yyvsp[-5].str_token), (yyloc));
	Declaration* prev_decl = find_decl_in_scope(curr_scope, decl->name);
	if(prev_decl) {
		error_print((yyloc), "Redefinition of '%s' within same declarative region", ST(decl->name));
        error_print(prev_decl->line_num, "Previous definition here");
        error_exit();
	}
	decl->is_constant = true;
	decl->type = &universal_int_type;
	decl->init_expr = (yyvsp[-1].expr);
	push_declaration(context, &decl->base);
	(yyval.decl) = &decl->base;
}
#line 2746 "grammar83.tab.c"
    break;

  case 25: /* def_id_s: def_id_s ',' def_id  */
#line 268 "grammar83.y"
                              { (yyval.str_token) = (yyvsp[0].str_token); /*TODO: make list*/ }
#line 2752 "grammar83.tab.c"
    break;

  case 27: /* object_qualifier_opt: %empty  */
#line 274 "grammar83.y"
                       { (yyval.bool_) = false; }
#line 2758 "grammar83.tab.c"
    break;

  case 28: /* object_qualifier_opt: CONSTANT  */
#line 275 "grammar83.y"
                           { (yyval.bool_) = true; }
#line 2764 "grammar83.tab.c"
    break;

  case 31: /* init_opt: %empty  */
#line 282 "grammar83.y"
                             { (yyval.expr) = NULL; }
#line 2770 "grammar83.tab.c"
    break;

  case 32: /* init_opt: IS_ASSIGNED expression  */
#line 283 "grammar83.y"
                                 { (yyval.expr) = (yyvsp[0].expr); }
#line 2776 "grammar83.tab.c"
    break;

  case 33: /* type_decl: TYPE identifier discrim_part_opt type_completion ';'  */
#line 286 "grammar83.y"
                                                                 {
	// TODO: discriminant
	TypeDecl* decl = (yyvsp[-1].type_decl);
	// Note: decl->base.kind is set by the specific type_completion
	decl->base.line_num = (yyloc);
	decl->name = (yyvsp[-3].str_token);
	Declaration* prev_decl = find_decl_in_scope(curr_scope, decl->name);
	if(prev_decl) {
		error_print((yyloc), "Redefinition of '%s' within same declarative region", ST(decl->name));
        error_print(prev_decl->line_num, "Previous definition here");
        error_exit();
	}
	push_declaration(context, &decl->base);
	(yyval.decl) = &decl->base;
}
#line 2796 "grammar83.tab.c"
    break;

  case 38: /* type_completion: IS type_def  */
#line 309 "grammar83.y"
                      { (yyval.type_decl) = (yyvsp[0].type_decl); }
#line 2802 "grammar83.tab.c"
    break;

  case 47: /* subtype_decl: SUBTYPE identifier IS subtype_ind ';'  */
#line 322 "grammar83.y"
                                                     {
	TypeDecl* decl = create_type_decl(TYPE_SUBTYPE);
	decl->base.line_num = (yyloc);
	decl->name = (yyvsp[-3].str_token);
	Declaration* prev_decl = find_decl_in_scope(curr_scope, decl->name);
	if(prev_decl) {
		error_print((yyloc), "Redefinition of '%s' within same declarative region", ST(decl->name));
        error_print(prev_decl->line_num, "Previous definition here");
        error_exit();
	}
	TypeDecl* base_type = find_type_decl(context, (yyvsp[-1].str_token));
	if(!base_type) {
		error_print((yyloc), "Unknown base type: %s", ST((yyvsp[-1].str_token)));
		error_exit();
	}
	decl->u.subtype.base = base_type;
	push_declaration(context, &decl->base);
	(yyval.decl) = &decl->base;
}
#line 2826 "grammar83.tab.c"
    break;

  case 48: /* subtype_ind: name constraint  */
#line 343 "grammar83.y"
                              { (yyval.str_token) = (yyvsp[-1].str_token); }
#line 2832 "grammar83.tab.c"
    break;

  case 53: /* derived_type: NEW subtype_ind  */
#line 354 "grammar83.y"
                               {
	(yyval.type_decl) = create_type_decl(TYPE_DERIVED);
	TypeDecl* base_type = find_type_decl(context, (yyvsp[0].str_token));
	if(!base_type) {
		error_print((yyloc), "Unknown base type: %s", ST((yyvsp[0].str_token)));
		error_exit();
	}
	(yyval.type_decl)->u.subtype.base = base_type;
}
#line 2846 "grammar83.tab.c"
    break;

  case 54: /* range_constraint: RANGE range  */
#line 364 "grammar83.y"
                               { (yyval.expr) = (yyvsp[0].expr); }
#line 2852 "grammar83.tab.c"
    break;

  case 55: /* range_constr_opt: %empty  */
#line 367 "grammar83.y"
                       { (yyval.expr) = NULL; }
#line 2858 "grammar83.tab.c"
    break;

  case 57: /* range: simple_expression DOT_DOT simple_expression  */
#line 371 "grammar83.y"
                                                    { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_RANGE, (yyvsp[0].expr)); }
#line 2864 "grammar83.tab.c"
    break;

  case 60: /* enumeration_type: '(' enum_id_s ')'  */
#line 376 "grammar83.y"
                                     {
	(yyval.type_decl) = create_type_decl(TYPE_ENUM);
	// TODO: set literals
}
#line 2873 "grammar83.tab.c"
    break;

  case 62: /* enum_id_s: enum_id_s ',' enum_id  */
#line 382 "grammar83.y"
                                { (yyval.expr) = (yyvsp[0].expr); /* TODO: make list */ }
#line 2879 "grammar83.tab.c"
    break;

  case 63: /* enum_id: identifier  */
#line 385 "grammar83.y"
                     {
	(yyval.expr) = create_expr(EXPR_NAME, (yyloc));
	(yyval.expr)->u.name = (yyvsp[0].str_token);
}
#line 2888 "grammar83.tab.c"
    break;

  case 64: /* enum_id: char_lit  */
#line 389 "grammar83.y"
                   {
	(yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
	(yyval.expr)->u.char_lit = (yyvsp[0].c);
}
#line 2897 "grammar83.tab.c"
    break;

  case 65: /* integer_type: range_constraint  */
#line 394 "grammar83.y"
                                {
	(yyval.type_decl) = create_type_decl(TYPE_INTEGER);
	(yyval.type_decl)->u.int_.range = (yyvsp[0].expr);
}
#line 2906 "grammar83.tab.c"
    break;

  case 146: /* used_char: char_lit  */
#line 562 "grammar83.y"
                     {
	(yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
	(yyval.expr)->u.char_lit = (yyvsp[0].c);
}
#line 2915 "grammar83.tab.c"
    break;

  case 164: /* literal: numeric_lit  */
#line 598 "grammar83.y"
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
#line 2939 "grammar83.tab.c"
    break;

  case 176: /* expression: expression logical relation  */
#line 636 "grammar83.y"
                                            { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 2945 "grammar83.tab.c"
    break;

  case 177: /* expression: expression short_circuit relation  */
#line 637 "grammar83.y"
                                            { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 2951 "grammar83.tab.c"
    break;

  case 178: /* logical: AND  */
#line 640 "grammar83.y"
              { (yyval.unary_op) = OP_AND; }
#line 2957 "grammar83.tab.c"
    break;

  case 179: /* logical: OR  */
#line 641 "grammar83.y"
                  { (yyval.unary_op) = OP_OR; }
#line 2963 "grammar83.tab.c"
    break;

  case 180: /* logical: XOR  */
#line 642 "grammar83.y"
                  { (yyval.unary_op) = OP_XOR; }
#line 2969 "grammar83.tab.c"
    break;

  case 181: /* short_circuit: AND THEN  */
#line 645 "grammar83.y"
                         { (yyval.unary_op) = OP_AND_THEN; }
#line 2975 "grammar83.tab.c"
    break;

  case 182: /* short_circuit: OR ELSE  */
#line 646 "grammar83.y"
                             { (yyval.unary_op) = OP_OR_ELSE; }
#line 2981 "grammar83.tab.c"
    break;

  case 184: /* relation: simple_expression relational simple_expression  */
#line 651 "grammar83.y"
                                                         { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 2987 "grammar83.tab.c"
    break;

  case 185: /* relation: simple_expression membership range  */
#line 652 "grammar83.y"
                                                         { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 2993 "grammar83.tab.c"
    break;

  case 186: /* relation: simple_expression membership name  */
#line 653 "grammar83.y"
                                                         { (yyval.expr) = (yyvsp[-2].expr); /*TODO*/ }
#line 2999 "grammar83.tab.c"
    break;

  case 187: /* relational: '='  */
#line 656 "grammar83.y"
                 { (yyval.unary_op) = OP_EQ; }
#line 3005 "grammar83.tab.c"
    break;

  case 188: /* relational: NE  */
#line 657 "grammar83.y"
                     { (yyval.unary_op) = OP_NEQ; }
#line 3011 "grammar83.tab.c"
    break;

  case 189: /* relational: '<'  */
#line 658 "grammar83.y"
                     { (yyval.unary_op) = OP_LT; }
#line 3017 "grammar83.tab.c"
    break;

  case 190: /* relational: LT_EQ  */
#line 659 "grammar83.y"
                     { (yyval.unary_op) = OP_LTE; }
#line 3023 "grammar83.tab.c"
    break;

  case 191: /* relational: '>'  */
#line 660 "grammar83.y"
                     { (yyval.unary_op) = OP_GT; }
#line 3029 "grammar83.tab.c"
    break;

  case 192: /* relational: GE  */
#line 661 "grammar83.y"
                     { (yyval.unary_op) = OP_GTE; }
#line 3035 "grammar83.tab.c"
    break;

  case 193: /* membership: IN  */
#line 664 "grammar83.y"
                { (yyval.unary_op) = OP_IN; }
#line 3041 "grammar83.tab.c"
    break;

  case 194: /* membership: NOT IN  */
#line 665 "grammar83.y"
                    { (yyval.unary_op) = OP_NOT_IN; }
#line 3047 "grammar83.tab.c"
    break;

  case 196: /* simple_expression: unary term  */
#line 669 "grammar83.y"
                                        { (yyval.expr) = make_unary_expr((yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3053 "grammar83.tab.c"
    break;

  case 197: /* simple_expression: simple_expression adding term  */
#line 670 "grammar83.y"
                                        { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3059 "grammar83.tab.c"
    break;

  case 198: /* unary: '+'  */
#line 673 "grammar83.y"
            { (yyval.unary_op) = OP_UNARY_PLUS; }
#line 3065 "grammar83.tab.c"
    break;

  case 199: /* unary: '-'  */
#line 674 "grammar83.y"
                { (yyval.unary_op) = OP_UNARY_MINUS; }
#line 3071 "grammar83.tab.c"
    break;

  case 200: /* adding: '+'  */
#line 677 "grammar83.y"
             { (yyval.unary_op) = OP_PLUS; }
#line 3077 "grammar83.tab.c"
    break;

  case 201: /* adding: '-'  */
#line 678 "grammar83.y"
                 { (yyval.unary_op) = OP_MINUS; }
#line 3083 "grammar83.tab.c"
    break;

  case 202: /* adding: '&'  */
#line 679 "grammar83.y"
                 { (yyval.unary_op) = OP_AMP; }
#line 3089 "grammar83.tab.c"
    break;

  case 204: /* term: term multiplying factor  */
#line 683 "grammar83.y"
                                  { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3095 "grammar83.tab.c"
    break;

  case 205: /* multiplying: '*'  */
#line 686 "grammar83.y"
                  { (yyval.unary_op) = OP_MULT; }
#line 3101 "grammar83.tab.c"
    break;

  case 206: /* multiplying: '/'  */
#line 687 "grammar83.y"
                      { (yyval.unary_op) = OP_DIVIDE; }
#line 3107 "grammar83.tab.c"
    break;

  case 207: /* multiplying: MOD  */
#line 688 "grammar83.y"
                      { (yyval.unary_op) = OP_MOD; }
#line 3113 "grammar83.tab.c"
    break;

  case 208: /* multiplying: REM  */
#line 689 "grammar83.y"
                      { (yyval.unary_op) = OP_REM; }
#line 3119 "grammar83.tab.c"
    break;

  case 210: /* factor: NOT primary  */
#line 693 "grammar83.y"
                                { (yyval.expr) = make_unary_expr(OP_NOT, (yyvsp[0].expr)); }
#line 3125 "grammar83.tab.c"
    break;

  case 211: /* factor: ABS primary  */
#line 694 "grammar83.y"
                                { (yyval.expr) = make_unary_expr(OP_ABS, (yyvsp[0].expr)); }
#line 3131 "grammar83.tab.c"
    break;

  case 212: /* factor: primary EXPON primary  */
#line 695 "grammar83.y"
                                { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_EXP, (yyvsp[0].expr)); }
#line 3137 "grammar83.tab.c"
    break;

  case 219: /* parenthesized_primary: '(' expression ')'  */
#line 706 "grammar83.y"
                                      { (yyval.expr) = (yyvsp[-1].expr); }
#line 3143 "grammar83.tab.c"
    break;

  case 224: /* statement_s: statement_s statement  */
#line 717 "grammar83.y"
                                { (yyval.stmt) = (yyvsp[-1].stmt); /* TODO (currently causes segfault) $$->next = $2;*/ }
#line 3149 "grammar83.tab.c"
    break;

  case 243: /* null_stmt: NuLL ';'  */
#line 746 "grammar83.y"
                     { (yyval.stmt) = create_stmt(STMT_NULL, (yyloc)); }
#line 3155 "grammar83.tab.c"
    break;

  case 244: /* assign_stmt: name IS_ASSIGNED expression ';'  */
#line 750 "grammar83.y"
                                              {
	(yyval.stmt) = create_stmt(STMT_ASSIGN, (yyloc));
	(yyval.stmt)->u.assign.expr = (yyvsp[-1].expr);
}
#line 3164 "grammar83.tab.c"
    break;

  case 249: /* cond_part: condition THEN  */
#line 765 "grammar83.y"
                           { (yyval.expr) = (yyvsp[-1].expr); }
#line 3170 "grammar83.tab.c"
    break;

  case 258: /* loop_stmt: label_opt loop_content id_opt ';'  */
#line 789 "grammar83.y"
                                              { (yyval.stmt) = (yyvsp[-2].stmt); }
#line 3176 "grammar83.tab.c"
    break;

  case 261: /* loop_content: basic_loop  */
#line 796 "grammar83.y"
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
#line 3191 "grammar83.tab.c"
    break;

  case 262: /* loop_content: WHILE condition basic_loop  */
#line 806 "grammar83.y"
                                     {
	(yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
	(yyval.stmt)->u.loop.kind = LOOP_WHILE;
	(yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
	(yyval.stmt)->u.loop.u.while_.condition = (yyvsp[-1].expr);
}
#line 3202 "grammar83.tab.c"
    break;

  case 263: /* loop_content: iter_part reverse_opt discrete_range basic_loop  */
#line 812 "grammar83.y"
                                                          {
	// TODO: identifier
	(yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
	(yyval.stmt)->u.loop.kind = LOOP_FOR;
	(yyval.stmt)->u.loop.reverse = (yyvsp[-2].bool_);
	(yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
}
#line 3214 "grammar83.tab.c"
    break;

  case 265: /* reverse_opt: %empty  */
#line 823 "grammar83.y"
              { (yyval.bool_) = false; }
#line 3220 "grammar83.tab.c"
    break;

  case 266: /* reverse_opt: REVERSE  */
#line 824 "grammar83.y"
                  { (yyval.bool_) = true; }
#line 3226 "grammar83.tab.c"
    break;

  case 267: /* basic_loop: LOOP statement_s END LOOP  */
#line 827 "grammar83.y"
                                       { (yyval.stmt) = (yyvsp[-2].stmt); }
#line 3232 "grammar83.tab.c"
    break;

  case 277: /* exit_stmt: EXIT name_opt when_opt ';'  */
#line 851 "grammar83.y"
                                       {
	(yyval.stmt) = create_stmt(STMT_EXIT, (yyloc));
	// TODO: name_opt
	(yyval.stmt)->u.exit.condition = (yyvsp[-1].expr);
}
#line 3242 "grammar83.tab.c"
    break;

  case 280: /* when_opt: %empty  */
#line 861 "grammar83.y"
                     { (yyval.expr) = NULL; }
#line 3248 "grammar83.tab.c"
    break;

  case 281: /* when_opt: WHEN condition  */
#line 862 "grammar83.y"
                         { (yyval.expr) = (yyvsp[0].expr); }
#line 3254 "grammar83.tab.c"
    break;

  case 282: /* return_stmt: RETURN ';'  */
#line 865 "grammar83.y"
                            { (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc)); }
#line 3260 "grammar83.tab.c"
    break;

  case 283: /* return_stmt: RETURN expression ';'  */
#line 866 "grammar83.y"
                                {
	(yyval.stmt) = create_stmt(STMT_RETURN, (yyloc));
	(yyval.stmt)->u.return_.expr = (yyvsp[-1].expr);
}
#line 3269 "grammar83.tab.c"
    break;

  case 284: /* goto_stmt: GOTO name ';'  */
#line 872 "grammar83.y"
                          {
	(yyval.stmt) = create_stmt(STMT_GOTO, (yyloc));
}
#line 3277 "grammar83.tab.c"
    break;

  case 299: /* mode: %empty  */
#line 904 "grammar83.y"
             { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3283 "grammar83.tab.c"
    break;

  case 300: /* mode: IN  */
#line 905 "grammar83.y"
                 { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3289 "grammar83.tab.c"
    break;

  case 301: /* mode: OUT  */
#line 906 "grammar83.y"
                 { (yyval.param_mode) = PARAM_MODE_OUT; }
#line 3295 "grammar83.tab.c"
    break;

  case 302: /* mode: IN OUT  */
#line 907 "grammar83.y"
                 { (yyval.param_mode) = PARAM_MODE_IN_OUT; }
#line 3301 "grammar83.tab.c"
    break;


#line 3305 "grammar83.tab.c"

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

#line 1119 "grammar83.y"


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
	if(context->curr_scope_idx + 1 >= cnt_of_array(context->scope_stack)) {
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
		// TODO: resize table when it hits threshold
		// (note that some collisions are expected since many decls
		// will have same name, so those collisions shouldn't count
		// towards resizing threshold)
		DeclList* bucket = find_bucket(context, name);
		// Prepend new declaration to the bucket's list
		decl->next_in_bucket = bucket->first;
		bucket->first = decl;
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
	DeclList* bucket = find_bucket(context, name);
	for(Declaration* decl = bucket->first; decl; decl = decl->next_in_bucket) {
		if(decl->kind == DECL_TYPE && get_decl_name(decl) == name) {
			return (TypeDecl*)decl;
		}
	}
	return NULL;
}

static
Declaration* find_first_overload(ParseContext* context, StringToken name)
{
	DeclList* bucket = find_bucket(context, name);
	for(Declaration* decl = bucket->first; decl; decl = decl->next_in_bucket) {
		if(get_decl_name(decl) == name) {
			return decl;
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
DeclList* find_bucket(ParseContext* context, StringToken name)
{
	uint32_t hash = hash_fnv(name);
	uint32_t idx = hash % context->symbol_table_capacity;
	return context->symbol_table + idx;
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
