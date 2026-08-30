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
  YYSYMBOL_216_1 = 216,                    /* $@1  */
  YYSYMBOL_block_body = 217,               /* block_body  */
  YYSYMBOL_handled_stmt_s = 218,           /* handled_stmt_s  */
  YYSYMBOL_except_handler_part_opt = 219,  /* except_handler_part_opt  */
  YYSYMBOL_exit_stmt = 220,                /* exit_stmt  */
  YYSYMBOL_name_opt = 221,                 /* name_opt  */
  YYSYMBOL_when_opt = 222,                 /* when_opt  */
  YYSYMBOL_return_stmt = 223,              /* return_stmt  */
  YYSYMBOL_goto_stmt = 224,                /* goto_stmt  */
  YYSYMBOL_subprog_decl = 225,             /* subprog_decl  */
  YYSYMBOL_subprog_spec = 226,             /* subprog_spec  */
  YYSYMBOL_227_2 = 227,                    /* @2  */
  YYSYMBOL_228_3 = 228,                    /* @3  */
  YYSYMBOL_designator = 229,               /* designator  */
  YYSYMBOL_formal_part_opt = 230,          /* formal_part_opt  */
  YYSYMBOL_formal_part = 231,              /* formal_part  */
  YYSYMBOL_param_s = 232,                  /* param_s  */
  YYSYMBOL_param = 233,                    /* param  */
  YYSYMBOL_mode = 234,                     /* mode  */
  YYSYMBOL_subprog_spec_is_push = 235,     /* subprog_spec_is_push  */
  YYSYMBOL_subprog_body = 236,             /* subprog_body  */
  YYSYMBOL_procedure_call = 237,           /* procedure_call  */
  YYSYMBOL_pkg_decl = 238,                 /* pkg_decl  */
  YYSYMBOL_pkg_spec = 239,                 /* pkg_spec  */
  YYSYMBOL_240_4 = 240,                    /* @4  */
  YYSYMBOL_private_part = 241,             /* private_part  */
  YYSYMBOL_identifier_opt = 242,           /* identifier_opt  */
  YYSYMBOL_pkg_body = 243,                 /* pkg_body  */
  YYSYMBOL_244_5 = 244,                    /* @5  */
  YYSYMBOL_body_opt = 245,                 /* body_opt  */
  YYSYMBOL_private_type = 246,             /* private_type  */
  YYSYMBOL_limited_opt = 247,              /* limited_opt  */
  YYSYMBOL_use_name_s = 248,               /* use_name_s  */
  YYSYMBOL_use_clause = 249,               /* use_clause  */
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
#line 76 "grammar83.y"

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

    static
    Expression* make_binary_expr(Expression* left, BinaryOperator op, Expression* right);

    static
    Expression* make_unary_expr(UnaryOperator op, Expression* right);

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

#line 498 "grammar83.tab.c"

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
#define YYFINAL  34
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1262

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  95
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  189
/* YYNRULES -- Number of rules.  */
#define YYNRULES  403
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
       0,   286,   286,   290,   291,   295,   296,   300,   301,   305,
     306,   310,   311,   312,   313,   314,   315,   316,   317,   318,
     319,   320,   324,   352,   369,   373,   380,   381,   385,   386,
     390,   391,   395,   407,   408,   409,   414,   415,   419,   420,
     421,   422,   423,   424,   425,   426,   430,   447,   451,   455,
     456,   460,   464,   475,   479,   480,   484,   485,   486,   490,
     497,   501,   507,   514,   528,   532,   536,   537,   541,   545,
     546,   550,   551,   555,   559,   563,   567,   568,   572,   576,
     580,   581,   585,   586,   590,   594,   595,   599,   600,   601,
     605,   606,   610,   611,   615,   616,   620,   624,   625,   629,
     630,   634,   635,   639,   643,   644,   648,   652,   656,   662,
     666,   667,   671,   672,   676,   677,   681,   682,   686,   687,
     691,   692,   695,   696,   697,   698,   702,   703,   706,   707,
     711,   712,   716,   720,   721,   722,   723,   730,   731,   732,
     736,   742,   746,   750,   751,   755,   756,   757,   758,   762,
     763,   764,   765,   769,   773,   774,   775,   776,   780,   799,
     800,   804,   805,   806,   807,   808,   812,   813,   817,   821,
     822,   823,   827,   828,   829,   833,   834,   839,   840,   841,
     842,   849,   850,   851,   852,   853,   854,   858,   859,   863,
     864,   865,   869,   870,   874,   875,   876,   880,   881,   885,
     886,   887,   888,   892,   893,   894,   895,   899,   900,   904,
     905,   906,   910,   911,   915,   929,   930,   934,   938,   944,
     945,   966,   967,   968,   972,   973,   974,   975,   976,   977,
     978,   979,   980,   984,   985,   986,   987,   991,   995,  1004,
    1015,  1016,  1022,  1029,  1033,  1037,  1038,  1042,  1049,  1055,
    1056,  1062,  1071,  1075,  1076,  1080,  1089,  1095,  1105,  1113,
    1114,  1118,  1122,  1123,  1128,  1139,  1140,  1140,  1149,  1154,
    1158,  1159,  1163,  1170,  1171,  1175,  1176,  1180,  1181,  1187,
    1208,  1212,  1217,  1217,  1224,  1224,  1231,  1235,  1236,  1240,
    1241,  1245,  1249,  1250,  1254,  1255,  1259,  1260,  1261,  1262,
    1266,  1272,  1281,  1289,  1290,  1294,  1294,  1315,  1316,  1320,
    1321,  1325,  1325,  1346,  1347,  1351,  1355,  1356,  1361,  1365,
    1366,  1370,  1374,  1405,  1424,  1425,  1429,  1430,  1431,  1432,
    1436,  1440,  1441,  1445,  1446,  1447,  1451,  1462,  1463,  1467,
    1471,  1475,  1479,  1483,  1484,  1485,  1489,  1493,  1494,  1498,
    1499,  1503,  1507,  1508,  1512,  1513,  1517,  1518,  1522,  1523,
    1527,  1531,  1532,  1536,  1537,  1541,  1542,  1543,  1544,  1545,
    1546,  1547,  1551,  1552,  1553,  1557,  1558,  1559,  1563,  1564,
    1565,  1566,  1567,  1568,  1569,  1570,  1571,  1572,  1576,  1577,
    1581,  1585,  1589,  1593,  1594,  1595,  1599,  1603,  1607,  1608,
    1612,  1613,  1617,  1621
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
  "id_opt", "block", "block_decl", "$@1", "block_body", "handled_stmt_s",
  "except_handler_part_opt", "exit_stmt", "name_opt", "when_opt",
  "return_stmt", "goto_stmt", "subprog_decl", "subprog_spec", "@2", "@3",
  "designator", "formal_part_opt", "formal_part", "param_s", "param",
  "mode", "subprog_spec_is_push", "subprog_body", "procedure_call",
  "pkg_decl", "pkg_spec", "@4", "private_part", "identifier_opt",
  "pkg_body", "@5", "body_opt", "private_type", "limited_opt",
  "use_name_s", "use_clause", "rename_decl", "rename_unit", "renames",
  "comp_unit", "context_spec", "with_clause", "use_clause_opt", "unit",
  "subunit", "subunit_body", "body_stub", "exception_decl",
  "except_handler_part", "exception_handler", "except_choice_s",
  "except_choice", "raise_stmt", "generic_decl", "generic_formal_part",
  "generic_formal", "generic_discrim_part_opt", "subp_default",
  "generic_type_def", "generic_derived_type", "generic_subp_inst",
  "generic_pkg_inst", "generic_inst", "rep_spec", "attrib_def",
  "record_type_spec", "align_opt", "comp_loc_s", "address_spec",
  "code_stmt", YY_NULLPTR
  };
  return yy_sname[yysymbol];
}
#endif

#define YYPACT_NINF (-582)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-356)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     656,    12,  -582,   101,   122,    14,   182,   280,  -582,   264,
    1171,  -582,  -582,   233,  -582,  -582,  -582,   594,  -582,  -582,
    -582,  -582,   484,   241,   248,  -582,  -582,     6,   250,    39,
    -582,   283,  -582,    -1,  -582,   351,   537,  -582,   305,   342,
     360,   125,   369,   384,   390,   545,  -582,  -582,  -582,  -582,
     580,  -582,  -582,   472,  -582,   218,  -582,  -582,  -582,   296,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,   426,   468,  -582,
     447,   462,   366,   589,   136,   445,   463,  -582,  -582,  -582,
    -582,   470,   523,   351,   481,   470,   495,  -582,   503,   537,
    -582,  -582,  -582,   507,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,   457,   514,   632,   552,   519,     1,   507,   327,   449,
     340,  1065,   571,  -582,   246,   426,   468,  -582,  -582,   301,
     546,    12,   532,   556,   142,  -582,   549,  -582,  -582,    71,
     574,  -582,  -582,  1080,  -582,  -582,  -582,   454,  -582,   507,
     520,   180,   128,   672,   180,   563,   601,  -582,   727,   537,
      67,   604,  -582,  -582,   655,   641,   252,    44,   581,   747,
     537,   590,   747,   588,   537,   690,   593,  1065,  -582,    30,
     609,   846,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,   274,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,    12,   615,  -582,   618,    69,  -582,   665,   470,   668,
     470,   670,  -582,   360,  -582,   103,  -582,   537,  1123,   165,
     660,  1149,  -582,   374,   748,   742,  -582,  -582,  -582,  -582,
     919,   537,   919,  -582,  -582,  -582,  -582,   528,  -582,  -582,
    -582,    41,  -582,   651,    11,  -582,   659,  -582,  -582,  -582,
    -582,   219,  -582,   494,  1182,   364,  -582,   784,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
     747,   778,   553,   202,   635,  -582,  -582,   734,   788,   661,
    -582,   308,   726,   348,  -582,   725,   662,   148,  -582,   386,
     729,   467,   476,   747,  -582,   730,   731,   801,   754,  -582,
    -582,  -582,  -582,   185,   507,   744,   736,   308,   722,  -582,
    1065,   749,  -582,   740,  -582,   226,  -582,  -582,   747,  -582,
     259,  -582,   750,  -582,  -582,   750,   468,  -582,   751,  1065,
     747,    12,   764,  -582,   472,   752,  -582,  -582,  -582,   755,
     829,   768,   785,   793,  -582,    46,    71,  -582,   507,   472,
     760,  1180,   812,  -582,   246,  -582,  -582,   553,  -582,  -582,
     794,   770,   675,   769,   135,   747,   709,   747,   235,  -582,
    -582,   520,   789,   830,  -582,   747,   747,   747,  -582,  -582,
    -582,  -582,   820,  -582,  -582,  -582,  -582,  -582,  -582,   747,
     747,  1182,   364,  -582,  -582,  -582,  -582,  1182,   919,   317,
     817,  -582,  -582,   782,   747,   783,   727,  -582,   747,  -582,
    -582,  -582,  -582,   850,    74,  -582,   313,   747,   747,  -582,
     747,   537,   663,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,   522,  -582,   337,  -582,   747,
     821,   747,   795,   798,  -582,   747,   802,  -582,  1065,   747,
     860,   811,  -582,  -582,  -582,   367,  -582,   375,  -582,  -582,
     181,  1171,   852,  1002,   854,   815,  -582,   747,   869,  -582,
    -582,   896,   898,   899,   537,   901,   910,  -582,  -582,  -582,
     862,   834,  -582,   537,   537,   111,   839,  -582,  -582,  -582,
     889,   883,  -582,   856,   844,   520,  -582,   520,  -582,   624,
    -582,   308,  -582,  -582,   308,  -582,   569,    45,   847,  -582,
    -582,  -582,  -582,  -582,   591,  -582,   591,  -582,   420,   364,
    -582,  -582,  -582,   747,   149,  -582,   308,  -582,  -582,   240,
    -582,   360,  -582,   537,  -582,    34,   240,   308,  -582,  -582,
    -582,   679,  -582,   876,  -582,  -582,  -582,  -582,  -582,   693,
    -582,   695,  -582,   892,   537,   308,  -582,  -582,  -582,  -582,
    1030,  -582,   897,  -582,  -582,   853,   507,    42,  -582,   916,
     709,  -582,  -582,  -582,   911,  -582,  -582,   854,  1081,    12,
     912,  -582,  -582,   868,  -582,   873,  -582,    19,   572,  -582,
     507,  -582,   856,   734,  -582,  -582,  -582,  -582,  -582,   884,
     697,   747,   383,   900,   155,  -582,  -582,    46,  -582,   747,
    -582,  -582,  -582,   663,  -582,    56,   913,   537,  -582,   747,
     393,  -582,  -582,  -582,   877,   412,  1065,   412,   879,    53,
    -582,  -582,   880,   958,   914,  -582,   887,  -582,   302,   891,
     890,  -582,   107,  -582,   902,   747,  -582,   240,  -582,   903,
     908,   923,   705,   945,  -582,  -582,  -582,   537,  -582,   363,
    -582,  -582,  -582,    54,   874,  -582,  -582,  1065,  -582,  -582,
    -582,  -582,   917,  -582,  -582,  -582,  -582,   461,  -582,  -582,
     937,  -582,   537,   932,   168,  -582,   468,  -582,   987,  1065,
     909,   927,   747,  -582,   468,   801,  -582,  -582,  -582,   967,
    -582,   928,   466,   929,   468,  -582,   709,   232,  -582,  -582,
      55,   971,  -582,  -582,   931,    56,  -582,  -582
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     0,   363,     0,     0,     0,     0,     0,   341,     0,
       0,   342,   339,     0,   340,   345,     2,     0,   337,     9,
     343,   344,     0,     0,     0,   287,   288,   286,     0,     0,
     282,     0,    24,     0,     1,   300,     0,   280,     0,     0,
       0,     0,     0,     0,     0,     0,   125,   122,    11,    12,
       0,    13,    14,     0,   129,     0,   126,   128,    15,     0,
     130,    16,   131,   123,    18,   325,    20,    17,    19,   124,
     393,   394,   395,   303,   335,   337,     9,   333,   332,   295,
       0,     0,     0,     0,     0,     0,     0,   371,   364,   281,
     304,   289,     0,   305,     0,   289,     0,   336,     0,     0,
     390,   132,   141,   330,   136,   133,   134,   135,   327,    21,
     137,     0,     0,     0,     0,    33,   132,     0,   134,     0,
      26,     0,     0,   127,   300,   334,   331,   338,    10,     0,
     372,     0,     0,     0,   296,   361,     0,   365,   362,     0,
       0,   290,   311,     0,   391,   326,   283,     0,    25,   392,
       0,     0,     0,     0,     0,     0,     0,     3,     0,     0,
       0,    36,    34,   322,     0,    27,     0,     0,     0,     0,
     273,     0,     0,     0,   273,     0,   132,     0,   223,     0,
       0,     0,   217,   219,   221,   222,   224,   225,   233,   234,
       9,   235,   265,   236,   268,   226,   227,   228,   229,   230,
     231,   262,     0,   305,     0,     0,   373,     0,   289,     0,
     289,   297,   298,     0,   329,     0,   292,     0,     0,     0,
     307,     0,   120,     0,     0,     0,   347,   348,   346,   148,
       0,     0,     0,   160,   111,   140,   158,     0,   192,   193,
     113,     0,   107,   110,   208,   159,     0,   143,   207,   212,
     146,   109,   169,   177,     0,   189,   197,   203,   211,   210,
     209,   157,   156,   155,   154,   153,   152,   149,   150,   151,
       0,   398,   208,     0,   177,   138,   139,   311,   132,     0,
       5,     7,     0,    48,   100,     0,     0,     0,    97,   316,
       0,   132,   134,     0,   351,     0,     0,    30,    28,    29,
      71,    72,   232,     0,   274,   275,     0,   244,   245,   240,
       0,     0,   237,     0,   277,     0,   254,   220,     0,   302,
       0,   403,     0,   218,   269,   271,   249,   266,     0,     0,
       0,   262,   259,   255,     0,     0,   263,   350,   328,     0,
     316,     0,     0,   375,   299,    30,     0,   291,   285,   313,
       0,     0,     0,   121,     0,   300,   205,   215,   216,   204,
     160,     0,     0,   146,   109,     0,     0,     0,     0,   112,
     142,     0,   172,   173,   174,     0,     0,     0,   184,   182,
     186,   187,     0,   181,   183,   185,   194,   195,   196,     0,
       0,     0,   190,   201,   202,   199,   200,     0,     0,     0,
       0,   400,   396,     0,     0,     0,     0,    46,     0,    47,
      50,    49,    35,   101,     0,    96,     0,     0,     0,   317,
       0,     0,     0,    37,    44,    64,    38,    39,    40,    66,
      67,    41,    42,    43,    45,     0,    32,     0,   324,     0,
       0,     0,     0,     0,   248,     0,     0,   279,     0,     0,
       0,     0,   243,   360,   278,     0,   214,     0,   352,   353,
       0,     0,     0,     0,     0,     0,   260,     0,     0,   301,
     374,     0,     0,     0,     0,     0,     0,   384,   385,   386,
       0,     0,   387,     0,     0,     0,     0,   294,   293,   314,
       0,     0,   308,   309,     0,     0,   162,     0,   161,     0,
     213,   168,   108,   110,   109,    53,   208,     0,    57,   144,
     175,   176,   170,   171,    56,   188,   178,   179,   180,   191,
     198,   206,   402,     0,     0,   349,     8,     4,     6,    54,
     102,     0,    98,     0,   114,     0,    54,    65,    52,    63,
      62,     0,    60,     0,   315,     9,    84,    23,    83,     0,
      76,     0,    80,   208,     0,    31,    22,   323,   276,   272,
       0,   241,     0,   238,   359,   132,   358,     0,   356,     0,
       0,   250,   267,   258,     0,   256,   252,     0,   208,   262,
     381,   383,   380,   388,   379,     0,   366,   375,     0,   377,
     376,   367,   309,     0,   310,   306,   165,   166,   167,   160,
       0,     0,     0,     0,     0,    55,    51,    30,   115,     0,
      69,    68,    59,     0,    86,     0,     0,     0,    79,     0,
       0,    82,    75,    74,     0,     0,     0,     0,     0,     0,
     261,   257,     0,     0,     0,   378,     0,   370,     0,     0,
       0,   163,     0,   399,     0,     0,    99,    54,    61,     0,
       0,     0,     0,     0,     9,    90,     9,     0,    77,     0,
      81,    78,   239,     0,     0,   357,   247,     0,   264,   382,
     389,   368,     0,   312,   164,    58,   397,     0,    70,    95,
       0,     9,     0,     0,     0,    87,    88,    73,     0,     0,
       0,     0,     0,     9,    89,    30,    85,    91,     9,     0,
     369,     0,     0,     0,    93,   401,     0,     0,   104,    94,
       0,     0,   105,     9,     0,     0,   103,   106
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -582,  -582,    -9,  -582,   605,   -66,  -582,  -582,  -582,    -6,
    -582,  -582,  -328,  -582,  -582,  -582,  -582,  -582,  -153,  -582,
    -582,  -582,  -240,  -482,  -354,  -582,  -582,   399,  -582,  -582,
    -582,  -582,   -93,  -582,  -582,  -581,  -582,   397,  -582,  -582,
    -447,  -582,  -582,   300,  -582,  -582,   332,   888,  -582,   603,
    -582,   336,  -582,   319,  -529,   658,  -357,   688,  -197,   678,
    -582,  -121,  -582,   975,  -582,    -3,  -194,   881,   882,  -582,
     542,  -219,   -11,  -582,   893,  -582,  -582,  -582,   809,   -47,
    -582,  -582,   419,  -582,  -582,  -145,  -582,  -582,  -228,  -582,
     653,  -205,  -264,  -119,  -582,  -298,  -174,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,   606,  -582,  -277,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -429,  -320,  -582,
    -582,  -582,  -260,  -582,  -582,  -582,   878,  -582,  -582,  -582,
     455,    15,  -582,  -582,    29,   -51,  -582,  -582,  -115,  -582,
    -582,    23,  -582,   486,  1034,  -582,  -582,   469,    28,  -582,
    -582,   718,   719,  -582,    16,  -582,   498,     0,  -582,  -582,
    1048,   994,  1053,  -582,  -582,  -582,  -582,  -582,   746,   450,
     452,  -582,   527,  -582,  -582,  -582,   487,  -582,  -582,  -582,
    -582,   990,  -582,  -582,  -582,  -582,  -582,  -582,  -582
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     7,   178,   279,   280,    78,    47,    48,    49,    50,
     167,   297,   442,    51,   161,   290,   423,    52,   622,   409,
     410,   424,   605,   621,   240,   426,   541,   542,   427,   428,
     429,   430,   299,   300,   301,   623,   549,   550,   440,   551,
     552,   432,   546,   653,   654,   685,   655,   162,   287,   288,
     531,   656,   707,   708,   241,   242,   243,   433,    53,   220,
     221,    54,    55,    56,    57,   272,   111,   245,   104,   105,
     246,   247,   106,   107,   265,   248,   249,   362,   250,   251,
     375,   376,   252,   389,   390,   274,   254,   391,   255,   397,
     256,   257,   258,   259,   260,   181,   182,   183,   184,   185,
     186,   187,   188,   308,   309,   310,   311,   450,   189,   190,
     460,   571,   191,   192,   331,   332,   467,   333,   335,   193,
     334,   461,   122,   194,   324,   195,   305,   446,   196,   197,
      58,    59,    95,    91,   336,   140,   141,   215,    85,   213,
      10,    60,   198,    61,    13,   143,   352,   595,    62,   218,
     490,   434,   435,   119,    63,    64,    65,    38,    16,    17,
      18,    77,    19,    20,   228,    66,    67,   325,   458,   567,
     568,   199,    68,    22,    88,   207,   486,   481,   482,    23,
      24,   100,    69,    70,    71,   401,   524,    72,   200
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      33,    46,   180,   317,   369,   253,   282,   323,    74,   503,
     126,   465,   451,   505,   298,     9,    83,   487,   361,   345,
     577,   349,   222,    11,   216,   356,   392,   359,    14,    94,
      27,   463,     9,   103,   118,   575,   517,    84,    87,   318,
      11,   629,   117,   411,   146,    14,    46,   606,   377,   425,
     372,   365,   626,   464,   611,   441,   456,   649,   180,   485,
     609,   296,   180,   667,   689,   713,  -284,   367,   284,   128,
     284,   285,    79,   339,   468,   284,   687,   650,    97,    93,
    -318,    98,   373,  -318,   136,   548,  -284,    25,    26,   489,
     367,   150,   253,   127,    31,   368,   149,   152,    36,   150,
     353,   695,   651,   151,   456,   152,   273,   374,    42,   319,
     150,   281,   358,   548,   320,   589,   152,   128,   179,   101,
     102,    28,   303,   372,   326,   307,   366,   627,   315,   204,
     154,    32,   155,    83,    46,   386,   387,   388,   366,   627,
     366,   127,    32,   266,    32,   112,    32,   244,   631,    32,
     560,   372,   509,   292,   286,   373,   283,   341,   223,   343,
     208,   117,   225,   519,   283,   678,   295,   304,   558,   649,
     226,   304,   660,   645,   179,   227,    29,   710,   179,   603,
     374,   211,   346,   373,   347,   350,   101,   102,   675,   650,
     364,   180,   212,   521,   261,    36,   431,    30,   -92,   286,
      29,   372,   235,   267,   102,   262,   263,   499,   374,    46,
     180,   569,    46,   503,   348,   135,   500,  -145,   372,    39,
      42,   253,   507,   399,   110,   444,   253,   414,   357,   415,
     222,   488,   514,   373,   244,   372,   223,  -117,   -92,   154,
      29,   155,   372,    32,   516,   507,   437,   477,  -117,   261,
     373,   570,    40,     1,     2,   264,   372,    32,   374,   632,
     262,   263,   711,   534,   572,   548,   505,   373,   538,    41,
      42,   455,     4,   261,   373,   374,   597,   323,   598,   646,
      34,   402,    43,   307,   262,   263,    44,    45,   373,   323,
      99,   508,   374,    32,   507,   610,   367,   327,   443,   374,
    -145,  -145,   706,   229,    35,   454,   672,   179,   328,   202,
     264,    36,    73,   374,   230,   237,   329,   128,   501,   504,
      89,   583,   507,    36,   372,    92,   179,    90,   664,   180,
     604,   294,   180,   372,   264,   533,   124,   607,   701,   237,
      83,   203,    46,    37,   180,   330,   231,   232,   233,   503,
     253,   234,   253,   372,   253,    36,   373,   526,    96,   281,
      36,   529,   165,   244,   506,   373,   223,   703,   244,   690,
     535,   536,   166,   537,   408,    37,   235,   101,   102,   236,
     608,   374,   237,   372,   108,   373,   323,   518,   101,   102,
     374,   699,   238,   239,   555,    99,   522,   661,   307,   372,
     416,   131,   307,   296,   367,   230,  -319,   393,   286,  -319,
     374,   417,   418,   283,   354,   373,   547,   132,   283,   688,
     133,   109,   394,  -208,   564,   253,  -208,   419,   150,   420,
     421,   373,   151,    36,   152,   110,   553,   231,   232,   233,
     374,   180,   367,   150,   113,   179,   563,   151,   179,   152,
     565,   102,    46,    37,   566,     8,   374,   395,   396,   114,
     179,   564,   643,  -208,   578,   115,   422,   235,   101,   102,
     236,   283,     8,   237,   507,   507,   602,   372,  -208,   615,
     587,   588,   590,   238,   239,    79,    12,   101,   102,     1,
     323,   121,   244,   253,   244,    45,   244,   377,    15,   378,
     150,   379,   380,    12,   368,   224,   152,   180,     4,   373,
    -208,  -208,  -208,  -208,  -208,    15,   323,   692,    42,     1,
      42,   229,   129,   504,   137,   323,   153,    21,   163,   229,
     283,   164,   230,   381,   374,    80,   706,   130,     4,   382,
     230,   154,   138,   155,    21,   180,  -320,   507,   180,  -320,
     139,   283,    81,    45,   642,  -321,    82,   179,  -321,    32,
     145,   253,   647,   142,   231,   232,   233,   244,   543,   234,
     180,   180,   231,   232,   360,   544,   147,   234,   148,   545,
     180,   383,   384,   385,   386,   387,   388,   150,   684,   156,
     686,   151,   159,   152,   235,   101,   102,   236,   677,   160,
     237,   201,   235,   101,   102,   236,   128,   209,   237,   652,
     238,   239,   101,   102,   659,   694,   578,   506,   238,   239,
     116,   102,   566,   179,   566,   229,   205,   702,   214,     1,
       2,   210,   704,   150,   217,   244,   230,   320,   276,   152,
     378,   277,   379,   380,   289,     3,    42,   715,     4,   150,
     293,   637,   638,   368,   283,   152,   151,     5,   152,   504,
     302,   179,    98,   120,   179,   306,     6,   312,   231,   232,
     599,    98,   134,   234,   381,   128,   316,   128,   652,   283,
     382,   386,   387,   388,   230,   128,   179,   179,   321,   506,
     270,     1,     2,   128,   337,   128,   179,   338,   235,   101,
     102,   236,   230,   244,   237,   340,   128,     3,   342,   652,
       4,   157,   158,   351,   238,   239,   231,   232,   233,     5,
     344,   230,   383,   384,   385,   386,   387,   388,     6,   271,
     291,   102,  -147,  -147,   231,   232,   233,   539,   540,   230,
     370,   371,   405,   406,    98,   413,   235,   101,   102,   236,
     448,   449,   237,   231,   232,   233,   496,   497,   234,   230,
     612,   613,   238,   239,   235,   101,   102,   236,    28,   314,
     237,   231,   232,   233,   616,   617,   618,   619,   641,   371,
     238,   239,   355,   235,   101,   102,   236,    98,   682,   237,
     398,   231,   232,   233,   512,   513,   400,   403,   404,   238,
     239,   235,   278,   102,   236,   407,   412,   237,   436,   438,
     441,   439,   168,    36,   445,   447,   452,   238,   239,   453,
     457,   235,   101,   102,   236,   466,   462,   237,   483,   484,
    -253,   469,   169,   485,  -253,   491,   470,   238,   239,  -242,
    -242,  -242,   493,   416,   170,  -253,   296,   168,   171,   172,
     498,   494,   495,  -253,   471,   472,   510,   173,   511,   515,
     523,   525,   527,    42,   530,  -253,   174,   169,   554,  -253,
     419,   175,   473,   474,   556,   168,  -270,   557,   322,   170,
    -253,   559,  -253,   171,   172,   475,   176,   102,  -253,   177,
     562,   573,   173,  -253,   576,   169,   329,  -253,    42,   579,
     580,   174,   581,   582,  -354,   584,   175,   170,  -253,   476,
     168,   171,   172,   586,   585,   544,  -253,  -253,   591,   592,
     173,   176,   102,   593,   177,   596,    42,   601,  -253,   174,
     169,   594,  -253,   614,   175,   624,   625,   628,   633,  -251,
     634,   640,   170,  -253,  -354,  -253,   171,   172,   620,   176,
     102,  -253,   177,   630,   635,   173,   662,   644,   666,   668,
     657,    42,   669,   231,   174,   233,   671,   670,   168,   175,
     673,   674,   150,   -54,   -54,   683,   368,   693,   152,  -251,
    -253,   676,   679,   680,   176,   102,  -253,   177,   169,   696,
    -253,   661,   714,   235,   101,   102,   236,  -355,   691,   237,
     170,  -253,   681,   168,   171,   172,   700,   705,   709,  -253,
     716,   528,   648,   173,   658,   717,   697,   532,   206,    42,
     698,  -253,   174,   169,   502,  -253,   712,   175,   478,   492,
     123,   168,   574,   268,   269,   170,  -253,  -355,  -253,   171,
     172,   600,   176,   102,  -253,   177,   363,   275,   173,  -253,
     520,   169,   313,  -253,    42,   561,    86,   174,   479,   480,
    -246,   639,   175,   170,  -253,    75,   168,   171,   172,   125,
      76,   459,  -253,  -253,   636,   663,   173,   176,   102,   665,
     177,    39,    42,   144,  -253,   174,   169,     0,  -253,     0,
     175,     0,     0,     0,     0,     0,     0,     0,   170,  -253,
       0,  -253,   171,   172,     0,   176,   102,  -253,   177,     0,
    -118,   173,     0,     0,    40,     1,     2,    42,     0,     0,
     174,     0,     0,   -54,    39,   175,     0,     0,     0,     0,
       0,   219,    42,  -118,     4,     0,  -253,   367,     0,     0,
     176,   102,  -116,   177,    43,     0,     0,     0,    44,    45,
      39,     0,     0,  -116,     0,    32,     0,    40,     1,     2,
       0,   150,   -54,   -54,     0,   368,     0,   152,     0,     0,
       0,     0,    39,     0,    41,    42,     0,     4,     0,  -119,
       0,    39,     0,    40,     1,     2,     0,    43,     0,     0,
    -116,    44,    45,     0,   230,     0,     0,     0,    32,     0,
     219,    42,  -119,     4,     0,    40,     1,     2,     0,     0,
    -118,     0,     0,    43,    40,     1,     2,    44,    45,     0,
       0,     0,    41,    42,    32,     4,   231,   232,   233,     0,
       0,   219,    42,     0,     4,    43,     0,     0,     0,    44,
      45,     0,     0,     0,    43,     0,    32,     0,    44,    45,
       0,     0,     0,     0,     0,    32,   235,   101,   102,   236,
       0,     0,   237
};

static const yytype_int16 yycheck[] =
{
       6,    10,   121,   177,   244,   150,   159,   181,    17,   366,
      76,   331,   310,   367,   167,     0,    22,   345,   237,   213,
     467,   218,   143,     0,   139,   230,   254,   232,     0,    29,
       1,   329,    17,    36,    45,   464,   390,    22,    22,     9,
      17,   570,    45,   283,    95,    17,    55,   529,     3,   289,
      16,    10,    10,   330,   536,     9,   320,     1,   177,    40,
      26,    17,   181,    10,    10,    10,    60,    56,     1,    78,
       1,     4,     1,     4,   334,     1,   657,    21,    79,    40,
      79,    82,    48,    82,    84,   439,    80,    75,    76,   349,
      56,    80,   237,    77,    80,    84,    99,    86,    59,    80,
     221,   682,    46,    84,   368,    86,   153,    73,    52,    79,
      80,   158,   231,   467,    84,     4,    86,   126,   121,    75,
      76,    20,   169,    16,   190,   172,    85,    85,   175,   129,
      84,    75,    86,   139,   143,    90,    91,    92,    85,    85,
      85,   125,    75,    15,    75,    20,    75,   150,   577,    75,
     448,    16,   371,   164,   160,    48,   159,   208,   143,   210,
     131,   164,   147,   391,   167,   647,   166,   170,   445,     1,
     147,   174,   619,    18,   177,   147,    75,   706,   181,    30,
      73,    39,    79,    48,    81,    20,    75,    76,    81,    21,
     237,   310,    50,   398,    14,    59,   289,    75,    30,   205,
      75,    16,    74,    75,    76,    25,    26,    72,    73,   218,
     329,    30,   221,   570,   217,    79,    81,    82,    16,     1,
      52,   366,   367,   270,    75,    40,   371,    79,   231,    81,
     351,   346,   377,    48,   237,    16,   221,    19,    70,    84,
      75,    86,    16,    75,   389,   390,   293,   340,    30,    14,
      48,    70,    34,    35,    36,    75,    16,    75,    73,   579,
      25,    26,    30,   416,   461,   619,   620,    48,   421,    51,
      52,   318,    54,    14,    48,    73,   495,   451,   497,   607,
       0,    79,    64,   330,    25,    26,    68,    69,    48,   463,
      44,    56,    73,    75,   439,   535,    56,    23,   298,    73,
      81,    82,    70,     1,    40,    79,     4,   310,    34,    63,
      75,    59,    79,    73,    12,    80,    42,   326,   365,   366,
      79,   474,   467,    59,    16,    75,   329,    79,   626,   448,
     524,    79,   451,    16,    75,    22,    40,   531,   692,    80,
     346,    40,   351,    79,   463,    71,    44,    45,    46,   706,
     495,    49,   497,    16,   499,    59,    48,   404,    75,   406,
      59,   408,    22,   366,   367,    48,   351,   695,   371,   667,
     417,   418,    32,   420,    26,    79,    74,    75,    76,    77,
     533,    73,    80,    16,    79,    48,   560,   390,    75,    76,
      73,   689,    90,    91,   441,    44,    79,     4,   445,    16,
      14,    35,   449,    17,    56,    12,    79,    43,   414,    82,
      73,    25,    26,   416,    40,    48,    79,    51,   421,    56,
      54,    79,    58,     3,    49,   570,     6,    41,    80,    43,
      44,    48,    84,    59,    86,    75,   439,    44,    45,    46,
      73,   560,    56,    80,    75,   448,    79,    84,   451,    86,
      75,    76,   461,    79,   457,     0,    73,    93,    94,    75,
     463,    49,    79,    43,   467,    75,    80,    74,    75,    76,
      77,   474,    17,    80,   619,   620,   523,    16,    58,   545,
     483,   484,   485,    90,    91,     1,     0,    75,    76,    35,
     664,    19,   495,   638,   497,    69,   499,     3,     0,     5,
      80,     7,     8,    17,    84,    51,    86,   626,    54,    48,
      90,    91,    92,    93,    94,    17,   690,    56,    52,    35,
      52,     1,    75,   570,    79,   699,    69,     0,    79,     1,
     533,    82,    12,    39,    73,    51,    70,    75,    54,    45,
      12,    84,    79,    86,    17,   664,    79,   692,   667,    82,
      80,   554,    68,    69,   601,    79,    72,   560,    82,    75,
      79,   706,   609,    40,    44,    45,    46,   570,    46,    49,
     689,   690,    44,    45,    46,    53,    81,    49,    75,    57,
     699,    87,    88,    89,    90,    91,    92,    80,   654,    75,
     656,    84,    40,    86,    74,    75,    76,    77,   645,    80,
      80,    30,    74,    75,    76,    77,   615,    75,    80,   615,
      90,    91,    75,    76,   617,   681,   619,   620,    90,    91,
      75,    76,   625,   626,   627,     1,    80,   693,    79,    35,
      36,    75,   698,    80,    60,   638,    12,    84,    75,    86,
       5,    40,     7,     8,    40,    51,    52,   713,    54,    80,
       9,    79,    80,    84,   657,    86,    84,    63,    86,   706,
      79,   664,    82,    83,   667,    75,    72,    79,    44,    45,
      46,    82,    83,    49,    39,   684,    83,   686,   684,   682,
      45,    90,    91,    92,    12,   694,   689,   690,    79,   692,
      18,    35,    36,   702,    79,   704,   699,    79,    74,    75,
      76,    77,    12,   706,    80,    40,   715,    51,    40,   715,
      54,    79,    80,    53,    90,    91,    44,    45,    46,    63,
      50,    12,    87,    88,    89,    90,    91,    92,    72,    57,
      75,    76,    81,    82,    44,    45,    46,    74,    75,    12,
      81,    82,    81,    82,    82,    83,    74,    75,    76,    77,
      28,    29,    80,    44,    45,    46,    81,    82,    49,    12,
      81,    82,    90,    91,    74,    75,    76,    77,    20,    79,
      80,    44,    45,    46,    81,    82,    81,    82,    81,    82,
      90,    91,    40,    74,    75,    76,    77,    82,    83,    80,
       6,    44,    45,    46,   375,   376,    18,    63,    10,    90,
      91,    74,    75,    76,    77,    79,    81,    80,    79,    79,
       9,    80,     1,    59,    70,    79,    67,    90,    91,    79,
      70,    74,    75,    76,    77,    61,    75,    80,    60,    44,
      19,    79,    21,    40,    23,    75,    81,    90,    91,    28,
      29,    30,    30,    14,    33,    34,    17,     1,    37,    38,
      81,    57,    82,    42,    25,    26,    67,    46,    28,    39,
      43,    79,    79,    52,    14,    19,    55,    21,    47,    23,
      41,    60,    43,    44,    79,     1,    30,    79,    32,    33,
      34,    79,    71,    37,    38,    56,    75,    76,    42,    78,
      30,    39,    46,    19,    79,    21,    42,    23,    52,    30,
       4,    55,     4,     4,    30,     4,    60,    33,    34,    80,
       1,    37,    38,    79,     4,    53,    42,    71,    79,    30,
      46,    75,    76,    40,    78,    81,    52,    80,    19,    55,
      21,    75,    23,    57,    60,    38,    83,    21,    26,    30,
      72,    57,    33,    34,    70,    71,    37,    38,    56,    75,
      76,    42,    78,    42,    81,    46,    79,    57,    79,    79,
      47,    52,     4,    44,    55,    46,    79,    53,     1,    60,
      79,    81,    80,    81,    82,    30,    84,    40,    86,    70,
      71,    79,    79,    75,    75,    76,    19,    78,    21,    57,
      23,     4,    21,    74,    75,    76,    77,    30,    81,    80,
      33,    34,    79,     1,    37,    38,    79,    79,    79,    42,
      79,   406,   613,    46,   617,   715,   684,   414,   130,    52,
     684,    19,    55,    21,   366,    23,   707,    60,   340,   351,
      55,     1,    30,   152,   152,    33,    34,    70,    71,    37,
      38,   499,    75,    76,    42,    78,   237,   154,    46,    19,
     397,    21,   174,    23,    52,   449,    22,    55,   340,   340,
      30,   592,    60,    33,    34,    17,     1,    37,    38,    75,
      17,   325,    42,    71,   587,   625,    46,    75,    76,   627,
      78,     1,    52,    93,    19,    55,    21,    -1,    23,    -1,
      60,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    33,    34,
      -1,    71,    37,    38,    -1,    75,    76,    42,    78,    -1,
      30,    46,    -1,    -1,    34,    35,    36,    52,    -1,    -1,
      55,    -1,    -1,    42,     1,    60,    -1,    -1,    -1,    -1,
      -1,    51,    52,    53,    54,    -1,    71,    56,    -1,    -1,
      75,    76,    19,    78,    64,    -1,    -1,    -1,    68,    69,
       1,    -1,    -1,    30,    -1,    75,    -1,    34,    35,    36,
      -1,    80,    81,    82,    -1,    84,    -1,    86,    -1,    -1,
      -1,    -1,     1,    -1,    51,    52,    -1,    54,    -1,    30,
      -1,     1,    -1,    34,    35,    36,    -1,    64,    -1,    -1,
      19,    68,    69,    -1,    12,    -1,    -1,    -1,    75,    -1,
      51,    52,    53,    54,    -1,    34,    35,    36,    -1,    -1,
      30,    -1,    -1,    64,    34,    35,    36,    68,    69,    -1,
      -1,    -1,    51,    52,    75,    54,    44,    45,    46,    -1,
      -1,    51,    52,    -1,    54,    64,    -1,    -1,    -1,    68,
      69,    -1,    -1,    -1,    64,    -1,    75,    -1,    68,    69,
      -1,    -1,    -1,    -1,    -1,    75,    74,    75,    76,    77,
      -1,    -1,    80
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    35,    36,    51,    54,    63,    72,    96,   225,   226,
     235,   236,   238,   239,   243,   251,   253,   254,   255,   257,
     258,   267,   268,   274,   275,    75,    76,   229,    20,    75,
      75,    80,    75,   104,     0,    40,    59,    79,   252,     1,
      34,    51,    52,    64,    68,    69,    97,   101,   102,   103,
     104,   108,   112,   153,   156,   157,   158,   159,   225,   226,
     236,   238,   243,   249,   250,   251,   260,   261,   267,   277,
     278,   279,   282,    79,    97,   255,   257,   256,   100,     1,
      51,    68,    72,   104,   226,   233,   239,   249,   269,    79,
      79,   228,    75,    40,   252,   227,    75,    79,    82,    44,
     276,    75,    76,   160,   163,   164,   167,   168,    79,    79,
      75,   161,    20,    75,    75,    75,    75,   160,   167,   248,
      83,    19,   217,   158,    40,   256,   100,   249,    97,    75,
      75,    35,    51,    54,    83,    79,   252,    79,    79,    80,
     230,   231,    40,   240,   276,    79,   230,    81,    75,   160,
      80,    84,    86,    69,    84,    86,    75,    79,    80,    40,
      80,   109,   142,    79,    82,    22,    32,   105,     1,    21,
      33,    37,    38,    46,    55,    60,    75,    78,    97,   160,
     188,   190,   191,   192,   193,   194,   195,   196,   197,   203,
     204,   207,   208,   214,   218,   220,   223,   224,   237,   266,
     283,    30,    63,    40,   252,    80,   142,   270,   229,    75,
      75,    39,    50,   234,    79,   232,   233,    60,   244,    51,
     154,   155,   156,   226,    51,   226,   236,   243,   259,     1,
      12,    44,    45,    46,    49,    74,    77,    80,    90,    91,
     119,   149,   150,   151,   160,   162,   165,   166,   170,   171,
     173,   174,   177,   180,   181,   183,   185,   186,   187,   188,
     189,    14,    25,    26,    75,   169,    15,    75,   162,   163,
      18,    57,   160,   174,   180,   169,    75,    40,    75,    98,
      99,   174,   113,   160,     1,     4,   104,   143,   144,    40,
     110,    75,   167,     9,    79,   252,    17,   106,   113,   127,
     128,   129,    79,   174,   160,   221,    75,   174,   198,   199,
     200,   201,    79,   221,    79,   174,    83,   191,     9,    79,
      84,    79,    32,   191,   219,   262,   100,    23,    34,    42,
      71,   209,   210,   212,   215,   213,   229,    79,    79,     4,
      40,   230,    40,   230,    50,   161,    79,    81,   160,   153,
      20,    53,   241,   156,    40,    40,   186,   160,   188,   186,
      46,   166,   172,   173,   174,    10,    85,    56,    84,   117,
      81,    82,    16,    48,    73,   175,   176,     3,     5,     7,
       8,    39,    45,    87,    88,    89,    90,    91,    92,   178,
     179,   182,   183,    43,    58,    93,    94,   184,     6,   174,
      18,   280,    79,    63,    10,    81,    82,    79,    26,   114,
     115,   117,    81,    83,    79,    81,    14,    25,    26,    41,
      43,    44,    80,   111,   116,   117,   120,   123,   124,   125,
     126,   127,   136,   152,   246,   247,    79,   174,    79,    80,
     133,     9,   107,   252,    40,    70,   222,    79,    28,    29,
     202,   190,    67,    79,    79,   174,   187,    70,   263,   263,
     205,   216,    75,   190,   201,   213,    61,   211,   217,    79,
      81,    25,    26,    43,    44,    56,    80,   127,   152,   246,
     247,   272,   273,    60,    44,    40,   271,   107,   233,   217,
     245,    75,   154,    30,    57,    82,    81,    82,    81,    72,
      81,   174,   150,   151,   174,   119,   160,   180,    56,   166,
      67,    28,   177,   177,   180,    39,   180,   119,   160,   183,
     185,   186,    79,    43,   281,    79,   174,    79,    99,   174,
      14,   145,   144,    22,   113,   174,   174,   174,   113,    74,
      75,   121,   122,    46,    53,    57,   137,    79,   119,   131,
     132,   134,   135,   160,    47,   174,    79,    79,   201,    79,
     190,   199,    30,    79,    49,    75,   160,   264,   265,    30,
      70,   206,   153,    39,    30,   212,    79,   135,   160,    30,
       4,     4,     4,   113,     4,     4,    79,   160,   160,     4,
     160,    79,    30,    40,    75,   242,    81,   166,   166,    46,
     165,    80,   174,    30,   161,   117,   118,   161,   113,    26,
     117,   118,    81,    82,    57,   100,    81,    82,    81,    82,
      56,   118,   113,   130,    38,    83,    10,    85,    21,   149,
      42,   212,   213,    26,    72,    81,   271,    79,    80,   242,
      57,    81,   174,    79,    57,    18,   107,   174,   122,     1,
      21,    46,   104,   138,   139,   141,   146,    47,   132,   160,
     135,     4,    79,   264,   190,   265,    79,    10,    79,     4,
      53,    79,     4,    79,    81,    81,    79,   174,   118,    79,
      75,    79,    83,    30,   100,   140,   100,   130,    56,    10,
     190,    81,    56,    40,   100,   130,    57,   141,   146,   190,
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
     211,   212,   213,   213,   214,   215,   216,   215,   217,   218,
     219,   219,   220,   221,   221,   222,   222,   223,   223,   224,
     225,   225,   227,   226,   228,   226,   226,   229,   229,   230,
     230,   231,   232,   232,   233,   233,   234,   234,   234,   234,
     235,   236,   237,   238,   238,   240,   239,   241,   241,   242,
     242,   244,   243,   245,   245,   246,   247,   247,   248,   248,
     248,   248,   249,   250,   250,   250,   251,   251,   251,   251,
     252,   253,   253,   254,   254,   254,   255,   256,   256,   257,
     257,   257,   257,   257,   257,   257,   258,   259,   259,   260,
     260,   261,   262,   262,   263,   263,   264,   264,   265,   265,
     266,   267,   267,   268,   268,   269,   269,   269,   269,   269,
     269,   269,   270,   270,   270,   271,   271,   271,   272,   272,
     272,   272,   272,   272,   272,   272,   272,   272,   273,   273,
     274,   275,   276,   277,   277,   277,   278,   279,   280,   280,
     281,   281,   282,   283
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
       1,     4,     0,     1,     6,     0,     0,     3,     2,     2,
       0,     1,     4,     0,     1,     0,     2,     2,     3,     3,
       2,     2,     0,     4,     0,     6,     2,     1,     1,     0,
       1,     3,     1,     3,     5,     1,     0,     1,     1,     2,
       2,     6,     2,     2,     2,     0,     8,     0,     2,     0,
       1,     0,    10,     0,     1,     2,     0,     1,     1,     1,
       3,     3,     3,     6,     5,     1,     4,     3,     5,     4,
       2,     3,     2,     2,     3,     2,     3,     0,     2,     1,
       1,     1,     1,     1,     1,     1,     5,     1,     1,     6,
       4,     4,     2,     2,     4,     6,     1,     3,     1,     1,
       3,     3,     3,     1,     2,     2,     6,     6,     8,    10,
       7,     1,     0,     1,     3,     0,     2,     2,     3,     2,
       2,     2,     4,     2,     1,     1,     1,     1,     2,     4,
       3,     4,     2,     1,     1,     1,     5,     9,     0,     4,
       0,     7,     6,     2
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
#line 259 "grammar83.y"
{
    yylloc.file_id = context->file_id;
    yylloc.line_num = 1;
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

#line 2514 "grammar83.tab.c"

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
#line 286 "grammar83.y"
                        { context->comp_unit = (yyvsp[0].comp_unit); }
#line 2734 "grammar83.tab.c"
    break;

  case 22: /* object_decl: def_id_s ':' object_qualifier_opt object_subtype_def init_opt ';'  */
#line 324 "grammar83.y"
                                                                      {
        TypeDecl* type_decl = find_type_decl(context, (yyvsp[-2].str_token));
        if(!type_decl) {
            error_print((yyloc), "Unknown type: %s", ST((yyvsp[-2].str_token)));
            error_exit();
        }

        (yyval.decl) = NULL;
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
            push_declaration(context, &decl->base);
            if(!(yyval.decl)) {
                (yyval.decl) = &decl->base;
            }
        }
    }
#line 2765 "grammar83.tab.c"
    break;

  case 23: /* number_decl: def_id_s ':' CONSTANT IS_ASSIGNED expression ';'  */
#line 352 "grammar83.y"
                                                     {
        (yyval.decl) = NULL;
        uint32_t name_count = StringTokenArray_size(&(yyvsp[-5].str_token_array));
        for(uint32_t i = 0; i < name_count; ++i) {
            ObjectDecl* decl = create_object_decl((yyvsp[-5].str_token_array).data[i], (yyloc));
            check_for_redefinition(context, decl->name, (yyloc));
            decl->is_constant = true;
            decl->type = &universal_int_type;
            decl->init_expr = (yyvsp[-1].expr);
            push_declaration(context, &decl->base);
            if(!(yyval.decl)) {
                (yyval.decl) = &decl->base;
            }
        }
    }
#line 2785 "grammar83.tab.c"
    break;

  case 24: /* def_id_s: identifier  */
#line 369 "grammar83.y"
                            {
        StringTokenArray_init(&(yyval.str_token_array));
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2794 "grammar83.tab.c"
    break;

  case 25: /* def_id_s: def_id_s ',' identifier  */
#line 373 "grammar83.y"
                            {
        (yyval.str_token_array) = (yyvsp[-2].str_token_array);
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2803 "grammar83.tab.c"
    break;

  case 26: /* object_qualifier_opt: %empty  */
#line 380 "grammar83.y"
             { (yyval.bool_) = false; }
#line 2809 "grammar83.tab.c"
    break;

  case 27: /* object_qualifier_opt: CONSTANT  */
#line 381 "grammar83.y"
             { (yyval.bool_) = true; }
#line 2815 "grammar83.tab.c"
    break;

  case 30: /* init_opt: %empty  */
#line 390 "grammar83.y"
                           { (yyval.expr) = NULL; }
#line 2821 "grammar83.tab.c"
    break;

  case 31: /* init_opt: IS_ASSIGNED expression  */
#line 391 "grammar83.y"
                           { (yyval.expr) = (yyvsp[0].expr); }
#line 2827 "grammar83.tab.c"
    break;

  case 32: /* type_decl: TYPE identifier discrim_part_opt type_completion ';'  */
#line 395 "grammar83.y"
                                                         {
        // TODO: discriminant
        TypeDecl* decl = (yyvsp[-1].type_decl);
        // Note: decl->base.kind is set by the specific type_completion
        decl->base.loc = (yyloc);
        decl->name = (yyvsp[-3].str_token);
        check_for_redefinition(context, decl->name, (yyloc));
        push_declaration(context, &decl->base);
        (yyval.decl) = &decl->base;
    }
#line 2842 "grammar83.tab.c"
    break;

  case 37: /* type_completion: IS type_def  */
#line 415 "grammar83.y"
                { (yyval.type_decl) = (yyvsp[0].type_decl); }
#line 2848 "grammar83.tab.c"
    break;

  case 46: /* subtype_decl: SUBTYPE identifier IS subtype_ind ';'  */
#line 430 "grammar83.y"
                                          {
        TypeDecl* decl = create_type_decl(TYPE_SUBTYPE);
        decl->base.loc = (yyloc);
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
#line 2867 "grammar83.tab.c"
    break;

  case 47: /* subtype_ind: name constraint  */
#line 447 "grammar83.y"
                    {
        // TODO: propagate constraint somehow
        (yyval.str_token) = (yyvsp[-1].name).name;
    }
#line 2876 "grammar83.tab.c"
    break;

  case 48: /* subtype_ind: name  */
#line 451 "grammar83.y"
         { (yyval.str_token) = (yyvsp[0].name).name; }
#line 2882 "grammar83.tab.c"
    break;

  case 52: /* derived_type: NEW subtype_ind  */
#line 464 "grammar83.y"
                    {
        (yyval.type_decl) = create_type_decl(TYPE_DERIVED);
        TypeDecl* base_type = find_type_decl(context, (yyvsp[0].str_token));
        if(!base_type) {
            error_print((yyloc), "Unknown base type: %s", ST((yyvsp[0].str_token)));
            error_exit();
        }
        (yyval.type_decl)->u.subtype.base = base_type;
    }
#line 2896 "grammar83.tab.c"
    break;

  case 53: /* range_constraint: RANGE range  */
#line 475 "grammar83.y"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 2902 "grammar83.tab.c"
    break;

  case 54: /* range_constr_opt: %empty  */
#line 479 "grammar83.y"
           { (yyval.expr) = NULL; }
#line 2908 "grammar83.tab.c"
    break;

  case 56: /* range: simple_expression DOT_DOT simple_expression  */
#line 484 "grammar83.y"
                                                             { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_RANGE, (yyvsp[0].expr)); }
#line 2914 "grammar83.tab.c"
    break;

  case 59: /* enumeration_type: '(' enum_id_s ')'  */
#line 490 "grammar83.y"
                      {
        (yyval.type_decl) = create_type_decl(TYPE_ENUM);
        (yyval.type_decl)->u.enum_.literals = (yyvsp[-1].enum_literals).data;
        (yyval.type_decl)->u.enum_.literal_count = EnumLiteralArray_size(&(yyvsp[-1].enum_literals));
    }
#line 2924 "grammar83.tab.c"
    break;

  case 60: /* enum_id_s: enum_id  */
#line 497 "grammar83.y"
            {
        EnumLiteralArray_init(&(yyval.enum_literals));
        EnumLiteralArray_append(&(yyval.enum_literals), (yyvsp[0].enum_literal));
    }
#line 2933 "grammar83.tab.c"
    break;

  case 61: /* enum_id_s: enum_id_s ',' enum_id  */
#line 501 "grammar83.y"
                                {
        (yyval.enum_literals) = (yyvsp[-2].enum_literals);
        EnumLiteralArray_append(&(yyval.enum_literals), (yyvsp[0].enum_literal));
    }
#line 2942 "grammar83.tab.c"
    break;

  case 62: /* enum_id: identifier  */
#line 507 "grammar83.y"
               {
        clr_struct(&(yyval.enum_literal));
        (yyval.enum_literal).base.kind = DECL_ENUM_LIT;
        (yyval.enum_literal).base.loc = (yyloc);
        (yyval.enum_literal).name = (yyvsp[0].str_token);
        (yyval.enum_literal).is_char_lit = false;
    }
#line 2954 "grammar83.tab.c"
    break;

  case 63: /* enum_id: char_lit  */
#line 514 "grammar83.y"
             {
        clr_struct(&(yyval.enum_literal));
        (yyval.enum_literal).base.kind = DECL_ENUM_LIT;
        (yyval.enum_literal).base.loc = (yyloc);
        char buffer[3] = {0};
        buffer[0] = '\'';
        buffer[1] = (yyvsp[0].c);
        buffer[2] = '\'';
        StringView literal_text = { .value = buffer, .len = sizeof(buffer) };
        (yyval.enum_literal).name = string_pool_to_token(literal_text);
        (yyval.enum_literal).is_char_lit = true;
    }
#line 2971 "grammar83.tab.c"
    break;

  case 64: /* integer_type: range_constraint  */
#line 528 "grammar83.y"
                     {
        (yyval.type_decl) = create_type_decl(TYPE_INTEGER);
        (yyval.type_decl)->u.int_.range = (yyvsp[0].expr);
    }
#line 2980 "grammar83.tab.c"
    break;

  case 107: /* choice_s: choice  */
#line 652 "grammar83.y"
                        {
        ChoiceArray_init(&(yyval.choice_array));
        ChoiceArray_append(&(yyval.choice_array), (yyvsp[0].choice));
    }
#line 2989 "grammar83.tab.c"
    break;

  case 108: /* choice_s: choice_s '|' choice  */
#line 656 "grammar83.y"
                              {
        (yyval.choice_array) = (yyvsp[-2].choice_array);
        ChoiceArray_append(&(yyval.choice_array), (yyvsp[0].choice));
    }
#line 2998 "grammar83.tab.c"
    break;

  case 109: /* choice: expression  */
#line 662 "grammar83.y"
                         {
        (yyval.choice).kind = CHOICE_EXPR;
        (yyval.choice).u.expr = (yyvsp[0].expr);
    }
#line 3007 "grammar83.tab.c"
    break;

  case 111: /* choice: OTHERS  */
#line 667 "grammar83.y"
                         { (yyval.choice).kind = CHOICE_OTHERS; }
#line 3013 "grammar83.tab.c"
    break;

  case 116: /* decl_part: %empty  */
#line 681 "grammar83.y"
                         { (yyval.decl) = NULL; }
#line 3019 "grammar83.tab.c"
    break;

  case 118: /* decl_item_s: %empty  */
#line 686 "grammar83.y"
                 { (yyval.decl) = NULL; }
#line 3025 "grammar83.tab.c"
    break;

  case 121: /* decl_item_s1: decl_item_s1 decl_item  */
#line 692 "grammar83.y"
                           { (yyval.decl) = (yyvsp[-1].decl); }
#line 3031 "grammar83.tab.c"
    break;

  case 127: /* decl_item_or_body_s1: decl_item_or_body_s1 decl_item_or_body  */
#line 703 "grammar83.y"
                                           { (yyval.decl) = (yyvsp[-1].decl); }
#line 3037 "grammar83.tab.c"
    break;

  case 130: /* body: subprog_body  */
#line 711 "grammar83.y"
                 { (yyval.decl) = &(yyvsp[0].subprogram_decl)->base; }
#line 3043 "grammar83.tab.c"
    break;

  case 131: /* body: pkg_body  */
#line 712 "grammar83.y"
                 { (yyval.decl) = &(yyvsp[0].pkg_body)->base; }
#line 3049 "grammar83.tab.c"
    break;

  case 132: /* name: identifier  */
#line 716 "grammar83.y"
               {
        clr_struct(&(yyval.name));
        (yyval.name).name = (yyvsp[0].str_token);
    }
#line 3058 "grammar83.tab.c"
    break;

  case 136: /* name: operator_symbol  */
#line 723 "grammar83.y"
                    {
        clr_struct(&(yyval.name));
        (yyval.name).name = (yyvsp[0].str_token);
        //TODO: lookup operator, determine its arity, and allocate args array
    }
#line 3068 "grammar83.tab.c"
    break;

  case 140: /* used_char: char_lit  */
#line 736 "grammar83.y"
             {
        (yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
        (yyval.expr)->u.char_lit = (yyvsp[0].c);
    }
#line 3077 "grammar83.tab.c"
    break;

  case 158: /* literal: numeric_lit  */
#line 780 "grammar83.y"
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
#line 3101 "grammar83.tab.c"
    break;

  case 170: /* expression: expression logical relation  */
#line 822 "grammar83.y"
                                                       { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3107 "grammar83.tab.c"
    break;

  case 171: /* expression: expression short_circuit relation  */
#line 823 "grammar83.y"
                                                       { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3113 "grammar83.tab.c"
    break;

  case 172: /* logical: AND  */
#line 827 "grammar83.y"
        { (yyval.unary_op) = OP_AND; }
#line 3119 "grammar83.tab.c"
    break;

  case 173: /* logical: OR  */
#line 828 "grammar83.y"
        { (yyval.unary_op) = OP_OR; }
#line 3125 "grammar83.tab.c"
    break;

  case 174: /* logical: XOR  */
#line 829 "grammar83.y"
        { (yyval.unary_op) = OP_XOR; }
#line 3131 "grammar83.tab.c"
    break;

  case 175: /* short_circuit: AND THEN  */
#line 833 "grammar83.y"
               { (yyval.unary_op) = OP_AND_THEN; }
#line 3137 "grammar83.tab.c"
    break;

  case 176: /* short_circuit: OR ELSE  */
#line 834 "grammar83.y"
               { (yyval.unary_op) = OP_OR_ELSE; }
#line 3143 "grammar83.tab.c"
    break;

  case 178: /* relation: simple_expression relational simple_expression  */
#line 840 "grammar83.y"
                                                                    { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3149 "grammar83.tab.c"
    break;

  case 179: /* relation: simple_expression membership range  */
#line 841 "grammar83.y"
                                                                    { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3155 "grammar83.tab.c"
    break;

  case 180: /* relation: simple_expression membership name  */
#line 842 "grammar83.y"
                                                                    {
        Expression* right = create_expr(EXPR_NAME, (yylsp[0]));
        right->u.name = (yyvsp[0].name);
        (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), right);
    }
#line 3165 "grammar83.tab.c"
    break;

  case 181: /* relational: '='  */
#line 849 "grammar83.y"
          { (yyval.unary_op) = OP_EQ; }
#line 3171 "grammar83.tab.c"
    break;

  case 182: /* relational: NE  */
#line 850 "grammar83.y"
          { (yyval.unary_op) = OP_NEQ; }
#line 3177 "grammar83.tab.c"
    break;

  case 183: /* relational: '<'  */
#line 851 "grammar83.y"
          { (yyval.unary_op) = OP_LT; }
#line 3183 "grammar83.tab.c"
    break;

  case 184: /* relational: LT_EQ  */
#line 852 "grammar83.y"
          { (yyval.unary_op) = OP_LTE; }
#line 3189 "grammar83.tab.c"
    break;

  case 185: /* relational: '>'  */
#line 853 "grammar83.y"
          { (yyval.unary_op) = OP_GT; }
#line 3195 "grammar83.tab.c"
    break;

  case 186: /* relational: GE  */
#line 854 "grammar83.y"
          { (yyval.unary_op) = OP_GTE; }
#line 3201 "grammar83.tab.c"
    break;

  case 187: /* membership: IN  */
#line 858 "grammar83.y"
           { (yyval.unary_op) = OP_IN; }
#line 3207 "grammar83.tab.c"
    break;

  case 188: /* membership: NOT IN  */
#line 859 "grammar83.y"
           { (yyval.unary_op) = OP_NOT_IN; }
#line 3213 "grammar83.tab.c"
    break;

  case 190: /* simple_expression: unary term  */
#line 864 "grammar83.y"
                                                   { (yyval.expr) = make_unary_expr((yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3219 "grammar83.tab.c"
    break;

  case 191: /* simple_expression: simple_expression adding term  */
#line 865 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3225 "grammar83.tab.c"
    break;

  case 192: /* unary: '+'  */
#line 869 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_PLUS; }
#line 3231 "grammar83.tab.c"
    break;

  case 193: /* unary: '-'  */
#line 870 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_MINUS; }
#line 3237 "grammar83.tab.c"
    break;

  case 194: /* adding: '+'  */
#line 874 "grammar83.y"
        { (yyval.unary_op) = OP_PLUS; }
#line 3243 "grammar83.tab.c"
    break;

  case 195: /* adding: '-'  */
#line 875 "grammar83.y"
        { (yyval.unary_op) = OP_MINUS; }
#line 3249 "grammar83.tab.c"
    break;

  case 196: /* adding: '&'  */
#line 876 "grammar83.y"
        { (yyval.unary_op) = OP_AMP; }
#line 3255 "grammar83.tab.c"
    break;

  case 198: /* term: term multiplying factor  */
#line 881 "grammar83.y"
                                             { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3261 "grammar83.tab.c"
    break;

  case 199: /* multiplying: '*'  */
#line 885 "grammar83.y"
        { (yyval.unary_op) = OP_MULT; }
#line 3267 "grammar83.tab.c"
    break;

  case 200: /* multiplying: '/'  */
#line 886 "grammar83.y"
        { (yyval.unary_op) = OP_DIVIDE; }
#line 3273 "grammar83.tab.c"
    break;

  case 201: /* multiplying: MOD  */
#line 887 "grammar83.y"
        { (yyval.unary_op) = OP_MOD; }
#line 3279 "grammar83.tab.c"
    break;

  case 202: /* multiplying: REM  */
#line 888 "grammar83.y"
        { (yyval.unary_op) = OP_REM; }
#line 3285 "grammar83.tab.c"
    break;

  case 204: /* factor: NOT primary  */
#line 893 "grammar83.y"
                                       { (yyval.expr) = make_unary_expr(OP_NOT, (yyvsp[0].expr)); }
#line 3291 "grammar83.tab.c"
    break;

  case 205: /* factor: ABS primary  */
#line 894 "grammar83.y"
                                       { (yyval.expr) = make_unary_expr(OP_ABS, (yyvsp[0].expr)); }
#line 3297 "grammar83.tab.c"
    break;

  case 206: /* factor: primary EXPON primary  */
#line 895 "grammar83.y"
                                       { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_EXP, (yyvsp[0].expr)); }
#line 3303 "grammar83.tab.c"
    break;

  case 208: /* primary: name  */
#line 900 "grammar83.y"
         {
        (yyval.expr) = create_expr(EXPR_NAME, (yyloc));
        (yyval.expr)->u.name = (yyvsp[0].name);
    }
#line 3312 "grammar83.tab.c"
    break;

  case 213: /* parenthesized_primary: '(' expression ')'  */
#line 911 "grammar83.y"
                       { (yyval.expr) = (yyvsp[-1].expr); }
#line 3318 "grammar83.tab.c"
    break;

  case 214: /* qualified: name '\'' parenthesized_primary  */
#line 915 "grammar83.y"
                                          {
        // TODO: support other kinds of names
        assert((yyvsp[-2].name).arg_count == 0);
        TypeDecl* type_decl = find_type_decl(context, (yyvsp[-2].name).name);
        if(!type_decl) {
            error_print((yyloc), "Unknown type: %s", ST((yyvsp[-2].name).name));
            error_exit();
        }
        (yyval.expr) = create_expr(EXPR_QUALIFIED, (yyloc));
        (yyval.expr)->u.qualified.type = type_decl;
        (yyval.expr)->u.qualified.expr = (yyvsp[0].expr);
    }
#line 3335 "grammar83.tab.c"
    break;

  case 217: /* statement_s: statement  */
#line 934 "grammar83.y"
                          {
        clr_struct(&(yyval.stmt_list));
        StmtList_append(&(yyval.stmt_list), (yyvsp[0].stmt));
    }
#line 3344 "grammar83.tab.c"
    break;

  case 218: /* statement_s: statement_s statement  */
#line 938 "grammar83.y"
                                {
        StmtList_append(&(yyvsp[-1].stmt_list), (yyvsp[0].stmt));
        (yyval.stmt_list) = (yyvsp[-1].stmt_list);
    }
#line 3353 "grammar83.tab.c"
    break;

  case 220: /* statement: goto_label statement  */
#line 945 "grammar83.y"
                         {
        LabelDecl* label = find_label(context, (yyvsp[-1].str_token));
        if(label) {
            if(label->is_placeholder) {
                // Fill in the placeholder
                label->is_placeholder = false;
                label->base.loc = (yylsp[-1]);
            } else {
                error_print((yylsp[-1]), "Redefinition of label '%s'", ST((yyvsp[-1].str_token)));
                error_print(label->base.loc, "Previous definition here");
                error_exit();
            }
        } else {
            check_for_redefinition(context, (yyvsp[-1].str_token), (yylsp[-1]));
            label = create_label((yyvsp[-1].str_token), (yylsp[-1]));
            push_declaration(context, (Declaration*)label);
        }
        (yyval.stmt) = (yyvsp[0].stmt);
    }
#line 3377 "grammar83.tab.c"
    break;

  case 237: /* null_stmt: NuLL ';'  */
#line 991 "grammar83.y"
             { (yyval.stmt) = create_stmt(STMT_NULL, (yyloc)); }
#line 3383 "grammar83.tab.c"
    break;

  case 238: /* assign_stmt: name IS_ASSIGNED expression ';'  */
#line 995 "grammar83.y"
                                    {
        (yyval.stmt) = create_stmt(STMT_ASSIGN, (yyloc));
        (yyval.stmt)->u.assign.dest.kind = EXPR_NAME;
        (yyval.stmt)->u.assign.dest.loc = (yyloc);
        (yyval.stmt)->u.assign.dest.u.name = (yyvsp[-3].name);
        (yyval.stmt)->u.assign.expr = (yyvsp[-1].expr);
    }
#line 3395 "grammar83.tab.c"
    break;

  case 239: /* if_stmt: IF cond_clause_s else_opt END IF ';'  */
#line 1004 "grammar83.y"
                                         {
        (yyval.stmt) = (yyvsp[-4].stmt);
        Statement* branch = (yyvsp[-4].stmt);
        while(branch->u.if_.else_) {
            branch = branch->u.if_.else_;
            assert(branch->kind == STMT_IF);
        }
        branch->u.if_.else_ = (yyvsp[-3].stmt);
    }
#line 3409 "grammar83.tab.c"
    break;

  case 241: /* cond_clause_s: cond_clause_s ELSIF cond_clause  */
#line 1016 "grammar83.y"
                                               {
        (yyval.stmt) = (yyvsp[-2].stmt);
        (yyval.stmt)->u.if_.else_ = (yyvsp[0].stmt);
    }
#line 3418 "grammar83.tab.c"
    break;

  case 242: /* cond_clause: cond_part statement_s  */
#line 1022 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_IF, (yyloc));
        (yyval.stmt)->u.if_.condition = (yyvsp[-1].expr);
        (yyval.stmt)->u.if_.stmts = (yyvsp[0].stmt_list).first;
    }
#line 3428 "grammar83.tab.c"
    break;

  case 243: /* cond_part: condition THEN  */
#line 1029 "grammar83.y"
                   { (yyval.expr) = (yyvsp[-1].expr); }
#line 3434 "grammar83.tab.c"
    break;

  case 245: /* else_opt: %empty  */
#line 1037 "grammar83.y"
                     { (yyval.stmt) = NULL; }
#line 3440 "grammar83.tab.c"
    break;

  case 246: /* else_opt: ELSE statement_s  */
#line 1038 "grammar83.y"
                     { (yyval.stmt) = (yyvsp[0].stmt_list).first; }
#line 3446 "grammar83.tab.c"
    break;

  case 247: /* case_stmt: case_hdr pragma_s alternative_s END CASE ';'  */
#line 1042 "grammar83.y"
                                                 {
        (yyval.stmt) = (yyvsp[-5].stmt);
        // TODO: pragmas
        (yyval.stmt)->u.case_.cases = (yyvsp[-3].case_list).first;
    }
#line 3456 "grammar83.tab.c"
    break;

  case 248: /* case_hdr: CASE expression IS  */
#line 1049 "grammar83.y"
                       {
        (yyval.stmt) = create_stmt(STMT_CASE, (yyloc));
        (yyval.stmt)->u.case_.expr = (yyvsp[-1].expr);
    }
#line 3465 "grammar83.tab.c"
    break;

  case 249: /* alternative_s: %empty  */
#line 1055 "grammar83.y"
                              { clr_struct(&(yyval.case_list)); }
#line 3471 "grammar83.tab.c"
    break;

  case 250: /* alternative_s: alternative_s alternative  */
#line 1056 "grammar83.y"
                                    {
        (yyval.case_list) = (yyvsp[-1].case_list);
        AltList_append(&(yyval.case_list), (yyvsp[0].case_));
    }
#line 3480 "grammar83.tab.c"
    break;

  case 251: /* alternative: WHEN choice_s RIGHT_SHAFT statement_s  */
#line 1062 "grammar83.y"
                                          {
        (yyval.case_) = calloc(1, sizeof(Alternative));
        (yyval.case_)->choices.choices = (yyvsp[-2].choice_array).data;
        (yyval.case_)->choices.count = ChoiceArray_size(&(yyvsp[-2].choice_array));
        (yyval.case_)->stmts = (yyvsp[0].stmt_list).first;
    }
#line 3491 "grammar83.tab.c"
    break;

  case 252: /* loop_stmt: label_opt loop_content id_opt ';'  */
#line 1071 "grammar83.y"
                                      { (yyval.stmt) = (yyvsp[-2].stmt); }
#line 3497 "grammar83.tab.c"
    break;

  case 255: /* loop_content: basic_loop  */
#line 1080 "grammar83.y"
               {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_WHILE;
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
        // Create condition so this becomes a 'while True' loop
        Expression* condition = create_expr(EXPR_ENUM_LIT, (yyloc));
        condition->u.enum_lit = &boolean_type.u.enum_.literals[true];
        (yyval.stmt)->u.loop.u.while_.condition = condition;
    }
#line 3511 "grammar83.tab.c"
    break;

  case 256: /* loop_content: WHILE condition basic_loop  */
#line 1089 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_WHILE;
        (yyval.stmt)->u.loop.u.while_.condition = (yyvsp[-1].expr);
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
    }
#line 3522 "grammar83.tab.c"
    break;

  case 257: /* loop_content: iter_part reverse_opt discrete_range basic_loop  */
#line 1095 "grammar83.y"
                                                    {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_FOR;
        (yyval.stmt)->u.loop.reverse = (yyvsp[-2].bool_);
        (yyval.stmt)->u.loop.u.for_.var = (yyvsp[-3].object_decl);
        (yyval.stmt)->u.loop.u.for_.range = (yyvsp[-1].expr);
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
    }
#line 3535 "grammar83.tab.c"
    break;

  case 258: /* iter_part: FOR identifier IN  */
#line 1105 "grammar83.y"
                      {
        clr_struct(&(yyval.object_decl));
        (yyval.object_decl).base.kind = DECL_OBJECT;
        (yyval.object_decl).base.loc = (yyloc);
        (yyval.object_decl).name = (yyvsp[-1].str_token);
    }
#line 3546 "grammar83.tab.c"
    break;

  case 259: /* reverse_opt: %empty  */
#line 1113 "grammar83.y"
            { (yyval.bool_) = false; }
#line 3552 "grammar83.tab.c"
    break;

  case 260: /* reverse_opt: REVERSE  */
#line 1114 "grammar83.y"
            { (yyval.bool_) = true; }
#line 3558 "grammar83.tab.c"
    break;

  case 261: /* basic_loop: LOOP statement_s END LOOP  */
#line 1118 "grammar83.y"
                              { (yyval.stmt) = (yyvsp[-2].stmt_list).first; }
#line 3564 "grammar83.tab.c"
    break;

  case 264: /* block: label_opt block_decl block_body END id_opt ';'  */
#line 1128 "grammar83.y"
                                                   {
        (yyval.stmt) = create_stmt(STMT_BLOCK, (yyloc));
        (yyval.stmt)->u.block.decls = (yyvsp[-4].decl);
        (yyval.stmt)->u.block.stmts = (yyvsp[-3].stmt);
        // Close scope if there was a declaration section
        if((yyvsp[-4].decl)) {
            end_scope(context, (yylsp[-2]));
        }
    }
#line 3578 "grammar83.tab.c"
    break;

  case 265: /* block_decl: %empty  */
#line 1139 "grammar83.y"
                                                          { (yyval.decl) = NULL; }
#line 3584 "grammar83.tab.c"
    break;

  case 266: /* $@1: %empty  */
#line 1140 "grammar83.y"
            { begin_scope(context, (yylsp[0])); }
#line 3590 "grammar83.tab.c"
    break;

  case 267: /* block_decl: DECLARE $@1 decl_part  */
#line 1140 "grammar83.y"
                                                          {
        (yyval.decl) = (yyvsp[0].decl);
        // Close scope if no declaration section
        if(!(yyval.decl)) {
            end_scope(context, (yylsp[-2]));
        }
    }
#line 3602 "grammar83.tab.c"
    break;

  case 268: /* block_body: BEGiN handled_stmt_s  */
#line 1149 "grammar83.y"
                         { (yyval.stmt) = (yyvsp[0].stmt); }
#line 3608 "grammar83.tab.c"
    break;

  case 269: /* handled_stmt_s: statement_s except_handler_part_opt  */
#line 1154 "grammar83.y"
                                        { (yyval.stmt) = (yyvsp[-1].stmt_list).first; }
#line 3614 "grammar83.tab.c"
    break;

  case 272: /* exit_stmt: EXIT name_opt when_opt ';'  */
#line 1163 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_EXIT, (yyloc));
        // TODO: name_opt
        (yyval.stmt)->u.exit.condition = (yyvsp[-1].expr);
    }
#line 3624 "grammar83.tab.c"
    break;

  case 275: /* when_opt: %empty  */
#line 1175 "grammar83.y"
                   { (yyval.expr) = NULL; }
#line 3630 "grammar83.tab.c"
    break;

  case 276: /* when_opt: WHEN condition  */
#line 1176 "grammar83.y"
                   { (yyval.expr) = (yyvsp[0].expr); }
#line 3636 "grammar83.tab.c"
    break;

  case 277: /* return_stmt: RETURN ';'  */
#line 1180 "grammar83.y"
                          { (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc)); }
#line 3642 "grammar83.tab.c"
    break;

  case 278: /* return_stmt: RETURN expression ';'  */
#line 1181 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc));
        (yyval.stmt)->u.return_.expr = (yyvsp[-1].expr);
    }
#line 3651 "grammar83.tab.c"
    break;

  case 279: /* goto_stmt: GOTO identifier ';'  */
#line 1187 "grammar83.y"
                        {
        StringToken label_name = (yyvsp[-1].str_token);

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
            label->is_placeholder = true;
            (yyval.stmt)->u.goto_.label = label;
            push_declaration(context, (Declaration*)label);
        }
    }
#line 3675 "grammar83.tab.c"
    break;

  case 280: /* subprog_decl: subprog_spec ';'  */
#line 1208 "grammar83.y"
                          {
        (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl);
        end_scope(context, (yylsp[0]));
    }
#line 3684 "grammar83.tab.c"
    break;

  case 282: /* @2: %empty  */
#line 1217 "grammar83.y"
                                          {
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
        push_declaration(context, &(yyval.subprogram_decl)->base);
        begin_scope(context, (yylsp[0]));
    }
#line 3695 "grammar83.tab.c"
    break;

  case 283: /* subprog_spec: PROCEDURE identifier @2 formal_part_opt  */
#line 1223 "grammar83.y"
                                { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3701 "grammar83.tab.c"
    break;

  case 284: /* @3: %empty  */
#line 1224 "grammar83.y"
                                         {
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
        push_declaration(context, &(yyval.subprogram_decl)->base);
        begin_scope(context, (yylsp[0]));
    }
#line 3712 "grammar83.tab.c"
    break;

  case 285: /* subprog_spec: FUNCTION designator @3 formal_part_opt RETURN name  */
#line 1230 "grammar83.y"
                                { (yyval.subprogram_decl) = (yyvsp[-3].subprogram_decl); }
#line 3718 "grammar83.tab.c"
    break;

  case 288: /* designator: char_string  */
#line 1236 "grammar83.y"
                { (yyval.str_token) = string_pool_to_token((yyvsp[0].str)); }
#line 3724 "grammar83.tab.c"
    break;

  case 296: /* mode: %empty  */
#line 1259 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3730 "grammar83.tab.c"
    break;

  case 297: /* mode: IN  */
#line 1260 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3736 "grammar83.tab.c"
    break;

  case 298: /* mode: OUT  */
#line 1261 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_OUT; }
#line 3742 "grammar83.tab.c"
    break;

  case 299: /* mode: IN OUT  */
#line 1262 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN_OUT; }
#line 3748 "grammar83.tab.c"
    break;

  case 300: /* subprog_spec_is_push: subprog_spec IS  */
#line 1266 "grammar83.y"
                    { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3754 "grammar83.tab.c"
    break;

  case 301: /* subprog_body: subprog_spec_is_push decl_part block_body END id_opt ';'  */
#line 1272 "grammar83.y"
                                                             {
        (yyval.subprogram_decl) = (yyvsp[-5].subprogram_decl);
        (yyval.subprogram_decl)->decls = (yyvsp[-4].decl);
        (yyval.subprogram_decl)->stmts = (yyvsp[-3].stmt);
        // Close scope opened in subprog_spec
        end_scope(context, (yylsp[-2]));
    }
#line 3766 "grammar83.tab.c"
    break;

  case 302: /* procedure_call: name ';'  */
#line 1281 "grammar83.y"
             {
        (yyval.stmt) = create_stmt(STMT_EXPR, (yyloc));
        (yyval.stmt)->u.expr.kind = EXPR_NAME;
        (yyval.stmt)->u.expr.loc = (yyloc);
        (yyval.stmt)->u.expr.u.name = (yyvsp[-1].name);
    }
#line 3777 "grammar83.tab.c"
    break;

  case 303: /* pkg_decl: pkg_spec ';'  */
#line 1289 "grammar83.y"
                         { (yyval.pkg_spec) = (yyvsp[-1].pkg_spec); }
#line 3783 "grammar83.tab.c"
    break;

  case 305: /* @4: %empty  */
#line 1294 "grammar83.y"
                                    {
        begin_scope(context, (yylsp[0]));
        (yyval.pkg_spec) = calloc(1, sizeof(PackageSpec));
        (yyval.pkg_spec)->base.kind = DECL_PKG_SPEC;
        (yyval.pkg_spec)->base.loc = (yyloc);
        (yyval.pkg_spec)->name = (yyvsp[-1].str_token);
    }
#line 3795 "grammar83.tab.c"
    break;

  case 306: /* pkg_spec: PACKAGE identifier IS @4 decl_item_s private_part END identifier_opt  */
#line 1301 "grammar83.y"
                                                {
        (yyval.pkg_spec) = (yyvsp[-4].pkg_spec);
        (yyval.pkg_spec)->decls = (yyvsp[-3].decl);
        // TODO: private part
        end_scope(context, (yylsp[-1]));
        if((yyvsp[0].str_token) && (yyval.pkg_spec)->name != (yyvsp[0].str_token)) {
            error_print((yylsp[0]),
                "End label '%s' does not match package name ('%s')", ST((yyvsp[0].str_token)), ST((yyval.pkg_spec)->name));
            error_exit();
        }
        push_declaration(context, &(yyval.pkg_spec)->base);
    }
#line 3812 "grammar83.tab.c"
    break;

  case 309: /* identifier_opt: %empty  */
#line 1320 "grammar83.y"
               { (yyval.str_token) = 0; }
#line 3818 "grammar83.tab.c"
    break;

  case 311: /* @5: %empty  */
#line 1325 "grammar83.y"
                                         {
        begin_scope(context, (yylsp[0]));
        (yyval.pkg_body) = calloc(1, sizeof(PackageBody));
        (yyval.pkg_body)->base.kind = DECL_PKG_BODY;
        (yyval.pkg_body)->base.loc = (yyloc);
        (yyval.pkg_body)->name = (yyvsp[-1].str_token);
    }
#line 3830 "grammar83.tab.c"
    break;

  case 312: /* pkg_body: PACKAGE BODY identifier IS @5 decl_part body_opt END identifier_opt ';'  */
#line 1332 "grammar83.y"
                                              {
        (yyval.pkg_body) = (yyvsp[-5].pkg_body);
        (yyval.pkg_body)->decls = (yyvsp[-4].decl);
        // TODO: body_opt
        end_scope(context, (yylsp[-2]));
        if((yyvsp[-1].str_token) && (yyval.pkg_body)->name != (yyvsp[-1].str_token)) {
            error_print((yylsp[-1]),
                "End label '%s' does not match package name ('%s')", ST((yyvsp[-1].str_token)), ST((yyval.pkg_body)->name));
            error_exit();
        }
        push_declaration(context, &(yyval.pkg_body)->base);
    }
#line 3847 "grammar83.tab.c"
    break;

  case 318: /* use_name_s: identifier  */
#line 1361 "grammar83.y"
                                 {
        StringTokenArray_init(&(yyval.str_token_array));
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 3856 "grammar83.tab.c"
    break;

  case 320: /* use_name_s: use_name_s ',' identifier  */
#line 1366 "grammar83.y"
                                 {
        (yyval.str_token_array) = (yyvsp[-2].str_token_array);
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 3865 "grammar83.tab.c"
    break;

  case 322: /* use_clause: USE use_name_s ';'  */
#line 1374 "grammar83.y"
                       {
        (yyval.decl) = NULL;
        uint32_t package_count = StringTokenArray_size(&(yyvsp[-1].str_token_array));
        for(uint32_t i = 0; i < package_count; ++i) {
            StringToken package_name = (yyvsp[-1].str_token_array).data[i];
            PackageSpec* package_spec = find_package_spec(context, package_name);
            if(!package_spec) {
                error_print((yylsp[-1]), "Unknown package name '%s'", ST(package_name));
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
            use_clause->base.loc = (yyloc);
            use_clause->package_spec = package_spec;
            push_declaration(context, &use_clause->base);
            if(!(yyval.decl)) {
                (yyval.decl) = &use_clause->base;
            }
        }
    }
#line 3898 "grammar83.tab.c"
    break;

  case 323: /* rename_decl: def_id_s ':' object_qualifier_opt subtype_ind renames ';'  */
#line 1405 "grammar83.y"
                                                              {
        uint32_t ident_count = StringTokenArray_size(&(yyvsp[-5].str_token_array));
        if(ident_count != 1) {
            error_print((yylsp[-5]),
                "Renames declarations must have exactly one identifier on the left-hand side of the 'renames' keyword");
            error_exit();
        }
        RenameDecl* rename_decl = calloc(1, sizeof(RenameDecl));
        rename_decl->base.kind = DECL_RENAME;
        rename_decl->base.loc = (yyloc);
        rename_decl->name = (yyvsp[-5].str_token_array).data[0];
        rename_decl->target.kind = EXPR_NAME;
        rename_decl->target.loc = (yyloc);
        rename_decl->target.u.name = (yyvsp[-1].name);
        // TODO: handle object_qualifier_opt
        // TODO: handle subtype_ind
        // TODO: check that the target is an object (or some kind of slice/expression that yields an object)
        push_declaration(context, &rename_decl->base);
    }
#line 3922 "grammar83.tab.c"
    break;

  case 330: /* renames: RENAMES name  */
#line 1436 "grammar83.y"
                 { (yyval.name) = (yyvsp[0].name); }
#line 3928 "grammar83.tab.c"
    break;

  case 331: /* comp_unit: context_spec unit pragma_s  */
#line 1440 "grammar83.y"
                               { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3934 "grammar83.tab.c"
    break;

  case 332: /* comp_unit: unit pragma_s  */
#line 1441 "grammar83.y"
                               { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3940 "grammar83.tab.c"
    break;

  case 336: /* with_clause: WITH def_id_s ';'  */
#line 1451 "grammar83.y"
                      {
        uint32_t package_count = StringTokenArray_size(&(yyvsp[-1].str_token_array));
        for(uint32_t i = 0; i < package_count; ++i) {
            const char* package_name = string_pool_to_str((yyvsp[-1].str_token_array).data[i]);
            CompilationUnit* unit = comp_manager_parse_spec(context->comp_manager, package_name, &(yyloc));
            assert(unit->kind == COMP_UNIT_PACKAGE_SPEC);
            push_declaration(context, &unit->u.package_spec->base);
        }
    }
#line 3954 "grammar83.tab.c"
    break;

  case 339: /* unit: pkg_decl  */
#line 1467 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_PACKAGE_SPEC);
        (yyval.comp_unit)->u.package_spec = (yyvsp[0].pkg_spec);
    }
#line 3963 "grammar83.tab.c"
    break;

  case 340: /* unit: pkg_body  */
#line 1471 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_PACKAGE_BODY);
        (yyval.comp_unit)->u.package_body = (yyvsp[0].pkg_body);
    }
#line 3972 "grammar83.tab.c"
    break;

  case 341: /* unit: subprog_decl  */
#line 1475 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        (yyval.comp_unit)->u.subprogram_decl = (yyvsp[0].subprogram_decl);
    }
#line 3981 "grammar83.tab.c"
    break;

  case 342: /* unit: subprog_body  */
#line 1479 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        (yyval.comp_unit)->u.subprogram_decl = (yyvsp[0].subprogram_decl);
    }
#line 3990 "grammar83.tab.c"
    break;


#line 3994 "grammar83.tab.c"

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

#line 1624 "grammar83.y"


static
Expression* make_binary_expr(Expression* left, BinaryOperator op, Expression* right)
{
    Expression* expr = create_expr(EXPR_BINARY, left->loc);
    expr->u.binary.left = left;
    expr->u.binary.op = op;
    expr->u.binary.right = right;
    return expr;
}

static
Expression* make_unary_expr(UnaryOperator op, Expression* right)
{
    Expression* expr = create_expr(EXPR_UNARY, right->loc);
    expr->u.unary.op = op;
    expr->u.unary.right = right;
    return expr;
}

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
