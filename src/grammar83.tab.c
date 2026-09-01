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
  YYSYMBOL_214_1 = 214,                    /* $@1  */
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
  YYSYMBOL_225_2 = 225,                    /* @2  */
  YYSYMBOL_226_3 = 226,                    /* @3  */
  YYSYMBOL_designator = 227,               /* designator  */
  YYSYMBOL_formal_part_opt = 228,          /* formal_part_opt  */
  YYSYMBOL_formal_part = 229,              /* formal_part  */
  YYSYMBOL_param_s = 230,                  /* param_s  */
  YYSYMBOL_param = 231,                    /* param  */
  YYSYMBOL_mode = 232,                     /* mode  */
  YYSYMBOL_subprog_spec_is_push = 233,     /* subprog_spec_is_push  */
  YYSYMBOL_subprog_body = 234,             /* subprog_body  */
  YYSYMBOL_procedure_call = 235,           /* procedure_call  */
  YYSYMBOL_pkg_decl = 236,                 /* pkg_decl  */
  YYSYMBOL_pkg_spec = 237,                 /* pkg_spec  */
  YYSYMBOL_238_4 = 238,                    /* @4  */
  YYSYMBOL_private_part = 239,             /* private_part  */
  YYSYMBOL_identifier_opt = 240,           /* identifier_opt  */
  YYSYMBOL_pkg_body = 241,                 /* pkg_body  */
  YYSYMBOL_242_5 = 242,                    /* @5  */
  YYSYMBOL_body_opt = 243,                 /* body_opt  */
  YYSYMBOL_private_type = 244,             /* private_type  */
  YYSYMBOL_limited_opt = 245,              /* limited_opt  */
  YYSYMBOL_use_name_s = 246,               /* use_name_s  */
  YYSYMBOL_use_clause = 247,               /* use_clause  */
  YYSYMBOL_rename_decl = 248,              /* rename_decl  */
  YYSYMBOL_rename_unit = 249,              /* rename_unit  */
  YYSYMBOL_comp_unit = 250,                /* comp_unit  */
  YYSYMBOL_context_spec = 251,             /* context_spec  */
  YYSYMBOL_with_clause = 252,              /* with_clause  */
  YYSYMBOL_use_clause_opt = 253,           /* use_clause_opt  */
  YYSYMBOL_unit = 254,                     /* unit  */
  YYSYMBOL_subunit = 255,                  /* subunit  */
  YYSYMBOL_subunit_body = 256,             /* subunit_body  */
  YYSYMBOL_body_stub = 257,                /* body_stub  */
  YYSYMBOL_exception_decl = 258,           /* exception_decl  */
  YYSYMBOL_except_handler_part = 259,      /* except_handler_part  */
  YYSYMBOL_exception_handler = 260,        /* exception_handler  */
  YYSYMBOL_except_choice_s = 261,          /* except_choice_s  */
  YYSYMBOL_except_choice = 262,            /* except_choice  */
  YYSYMBOL_raise_stmt = 263,               /* raise_stmt  */
  YYSYMBOL_generic_decl = 264,             /* generic_decl  */
  YYSYMBOL_generic_formal_part = 265,      /* generic_formal_part  */
  YYSYMBOL_generic_formal = 266,           /* generic_formal  */
  YYSYMBOL_generic_discrim_part_opt = 267, /* generic_discrim_part_opt  */
  YYSYMBOL_subp_default = 268,             /* subp_default  */
  YYSYMBOL_generic_type_def = 269,         /* generic_type_def  */
  YYSYMBOL_generic_derived_type = 270,     /* generic_derived_type  */
  YYSYMBOL_generic_subp_inst = 271,        /* generic_subp_inst  */
  YYSYMBOL_generic_pkg_inst = 272,         /* generic_pkg_inst  */
  YYSYMBOL_generic_inst = 273,             /* generic_inst  */
  YYSYMBOL_rep_spec = 274,                 /* rep_spec  */
  YYSYMBOL_attrib_def = 275,               /* attrib_def  */
  YYSYMBOL_record_type_spec = 276,         /* record_type_spec  */
  YYSYMBOL_align_opt = 277,                /* align_opt  */
  YYSYMBOL_comp_loc_s = 278,               /* comp_loc_s  */
  YYSYMBOL_address_spec = 279,             /* address_spec  */
  YYSYMBOL_code_stmt = 280                 /* code_stmt  */
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

#line 495 "grammar83.tab.c"

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
#define YYLAST   1330

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  95
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  186
/* YYNRULES -- Number of rules.  */
#define YYNRULES  400
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  720

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
       0,   285,   285,   289,   290,   294,   295,   299,   300,   304,
     305,   309,   310,   311,   312,   313,   314,   315,   316,   317,
     318,   319,   323,   351,   368,   372,   379,   380,   384,   385,
     389,   390,   394,   406,   407,   408,   413,   414,   418,   419,
     420,   421,   422,   423,   424,   425,   429,   446,   450,   454,
     455,   459,   463,   474,   478,   479,   483,   484,   485,   489,
     496,   500,   506,   513,   527,   531,   535,   536,   540,   544,
     545,   549,   550,   554,   558,   562,   566,   567,   571,   575,
     579,   580,   584,   585,   589,   593,   594,   598,   599,   600,
     604,   605,   609,   610,   614,   615,   619,   623,   624,   628,
     629,   633,   634,   638,   642,   643,   647,   651,   655,   661,
     665,   666,   670,   671,   675,   676,   680,   681,   685,   686,
     690,   691,   694,   695,   696,   697,   701,   702,   705,   706,
     710,   711,   718,   722,   723,   724,   725,   732,   733,   734,
     738,   744,   748,   752,   753,   757,   758,   759,   760,   764,
     765,   766,   767,   771,   775,   776,   777,   778,   782,   801,
     802,   806,   807,   808,   809,   810,   814,   815,   819,   823,
     824,   825,   829,   830,   831,   835,   836,   841,   842,   843,
     844,   851,   852,   853,   854,   855,   856,   860,   861,   865,
     866,   867,   871,   872,   876,   877,   878,   882,   883,   887,
     888,   889,   890,   894,   895,   896,   897,   901,   902,   906,
     907,   908,   912,   913,   917,   931,   932,   936,   940,   946,
     947,   968,   969,   970,   974,   975,   976,   977,   978,   979,
     980,   981,   982,   986,   987,   988,   989,   993,   997,  1006,
    1017,  1018,  1024,  1031,  1035,  1036,  1040,  1047,  1053,  1054,
    1060,  1069,  1073,  1074,  1078,  1086,  1091,  1100,  1101,  1105,
    1109,  1110,  1115,  1126,  1127,  1127,  1136,  1141,  1145,  1146,
    1150,  1157,  1158,  1162,  1163,  1167,  1168,  1174,  1195,  1199,
    1204,  1204,  1211,  1211,  1218,  1222,  1223,  1227,  1228,  1232,
    1236,  1237,  1241,  1242,  1246,  1247,  1248,  1249,  1253,  1259,
    1268,  1276,  1277,  1281,  1281,  1302,  1303,  1307,  1308,  1312,
    1312,  1333,  1334,  1338,  1342,  1343,  1348,  1352,  1353,  1357,
    1361,  1392,  1411,  1412,  1416,  1417,  1418,  1419,  1423,  1424,
    1428,  1429,  1430,  1434,  1445,  1446,  1450,  1454,  1458,  1462,
    1466,  1467,  1468,  1472,  1476,  1477,  1481,  1482,  1486,  1490,
    1491,  1495,  1496,  1500,  1501,  1505,  1506,  1510,  1514,  1515,
    1519,  1520,  1524,  1525,  1526,  1527,  1528,  1529,  1530,  1534,
    1535,  1536,  1540,  1541,  1542,  1546,  1547,  1548,  1549,  1550,
    1551,  1552,  1553,  1554,  1555,  1559,  1560,  1564,  1568,  1572,
    1576,  1577,  1578,  1582,  1586,  1590,  1591,  1595,  1596,  1600,
    1604
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
  "block_decl", "$@1", "block_body", "handled_stmt_s",
  "except_handler_part_opt", "exit_stmt", "name_opt", "when_opt",
  "return_stmt", "goto_stmt", "subprog_decl", "subprog_spec", "@2", "@3",
  "designator", "formal_part_opt", "formal_part", "param_s", "param",
  "mode", "subprog_spec_is_push", "subprog_body", "procedure_call",
  "pkg_decl", "pkg_spec", "@4", "private_part", "identifier_opt",
  "pkg_body", "@5", "body_opt", "private_type", "limited_opt",
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

#define YYPACT_NINF (-564)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-353)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     854,   131,  -564,    35,     4,     5,    13,   107,  -564,    30,
    1216,  -564,  -564,   113,  -564,  -564,  -564,   964,  -564,  -564,
    -564,  -564,   168,   186,   211,  -564,  -564,    16,   111,   156,
    -564,   249,  -564,   314,  -564,   287,   183,  -564,   286,   305,
      43,   319,   333,   345,   340,  -564,  -564,  -564,  -564,   221,
    -564,  -564,   415,  -564,  1155,  -564,  -564,  -564,   205,  -564,
    -564,  -564,  -564,  -564,  -564,  -564,  -564,  -564,  -564,  -564,
    -564,  -564,  -564,  -564,  -564,  -564,   372,   396,  -564,   380,
     400,   158,   538,    18,   402,   423,  -564,  -564,  -564,  -564,
     420,   466,   287,   183,   420,   434,  -564,   444,   183,  -564,
    -564,  -564,   450,  -564,  -564,  -564,  -564,  -564,  -564,   209,
     451,   558,   501,   475,   363,   591,   441,   456,   259,  1127,
     516,  -564,   157,   372,   396,  -564,  -564,   201,   477,   131,
     476,   486,   152,   183,  -564,  -564,  -564,    83,   509,  -564,
    -564,   599,  -564,   493,  -564,   457,  -564,   591,  -564,   669,
     174,   149,   770,   174,   510,   540,  -564,   789,   183,    47,
     549,  -564,  -564,   565,   522,    23,    37,   512,   796,   183,
     518,   796,   530,   183,   468,   532,  1127,  -564,    58,   545,
     908,  -564,  -564,  -564,  -564,  -564,  -564,  -564,  -564,  -564,
    -564,   424,  -564,  -564,  -564,  -564,  -564,  -564,  -564,  -564,
     131,   553,  -564,   183,    74,  -564,   577,   420,   602,   420,
     595,  -564,   305,   508,   248,  -564,   183,  1180,    49,   606,
    1207,  -564,   310,  -564,   628,   614,  -564,  -564,  -564,  -564,
    1235,   183,  1235,  -564,  -564,  -564,  -564,   722,  -564,  -564,
    -564,    48,  -564,   583,     6,  -564,   601,  -564,  -564,  -564,
    -564,   289,  -564,   337,  1250,   360,  -564,   682,  -564,  -564,
    -564,  -564,  -564,  -564,  -564,  -564,  -564,  -564,  -564,  -564,
     796,   680,   600,   357,   165,  -564,  -564,   639,   714,   635,
    -564,   491,   630,   353,  -564,   646,   590,   408,  -564,   308,
     652,   520,   526,   796,   183,  -564,   658,   731,   683,  -564,
    -564,  -564,  -564,   350,   591,   691,   674,   491,   700,  -564,
     696,  -564,   686,  -564,   445,  -564,  -564,   796,  -564,   322,
    -564,   702,  -564,  -564,   702,   396,  -564,   725,  1127,   796,
     131,  -564,   415,   728,  -564,  -564,   576,   703,   460,   757,
     774,   784,  -564,    41,  -564,    83,  -564,   591,   415,   750,
    1253,   798,  -564,   157,  -564,  -564,   600,  -564,  -564,   772,
     748,   666,   751,   226,   796,   777,   796,   263,  -564,  -564,
     669,   764,   808,  -564,   796,   796,   796,  -564,  -564,  -564,
    -564,   799,  -564,  -564,  -564,  -564,  -564,  -564,   796,   796,
    1250,   360,  -564,  -564,  -564,  -564,  1250,  1235,   449,   794,
    -564,  -564,   760,   796,   769,   789,  -564,   796,  -564,  -564,
    -564,  -564,   829,    86,  -564,   172,   796,   796,  -564,   796,
     183,   695,  -564,  -564,  -564,  -564,  -564,  -564,  -564,  -564,
    -564,  -564,  -564,  -564,   328,  -564,   489,   620,   796,   802,
     796,   776,   183,  -564,   796,   779,  -564,  1127,   796,   826,
    1127,  -564,  -564,   505,  -564,   141,  -564,  -564,    44,  1216,
     820,  1064,   833,   783,   847,  -564,  -564,  -564,   874,   877,
     878,   183,   879,   880,  -564,  -564,  -564,   832,   809,  -564,
     183,   183,   104,   812,  -564,  -564,  -564,   863,   855,  -564,
     822,   817,   669,  -564,   669,  -564,   729,  -564,   491,  -564,
    -564,   491,  -564,   621,    38,   819,  -564,  -564,  -564,  -564,
    -564,   377,  -564,   377,  -564,   320,   360,  -564,  -564,  -564,
     796,   146,  -564,   491,  -564,  -564,   403,  -564,   305,  -564,
     183,  -564,   588,   403,   491,  -564,  -564,  -564,   705,  -564,
     843,  -564,  -564,  -564,  -564,  -564,  -564,   709,  -564,   711,
    -564,   699,   183,   491,  -564,   653,  -564,  -564,  1092,  -564,
     866,   873,  -564,  -564,   830,   591,    54,  -564,   891,   777,
    -564,  -564,   853,   876,  -564,  -564,   131,   890,  -564,  -564,
     848,  -564,   840,  -564,   315,   678,  -564,   591,  -564,   822,
     639,  -564,  -564,  -564,  -564,  -564,   865,   713,   796,   534,
     867,   101,  -564,  -564,    41,  -564,   796,  -564,  -564,  -564,
     695,  -564,    59,   883,   183,  -564,   796,   521,  -564,  -564,
    -564,  -564,   844,   237,  1127,   237,   856,    56,  -564,   796,
    -564,   857,   928,   881,  -564,   860,  -564,   645,   864,   871,
    -564,   228,  -564,   868,   796,  -564,   403,  -564,   882,   887,
     885,   694,   923,  -564,  -564,  -564,   183,  -564,   376,  -564,
     670,  -564,  -564,    61,   936,  -564,  -564,  1127,   833,  -564,
    -564,  -564,  -564,   875,  -564,  -564,  -564,  -564,   574,  -564,
    -564,   918,  -564,   183,   910,   245,  -564,   396,  -564,   961,
    1127,   971,  -564,   892,   796,  -564,   396,   731,  -564,  -564,
    -564,  1029,  -564,   896,    21,   897,   396,  -564,   777,    53,
    -564,  -564,    62,   956,  -564,  -564,   901,    59,  -564,  -564
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     0,   360,     0,     0,     0,     0,     0,   338,     0,
       0,   339,   336,     0,   337,   342,     2,     0,   334,     9,
     340,   341,     0,     0,     0,   285,   286,   284,     0,     0,
     280,     0,    24,     0,     1,   298,     0,   278,     0,     0,
       0,     0,     0,     0,     0,   125,   122,    11,    12,     0,
      13,    14,     0,   129,     0,   126,   128,    15,     0,   130,
      16,   131,   123,    18,   323,    20,    17,    19,   124,   390,
     391,   392,   301,   332,   334,     9,   330,   329,   293,     0,
       0,     0,     0,     0,     0,     0,   368,   361,   279,   302,
     287,     0,   303,     0,   287,     0,   333,     0,     0,   387,
     132,   141,     0,   136,   133,   134,   135,    21,   137,     0,
       0,     0,     0,    33,   132,     0,   134,     0,    26,     0,
       0,   127,   298,   331,   328,   335,    10,     0,   369,     0,
       0,     0,   294,     0,   358,   362,   359,     0,     0,   288,
     309,     0,   388,     0,   281,     0,    25,   389,   325,     0,
       0,     0,     0,     0,     0,     0,     3,     0,     0,     0,
      36,    34,   320,     0,    27,     0,     0,     0,     0,   271,
       0,     0,     0,   271,     0,   132,     0,   223,     0,     0,
       0,   217,   219,   221,   222,   224,   225,   233,   234,     9,
     235,   263,   236,   266,   226,   227,   228,   229,   230,   231,
     260,     0,   303,     0,     0,   370,     0,   287,     0,   287,
     295,   296,     0,     0,     0,   290,     0,     0,     0,   305,
       0,   120,     0,   324,     0,     0,   344,   345,   343,   148,
       0,     0,     0,   160,   111,   140,   158,     0,   192,   193,
     113,     0,   107,   110,   208,   159,     0,   143,   207,   212,
     146,   109,   169,   177,     0,   189,   197,   203,   211,   210,
     209,   157,   156,   155,   154,   153,   152,   149,   150,   151,
       0,   395,   208,     0,   177,   138,   139,   309,   132,     0,
       5,     7,     0,    48,   100,     0,     0,     0,    97,   314,
       0,   132,   134,     0,     0,   348,     0,    30,    28,    29,
      71,    72,   232,     0,   272,   273,     0,   243,   244,   240,
       0,   237,     0,   275,     0,   253,   220,     0,   300,     0,
     400,     0,   218,   267,   269,   248,   264,     0,     0,     0,
     260,   254,     0,     0,   261,   347,     0,     0,   314,     0,
       0,   372,   297,    30,   327,     0,   289,   283,   311,     0,
       0,     0,   121,     0,   298,   205,   215,   216,   204,   160,
       0,     0,   146,   109,     0,     0,     0,     0,   112,   142,
       0,   172,   173,   174,     0,     0,     0,   184,   182,   186,
     187,     0,   181,   183,   185,   194,   195,   196,     0,     0,
       0,   190,   201,   202,   199,   200,     0,     0,     0,     0,
     397,   393,     0,     0,     0,     0,    46,     0,    47,    50,
      49,    35,   101,     0,    96,     0,     0,     0,   315,     0,
       0,     0,    37,    44,    64,    38,    39,    40,    66,    67,
      41,    42,    43,    45,     0,    32,     0,     0,     0,     0,
       0,     0,     0,   247,     0,     0,   277,     0,     0,     0,
       0,   357,   276,     0,   214,     0,   349,   350,     0,     0,
       0,     0,     0,     0,     0,   299,   326,   371,     0,     0,
       0,     0,     0,     0,   381,   382,   383,     0,     0,   384,
       0,     0,     0,     0,   292,   291,   312,     0,     0,   306,
     307,     0,     0,   162,     0,   161,     0,   213,   168,   108,
     110,   109,    53,   208,     0,    57,   144,   175,   176,   170,
     171,    56,   188,   178,   179,   180,   191,   198,   206,   399,
       0,     0,   346,     8,     4,     6,    54,   102,     0,    98,
       0,   114,     0,    54,    65,    52,    63,    62,     0,    60,
       0,   313,     9,    84,    23,   322,    83,     0,    76,     0,
      80,   208,     0,    31,    22,     0,   274,   270,     0,   241,
       0,     0,   238,   356,   132,   355,     0,   353,     0,     0,
     249,   265,   257,     0,   255,   251,   260,   378,   380,   377,
     385,   376,     0,   363,   372,     0,   374,   373,   364,   307,
       0,   308,   304,   165,   166,   167,   160,     0,     0,     0,
       0,     0,    55,    51,    30,   115,     0,    69,    68,    59,
       0,    86,     0,     0,     0,    79,     0,     0,    82,    75,
      74,   321,     0,     0,     0,     0,     0,     0,   258,     0,
     259,     0,     0,     0,   375,     0,   367,     0,     0,     0,
     163,     0,   396,     0,     0,    99,    54,    61,     0,     0,
       0,     0,     0,     9,    90,     9,     0,    77,     0,    81,
     208,    78,   239,     0,     0,   354,   246,     0,     0,   262,
     379,   386,   365,     0,   310,   164,    58,   394,     0,    70,
      95,     0,     9,     0,     0,     0,    87,    88,    73,     0,
       0,     0,   256,     0,     0,     9,    89,    30,    85,    91,
       9,     0,   366,     0,     0,     0,    93,   398,     0,     0,
     104,    94,     0,     0,   105,     9,     0,     0,   103,   106
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -564,  -564,    -9,  -564,   580,   -72,  -564,  -564,  -564,    -6,
    -564,  -564,  -336,  -564,  -564,  -564,  -564,  -564,  -153,  -564,
    -564,  -564,  -230,  -490,  -337,  -564,  -564,   371,  -564,  -564,
    -564,  -564,  -232,  -564,  -564,  -563,  -564,   373,  -564,  -564,
    -535,  -564,  -564,   272,  -564,  -564,   312,   870,  -564,   582,
    -564,   317,  -564,   284,  -541,   638,  -355,   672,  -198,   671,
    -564,  -116,  -564,   965,  -564,     2,  -200,   869,   884,  -564,
     528,  -222,   -11,  -564,   872,  -564,  -564,  -564,   785,  -134,
    -564,  -564,   436,  -564,  -564,  -138,  -564,  -564,  -224,  -564,
     632,  -208,  -193,  -115,  -564,  -307,  -174,  -564,  -564,  -564,
    -564,  -564,  -564,  -564,   581,  -282,  -564,  -564,  -564,  -564,
    -564,  -564,  -564,  -564,  -564,  -442,  -313,  -564,  -564,  -564,
    -151,  -564,  -564,  -564,   859,  -564,  -564,  -564,   294,     9,
    -564,  -564,    34,   -52,  -564,  -564,  -110,  -564,  -564,    32,
    -564,   375,  1011,  -564,  -564,   448,    39,  -564,  -564,   701,
     706,  -564,    22,  -564,   421,  -564,  -564,  1017,   966,  1021,
    -564,  -564,  -564,  -564,  -564,   719,   422,   426,  -564,   471,
    -564,  -564,  -564,   469,  -564,  -564,  -564,  -564,   962,  -564,
    -564,  -564,  -564,  -564,  -564,  -564
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     7,   177,   279,   280,    77,    46,    47,    48,    49,
     166,   297,   441,    50,   160,   290,   422,    51,   619,   408,
     409,   423,   602,   618,   240,   425,   538,   539,   426,   427,
     428,   429,   299,   300,   301,   620,   547,   548,   439,   549,
     550,   431,   543,   652,   653,   686,   654,   161,   287,   288,
     528,   655,   709,   710,   241,   242,   243,   432,    52,   219,
     220,    53,    54,    55,    56,   272,   109,   245,   103,   104,
     246,   247,   105,   106,   265,   248,   249,   361,   250,   251,
     374,   375,   252,   388,   389,   274,   254,   390,   255,   396,
     256,   257,   258,   259,   260,   180,   181,   182,   183,   184,
     185,   186,   187,   308,   309,   310,   449,   188,   189,   458,
     570,   190,   191,   330,   629,   331,   333,   192,   332,   459,
     120,   193,   323,   194,   305,   445,   195,   196,    57,    58,
      94,    90,   334,   138,   139,   214,    84,   212,    10,    59,
     197,    60,    13,   141,   351,   592,    61,   217,   487,   433,
     434,   117,    62,    63,    64,    16,    17,    18,    76,    19,
      20,   228,    65,    66,   324,   456,   566,   567,   198,    67,
      22,    87,   206,   483,   478,   479,    23,    24,    99,    68,
      69,    70,   400,   521,    71,   199
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      33,    45,   316,   124,   179,   282,   322,   484,    73,     9,
     500,   253,   343,   298,   368,   360,    82,   463,   273,   348,
     574,   461,   355,   281,   358,   221,     9,   215,   627,   502,
     391,    83,    11,   116,   303,    27,   603,   307,   102,    14,
     314,   376,   144,   608,    86,    45,   115,   462,   284,    11,
     440,   285,   514,   410,   296,    28,    14,   430,   364,   424,
     648,   179,   366,   110,   624,   179,   667,   317,   126,   349,
      35,   690,   715,    41,   568,   284,  -282,   133,   337,    30,
     649,   659,   294,   713,    78,    31,   149,   284,    32,    36,
     367,   708,   151,   688,   668,   143,  -282,   134,   125,   253,
     147,   546,   295,   363,   352,   650,   474,    34,   586,    37,
      29,    41,   100,   101,   569,   126,   357,   325,    29,   644,
     697,   178,    32,   708,    29,   153,   454,   154,   385,   386,
     387,    82,    45,   365,    32,   213,   398,   318,   149,   625,
     558,   365,   319,   561,   151,   125,   625,   365,   506,    32,
     222,   244,   292,   286,   225,   339,   679,   341,    32,   436,
     283,    32,   556,   207,   266,   115,   516,   712,   283,    78,
     377,   304,   378,   379,   454,   304,   600,   226,   178,   100,
     101,   464,   178,   453,   227,   153,    91,   154,   261,   518,
     563,   210,    72,   129,   530,   307,    92,   486,   286,   262,
     263,    98,   211,     1,   380,   336,    25,    26,    45,   130,
     381,    45,   131,   179,   500,    93,   564,   101,   347,    79,
     201,   108,     4,   235,   267,   101,   692,   253,   504,   222,
     498,   501,   253,   356,   221,   485,    80,    44,   511,   244,
      81,   202,   371,    32,   371,   122,   648,   100,   101,   264,
     513,   504,   382,   383,   384,   385,   386,   387,   100,   101,
     203,   571,   531,   631,    36,    88,   649,   535,   645,   523,
     594,   281,   595,   526,   372,   -92,   372,   261,   152,   546,
     502,   164,   532,   533,    37,   534,   563,   322,   262,   263,
      89,   165,   546,   153,     8,   154,   437,    41,   496,   373,
     504,   373,   607,    97,   118,   371,   553,   497,  -145,   676,
     307,     8,   100,   101,   307,   -92,   126,   664,   580,   505,
      32,   601,   415,  -208,    95,   296,  -208,   345,   604,   346,
     178,    98,   179,   416,   417,   179,   261,   372,   264,    82,
     376,    45,   377,   237,   378,   379,   179,   262,   263,   418,
     353,   419,   420,   500,   253,   482,   253,   703,   253,   222,
     691,   705,   373,  -208,   366,   107,   371,   244,   503,    36,
    -145,  -145,   244,   371,   540,    12,   380,   605,  -208,   407,
     108,   541,   381,   701,   322,   542,   599,   322,   421,    37,
     443,   515,    12,    96,   111,   149,    97,   264,   372,   150,
     149,   151,   237,   392,   367,   372,   151,   286,   112,   366,
    -208,  -208,  -208,  -208,  -208,   114,   101,   283,   393,   371,
     113,    15,   283,   373,   382,   383,   384,   385,   386,   387,
     373,   253,   689,   149,   119,   501,   401,   150,    15,   151,
     551,    44,  -316,   179,   555,  -316,   179,   326,    41,   178,
      45,   372,   178,   394,   395,   127,   149,   565,   327,   366,
     150,   371,   151,   178,   641,   371,   328,   385,   386,   387,
     612,    21,   646,   283,   415,   128,   373,   296,   504,   504,
     230,   135,   584,   585,   587,   468,   469,   413,    21,   414,
     322,   504,     1,   372,   244,   329,   244,   372,   244,   253,
     137,   418,   136,   470,   471,   371,   140,   371,   224,   179,
     678,     4,   231,   232,   233,   145,   472,   322,   373,   146,
    -317,   371,   373,  -317,   452,   661,   155,   322,   519,   148,
     149,   293,   283,   230,   150,   162,   151,   372,   163,   372,
     473,   158,   235,   100,   101,   236,   200,   313,   237,   179,
     371,   208,   179,   372,   283,   159,   504,   204,   238,   239,
     178,   209,   373,   178,   373,   231,   232,   233,   544,   216,
     253,   244,   223,   149,   501,   179,   179,   150,   373,   151,
     277,   685,   372,   687,   562,   276,   179,   344,   149,   289,
     371,   302,   150,   306,   151,   235,   100,   101,   236,  -318,
      38,   237,  -318,   126,   371,  -319,   651,   373,  -319,   311,
     696,   238,   239,   642,   606,   315,   658,   338,   660,   503,
      97,   132,   372,   704,   320,   565,   178,   565,   706,  -118,
     694,   660,   335,    39,     1,     2,   372,   156,   157,   244,
     291,   101,   340,   717,   366,   342,   229,   373,    28,   673,
     218,    41,  -118,     4,   354,   466,   149,   230,   283,   350,
     150,   373,   151,    42,  -147,  -147,   178,    43,    44,   178,
     229,   149,    97,   412,    32,   150,   126,   151,   126,   651,
     149,   230,   369,   370,   319,   283,   151,   126,   397,   231,
     232,   233,   178,   178,   234,   126,   503,   126,   399,   545,
     149,   149,   402,   178,   150,   367,   151,   151,   126,   406,
     244,   651,   -54,   231,   232,   233,   404,   405,   234,   235,
     100,   101,   236,   229,   403,   237,   366,   411,   447,   448,
     229,   435,   621,   149,   230,   238,   239,   150,   438,   151,
     440,   230,   442,   235,   100,   101,   236,   493,   494,   237,
     149,   -54,   -54,   446,   367,   617,   151,   636,   637,   238,
     239,   444,   150,   450,   151,   451,   231,   232,   359,   536,
     537,   234,   455,   231,   232,   596,    97,   683,   234,   149,
     -54,   -54,   230,   367,   467,   151,   609,   610,   270,   230,
     613,   614,   615,   616,   640,   370,   235,   100,   101,   236,
     460,   230,   237,   235,   100,   101,   236,   465,   230,   237,
     509,   510,   238,   239,   231,   232,   233,   480,   481,   238,
     239,   231,   232,   233,   482,   488,   234,   271,   490,   491,
     492,   507,   495,   231,   232,   233,   508,   520,   512,   522,
     231,   232,   233,   527,   235,   100,   101,   236,   524,   552,
     237,   235,   100,   101,   236,   554,   560,   237,   557,   572,
     238,   239,   575,   235,   278,   101,   236,   238,   239,   237,
     235,   100,   101,   236,   167,   328,   237,   576,   577,   238,
     239,   578,   579,   581,   582,   541,   238,   239,   583,     1,
       2,   588,  -252,   589,   168,   590,  -252,   591,   593,   598,
     611,  -242,  -242,  -242,   622,     3,   169,  -252,     4,   167,
     170,   171,   626,   623,   628,  -252,   632,     5,   630,   172,
     633,   634,   639,   662,   643,    41,     6,  -252,   173,   168,
     656,  -252,   670,   174,   671,   666,   669,   167,  -268,   672,
     321,   169,  -252,   674,  -252,   170,   171,   677,   175,   101,
    -252,   176,   675,   684,   172,  -252,   693,   168,   695,  -252,
      41,   680,   681,   173,   682,   661,  -351,   698,   174,   169,
    -252,   702,   167,   170,   171,   707,   711,   716,  -252,  -252,
     718,   647,   172,   175,   101,   525,   176,   657,    41,   719,
    -252,   173,   168,   714,  -252,   529,   174,   699,   205,     1,
       2,  -250,   700,   499,   169,  -252,  -351,  -252,   170,   171,
     475,   175,   101,  -252,   176,     3,    41,   172,     4,   121,
     268,   489,   362,    41,   597,   275,   173,     5,   517,   559,
     167,   174,   312,    85,    74,   269,     6,   638,    75,   476,
     123,  -250,  -252,   457,   477,   663,   175,   101,  -252,   176,
     168,   665,  -252,   635,   142,     0,     0,     0,     0,  -352,
       0,     0,   169,  -252,     0,   167,   170,   171,     0,     0,
       0,  -252,     0,     0,     0,   172,     0,     0,     0,     0,
       0,    41,     0,  -252,   173,   168,     0,  -252,     0,   174,
       0,     0,     0,   167,   573,     0,     0,   169,  -252,  -352,
    -252,   170,   171,     0,   175,   101,  -252,   176,     0,     0,
     172,  -252,     0,   168,     0,  -252,    41,     0,     0,   173,
       0,     0,  -245,     0,   174,   169,  -252,     0,   167,   170,
     171,     0,     0,     0,  -252,  -252,     0,     0,   172,   175,
     101,     0,   176,     0,    41,     0,  -252,   173,   168,     0,
    -252,     0,   174,     0,     0,     0,    38,     0,     0,     0,
     169,  -252,     0,  -252,   170,   171,     0,   175,   101,  -252,
     176,     0,     0,   172,  -117,     0,     0,     0,     0,    41,
       0,    38,   173,     0,     0,  -117,     0,   174,     0,    39,
       1,     2,     0,     0,     0,     0,     0,     0,  -252,  -116,
       0,     0,   175,   101,     0,   176,    40,    41,    38,     4,
    -116,     0,     0,     0,    39,     1,     2,    38,     0,    42,
       0,     0,     0,    43,    44,     0,     0,     0,     0,     0,
      32,    40,    41,     0,     4,  -116,     0,  -119,     0,     0,
       0,    39,     1,     2,    42,     0,     0,     0,    43,    44,
      39,     1,     2,     0,    38,    32,     0,     0,   218,    41,
    -119,     4,   230,     0,     0,     0,     0,    40,    41,     0,
       4,    42,     0,     0,     0,    43,    44,     0,     0,   231,
      42,   233,    32,  -118,    43,    44,     0,    39,     1,     2,
       0,    32,     0,     0,   231,   232,   233,     0,     0,     0,
       0,     0,     0,     0,   218,    41,     0,     4,     0,   235,
     100,   101,   236,     0,     0,   237,     0,    42,     0,     0,
       0,    43,    44,     0,   235,   100,   101,   236,    32,     0,
     237
};

static const yytype_int16 yycheck[] =
{
       6,    10,   176,    75,   119,   158,   180,   343,    17,     0,
     365,   149,   212,   166,   244,   237,    22,   330,   152,   217,
     462,   328,   230,   157,   232,   141,    17,   137,   569,   366,
     254,    22,     0,    44,   168,     1,   526,   171,    36,     0,
     174,     3,    94,   533,    22,    54,    44,   329,     1,    17,
       9,     4,   389,   283,    17,    20,    17,   289,    10,   289,
       1,   176,    56,    20,    10,   180,    10,     9,    77,    20,
      40,    10,    10,    52,    30,     1,    60,    59,     4,    75,
      21,   616,    59,    30,     1,    80,    80,     1,    75,    59,
      84,    70,    86,   656,   629,    93,    80,    79,    76,   237,
      98,   438,    79,   237,   220,    46,   338,     0,     4,    79,
      75,    52,    75,    76,    70,   124,   231,   189,    75,    18,
     683,   119,    75,    70,    75,    84,   319,    86,    90,    91,
      92,   137,   141,    85,    75,   133,   270,    79,    80,    85,
     447,    85,    84,   450,    86,   123,    85,    85,   370,    75,
     141,   149,   163,   159,   145,   207,   646,   209,    75,   293,
     158,    75,   444,   129,    15,   163,   390,   708,   166,     1,
       5,   169,     7,     8,   367,   173,    30,   145,   176,    75,
      76,   332,   180,   317,   145,    84,    75,    86,    14,   397,
      49,    39,    79,    35,    22,   329,    40,   348,   204,    25,
      26,    44,    50,    35,    39,   203,    75,    76,   217,    51,
      45,   220,    54,   328,   569,    59,    75,    76,   216,    51,
      63,    75,    54,    74,    75,    76,   668,   365,   366,   220,
     364,   365,   370,   231,   350,   345,    68,    69,   376,   237,
      72,    40,    16,    75,    16,    40,     1,    75,    76,    75,
     388,   389,    87,    88,    89,    90,    91,    92,    75,    76,
      59,   459,   415,   576,    59,    79,    21,   420,   604,   403,
     492,   405,   494,   407,    48,    30,    48,    14,    69,   616,
     617,    22,   416,   417,    79,   419,    49,   461,    25,    26,
      79,    32,   629,    84,     0,    86,   294,    52,    72,    73,
     438,    73,   532,    82,    83,    16,   440,    81,    82,    81,
     444,    17,    75,    76,   448,    70,   325,   624,   471,    56,
      75,   521,    14,     3,    75,    17,     6,    79,   528,    81,
     328,    44,   447,    25,    26,   450,    14,    48,    75,   345,
       3,   350,     5,    80,     7,     8,   461,    25,    26,    41,
      40,    43,    44,   708,   492,    40,   494,   694,   496,   350,
     667,   697,    73,    43,    56,    79,    16,   365,   366,    59,
      81,    82,   370,    16,    46,     0,    39,   530,    58,    26,
      75,    53,    45,   690,   558,    57,   520,   561,    80,    79,
      40,   389,    17,    79,    75,    80,    82,    75,    48,    84,
      80,    86,    80,    43,    84,    48,    86,   413,    75,    56,
      90,    91,    92,    93,    94,    75,    76,   415,    58,    16,
      75,     0,   420,    73,    87,    88,    89,    90,    91,    92,
      73,   569,    56,    80,    19,   569,    79,    84,    17,    86,
     438,    69,    79,   558,   442,    82,   561,    23,    52,   447,
     459,    48,   450,    93,    94,    75,    80,   455,    34,    56,
      84,    16,    86,   461,   598,    16,    42,    90,    91,    92,
     542,     0,   606,   471,    14,    75,    73,    17,   616,   617,
      12,    79,   480,   481,   482,    25,    26,    79,    17,    81,
     664,   629,    35,    48,   492,    71,   494,    48,   496,   637,
      80,    41,    79,    43,    44,    16,    40,    16,    51,   624,
     644,    54,    44,    45,    46,    81,    56,   691,    73,    75,
      79,    16,    73,    82,    79,     4,    75,   701,    79,    79,
      80,     9,   530,    12,    84,    79,    86,    48,    82,    48,
      80,    40,    74,    75,    76,    77,    30,    79,    80,   664,
      16,    75,   667,    48,   552,    80,   694,    80,    90,    91,
     558,    75,    73,   561,    73,    44,    45,    46,    79,    60,
     708,   569,    79,    80,   708,   690,   691,    84,    73,    86,
      40,   653,    48,   655,    79,    75,   701,    79,    80,    40,
      16,    79,    84,    75,    86,    74,    75,    76,    77,    79,
       1,    80,    82,   612,    16,    79,   612,    73,    82,    79,
     682,    90,    91,    79,    26,    83,   614,    40,   616,   617,
      82,    83,    48,   695,    79,   623,   624,   625,   700,    30,
      56,   629,    79,    34,    35,    36,    48,    79,    80,   637,
      75,    76,    40,   715,    56,    50,     1,    73,    20,     4,
      51,    52,    53,    54,    40,    79,    80,    12,   656,    53,
      84,    73,    86,    64,    81,    82,   664,    68,    69,   667,
       1,    80,    82,    83,    75,    84,   685,    86,   687,   685,
      80,    12,    81,    82,    84,   683,    86,   696,     6,    44,
      45,    46,   690,   691,    49,   704,   694,   706,    18,    79,
      80,    80,    63,   701,    84,    84,    86,    86,   717,    79,
     708,   717,    42,    44,    45,    46,    81,    82,    49,    74,
      75,    76,    77,     1,    10,    80,    56,    81,    28,    29,
       1,    79,    79,    80,    12,    90,    91,    84,    80,    86,
       9,    12,    59,    74,    75,    76,    77,    81,    82,    80,
      80,    81,    82,    79,    84,    56,    86,    79,    80,    90,
      91,    70,    84,    67,    86,    79,    44,    45,    46,    74,
      75,    49,    70,    44,    45,    46,    82,    83,    49,    80,
      81,    82,    12,    84,    81,    86,    81,    82,    18,    12,
      81,    82,    81,    82,    81,    82,    74,    75,    76,    77,
      75,    12,    80,    74,    75,    76,    77,    79,    12,    80,
     374,   375,    90,    91,    44,    45,    46,    60,    44,    90,
      91,    44,    45,    46,    40,    75,    49,    57,    30,    57,
      82,    67,    81,    44,    45,    46,    28,    43,    39,    79,
      44,    45,    46,    14,    74,    75,    76,    77,    79,    47,
      80,    74,    75,    76,    77,    79,    30,    80,    79,    39,
      90,    91,    79,    74,    75,    76,    77,    90,    91,    80,
      74,    75,    76,    77,     1,    42,    80,    30,     4,    90,
      91,     4,     4,     4,     4,    53,    90,    91,    79,    35,
      36,    79,    19,    30,    21,    40,    23,    75,    81,    80,
      57,    28,    29,    30,    38,    51,    33,    34,    54,     1,
      37,    38,    21,    83,    61,    42,    26,    63,    42,    46,
      72,    81,    57,    79,    57,    52,    72,    19,    55,    21,
      47,    23,     4,    60,    53,    79,    79,     1,    30,    79,
      32,    33,    34,    79,    71,    37,    38,    79,    75,    76,
      42,    78,    81,    30,    46,    19,    81,    21,    40,    23,
      52,    79,    75,    55,    79,     4,    30,    57,    60,    33,
      34,    79,     1,    37,    38,    79,    79,    21,    42,    71,
      79,   610,    46,    75,    76,   405,    78,   614,    52,   717,
      19,    55,    21,   709,    23,   413,    60,   685,   128,    35,
      36,    30,   685,   365,    33,    34,    70,    71,    37,    38,
     338,    75,    76,    42,    78,    51,    52,    46,    54,    54,
     151,   350,   237,    52,   496,   153,    55,    63,   396,   448,
       1,    60,   173,    22,    17,   151,    72,   589,    17,   338,
      74,    70,    71,   324,   338,   623,    75,    76,    19,    78,
      21,   625,    23,   584,    92,    -1,    -1,    -1,    -1,    30,
      -1,    -1,    33,    34,    -1,     1,    37,    38,    -1,    -1,
      -1,    42,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      -1,    52,    -1,    19,    55,    21,    -1,    23,    -1,    60,
      -1,    -1,    -1,     1,    30,    -1,    -1,    33,    34,    70,
      71,    37,    38,    -1,    75,    76,    42,    78,    -1,    -1,
      46,    19,    -1,    21,    -1,    23,    52,    -1,    -1,    55,
      -1,    -1,    30,    -1,    60,    33,    34,    -1,     1,    37,
      38,    -1,    -1,    -1,    42,    71,    -1,    -1,    46,    75,
      76,    -1,    78,    -1,    52,    -1,    19,    55,    21,    -1,
      23,    -1,    60,    -1,    -1,    -1,     1,    -1,    -1,    -1,
      33,    34,    -1,    71,    37,    38,    -1,    75,    76,    42,
      78,    -1,    -1,    46,    19,    -1,    -1,    -1,    -1,    52,
      -1,     1,    55,    -1,    -1,    30,    -1,    60,    -1,    34,
      35,    36,    -1,    -1,    -1,    -1,    -1,    -1,    71,    19,
      -1,    -1,    75,    76,    -1,    78,    51,    52,     1,    54,
      30,    -1,    -1,    -1,    34,    35,    36,     1,    -1,    64,
      -1,    -1,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,
      75,    51,    52,    -1,    54,    19,    -1,    30,    -1,    -1,
      -1,    34,    35,    36,    64,    -1,    -1,    -1,    68,    69,
      34,    35,    36,    -1,     1,    75,    -1,    -1,    51,    52,
      53,    54,    12,    -1,    -1,    -1,    -1,    51,    52,    -1,
      54,    64,    -1,    -1,    -1,    68,    69,    -1,    -1,    44,
      64,    46,    75,    30,    68,    69,    -1,    34,    35,    36,
      -1,    75,    -1,    -1,    44,    45,    46,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    51,    52,    -1,    54,    -1,    74,
      75,    76,    77,    -1,    -1,    80,    -1,    64,    -1,    -1,
      -1,    68,    69,    -1,    74,    75,    76,    77,    75,    -1,
      80
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    35,    36,    51,    54,    63,    72,    96,   223,   224,
     233,   234,   236,   237,   241,   249,   250,   251,   252,   254,
     255,   264,   265,   271,   272,    75,    76,   227,    20,    75,
      75,    80,    75,   104,     0,    40,    59,    79,     1,    34,
      51,    52,    64,    68,    69,    97,   101,   102,   103,   104,
     108,   112,   153,   156,   157,   158,   159,   223,   224,   234,
     236,   241,   247,   248,   249,   257,   258,   264,   274,   275,
     276,   279,    79,    97,   252,   254,   253,   100,     1,    51,
      68,    72,   104,   224,   231,   237,   247,   266,    79,    79,
     226,    75,    40,    59,   225,    75,    79,    82,    44,   273,
      75,    76,   160,   163,   164,   167,   168,    79,    75,   161,
      20,    75,    75,    75,    75,   160,   167,   246,    83,    19,
     215,   158,    40,   253,   100,   247,    97,    75,    75,    35,
      51,    54,    83,    59,    79,    79,    79,    80,   228,   229,
      40,   238,   273,   160,   228,    81,    75,   160,    79,    80,
      84,    86,    69,    84,    86,    75,    79,    80,    40,    80,
     109,   142,    79,    82,    22,    32,   105,     1,    21,    33,
      37,    38,    46,    55,    60,    75,    78,    97,   160,   188,
     190,   191,   192,   193,   194,   195,   196,   197,   202,   203,
     206,   207,   212,   216,   218,   221,   222,   235,   263,   280,
      30,    63,    40,    59,    80,   142,   267,   227,    75,    75,
      39,    50,   232,   160,   230,   231,    60,   242,    51,   154,
     155,   156,   224,    79,    51,   224,   234,   241,   256,     1,
      12,    44,    45,    46,    49,    74,    77,    80,    90,    91,
     119,   149,   150,   151,   160,   162,   165,   166,   170,   171,
     173,   174,   177,   180,   181,   183,   185,   186,   187,   188,
     189,    14,    25,    26,    75,   169,    15,    75,   162,   163,
      18,    57,   160,   174,   180,   169,    75,    40,    75,    98,
      99,   174,   113,   160,     1,     4,   104,   143,   144,    40,
     110,    75,   167,     9,    59,    79,    17,   106,   113,   127,
     128,   129,    79,   174,   160,   219,    75,   174,   198,   199,
     200,    79,   219,    79,   174,    83,   191,     9,    79,    84,
      79,    32,   191,   217,   259,   100,    23,    34,    42,    71,
     208,   210,   213,   211,   227,    79,   160,     4,    40,   228,
      40,   228,    50,   161,    79,    79,    81,   160,   153,    20,
      53,   239,   156,    40,    40,   186,   160,   188,   186,    46,
     166,   172,   173,   174,    10,    85,    56,    84,   117,    81,
      82,    16,    48,    73,   175,   176,     3,     5,     7,     8,
      39,    45,    87,    88,    89,    90,    91,    92,   178,   179,
     182,   183,    43,    58,    93,    94,   184,     6,   174,    18,
     277,    79,    63,    10,    81,    82,    79,    26,   114,   115,
     117,    81,    83,    79,    81,    14,    25,    26,    41,    43,
      44,    80,   111,   116,   117,   120,   123,   124,   125,   126,
     127,   136,   152,   244,   245,    79,   174,   160,    80,   133,
       9,   107,    59,    40,    70,   220,    79,    28,    29,   201,
      67,    79,    79,   174,   187,    70,   260,   260,   204,   214,
      75,   190,   200,   211,   215,    79,    79,    81,    25,    26,
      43,    44,    56,    80,   127,   152,   244,   245,   269,   270,
      60,    44,    40,   268,   107,   231,   215,   243,    75,   154,
      30,    57,    82,    81,    82,    81,    72,    81,   174,   150,
     151,   174,   119,   160,   180,    56,   166,    67,    28,   177,
     177,   180,    39,   180,   119,   160,   183,   185,   186,    79,
      43,   278,    79,   174,    79,    99,   174,    14,   145,   144,
      22,   113,   174,   174,   174,   113,    74,    75,   121,   122,
      46,    53,    57,   137,    79,    79,   119,   131,   132,   134,
     135,   160,    47,   174,    79,   160,   200,    79,   190,   199,
      30,   190,    79,    49,    75,   160,   261,   262,    30,    70,
     205,   153,    39,    30,   210,    79,    30,     4,     4,     4,
     113,     4,     4,    79,   160,   160,     4,   160,    79,    30,
      40,    75,   240,    81,   166,   166,    46,   165,    80,   174,
      30,   161,   117,   118,   161,   113,    26,   117,   118,    81,
      82,    57,   100,    81,    82,    81,    82,    56,   118,   113,
     130,    79,    38,    83,    10,    85,    21,   149,    61,   209,
      42,   211,    26,    72,    81,   268,    79,    80,   240,    57,
      81,   174,    79,    57,    18,   107,   174,   122,     1,    21,
      46,   104,   138,   139,   141,   146,    47,   132,   160,   135,
     160,     4,    79,   261,   190,   262,    79,    10,   135,    79,
       4,    53,    79,     4,    79,    81,    81,    79,   174,   118,
      79,    75,    79,    83,    30,   100,   140,   100,   130,    56,
      10,   190,   210,    81,    56,    40,   100,   130,    57,   141,
     146,   190,    79,   119,   100,   107,   100,    79,    70,   147,
     148,    79,   149,    30,   148,    10,    21,   100,    79,   138
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
     211,   211,   212,   213,   214,   213,   215,   216,   217,   217,
     218,   219,   219,   220,   220,   221,   221,   222,   223,   223,
     225,   224,   226,   224,   224,   227,   227,   228,   228,   229,
     230,   230,   231,   231,   232,   232,   232,   232,   233,   234,
     235,   236,   236,   238,   237,   239,   239,   240,   240,   242,
     241,   243,   243,   244,   245,   245,   246,   246,   246,   246,
     247,   248,   248,   248,   249,   249,   249,   249,   250,   250,
     251,   251,   251,   252,   253,   253,   254,   254,   254,   254,
     254,   254,   254,   255,   256,   256,   257,   257,   258,   259,
     259,   260,   260,   261,   261,   262,   262,   263,   264,   264,
     265,   265,   266,   266,   266,   266,   266,   266,   266,   267,
     267,   267,   268,   268,   268,   269,   269,   269,   269,   269,
     269,   269,   269,   269,   269,   270,   270,   271,   272,   273,
     274,   274,   274,   275,   276,   277,   277,   278,   278,   279,
     280
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
       0,     1,     6,     0,     0,     3,     2,     2,     0,     1,
       4,     0,     1,     0,     2,     2,     3,     3,     2,     2,
       0,     4,     0,     6,     2,     1,     1,     0,     1,     3,
       1,     3,     5,     1,     0,     1,     1,     2,     2,     6,
       2,     2,     2,     0,     8,     0,     2,     0,     1,     0,
      10,     0,     1,     2,     0,     1,     1,     1,     3,     3,
       3,     7,     6,     1,     5,     4,     6,     5,     3,     2,
       2,     3,     2,     3,     0,     2,     1,     1,     1,     1,
       1,     1,     1,     5,     1,     1,     6,     4,     4,     2,
       2,     4,     6,     1,     3,     1,     1,     3,     3,     3,
       1,     2,     2,     6,     6,     8,    10,     7,     1,     0,
       1,     3,     0,     2,     2,     3,     2,     2,     2,     4,
       2,     1,     1,     1,     1,     2,     4,     3,     4,     2,
       1,     1,     1,     5,     9,     0,     4,     0,     7,     6,
       2
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
#line 258 "grammar83.y"
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

#line 2524 "grammar83.tab.c"

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
#line 285 "grammar83.y"
                        { context->comp_unit = (yyvsp[0].comp_unit); }
#line 2744 "grammar83.tab.c"
    break;

  case 22: /* object_decl: def_id_s ':' object_qualifier_opt object_subtype_def init_opt ';'  */
#line 323 "grammar83.y"
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
#line 2775 "grammar83.tab.c"
    break;

  case 23: /* number_decl: def_id_s ':' CONSTANT IS_ASSIGNED expression ';'  */
#line 351 "grammar83.y"
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
#line 2795 "grammar83.tab.c"
    break;

  case 24: /* def_id_s: identifier  */
#line 368 "grammar83.y"
                            {
        StringTokenArray_init(&(yyval.str_token_array));
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2804 "grammar83.tab.c"
    break;

  case 25: /* def_id_s: def_id_s ',' identifier  */
#line 372 "grammar83.y"
                            {
        (yyval.str_token_array) = (yyvsp[-2].str_token_array);
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2813 "grammar83.tab.c"
    break;

  case 26: /* object_qualifier_opt: %empty  */
#line 379 "grammar83.y"
             { (yyval.bool_) = false; }
#line 2819 "grammar83.tab.c"
    break;

  case 27: /* object_qualifier_opt: CONSTANT  */
#line 380 "grammar83.y"
             { (yyval.bool_) = true; }
#line 2825 "grammar83.tab.c"
    break;

  case 30: /* init_opt: %empty  */
#line 389 "grammar83.y"
                           { (yyval.expr) = NULL; }
#line 2831 "grammar83.tab.c"
    break;

  case 31: /* init_opt: IS_ASSIGNED expression  */
#line 390 "grammar83.y"
                           { (yyval.expr) = (yyvsp[0].expr); }
#line 2837 "grammar83.tab.c"
    break;

  case 32: /* type_decl: TYPE identifier discrim_part_opt type_completion ';'  */
#line 394 "grammar83.y"
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
#line 2852 "grammar83.tab.c"
    break;

  case 37: /* type_completion: IS type_def  */
#line 414 "grammar83.y"
                { (yyval.type_decl) = (yyvsp[0].type_decl); }
#line 2858 "grammar83.tab.c"
    break;

  case 46: /* subtype_decl: SUBTYPE identifier IS subtype_ind ';'  */
#line 429 "grammar83.y"
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
#line 2877 "grammar83.tab.c"
    break;

  case 47: /* subtype_ind: name constraint  */
#line 446 "grammar83.y"
                    {
        // TODO: propagate constraint somehow
        (yyval.str_token) = (yyvsp[-1].name).name;
    }
#line 2886 "grammar83.tab.c"
    break;

  case 48: /* subtype_ind: name  */
#line 450 "grammar83.y"
         { (yyval.str_token) = (yyvsp[0].name).name; }
#line 2892 "grammar83.tab.c"
    break;

  case 52: /* derived_type: NEW subtype_ind  */
#line 463 "grammar83.y"
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
#line 474 "grammar83.y"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 2912 "grammar83.tab.c"
    break;

  case 54: /* range_constr_opt: %empty  */
#line 478 "grammar83.y"
           { (yyval.expr) = NULL; }
#line 2918 "grammar83.tab.c"
    break;

  case 56: /* range: simple_expression DOT_DOT simple_expression  */
#line 483 "grammar83.y"
                                                             { (yyval.expr) = create_binary_expr((yyvsp[-2].expr), OP_RANGE, (yyvsp[0].expr)); }
#line 2924 "grammar83.tab.c"
    break;

  case 59: /* enumeration_type: '(' enum_id_s ')'  */
#line 489 "grammar83.y"
                      {
        (yyval.type_decl) = create_type_decl(TYPE_ENUM);
        (yyval.type_decl)->u.enum_.literals = (yyvsp[-1].enum_literals).data;
        (yyval.type_decl)->u.enum_.literal_count = EnumLiteralArray_size(&(yyvsp[-1].enum_literals));
    }
#line 2934 "grammar83.tab.c"
    break;

  case 60: /* enum_id_s: enum_id  */
#line 496 "grammar83.y"
            {
        EnumLiteralArray_init(&(yyval.enum_literals));
        EnumLiteralArray_append(&(yyval.enum_literals), (yyvsp[0].enum_literal));
    }
#line 2943 "grammar83.tab.c"
    break;

  case 61: /* enum_id_s: enum_id_s ',' enum_id  */
#line 500 "grammar83.y"
                                {
        (yyval.enum_literals) = (yyvsp[-2].enum_literals);
        EnumLiteralArray_append(&(yyval.enum_literals), (yyvsp[0].enum_literal));
    }
#line 2952 "grammar83.tab.c"
    break;

  case 62: /* enum_id: identifier  */
#line 506 "grammar83.y"
               {
        clr_struct(&(yyval.enum_literal));
        (yyval.enum_literal).base.kind = DECL_ENUM_LIT;
        (yyval.enum_literal).base.loc = (yyloc);
        (yyval.enum_literal).name = (yyvsp[0].str_token);
        (yyval.enum_literal).is_char_lit = false;
    }
#line 2964 "grammar83.tab.c"
    break;

  case 63: /* enum_id: char_lit  */
#line 513 "grammar83.y"
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
#line 2981 "grammar83.tab.c"
    break;

  case 64: /* integer_type: range_constraint  */
#line 527 "grammar83.y"
                     {
        (yyval.type_decl) = create_type_decl(TYPE_INTEGER);
        (yyval.type_decl)->u.int_.range = (yyvsp[0].expr);
    }
#line 2990 "grammar83.tab.c"
    break;

  case 107: /* choice_s: choice  */
#line 651 "grammar83.y"
                        {
        ChoiceArray_init(&(yyval.choice_array));
        ChoiceArray_append(&(yyval.choice_array), (yyvsp[0].choice));
    }
#line 2999 "grammar83.tab.c"
    break;

  case 108: /* choice_s: choice_s '|' choice  */
#line 655 "grammar83.y"
                              {
        (yyval.choice_array) = (yyvsp[-2].choice_array);
        ChoiceArray_append(&(yyval.choice_array), (yyvsp[0].choice));
    }
#line 3008 "grammar83.tab.c"
    break;

  case 109: /* choice: expression  */
#line 661 "grammar83.y"
                         {
        (yyval.choice).kind = CHOICE_EXPR;
        (yyval.choice).u.expr = (yyvsp[0].expr);
    }
#line 3017 "grammar83.tab.c"
    break;

  case 111: /* choice: OTHERS  */
#line 666 "grammar83.y"
                         { (yyval.choice).kind = CHOICE_OTHERS; }
#line 3023 "grammar83.tab.c"
    break;

  case 116: /* decl_part: %empty  */
#line 680 "grammar83.y"
                         { (yyval.decl) = NULL; }
#line 3029 "grammar83.tab.c"
    break;

  case 118: /* decl_item_s: %empty  */
#line 685 "grammar83.y"
                 { (yyval.decl) = NULL; }
#line 3035 "grammar83.tab.c"
    break;

  case 121: /* decl_item_s1: decl_item_s1 decl_item  */
#line 691 "grammar83.y"
                           { (yyval.decl) = (yyvsp[-1].decl); }
#line 3041 "grammar83.tab.c"
    break;

  case 127: /* decl_item_or_body_s1: decl_item_or_body_s1 decl_item_or_body  */
#line 702 "grammar83.y"
                                           { (yyval.decl) = (yyvsp[-1].decl); }
#line 3047 "grammar83.tab.c"
    break;

  case 130: /* body: subprog_body  */
#line 710 "grammar83.y"
                 { (yyval.decl) = &(yyvsp[0].subprogram_decl)->base; }
#line 3053 "grammar83.tab.c"
    break;

  case 131: /* body: pkg_body  */
#line 711 "grammar83.y"
                 { (yyval.decl) = &(yyvsp[0].pkg_body)->base; }
#line 3059 "grammar83.tab.c"
    break;

  case 132: /* name: identifier  */
#line 718 "grammar83.y"
               {
        clr_struct(&(yyval.name));
        (yyval.name).name = (yyvsp[0].str_token);
    }
#line 3068 "grammar83.tab.c"
    break;

  case 136: /* name: operator_symbol  */
#line 725 "grammar83.y"
                    {
        clr_struct(&(yyval.name));
        (yyval.name).name = (yyvsp[0].str_token);
        //TODO: lookup operator, determine its arity, and allocate args array
    }
#line 3078 "grammar83.tab.c"
    break;

  case 140: /* used_char: char_lit  */
#line 738 "grammar83.y"
             {
        (yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
        (yyval.expr)->u.char_lit = (yyvsp[0].c);
    }
#line 3087 "grammar83.tab.c"
    break;

  case 158: /* literal: numeric_lit  */
#line 782 "grammar83.y"
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
#line 3111 "grammar83.tab.c"
    break;

  case 170: /* expression: expression logical relation  */
#line 824 "grammar83.y"
                                                       { (yyval.expr) = create_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3117 "grammar83.tab.c"
    break;

  case 171: /* expression: expression short_circuit relation  */
#line 825 "grammar83.y"
                                                       { (yyval.expr) = create_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3123 "grammar83.tab.c"
    break;

  case 172: /* logical: AND  */
#line 829 "grammar83.y"
        { (yyval.unary_op) = OP_AND; }
#line 3129 "grammar83.tab.c"
    break;

  case 173: /* logical: OR  */
#line 830 "grammar83.y"
        { (yyval.unary_op) = OP_OR; }
#line 3135 "grammar83.tab.c"
    break;

  case 174: /* logical: XOR  */
#line 831 "grammar83.y"
        { (yyval.unary_op) = OP_XOR; }
#line 3141 "grammar83.tab.c"
    break;

  case 175: /* short_circuit: AND THEN  */
#line 835 "grammar83.y"
               { (yyval.unary_op) = OP_AND_THEN; }
#line 3147 "grammar83.tab.c"
    break;

  case 176: /* short_circuit: OR ELSE  */
#line 836 "grammar83.y"
               { (yyval.unary_op) = OP_OR_ELSE; }
#line 3153 "grammar83.tab.c"
    break;

  case 178: /* relation: simple_expression relational simple_expression  */
#line 842 "grammar83.y"
                                                                    { (yyval.expr) = create_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3159 "grammar83.tab.c"
    break;

  case 179: /* relation: simple_expression membership range  */
#line 843 "grammar83.y"
                                                                    { (yyval.expr) = create_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3165 "grammar83.tab.c"
    break;

  case 180: /* relation: simple_expression membership name  */
#line 844 "grammar83.y"
                                                                    {
        Expression* right = create_expr(EXPR_NAME, (yylsp[0]));
        right->u.name = (yyvsp[0].name);
        (yyval.expr) = create_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), right);
    }
#line 3175 "grammar83.tab.c"
    break;

  case 181: /* relational: '='  */
#line 851 "grammar83.y"
          { (yyval.unary_op) = OP_EQ; }
#line 3181 "grammar83.tab.c"
    break;

  case 182: /* relational: NE  */
#line 852 "grammar83.y"
          { (yyval.unary_op) = OP_NEQ; }
#line 3187 "grammar83.tab.c"
    break;

  case 183: /* relational: '<'  */
#line 853 "grammar83.y"
          { (yyval.unary_op) = OP_LT; }
#line 3193 "grammar83.tab.c"
    break;

  case 184: /* relational: LT_EQ  */
#line 854 "grammar83.y"
          { (yyval.unary_op) = OP_LTE; }
#line 3199 "grammar83.tab.c"
    break;

  case 185: /* relational: '>'  */
#line 855 "grammar83.y"
          { (yyval.unary_op) = OP_GT; }
#line 3205 "grammar83.tab.c"
    break;

  case 186: /* relational: GE  */
#line 856 "grammar83.y"
          { (yyval.unary_op) = OP_GTE; }
#line 3211 "grammar83.tab.c"
    break;

  case 187: /* membership: IN  */
#line 860 "grammar83.y"
           { (yyval.unary_op) = OP_IN; }
#line 3217 "grammar83.tab.c"
    break;

  case 188: /* membership: NOT IN  */
#line 861 "grammar83.y"
           { (yyval.unary_op) = OP_NOT_IN; }
#line 3223 "grammar83.tab.c"
    break;

  case 190: /* simple_expression: unary term  */
#line 866 "grammar83.y"
                                                   { (yyval.expr) = create_unary_expr((yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3229 "grammar83.tab.c"
    break;

  case 191: /* simple_expression: simple_expression adding term  */
#line 867 "grammar83.y"
                                                   { (yyval.expr) = create_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3235 "grammar83.tab.c"
    break;

  case 192: /* unary: '+'  */
#line 871 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_PLUS; }
#line 3241 "grammar83.tab.c"
    break;

  case 193: /* unary: '-'  */
#line 872 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_MINUS; }
#line 3247 "grammar83.tab.c"
    break;

  case 194: /* adding: '+'  */
#line 876 "grammar83.y"
        { (yyval.unary_op) = OP_PLUS; }
#line 3253 "grammar83.tab.c"
    break;

  case 195: /* adding: '-'  */
#line 877 "grammar83.y"
        { (yyval.unary_op) = OP_MINUS; }
#line 3259 "grammar83.tab.c"
    break;

  case 196: /* adding: '&'  */
#line 878 "grammar83.y"
        { (yyval.unary_op) = OP_AMP; }
#line 3265 "grammar83.tab.c"
    break;

  case 198: /* term: term multiplying factor  */
#line 883 "grammar83.y"
                                             { (yyval.expr) = create_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3271 "grammar83.tab.c"
    break;

  case 199: /* multiplying: '*'  */
#line 887 "grammar83.y"
        { (yyval.unary_op) = OP_MULT; }
#line 3277 "grammar83.tab.c"
    break;

  case 200: /* multiplying: '/'  */
#line 888 "grammar83.y"
        { (yyval.unary_op) = OP_DIVIDE; }
#line 3283 "grammar83.tab.c"
    break;

  case 201: /* multiplying: MOD  */
#line 889 "grammar83.y"
        { (yyval.unary_op) = OP_MOD; }
#line 3289 "grammar83.tab.c"
    break;

  case 202: /* multiplying: REM  */
#line 890 "grammar83.y"
        { (yyval.unary_op) = OP_REM; }
#line 3295 "grammar83.tab.c"
    break;

  case 204: /* factor: NOT primary  */
#line 895 "grammar83.y"
                                       { (yyval.expr) = create_unary_expr(OP_NOT, (yyvsp[0].expr)); }
#line 3301 "grammar83.tab.c"
    break;

  case 205: /* factor: ABS primary  */
#line 896 "grammar83.y"
                                       { (yyval.expr) = create_unary_expr(OP_ABS, (yyvsp[0].expr)); }
#line 3307 "grammar83.tab.c"
    break;

  case 206: /* factor: primary EXPON primary  */
#line 897 "grammar83.y"
                                       { (yyval.expr) = create_binary_expr((yyvsp[-2].expr), OP_EXP, (yyvsp[0].expr)); }
#line 3313 "grammar83.tab.c"
    break;

  case 208: /* primary: name  */
#line 902 "grammar83.y"
         {
        (yyval.expr) = create_expr(EXPR_NAME, (yyloc));
        (yyval.expr)->u.name = (yyvsp[0].name);
    }
#line 3322 "grammar83.tab.c"
    break;

  case 213: /* parenthesized_primary: '(' expression ')'  */
#line 913 "grammar83.y"
                       { (yyval.expr) = (yyvsp[-1].expr); }
#line 3328 "grammar83.tab.c"
    break;

  case 214: /* qualified: name '\'' parenthesized_primary  */
#line 917 "grammar83.y"
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
#line 3345 "grammar83.tab.c"
    break;

  case 217: /* statement_s: statement  */
#line 936 "grammar83.y"
                          {
        clr_struct(&(yyval.stmt_list));
        StmtList_append(&(yyval.stmt_list), (yyvsp[0].stmt));
    }
#line 3354 "grammar83.tab.c"
    break;

  case 218: /* statement_s: statement_s statement  */
#line 940 "grammar83.y"
                                {
        StmtList_append(&(yyvsp[-1].stmt_list), (yyvsp[0].stmt));
        (yyval.stmt_list) = (yyvsp[-1].stmt_list);
    }
#line 3363 "grammar83.tab.c"
    break;

  case 220: /* statement: goto_label statement  */
#line 947 "grammar83.y"
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
#line 3387 "grammar83.tab.c"
    break;

  case 237: /* null_stmt: NuLL ';'  */
#line 993 "grammar83.y"
             { (yyval.stmt) = create_stmt(STMT_NULL, (yyloc)); }
#line 3393 "grammar83.tab.c"
    break;

  case 238: /* assign_stmt: name IS_ASSIGNED expression ';'  */
#line 997 "grammar83.y"
                                    {
        (yyval.stmt) = create_stmt(STMT_ASSIGN, (yyloc));
        (yyval.stmt)->u.assign.dest.kind = EXPR_NAME;
        (yyval.stmt)->u.assign.dest.loc = (yyloc);
        (yyval.stmt)->u.assign.dest.u.name = (yyvsp[-3].name);
        (yyval.stmt)->u.assign.expr = (yyvsp[-1].expr);
    }
#line 3405 "grammar83.tab.c"
    break;

  case 239: /* if_stmt: IF cond_clause_s else_opt END IF ';'  */
#line 1006 "grammar83.y"
                                         {
        (yyval.stmt) = (yyvsp[-4].stmt);
        Statement* branch = (yyvsp[-4].stmt);
        while(branch->u.if_.else_) {
            branch = branch->u.if_.else_;
            assert(branch->kind == STMT_IF);
        }
        branch->u.if_.else_ = (yyvsp[-3].stmt);
    }
#line 3419 "grammar83.tab.c"
    break;

  case 241: /* cond_clause_s: cond_clause_s ELSIF cond_clause  */
#line 1018 "grammar83.y"
                                               {
        (yyval.stmt) = (yyvsp[-2].stmt);
        (yyval.stmt)->u.if_.else_ = (yyvsp[0].stmt);
    }
#line 3428 "grammar83.tab.c"
    break;

  case 242: /* cond_clause: condition THEN statement_s  */
#line 1024 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_IF, (yyloc));
        (yyval.stmt)->u.if_.condition = (yyvsp[-2].expr);
        (yyval.stmt)->u.if_.stmts = (yyvsp[0].stmt_list).first;
    }
#line 3438 "grammar83.tab.c"
    break;

  case 244: /* else_opt: %empty  */
#line 1035 "grammar83.y"
                     { (yyval.stmt) = NULL; }
#line 3444 "grammar83.tab.c"
    break;

  case 245: /* else_opt: ELSE statement_s  */
#line 1036 "grammar83.y"
                     { (yyval.stmt) = (yyvsp[0].stmt_list).first; }
#line 3450 "grammar83.tab.c"
    break;

  case 246: /* case_stmt: case_hdr pragma_s alternative_s END CASE ';'  */
#line 1040 "grammar83.y"
                                                 {
        (yyval.stmt) = (yyvsp[-5].stmt);
        // TODO: pragmas
        (yyval.stmt)->u.case_.cases = (yyvsp[-3].case_list).first;
    }
#line 3460 "grammar83.tab.c"
    break;

  case 247: /* case_hdr: CASE expression IS  */
#line 1047 "grammar83.y"
                       {
        (yyval.stmt) = create_stmt(STMT_CASE, (yyloc));
        (yyval.stmt)->u.case_.expr = (yyvsp[-1].expr);
    }
#line 3469 "grammar83.tab.c"
    break;

  case 248: /* alternative_s: %empty  */
#line 1053 "grammar83.y"
                              { clr_struct(&(yyval.case_list)); }
#line 3475 "grammar83.tab.c"
    break;

  case 249: /* alternative_s: alternative_s alternative  */
#line 1054 "grammar83.y"
                                    {
        (yyval.case_list) = (yyvsp[-1].case_list);
        AltList_append(&(yyval.case_list), (yyvsp[0].case_));
    }
#line 3484 "grammar83.tab.c"
    break;

  case 250: /* alternative: WHEN choice_s RIGHT_SHAFT statement_s  */
#line 1060 "grammar83.y"
                                          {
        (yyval.case_) = calloc(1, sizeof(Alternative));
        (yyval.case_)->choices.choices = (yyvsp[-2].choice_array).data;
        (yyval.case_)->choices.count = ChoiceArray_size(&(yyvsp[-2].choice_array));
        (yyval.case_)->stmts = (yyvsp[0].stmt_list).first;
    }
#line 3495 "grammar83.tab.c"
    break;

  case 251: /* loop_stmt: label_opt loop_content id_opt ';'  */
#line 1069 "grammar83.y"
                                      { (yyval.stmt) = (yyvsp[-2].stmt); }
#line 3501 "grammar83.tab.c"
    break;

  case 254: /* loop_content: basic_loop  */
#line 1078 "grammar83.y"
               {
        (yyval.stmt) = create_stmt(STMT_WHILE, (yyloc));
        // Create condition so this becomes a 'while True' loop
        Expression* condition = create_expr(EXPR_ENUM_LIT, (yyloc));
        condition->u.enum_lit = &boolean_type.u.enum_.literals[true];
        (yyval.stmt)->u.while_.condition = condition;
        (yyval.stmt)->u.while_.stmts = (yyvsp[0].stmt);
    }
#line 3514 "grammar83.tab.c"
    break;

  case 255: /* loop_content: WHILE condition basic_loop  */
#line 1086 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_WHILE, (yyloc));
        (yyval.stmt)->u.while_.condition = (yyvsp[-1].expr);
        (yyval.stmt)->u.while_.stmts = (yyvsp[0].stmt);
    }
#line 3524 "grammar83.tab.c"
    break;

  case 256: /* loop_content: FOR identifier IN reverse_opt discrete_range basic_loop  */
#line 1091 "grammar83.y"
                                                            {
        (yyval.stmt) = create_stmt(STMT_FOR, (yyloc));
        (yyval.stmt)->u.for_.var = create_object_decl((yyvsp[-4].str_token), (yylsp[-4]));
        (yyval.stmt)->u.for_.reverse = (yyvsp[-2].bool_);
        (yyval.stmt)->u.for_.range = (yyvsp[-1].expr);
        (yyval.stmt)->u.for_.stmts = (yyvsp[0].stmt);
    }
#line 3536 "grammar83.tab.c"
    break;

  case 257: /* reverse_opt: %empty  */
#line 1100 "grammar83.y"
            { (yyval.bool_) = false; }
#line 3542 "grammar83.tab.c"
    break;

  case 258: /* reverse_opt: REVERSE  */
#line 1101 "grammar83.y"
            { (yyval.bool_) = true; }
#line 3548 "grammar83.tab.c"
    break;

  case 259: /* basic_loop: LOOP statement_s END LOOP  */
#line 1105 "grammar83.y"
                              { (yyval.stmt) = (yyvsp[-2].stmt_list).first; }
#line 3554 "grammar83.tab.c"
    break;

  case 262: /* block: label_opt block_decl block_body END id_opt ';'  */
#line 1115 "grammar83.y"
                                                   {
        (yyval.stmt) = create_stmt(STMT_BLOCK, (yyloc));
        (yyval.stmt)->u.block.decls = (yyvsp[-4].decl);
        (yyval.stmt)->u.block.stmts = (yyvsp[-3].stmt);
        // Close scope if there was a declaration section
        if((yyvsp[-4].decl)) {
            end_scope(context, (yylsp[-2]));
        }
    }
#line 3568 "grammar83.tab.c"
    break;

  case 263: /* block_decl: %empty  */
#line 1126 "grammar83.y"
                                                          { (yyval.decl) = NULL; }
#line 3574 "grammar83.tab.c"
    break;

  case 264: /* $@1: %empty  */
#line 1127 "grammar83.y"
            { begin_scope(context, (yylsp[0])); }
#line 3580 "grammar83.tab.c"
    break;

  case 265: /* block_decl: DECLARE $@1 decl_part  */
#line 1127 "grammar83.y"
                                                          {
        (yyval.decl) = (yyvsp[0].decl);
        // Close scope if no declaration section
        if(!(yyval.decl)) {
            end_scope(context, (yylsp[-2]));
        }
    }
#line 3592 "grammar83.tab.c"
    break;

  case 266: /* block_body: BEGiN handled_stmt_s  */
#line 1136 "grammar83.y"
                         { (yyval.stmt) = (yyvsp[0].stmt); }
#line 3598 "grammar83.tab.c"
    break;

  case 267: /* handled_stmt_s: statement_s except_handler_part_opt  */
#line 1141 "grammar83.y"
                                        { (yyval.stmt) = (yyvsp[-1].stmt_list).first; }
#line 3604 "grammar83.tab.c"
    break;

  case 270: /* exit_stmt: EXIT name_opt when_opt ';'  */
#line 1150 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_EXIT, (yyloc));
        // TODO: name_opt
        (yyval.stmt)->u.exit.condition = (yyvsp[-1].expr);
    }
#line 3614 "grammar83.tab.c"
    break;

  case 273: /* when_opt: %empty  */
#line 1162 "grammar83.y"
                   { (yyval.expr) = NULL; }
#line 3620 "grammar83.tab.c"
    break;

  case 274: /* when_opt: WHEN condition  */
#line 1163 "grammar83.y"
                   { (yyval.expr) = (yyvsp[0].expr); }
#line 3626 "grammar83.tab.c"
    break;

  case 275: /* return_stmt: RETURN ';'  */
#line 1167 "grammar83.y"
                          { (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc)); }
#line 3632 "grammar83.tab.c"
    break;

  case 276: /* return_stmt: RETURN expression ';'  */
#line 1168 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc));
        (yyval.stmt)->u.return_.expr = (yyvsp[-1].expr);
    }
#line 3641 "grammar83.tab.c"
    break;

  case 277: /* goto_stmt: GOTO identifier ';'  */
#line 1174 "grammar83.y"
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
#line 3665 "grammar83.tab.c"
    break;

  case 278: /* subprog_decl: subprog_spec ';'  */
#line 1195 "grammar83.y"
                          {
        (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl);
        end_scope(context, (yylsp[0]));
    }
#line 3674 "grammar83.tab.c"
    break;

  case 280: /* @2: %empty  */
#line 1204 "grammar83.y"
                                          {
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
        push_declaration(context, &(yyval.subprogram_decl)->base);
        begin_scope(context, (yylsp[0]));
    }
#line 3685 "grammar83.tab.c"
    break;

  case 281: /* subprog_spec: PROCEDURE identifier @2 formal_part_opt  */
#line 1210 "grammar83.y"
                                { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3691 "grammar83.tab.c"
    break;

  case 282: /* @3: %empty  */
#line 1211 "grammar83.y"
                                         {
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
        push_declaration(context, &(yyval.subprogram_decl)->base);
        begin_scope(context, (yylsp[0]));
    }
#line 3702 "grammar83.tab.c"
    break;

  case 283: /* subprog_spec: FUNCTION designator @3 formal_part_opt RETURN name  */
#line 1217 "grammar83.y"
                                { (yyval.subprogram_decl) = (yyvsp[-3].subprogram_decl); }
#line 3708 "grammar83.tab.c"
    break;

  case 286: /* designator: char_string  */
#line 1223 "grammar83.y"
                { (yyval.str_token) = string_pool_to_token((yyvsp[0].str)); }
#line 3714 "grammar83.tab.c"
    break;

  case 294: /* mode: %empty  */
#line 1246 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3720 "grammar83.tab.c"
    break;

  case 295: /* mode: IN  */
#line 1247 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3726 "grammar83.tab.c"
    break;

  case 296: /* mode: OUT  */
#line 1248 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_OUT; }
#line 3732 "grammar83.tab.c"
    break;

  case 297: /* mode: IN OUT  */
#line 1249 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN_OUT; }
#line 3738 "grammar83.tab.c"
    break;

  case 298: /* subprog_spec_is_push: subprog_spec IS  */
#line 1253 "grammar83.y"
                    { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3744 "grammar83.tab.c"
    break;

  case 299: /* subprog_body: subprog_spec_is_push decl_part block_body END id_opt ';'  */
#line 1259 "grammar83.y"
                                                             {
        (yyval.subprogram_decl) = (yyvsp[-5].subprogram_decl);
        (yyval.subprogram_decl)->decls = (yyvsp[-4].decl);
        (yyval.subprogram_decl)->stmts = (yyvsp[-3].stmt);
        // Close scope opened in subprog_spec
        end_scope(context, (yylsp[-2]));
    }
#line 3756 "grammar83.tab.c"
    break;

  case 300: /* procedure_call: name ';'  */
#line 1268 "grammar83.y"
             {
        (yyval.stmt) = create_stmt(STMT_EXPR, (yyloc));
        (yyval.stmt)->u.expr.kind = EXPR_NAME;
        (yyval.stmt)->u.expr.loc = (yyloc);
        (yyval.stmt)->u.expr.u.name = (yyvsp[-1].name);
    }
#line 3767 "grammar83.tab.c"
    break;

  case 301: /* pkg_decl: pkg_spec ';'  */
#line 1276 "grammar83.y"
                         { (yyval.pkg_spec) = (yyvsp[-1].pkg_spec); }
#line 3773 "grammar83.tab.c"
    break;

  case 303: /* @4: %empty  */
#line 1281 "grammar83.y"
                                    {
        begin_scope(context, (yylsp[0]));
        (yyval.pkg_spec) = calloc(1, sizeof(PackageSpec));
        (yyval.pkg_spec)->base.kind = DECL_PKG_SPEC;
        (yyval.pkg_spec)->base.loc = (yyloc);
        (yyval.pkg_spec)->name = (yyvsp[-1].str_token);
    }
#line 3785 "grammar83.tab.c"
    break;

  case 304: /* pkg_spec: PACKAGE identifier IS @4 decl_item_s private_part END identifier_opt  */
#line 1288 "grammar83.y"
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
#line 3802 "grammar83.tab.c"
    break;

  case 307: /* identifier_opt: %empty  */
#line 1307 "grammar83.y"
               { (yyval.str_token) = 0; }
#line 3808 "grammar83.tab.c"
    break;

  case 309: /* @5: %empty  */
#line 1312 "grammar83.y"
                                         {
        begin_scope(context, (yylsp[0]));
        (yyval.pkg_body) = calloc(1, sizeof(PackageBody));
        (yyval.pkg_body)->base.kind = DECL_PKG_BODY;
        (yyval.pkg_body)->base.loc = (yyloc);
        (yyval.pkg_body)->name = (yyvsp[-1].str_token);
    }
#line 3820 "grammar83.tab.c"
    break;

  case 310: /* pkg_body: PACKAGE BODY identifier IS @5 decl_part body_opt END identifier_opt ';'  */
#line 1319 "grammar83.y"
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
#line 3837 "grammar83.tab.c"
    break;

  case 316: /* use_name_s: identifier  */
#line 1348 "grammar83.y"
                                 {
        StringTokenArray_init(&(yyval.str_token_array));
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 3846 "grammar83.tab.c"
    break;

  case 318: /* use_name_s: use_name_s ',' identifier  */
#line 1353 "grammar83.y"
                                 {
        (yyval.str_token_array) = (yyvsp[-2].str_token_array);
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 3855 "grammar83.tab.c"
    break;

  case 320: /* use_clause: USE use_name_s ';'  */
#line 1361 "grammar83.y"
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
#line 3888 "grammar83.tab.c"
    break;

  case 321: /* rename_decl: def_id_s ':' object_qualifier_opt subtype_ind RENAMES name ';'  */
#line 1392 "grammar83.y"
                                                                   {
        uint32_t ident_count = StringTokenArray_size(&(yyvsp[-6].str_token_array));
        if(ident_count != 1) {
            error_print((yylsp[-6]),
                "Renames declarations must have exactly one identifier on the left-hand side of the 'renames' keyword");
            error_exit();
        }
        RenameDecl* rename_decl = calloc(1, sizeof(RenameDecl));
        rename_decl->base.kind = DECL_RENAME;
        rename_decl->base.loc = (yyloc);
        rename_decl->name = (yyvsp[-6].str_token_array).data[0];
        rename_decl->target.kind = EXPR_NAME;
        rename_decl->target.loc = (yyloc);
        rename_decl->target.u.name = (yyvsp[-1].name);
        // TODO: handle object_qualifier_opt
        // TODO: handle subtype_ind
        // TODO: check that the target is an object (or some kind of slice/expression that yields an object)
        push_declaration(context, &rename_decl->base);
    }
#line 3912 "grammar83.tab.c"
    break;

  case 328: /* comp_unit: context_spec unit pragma_s  */
#line 1423 "grammar83.y"
                               { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3918 "grammar83.tab.c"
    break;

  case 329: /* comp_unit: unit pragma_s  */
#line 1424 "grammar83.y"
                               { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3924 "grammar83.tab.c"
    break;

  case 333: /* with_clause: WITH def_id_s ';'  */
#line 1434 "grammar83.y"
                      {
        uint32_t package_count = StringTokenArray_size(&(yyvsp[-1].str_token_array));
        for(uint32_t i = 0; i < package_count; ++i) {
            const char* package_name = string_pool_to_str((yyvsp[-1].str_token_array).data[i]);
            CompilationUnit* unit = comp_manager_parse_spec(context->comp_manager, package_name, &(yyloc));
            assert(unit->kind == COMP_UNIT_PACKAGE_SPEC);
            push_declaration(context, &unit->u.package_spec->base);
        }
    }
#line 3938 "grammar83.tab.c"
    break;

  case 336: /* unit: pkg_decl  */
#line 1450 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_PACKAGE_SPEC);
        (yyval.comp_unit)->u.package_spec = (yyvsp[0].pkg_spec);
    }
#line 3947 "grammar83.tab.c"
    break;

  case 337: /* unit: pkg_body  */
#line 1454 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_PACKAGE_BODY);
        (yyval.comp_unit)->u.package_body = (yyvsp[0].pkg_body);
    }
#line 3956 "grammar83.tab.c"
    break;

  case 338: /* unit: subprog_decl  */
#line 1458 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        (yyval.comp_unit)->u.subprogram_decl = (yyvsp[0].subprogram_decl);
    }
#line 3965 "grammar83.tab.c"
    break;

  case 339: /* unit: subprog_body  */
#line 1462 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        (yyval.comp_unit)->u.subprogram_decl = (yyvsp[0].subprogram_decl);
    }
#line 3974 "grammar83.tab.c"
    break;


#line 3978 "grammar83.tab.c"

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

#line 1607 "grammar83.y"


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
