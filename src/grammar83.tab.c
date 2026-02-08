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
  YYSYMBOL_compilation = 254,              /* compilation  */
  YYSYMBOL_comp_unit = 255,                /* comp_unit  */
  YYSYMBOL_private_opt = 256,              /* private_opt  */
  YYSYMBOL_context_spec = 257,             /* context_spec  */
  YYSYMBOL_with_clause = 258,              /* with_clause  */
  YYSYMBOL_use_clause_opt = 259,           /* use_clause_opt  */
  YYSYMBOL_unit = 260,                     /* unit  */
  YYSYMBOL_subunit = 261,                  /* subunit  */
  YYSYMBOL_subunit_body = 262,             /* subunit_body  */
  YYSYMBOL_body_stub = 263,                /* body_stub  */
  YYSYMBOL_exception_decl = 264,           /* exception_decl  */
  YYSYMBOL_except_handler_part = 265,      /* except_handler_part  */
  YYSYMBOL_exception_handler = 266,        /* exception_handler  */
  YYSYMBOL_except_choice_s = 267,          /* except_choice_s  */
  YYSYMBOL_except_choice = 268,            /* except_choice  */
  YYSYMBOL_raise_stmt = 269,               /* raise_stmt  */
  YYSYMBOL_generic_decl = 270,             /* generic_decl  */
  YYSYMBOL_generic_formal_part = 271,      /* generic_formal_part  */
  YYSYMBOL_generic_formal = 272,           /* generic_formal  */
  YYSYMBOL_generic_discrim_part_opt = 273, /* generic_discrim_part_opt  */
  YYSYMBOL_subp_default = 274,             /* subp_default  */
  YYSYMBOL_generic_type_def = 275,         /* generic_type_def  */
  YYSYMBOL_generic_derived_type = 276,     /* generic_derived_type  */
  YYSYMBOL_generic_subp_inst = 277,        /* generic_subp_inst  */
  YYSYMBOL_generic_pkg_inst = 278,         /* generic_pkg_inst  */
  YYSYMBOL_generic_inst = 279,             /* generic_inst  */
  YYSYMBOL_rep_spec = 280,                 /* rep_spec  */
  YYSYMBOL_attrib_def = 281,               /* attrib_def  */
  YYSYMBOL_record_type_spec = 282,         /* record_type_spec  */
  YYSYMBOL_align_opt = 283,                /* align_opt  */
  YYSYMBOL_comp_loc_s = 284,               /* comp_loc_s  */
  YYSYMBOL_address_spec = 285,             /* address_spec  */
  YYSYMBOL_code_stmt = 286                 /* code_stmt  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;



/* Unqualified %code blocks.  */
#line 70 "grammar83.y"

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

#line 481 "grammar83.tab.c"

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
#define YYLAST   1375

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  95
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  192
/* YYNRULES -- Number of rules.  */
#define YYNRULES  409
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  727

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
       0,   222,   222,   226,   227,   231,   232,   236,   237,   241,
     242,   246,   247,   248,   249,   250,   251,   252,   253,   254,
     255,   256,   260,   284,   297,   301,   306,   307,   311,   312,
     316,   317,   321,   332,   333,   334,   339,   340,   344,   345,
     346,   347,   348,   349,   350,   351,   355,   371,   375,   379,
     380,   384,   388,   399,   403,   404,   408,   409,   410,   414,
     421,   425,   429,   433,   439,   443,   447,   448,   452,   456,
     457,   461,   462,   466,   470,   474,   478,   479,   483,   487,
     491,   492,   496,   497,   501,   505,   506,   510,   511,   512,
     516,   517,   521,   522,   526,   527,   531,   535,   536,   540,
     541,   545,   546,   550,   554,   555,   559,   563,   564,   568,
     569,   570,   574,   575,   579,   580,   584,   585,   589,   590,
     594,   595,   599,   600,   601,   602,   606,   607,   611,   612,
     616,   617,   621,   625,   626,   627,   628,   635,   636,   637,
     641,   645,   646,   650,   651,   655,   661,   665,   669,   670,
     674,   675,   676,   677,   681,   682,   683,   684,   688,   692,
     693,   694,   695,   699,   718,   719,   723,   724,   725,   726,
     727,   731,   732,   736,   740,   741,   742,   746,   747,   748,
     752,   753,   758,   759,   760,   761,   768,   769,   770,   771,
     772,   773,   777,   778,   782,   783,   784,   788,   789,   793,
     794,   795,   799,   800,   804,   805,   806,   807,   811,   812,
     813,   814,   818,   819,   823,   824,   825,   829,   830,   834,
     838,   839,   843,   844,   851,   852,   860,   861,   862,   866,
     867,   868,   869,   870,   871,   872,   873,   874,   878,   879,
     880,   881,   885,   890,   896,   900,   901,   905,   909,   913,
     917,   918,   922,   926,   930,   931,   935,   940,   944,   945,
     949,   959,   965,   974,   978,   979,   983,   987,   988,   993,
    1003,  1004,  1004,  1008,  1013,  1017,  1018,  1022,  1029,  1030,
    1034,  1035,  1039,  1040,  1046,  1070,  1071,  1075,  1075,  1081,
    1081,  1087,  1091,  1092,  1096,  1097,  1101,  1105,  1106,  1110,
    1111,  1115,  1116,  1117,  1118,  1122,  1126,  1130,  1138,  1139,
    1143,  1147,  1148,  1152,  1153,  1157,  1161,  1162,  1166,  1170,
    1171,  1175,  1179,  1180,  1184,  1185,  1186,  1190,  1191,  1192,
    1193,  1197,  1201,  1202,  1203,  1207,  1208,  1212,  1213,  1217,
    1218,  1219,  1223,  1227,  1228,  1232,  1233,  1234,  1235,  1236,
    1237,  1238,  1242,  1246,  1247,  1251,  1252,  1256,  1260,  1261,
    1265,  1266,  1270,  1271,  1275,  1276,  1280,  1284,  1285,  1289,
    1290,  1294,  1295,  1296,  1297,  1298,  1299,  1300,  1304,  1305,
    1306,  1310,  1311,  1312,  1316,  1317,  1318,  1319,  1320,  1321,
    1322,  1323,  1324,  1325,  1329,  1330,  1334,  1338,  1342,  1346,
    1347,  1348,  1352,  1356,  1360,  1361,  1365,  1366,  1370,  1374
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
  "rename_decl", "rename_unit", "renames", "compilation", "comp_unit",
  "private_opt", "context_spec", "with_clause", "use_clause_opt", "unit",
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

#define YYPACT_NINF (-610)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-362)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      21,    10,    95,  -610,   112,    18,   102,  -610,    21,  -610,
     162,  -610,   879,    28,  -610,  -610,   812,  -610,  -610,  -610,
     153,    87,   303,  -610,    45,   162,   167,  -610,   201,   447,
    -610,  -610,   172,  -610,  -610,  -610,  -610,  -610,   276,   199,
     220,  -610,   879,  -610,   234,   849,   421,   849,  -610,  -610,
    -610,  -610,   340,  -610,  -610,   499,  -610,   749,   297,  -610,
    -610,  -610,  -610,  -610,  -610,  -610,   253,  -610,   710,   515,
     431,  -610,   334,  -610,  -610,  -610,   162,  -610,   162,  -610,
    -610,   290,   162,    16,  -610,   162,   269,   421,  -610,   238,
     263,   162,    56,   274,   281,   421,  -610,  -610,  -610,  -610,
    -610,   534,  -610,  -610,   353,  -610,  1202,  -610,  -610,  -610,
     347,  -610,  -610,  -610,  -610,  -610,  -610,  -610,  -610,  -610,
    -610,  -610,  -610,  -610,  -610,    21,  -610,   162,   162,   214,
     557,     7,   330,   346,  -610,  -610,  -610,  -610,  -610,   234,
    -610,  -610,  -610,   749,  -610,  -610,  -610,   392,  -610,  -610,
      41,  -610,   587,   552,   383,   599,   388,   256,   354,   356,
     812,   508,   378,   307,   812,   411,   458,  -610,   812,   812,
    -610,  -610,  -610,  -610,   468,  -610,  -610,  -610,  -610,  -610,
    -610,   812,   812,   515,   431,  -610,  -610,  -610,  -610,   515,
     849,  -610,   153,   449,    31,   830,   440,   449,   455,   421,
    -610,   824,  -610,  -610,   416,  -610,   162,   470,   463,   824,
     312,   471,    57,  1174,   526,  -610,   211,   132,   483,   303,
     162,   162,    44,  -610,   479,  -610,  -610,    21,   491,   812,
     764,   812,   349,  -610,   508,  -610,   508,  -610,   574,  -610,
     812,  -610,  -610,   601,  -610,  -610,   422,  -610,  -610,  -610,
    -610,  -610,  -610,  -610,  -610,  -610,  -610,   253,  -610,  -610,
    -610,  -610,  -610,   478,  -610,    84,    32,   431,  -610,  -610,
      60,   527,  -610,  1227,    83,   549,  1254,  -610,   454,  -610,
    -610,  -610,   561,   824,   261,   714,   261,   162,   145,   421,
      66,   591,  -610,  -610,   421,  -610,   625,   318,   170,   562,
     812,   421,   421,   812,   568,   421,   772,   569,  1174,  -610,
      50,   579,   955,  -610,  -610,  -610,  -610,  -610,  -610,  -610,
    -610,  -610,  -610,   532,  -610,  -610,  -610,  -610,  -610,  -610,
    -610,  -610,   303,   583,  1263,   596,    92,  -610,   657,   449,
     661,   449,   642,  -610,   162,  -610,  -610,   253,  -610,  -610,
     253,  -610,   860,   619,  -610,  -610,   648,   608,   478,  -610,
     508,   321,  -610,   421,   353,   162,  1300,   680,  -610,   211,
     696,   681,  -610,  -610,  -610,   812,   717,   439,  -610,  -610,
     592,   649,   437,  -610,   662,   626,   451,  -610,   396,   673,
     824,   812,  -610,   677,   668,   754,   706,  -610,  -610,  -610,
    -610,   654,   824,   700,   701,   253,   585,  -610,  1174,   712,
    -610,   698,  -610,   492,  -610,  -610,   812,  -610,  -610,   722,
    -610,  -610,   722,    21,  -610,   721,  1174,   812,   303,   750,
    -610,   353,   733,  -610,  -610,  -610,   734,  1094,   767,   788,
     794,  -610,    27,   812,   755,  -610,  -610,    60,  -610,   824,
    -610,   807,   166,  -610,   162,  -610,   528,   799,  -610,  -610,
     766,  -610,   812,  -610,  -610,  -610,  -610,   829,    68,  -610,
     181,   812,   812,  -610,   812,   421,   645,  -610,  -610,  -610,
    -610,  -610,  -610,  -610,  -610,  -610,  -610,  -610,  -610,   268,
    -610,   531,  -610,   812,   803,   812,   774,   780,  -610,   812,
     782,  -610,  1174,   812,   837,   920,  -610,  -610,  -610,   605,
     358,  -610,  -610,    48,   447,   831,  1111,   826,   796,  -610,
     812,   843,  -610,  -610,   872,   876,   881,   421,   887,   892,
    -610,  -610,  -610,   844,   821,  -610,   421,   421,   105,   822,
    -610,   180,  -610,  -610,   162,   846,   153,  -610,  -610,   812,
      86,  -610,   658,  -610,   162,  -610,   421,  -610,   317,   658,
     253,  -610,  -610,  -610,   652,  -610,   850,  -610,  -610,  -610,
    -610,  -610,   659,  -610,   669,  -610,  1001,   421,   253,  -610,
    -610,  -610,  -610,  1139,  -610,   873,  -610,  -610,   834,   824,
      43,  -610,   891,   764,  -610,  -610,  -610,   877,  -610,  -610,
     826,  1277,   303,   894,  -610,  -610,   855,  -610,   841,  -610,
     593,   742,  -610,   824,  -610,  -610,   839,   650,   874,   137,
    -610,  -610,    27,  -610,   812,  -610,  -610,  -610,   645,  -610,
     158,   885,   421,  -610,   812,   692,  -610,  -610,  -610,   858,
     430,  1174,   430,   868,    52,  -610,  -610,   880,   930,   898,
    -610,   882,  -610,   218,  -610,  -610,   884,   812,  -610,   658,
    -610,   886,   162,   888,   679,   925,  -610,  -610,  -610,   421,
    -610,   739,  -610,  -610,  -610,    53,   983,  -610,  -610,  1174,
    -610,  -610,  -610,  -610,   883,  -610,   730,  -610,  -610,   928,
    -610,   421,   903,   196,  -610,    21,  -610,   965,  1174,  1018,
     900,   812,  -610,    21,   754,  -610,  -610,  -610,  1076,  -610,
     902,   138,   904,    21,  -610,   764,   143,  -610,  -610,    54,
     949,  -610,  -610,   907,   158,  -610,  -610
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
     332,     0,     0,     9,   337,   140,     0,     1,   334,   338,
       0,   333,     0,   337,   343,     3,     0,    10,   140,   141,
     143,     0,     0,   369,     0,     0,     0,   347,     0,     0,
     348,   345,     0,   346,   351,     9,   349,   350,     0,     0,
       0,   341,     0,   343,   339,     0,     0,     0,   165,   145,
     146,   163,     0,   197,   198,     0,     5,   213,   132,   164,
     136,   133,   134,   135,   212,   217,     7,   174,   182,     0,
     194,   202,   208,   216,   215,   214,     0,   342,     0,   293,
     292,   291,     0,     0,   287,     0,   305,     0,   285,     0,
       0,     0,     0,     0,     0,     0,    24,   125,   122,    11,
      12,     0,    13,    14,     0,   129,     0,   126,   128,    15,
       0,   130,    16,   131,   123,    18,   326,    20,    17,    19,
     124,   399,   400,   401,   308,   336,   300,     0,     0,     0,
       0,     0,     0,     0,   377,   370,   286,   309,     9,   340,
     344,   132,   210,   220,   221,   209,   153,   165,   111,   113,
       0,   107,   110,   213,     0,     0,   151,   109,   182,     0,
       0,     0,     0,     0,     0,   177,   178,   179,     0,     0,
     189,   187,   191,   192,     0,   186,   188,   190,   199,   200,
     201,     0,     0,     0,   195,   206,   207,   204,   205,     0,
       0,   142,   144,   294,     0,     0,     0,   294,     0,     0,
     396,   331,   328,    21,     0,   137,     0,     0,    33,   322,
       0,     0,    26,     0,     0,   127,   305,     0,   378,     0,
       0,     0,   301,   367,     0,   371,   368,   335,     0,     0,
       0,     0,     0,   112,     0,   167,     0,   166,     0,   218,
       0,     4,     6,     0,   148,   151,   109,   162,   161,   160,
     159,   158,   219,   157,   154,   155,   156,     8,   180,   181,
     175,   176,   193,   183,   184,   185,     0,   196,   203,   211,
       0,     0,   295,     0,     0,   311,     0,   120,     0,   397,
     327,   288,     0,   398,     0,     0,     0,     0,     0,     0,
       0,    36,    34,   321,     0,    25,    27,     0,     0,     0,
       0,   278,     0,     0,     0,   278,     0,   140,     0,   228,
       0,     0,     0,   222,   224,   226,   227,   229,   230,   238,
     239,     9,   240,   270,   241,   273,   231,   232,   233,   234,
     235,   236,   267,     0,     0,     0,     0,   379,     0,   294,
       0,   294,   302,   303,     0,   330,   170,   173,   108,   110,
     109,    53,   213,    57,   171,   172,   165,     0,    56,   147,
       0,     0,   297,     0,   316,     0,     0,     0,   121,     0,
       0,     0,   353,   354,   352,     0,   404,     0,   138,   139,
       0,     0,    48,   100,     0,     0,     0,    97,   319,     0,
     323,     0,   357,     0,     0,    30,    28,    29,    71,    72,
     237,     0,   279,   280,     0,   249,   250,   245,     0,     0,
     242,     0,   282,     0,   259,   225,     0,   307,   409,     0,
     223,   274,   276,   254,   271,     0,     0,     0,   267,   264,
     260,     0,     0,   268,   356,   329,     0,   319,     0,     0,
     381,   304,    30,     0,     0,   168,   149,     0,   296,   290,
     317,     0,     0,   312,   313,   305,     0,     0,   406,   402,
       0,    46,     0,    47,    50,    49,    35,   101,     0,    96,
       0,     0,     0,   320,     0,     0,     0,    37,    44,    64,
      38,    39,    40,    66,    67,    41,    42,    43,    45,     0,
      32,     0,   325,     0,     0,     0,     0,     0,   253,     0,
       0,   284,     0,     0,     0,     0,   248,   366,   283,     0,
       0,   358,   359,     0,     0,     0,     0,     0,     0,   265,
       0,     0,   306,   380,     0,     0,     0,     0,     0,     0,
     390,   391,   392,     0,     0,   393,     0,     0,     0,     0,
     299,     0,   169,   298,   313,     0,   314,   310,   408,     0,
       0,   355,    54,   102,     0,    98,     0,   114,     0,    54,
      65,    52,    63,    62,     0,    60,     0,   318,     9,    84,
      23,    83,     0,    76,     0,    80,   213,     0,    31,    22,
     324,   281,   277,     0,   246,     0,   243,   365,   140,   364,
       0,   362,     0,     0,   255,   272,   263,     0,   261,   257,
       0,   213,   267,   387,   389,   386,   394,   385,     0,   372,
     381,     0,   383,   382,   373,    58,     0,     0,     0,     0,
      55,    51,    30,   115,     0,    69,    68,    59,     0,    86,
       0,     0,     0,    79,     0,     0,    82,    75,    74,     0,
       0,     0,     0,     0,     0,   266,   262,     0,     0,     0,
     384,     0,   376,     0,   315,   405,     0,     0,    99,    54,
      61,     0,     0,     0,     0,     0,     9,    90,     9,     0,
      77,     0,    81,    78,   244,     0,     0,   363,   252,     0,
     269,   388,   395,   374,     0,   403,     0,    70,    95,     0,
       9,     0,     0,     0,    87,    88,    73,     0,     0,     0,
       0,     0,     9,    89,    30,    85,    91,     9,     0,   375,
       0,     0,     0,    93,   407,     0,     0,   104,    94,     0,
       0,   105,     9,     0,     0,   103,   106
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -610,  -610,     0,  -610,   811,   -31,  -610,  -610,  -610,   -36,
    -610,  -610,  -422,  -610,  -610,  -610,  -610,  -610,  -277,  -610,
    -610,  -610,  -150,  -502,  -173,  -610,  -610,   345,  -610,  -610,
    -610,  -610,  -333,  -610,  -610,  -609,  -610,   350,  -610,  -610,
    -486,  -610,  -610,   266,  -610,  -610,   284,   776,  -610,   535,
    -610,   306,  -610,   289,  -561,   770,  -225,   571,   -22,   643,
    -610,  -178,  -610,   905,  -610,   237,  -314,    23,     6,  -610,
     851,   859,  -610,   785,   -42,  -610,  -610,   726,  -610,  -610,
    -610,   966,   -15,  -610,  -610,   638,  -610,  -610,   -29,  -610,
    -610,   -20,  -610,   835,   -19,  -122,    46,  -610,  -383,  -281,
    -610,  -610,  -610,  -610,  -610,  -610,  -610,   524,  -610,  -353,
    -610,  -610,  -610,  -610,  -610,  -610,  -610,  -610,  -610,  -610,
    -479,  -413,  -610,  -610,  -610,  -287,  -610,  -610,  -610,   723,
    -610,  -610,  -610,   293,    30,  -610,  -610,    -3,  -179,  -610,
    -610,  -248,  -610,  -610,    -1,  -610,   304,   994,  -610,   490,
       2,  -610,   603,   607,     8,  -610,  -610,   311,   -77,  -610,
    -610,  1023,  -610,  1029,  1002,  1004,  -610,  -610,  -610,  -610,
    -610,   627,   407,   408,  -610,   327,  -610,  -610,  -610,   452,
    -610,  -610,  -610,  -610,   870,  -610,  -610,  -610,  -610,  -610,
    -610,  -610
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     2,   309,    55,    56,     8,    98,    99,   100,   101,
     298,   395,   496,   102,   291,   389,   477,   103,   637,   463,
     464,   478,   620,   636,   149,   480,   564,   565,   481,   482,
     483,   484,   397,   398,   399,   638,   572,   573,   494,   574,
     575,   486,   569,   665,   666,   694,   667,   292,   386,   387,
     554,   668,   716,   717,   150,   151,   152,   487,   364,   275,
     276,   105,   106,   107,   108,    57,   204,   141,    83,    21,
      59,    60,    61,   243,   244,    62,    63,   251,    64,    65,
     155,   245,   246,   168,   169,    67,   181,   182,    68,    69,
     183,    70,   189,    71,    72,    73,    74,    75,   312,   313,
     314,   315,   316,   317,   318,   319,   406,   407,   408,   409,
     504,   320,   321,   513,   594,   322,   323,   428,   429,   520,
     430,   432,   324,   431,   514,   214,   325,   421,   326,   403,
     500,   327,   328,   109,   110,   197,   193,   433,   271,   272,
     361,   132,   344,    29,   111,   329,   112,    32,   367,   547,
     113,   451,   488,   489,   114,   210,   115,   116,    89,     4,
      11,    12,    13,    14,    44,    35,    36,   374,   117,   118,
     422,   511,   590,   591,   330,   119,    38,   135,   338,   539,
     534,   535,    39,    40,   200,   120,   121,   122,   458,   550,
     123,   331
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
       3,    66,   130,   233,   125,   349,   196,   104,    17,   264,
     154,    30,   381,    41,    33,   518,    20,   277,   281,    81,
     540,   396,   362,   158,     6,   505,   142,   415,   145,    97,
     442,   420,   644,    19,   600,   240,   495,   157,   598,    58,
     252,    30,    28,   516,    33,    80,   134,    19,    84,   184,
     621,   229,   140,   641,   224,   485,   195,   626,   351,   416,
     696,   126,   679,   698,   722,    82,    87,   383,   131,   383,
     384,   273,    28,     1,   517,    87,   206,   450,   592,   296,
       1,     9,   704,   342,   192,     5,   223,  -213,   194,   297,
    -213,   198,   144,   383,   343,     7,   436,    15,   368,   191,
      10,    19,    76,   365,   530,    19,    97,   227,    19,   612,
     252,   286,    -2,   287,   205,    19,   618,    76,   593,   583,
      18,   646,   178,   179,   180,    17,   230,  -213,   642,   417,
     161,    18,   158,   217,   162,    96,   163,   230,   642,   230,
     335,    96,  -213,    96,   521,    66,   581,   140,   672,   257,
      19,   218,   263,   266,   719,   657,   277,   687,    18,   661,
     438,    18,   440,   267,   161,     9,    77,    96,   232,    78,
     163,   269,   334,   720,  -213,  -213,  -213,  -213,  -213,   662,
      18,    50,    16,    58,    10,   380,   254,   394,   277,   647,
       1,    87,   354,   557,   355,    97,   165,   661,   561,   543,
     658,   158,   266,   556,   663,   158,   545,   158,   715,   158,
       1,   358,   288,   715,   347,   350,   339,   662,    76,   146,
     393,   286,   684,   287,   420,   278,   -92,    17,   166,    19,
      45,    76,   465,    96,   130,   420,   619,    18,   479,    76,
     622,    86,    80,   340,   341,    18,    50,    85,     1,   219,
     606,   124,    76,   167,   385,   199,    18,    50,   676,   311,
      87,   615,    46,    47,    48,   220,   -92,   148,   221,   165,
     377,    96,   165,    97,   333,   247,    97,   126,   136,   623,
      88,   372,   712,   143,   373,   401,   248,   249,   405,   153,
     423,   413,    49,    18,    50,    51,   699,    19,    52,   137,
     385,   166,   420,    95,   166,    27,   278,   164,    53,    54,
     379,    22,   371,   199,   566,   708,    31,   202,   446,   497,
     571,   567,   253,    34,   201,   568,   167,   127,   238,   167,
      25,   158,   209,   165,    97,    27,   250,   239,  -150,    37,
     190,   146,   203,   624,   128,    95,    31,   571,   129,   207,
    -289,    96,    45,    34,   311,    80,   208,   240,   311,   170,
     456,   171,   172,   247,   278,   166,    97,   205,   349,    37,
    -289,   452,   213,   231,   248,   249,   491,    87,    18,    79,
      97,    49,    18,    50,    46,    47,   147,   216,    19,   148,
     167,   293,   247,   173,   294,   420,   278,   392,   153,   174,
     447,   509,   448,   248,   249,   353,    87,   587,   625,   225,
     470,   130,   405,   394,    49,    18,    50,    51,   420,   265,
      52,   471,   472,    17,   250,   226,    88,   420,   541,    52,
      53,    54,   385,   588,    50,   241,   283,   473,   165,   474,
     475,   175,   176,   177,   178,   179,   180,   552,    90,   228,
     310,    80,   231,   250,   311,   165,   558,   559,    52,   560,
     546,   571,   351,   462,   266,   234,  -116,   153,   352,   237,
     166,   153,   311,   153,   185,   153,   476,    19,   258,   587,
     578,    91,    22,    23,   405,   285,   259,   166,   405,   186,
     349,   266,   595,   231,   369,   167,    18,    50,    92,     1,
     286,    25,   287,  -150,  -150,    18,    50,   262,   165,   146,
     289,    93,   167,    87,    97,    94,    95,   161,   459,   280,
      45,   284,    96,   163,   187,   188,   382,    45,   710,   270,
     468,   390,   469,    88,   617,   382,   282,   630,   402,   404,
     166,    76,   402,   290,   165,   310,   295,   165,   311,   310,
     546,   311,    46,    47,    48,   424,   332,   148,   345,    46,
      47,    48,   311,   336,   158,   167,   425,    19,   178,   179,
     180,   508,   346,   205,   426,   146,   166,   205,   350,   166,
     159,   160,    49,    18,    50,    51,    45,   363,    52,    49,
      18,    50,    51,    90,   664,    52,    22,   153,    53,    54,
     449,   167,   366,   427,   167,   266,   266,   548,   231,   659,
     570,  -116,   370,   502,   503,    25,   211,   212,    46,    47,
     356,   165,  -116,   148,   158,    80,    91,    22,    23,   311,
      17,   388,   161,   538,   391,   693,   232,   695,   163,   211,
     222,   400,   686,    92,     1,   310,    25,   410,    49,    18,
      50,    51,   414,   166,    52,   460,    93,   664,   418,   703,
      94,    95,   434,   310,    53,    54,   165,    96,  -152,  -152,
     165,   711,   266,   161,   165,   435,   713,   284,   167,   163,
     235,   236,   359,   360,   586,   689,   158,   311,   664,   445,
     360,   724,   441,    17,   498,    17,   673,   437,   166,   443,
     350,   439,   166,    17,    45,   444,   166,   382,   211,   467,
     454,    17,   382,    17,   231,   170,    82,   171,   172,   562,
     563,   455,   311,   167,    17,   311,    45,   167,   461,   655,
     576,   167,   375,   627,   628,   457,    46,    47,    48,   310,
     631,   632,   310,   466,   311,   311,   165,   589,   493,   173,
     633,   634,   490,   310,   311,   174,   492,   601,    46,    47,
      48,   211,   691,   495,   382,    87,    49,    18,    50,    51,
     499,   376,    52,   610,   611,   613,    45,   507,   166,   506,
     501,   161,    53,    54,    45,   284,   701,   163,    49,    18,
      50,    51,   510,   382,    52,   697,   515,   175,   176,   177,
     178,   179,   180,   167,    53,    54,   260,   261,    46,    47,
      48,   519,   522,   148,   382,   523,    46,    47,    48,   161,
     310,   652,   653,   284,    45,   163,   284,   536,   163,   161,
     153,    90,   537,   162,   538,   163,   542,   544,    49,    18,
      50,    51,   549,   553,    52,   551,    49,    18,    50,    51,
     577,   412,    52,   579,    53,    54,    46,    47,    48,   580,
    -118,   582,    53,    54,    91,    22,    23,   585,   426,   671,
     596,   601,   352,   602,   199,   599,   603,   589,   310,   589,
     604,   274,     1,  -118,    25,   605,    49,    18,    50,    51,
     153,   607,    52,    46,    93,    48,   608,   567,    94,    95,
     609,   614,    53,    54,   161,    96,   382,   629,   284,   460,
     163,   639,   643,   310,    22,    23,   310,   640,   654,   645,
     648,   299,   650,    49,    18,    50,    51,   649,   382,    52,
      24,   656,   669,    25,   681,   310,   310,   674,   352,  -258,
     161,   300,    26,  -258,   232,   310,   163,   678,  -247,  -247,
    -247,   682,   153,   301,  -258,   692,   299,   302,   303,   680,
     705,   683,  -258,   685,   700,   688,   304,   690,   702,   673,
     723,   242,     1,   660,  -258,   305,   300,   706,  -258,   709,
     306,   714,   670,   718,   299,  -275,   725,   419,   301,  -258,
     726,  -258,   302,   303,   337,   307,    50,  -258,   308,   707,
     348,   304,  -258,   555,   300,   721,  -258,     1,   531,   453,
     305,   215,   378,  -360,   255,   306,   301,  -258,   156,   299,
     302,   303,   256,   357,   268,  -258,  -258,   584,   411,   304,
     307,    50,   133,   308,   616,     1,    42,  -258,   305,   300,
     532,  -258,    43,   306,   533,   139,   138,   675,  -256,   512,
     677,   301,  -258,  -360,  -258,   302,   303,   635,   307,    50,
    -258,   308,   651,     0,   304,   279,     0,     0,     0,     0,
       1,     0,     0,   305,     0,     0,     0,   299,   306,     0,
       0,   161,   -54,   -54,     0,   232,     0,   163,  -256,  -258,
       0,     0,     0,   307,    50,  -258,   308,   300,     0,  -258,
       0,     0,     0,     0,     0,     0,  -361,     0,   470,   301,
    -258,   394,   299,   302,   303,     0,     0,     0,  -258,   524,
     525,     0,   304,     0,     0,     0,     0,     0,     1,     0,
    -258,   305,   300,     0,  -258,   473,   306,   526,   527,     0,
     299,   597,     0,     0,   301,  -258,  -361,  -258,   302,   303,
     528,   307,    50,  -258,   308,     0,     0,   304,  -258,     0,
     300,     0,  -258,     1,     0,     0,   305,     0,     0,  -251,
       0,   306,   301,  -258,   529,   299,   302,   303,     0,     0,
       0,  -258,  -258,     0,     0,   304,   307,    50,     0,   308,
       0,     1,     0,  -258,   305,   300,     0,  -258,     0,   306,
       0,     0,     0,    90,     0,     0,     0,   301,  -258,     0,
    -258,   302,   303,     0,   307,    50,  -258,   308,     0,     0,
     304,  -117,     0,     0,     0,     0,     1,     0,    90,   305,
       0,     0,  -117,     0,   306,     0,    91,    22,    23,     0,
       0,     0,     0,     0,     0,  -258,  -116,     0,     0,   307,
      50,     0,   308,    92,     1,    90,    25,  -116,     0,     0,
       0,    91,    22,    23,    90,     0,    93,     0,     0,     0,
      94,    95,     0,     0,     0,     0,     0,    96,    92,     1,
       0,    25,     0,     0,  -119,     0,     0,     0,    91,    22,
      23,    93,     0,  -118,     0,    94,    95,    91,    22,    23,
       0,    90,    96,     0,     0,   274,     1,  -119,    25,     0,
       0,     0,     0,     0,   274,     1,  -118,    25,    93,   -54,
       0,     0,    94,    95,     0,     0,     0,    93,     0,    96,
    -118,    94,    95,   231,    91,    22,    23,     0,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   274,     1,     0,    25,     0,     0,   161,   -54,   -54,
       0,   232,     0,   163,    93,     0,     0,     0,    94,    95,
       0,     0,     0,     0,     0,    96
};

static const yytype_int16 yycheck[] =
{
       0,    16,    38,   153,    35,   230,    83,    29,     8,   182,
      52,    12,   289,    13,    12,   428,    10,   195,   197,    22,
     442,   298,   270,    52,     1,   408,    45,   308,    47,    29,
     344,   312,   593,    10,   520,     3,     9,    52,   517,    16,
     162,    42,    12,   426,    42,    22,    38,    24,    25,    69,
     552,    10,    44,    10,   131,   388,    40,   559,   231,     9,
     669,     1,    10,    10,    10,    20,    59,     1,    38,     1,
       4,    40,    42,    52,   427,    59,    20,   364,    30,    22,
      52,    53,   691,    39,    78,    75,    79,     3,    82,    32,
       6,    85,    46,     1,    50,     0,     4,    79,   276,    76,
      72,    78,    86,    20,   437,    82,   106,   138,    85,     4,
     232,    84,     0,    86,    91,    92,    30,    86,    70,   502,
      75,   600,    90,    91,    92,   125,    85,    43,    85,    79,
      80,    75,   161,   127,    84,    75,    86,    85,    85,    85,
     217,    75,    58,    75,   431,   160,   499,   139,   634,   164,
     127,   128,   181,   182,   715,    18,   334,   659,    75,     1,
     339,    75,   341,   183,    80,    53,    79,    75,    84,    82,
      86,   190,    40,    30,    90,    91,    92,    93,    94,    21,
      75,    76,    80,   160,    72,    40,   163,    17,   366,   602,
      52,    59,   234,   470,   236,   195,    16,     1,   475,   447,
     622,   230,   231,    22,    46,   234,    40,   236,    70,   238,
      52,   240,   206,    70,   229,   230,   219,    21,    86,     1,
     297,    84,     4,    86,   505,   195,    30,   227,    48,   206,
      12,    86,   382,    75,   270,   516,   550,    75,   388,    86,
     554,    40,   219,   220,   221,    75,    76,    80,    52,    35,
     527,    79,    86,    73,   290,    44,    75,    76,   641,   213,
      59,    81,    44,    45,    46,    51,    70,    49,    54,    16,
     285,    75,    16,   273,    63,    14,   276,     1,    79,   556,
      79,   282,   704,    46,   282,   300,    25,    26,   303,    52,
     321,   306,    74,    75,    76,    77,   679,   274,    80,    79,
     336,    48,   583,    69,    48,    12,   276,    10,    90,    91,
     287,    35,   282,    44,    46,   698,    12,    79,   360,   396,
     493,    53,    15,    12,    87,    57,    73,    51,    72,    73,
      54,   360,    95,    16,   334,    42,    75,    81,    82,    12,
       6,     1,    79,    26,    68,    69,    42,   520,    72,    75,
      60,    75,    12,    42,   308,   332,    75,     3,   312,     5,
     375,     7,     8,    14,   334,    48,   366,   344,   593,    42,
      80,   365,    19,    56,    25,    26,   391,    59,    75,    76,
     380,    74,    75,    76,    44,    45,    46,    40,   365,    49,
      73,    79,    14,    39,    82,   676,   366,    79,   161,    45,
      79,   416,    81,    25,    26,    56,    59,    49,   558,    79,
      14,   447,   427,    17,    74,    75,    76,    77,   699,   182,
      80,    25,    26,   423,    75,    79,    79,   708,   443,    80,
      90,    91,   468,    75,    76,    79,   199,    41,    16,    43,
      44,    87,    88,    89,    90,    91,    92,   462,     1,    57,
     213,   428,    56,    75,   408,    16,   471,   472,    80,   474,
     454,   634,   635,    26,   493,    82,    19,   230,   231,    81,
      48,   234,   426,   236,    43,   238,    80,   454,    67,    49,
     495,    34,    35,    36,   499,    69,    28,    48,   503,    58,
     715,   520,   514,    56,    40,    73,    75,    76,    51,    52,
      84,    54,    86,    81,    82,    75,    76,    39,    16,     1,
      40,    64,    73,    59,   514,    68,    69,    80,    79,    79,
      12,    84,    75,    86,    93,    94,   289,    12,   701,    80,
      79,   294,    81,    79,   549,   298,    81,   568,   301,   302,
      48,    86,   305,    80,    16,   308,    75,    16,   502,   312,
     544,   505,    44,    45,    46,    23,    30,    49,    79,    44,
      45,    46,   516,    80,   593,    73,    34,   544,    90,    91,
      92,    79,    81,   550,    42,     1,    48,   554,   593,    48,
      81,    82,    74,    75,    76,    77,    12,    60,    80,    74,
      75,    76,    77,     1,   630,    80,    35,   360,    90,    91,
     363,    73,    53,    71,    73,   634,   635,    79,    56,   624,
      79,    19,    51,    28,    29,    54,    82,    83,    44,    45,
      46,    16,    30,    49,   653,   602,    34,    35,    36,   583,
     630,    40,    80,    40,     9,   666,    84,   668,    86,    82,
      83,    79,   657,    51,    52,   408,    54,    79,    74,    75,
      76,    77,    83,    48,    80,    63,    64,   693,    79,   690,
      68,    69,    79,   426,    90,    91,    16,    75,    81,    82,
      16,   702,   701,    80,    16,    79,   707,    84,    73,    86,
      81,    82,    81,    82,    79,   662,   715,   641,   724,    81,
      82,   722,    50,   693,    40,   695,     4,    40,    48,    80,
     715,    40,    48,   703,    12,    57,    48,   470,    82,    83,
      30,   711,   475,   713,    56,     5,    20,     7,     8,    74,
      75,    40,   676,    73,   724,   679,    12,    73,    79,    79,
     493,    73,    18,    81,    82,    18,    44,    45,    46,   502,
      81,    82,   505,    81,   698,   699,    16,   510,    80,    39,
      81,    82,    79,   516,   708,    45,    79,   520,    44,    45,
      46,    82,    83,     9,   527,    59,    74,    75,    76,    77,
      70,    57,    80,   536,   537,   538,    12,    79,    48,    67,
      79,    80,    90,    91,    12,    84,    56,    86,    74,    75,
      76,    77,    70,   556,    80,    56,    75,    87,    88,    89,
      90,    91,    92,    73,    90,    91,   168,   169,    44,    45,
      46,    61,    79,    49,   577,    81,    44,    45,    46,    80,
     583,    79,    80,    84,    12,    86,    84,    60,    86,    80,
     593,     1,    44,    84,    40,    86,    81,    30,    74,    75,
      76,    77,    43,    14,    80,    79,    74,    75,    76,    77,
      47,    79,    80,    79,    90,    91,    44,    45,    46,    79,
      30,    79,    90,    91,    34,    35,    36,    30,    42,   632,
      39,   634,   635,    30,    44,    79,     4,   640,   641,   642,
       4,    51,    52,    53,    54,     4,    74,    75,    76,    77,
     653,     4,    80,    44,    64,    46,     4,    53,    68,    69,
      79,    79,    90,    91,    80,    75,   669,    57,    84,    63,
      86,    38,    21,   676,    35,    36,   679,    83,    79,    42,
      26,     1,    81,    74,    75,    76,    77,    72,   691,    80,
      51,    57,    47,    54,     4,   698,   699,    79,   701,    19,
      80,    21,    63,    23,    84,   708,    86,    79,    28,    29,
      30,    53,   715,    33,    34,    30,     1,    37,    38,    79,
      57,    79,    42,    79,    81,    79,    46,    79,    40,     4,
      21,   160,    52,   628,    19,    55,    21,   693,    23,    79,
      60,    79,   632,    79,     1,    30,    79,    32,    33,    34,
     724,    71,    37,    38,   218,    75,    76,    42,    78,   693,
     230,    46,    19,   468,    21,   716,    23,    52,   437,   366,
      55,   106,   286,    30,   163,    60,    33,    34,    52,     1,
      37,    38,   163,   238,   189,    42,    71,   503,   305,    46,
      75,    76,    38,    78,   544,    52,    13,    19,    55,    21,
     437,    23,    13,    60,   437,    43,    42,   640,    30,   422,
     642,    33,    34,    70,    71,    37,    38,    56,    75,    76,
      42,    78,   610,    -1,    46,   195,    -1,    -1,    -1,    -1,
      52,    -1,    -1,    55,    -1,    -1,    -1,     1,    60,    -1,
      -1,    80,    81,    82,    -1,    84,    -1,    86,    70,    71,
      -1,    -1,    -1,    75,    76,    19,    78,    21,    -1,    23,
      -1,    -1,    -1,    -1,    -1,    -1,    30,    -1,    14,    33,
      34,    17,     1,    37,    38,    -1,    -1,    -1,    42,    25,
      26,    -1,    46,    -1,    -1,    -1,    -1,    -1,    52,    -1,
      19,    55,    21,    -1,    23,    41,    60,    43,    44,    -1,
       1,    30,    -1,    -1,    33,    34,    70,    71,    37,    38,
      56,    75,    76,    42,    78,    -1,    -1,    46,    19,    -1,
      21,    -1,    23,    52,    -1,    -1,    55,    -1,    -1,    30,
      -1,    60,    33,    34,    80,     1,    37,    38,    -1,    -1,
      -1,    42,    71,    -1,    -1,    46,    75,    76,    -1,    78,
      -1,    52,    -1,    19,    55,    21,    -1,    23,    -1,    60,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    33,    34,    -1,
      71,    37,    38,    -1,    75,    76,    42,    78,    -1,    -1,
      46,    19,    -1,    -1,    -1,    -1,    52,    -1,     1,    55,
      -1,    -1,    30,    -1,    60,    -1,    34,    35,    36,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    19,    -1,    -1,    75,
      76,    -1,    78,    51,    52,     1,    54,    30,    -1,    -1,
      -1,    34,    35,    36,     1,    -1,    64,    -1,    -1,    -1,
      68,    69,    -1,    -1,    -1,    -1,    -1,    75,    51,    52,
      -1,    54,    -1,    -1,    30,    -1,    -1,    -1,    34,    35,
      36,    64,    -1,    30,    -1,    68,    69,    34,    35,    36,
      -1,     1,    75,    -1,    -1,    51,    52,    53,    54,    -1,
      -1,    -1,    -1,    -1,    51,    52,    53,    54,    64,    42,
      -1,    -1,    68,    69,    -1,    -1,    -1,    64,    -1,    75,
      30,    68,    69,    56,    34,    35,    36,    -1,    75,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    51,    52,    -1,    54,    -1,    -1,    80,    81,    82,
      -1,    84,    -1,    86,    64,    -1,    -1,    -1,    68,    69,
      -1,    -1,    -1,    -1,    -1,    75
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    52,    96,    97,   254,    75,   162,     0,   100,    53,
      72,   255,   256,   257,   258,    79,    80,    97,    75,   162,
     163,   164,    35,    36,    51,    54,    63,   228,   229,   238,
     239,   241,   242,   245,   252,   260,   261,   270,   271,   277,
     278,    97,   256,   258,   259,    12,    44,    45,    46,    74,
      76,    77,    80,    90,    91,    98,    99,   160,   162,   165,
     166,   167,   170,   171,   173,   174,   177,   180,   183,   184,
     186,   188,   189,   190,   191,   192,    86,    79,    82,    76,
     162,   232,    20,   163,   162,    80,    40,    59,    79,   253,
       1,    34,    51,    64,    68,    69,    75,    97,   101,   102,
     103,   104,   108,   112,   153,   156,   157,   158,   159,   228,
     229,   239,   241,   245,   249,   251,   252,   263,   264,   270,
     280,   281,   282,   285,    79,   100,     1,    51,    68,    72,
     104,   229,   236,   242,   249,   272,    79,    79,   260,   259,
     249,   162,   189,   160,   191,   189,     1,    46,    49,   119,
     149,   150,   151,   160,   169,   175,   176,   177,   183,    81,
      82,    80,    84,    86,    10,    16,    48,    73,   178,   179,
       5,     7,     8,    39,    45,    87,    88,    89,    90,    91,
      92,   181,   182,   185,   186,    43,    58,    93,    94,   187,
       6,   162,   163,   231,   163,    40,   253,   230,   163,    44,
     279,   160,    79,    79,   161,   162,    20,    75,    75,   160,
     250,    82,    83,    19,   220,   158,    40,   163,   162,    35,
      51,    54,    83,    79,   253,    79,    79,   100,    57,    10,
      85,    56,    84,   117,    82,    81,    82,    81,    72,    81,
       3,    79,    99,   168,   169,   176,   177,    14,    25,    26,
      75,   172,   190,    15,   162,   165,   166,   177,    67,    28,
     180,   180,    39,   183,   119,   160,   183,   186,   188,   189,
      80,   233,   234,    40,    51,   154,   155,   156,   229,   279,
      79,   233,    81,   160,    84,    69,    84,    86,   163,    40,
      80,   109,   142,    79,    82,    75,    22,    32,   105,     1,
      21,    33,    37,    38,    46,    55,    60,    75,    78,    97,
     160,   191,   193,   194,   195,   196,   197,   198,   199,   200,
     206,   207,   210,   211,   217,   221,   223,   226,   227,   240,
     269,   286,    30,    63,    40,   253,    80,   142,   273,   232,
     162,   162,    39,    50,   237,    79,    81,   177,   150,   151,
     177,   119,   160,    56,   169,   169,    46,   168,   183,    81,
      82,   235,   236,    60,   153,    20,    53,   243,   156,    40,
      51,   229,   239,   245,   262,    18,    57,   177,   172,   162,
      40,   113,   160,     1,     4,   104,   143,   144,    40,   110,
     160,     9,    79,   253,    17,   106,   113,   127,   128,   129,
      79,   177,   160,   224,   160,   177,   201,   202,   203,   204,
      79,   224,    79,   177,    83,   194,     9,    79,    79,    32,
     194,   222,   265,   100,    23,    34,    42,    71,   212,   213,
     215,   218,   216,   232,    79,    79,     4,    40,   233,    40,
     233,    50,   161,    80,    57,    81,   169,    79,    81,   160,
     220,   246,   163,   154,    30,    40,   177,    18,   283,    79,
      63,    79,    26,   114,   115,   117,    81,    83,    79,    81,
      14,    25,    26,    41,    43,    44,    80,   111,   116,   117,
     120,   123,   124,   125,   126,   127,   136,   152,   247,   248,
      79,   177,    79,    80,   133,     9,   107,   253,    40,    70,
     225,    79,    28,    29,   205,   193,    67,    79,    79,   177,
      70,   266,   266,   208,   219,    75,   193,   204,   216,    61,
     214,   220,    79,    81,    25,    26,    43,    44,    56,    80,
     127,   152,   247,   248,   275,   276,    60,    44,    40,   274,
     107,   177,    81,   236,    30,    40,   163,   244,    79,    43,
     284,    79,   177,    14,   145,   144,    22,   113,   177,   177,
     177,   113,    74,    75,   121,   122,    46,    53,    57,   137,
      79,   119,   131,   132,   134,   135,   160,    47,   177,    79,
      79,   204,    79,   193,   202,    30,    79,    49,    75,   160,
     267,   268,    30,    70,   209,   153,    39,    30,   215,    79,
     135,   160,    30,     4,     4,     4,   113,     4,     4,    79,
     160,   160,     4,   160,    79,    81,   244,   177,    30,   161,
     117,   118,   161,   113,    26,   117,   118,    81,    82,    57,
     100,    81,    82,    81,    82,    56,   118,   113,   130,    38,
      83,    10,    85,    21,   149,    42,   215,   216,    26,    72,
      81,   274,    79,    80,    79,    79,    57,    18,   107,   177,
     122,     1,    21,    46,   104,   138,   139,   141,   146,    47,
     132,   160,   135,     4,    79,   267,   193,   268,    79,    10,
      79,     4,    53,    79,     4,    79,   177,   118,    79,   162,
      79,    83,    30,   100,   140,   100,   130,    56,    10,   193,
      81,    56,    40,   100,   130,    57,   141,   146,   193,    79,
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
     252,   253,   254,   254,   254,   255,   255,   256,   256,   257,
     257,   257,   258,   259,   259,   260,   260,   260,   260,   260,
     260,   260,   261,   262,   262,   263,   263,   264,   265,   265,
     266,   266,   267,   267,   268,   268,   269,   270,   270,   271,
     271,   272,   272,   272,   272,   272,   272,   272,   273,   273,
     273,   274,   274,   274,   275,   275,   275,   275,   275,   275,
     275,   275,   275,   275,   276,   276,   277,   278,   279,   280,
     280,   280,   281,   282,   283,   283,   284,   284,   285,   286
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
       4,     2,     0,     2,     2,     4,     3,     0,     1,     2,
       3,     2,     3,     0,     2,     1,     1,     1,     1,     1,
       1,     1,     5,     1,     1,     6,     4,     4,     2,     2,
       4,     6,     1,     3,     1,     1,     3,     3,     3,     1,
       2,     2,     6,     6,     8,    10,     7,     1,     0,     1,
       3,     0,     2,     2,     3,     2,     2,     2,     4,     2,
       1,     1,     1,     1,     2,     4,     3,     4,     2,     1,
       1,     1,     5,     9,     0,     4,     0,     7,     6,     2
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
#line 208 "grammar83.y"
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

#line 2512 "grammar83.tab.c"

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
#line 260 "grammar83.y"
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
#line 2753 "grammar83.tab.c"
    break;

  case 23: /* number_decl: def_id_s ':' CONSTANT IS_ASSIGNED expression ';'  */
#line 284 "grammar83.y"
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
#line 2769 "grammar83.tab.c"
    break;

  case 24: /* def_id_s: identifier  */
#line 297 "grammar83.y"
               {
        array_StringToken_init(&(yyval.str_token_array));
        array_StringToken_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2778 "grammar83.tab.c"
    break;

  case 25: /* def_id_s: def_id_s ',' identifier  */
#line 301 "grammar83.y"
                            { array_StringToken_append(&(yyval.str_token_array), (yyvsp[0].str_token)); }
#line 2784 "grammar83.tab.c"
    break;

  case 26: /* object_qualifier_opt: %empty  */
#line 306 "grammar83.y"
             { (yyval.bool_) = false; }
#line 2790 "grammar83.tab.c"
    break;

  case 27: /* object_qualifier_opt: CONSTANT  */
#line 307 "grammar83.y"
             { (yyval.bool_) = true; }
#line 2796 "grammar83.tab.c"
    break;

  case 30: /* init_opt: %empty  */
#line 316 "grammar83.y"
                           { (yyval.expr) = NULL; }
#line 2802 "grammar83.tab.c"
    break;

  case 31: /* init_opt: IS_ASSIGNED expression  */
#line 317 "grammar83.y"
                           { (yyval.expr) = (yyvsp[0].expr); }
#line 2808 "grammar83.tab.c"
    break;

  case 32: /* type_decl: TYPE identifier discrim_part_opt type_completion ';'  */
#line 321 "grammar83.y"
                                                         {
        // TODO: discriminant
        TypeDecl* decl = (yyvsp[-1].type_decl);
        // Note: decl->base.kind is set by the specific type_completion
        decl->base.line_num = (yyloc);
        decl->name = (yyvsp[-3].str_token);
        check_for_redefinition(context, decl->name, (yyloc));
        push_declaration(context, &decl->base);
    }
#line 2822 "grammar83.tab.c"
    break;

  case 37: /* type_completion: IS type_def  */
#line 340 "grammar83.y"
                { (yyval.type_decl) = (yyvsp[0].type_decl); }
#line 2828 "grammar83.tab.c"
    break;

  case 46: /* subtype_decl: SUBTYPE identifier IS subtype_ind ';'  */
#line 355 "grammar83.y"
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
#line 2846 "grammar83.tab.c"
    break;

  case 47: /* subtype_ind: name constraint  */
#line 371 "grammar83.y"
                    {
        // TODO: propagate constraint somehow
        (yyval.str_token) = (yyvsp[-1].name).name;
    }
#line 2855 "grammar83.tab.c"
    break;

  case 48: /* subtype_ind: name  */
#line 375 "grammar83.y"
         { (yyval.str_token) = (yyvsp[0].name).name; }
#line 2861 "grammar83.tab.c"
    break;

  case 52: /* derived_type: NEW subtype_ind  */
#line 388 "grammar83.y"
                    {
        (yyval.type_decl) = create_type_decl(TYPE_DERIVED);
        TypeDecl* base_type = find_type_decl(context, (yyvsp[0].str_token));
        if(!base_type) {
            error_print((yyloc), "Unknown base type: %s", ST((yyvsp[0].str_token)));
            error_exit();
        }
        (yyval.type_decl)->u.subtype.base = base_type;
    }
#line 2875 "grammar83.tab.c"
    break;

  case 53: /* range_constraint: RANGE range  */
#line 399 "grammar83.y"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 2881 "grammar83.tab.c"
    break;

  case 54: /* range_constr_opt: %empty  */
#line 403 "grammar83.y"
           { (yyval.expr) = NULL; }
#line 2887 "grammar83.tab.c"
    break;

  case 56: /* range: simple_expression DOT_DOT simple_expression  */
#line 408 "grammar83.y"
                                                { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_RANGE, (yyvsp[0].expr)); }
#line 2893 "grammar83.tab.c"
    break;

  case 59: /* enumeration_type: '(' enum_id_s ')'  */
#line 414 "grammar83.y"
                      {
        (yyval.type_decl) = create_type_decl(TYPE_ENUM);
        (yyval.type_decl)->u.enum_.literals = (yyvsp[-1].expr_array).data;
        (yyval.type_decl)->u.enum_.literal_count = array_ExprPtr_size(&(yyvsp[-1].expr_array));
    }
#line 2903 "grammar83.tab.c"
    break;

  case 60: /* enum_id_s: enum_id  */
#line 421 "grammar83.y"
            {
        array_ExprPtr_init(&(yyval.expr_array));
        array_ExprPtr_append(&(yyval.expr_array), (yyvsp[0].expr));
    }
#line 2912 "grammar83.tab.c"
    break;

  case 61: /* enum_id_s: enum_id_s ',' enum_id  */
#line 425 "grammar83.y"
                          { array_ExprPtr_append(&(yyval.expr_array), (yyvsp[0].expr)); }
#line 2918 "grammar83.tab.c"
    break;

  case 62: /* enum_id: identifier  */
#line 429 "grammar83.y"
               {
        (yyval.expr) = create_expr(EXPR_NAME, (yyloc));
        (yyval.expr)->u.name.name = (yyvsp[0].str_token);
    }
#line 2927 "grammar83.tab.c"
    break;

  case 63: /* enum_id: char_lit  */
#line 433 "grammar83.y"
             {
        (yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
        (yyval.expr)->u.char_lit = (yyvsp[0].c);
    }
#line 2936 "grammar83.tab.c"
    break;

  case 64: /* integer_type: range_constraint  */
#line 439 "grammar83.y"
                     {
        (yyval.type_decl) = create_type_decl(TYPE_INTEGER);
        (yyval.type_decl)->u.int_.range = (yyvsp[0].expr);
    }
#line 2945 "grammar83.tab.c"
    break;

  case 132: /* name: simple_name  */
#line 621 "grammar83.y"
                {
        memset(&(yyval.name), 0, sizeof((yyval.name)));
        (yyval.name).name = (yyvsp[0].str_token);
    }
#line 2954 "grammar83.tab.c"
    break;

  case 136: /* name: operator_symbol  */
#line 628 "grammar83.y"
                    {
        memset(&(yyval.name), 0, sizeof((yyval.name)));
        (yyval.name).name = (yyvsp[0].str_token);
        //TODO: lookup operator, determine its arity, and allocate args array
    }
#line 2964 "grammar83.tab.c"
    break;

  case 145: /* used_char: char_lit  */
#line 655 "grammar83.y"
             {
        (yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
        (yyval.expr)->u.char_lit = (yyvsp[0].c);
    }
#line 2973 "grammar83.tab.c"
    break;

  case 163: /* literal: numeric_lit  */
#line 699 "grammar83.y"
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
#line 2997 "grammar83.tab.c"
    break;

  case 175: /* expression: expression logical relation  */
#line 741 "grammar83.y"
                                      { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3003 "grammar83.tab.c"
    break;

  case 176: /* expression: expression short_circuit relation  */
#line 742 "grammar83.y"
                                      { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3009 "grammar83.tab.c"
    break;

  case 177: /* logical: AND  */
#line 746 "grammar83.y"
        { (yyval.unary_op) = OP_AND; }
#line 3015 "grammar83.tab.c"
    break;

  case 178: /* logical: OR  */
#line 747 "grammar83.y"
        { (yyval.unary_op) = OP_OR; }
#line 3021 "grammar83.tab.c"
    break;

  case 179: /* logical: XOR  */
#line 748 "grammar83.y"
        { (yyval.unary_op) = OP_XOR; }
#line 3027 "grammar83.tab.c"
    break;

  case 180: /* short_circuit: AND THEN  */
#line 752 "grammar83.y"
               { (yyval.unary_op) = OP_AND_THEN; }
#line 3033 "grammar83.tab.c"
    break;

  case 181: /* short_circuit: OR ELSE  */
#line 753 "grammar83.y"
               { (yyval.unary_op) = OP_OR_ELSE; }
#line 3039 "grammar83.tab.c"
    break;

  case 183: /* relation: simple_expression relational simple_expression  */
#line 759 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3045 "grammar83.tab.c"
    break;

  case 184: /* relation: simple_expression membership range  */
#line 760 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3051 "grammar83.tab.c"
    break;

  case 185: /* relation: simple_expression membership name  */
#line 761 "grammar83.y"
                                                   {
        Expression* right = create_expr(EXPR_NAME, (yylsp[0]));
        right->u.name = (yyvsp[0].name);
        (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), right);
    }
#line 3061 "grammar83.tab.c"
    break;

  case 186: /* relational: '='  */
#line 768 "grammar83.y"
          { (yyval.unary_op) = OP_EQ; }
#line 3067 "grammar83.tab.c"
    break;

  case 187: /* relational: NE  */
#line 769 "grammar83.y"
          { (yyval.unary_op) = OP_NEQ; }
#line 3073 "grammar83.tab.c"
    break;

  case 188: /* relational: '<'  */
#line 770 "grammar83.y"
          { (yyval.unary_op) = OP_LT; }
#line 3079 "grammar83.tab.c"
    break;

  case 189: /* relational: LT_EQ  */
#line 771 "grammar83.y"
          { (yyval.unary_op) = OP_LTE; }
#line 3085 "grammar83.tab.c"
    break;

  case 190: /* relational: '>'  */
#line 772 "grammar83.y"
          { (yyval.unary_op) = OP_GT; }
#line 3091 "grammar83.tab.c"
    break;

  case 191: /* relational: GE  */
#line 773 "grammar83.y"
          { (yyval.unary_op) = OP_GTE; }
#line 3097 "grammar83.tab.c"
    break;

  case 192: /* membership: IN  */
#line 777 "grammar83.y"
           { (yyval.unary_op) = OP_IN; }
#line 3103 "grammar83.tab.c"
    break;

  case 193: /* membership: NOT IN  */
#line 778 "grammar83.y"
           { (yyval.unary_op) = OP_NOT_IN; }
#line 3109 "grammar83.tab.c"
    break;

  case 195: /* simple_expression: unary term  */
#line 783 "grammar83.y"
                                  { (yyval.expr) = make_unary_expr((yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3115 "grammar83.tab.c"
    break;

  case 196: /* simple_expression: simple_expression adding term  */
#line 784 "grammar83.y"
                                  { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3121 "grammar83.tab.c"
    break;

  case 197: /* unary: '+'  */
#line 788 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_PLUS; }
#line 3127 "grammar83.tab.c"
    break;

  case 198: /* unary: '-'  */
#line 789 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_MINUS; }
#line 3133 "grammar83.tab.c"
    break;

  case 199: /* adding: '+'  */
#line 793 "grammar83.y"
        { (yyval.unary_op) = OP_PLUS; }
#line 3139 "grammar83.tab.c"
    break;

  case 200: /* adding: '-'  */
#line 794 "grammar83.y"
        { (yyval.unary_op) = OP_MINUS; }
#line 3145 "grammar83.tab.c"
    break;

  case 201: /* adding: '&'  */
#line 795 "grammar83.y"
        { (yyval.unary_op) = OP_AMP; }
#line 3151 "grammar83.tab.c"
    break;

  case 203: /* term: term multiplying factor  */
#line 800 "grammar83.y"
                            { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3157 "grammar83.tab.c"
    break;

  case 204: /* multiplying: '*'  */
#line 804 "grammar83.y"
        { (yyval.unary_op) = OP_MULT; }
#line 3163 "grammar83.tab.c"
    break;

  case 205: /* multiplying: '/'  */
#line 805 "grammar83.y"
        { (yyval.unary_op) = OP_DIVIDE; }
#line 3169 "grammar83.tab.c"
    break;

  case 206: /* multiplying: MOD  */
#line 806 "grammar83.y"
        { (yyval.unary_op) = OP_MOD; }
#line 3175 "grammar83.tab.c"
    break;

  case 207: /* multiplying: REM  */
#line 807 "grammar83.y"
        { (yyval.unary_op) = OP_REM; }
#line 3181 "grammar83.tab.c"
    break;

  case 209: /* factor: NOT primary  */
#line 812 "grammar83.y"
                          { (yyval.expr) = make_unary_expr(OP_NOT, (yyvsp[0].expr)); }
#line 3187 "grammar83.tab.c"
    break;

  case 210: /* factor: ABS primary  */
#line 813 "grammar83.y"
                          { (yyval.expr) = make_unary_expr(OP_ABS, (yyvsp[0].expr)); }
#line 3193 "grammar83.tab.c"
    break;

  case 211: /* factor: primary EXPON primary  */
#line 814 "grammar83.y"
                          { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_EXP, (yyvsp[0].expr)); }
#line 3199 "grammar83.tab.c"
    break;

  case 213: /* primary: name  */
#line 819 "grammar83.y"
         {
        (yyval.expr) = create_expr(EXPR_NAME, (yyloc));
        (yyval.expr)->u.name = (yyvsp[0].name);
    }
#line 3208 "grammar83.tab.c"
    break;

  case 218: /* parenthesized_primary: '(' expression ')'  */
#line 830 "grammar83.y"
                       { (yyval.expr) = (yyvsp[-1].expr); }
#line 3214 "grammar83.tab.c"
    break;

  case 223: /* statement_s: statement_s statement  */
#line 844 "grammar83.y"
                          {
        (yyval.stmt) = (yyvsp[-1].stmt);
        (yyval.stmt)->next = (yyvsp[0].stmt);
        (yyval.stmt) = (yyvsp[0].stmt);
    }
#line 3224 "grammar83.tab.c"
    break;

  case 225: /* statement: goto_label statement  */
#line 852 "grammar83.y"
                         {
        check_for_redefinition(context, (yyvsp[-1].str_token), (yylsp[-1]));
        LabelDecl* label = create_label((yyvsp[-1].str_token), (yylsp[-1]));
        push_declaration(context, (Declaration*)label);
        (yyval.stmt) = (yyvsp[0].stmt);
    }
#line 3235 "grammar83.tab.c"
    break;

  case 242: /* null_stmt: NuLL ';'  */
#line 885 "grammar83.y"
             { (yyval.stmt) = create_stmt(STMT_NULL, (yyloc)); }
#line 3241 "grammar83.tab.c"
    break;

  case 243: /* assign_stmt: name IS_ASSIGNED expression ';'  */
#line 890 "grammar83.y"
                                    {
        (yyval.stmt) = create_stmt(STMT_ASSIGN, (yyloc));
        (yyval.stmt)->u.assign.expr = (yyvsp[-1].expr);
    }
#line 3250 "grammar83.tab.c"
    break;

  case 248: /* cond_part: condition THEN  */
#line 909 "grammar83.y"
                   { (yyval.expr) = (yyvsp[-1].expr); }
#line 3256 "grammar83.tab.c"
    break;

  case 257: /* loop_stmt: label_opt loop_content id_opt ';'  */
#line 940 "grammar83.y"
                                      { (yyval.stmt) = (yyvsp[-2].stmt); }
#line 3262 "grammar83.tab.c"
    break;

  case 260: /* loop_content: basic_loop  */
#line 949 "grammar83.y"
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
#line 3277 "grammar83.tab.c"
    break;

  case 261: /* loop_content: WHILE condition basic_loop  */
#line 959 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_WHILE;
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
        (yyval.stmt)->u.loop.u.while_.condition = (yyvsp[-1].expr);
    }
#line 3288 "grammar83.tab.c"
    break;

  case 262: /* loop_content: iter_part reverse_opt discrete_range basic_loop  */
#line 965 "grammar83.y"
                                                    {
        // TODO: identifier
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_FOR;
        (yyval.stmt)->u.loop.reverse = (yyvsp[-2].bool_);
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
    }
#line 3300 "grammar83.tab.c"
    break;

  case 264: /* reverse_opt: %empty  */
#line 978 "grammar83.y"
            { (yyval.bool_) = false; }
#line 3306 "grammar83.tab.c"
    break;

  case 265: /* reverse_opt: REVERSE  */
#line 979 "grammar83.y"
            { (yyval.bool_) = true; }
#line 3312 "grammar83.tab.c"
    break;

  case 266: /* basic_loop: LOOP statement_s END LOOP  */
#line 983 "grammar83.y"
                              { (yyval.stmt) = (yyvsp[-2].stmt); }
#line 3318 "grammar83.tab.c"
    break;

  case 269: /* block: label_opt block_decl block_body END id_opt ';'  */
#line 993 "grammar83.y"
                                                   {
        (yyval.stmt) = create_stmt(STMT_BLOCK, (yyloc));
        (yyval.stmt)->u.block.stmts = (yyvsp[-3].stmt);
        // Close scope if needed (i.e. if there was a declaration section)
        if((yyvsp[-4].bool_)) {
            end_scope(context, (yylsp[-2]));
        }
    }
#line 3331 "grammar83.tab.c"
    break;

  case 270: /* block_decl: %empty  */
#line 1003 "grammar83.y"
            { (yyval.bool_) = false; }
#line 3337 "grammar83.tab.c"
    break;

  case 271: /* $@1: %empty  */
#line 1004 "grammar83.y"
            { begin_scope(context, (yylsp[0])); }
#line 3343 "grammar83.tab.c"
    break;

  case 272: /* block_decl: DECLARE $@1 decl_part  */
#line 1004 "grammar83.y"
                                                    { (yyval.bool_) = true; }
#line 3349 "grammar83.tab.c"
    break;

  case 273: /* block_body: BEGiN handled_stmt_s  */
#line 1008 "grammar83.y"
                         { (yyval.stmt) = (yyvsp[0].stmt); }
#line 3355 "grammar83.tab.c"
    break;

  case 274: /* handled_stmt_s: statement_s except_handler_part_opt  */
#line 1013 "grammar83.y"
                                        { (yyval.stmt) = (yyvsp[-1].stmt); }
#line 3361 "grammar83.tab.c"
    break;

  case 277: /* exit_stmt: EXIT name_opt when_opt ';'  */
#line 1022 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_EXIT, (yyloc));
        // TODO: name_opt
        (yyval.stmt)->u.exit.condition = (yyvsp[-1].expr);
    }
#line 3371 "grammar83.tab.c"
    break;

  case 280: /* when_opt: %empty  */
#line 1034 "grammar83.y"
                   { (yyval.expr) = NULL; }
#line 3377 "grammar83.tab.c"
    break;

  case 281: /* when_opt: WHEN condition  */
#line 1035 "grammar83.y"
                   { (yyval.expr) = (yyvsp[0].expr); }
#line 3383 "grammar83.tab.c"
    break;

  case 282: /* return_stmt: RETURN ';'  */
#line 1039 "grammar83.y"
                  { (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc)); }
#line 3389 "grammar83.tab.c"
    break;

  case 283: /* return_stmt: RETURN expression ';'  */
#line 1040 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc));
        (yyval.stmt)->u.return_.expr = (yyvsp[-1].expr);
    }
#line 3398 "grammar83.tab.c"
    break;

  case 284: /* goto_stmt: GOTO name ';'  */
#line 1046 "grammar83.y"
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
#line 3425 "grammar83.tab.c"
    break;

  case 287: /* @2: %empty  */
#line 1075 "grammar83.y"
                                           {
        begin_scope(context, (yylsp[0]));
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
    }
#line 3435 "grammar83.tab.c"
    break;

  case 289: /* @3: %empty  */
#line 1081 "grammar83.y"
                                         {
        begin_scope(context, (yylsp[0]));
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
    }
#line 3445 "grammar83.tab.c"
    break;

  case 293: /* designator: char_string  */
#line 1092 "grammar83.y"
                { (yyval.str_token) = string_pool_to_token((yyvsp[0].str)); }
#line 3451 "grammar83.tab.c"
    break;

  case 301: /* mode: %empty  */
#line 1115 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3457 "grammar83.tab.c"
    break;

  case 302: /* mode: IN  */
#line 1116 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3463 "grammar83.tab.c"
    break;

  case 303: /* mode: OUT  */
#line 1117 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_OUT; }
#line 3469 "grammar83.tab.c"
    break;

  case 304: /* mode: IN OUT  */
#line 1118 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN_OUT; }
#line 3475 "grammar83.tab.c"
    break;

  case 307: /* procedure_call: name ';'  */
#line 1130 "grammar83.y"
             {
        (yyval.stmt) = create_stmt(STMT_NAME, (yyloc));
        (yyval.stmt)->u.name = (yyvsp[-1].name);
        (yyval.stmt)->u.name.is_function = false;
        (yyval.stmt)->u.name.is_subprogram = true;
    }
#line 3486 "grammar83.tab.c"
    break;


#line 3490 "grammar83.tab.c"

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

#line 1377 "grammar83.y"


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
