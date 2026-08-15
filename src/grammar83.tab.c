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
#line 77 "grammar83.y"

    #include <assert.h>
    #include <stdlib.h>
    #include <stdbool.h>
    #include "error.h"
    #include "string_pool.h"
    #include "string_view.h"
    #include "lexer.h"

    #define TABLE_GROWTH_FACTOR 2

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

#line 485 "grammar83.tab.c"

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
#define YYLAST   1274

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  95
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  189
/* YYNRULES -- Number of rules.  */
#define YYNRULES  401
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  715

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
       0,   273,   273,   277,   278,   282,   283,   287,   288,   292,
     293,   297,   298,   299,   303,   307,   311,   315,   316,   317,
     318,   319,   323,   349,   364,   368,   373,   374,   378,   379,
     383,   384,   388,   400,   401,   402,   407,   408,   412,   413,
     414,   415,   416,   417,   418,   419,   423,   440,   444,   448,
     449,   453,   457,   468,   472,   473,   477,   478,   479,   483,
     494,   498,   504,   511,   525,   529,   533,   534,   538,   542,
     543,   547,   548,   552,   556,   560,   564,   565,   569,   573,
     577,   578,   582,   583,   587,   591,   592,   596,   597,   598,
     602,   603,   607,   608,   612,   613,   617,   621,   622,   626,
     627,   631,   632,   636,   640,   641,   645,   649,   653,   659,
     663,   664,   668,   669,   673,   674,   678,   679,   683,   684,
     688,   689,   695,   696,   697,   698,   702,   703,   709,   713,
     717,   718,   722,   726,   727,   728,   729,   736,   737,   738,
     742,   748,   752,   756,   757,   761,   762,   763,   764,   768,
     769,   770,   771,   775,   779,   780,   781,   782,   786,   805,
     806,   810,   811,   812,   813,   814,   818,   819,   823,   827,
     828,   829,   833,   834,   835,   839,   840,   845,   846,   847,
     848,   855,   856,   857,   858,   859,   860,   864,   865,   869,
     870,   871,   875,   876,   880,   881,   882,   886,   887,   891,
     892,   893,   894,   898,   899,   900,   901,   905,   906,   910,
     911,   912,   916,   917,   921,   935,   936,   940,   944,   950,
     951,   959,   960,   961,   965,   966,   967,   968,   969,   970,
     971,   972,   973,   977,   978,   979,   980,   984,   988,   997,
    1008,  1009,  1015,  1022,  1026,  1030,  1031,  1035,  1042,  1048,
    1049,  1055,  1064,  1068,  1069,  1073,  1082,  1088,  1098,  1106,
    1107,  1111,  1115,  1116,  1121,  1132,  1133,  1133,  1137,  1142,
    1146,  1147,  1151,  1158,  1159,  1163,  1164,  1168,  1169,  1175,
    1199,  1203,  1208,  1208,  1215,  1215,  1222,  1226,  1227,  1231,
    1232,  1236,  1240,  1241,  1245,  1246,  1250,  1251,  1252,  1253,
    1257,  1263,  1272,  1280,  1281,  1285,  1285,  1304,  1305,  1309,
    1310,  1314,  1314,  1333,  1334,  1338,  1342,  1343,  1347,  1351,
    1352,  1356,  1357,  1358,  1362,  1363,  1364,  1365,  1369,  1373,
    1374,  1378,  1379,  1380,  1384,  1388,  1389,  1393,  1397,  1401,
    1405,  1409,  1410,  1411,  1415,  1419,  1420,  1424,  1425,  1429,
    1433,  1434,  1438,  1439,  1443,  1444,  1448,  1449,  1453,  1457,
    1458,  1462,  1463,  1467,  1468,  1469,  1470,  1471,  1472,  1473,
    1477,  1478,  1479,  1483,  1484,  1485,  1489,  1490,  1491,  1492,
    1493,  1494,  1495,  1496,  1497,  1498,  1502,  1503,  1507,  1511,
    1515,  1519,  1520,  1521,  1525,  1529,  1533,  1534,  1538,  1539,
    1543,  1547
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
  "use_clause", "name_s", "rename_decl", "rename_unit", "renames",
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

#define YYPACT_NINF (-531)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-354)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     564,    10,  -531,    68,    -5,    -7,    32,    94,  -531,   156,
    1173,  -531,  -531,   100,  -531,  -531,  -531,   532,  -531,  -531,
    -531,  -531,   237,   108,   123,  -531,  -531,    20,   181,   241,
    -531,   189,  -531,   134,  -531,   196,   446,  -531,   210,   236,
     254,   106,   266,   284,   287,   446,  -531,  -531,  -531,  -531,
     506,  -531,  -531,   376,  -531,  1090,  -531,  -531,  -531,   158,
    -531,  -531,  -531,  -531,  -531,  -531,  -531,  -531,  -531,  -531,
    -531,  -531,  -531,  -531,  -531,  -531,  -531,   342,   392,  -531,
     371,   384,   448,   527,   231,   362,   393,  -531,  -531,  -531,
    -531,   414,   456,   196,   431,   414,   450,  -531,   462,   446,
    -531,  -531,  -531,   559,  -531,  -531,  -531,  -531,  -531,  -531,
    -531,   469,   476,   537,   479,   478,   559,   248,   245,  1062,
     535,  -531,   308,   342,   392,  -531,  -531,   366,   498,    10,
     521,   523,    58,  -531,   502,  -531,  -531,    48,   542,  -531,
    -531,  1115,  -531,  -531,  -531,   466,  -531,   559,   459,   373,
     292,   683,   373,   530,   597,  -531,   731,   446,    60,   600,
    -531,  -531,   446,   651,   349,    34,   594,   743,   446,   446,
     743,   602,   446,   692,   566,  1062,  -531,    54,   612,   843,
    -531,  -531,  -531,  -531,  -531,  -531,  -531,  -531,  -531,  -531,
     274,  -531,  -531,  -531,  -531,  -531,  -531,  -531,  -531,    10,
     615,  -531,   620,    78,  -531,   627,   414,   645,   414,   652,
    -531,   254,  -531,   257,  -531,   446,  1142,   173,   599,  1151,
    -531,   416,   685,   686,  -531,  -531,  -531,  -531,  1194,   446,
    1194,  -531,  -531,  -531,  -531,   517,  -531,  -531,  -531,    56,
    -531,   541,     9,  -531,   544,  -531,  -531,  -531,  -531,   113,
    -531,   424,   497,   394,  -531,   703,  -531,  -531,  -531,  -531,
    -531,  -531,  -531,  -531,  -531,  -531,  -531,  -531,   743,   714,
     570,   238,   623,  -531,  -531,   672,   729,   596,  -531,   230,
     675,    19,  -531,   684,   588,   328,  -531,   826,   677,   559,
     743,  -531,   691,   705,   755,   727,  -531,  -531,  -531,  -531,
     212,   559,   720,   496,   230,   679,  -531,  1062,   724,  -531,
     713,  -531,   271,  -531,  -531,   743,  -531,   175,  -531,   723,
    -531,  -531,   723,   392,  -531,   721,  1062,   743,    10,   736,
    -531,   376,   719,  -531,  -531,  -531,   718,   982,   741,   758,
     763,  -531,    41,    48,  -531,   559,   376,   735,  1198,   774,
    -531,   308,  -531,  -531,   570,  -531,  -531,   756,   730,   640,
     733,   253,   743,   704,   743,   178,  -531,  -531,   459,   748,
     788,  -531,   743,   743,   743,  -531,  -531,  -531,  -531,   793,
    -531,  -531,  -531,  -531,  -531,  -531,   743,   743,   497,   394,
    -531,  -531,  -531,  -531,   497,  1194,   324,   792,  -531,  -531,
     760,   743,   768,   731,  -531,   743,  -531,  -531,  -531,  -531,
     834,    82,  -531,   302,   743,   743,  -531,   743,   446,   650,
    -531,  -531,  -531,  -531,  -531,  -531,  -531,  -531,  -531,  -531,
    -531,  -531,   499,  -531,   340,  -531,   743,   802,   743,   776,
     777,  -531,   743,   778,  -531,  1062,   743,   823,   808,  -531,
    -531,  -531,   341,  -531,    38,  -531,  -531,    47,  1173,   819,
     999,   817,   782,  -531,   743,   835,  -531,  -531,   867,   870,
     874,   446,   883,   884,  -531,  -531,  -531,   838,   814,  -531,
     446,   446,   102,   818,  -531,  -531,  -531,   866,   859,  -531,
     825,   821,   459,  -531,   459,  -531,   589,  -531,   230,  -531,
    -531,   230,  -531,   573,    28,   830,  -531,  -531,  -531,  -531,
    -531,   606,  -531,   606,  -531,   289,   394,  -531,  -531,  -531,
     743,   193,  -531,   230,  -531,  -531,   370,  -531,   254,  -531,
     446,  -531,   348,   370,   230,  -531,  -531,  -531,   649,  -531,
     854,  -531,  -531,  -531,  -531,  -531,   660,  -531,   663,  -531,
     744,   446,   230,  -531,  -531,  -531,  -531,  1027,  -531,   877,
    -531,  -531,   829,   559,    57,  -531,   895,   704,  -531,  -531,
    -531,   878,  -531,  -531,   817,   444,    10,   896,  -531,  -531,
     852,  -531,   847,  -531,   165,   562,  -531,   559,  -531,   825,
     672,  -531,  -531,  -531,  -531,  -531,   873,   665,   743,   374,
     875,    53,  -531,  -531,    41,  -531,   743,  -531,  -531,  -531,
     650,  -531,   190,   886,   446,  -531,   743,   643,  -531,  -531,
    -531,   855,   409,  1062,   409,   856,    62,  -531,  -531,   858,
     934,   892,  -531,   872,  -531,   390,   876,   869,  -531,   166,
    -531,   880,   743,  -531,   370,  -531,   881,   879,   885,   669,
     923,  -531,  -531,  -531,   446,  -531,   262,  -531,  -531,  -531,
      64,   871,  -531,  -531,  1062,  -531,  -531,  -531,  -531,   882,
    -531,  -531,  -531,  -531,   434,  -531,  -531,   916,  -531,   446,
     900,   154,  -531,   392,  -531,   958,  1062,   906,   888,   743,
    -531,   392,   755,  -531,  -531,  -531,   964,  -531,   889,   360,
     890,   392,  -531,   704,    51,  -531,  -531,    66,   949,  -531,
    -531,   893,   190,  -531,  -531
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     0,   361,     0,     0,     0,     0,     0,   339,     0,
       0,   340,   337,     0,   338,   343,     2,     0,   335,     9,
     341,   342,     0,     0,     0,   287,   288,   286,     0,     0,
     282,     0,    24,     0,     1,   300,     0,   280,     0,     0,
       0,     0,     0,     0,     0,     0,   125,   122,    11,    12,
       0,    13,    14,     0,   129,     0,   126,   128,    15,     0,
     130,    16,   131,   123,    18,   323,    20,    17,    19,   124,
     391,   392,   393,   303,   333,   335,     9,   331,   330,   295,
       0,     0,     0,     0,     0,     0,     0,   369,   362,   281,
     304,   289,     0,   305,     0,   289,     0,   334,     0,     0,
     388,   132,   141,   328,   136,   133,   134,   135,   325,    21,
     137,     0,     0,     0,     0,    33,   319,     0,    26,     0,
       0,   127,   300,   332,   329,   336,    10,     0,   370,     0,
       0,     0,   296,   359,     0,   363,   360,     0,     0,   290,
     311,     0,   389,   324,   283,     0,    25,   390,     0,     0,
       0,     0,     0,     0,     0,     3,     0,     0,     0,    36,
      34,   318,     0,    27,     0,     0,     0,     0,   273,     0,
       0,     0,   273,     0,   132,     0,   223,     0,     0,     0,
     217,   219,   221,   222,   224,   225,   233,   234,     9,   235,
     265,   236,   268,   226,   227,   228,   229,   230,   231,   262,
       0,   305,     0,     0,   371,     0,   289,     0,   289,   297,
     298,     0,   327,     0,   292,     0,     0,     0,   307,     0,
     120,     0,     0,     0,   345,   346,   344,   148,     0,     0,
       0,   160,   111,   140,   158,     0,   192,   193,   113,     0,
     107,   110,   208,   159,     0,   143,   207,   212,   146,   109,
     169,   177,     0,   189,   197,   203,   211,   210,   209,   157,
     156,   155,   154,   153,   152,   149,   150,   151,     0,   396,
     208,     0,   177,   138,   139,   311,   132,     0,     5,     7,
       0,    48,   100,     0,     0,     0,    97,   316,     0,   320,
       0,   349,     0,     0,    30,    28,    29,    71,    72,   232,
       0,   274,   275,     0,   244,   245,   240,     0,     0,   237,
       0,   277,     0,   254,   220,     0,   302,     0,   401,     0,
     218,   269,   271,   249,   266,     0,     0,     0,   262,   259,
     255,     0,     0,   263,   348,   326,     0,   316,     0,     0,
     373,   299,    30,     0,   291,   285,   313,     0,     0,     0,
     121,     0,   300,   205,   215,   216,   204,   160,     0,     0,
     146,   109,     0,     0,     0,     0,   112,   142,     0,   172,
     173,   174,     0,     0,     0,   184,   182,   186,   187,     0,
     181,   183,   185,   194,   195,   196,     0,     0,     0,   190,
     201,   202,   199,   200,     0,     0,     0,     0,   398,   394,
       0,     0,     0,     0,    46,     0,    47,    50,    49,    35,
     101,     0,    96,     0,     0,     0,   317,     0,     0,     0,
      37,    44,    64,    38,    39,    40,    66,    67,    41,    42,
      43,    45,     0,    32,     0,   322,     0,     0,     0,     0,
       0,   248,     0,     0,   279,     0,     0,     0,     0,   243,
     358,   278,     0,   214,     0,   350,   351,     0,     0,     0,
       0,     0,     0,   260,     0,     0,   301,   372,     0,     0,
       0,     0,     0,     0,   382,   383,   384,     0,     0,   385,
       0,     0,     0,     0,   294,   293,   314,     0,     0,   308,
     309,     0,     0,   162,     0,   161,     0,   213,   168,   108,
     110,   109,    53,   208,     0,    57,   144,   175,   176,   170,
     171,    56,   188,   178,   179,   180,   191,   198,   206,   400,
       0,     0,   347,     8,     4,     6,    54,   102,     0,    98,
       0,   114,     0,    54,    65,    52,    63,    62,     0,    60,
       0,   315,     9,    84,    23,    83,     0,    76,     0,    80,
     208,     0,    31,    22,   321,   276,   272,     0,   241,     0,
     238,   357,   132,   356,     0,   354,     0,     0,   250,   267,
     258,     0,   256,   252,     0,   208,   262,   379,   381,   378,
     386,   377,     0,   364,   373,     0,   375,   374,   365,   309,
       0,   310,   306,   165,   166,   167,   160,     0,     0,     0,
       0,     0,    55,    51,    30,   115,     0,    69,    68,    59,
       0,    86,     0,     0,     0,    79,     0,     0,    82,    75,
      74,     0,     0,     0,     0,     0,     0,   261,   257,     0,
       0,     0,   376,     0,   368,     0,     0,     0,   163,     0,
     397,     0,     0,    99,    54,    61,     0,     0,     0,     0,
       0,     9,    90,     9,     0,    77,     0,    81,    78,   239,
       0,     0,   355,   247,     0,   264,   380,   387,   366,     0,
     312,   164,    58,   395,     0,    70,    95,     0,     9,     0,
       0,     0,    87,    88,    73,     0,     0,     0,     0,     0,
       9,    89,    30,    85,    91,     9,     0,   367,     0,     0,
       0,    93,   399,     0,     0,   104,    94,     0,     0,   105,
       9,     0,     0,   103,   106
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -531,  -531,    -9,  -531,   568,   -66,  -531,  -531,  -531,    -6,
    -531,  -531,  -338,  -531,  -531,  -531,  -531,  -531,  -138,  -531,
    -531,  -531,  -233,  -474,  -334,  -531,  -531,   363,  -531,  -531,
    -531,  -531,  -209,  -531,  -531,  -446,  -531,   361,  -531,  -531,
    -436,  -531,  -531,   267,  -531,  -531,   293,   850,  -531,   569,
    -531,   305,  -531,   285,  -530,   625,  -345,   653,  -195,   644,
    -531,  -121,  -531,   936,  -531,    -3,  -208,   845,   853,  -531,
     508,  -218,  -531,  -531,   841,  -531,  -531,  -531,   770,  -144,
    -531,  -531,   389,  -531,  -531,  -143,  -531,  -531,  -214,  -531,
     617,  -196,  -261,  -117,  -531,  -301,  -164,  -531,  -531,  -531,
    -531,  -531,  -531,  -531,   563,  -531,  -286,  -531,  -531,  -531,
    -531,  -531,  -531,  -531,  -531,  -531,  -531,  -406,  -314,  -531,
    -531,  -531,    70,  -531,  -531,  -531,   840,  -531,  -531,  -531,
      84,    22,  -531,  -531,    35,   -48,  -531,  -531,  -113,  -531,
    -531,    40,  -531,   400,   991,  -531,  -531,   425,    43,  -531,
    -531,   678,   680,    13,  -531,  -531,   438,   -16,  -531,  -531,
    1004,   952,  1013,  -531,  -531,  -531,  -531,  -531,   709,   421,
     420,  -531,   540,  -531,  -531,  -531,   463,  -531,  -531,  -531,
    -531,   956,  -531,  -531,  -531,  -531,  -531,  -531,  -531
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     7,   176,   277,   278,    78,    47,    48,    49,    50,
     165,   294,   439,    51,   159,   288,   420,    52,   619,   406,
     407,   421,   602,   618,   238,   423,   538,   539,   424,   425,
     426,   427,   296,   297,   298,   620,   546,   547,   437,   548,
     549,   429,   543,   650,   651,   682,   652,   160,   285,   286,
     528,   653,   704,   705,   239,   240,   241,   430,    53,   218,
     219,    54,    55,    56,    57,   270,   111,   243,   104,   105,
     244,   245,   106,   107,   263,   246,   247,   359,   248,   249,
     372,   373,   250,   386,   387,   272,   252,   388,   253,   394,
     254,   255,   256,   257,   258,   179,   180,   181,   182,   183,
     184,   185,   186,   305,   306,   307,   308,   447,   187,   188,
     457,   568,   189,   190,   328,   329,   464,   330,   332,   191,
     331,   458,   120,   192,   321,   193,   302,   443,   194,   195,
      58,    59,    95,    91,   333,   138,   139,   213,    85,   211,
      10,    60,   196,    61,    13,   141,   349,   592,    62,   216,
     487,   431,   432,    63,   117,    64,    65,    38,    16,    17,
      18,    77,    19,    20,   226,    66,    67,   322,   455,   564,
     565,   197,    68,    22,    88,   205,   483,   478,   479,    23,
      24,   100,    69,    70,    71,   398,   521,    72,   198
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      33,    46,   178,   342,   484,   251,   448,   271,    74,   366,
     124,   314,   279,    94,   462,   320,    83,   358,   500,   280,
     220,   346,     9,   300,   214,   460,   304,   295,   574,   312,
     502,   374,   353,   103,   356,    87,    27,   626,   389,     9,
      11,   461,   116,    14,    84,   405,    46,   144,   408,    79,
     438,   293,   603,   514,   422,   572,   453,    11,   178,   608,
      14,   282,   178,   315,   283,   364,   362,   623,   134,   126,
      30,   642,   664,    31,   686,   364,   710,   566,   428,   282,
    -284,   708,   336,   282,     8,    25,    26,   561,    28,   148,
     125,   361,   251,   365,    34,   150,   147,   209,   350,   148,
    -284,     8,   545,   149,   453,   150,   586,    32,   210,   101,
     102,   202,   355,   562,   102,   126,   177,   567,   383,   384,
     385,   703,   323,    32,   396,   152,   112,   153,   474,   369,
     545,    83,    46,   316,   148,    32,   125,   152,   317,   153,
     150,   363,   624,    29,   557,   242,   434,   363,   292,   624,
     506,   363,   284,    32,   281,   646,   555,    32,   338,   289,
     340,   370,   281,   221,   206,   301,   303,   223,   628,   301,
     675,   452,   177,   707,   516,   647,   177,   101,   102,    73,
     657,    29,   369,   304,   -92,   224,   371,    89,   225,   259,
     178,   646,   259,   347,  -145,  -145,    35,   284,   122,   518,
     260,   261,    90,   260,   261,   482,    42,    46,   684,   178,
      46,   647,   345,    97,   370,    36,    98,    36,   498,   501,
     251,   504,   500,   600,   -92,   251,   354,   220,   369,    32,
     485,   511,   242,   692,   505,    37,   648,    37,    79,   371,
      99,   221,    42,   513,   504,   148,   369,   672,    29,   149,
     262,   150,   441,   262,   369,   235,    92,   523,   235,   279,
     370,   526,   629,   569,    96,    32,   643,   163,   110,   369,
     532,   533,     1,   534,   594,   531,   595,   164,   370,   440,
     535,    93,   545,   502,   320,   371,   370,   369,    80,   108,
      36,     4,  -208,   504,   552,  -208,   320,   324,   304,   607,
      36,   370,   304,   371,   177,    81,    45,   264,   325,    82,
     133,   371,    32,   601,   126,   109,   326,   399,   685,   370,
     604,   504,   661,   177,   530,   496,   371,   161,   178,   110,
     162,   178,  -208,   580,   497,  -145,   343,    83,   344,    46,
     369,   113,   148,   178,   371,   327,   149,  -208,   150,   251,
     451,   251,    99,   251,   700,   698,   369,   369,   500,   114,
     242,   503,   115,   687,   369,   242,   233,   265,   102,   148,
     221,   200,   370,   365,   606,   150,   599,   101,   102,  -208,
    -208,  -208,  -208,  -208,   515,   696,   369,   259,   370,   370,
     369,   227,   605,   320,   669,   119,   370,   371,   260,   261,
      12,   465,   228,   519,   364,   284,   201,   411,    36,   412,
     281,    45,    42,   371,   371,   281,   486,    12,   370,   544,
     560,   371,   370,   501,   251,    36,   364,   374,   291,   375,
     703,   376,   377,   550,   229,   230,   231,   390,    15,   232,
     178,   135,   177,   371,    42,   177,   127,   371,   262,    46,
     369,   563,   391,   640,   639,    15,   351,   177,   561,   128,
     227,   575,   644,   378,   233,   101,   102,   234,   281,   379,
     235,   228,   136,   504,   504,    36,   612,   584,   585,   587,
     236,   237,   370,   129,   101,   102,   -54,   392,   393,   242,
     689,   242,   251,   242,   137,    37,   140,   320,   674,   130,
     364,     1,   131,   229,   230,   231,   178,   371,   232,   228,
     143,   380,   381,   382,   383,   384,   385,   222,   227,   157,
       4,   101,   102,   320,   148,   -54,   -54,   281,   365,   228,
     150,   145,   320,   233,   101,   102,   234,   146,   151,   235,
      21,   229,   230,   231,   178,   540,   504,   178,   281,   236,
     237,   154,   541,   152,   177,   153,   542,    21,   158,   501,
     251,   229,   230,   357,   242,   199,   232,     1,     2,   178,
     178,   233,   101,   102,   234,   444,   148,   235,   203,   178,
     149,   212,   150,     3,    42,   681,     4,   683,    98,   118,
     227,   233,   101,   102,   234,     5,   207,   235,   208,     1,
       2,   228,   215,   126,     6,   274,   649,   236,   237,    98,
     132,   656,   691,   575,   503,     3,   155,   156,     4,   563,
     177,   563,  -147,  -147,   699,   367,   368,     5,   375,   701,
     376,   377,   242,   229,   230,   596,     6,   275,   232,   148,
     287,   634,   635,   149,   712,   150,   149,   658,   150,   313,
     148,   281,   348,   148,   317,   228,   150,   365,   177,   150,
     290,   177,   378,   233,   101,   102,   234,   337,   379,   235,
      98,   410,   126,   299,   126,   649,   281,   402,   403,   236,
     237,   309,   126,   177,   177,   339,   503,   229,   230,   231,
     126,   318,   126,   177,   334,   228,   383,   384,   385,   335,
     242,   268,   341,   126,   228,    28,   649,   445,   446,   395,
     380,   381,   382,   383,   384,   385,   228,   233,   101,   102,
     234,   493,   494,   235,   536,   537,   352,   229,   230,   231,
     609,   610,   397,   236,   237,   400,   229,   230,   231,   401,
     269,   613,   614,   228,   615,   616,   638,   368,   229,   230,
     231,    98,   679,   232,   404,   228,   433,   233,   101,   102,
     234,   509,   510,   235,   438,   409,   233,   101,   102,   234,
     435,   311,   235,   236,   237,   229,   230,   231,   233,   101,
     102,   234,   236,   237,   235,   436,    36,   229,   230,   231,
     442,   449,   450,   454,   236,   237,   459,   463,   466,   467,
     617,   480,   481,   482,   490,   233,   276,   102,   234,   166,
     488,   235,   492,   491,   495,   507,   508,   233,   101,   102,
     234,   236,   237,   235,   148,   -54,   -54,  -253,   365,   167,
     150,  -253,   512,   236,   237,   520,  -242,  -242,  -242,   522,
     413,   168,  -253,   293,   166,   169,   170,   524,   527,   551,
    -253,   414,   415,   559,   171,   553,   554,   556,   570,   326,
      42,   573,  -253,   172,   167,   576,  -253,   416,   173,   417,
     418,   577,   166,  -270,   578,   319,   168,  -253,   579,  -253,
     169,   170,   364,   174,   102,  -253,   175,   581,   582,   171,
    -253,   541,   167,   583,  -253,    42,   589,   588,   172,   590,
     591,  -352,   593,   173,   168,  -253,   419,   166,   169,   170,
     598,   611,   622,  -253,  -253,   621,   625,   171,   174,   102,
     627,   175,   630,    42,   631,  -253,   172,   167,   632,  -253,
     637,   173,   641,   654,   659,   663,  -251,   665,   666,   168,
    -253,  -352,  -253,   169,   170,   667,   174,   102,  -253,   175,
     671,   668,   171,   680,   677,   670,   690,   693,    42,   673,
     676,   172,   658,   688,   678,   166,   173,   697,   702,   706,
     711,   525,   713,   645,   694,   655,  -251,  -253,   204,   714,
     529,   174,   102,  -253,   175,   167,   695,  -253,   499,   709,
     475,   121,   489,   273,  -353,   266,   413,   168,  -253,   293,
     166,   169,   170,   267,   597,   360,  -253,   468,   469,   558,
     171,   517,   310,    86,   636,   476,    42,   477,  -253,   172,
     167,    75,  -253,   416,   173,   470,   471,   123,   166,   571,
      76,   456,   168,  -253,  -353,  -253,   169,   170,   472,   174,
     102,  -253,   175,   660,   662,   171,  -253,   633,   167,   142,
    -253,    42,     0,     0,   172,     0,     0,  -246,     0,   173,
     168,  -253,   473,   166,   169,   170,     0,     0,     0,  -253,
    -253,     0,     0,   171,   174,   102,     0,   175,     0,    42,
       0,  -253,   172,   167,     0,  -253,     0,   173,     0,     0,
       0,    39,     0,     0,     0,   168,  -253,     0,  -253,   169,
     170,     0,   174,   102,  -253,   175,     0,     0,   171,  -117,
       0,     0,     0,     0,    42,     0,    39,   172,     0,     0,
    -117,     0,   173,     0,    40,     1,     2,     0,     0,     0,
       0,     0,     0,  -253,     0,     0,     0,   174,   102,     0,
     175,    41,    42,    39,     4,  -118,     0,     0,     0,    40,
       1,     2,    39,     0,    43,     0,     0,     0,    44,    45,
       0,  -116,     0,     0,     0,    32,   217,    42,  -118,     4,
       0,     0,  -116,     0,    39,     0,    40,     1,     2,    43,
       0,  -119,     0,    44,    45,    40,     1,     2,     0,     0,
      32,     0,  -116,    41,    42,     0,     4,     0,     0,    39,
       0,     0,   217,    42,  -119,     4,    43,    40,     1,     2,
      44,    45,     0,     0,     0,    43,     0,    32,     0,    44,
      45,     0,     0,     0,    41,    42,    32,     4,  -118,     0,
       0,     0,    40,     1,     2,     0,     0,    43,   229,     0,
     231,    44,    45,     0,     0,     0,     0,     0,    32,   217,
      42,     0,     4,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    43,     0,     0,     0,    44,    45,   233,   101,
     102,   234,     0,    32,   235
};

static const yytype_int16 yycheck[] =
{
       6,    10,   119,   211,   342,   148,   307,   151,    17,   242,
      76,   175,   156,    29,   328,   179,    22,   235,   363,   157,
     141,   216,     0,   167,   137,   326,   170,   165,   464,   173,
     364,     3,   228,    36,   230,    22,     1,   567,   252,    17,
       0,   327,    45,     0,    22,    26,    55,    95,   281,     1,
       9,    17,   526,   387,   287,   461,   317,    17,   175,   533,
      17,     1,   179,     9,     4,    56,    10,    10,    84,    78,
      75,    18,    10,    80,    10,    56,    10,    30,   287,     1,
      60,    30,     4,     1,     0,    75,    76,    49,    20,    80,
      77,   235,   235,    84,     0,    86,    99,    39,   219,    80,
      80,    17,   436,    84,   365,    86,     4,    75,    50,    75,
      76,   127,   229,    75,    76,   124,   119,    70,    90,    91,
      92,    70,   188,    75,   268,    84,    20,    86,   337,    16,
     464,   137,   141,    79,    80,    75,   123,    84,    84,    86,
      86,    85,    85,    75,   445,   148,   290,    85,   164,    85,
     368,    85,   158,    75,   157,     1,   442,    75,   206,   162,
     208,    48,   165,   141,   129,   168,   169,   145,   574,   172,
     644,   315,   175,   703,   388,    21,   179,    75,    76,    79,
     616,    75,    16,   327,    30,   145,    73,    79,   145,    14,
     307,     1,    14,    20,    81,    82,    40,   203,    40,   395,
      25,    26,    79,    25,    26,    40,    52,   216,   654,   326,
     219,    21,   215,    79,    48,    59,    82,    59,   362,   363,
     363,   364,   567,    30,    70,   368,   229,   348,    16,    75,
     343,   374,   235,   679,    56,    79,    46,    79,     1,    73,
      44,   219,    52,   386,   387,    80,    16,    81,    75,    84,
      75,    86,    40,    75,    16,    80,    75,   401,    80,   403,
      48,   405,   576,   458,    75,    75,   604,    22,    75,    16,
     414,   415,    35,   417,   492,   413,   494,    32,    48,   295,
     418,    40,   616,   617,   448,    73,    48,    16,    51,    79,
      59,    54,     3,   436,   438,     6,   460,    23,   442,   532,
      59,    48,   446,    73,   307,    68,    69,    15,    34,    72,
      79,    73,    75,   521,   323,    79,    42,    79,    56,    48,
     528,   464,   623,   326,    22,    72,    73,    79,   445,    75,
      82,   448,    43,   471,    81,    82,    79,   343,    81,   348,
      16,    75,    80,   460,    73,    71,    84,    58,    86,   492,
      79,   494,    44,   496,   692,   689,    16,    16,   703,    75,
     363,   364,    75,   664,    16,   368,    74,    75,    76,    80,
     348,    63,    48,    84,    26,    86,   520,    75,    76,    90,
      91,    92,    93,    94,   387,   686,    16,    14,    48,    48,
      16,     1,   530,   557,     4,    19,    48,    73,    25,    26,
       0,   331,    12,    79,    56,   411,    40,    79,    59,    81,
     413,    69,    52,    73,    73,   418,   346,    17,    48,    79,
      79,    73,    48,   567,   567,    59,    56,     3,    79,     5,
      70,     7,     8,   436,    44,    45,    46,    43,     0,    49,
     557,    79,   445,    73,    52,   448,    75,    73,    75,   458,
      16,   454,    58,    79,   598,    17,    40,   460,    49,    75,
       1,   464,   606,    39,    74,    75,    76,    77,   471,    45,
      80,    12,    79,   616,   617,    59,   542,   480,   481,   482,
      90,    91,    48,    35,    75,    76,    42,    93,    94,   492,
      56,   494,   635,   496,    80,    79,    40,   661,   642,    51,
      56,    35,    54,    44,    45,    46,   623,    73,    49,    12,
      79,    87,    88,    89,    90,    91,    92,    51,     1,    40,
      54,    75,    76,   687,    80,    81,    82,   530,    84,    12,
      86,    81,   696,    74,    75,    76,    77,    75,    69,    80,
       0,    44,    45,    46,   661,    46,   689,   664,   551,    90,
      91,    75,    53,    84,   557,    86,    57,    17,    80,   703,
     703,    44,    45,    46,   567,    30,    49,    35,    36,   686,
     687,    74,    75,    76,    77,    79,    80,    80,    80,   696,
      84,    79,    86,    51,    52,   651,    54,   653,    82,    83,
       1,    74,    75,    76,    77,    63,    75,    80,    75,    35,
      36,    12,    60,   612,    72,    75,   612,    90,    91,    82,
      83,   614,   678,   616,   617,    51,    79,    80,    54,   622,
     623,   624,    81,    82,   690,    81,    82,    63,     5,   695,
       7,     8,   635,    44,    45,    46,    72,    40,    49,    80,
      40,    79,    80,    84,   710,    86,    84,     4,    86,    83,
      80,   654,    53,    80,    84,    12,    86,    84,   661,    86,
       9,   664,    39,    74,    75,    76,    77,    40,    45,    80,
      82,    83,   681,    79,   683,   681,   679,    81,    82,    90,
      91,    79,   691,   686,   687,    40,   689,    44,    45,    46,
     699,    79,   701,   696,    79,    12,    90,    91,    92,    79,
     703,    18,    50,   712,    12,    20,   712,    28,    29,     6,
      87,    88,    89,    90,    91,    92,    12,    74,    75,    76,
      77,    81,    82,    80,    74,    75,    40,    44,    45,    46,
      81,    82,    18,    90,    91,    63,    44,    45,    46,    10,
      57,    81,    82,    12,    81,    82,    81,    82,    44,    45,
      46,    82,    83,    49,    79,    12,    79,    74,    75,    76,
      77,   372,   373,    80,     9,    81,    74,    75,    76,    77,
      79,    79,    80,    90,    91,    44,    45,    46,    74,    75,
      76,    77,    90,    91,    80,    80,    59,    44,    45,    46,
      70,    67,    79,    70,    90,    91,    75,    61,    79,    81,
      56,    60,    44,    40,    30,    74,    75,    76,    77,     1,
      75,    80,    82,    57,    81,    67,    28,    74,    75,    76,
      77,    90,    91,    80,    80,    81,    82,    19,    84,    21,
      86,    23,    39,    90,    91,    43,    28,    29,    30,    79,
      14,    33,    34,    17,     1,    37,    38,    79,    14,    47,
      42,    25,    26,    30,    46,    79,    79,    79,    39,    42,
      52,    79,    19,    55,    21,    30,    23,    41,    60,    43,
      44,     4,     1,    30,     4,    32,    33,    34,     4,    71,
      37,    38,    56,    75,    76,    42,    78,     4,     4,    46,
      19,    53,    21,    79,    23,    52,    30,    79,    55,    40,
      75,    30,    81,    60,    33,    34,    80,     1,    37,    38,
      80,    57,    83,    42,    71,    38,    21,    46,    75,    76,
      42,    78,    26,    52,    72,    19,    55,    21,    81,    23,
      57,    60,    57,    47,    79,    79,    30,    79,     4,    33,
      34,    70,    71,    37,    38,    53,    75,    76,    42,    78,
      81,    79,    46,    30,    75,    79,    40,    57,    52,    79,
      79,    55,     4,    81,    79,     1,    60,    79,    79,    79,
      21,   403,    79,   610,   681,   614,    70,    71,   128,   712,
     411,    75,    76,    19,    78,    21,   681,    23,   363,   704,
     337,    55,   348,   152,    30,   150,    14,    33,    34,    17,
       1,    37,    38,   150,   496,   235,    42,    25,    26,   446,
      46,   394,   172,    22,   589,   337,    52,   337,    19,    55,
      21,    17,    23,    41,    60,    43,    44,    75,     1,    30,
      17,   322,    33,    34,    70,    71,    37,    38,    56,    75,
      76,    42,    78,   622,   624,    46,    19,   584,    21,    93,
      23,    52,    -1,    -1,    55,    -1,    -1,    30,    -1,    60,
      33,    34,    80,     1,    37,    38,    -1,    -1,    -1,    42,
      71,    -1,    -1,    46,    75,    76,    -1,    78,    -1,    52,
      -1,    19,    55,    21,    -1,    23,    -1,    60,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    33,    34,    -1,    71,    37,
      38,    -1,    75,    76,    42,    78,    -1,    -1,    46,    19,
      -1,    -1,    -1,    -1,    52,    -1,     1,    55,    -1,    -1,
      30,    -1,    60,    -1,    34,    35,    36,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    75,    76,    -1,
      78,    51,    52,     1,    54,    30,    -1,    -1,    -1,    34,
      35,    36,     1,    -1,    64,    -1,    -1,    -1,    68,    69,
      -1,    19,    -1,    -1,    -1,    75,    51,    52,    53,    54,
      -1,    -1,    30,    -1,     1,    -1,    34,    35,    36,    64,
      -1,    30,    -1,    68,    69,    34,    35,    36,    -1,    -1,
      75,    -1,    19,    51,    52,    -1,    54,    -1,    -1,     1,
      -1,    -1,    51,    52,    53,    54,    64,    34,    35,    36,
      68,    69,    -1,    -1,    -1,    64,    -1,    75,    -1,    68,
      69,    -1,    -1,    -1,    51,    52,    75,    54,    30,    -1,
      -1,    -1,    34,    35,    36,    -1,    -1,    64,    44,    -1,
      46,    68,    69,    -1,    -1,    -1,    -1,    -1,    75,    51,
      52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    64,    -1,    -1,    -1,    68,    69,    74,    75,
      76,    77,    -1,    75,    80
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
     236,   238,   243,   248,   250,   251,   260,   261,   267,   277,
     278,   279,   282,    79,    97,   255,   257,   256,   100,     1,
      51,    68,    72,   104,   226,   233,   239,   248,   269,    79,
      79,   228,    75,    40,   252,   227,    75,    79,    82,    44,
     276,    75,    76,   160,   163,   164,   167,   168,    79,    79,
      75,   161,    20,    75,    75,    75,   160,   249,    83,    19,
     217,   158,    40,   256,   100,   248,    97,    75,    75,    35,
      51,    54,    83,    79,   252,    79,    79,    80,   230,   231,
      40,   240,   276,    79,   230,    81,    75,   160,    80,    84,
      86,    69,    84,    86,    75,    79,    80,    40,    80,   109,
     142,    79,    82,    22,    32,   105,     1,    21,    33,    37,
      38,    46,    55,    60,    75,    78,    97,   160,   188,   190,
     191,   192,   193,   194,   195,   196,   197,   203,   204,   207,
     208,   214,   218,   220,   223,   224,   237,   266,   283,    30,
      63,    40,   252,    80,   142,   270,   229,    75,    75,    39,
      50,   234,    79,   232,   233,    60,   244,    51,   154,   155,
     156,   226,    51,   226,   236,   243,   259,     1,    12,    44,
      45,    46,    49,    74,    77,    80,    90,    91,   119,   149,
     150,   151,   160,   162,   165,   166,   170,   171,   173,   174,
     177,   180,   181,   183,   185,   186,   187,   188,   189,    14,
      25,    26,    75,   169,    15,    75,   162,   163,    18,    57,
     160,   174,   180,   169,    75,    40,    75,    98,    99,   174,
     113,   160,     1,     4,   104,   143,   144,    40,   110,   160,
       9,    79,   252,    17,   106,   113,   127,   128,   129,    79,
     174,   160,   221,   160,   174,   198,   199,   200,   201,    79,
     221,    79,   174,    83,   191,     9,    79,    84,    79,    32,
     191,   219,   262,   100,    23,    34,    42,    71,   209,   210,
     212,   215,   213,   229,    79,    79,     4,    40,   230,    40,
     230,    50,   161,    79,    81,   160,   153,    20,    53,   241,
     156,    40,    40,   186,   160,   188,   186,    46,   166,   172,
     173,   174,    10,    85,    56,    84,   117,    81,    82,    16,
      48,    73,   175,   176,     3,     5,     7,     8,    39,    45,
      87,    88,    89,    90,    91,    92,   178,   179,   182,   183,
      43,    58,    93,    94,   184,     6,   174,    18,   280,    79,
      63,    10,    81,    82,    79,    26,   114,   115,   117,    81,
      83,    79,    81,    14,    25,    26,    41,    43,    44,    80,
     111,   116,   117,   120,   123,   124,   125,   126,   127,   136,
     152,   246,   247,    79,   174,    79,    80,   133,     9,   107,
     252,    40,    70,   222,    79,    28,    29,   202,   190,    67,
      79,    79,   174,   187,    70,   263,   263,   205,   216,    75,
     190,   201,   213,    61,   211,   217,    79,    81,    25,    26,
      43,    44,    56,    80,   127,   152,   246,   247,   272,   273,
      60,    44,    40,   271,   107,   233,   217,   245,    75,   154,
      30,    57,    82,    81,    82,    81,    72,    81,   174,   150,
     151,   174,   119,   160,   180,    56,   166,    67,    28,   177,
     177,   180,    39,   180,   119,   160,   183,   185,   186,    79,
      43,   281,    79,   174,    79,    99,   174,    14,   145,   144,
      22,   113,   174,   174,   174,   113,    74,    75,   121,   122,
      46,    53,    57,   137,    79,   119,   131,   132,   134,   135,
     160,    47,   174,    79,    79,   201,    79,   190,   199,    30,
      79,    49,    75,   160,   264,   265,    30,    70,   206,   153,
      39,    30,   212,    79,   135,   160,    30,     4,     4,     4,
     113,     4,     4,    79,   160,   160,     4,   160,    79,    30,
      40,    75,   242,    81,   166,   166,    46,   165,    80,   174,
      30,   161,   117,   118,   161,   113,    26,   117,   118,    81,
      82,    57,   100,    81,    82,    81,    82,    56,   118,   113,
     130,    38,    83,    10,    85,    21,   149,    42,   212,   213,
      26,    72,    81,   271,    79,    80,   242,    57,    81,   174,
      79,    57,    18,   107,   174,   122,     1,    21,    46,   104,
     138,   139,   141,   146,    47,   132,   160,   135,     4,    79,
     264,   190,   265,    79,    10,    79,     4,    53,    79,     4,
      79,    81,    81,    79,   174,   118,    79,    75,    79,    83,
      30,   100,   140,   100,   130,    56,    10,   190,    81,    56,
      40,   100,   130,    57,   141,   146,   190,    79,   119,   100,
     107,   100,    79,    70,   147,   148,    79,   149,    30,   148,
      10,    21,   100,    79,   138
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
     242,   244,   243,   245,   245,   246,   247,   247,   248,   249,
     249,   250,   250,   250,   251,   251,   251,   251,   252,   253,
     253,   254,   254,   254,   255,   256,   256,   257,   257,   257,
     257,   257,   257,   257,   258,   259,   259,   260,   260,   261,
     262,   262,   263,   263,   264,   264,   265,   265,   266,   267,
     267,   268,   268,   269,   269,   269,   269,   269,   269,   269,
     270,   270,   270,   271,   271,   271,   272,   272,   272,   272,
     272,   272,   272,   272,   272,   272,   273,   273,   274,   275,
     276,   277,   277,   277,   278,   279,   280,   280,   281,   281,
     282,   283
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
       1,     0,    10,     0,     1,     2,     0,     1,     3,     1,
       3,     6,     5,     1,     4,     3,     5,     4,     2,     3,
       2,     2,     3,     2,     3,     0,     2,     1,     1,     1,
       1,     1,     1,     1,     5,     1,     1,     6,     4,     4,
       2,     2,     4,     6,     1,     3,     1,     1,     3,     3,
       3,     1,     2,     2,     6,     6,     8,    10,     7,     1,
       0,     1,     3,     0,     2,     2,     3,     2,     2,     2,
       4,     2,     1,     1,     1,     1,     2,     4,     3,     4,
       2,     1,     1,     1,     5,     9,     0,     4,     0,     7,
       6,     2
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
#line 247 "grammar83.y"
{
    yylloc = 1;
    memset(context, 0, sizeof(*context));
    context->symbol_table = calloc(64, sizeof(Declaration*));
    context->symbol_table_capacity = 64;
    context->symbol_table_size = 0;
    if(!universal_int_type.name) {
        universal_int_type.name = string_pool_c_str_to_token("universal_integer");
    }
    if(!boolean_type.name) {
        boolean_type.name = string_pool_c_str_to_token("Boolean");
        EnumLiteral* literals = calloc(2, sizeof(EnumLiteral));
        literals[0].base.kind = DECL_ENUM_LIT;
        literals[0].name = string_pool_c_str_to_token("False");
        literals[1] = literals[0];
        literals[1].name = string_pool_c_str_to_token("True");
        boolean_type.u.enum_.literals = literals;
        boolean_type.u.enum_.literal_count = 2;
    }
    push_declaration(context, &boolean_type.base);
    push_declaration(context, &boolean_type.u.enum_.literals[false].base);
    push_declaration(context, &boolean_type.u.enum_.literals[true].base);
}

#line 2502 "grammar83.tab.c"

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
#line 273 "grammar83.y"
                        { context->comp_unit = (yyvsp[0].comp_unit); }
#line 2722 "grammar83.tab.c"
    break;

  case 13: /* decl: type_decl  */
#line 299 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), (yyvsp[0].decl));
    }
#line 2731 "grammar83.tab.c"
    break;

  case 14: /* decl: subtype_decl  */
#line 303 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), (yyvsp[0].decl));
    }
#line 2740 "grammar83.tab.c"
    break;

  case 15: /* decl: subprog_decl  */
#line 307 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), &(yyvsp[0].subprogram_decl)->base);
    }
#line 2749 "grammar83.tab.c"
    break;

  case 16: /* decl: pkg_decl  */
#line 311 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), &(yyvsp[0].pkg_spec)->base);
    }
#line 2758 "grammar83.tab.c"
    break;

  case 22: /* object_decl: def_id_s ':' object_qualifier_opt object_subtype_def init_opt ';'  */
#line 323 "grammar83.y"
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
#line 2787 "grammar83.tab.c"
    break;

  case 23: /* number_decl: def_id_s ':' CONSTANT IS_ASSIGNED expression ';'  */
#line 349 "grammar83.y"
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
#line 2805 "grammar83.tab.c"
    break;

  case 24: /* def_id_s: identifier  */
#line 364 "grammar83.y"
               {
        StringTokenArray_init(&(yyval.str_token_array));
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2814 "grammar83.tab.c"
    break;

  case 25: /* def_id_s: def_id_s ',' identifier  */
#line 368 "grammar83.y"
                            { StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token)); }
#line 2820 "grammar83.tab.c"
    break;

  case 26: /* object_qualifier_opt: %empty  */
#line 373 "grammar83.y"
             { (yyval.bool_) = false; }
#line 2826 "grammar83.tab.c"
    break;

  case 27: /* object_qualifier_opt: CONSTANT  */
#line 374 "grammar83.y"
             { (yyval.bool_) = true; }
#line 2832 "grammar83.tab.c"
    break;

  case 30: /* init_opt: %empty  */
#line 383 "grammar83.y"
                           { (yyval.expr) = NULL; }
#line 2838 "grammar83.tab.c"
    break;

  case 31: /* init_opt: IS_ASSIGNED expression  */
#line 384 "grammar83.y"
                           { (yyval.expr) = (yyvsp[0].expr); }
#line 2844 "grammar83.tab.c"
    break;

  case 32: /* type_decl: TYPE identifier discrim_part_opt type_completion ';'  */
#line 388 "grammar83.y"
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
#line 2859 "grammar83.tab.c"
    break;

  case 37: /* type_completion: IS type_def  */
#line 408 "grammar83.y"
                { (yyval.type_decl) = (yyvsp[0].type_decl); }
#line 2865 "grammar83.tab.c"
    break;

  case 46: /* subtype_decl: SUBTYPE identifier IS subtype_ind ';'  */
#line 423 "grammar83.y"
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
#line 2884 "grammar83.tab.c"
    break;

  case 47: /* subtype_ind: name constraint  */
#line 440 "grammar83.y"
                    {
        // TODO: propagate constraint somehow
        (yyval.str_token) = (yyvsp[-1].name).name;
    }
#line 2893 "grammar83.tab.c"
    break;

  case 48: /* subtype_ind: name  */
#line 444 "grammar83.y"
         { (yyval.str_token) = (yyvsp[0].name).name; }
#line 2899 "grammar83.tab.c"
    break;

  case 52: /* derived_type: NEW subtype_ind  */
#line 457 "grammar83.y"
                    {
        (yyval.type_decl) = create_type_decl(TYPE_DERIVED);
        TypeDecl* base_type = find_type_decl(context, (yyvsp[0].str_token));
        if(!base_type) {
            error_print((yyloc), "Unknown base type: %s", ST((yyvsp[0].str_token)));
            error_exit();
        }
        (yyval.type_decl)->u.subtype.base = base_type;
    }
#line 2913 "grammar83.tab.c"
    break;

  case 53: /* range_constraint: RANGE range  */
#line 468 "grammar83.y"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 2919 "grammar83.tab.c"
    break;

  case 54: /* range_constr_opt: %empty  */
#line 472 "grammar83.y"
           { (yyval.expr) = NULL; }
#line 2925 "grammar83.tab.c"
    break;

  case 56: /* range: simple_expression DOT_DOT simple_expression  */
#line 477 "grammar83.y"
                                                { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_RANGE, (yyvsp[0].expr)); }
#line 2931 "grammar83.tab.c"
    break;

  case 59: /* enumeration_type: '(' enum_id_s ')'  */
#line 483 "grammar83.y"
                      {
        (yyval.type_decl) = create_type_decl(TYPE_ENUM);
        (yyval.type_decl)->u.enum_.literals = (yyvsp[-1].enum_literals).data;
        uint32_t literal_count = EnumLiteralArray_size(&(yyvsp[-1].enum_literals));
        (yyval.type_decl)->u.enum_.literal_count = literal_count;
        for(uint32_t i = 0; i < literal_count; ++i) {
            push_declaration(context, &(yyval.type_decl)->u.enum_.literals[i].base);
        }
    }
#line 2945 "grammar83.tab.c"
    break;

  case 60: /* enum_id_s: enum_id  */
#line 494 "grammar83.y"
            {
        EnumLiteralArray_init(&(yyval.enum_literals));
        EnumLiteralArray_append(&(yyval.enum_literals), (yyvsp[0].enum_literal));
    }
#line 2954 "grammar83.tab.c"
    break;

  case 61: /* enum_id_s: enum_id_s ',' enum_id  */
#line 498 "grammar83.y"
                          {
        (yyval.enum_literals) = (yyvsp[-2].enum_literals);
        EnumLiteralArray_append(&(yyval.enum_literals), (yyvsp[0].enum_literal));
    }
#line 2963 "grammar83.tab.c"
    break;

  case 62: /* enum_id: identifier  */
#line 504 "grammar83.y"
               {
        memset(&(yyval.enum_literal), 0, sizeof((yyval.enum_literal)));
        (yyval.enum_literal).base.kind = DECL_ENUM_LIT;
        (yyval.enum_literal).base.line_num = (yyloc);
        (yyval.enum_literal).name = (yyvsp[0].str_token);
        (yyval.enum_literal).is_char_lit = false;
    }
#line 2975 "grammar83.tab.c"
    break;

  case 63: /* enum_id: char_lit  */
#line 511 "grammar83.y"
             {
        memset(&(yyval.enum_literal), 0, sizeof((yyval.enum_literal)));
        (yyval.enum_literal).base.kind = DECL_ENUM_LIT;
        (yyval.enum_literal).base.line_num = (yyloc);
        char buffer[3] = {0};
        buffer[0] = '\'';
        buffer[1] = (yyvsp[0].c);
        buffer[2] = '\'';
        StringView literal_text = { .value = buffer, .len = sizeof(buffer) };
        (yyval.enum_literal).name = string_pool_to_token(literal_text);
        (yyval.enum_literal).is_char_lit = true;
    }
#line 2992 "grammar83.tab.c"
    break;

  case 64: /* integer_type: range_constraint  */
#line 525 "grammar83.y"
                     {
        (yyval.type_decl) = create_type_decl(TYPE_INTEGER);
        (yyval.type_decl)->u.int_.range = (yyvsp[0].expr);
    }
#line 3001 "grammar83.tab.c"
    break;

  case 107: /* choice_s: choice  */
#line 649 "grammar83.y"
                        {
        ChoiceArray_init(&(yyval.choice_array));
        ChoiceArray_append(&(yyval.choice_array), (yyvsp[0].choice));
    }
#line 3010 "grammar83.tab.c"
    break;

  case 108: /* choice_s: choice_s '|' choice  */
#line 653 "grammar83.y"
                        {
        (yyval.choice_array) = (yyvsp[-2].choice_array);
        ChoiceArray_append(&(yyval.choice_array), (yyvsp[0].choice));
    }
#line 3019 "grammar83.tab.c"
    break;

  case 109: /* choice: expression  */
#line 659 "grammar83.y"
                         {
        (yyval.choice).kind = CHOICE_EXPR;
        (yyval.choice).u.expr = (yyvsp[0].expr);
    }
#line 3028 "grammar83.tab.c"
    break;

  case 111: /* choice: OTHERS  */
#line 664 "grammar83.y"
                         { (yyval.choice).kind = CHOICE_OTHERS; }
#line 3034 "grammar83.tab.c"
    break;

  case 116: /* decl_part: %empty  */
#line 678 "grammar83.y"
                         { (yyval.decl) = NULL; }
#line 3040 "grammar83.tab.c"
    break;

  case 117: /* decl_part: decl_item_or_body_s1  */
#line 679 "grammar83.y"
                         { (yyval.decl) = (yyvsp[0].decl_list).first; }
#line 3046 "grammar83.tab.c"
    break;

  case 118: /* decl_item_s: %empty  */
#line 683 "grammar83.y"
                 { (yyval.decl) = NULL; }
#line 3052 "grammar83.tab.c"
    break;

  case 119: /* decl_item_s: decl_item_s1  */
#line 684 "grammar83.y"
                 { (yyval.decl) = (yyvsp[0].decl_list).first; }
#line 3058 "grammar83.tab.c"
    break;

  case 121: /* decl_item_s1: decl_item_s1 decl_item  */
#line 689 "grammar83.y"
                           {
        DeclList_splice(&(yyvsp[-1].decl_list), &(yyvsp[0].decl_list));
        (yyval.decl_list) = (yyvsp[-1].decl_list);
    }
#line 3067 "grammar83.tab.c"
    break;

  case 127: /* decl_item_or_body_s1: decl_item_or_body_s1 decl_item_or_body  */
#line 703 "grammar83.y"
                                           {
        DeclList_splice(&(yyvsp[-1].decl_list), &(yyvsp[0].decl_list));
        (yyval.decl_list) = (yyvsp[-1].decl_list);
    }
#line 3076 "grammar83.tab.c"
    break;

  case 128: /* decl_item_or_body: body  */
#line 709 "grammar83.y"
              {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), (yyvsp[0].decl));
    }
#line 3085 "grammar83.tab.c"
    break;

  case 130: /* body: subprog_body  */
#line 717 "grammar83.y"
                 { (yyval.decl) = &(yyvsp[0].subprogram_decl)->base; }
#line 3091 "grammar83.tab.c"
    break;

  case 131: /* body: pkg_body  */
#line 718 "grammar83.y"
                 { (yyval.decl) = &(yyvsp[0].pkg_body)->base; }
#line 3097 "grammar83.tab.c"
    break;

  case 132: /* name: identifier  */
#line 722 "grammar83.y"
               {
        memset(&(yyval.name), 0, sizeof((yyval.name)));
        (yyval.name).name = (yyvsp[0].str_token);
    }
#line 3106 "grammar83.tab.c"
    break;

  case 136: /* name: operator_symbol  */
#line 729 "grammar83.y"
                    {
        memset(&(yyval.name), 0, sizeof((yyval.name)));
        (yyval.name).name = (yyvsp[0].str_token);
        //TODO: lookup operator, determine its arity, and allocate args array
    }
#line 3116 "grammar83.tab.c"
    break;

  case 140: /* used_char: char_lit  */
#line 742 "grammar83.y"
             {
        (yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
        (yyval.expr)->u.char_lit = (yyvsp[0].c);
    }
#line 3125 "grammar83.tab.c"
    break;

  case 158: /* literal: numeric_lit  */
#line 786 "grammar83.y"
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
#line 3149 "grammar83.tab.c"
    break;

  case 170: /* expression: expression logical relation  */
#line 828 "grammar83.y"
                                      { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3155 "grammar83.tab.c"
    break;

  case 171: /* expression: expression short_circuit relation  */
#line 829 "grammar83.y"
                                      { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3161 "grammar83.tab.c"
    break;

  case 172: /* logical: AND  */
#line 833 "grammar83.y"
        { (yyval.unary_op) = OP_AND; }
#line 3167 "grammar83.tab.c"
    break;

  case 173: /* logical: OR  */
#line 834 "grammar83.y"
        { (yyval.unary_op) = OP_OR; }
#line 3173 "grammar83.tab.c"
    break;

  case 174: /* logical: XOR  */
#line 835 "grammar83.y"
        { (yyval.unary_op) = OP_XOR; }
#line 3179 "grammar83.tab.c"
    break;

  case 175: /* short_circuit: AND THEN  */
#line 839 "grammar83.y"
               { (yyval.unary_op) = OP_AND_THEN; }
#line 3185 "grammar83.tab.c"
    break;

  case 176: /* short_circuit: OR ELSE  */
#line 840 "grammar83.y"
               { (yyval.unary_op) = OP_OR_ELSE; }
#line 3191 "grammar83.tab.c"
    break;

  case 178: /* relation: simple_expression relational simple_expression  */
#line 846 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3197 "grammar83.tab.c"
    break;

  case 179: /* relation: simple_expression membership range  */
#line 847 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3203 "grammar83.tab.c"
    break;

  case 180: /* relation: simple_expression membership name  */
#line 848 "grammar83.y"
                                                   {
        Expression* right = create_expr(EXPR_NAME, (yylsp[0]));
        right->u.name = (yyvsp[0].name);
        (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), right);
    }
#line 3213 "grammar83.tab.c"
    break;

  case 181: /* relational: '='  */
#line 855 "grammar83.y"
          { (yyval.unary_op) = OP_EQ; }
#line 3219 "grammar83.tab.c"
    break;

  case 182: /* relational: NE  */
#line 856 "grammar83.y"
          { (yyval.unary_op) = OP_NEQ; }
#line 3225 "grammar83.tab.c"
    break;

  case 183: /* relational: '<'  */
#line 857 "grammar83.y"
          { (yyval.unary_op) = OP_LT; }
#line 3231 "grammar83.tab.c"
    break;

  case 184: /* relational: LT_EQ  */
#line 858 "grammar83.y"
          { (yyval.unary_op) = OP_LTE; }
#line 3237 "grammar83.tab.c"
    break;

  case 185: /* relational: '>'  */
#line 859 "grammar83.y"
          { (yyval.unary_op) = OP_GT; }
#line 3243 "grammar83.tab.c"
    break;

  case 186: /* relational: GE  */
#line 860 "grammar83.y"
          { (yyval.unary_op) = OP_GTE; }
#line 3249 "grammar83.tab.c"
    break;

  case 187: /* membership: IN  */
#line 864 "grammar83.y"
           { (yyval.unary_op) = OP_IN; }
#line 3255 "grammar83.tab.c"
    break;

  case 188: /* membership: NOT IN  */
#line 865 "grammar83.y"
           { (yyval.unary_op) = OP_NOT_IN; }
#line 3261 "grammar83.tab.c"
    break;

  case 190: /* simple_expression: unary term  */
#line 870 "grammar83.y"
                                  { (yyval.expr) = make_unary_expr((yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3267 "grammar83.tab.c"
    break;

  case 191: /* simple_expression: simple_expression adding term  */
#line 871 "grammar83.y"
                                  { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3273 "grammar83.tab.c"
    break;

  case 192: /* unary: '+'  */
#line 875 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_PLUS; }
#line 3279 "grammar83.tab.c"
    break;

  case 193: /* unary: '-'  */
#line 876 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_MINUS; }
#line 3285 "grammar83.tab.c"
    break;

  case 194: /* adding: '+'  */
#line 880 "grammar83.y"
        { (yyval.unary_op) = OP_PLUS; }
#line 3291 "grammar83.tab.c"
    break;

  case 195: /* adding: '-'  */
#line 881 "grammar83.y"
        { (yyval.unary_op) = OP_MINUS; }
#line 3297 "grammar83.tab.c"
    break;

  case 196: /* adding: '&'  */
#line 882 "grammar83.y"
        { (yyval.unary_op) = OP_AMP; }
#line 3303 "grammar83.tab.c"
    break;

  case 198: /* term: term multiplying factor  */
#line 887 "grammar83.y"
                            { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3309 "grammar83.tab.c"
    break;

  case 199: /* multiplying: '*'  */
#line 891 "grammar83.y"
        { (yyval.unary_op) = OP_MULT; }
#line 3315 "grammar83.tab.c"
    break;

  case 200: /* multiplying: '/'  */
#line 892 "grammar83.y"
        { (yyval.unary_op) = OP_DIVIDE; }
#line 3321 "grammar83.tab.c"
    break;

  case 201: /* multiplying: MOD  */
#line 893 "grammar83.y"
        { (yyval.unary_op) = OP_MOD; }
#line 3327 "grammar83.tab.c"
    break;

  case 202: /* multiplying: REM  */
#line 894 "grammar83.y"
        { (yyval.unary_op) = OP_REM; }
#line 3333 "grammar83.tab.c"
    break;

  case 204: /* factor: NOT primary  */
#line 899 "grammar83.y"
                          { (yyval.expr) = make_unary_expr(OP_NOT, (yyvsp[0].expr)); }
#line 3339 "grammar83.tab.c"
    break;

  case 205: /* factor: ABS primary  */
#line 900 "grammar83.y"
                          { (yyval.expr) = make_unary_expr(OP_ABS, (yyvsp[0].expr)); }
#line 3345 "grammar83.tab.c"
    break;

  case 206: /* factor: primary EXPON primary  */
#line 901 "grammar83.y"
                          { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_EXP, (yyvsp[0].expr)); }
#line 3351 "grammar83.tab.c"
    break;

  case 208: /* primary: name  */
#line 906 "grammar83.y"
         {
        (yyval.expr) = create_expr(EXPR_NAME, (yyloc));
        (yyval.expr)->u.name = (yyvsp[0].name);
    }
#line 3360 "grammar83.tab.c"
    break;

  case 213: /* parenthesized_primary: '(' expression ')'  */
#line 917 "grammar83.y"
                       { (yyval.expr) = (yyvsp[-1].expr); }
#line 3366 "grammar83.tab.c"
    break;

  case 214: /* qualified: name '\'' parenthesized_primary  */
#line 921 "grammar83.y"
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
#line 3383 "grammar83.tab.c"
    break;

  case 217: /* statement_s: statement  */
#line 940 "grammar83.y"
                          {
        memset(&(yyval.stmt_list), 0, sizeof((yyval.stmt_list)));
        StmtList_append(&(yyval.stmt_list), (yyvsp[0].stmt));
    }
#line 3392 "grammar83.tab.c"
    break;

  case 218: /* statement_s: statement_s statement  */
#line 944 "grammar83.y"
                          {
        StmtList_append(&(yyvsp[-1].stmt_list), (yyvsp[0].stmt));
        (yyval.stmt_list) = (yyvsp[-1].stmt_list);
    }
#line 3401 "grammar83.tab.c"
    break;

  case 220: /* statement: goto_label statement  */
#line 951 "grammar83.y"
                         {
        check_for_redefinition(context, (yyvsp[-1].str_token), (yylsp[-1]));
        LabelDecl* label = create_label((yyvsp[-1].str_token), (yylsp[-1]));
        push_declaration(context, (Declaration*)label);
        (yyval.stmt) = (yyvsp[0].stmt);
    }
#line 3412 "grammar83.tab.c"
    break;

  case 237: /* null_stmt: NuLL ';'  */
#line 984 "grammar83.y"
             { (yyval.stmt) = create_stmt(STMT_NULL, (yyloc)); }
#line 3418 "grammar83.tab.c"
    break;

  case 238: /* assign_stmt: name IS_ASSIGNED expression ';'  */
#line 988 "grammar83.y"
                                    {
        (yyval.stmt) = create_stmt(STMT_ASSIGN, (yyloc));
        (yyval.stmt)->u.assign.dest.kind = EXPR_NAME;
        (yyval.stmt)->u.assign.dest.line_num = (yyloc);
        (yyval.stmt)->u.assign.dest.u.name = (yyvsp[-3].name);
        (yyval.stmt)->u.assign.expr = (yyvsp[-1].expr);
    }
#line 3430 "grammar83.tab.c"
    break;

  case 239: /* if_stmt: IF cond_clause_s else_opt END IF ';'  */
#line 997 "grammar83.y"
                                         {
        (yyval.stmt) = (yyvsp[-4].stmt);
        Statement* branch = (yyvsp[-4].stmt);
        while(branch->u.if_.else_) {
            branch = branch->u.if_.else_;
            assert(branch->kind == STMT_IF);
        }
        branch->u.if_.else_ = (yyvsp[-3].stmt);
    }
#line 3444 "grammar83.tab.c"
    break;

  case 241: /* cond_clause_s: cond_clause_s ELSIF cond_clause  */
#line 1009 "grammar83.y"
                                    {
        (yyval.stmt) = (yyvsp[-2].stmt);
        (yyval.stmt)->u.if_.else_ = (yyvsp[0].stmt);
    }
#line 3453 "grammar83.tab.c"
    break;

  case 242: /* cond_clause: cond_part statement_s  */
#line 1015 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_IF, (yyloc));
        (yyval.stmt)->u.if_.condition = (yyvsp[-1].expr);
        (yyval.stmt)->u.if_.stmts = (yyvsp[0].stmt_list).first;
    }
#line 3463 "grammar83.tab.c"
    break;

  case 243: /* cond_part: condition THEN  */
#line 1022 "grammar83.y"
                   { (yyval.expr) = (yyvsp[-1].expr); }
#line 3469 "grammar83.tab.c"
    break;

  case 245: /* else_opt: %empty  */
#line 1030 "grammar83.y"
                     { (yyval.stmt) = NULL; }
#line 3475 "grammar83.tab.c"
    break;

  case 246: /* else_opt: ELSE statement_s  */
#line 1031 "grammar83.y"
                     { (yyval.stmt) = (yyvsp[0].stmt_list).first; }
#line 3481 "grammar83.tab.c"
    break;

  case 247: /* case_stmt: case_hdr pragma_s alternative_s END CASE ';'  */
#line 1035 "grammar83.y"
                                                 {
        (yyval.stmt) = (yyvsp[-5].stmt);
        // TODO: pragmas
        (yyval.stmt)->u.case_.cases = (yyvsp[-3].case_list).first;
    }
#line 3491 "grammar83.tab.c"
    break;

  case 248: /* case_hdr: CASE expression IS  */
#line 1042 "grammar83.y"
                       {
        (yyval.stmt) = create_stmt(STMT_CASE, (yyloc));
        (yyval.stmt)->u.case_.expr = (yyvsp[-1].expr);
    }
#line 3500 "grammar83.tab.c"
    break;

  case 249: /* alternative_s: %empty  */
#line 1048 "grammar83.y"
                              { memset(&(yyval.case_list), 0, sizeof((yyval.case_list))); }
#line 3506 "grammar83.tab.c"
    break;

  case 250: /* alternative_s: alternative_s alternative  */
#line 1049 "grammar83.y"
                              {
        (yyval.case_list) = (yyvsp[-1].case_list);
        AltList_append(&(yyval.case_list), (yyvsp[0].case_));
    }
#line 3515 "grammar83.tab.c"
    break;

  case 251: /* alternative: WHEN choice_s RIGHT_SHAFT statement_s  */
#line 1055 "grammar83.y"
                                          {
        (yyval.case_) = calloc(1, sizeof(Alternative));
        (yyval.case_)->choices.choices = (yyvsp[-2].choice_array).data;
        (yyval.case_)->choices.count = ChoiceArray_size(&(yyvsp[-2].choice_array));
        (yyval.case_)->stmts = (yyvsp[0].stmt_list).first;
    }
#line 3526 "grammar83.tab.c"
    break;

  case 252: /* loop_stmt: label_opt loop_content id_opt ';'  */
#line 1064 "grammar83.y"
                                      { (yyval.stmt) = (yyvsp[-2].stmt); }
#line 3532 "grammar83.tab.c"
    break;

  case 255: /* loop_content: basic_loop  */
#line 1073 "grammar83.y"
               {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_WHILE;
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
        // Create condition so this becomes a 'while True' loop
        Expression* condition = create_expr(EXPR_ENUM_LIT, (yyloc));
        condition->u.enum_lit = &boolean_type.u.enum_.literals[true];
        (yyval.stmt)->u.loop.u.while_.condition = condition;
    }
#line 3546 "grammar83.tab.c"
    break;

  case 256: /* loop_content: WHILE condition basic_loop  */
#line 1082 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_WHILE;
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
        (yyval.stmt)->u.loop.u.while_.condition = (yyvsp[-1].expr);
    }
#line 3557 "grammar83.tab.c"
    break;

  case 257: /* loop_content: iter_part reverse_opt discrete_range basic_loop  */
#line 1088 "grammar83.y"
                                                    {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_FOR;
        (yyval.stmt)->u.loop.reverse = (yyvsp[-2].bool_);
        (yyval.stmt)->u.loop.u.for_.var = (yyvsp[-3].object_decl);
        (yyval.stmt)->u.loop.u.for_.range = (yyvsp[-1].expr);
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
    }
#line 3570 "grammar83.tab.c"
    break;

  case 258: /* iter_part: FOR identifier IN  */
#line 1098 "grammar83.y"
                      {
        memset(&(yyval.object_decl), 0, sizeof((yyval.object_decl)));
        (yyval.object_decl).base.kind = DECL_OBJECT;
        (yyval.object_decl).base.line_num = (yyloc);
        (yyval.object_decl).name = (yyvsp[-1].str_token);
    }
#line 3581 "grammar83.tab.c"
    break;

  case 259: /* reverse_opt: %empty  */
#line 1106 "grammar83.y"
            { (yyval.bool_) = false; }
#line 3587 "grammar83.tab.c"
    break;

  case 260: /* reverse_opt: REVERSE  */
#line 1107 "grammar83.y"
            { (yyval.bool_) = true; }
#line 3593 "grammar83.tab.c"
    break;

  case 261: /* basic_loop: LOOP statement_s END LOOP  */
#line 1111 "grammar83.y"
                              { (yyval.stmt) = (yyvsp[-2].stmt_list).first; }
#line 3599 "grammar83.tab.c"
    break;

  case 264: /* block: label_opt block_decl block_body END id_opt ';'  */
#line 1121 "grammar83.y"
                                                   {
        (yyval.stmt) = create_stmt(STMT_BLOCK, (yyloc));
        (yyval.stmt)->u.block.decls = (yyvsp[-4].decl);
        (yyval.stmt)->u.block.stmts = (yyvsp[-3].stmt);
        // Close scope if needed (i.e. if there was a declaration section)
        if((yyvsp[-4].decl)) {
            end_scope(context, (yylsp[-2]));
        }
    }
#line 3613 "grammar83.tab.c"
    break;

  case 265: /* block_decl: %empty  */
#line 1132 "grammar83.y"
                                                    { (yyval.decl) = NULL; }
#line 3619 "grammar83.tab.c"
    break;

  case 266: /* $@1: %empty  */
#line 1133 "grammar83.y"
            { begin_scope(context, (yylsp[0])); }
#line 3625 "grammar83.tab.c"
    break;

  case 267: /* block_decl: DECLARE $@1 decl_part  */
#line 1133 "grammar83.y"
                                                    { (yyval.decl) = (yyvsp[0].decl); }
#line 3631 "grammar83.tab.c"
    break;

  case 268: /* block_body: BEGiN handled_stmt_s  */
#line 1137 "grammar83.y"
                         { (yyval.stmt) = (yyvsp[0].stmt); }
#line 3637 "grammar83.tab.c"
    break;

  case 269: /* handled_stmt_s: statement_s except_handler_part_opt  */
#line 1142 "grammar83.y"
                                        { (yyval.stmt) = (yyvsp[-1].stmt_list).first; }
#line 3643 "grammar83.tab.c"
    break;

  case 272: /* exit_stmt: EXIT name_opt when_opt ';'  */
#line 1151 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_EXIT, (yyloc));
        // TODO: name_opt
        (yyval.stmt)->u.exit.condition = (yyvsp[-1].expr);
    }
#line 3653 "grammar83.tab.c"
    break;

  case 275: /* when_opt: %empty  */
#line 1163 "grammar83.y"
                   { (yyval.expr) = NULL; }
#line 3659 "grammar83.tab.c"
    break;

  case 276: /* when_opt: WHEN condition  */
#line 1164 "grammar83.y"
                   { (yyval.expr) = (yyvsp[0].expr); }
#line 3665 "grammar83.tab.c"
    break;

  case 277: /* return_stmt: RETURN ';'  */
#line 1168 "grammar83.y"
                  { (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc)); }
#line 3671 "grammar83.tab.c"
    break;

  case 278: /* return_stmt: RETURN expression ';'  */
#line 1169 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc));
        (yyval.stmt)->u.return_.expr = (yyvsp[-1].expr);
    }
#line 3680 "grammar83.tab.c"
    break;

  case 279: /* goto_stmt: GOTO name ';'  */
#line 1175 "grammar83.y"
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
#line 3707 "grammar83.tab.c"
    break;

  case 280: /* subprog_decl: subprog_spec ';'  */
#line 1199 "grammar83.y"
                          {
        (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl);
        end_scope(context, (yylsp[0]));
    }
#line 3716 "grammar83.tab.c"
    break;

  case 282: /* @2: %empty  */
#line 1208 "grammar83.y"
                                          {
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
        push_declaration(context, &(yyval.subprogram_decl)->base);
        begin_scope(context, (yylsp[0]));
    }
#line 3727 "grammar83.tab.c"
    break;

  case 283: /* subprog_spec: PROCEDURE identifier @2 formal_part_opt  */
#line 1214 "grammar83.y"
                                { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3733 "grammar83.tab.c"
    break;

  case 284: /* @3: %empty  */
#line 1215 "grammar83.y"
                                         {
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
        push_declaration(context, &(yyval.subprogram_decl)->base);
        begin_scope(context, (yylsp[0]));
    }
#line 3744 "grammar83.tab.c"
    break;

  case 285: /* subprog_spec: FUNCTION designator @3 formal_part_opt RETURN name  */
#line 1221 "grammar83.y"
                                { (yyval.subprogram_decl) = (yyvsp[-3].subprogram_decl); }
#line 3750 "grammar83.tab.c"
    break;

  case 288: /* designator: char_string  */
#line 1227 "grammar83.y"
                { (yyval.str_token) = string_pool_to_token((yyvsp[0].str)); }
#line 3756 "grammar83.tab.c"
    break;

  case 296: /* mode: %empty  */
#line 1250 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3762 "grammar83.tab.c"
    break;

  case 297: /* mode: IN  */
#line 1251 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3768 "grammar83.tab.c"
    break;

  case 298: /* mode: OUT  */
#line 1252 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_OUT; }
#line 3774 "grammar83.tab.c"
    break;

  case 299: /* mode: IN OUT  */
#line 1253 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN_OUT; }
#line 3780 "grammar83.tab.c"
    break;

  case 300: /* subprog_spec_is_push: subprog_spec IS  */
#line 1257 "grammar83.y"
                    { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3786 "grammar83.tab.c"
    break;

  case 301: /* subprog_body: subprog_spec_is_push decl_part block_body END id_opt ';'  */
#line 1263 "grammar83.y"
                                                             {
        (yyval.subprogram_decl) = (yyvsp[-5].subprogram_decl);
        (yyval.subprogram_decl)->decls = (yyvsp[-4].decl);
        (yyval.subprogram_decl)->stmts = (yyvsp[-3].stmt);
        // Close scope opened in subprog_spec
        end_scope(context, (yylsp[-2]));
    }
#line 3798 "grammar83.tab.c"
    break;

  case 302: /* procedure_call: name ';'  */
#line 1272 "grammar83.y"
             {
        (yyval.stmt) = create_stmt(STMT_EXPR, (yyloc));
        (yyval.stmt)->u.expr.kind = EXPR_NAME;
        (yyval.stmt)->u.expr.line_num = (yyloc);
        (yyval.stmt)->u.expr.u.name = (yyvsp[-1].name);
    }
#line 3809 "grammar83.tab.c"
    break;

  case 303: /* pkg_decl: pkg_spec ';'  */
#line 1280 "grammar83.y"
                         { (yyval.pkg_spec) = (yyvsp[-1].pkg_spec); }
#line 3815 "grammar83.tab.c"
    break;

  case 305: /* @4: %empty  */
#line 1285 "grammar83.y"
                                    {
        begin_scope(context, (yylsp[0]));
        (yyval.pkg_spec) = calloc(1, sizeof(PackageSpec));
        (yyval.pkg_spec)->base.kind = DECL_PKG_SPEC;
        (yyval.pkg_spec)->base.line_num = (yyloc);
        (yyval.pkg_spec)->name = (yyvsp[-1].str_token);
    }
#line 3827 "grammar83.tab.c"
    break;

  case 306: /* pkg_spec: PACKAGE identifier IS @4 decl_item_s private_part END identifier_opt  */
#line 1292 "grammar83.y"
                                                {
        (yyval.pkg_spec) = (yyvsp[-4].pkg_spec);
        (yyval.pkg_spec)->decls = (yyvsp[-3].decl);
        // TODO: private part
        end_scope(context, (yylsp[-1]));
        if((yyvsp[0].str_token) && (yyval.pkg_spec)->name != (yyvsp[0].str_token)) {
            error_print((yylsp[0]), "End label '%s' does not match package name ('%s')", ST((yyvsp[0].str_token)), ST((yyval.pkg_spec)->name));
            error_exit();
        }
    }
#line 3842 "grammar83.tab.c"
    break;

  case 309: /* identifier_opt: %empty  */
#line 1309 "grammar83.y"
               { (yyval.str_token) = 0; }
#line 3848 "grammar83.tab.c"
    break;

  case 311: /* @5: %empty  */
#line 1314 "grammar83.y"
                                         {
        begin_scope(context, (yylsp[0]));
        (yyval.pkg_body) = calloc(1, sizeof(PackageBody));
        (yyval.pkg_body)->base.kind = DECL_PKG_BODY;
        (yyval.pkg_body)->base.line_num = (yyloc);
        (yyval.pkg_body)->name = (yyvsp[-1].str_token);
    }
#line 3860 "grammar83.tab.c"
    break;

  case 312: /* pkg_body: PACKAGE BODY identifier IS @5 decl_part body_opt END identifier_opt ';'  */
#line 1321 "grammar83.y"
                                              {
        (yyval.pkg_body) = (yyvsp[-5].pkg_body);
        (yyval.pkg_body)->decls = (yyvsp[-4].decl);
        // TODO: body_opt
        end_scope(context, (yylsp[-2]));
        if((yyvsp[-1].str_token) && (yyval.pkg_body)->name != (yyvsp[-1].str_token)) {
            error_print((yylsp[-1]), "End label '%s' does not match package name ('%s')", ST((yyvsp[-1].str_token)), ST((yyval.pkg_body)->name));
            error_exit();
        }
    }
#line 3875 "grammar83.tab.c"
    break;

  case 329: /* comp_unit: context_spec unit pragma_s  */
#line 1373 "grammar83.y"
                               { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3881 "grammar83.tab.c"
    break;

  case 330: /* comp_unit: unit pragma_s  */
#line 1374 "grammar83.y"
                               { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3887 "grammar83.tab.c"
    break;

  case 337: /* unit: pkg_decl  */
#line 1393 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_PACKAGE_SPEC);
        (yyval.comp_unit)->u.package_spec = (yyvsp[0].pkg_spec);
    }
#line 3896 "grammar83.tab.c"
    break;

  case 338: /* unit: pkg_body  */
#line 1397 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_PACKAGE_BODY);
        (yyval.comp_unit)->u.package_body = (yyvsp[0].pkg_body);
    }
#line 3905 "grammar83.tab.c"
    break;

  case 339: /* unit: subprog_decl  */
#line 1401 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        (yyval.comp_unit)->u.subprogram_decl = (yyvsp[0].subprogram_decl);
    }
#line 3914 "grammar83.tab.c"
    break;

  case 340: /* unit: subprog_body  */
#line 1405 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        (yyval.comp_unit)->u.subprogram_decl = (yyvsp[0].subprogram_decl);
    }
#line 3923 "grammar83.tab.c"
    break;


#line 3927 "grammar83.tab.c"

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

#line 1550 "grammar83.y"


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
    // Remove all named declarations from the symbol table
    for(Declaration* decl = curr_scope->first; decl; decl = decl->next) {
        StringToken name = get_decl_name(decl);
        if(name) {
            // Will always be the first overload of the set since we are in the
            // innermost scope and will have just added it to the set
            Declaration** first_overload = find_bucket(context, name);
            Declaration* second_overload = (*first_overload)->next_overload;
            (*first_overload)->next_overload = NULL;
            *first_overload = second_overload;
        }
    }
    memset(curr_scope, 0, sizeof(*curr_scope));
    --context->curr_scope_idx;
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
        case DECL_ENUM_LIT:
            name = ((EnumLiteral*)decl)->name;
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
