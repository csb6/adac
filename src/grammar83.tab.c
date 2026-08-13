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
  YYSYMBOL_private_part = 240,             /* private_part  */
  YYSYMBOL_identifier_opt = 241,           /* identifier_opt  */
  YYSYMBOL_pkg_body = 242,                 /* pkg_body  */
  YYSYMBOL_body_opt = 243,                 /* body_opt  */
  YYSYMBOL_private_type = 244,             /* private_type  */
  YYSYMBOL_limited_opt = 245,              /* limited_opt  */
  YYSYMBOL_use_clause = 246,               /* use_clause  */
  YYSYMBOL_name_s = 247,                   /* name_s  */
  YYSYMBOL_rename_decl = 248,              /* rename_decl  */
  YYSYMBOL_rename_unit = 249,              /* rename_unit  */
  YYSYMBOL_renames = 250,                  /* renames  */
  YYSYMBOL_comp_unit = 251,                /* comp_unit  */
  YYSYMBOL_context_spec = 252,             /* context_spec  */
  YYSYMBOL_with_clause = 253,              /* with_clause  */
  YYSYMBOL_use_clause_opt = 254,           /* use_clause_opt  */
  YYSYMBOL_unit = 255,                     /* unit  */
  YYSYMBOL_subunit = 256,                  /* subunit  */
  YYSYMBOL_subunit_body = 257,             /* subunit_body  */
  YYSYMBOL_body_stub = 258,                /* body_stub  */
  YYSYMBOL_exception_decl = 259,           /* exception_decl  */
  YYSYMBOL_except_handler_part = 260,      /* except_handler_part  */
  YYSYMBOL_exception_handler = 261,        /* exception_handler  */
  YYSYMBOL_except_choice_s = 262,          /* except_choice_s  */
  YYSYMBOL_except_choice = 263,            /* except_choice  */
  YYSYMBOL_raise_stmt = 264,               /* raise_stmt  */
  YYSYMBOL_generic_decl = 265,             /* generic_decl  */
  YYSYMBOL_generic_formal_part = 266,      /* generic_formal_part  */
  YYSYMBOL_generic_formal = 267,           /* generic_formal  */
  YYSYMBOL_generic_discrim_part_opt = 268, /* generic_discrim_part_opt  */
  YYSYMBOL_subp_default = 269,             /* subp_default  */
  YYSYMBOL_generic_type_def = 270,         /* generic_type_def  */
  YYSYMBOL_generic_derived_type = 271,     /* generic_derived_type  */
  YYSYMBOL_generic_subp_inst = 272,        /* generic_subp_inst  */
  YYSYMBOL_generic_pkg_inst = 273,         /* generic_pkg_inst  */
  YYSYMBOL_generic_inst = 274,             /* generic_inst  */
  YYSYMBOL_rep_spec = 275,                 /* rep_spec  */
  YYSYMBOL_attrib_def = 276,               /* attrib_def  */
  YYSYMBOL_record_type_spec = 277,         /* record_type_spec  */
  YYSYMBOL_align_opt = 278,                /* align_opt  */
  YYSYMBOL_comp_loc_s = 279,               /* comp_loc_s  */
  YYSYMBOL_address_spec = 280,             /* address_spec  */
  YYSYMBOL_code_stmt = 281                 /* code_stmt  */
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

#line 477 "grammar83.tab.c"

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
#define YYLAST   1306

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  95
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  187
/* YYNRULES -- Number of rules.  */
#define YYNRULES  399
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  713

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
       0,   255,   255,   259,   260,   264,   265,   269,   270,   274,
     275,   279,   280,   281,   285,   289,   293,   297,   298,   299,
     300,   301,   305,   331,   346,   350,   355,   356,   360,   361,
     365,   366,   370,   382,   383,   384,   389,   390,   394,   395,
     396,   397,   398,   399,   400,   401,   405,   422,   426,   430,
     431,   435,   439,   450,   454,   455,   459,   460,   461,   465,
     476,   480,   486,   493,   507,   511,   515,   516,   520,   524,
     525,   529,   530,   534,   538,   542,   546,   547,   551,   555,
     559,   560,   564,   565,   569,   573,   574,   578,   579,   580,
     584,   585,   589,   590,   594,   595,   599,   603,   604,   608,
     609,   613,   614,   618,   622,   623,   627,   631,   635,   641,
     645,   646,   650,   651,   655,   656,   660,   661,   665,   666,
     670,   671,   677,   678,   679,   680,   684,   685,   691,   695,
     699,   700,   704,   708,   709,   710,   711,   718,   719,   720,
     724,   730,   734,   738,   739,   743,   744,   745,   746,   750,
     751,   752,   753,   757,   761,   762,   763,   764,   768,   787,
     788,   792,   793,   794,   795,   796,   800,   801,   805,   809,
     810,   811,   815,   816,   817,   821,   822,   827,   828,   829,
     830,   837,   838,   839,   840,   841,   842,   846,   847,   851,
     852,   853,   857,   858,   862,   863,   864,   868,   869,   873,
     874,   875,   876,   880,   881,   882,   883,   887,   888,   892,
     893,   894,   898,   899,   903,   907,   908,   912,   916,   922,
     923,   931,   932,   933,   937,   938,   939,   940,   941,   942,
     943,   944,   945,   949,   950,   951,   952,   956,   960,   969,
     980,   981,   987,   994,   998,  1002,  1003,  1007,  1014,  1020,
    1021,  1027,  1036,  1040,  1041,  1045,  1055,  1061,  1071,  1079,
    1080,  1084,  1088,  1089,  1094,  1105,  1106,  1106,  1110,  1115,
    1119,  1120,  1124,  1131,  1132,  1136,  1137,  1141,  1142,  1148,
    1172,  1173,  1178,  1178,  1184,  1184,  1190,  1194,  1195,  1199,
    1200,  1204,  1208,  1209,  1213,  1214,  1218,  1219,  1220,  1221,
    1225,  1229,  1236,  1244,  1245,  1249,  1260,  1261,  1265,  1266,
    1270,  1281,  1282,  1286,  1290,  1291,  1295,  1299,  1300,  1304,
    1305,  1306,  1310,  1311,  1312,  1313,  1317,  1321,  1322,  1326,
    1327,  1328,  1332,  1336,  1337,  1341,  1345,  1349,  1353,  1357,
    1358,  1359,  1363,  1367,  1368,  1372,  1373,  1377,  1381,  1382,
    1386,  1387,  1391,  1392,  1396,  1397,  1401,  1405,  1406,  1410,
    1411,  1415,  1416,  1417,  1418,  1419,  1420,  1421,  1425,  1426,
    1427,  1431,  1432,  1433,  1437,  1438,  1439,  1440,  1441,  1442,
    1443,  1444,  1445,  1446,  1450,  1451,  1455,  1459,  1463,  1467,
    1468,  1469,  1473,  1477,  1481,  1482,  1486,  1487,  1491,  1495
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
  "pkg_decl", "pkg_spec", "private_part", "identifier_opt", "pkg_body",
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

#define YYPACT_NINF (-580)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-352)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     833,   114,  -580,    40,    -5,    24,    -2,    84,  -580,   243,
    1205,  -580,  -580,    97,  -580,  -580,  -580,   871,  -580,  -580,
    -580,  -580,   157,   104,   118,  -580,  -580,     3,   199,   154,
    -580,   211,  -580,   215,  -580,   277,   498,  -580,   269,   287,
     309,    92,   335,   343,   364,   498,  -580,  -580,  -580,  -580,
     520,  -580,  -580,   429,  -580,  1103,  -580,  -580,  -580,   266,
    -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,
    -580,  -580,  -580,  -580,  -580,  -580,  -580,   323,   403,  -580,
     386,   398,   158,   524,     9,   402,   421,  -580,  -580,  -580,
    -580,   411,   478,  1049,   432,   411,   456,  -580,   466,   498,
    -580,  -580,  -580,   469,  -580,  -580,  -580,  -580,  -580,  -580,
    -580,   470,   505,   532,   528,   491,   469,   387,   453,  1034,
     565,  -580,   246,   323,   403,  -580,  -580,   369,   518,   114,
     552,   555,   212,  -580,   566,  -580,  -580,    86,   587,  -580,
    1147,   210,   603,  1139,  -580,   324,  -580,  -580,  -580,   337,
    -580,   469,   433,   301,    27,   640,   301,   584,   635,  -580,
     696,   498,    65,   648,  -580,  -580,   498,   662,    17,   203,
     604,   714,   498,   498,   714,   612,   498,   677,   615,  1034,
    -580,   116,   621,   815,  -580,  -580,  -580,  -580,  -580,  -580,
    -580,  -580,  -580,  -580,   258,  -580,  -580,  -580,  -580,  -580,
    -580,  -580,  -580,   114,   623,  1183,   628,    71,  -580,   678,
     411,   692,   411,   679,  -580,   309,  -580,   448,  -580,   498,
     429,   675,  1230,   725,  -580,   246,   741,   722,  -580,  -580,
    -580,  -580,  1226,   498,  1226,  -580,  -580,  -580,  -580,   487,
    -580,  -580,  -580,    36,  -580,   536,   390,  -580,   539,  -580,
    -580,  -580,  -580,   155,  -580,   589,   471,   342,  -580,   768,
    -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,
    -580,  -580,   714,   757,   486,   171,   414,  -580,  -580,  1095,
     773,   551,  -580,   255,   706,   318,  -580,   711,   553,   479,
    -580,   317,   716,   469,   714,  -580,   717,   713,   788,   739,
    -580,  -580,  -580,  -580,   671,   469,   730,   582,   255,   620,
    -580,  1034,   735,  -580,   727,  -580,   351,  -580,  -580,   714,
    -580,   321,  -580,   737,  -580,  -580,   737,   403,  -580,   736,
    1034,   714,   114,   758,  -580,   429,   742,  -580,  -580,  -580,
     744,   798,   760,   783,   789,  -580,    42,    86,  -580,   469,
    -580,   800,   791,  -580,   753,  -580,  -580,   486,  -580,  -580,
     776,   755,   588,   762,   232,   714,   689,   714,   412,  -580,
    -580,   433,   779,   822,  -580,   714,   714,   714,  -580,  -580,
    -580,  -580,   820,  -580,  -580,  -580,  -580,  -580,  -580,   714,
     714,   471,   342,  -580,  -580,  -580,  -580,   471,  1226,   447,
     817,  -580,  -580,   784,   714,   786,   696,  -580,   714,  -580,
    -580,  -580,  -580,   857,    93,  -580,   294,   714,   714,  -580,
     714,   498,   631,  -580,  -580,  -580,  -580,  -580,  -580,  -580,
    -580,  -580,  -580,  -580,  -580,   374,  -580,   449,  -580,   714,
     825,   714,   795,   803,  -580,   714,   804,  -580,  1034,   714,
     858,   780,  -580,  -580,  -580,   496,  -580,     4,  -580,  -580,
     140,  1205,   853,   971,   852,   821,  -580,   714,   872,  -580,
    -580,   900,   905,   906,   498,   913,   922,  -580,  -580,  -580,
     874,   849,  -580,   498,   498,    31,   850,  -580,  -580,   753,
     868,  -580,  -580,   851,   433,  -580,   433,  -580,   564,  -580,
     255,  -580,  -580,   255,  -580,   659,    90,   855,  -580,  -580,
    -580,  -580,  -580,   493,  -580,   493,  -580,   163,   342,  -580,
    -580,  -580,   714,   261,  -580,   255,  -580,  -580,    41,  -580,
     309,  -580,   498,  -580,   282,    41,   255,  -580,  -580,  -580,
     643,  -580,   879,  -580,  -580,  -580,  -580,  -580,   646,  -580,
     655,  -580,   895,   498,   255,  -580,  -580,  -580,  -580,   999,
    -580,   901,  -580,  -580,   859,   469,    44,  -580,   919,   689,
    -580,  -580,  -580,   899,  -580,  -580,   852,  1146,   114,   918,
    -580,  -580,   873,  -580,   865,  -580,   454,   698,  -580,   469,
    -580,   881,  -580,  -580,  -580,   890,   665,   714,   543,   893,
     156,  -580,  -580,    42,  -580,   714,  -580,  -580,  -580,   631,
    -580,   213,   911,   498,  -580,   714,   619,  -580,  -580,  -580,
     882,    15,  1034,    15,   883,    45,  -580,  -580,   884,   948,
     912,  -580,   885,  -580,   367,  -580,   886,  -580,   143,  -580,
     889,   714,  -580,    41,  -580,   892,   908,   907,   630,   950,
    -580,  -580,  -580,   498,  -580,   400,  -580,  -580,  -580,    49,
     843,  -580,  -580,  1034,  -580,  -580,  -580,  -580,   903,  -580,
    -580,  -580,   359,  -580,  -580,   945,  -580,   498,   930,   237,
    -580,   403,  -580,   985,  1034,   878,   914,   714,  -580,   403,
     788,  -580,  -580,  -580,   936,  -580,   916,   134,   920,   403,
    -580,   689,   148,  -580,  -580,    52,   976,  -580,  -580,   923,
     213,  -580,  -580
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     0,   359,     0,     0,     0,     0,     0,   337,     0,
       0,   338,   335,     0,   336,   341,     2,     0,   333,     9,
     339,   340,     0,     0,     0,   287,   288,   286,     0,     0,
     282,     0,    24,     0,     1,   300,     0,   280,     0,     0,
       0,     0,     0,     0,     0,     0,   125,   122,    11,    12,
       0,    13,    14,     0,   129,     0,   126,   128,    15,     0,
     130,    16,   131,   123,    18,   321,    20,    17,    19,   124,
     389,   390,   391,   303,   331,   333,     9,   329,   328,   295,
       0,     0,     0,     0,     0,     0,     0,   367,   360,   281,
     304,   289,     0,     0,     0,   289,     0,   332,     0,     0,
     386,   132,   141,   326,   136,   133,   134,   135,   323,    21,
     137,     0,     0,     0,     0,    33,   317,     0,    26,     0,
       0,   127,   300,   330,   327,   334,    10,     0,   368,     0,
       0,     0,   296,   357,     0,   361,   358,     0,     0,   290,
       0,     0,   306,     0,   120,     0,   387,   322,   283,     0,
      25,   388,     0,     0,     0,     0,     0,     0,     0,     3,
       0,     0,     0,    36,    34,   316,     0,    27,     0,     0,
       0,     0,   273,     0,     0,     0,   273,     0,   132,     0,
     223,     0,     0,     0,   217,   219,   221,   222,   224,   225,
     233,   234,     9,   235,   265,   236,   268,   226,   227,   228,
     229,   230,   231,   262,     0,     0,     0,     0,   369,     0,
     289,     0,   289,   297,   298,     0,   325,     0,   292,     0,
     311,     0,     0,     0,   121,     0,     0,     0,   343,   344,
     342,   148,     0,     0,     0,   160,   111,   140,   158,     0,
     192,   193,   113,     0,   107,   110,   208,   159,     0,   143,
     207,   212,   146,   109,   169,   177,     0,   189,   197,   203,
     211,   210,   209,   157,   156,   155,   154,   153,   152,   149,
     150,   151,     0,   394,   208,     0,   177,   138,   139,     0,
     132,     0,     5,     7,     0,    48,   100,     0,     0,     0,
      97,   314,     0,   318,     0,   347,     0,     0,    30,    28,
      29,    71,    72,   232,     0,   274,   275,     0,   244,   245,
     240,     0,     0,   237,     0,   277,     0,   254,   220,     0,
     302,     0,   399,     0,   218,   269,   271,   249,   266,     0,
       0,     0,   262,   259,   255,     0,     0,   263,   346,   324,
       0,   314,     0,     0,   371,   299,    30,     0,   291,   285,
     312,     0,     0,   307,   308,   300,   205,   215,   216,   204,
     160,     0,     0,   146,   109,     0,     0,     0,     0,   112,
     142,     0,   172,   173,   174,     0,     0,     0,   184,   182,
     186,   187,     0,   181,   183,   185,   194,   195,   196,     0,
       0,     0,   190,   201,   202,   199,   200,     0,     0,     0,
       0,   396,   392,     0,     0,     0,     0,    46,     0,    47,
      50,    49,    35,   101,     0,    96,     0,     0,     0,   315,
       0,     0,     0,    37,    44,    64,    38,    39,    40,    66,
      67,    41,    42,    43,    45,     0,    32,     0,   320,     0,
       0,     0,     0,     0,   248,     0,     0,   279,     0,     0,
       0,     0,   243,   356,   278,     0,   214,     0,   348,   349,
       0,     0,     0,     0,     0,     0,   260,     0,     0,   301,
     370,     0,     0,     0,     0,     0,     0,   380,   381,   382,
       0,     0,   383,     0,     0,     0,     0,   294,   293,   308,
       0,   309,   305,     0,     0,   162,     0,   161,     0,   213,
     168,   108,   110,   109,    53,   208,     0,    57,   144,   175,
     176,   170,   171,    56,   188,   178,   179,   180,   191,   198,
     206,   398,     0,     0,   345,     8,     4,     6,    54,   102,
       0,    98,     0,   114,     0,    54,    65,    52,    63,    62,
       0,    60,     0,   313,     9,    84,    23,    83,     0,    76,
       0,    80,   208,     0,    31,    22,   319,   276,   272,     0,
     241,     0,   238,   355,   132,   354,     0,   352,     0,     0,
     250,   267,   258,     0,   256,   252,     0,   208,   262,   377,
     379,   376,   384,   375,     0,   362,   371,     0,   373,   372,
     363,     0,   165,   166,   167,   160,     0,     0,     0,     0,
       0,    55,    51,    30,   115,     0,    69,    68,    59,     0,
      86,     0,     0,     0,    79,     0,     0,    82,    75,    74,
       0,     0,     0,     0,     0,     0,   261,   257,     0,     0,
       0,   374,     0,   366,     0,   310,     0,   163,     0,   395,
       0,     0,    99,    54,    61,     0,     0,     0,     0,     0,
       9,    90,     9,     0,    77,     0,    81,    78,   239,     0,
       0,   353,   247,     0,   264,   378,   385,   364,     0,   164,
      58,   393,     0,    70,    95,     0,     9,     0,     0,     0,
      87,    88,    73,     0,     0,     0,     0,     0,     9,    89,
      30,    85,    91,     9,     0,   365,     0,     0,     0,    93,
     397,     0,     0,   104,    94,     0,     0,   105,     9,     0,
       0,   103,   106
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -580,  -580,    -7,  -580,   592,   -51,  -580,  -580,  -580,    -6,
    -580,  -580,  -333,  -580,  -580,  -580,  -580,  -580,  -155,  -580,
    -580,  -580,  -241,  -496,  -347,  -580,  -580,   394,  -580,  -580,
    -580,  -580,  -233,  -580,  -580,  -579,  -580,   397,  -580,  -580,
    -440,  -580,  -580,   305,  -580,  -580,   340,   888,  -580,   607,
    -580,   345,  -580,   325,  -536,   664,  -362,   684,    -9,   806,
    -580,   -62,  -580,   979,  -580,   -34,  -206,   894,   898,  -580,
     540,  -221,  -580,  -580,   887,  -580,  -580,  -580,   801,   -55,
    -580,  -580,   373,  -580,  -580,  -144,  -580,  -580,  -227,  -580,
     642,  -210,  -244,   -70,  -580,  -304,  -164,  -580,  -580,  -580,
    -580,  -580,  -580,  -580,   595,  -580,  -290,  -580,  -580,  -580,
    -580,  -580,  -580,  -580,  -580,  -580,  -580,  -428,  -315,  -580,
    -580,  -580,  -182,  -580,  -580,  -580,   880,  -580,  -580,  -580,
      61,    30,  -580,  -580,    33,   -58,  -580,  -580,  -116,  -580,
    -580,    23,  -580,    82,  1036,  -580,   571,    28,  -580,   720,
     721,    34,  -580,  -580,   284,   -17,  -580,  -580,  1046,   989,
    1048,  -580,  -580,  -580,  -580,  -580,   740,   452,   446,  -580,
     389,  -580,  -580,  -580,   492,  -580,  -580,  -580,  -580,   988,
    -580,  -580,  -580,  -580,  -580,  -580,  -580
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     7,   180,   281,   282,    78,    47,    48,    49,    50,
     169,   298,   442,    51,   163,   292,   423,    52,   618,   409,
     410,   424,   601,   617,   242,   426,   540,   541,   427,   428,
     429,   430,   300,   301,   302,   619,   548,   549,   440,   550,
     551,   432,   545,   649,   650,   680,   651,   164,   289,   290,
     530,   652,   702,   703,   243,   244,   245,   433,   220,   142,
     143,    54,    55,    56,    57,   274,   111,   247,   104,   105,
     248,   249,   106,   107,   267,   250,   251,   362,   252,   253,
     375,   376,   254,   389,   390,   276,   256,   391,   257,   397,
     258,   259,   260,   261,   262,   183,   184,   185,   186,   187,
     188,   189,   190,   309,   310,   311,   312,   450,   191,   192,
     460,   570,   193,   194,   332,   333,   467,   334,   336,   195,
     335,   461,   120,   196,   325,   197,   306,   446,   198,   199,
      58,    59,    95,    91,   337,   138,   139,   217,    85,   215,
      10,    60,   200,    61,    13,   223,   492,    62,   351,   434,
     435,    63,   117,    64,    65,    38,    16,    17,    18,    77,
      19,    20,   230,    66,    67,   326,   458,   566,   567,   201,
      68,    22,    88,   209,   486,   481,   482,    23,    24,   100,
      69,    70,    71,   401,   523,    72,   202
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      33,    53,   103,    46,   502,   369,   284,   451,   255,   346,
      74,   116,    94,   487,   299,   318,    83,   465,   361,   324,
     504,   218,   356,    11,   359,   124,   463,   576,    14,   392,
       9,   144,   602,   625,    27,   588,   574,   148,   350,   607,
      11,   464,   268,   516,   411,    14,   365,     9,    46,   182,
     425,   441,    84,   563,   622,   663,    87,   372,   431,   684,
      28,     8,   708,  -284,   563,   151,   286,   134,    36,   287,
      30,   126,   286,    32,   682,   340,    36,   456,     8,   564,
     102,   224,    12,  -284,    34,   181,    46,    79,   133,   373,
     101,   102,   547,   377,   286,   255,   295,   367,   690,    12,
     275,   237,   269,   102,    31,   283,   101,   102,   477,   182,
     206,   125,   112,   182,   374,    29,   304,   126,   246,   308,
     547,   366,   316,   145,   456,   319,   156,   285,   157,   623,
     366,    83,   293,    46,   623,   285,    46,   366,   305,   307,
      32,   327,   305,   144,   559,   181,    32,   673,   627,   181,
     508,   296,   342,   468,   344,   557,   288,   125,    79,   372,
     144,    32,   210,   358,   518,   705,  -208,    29,    32,  -208,
     568,   372,   228,   145,   641,   656,    73,   229,   706,   227,
     386,   387,   388,    89,   364,   349,    42,   372,   520,    25,
      26,   373,     1,   129,    93,   320,   152,    90,    46,   357,
     321,   288,   154,   373,   701,   246,  -208,   502,    80,   130,
     569,     4,   131,    36,   645,    46,   374,   399,   701,   373,
     297,  -208,   255,   506,   670,    81,    45,   255,   374,    82,
     221,   488,    32,   513,   646,   145,  -145,  -145,   645,   437,
     156,   182,   157,   152,   374,   515,   506,   368,   372,   154,
     402,   213,   145,  -208,  -208,  -208,  -208,  -208,   646,   647,
     182,   533,   214,   628,   455,    42,   537,   -92,   547,   504,
     642,   372,    46,   593,    92,   594,   308,   181,   101,   102,
     373,   328,   443,    35,    15,    29,    96,   324,    32,    42,
      99,   599,   329,   606,    97,   506,   181,    98,   372,   324,
     330,    15,    36,   373,   498,   374,   122,   -92,   605,   204,
     500,   503,    32,   499,  -145,   263,   532,   600,   660,   582,
     126,    99,    37,   506,   603,    36,   264,   265,   374,   331,
     373,   416,   246,   505,   297,   263,   110,   246,   367,   502,
     696,    83,   417,   418,   408,    37,   264,   265,   108,   525,
     255,   283,   255,   528,   255,   374,   517,   698,   419,   685,
     420,   421,   534,   535,   225,   536,   109,   372,   231,   101,
     102,   668,     1,   367,   367,   372,   266,   604,   182,   232,
     694,   182,   285,    36,   110,   393,   554,   285,   226,    21,
     308,     4,    45,   182,   308,   324,   266,   422,   152,   373,
     394,   239,   153,    37,   154,   552,    21,   373,   288,   205,
     113,   233,   234,   235,   181,   687,   236,   181,   114,   378,
     542,   379,   380,   565,   374,   255,   263,   543,    36,   181,
     454,   544,   374,   577,   231,   395,   396,   264,   265,   115,
     285,   237,   101,   102,   238,   232,   367,   239,   119,   586,
     587,   589,   571,   381,    46,    42,   683,   240,   241,   382,
     246,   127,   246,   372,   246,   372,   165,   598,   507,   166,
     152,   506,   506,   128,   368,   167,   154,   233,   234,   235,
     152,   135,   236,   232,   153,   168,   154,   266,   231,   182,
     255,   137,   239,   611,   485,   373,   324,   373,   285,   232,
     136,   383,   384,   385,   386,   387,   388,   237,   101,   102,
     238,   147,   372,   239,   503,   233,   234,   235,   140,   285,
     374,   324,   374,   240,   241,   181,   521,   347,   546,   348,
     324,   233,   234,   360,   152,   246,   236,   149,   153,   155,
     154,   150,   638,   506,   373,   237,   101,   102,   238,   152,
     643,   239,   182,   153,   156,   154,   157,   255,   414,   372,
     415,   237,   101,   102,   238,   231,   152,   239,   161,   374,
     321,   162,   154,   101,   102,   562,   232,   240,   241,   655,
     158,   577,   505,   386,   387,   388,   672,   565,   181,   565,
     182,   373,   377,   182,   378,   203,   379,   380,   207,   679,
     246,   681,    98,   118,   126,   648,    98,   132,   233,   234,
     595,   159,   160,   236,   182,   182,   374,  -147,  -147,   285,
     370,   371,   639,   657,   182,   689,   181,   211,   381,   181,
     212,   232,   405,   406,   382,    98,   413,   697,   237,   101,
     102,   238,   699,   285,   239,   216,   503,   219,   448,   449,
     181,   181,   232,   505,   240,   241,   222,   710,   272,   278,
     181,   447,   152,   233,   234,   235,   153,   246,   154,   495,
     496,   294,   126,   648,   126,   279,   383,   384,   385,   386,
     387,   388,   126,   303,   233,   234,   235,   372,   291,   232,
     126,   313,   126,   237,   101,   102,   238,   273,   317,   239,
     322,   232,   338,   126,   648,   538,   539,   339,   232,   240,
     241,   444,    98,   677,   237,   101,   102,   238,   341,   373,
     239,   233,   234,   235,   608,   609,   232,   612,   613,   345,
     240,   241,   343,   233,   234,   235,   614,   615,   236,   152,
     233,   234,   235,   368,   374,   154,   637,   371,   511,   512,
     352,   237,   101,   102,   238,   354,   315,   239,   233,   234,
     235,    28,   355,   237,   101,   102,   238,   240,   241,   239,
     237,   280,   102,   238,   398,   400,   239,   633,   634,   240,
     241,   170,   153,   404,   154,   407,   240,   241,   237,   101,
     102,   238,   412,   439,   239,   436,   438,   441,    36,  -253,
     445,   171,   452,  -253,   240,   241,   453,   457,  -242,  -242,
    -242,   462,   416,   172,  -253,   297,   170,   173,   174,   466,
     483,   469,  -253,   471,   472,   470,   175,   484,   491,   485,
     489,   490,    42,   493,  -253,   176,   171,   494,  -253,   419,
     177,   473,   474,   497,   170,  -270,   509,   323,   172,  -253,
     510,  -253,   173,   174,   475,   178,   102,  -253,   179,   514,
     522,   175,  -253,   524,   171,   526,  -253,    42,     1,     2,
     176,   529,   553,  -350,   555,   177,   172,  -253,   476,   170,
     173,   174,   556,   558,     3,  -253,  -253,     4,   561,   175,
     178,   102,   572,   179,   330,    42,     5,  -253,   176,   171,
     575,  -253,   578,   177,   579,     6,     1,     2,  -251,   580,
     581,   172,  -253,  -350,  -253,   173,   174,   583,   178,   102,
    -253,   179,     3,    42,   175,     4,   584,   543,   585,   590,
      42,   403,   592,   176,     5,   597,   610,   170,   177,   620,
     624,   626,   621,     6,   629,   630,   631,   636,  -251,  -253,
     640,   616,   665,   178,   102,  -253,   179,   171,   653,  -253,
     635,   658,   662,   664,   667,   666,  -351,   669,   671,   172,
    -253,   674,   170,   173,   174,   152,   -54,   -54,  -253,   368,
     678,   154,   175,   675,   686,   688,   676,   691,    42,   657,
    -253,   176,   171,   695,  -253,   700,   177,   709,   527,   704,
     170,   573,   711,   644,   172,  -253,  -351,  -253,   173,   174,
     654,   178,   102,  -253,   179,   712,   208,   175,  -253,   692,
     171,   531,  -253,    42,   693,   478,   176,   707,   353,  -246,
     501,   177,   172,  -253,   121,   170,   173,   174,   596,   519,
     363,  -253,  -253,   277,   560,   175,   178,   102,   270,   179,
      39,    42,   271,  -253,   176,   171,   314,  -253,    86,   177,
     591,   479,   480,    75,   123,    76,   459,   172,  -253,   661,
    -253,   173,   174,   659,   178,   102,  -253,   179,   632,  -118,
     175,   146,     0,    40,     1,     2,    42,     0,     0,   176,
       0,     0,     0,    99,   177,     0,    39,     0,     0,     0,
     141,    42,  -118,     4,    39,  -253,     0,     0,     0,   178,
     102,     0,   179,    43,  -116,     0,     0,    44,    45,     0,
       0,     0,  -117,     0,    32,  -116,     0,     0,     0,    40,
       1,     2,     0,  -117,     0,     0,     0,    40,     1,     2,
      39,     0,     0,     0,     0,     0,    41,    42,    39,     4,
       0,     0,     0,     0,    41,    42,     0,     4,   403,    43,
       0,     0,     0,    44,    45,     0,  -116,    43,     0,  -119,
      32,    44,    45,    40,     1,     2,     0,  -116,    32,     0,
       0,    40,     1,     2,    39,     0,     0,     0,   -54,     0,
     141,    42,  -119,     4,     0,     0,     0,     0,    41,    42,
       0,     4,   367,    43,     0,     0,    39,    44,    45,     0,
       0,    43,     0,  -118,    32,    44,    45,    40,     1,     2,
       0,     0,    32,     0,  -116,     0,   152,   -54,   -54,     0,
     368,    39,   154,     0,   141,    42,  -118,     4,     0,    40,
       1,     2,     0,     0,     0,     0,     0,    43,     0,     0,
       0,    44,    45,     0,     0,     0,    41,    42,    32,     4,
    -118,     0,     0,     0,    40,     1,     2,     0,     0,    43,
     233,     0,   235,    44,    45,     0,     0,     0,     0,     0,
      32,   141,    42,     0,     4,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    43,     0,     0,     0,    44,    45,
     237,   101,   102,   238,     0,    32,   239
};

static const yytype_int16 yycheck[] =
{
       6,    10,    36,    10,   366,   246,   161,   311,   152,   215,
      17,    45,    29,   346,   169,   179,    22,   332,   239,   183,
     367,   137,   232,     0,   234,    76,   330,   467,     0,   256,
       0,    93,   528,   569,     1,     4,   464,    95,   220,   535,
      17,   331,    15,   390,   285,    17,    10,    17,    55,   119,
     291,     9,    22,    49,    10,    10,    22,    16,   291,    10,
      20,     0,    10,    60,    49,    99,     1,    84,    59,     4,
      75,    78,     1,    75,   653,     4,    59,   321,    17,    75,
      76,   143,     0,    80,     0,   119,    93,     1,    79,    48,
      75,    76,   439,     3,     1,   239,    79,    56,   677,    17,
     155,    74,    75,    76,    80,   160,    75,    76,   341,   179,
     127,    77,    20,   183,    73,    75,   171,   124,   152,   174,
     467,    85,   177,    93,   368,     9,    84,   161,    86,    85,
      85,   137,   166,   140,    85,   169,   143,    85,   172,   173,
      75,   192,   176,   205,   448,   179,    75,   643,   576,   183,
     371,   168,   210,   335,   212,   445,   162,   123,     1,    16,
     222,    75,   129,   233,   391,   701,     3,    75,    75,     6,
      30,    16,   149,   143,    18,   615,    79,   149,    30,   149,
      90,    91,    92,    79,   239,   219,    52,    16,   398,    75,
      76,    48,    35,    35,    40,    79,    80,    79,   205,   233,
      84,   207,    86,    48,    70,   239,    43,   569,    51,    51,
      70,    54,    54,    59,     1,   222,    73,   272,    70,    48,
      17,    58,   366,   367,    81,    68,    69,   371,    73,    72,
      20,   347,    75,   377,    21,   205,    81,    82,     1,   294,
      84,   311,    86,    80,    73,   389,   390,    84,    16,    86,
      79,    39,   222,    90,    91,    92,    93,    94,    21,    46,
     330,   416,    50,   578,   319,    52,   421,    30,   615,   616,
     603,    16,   279,   494,    75,   496,   331,   311,    75,    76,
      48,    23,   299,    40,     0,    75,    75,   451,    75,    52,
      44,    30,    34,   534,    79,   439,   330,    82,    16,   463,
      42,    17,    59,    48,    72,    73,    40,    70,    26,    63,
     365,   366,    75,    81,    82,    14,    22,   523,   622,   474,
     327,    44,    79,   467,   530,    59,    25,    26,    73,    71,
      48,    14,   366,   367,    17,    14,    75,   371,    56,   701,
     687,   347,    25,    26,    26,    79,    25,    26,    79,   404,
     494,   406,   496,   408,   498,    73,   390,   690,    41,   663,
      43,    44,   417,   418,    40,   420,    79,    16,     1,    75,
      76,     4,    35,    56,    56,    16,    75,   532,   448,    12,
     684,   451,   416,    59,    75,    43,   441,   421,    51,     0,
     445,    54,    69,   463,   449,   559,    75,    80,    80,    48,
      58,    80,    84,    79,    86,   439,    17,    48,   414,    40,
      75,    44,    45,    46,   448,    56,    49,   451,    75,     5,
      46,     7,     8,   457,    73,   569,    14,    53,    59,   463,
      79,    57,    73,   467,     1,    93,    94,    25,    26,    75,
     474,    74,    75,    76,    77,    12,    56,    80,    19,   483,
     484,   485,   461,    39,   461,    52,    56,    90,    91,    45,
     494,    75,   496,    16,   498,    16,    79,   522,    56,    82,
      80,   615,   616,    75,    84,    22,    86,    44,    45,    46,
      80,    79,    49,    12,    84,    32,    86,    75,     1,   559,
     634,    80,    80,   544,    40,    48,   660,    48,   532,    12,
      79,    87,    88,    89,    90,    91,    92,    74,    75,    76,
      77,    79,    16,    80,   569,    44,    45,    46,    40,   553,
      73,   685,    73,    90,    91,   559,    79,    79,    79,    81,
     694,    44,    45,    46,    80,   569,    49,    81,    84,    69,
      86,    75,   597,   687,    48,    74,    75,    76,    77,    80,
     605,    80,   622,    84,    84,    86,    86,   701,    79,    16,
      81,    74,    75,    76,    77,     1,    80,    80,    40,    73,
      84,    80,    86,    75,    76,    79,    12,    90,    91,   613,
      75,   615,   616,    90,    91,    92,   641,   621,   622,   623,
     660,    48,     3,   663,     5,    30,     7,     8,    80,   650,
     634,   652,    82,    83,   611,   611,    82,    83,    44,    45,
      46,    79,    80,    49,   684,   685,    73,    81,    82,   653,
      81,    82,    79,     4,   694,   676,   660,    75,    39,   663,
      75,    12,    81,    82,    45,    82,    83,   688,    74,    75,
      76,    77,   693,   677,    80,    79,   701,    60,    28,    29,
     684,   685,    12,   687,    90,    91,    53,   708,    18,    75,
     694,    79,    80,    44,    45,    46,    84,   701,    86,    81,
      82,     9,   679,   679,   681,    40,    87,    88,    89,    90,
      91,    92,   689,    79,    44,    45,    46,    16,    40,    12,
     697,    79,   699,    74,    75,    76,    77,    57,    83,    80,
      79,    12,    79,   710,   710,    74,    75,    79,    12,    90,
      91,    40,    82,    83,    74,    75,    76,    77,    40,    48,
      80,    44,    45,    46,    81,    82,    12,    81,    82,    50,
      90,    91,    40,    44,    45,    46,    81,    82,    49,    80,
      44,    45,    46,    84,    73,    86,    81,    82,   375,   376,
      75,    74,    75,    76,    77,    30,    79,    80,    44,    45,
      46,    20,    40,    74,    75,    76,    77,    90,    91,    80,
      74,    75,    76,    77,     6,    18,    80,    79,    80,    90,
      91,     1,    84,    10,    86,    79,    90,    91,    74,    75,
      76,    77,    81,    80,    80,    79,    79,     9,    59,    19,
      70,    21,    67,    23,    90,    91,    79,    70,    28,    29,
      30,    75,    14,    33,    34,    17,     1,    37,    38,    61,
      60,    79,    42,    25,    26,    81,    46,    44,    75,    40,
      30,    40,    52,    57,    19,    55,    21,    82,    23,    41,
      60,    43,    44,    81,     1,    30,    67,    32,    33,    34,
      28,    71,    37,    38,    56,    75,    76,    42,    78,    39,
      43,    46,    19,    79,    21,    79,    23,    52,    35,    36,
      55,    14,    47,    30,    79,    60,    33,    34,    80,     1,
      37,    38,    79,    79,    51,    42,    71,    54,    30,    46,
      75,    76,    39,    78,    42,    52,    63,    19,    55,    21,
      79,    23,    30,    60,     4,    72,    35,    36,    30,     4,
       4,    33,    34,    70,    71,    37,    38,     4,    75,    76,
      42,    78,    51,    52,    46,    54,     4,    53,    79,    79,
      52,    63,    81,    55,    63,    80,    57,     1,    60,    38,
      21,    42,    83,    72,    26,    72,    81,    57,    70,    71,
      57,    56,     4,    75,    76,    19,    78,    21,    47,    23,
      79,    79,    79,    79,    79,    53,    30,    81,    79,    33,
      34,    79,     1,    37,    38,    80,    81,    82,    42,    84,
      30,    86,    46,    75,    81,    40,    79,    57,    52,     4,
      19,    55,    21,    79,    23,    79,    60,    21,   406,    79,
       1,    30,    79,   609,    33,    34,    70,    71,    37,    38,
     613,    75,    76,    42,    78,   710,   128,    46,    19,   679,
      21,   414,    23,    52,   679,   341,    55,   702,   222,    30,
     366,    60,    33,    34,    55,     1,    37,    38,   498,   397,
     239,    42,    71,   156,   449,    46,    75,    76,   154,    78,
       1,    52,   154,    19,    55,    21,   176,    23,    22,    60,
     489,   341,   341,    17,    75,    17,   326,    33,    34,   623,
      71,    37,    38,   621,    75,    76,    42,    78,   586,    30,
      46,    93,    -1,    34,    35,    36,    52,    -1,    -1,    55,
      -1,    -1,    -1,    44,    60,    -1,     1,    -1,    -1,    -1,
      51,    52,    53,    54,     1,    71,    -1,    -1,    -1,    75,
      76,    -1,    78,    64,    19,    -1,    -1,    68,    69,    -1,
      -1,    -1,    19,    -1,    75,    30,    -1,    -1,    -1,    34,
      35,    36,    -1,    30,    -1,    -1,    -1,    34,    35,    36,
       1,    -1,    -1,    -1,    -1,    -1,    51,    52,     1,    54,
      -1,    -1,    -1,    -1,    51,    52,    -1,    54,    63,    64,
      -1,    -1,    -1,    68,    69,    -1,    19,    64,    -1,    30,
      75,    68,    69,    34,    35,    36,    -1,    30,    75,    -1,
      -1,    34,    35,    36,     1,    -1,    -1,    -1,    42,    -1,
      51,    52,    53,    54,    -1,    -1,    -1,    -1,    51,    52,
      -1,    54,    56,    64,    -1,    -1,     1,    68,    69,    -1,
      -1,    64,    -1,    30,    75,    68,    69,    34,    35,    36,
      -1,    -1,    75,    -1,    19,    -1,    80,    81,    82,    -1,
      84,     1,    86,    -1,    51,    52,    53,    54,    -1,    34,
      35,    36,    -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,
      -1,    68,    69,    -1,    -1,    -1,    51,    52,    75,    54,
      30,    -1,    -1,    -1,    34,    35,    36,    -1,    -1,    64,
      44,    -1,    46,    68,    69,    -1,    -1,    -1,    -1,    -1,
      75,    51,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    68,    69,
      74,    75,    76,    77,    -1,    75,    80
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    35,    36,    51,    54,    63,    72,    96,   225,   226,
     235,   236,   238,   239,   242,   249,   251,   252,   253,   255,
     256,   265,   266,   272,   273,    75,    76,   229,    20,    75,
      75,    80,    75,   104,     0,    40,    59,    79,   250,     1,
      34,    51,    52,    64,    68,    69,    97,   101,   102,   103,
     104,   108,   112,   153,   156,   157,   158,   159,   225,   226,
     236,   238,   242,   246,   248,   249,   258,   259,   265,   275,
     276,   277,   280,    79,    97,   253,   255,   254,   100,     1,
      51,    68,    72,   104,   226,   233,   239,   246,   267,    79,
      79,   228,    75,    40,   250,   227,    75,    79,    82,    44,
     274,    75,    76,   160,   163,   164,   167,   168,    79,    79,
      75,   161,    20,    75,    75,    75,   160,   247,    83,    19,
     217,   158,    40,   254,   100,   246,    97,    75,    75,    35,
      51,    54,    83,    79,   250,    79,    79,    80,   230,   231,
      40,    51,   154,   155,   156,   226,   274,    79,   230,    81,
      75,   160,    80,    84,    86,    69,    84,    86,    75,    79,
      80,    40,    80,   109,   142,    79,    82,    22,    32,   105,
       1,    21,    33,    37,    38,    46,    55,    60,    75,    78,
      97,   160,   188,   190,   191,   192,   193,   194,   195,   196,
     197,   203,   204,   207,   208,   214,   218,   220,   223,   224,
     237,   264,   281,    30,    63,    40,   250,    80,   142,   268,
     229,    75,    75,    39,    50,   234,    79,   232,   233,    60,
     153,    20,    53,   240,   156,    40,    51,   226,   236,   242,
     257,     1,    12,    44,    45,    46,    49,    74,    77,    80,
      90,    91,   119,   149,   150,   151,   160,   162,   165,   166,
     170,   171,   173,   174,   177,   180,   181,   183,   185,   186,
     187,   188,   189,    14,    25,    26,    75,   169,    15,    75,
     162,   163,    18,    57,   160,   174,   180,   169,    75,    40,
      75,    98,    99,   174,   113,   160,     1,     4,   104,   143,
     144,    40,   110,   160,     9,    79,   250,    17,   106,   113,
     127,   128,   129,    79,   174,   160,   221,   160,   174,   198,
     199,   200,   201,    79,   221,    79,   174,    83,   191,     9,
      79,    84,    79,    32,   191,   219,   260,   100,    23,    34,
      42,    71,   209,   210,   212,   215,   213,   229,    79,    79,
       4,    40,   230,    40,   230,    50,   161,    79,    81,   160,
     217,   243,    75,   154,    30,    40,   186,   160,   188,   186,
      46,   166,   172,   173,   174,    10,    85,    56,    84,   117,
      81,    82,    16,    48,    73,   175,   176,     3,     5,     7,
       8,    39,    45,    87,    88,    89,    90,    91,    92,   178,
     179,   182,   183,    43,    58,    93,    94,   184,     6,   174,
      18,   278,    79,    63,    10,    81,    82,    79,    26,   114,
     115,   117,    81,    83,    79,    81,    14,    25,    26,    41,
      43,    44,    80,   111,   116,   117,   120,   123,   124,   125,
     126,   127,   136,   152,   244,   245,    79,   174,    79,    80,
     133,     9,   107,   250,    40,    70,   222,    79,    28,    29,
     202,   190,    67,    79,    79,   174,   187,    70,   261,   261,
     205,   216,    75,   190,   201,   213,    61,   211,   217,    79,
      81,    25,    26,    43,    44,    56,    80,   127,   152,   244,
     245,   270,   271,    60,    44,    40,   269,   107,   233,    30,
      40,    75,   241,    57,    82,    81,    82,    81,    72,    81,
     174,   150,   151,   174,   119,   160,   180,    56,   166,    67,
      28,   177,   177,   180,    39,   180,   119,   160,   183,   185,
     186,    79,    43,   279,    79,   174,    79,    99,   174,    14,
     145,   144,    22,   113,   174,   174,   174,   113,    74,    75,
     121,   122,    46,    53,    57,   137,    79,   119,   131,   132,
     134,   135,   160,    47,   174,    79,    79,   201,    79,   190,
     199,    30,    79,    49,    75,   160,   262,   263,    30,    70,
     206,   153,    39,    30,   212,    79,   135,   160,    30,     4,
       4,     4,   113,     4,     4,    79,   160,   160,     4,   160,
      79,   241,    81,   166,   166,    46,   165,    80,   174,    30,
     161,   117,   118,   161,   113,    26,   117,   118,    81,    82,
      57,   100,    81,    82,    81,    82,    56,   118,   113,   130,
      38,    83,    10,    85,    21,   149,    42,   212,   213,    26,
      72,    81,   269,    79,    80,    79,    57,    81,   174,    79,
      57,    18,   107,   174,   122,     1,    21,    46,   104,   138,
     139,   141,   146,    47,   132,   160,   135,     4,    79,   262,
     190,   263,    79,    10,    79,     4,    53,    79,     4,    81,
      81,    79,   174,   118,    79,    75,    79,    83,    30,   100,
     140,   100,   130,    56,    10,   190,    81,    56,    40,   100,
     130,    57,   141,   146,   190,    79,   119,   100,   107,   100,
      79,    70,   147,   148,    79,   149,    30,   148,    10,    21,
     100,    79,   138
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
     235,   236,   237,   238,   238,   239,   240,   240,   241,   241,
     242,   243,   243,   244,   245,   245,   246,   247,   247,   248,
     248,   248,   249,   249,   249,   249,   250,   251,   251,   252,
     252,   252,   253,   254,   254,   255,   255,   255,   255,   255,
     255,   255,   256,   257,   257,   258,   258,   259,   260,   260,
     261,   261,   262,   262,   263,   263,   264,   265,   265,   266,
     266,   267,   267,   267,   267,   267,   267,   267,   268,   268,
     268,   269,   269,   269,   270,   270,   270,   270,   270,   270,
     270,   270,   270,   270,   271,   271,   272,   273,   274,   275,
     275,   275,   276,   277,   278,   278,   279,   279,   280,   281
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
       2,     6,     2,     2,     2,     7,     0,     2,     0,     1,
       9,     0,     1,     2,     0,     1,     3,     1,     3,     6,
       5,     1,     4,     3,     5,     4,     2,     3,     2,     2,
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
#line 241 "grammar83.y"
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

#line 2484 "grammar83.tab.c"

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
#line 255 "grammar83.y"
                        { context->comp_unit = (yyvsp[0].comp_unit); }
#line 2704 "grammar83.tab.c"
    break;

  case 13: /* decl: type_decl  */
#line 281 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), (yyvsp[0].decl));
    }
#line 2713 "grammar83.tab.c"
    break;

  case 14: /* decl: subtype_decl  */
#line 285 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), (yyvsp[0].decl));
    }
#line 2722 "grammar83.tab.c"
    break;

  case 15: /* decl: subprog_decl  */
#line 289 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), &(yyvsp[0].subprogram_decl)->base);
    }
#line 2731 "grammar83.tab.c"
    break;

  case 16: /* decl: pkg_decl  */
#line 293 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), &(yyvsp[0].pkg_spec)->base);
    }
#line 2740 "grammar83.tab.c"
    break;

  case 22: /* object_decl: def_id_s ':' object_qualifier_opt object_subtype_def init_opt ';'  */
#line 305 "grammar83.y"
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
#line 2769 "grammar83.tab.c"
    break;

  case 23: /* number_decl: def_id_s ':' CONSTANT IS_ASSIGNED expression ';'  */
#line 331 "grammar83.y"
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
#line 2787 "grammar83.tab.c"
    break;

  case 24: /* def_id_s: identifier  */
#line 346 "grammar83.y"
               {
        StringTokenArray_init(&(yyval.str_token_array));
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2796 "grammar83.tab.c"
    break;

  case 25: /* def_id_s: def_id_s ',' identifier  */
#line 350 "grammar83.y"
                            { StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token)); }
#line 2802 "grammar83.tab.c"
    break;

  case 26: /* object_qualifier_opt: %empty  */
#line 355 "grammar83.y"
             { (yyval.bool_) = false; }
#line 2808 "grammar83.tab.c"
    break;

  case 27: /* object_qualifier_opt: CONSTANT  */
#line 356 "grammar83.y"
             { (yyval.bool_) = true; }
#line 2814 "grammar83.tab.c"
    break;

  case 30: /* init_opt: %empty  */
#line 365 "grammar83.y"
                           { (yyval.expr) = NULL; }
#line 2820 "grammar83.tab.c"
    break;

  case 31: /* init_opt: IS_ASSIGNED expression  */
#line 366 "grammar83.y"
                           { (yyval.expr) = (yyvsp[0].expr); }
#line 2826 "grammar83.tab.c"
    break;

  case 32: /* type_decl: TYPE identifier discrim_part_opt type_completion ';'  */
#line 370 "grammar83.y"
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
#line 2841 "grammar83.tab.c"
    break;

  case 37: /* type_completion: IS type_def  */
#line 390 "grammar83.y"
                { (yyval.type_decl) = (yyvsp[0].type_decl); }
#line 2847 "grammar83.tab.c"
    break;

  case 46: /* subtype_decl: SUBTYPE identifier IS subtype_ind ';'  */
#line 405 "grammar83.y"
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
#line 2866 "grammar83.tab.c"
    break;

  case 47: /* subtype_ind: name constraint  */
#line 422 "grammar83.y"
                    {
        // TODO: propagate constraint somehow
        (yyval.str_token) = (yyvsp[-1].name).name;
    }
#line 2875 "grammar83.tab.c"
    break;

  case 48: /* subtype_ind: name  */
#line 426 "grammar83.y"
         { (yyval.str_token) = (yyvsp[0].name).name; }
#line 2881 "grammar83.tab.c"
    break;

  case 52: /* derived_type: NEW subtype_ind  */
#line 439 "grammar83.y"
                    {
        (yyval.type_decl) = create_type_decl(TYPE_DERIVED);
        TypeDecl* base_type = find_type_decl(context, (yyvsp[0].str_token));
        if(!base_type) {
            error_print((yyloc), "Unknown base type: %s", ST((yyvsp[0].str_token)));
            error_exit();
        }
        (yyval.type_decl)->u.subtype.base = base_type;
    }
#line 2895 "grammar83.tab.c"
    break;

  case 53: /* range_constraint: RANGE range  */
#line 450 "grammar83.y"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 2901 "grammar83.tab.c"
    break;

  case 54: /* range_constr_opt: %empty  */
#line 454 "grammar83.y"
           { (yyval.expr) = NULL; }
#line 2907 "grammar83.tab.c"
    break;

  case 56: /* range: simple_expression DOT_DOT simple_expression  */
#line 459 "grammar83.y"
                                                { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_RANGE, (yyvsp[0].expr)); }
#line 2913 "grammar83.tab.c"
    break;

  case 59: /* enumeration_type: '(' enum_id_s ')'  */
#line 465 "grammar83.y"
                      {
        (yyval.type_decl) = create_type_decl(TYPE_ENUM);
        (yyval.type_decl)->u.enum_.literals = (yyvsp[-1].enum_literals).data;
        uint32_t literal_count = EnumLiteralArray_size(&(yyvsp[-1].enum_literals));
        (yyval.type_decl)->u.enum_.literal_count = literal_count;
        for(uint32_t i = 0; i < literal_count; ++i) {
            push_declaration(context, &(yyval.type_decl)->u.enum_.literals[i].base);
        }
    }
#line 2927 "grammar83.tab.c"
    break;

  case 60: /* enum_id_s: enum_id  */
#line 476 "grammar83.y"
            {
        EnumLiteralArray_init(&(yyval.enum_literals));
        EnumLiteralArray_append(&(yyval.enum_literals), (yyvsp[0].enum_literal));
    }
#line 2936 "grammar83.tab.c"
    break;

  case 61: /* enum_id_s: enum_id_s ',' enum_id  */
#line 480 "grammar83.y"
                          {
        (yyval.enum_literals) = (yyvsp[-2].enum_literals);
        EnumLiteralArray_append(&(yyval.enum_literals), (yyvsp[0].enum_literal));
    }
#line 2945 "grammar83.tab.c"
    break;

  case 62: /* enum_id: identifier  */
#line 486 "grammar83.y"
               {
        memset(&(yyval.enum_literal), 0, sizeof((yyval.enum_literal)));
        (yyval.enum_literal).base.kind = DECL_ENUM_LIT;
        (yyval.enum_literal).base.line_num = (yyloc);
        (yyval.enum_literal).name = (yyvsp[0].str_token);
        (yyval.enum_literal).is_char_lit = false;
    }
#line 2957 "grammar83.tab.c"
    break;

  case 63: /* enum_id: char_lit  */
#line 493 "grammar83.y"
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
#line 2974 "grammar83.tab.c"
    break;

  case 64: /* integer_type: range_constraint  */
#line 507 "grammar83.y"
                     {
        (yyval.type_decl) = create_type_decl(TYPE_INTEGER);
        (yyval.type_decl)->u.int_.range = (yyvsp[0].expr);
    }
#line 2983 "grammar83.tab.c"
    break;

  case 107: /* choice_s: choice  */
#line 631 "grammar83.y"
                        {
        ChoiceArray_init(&(yyval.choice_array));
        ChoiceArray_append(&(yyval.choice_array), (yyvsp[0].choice));
    }
#line 2992 "grammar83.tab.c"
    break;

  case 108: /* choice_s: choice_s '|' choice  */
#line 635 "grammar83.y"
                        {
        (yyval.choice_array) = (yyvsp[-2].choice_array);
        ChoiceArray_append(&(yyval.choice_array), (yyvsp[0].choice));
    }
#line 3001 "grammar83.tab.c"
    break;

  case 109: /* choice: expression  */
#line 641 "grammar83.y"
                         {
        (yyval.choice).kind = CHOICE_EXPR;
        (yyval.choice).u.expr = (yyvsp[0].expr);
    }
#line 3010 "grammar83.tab.c"
    break;

  case 111: /* choice: OTHERS  */
#line 646 "grammar83.y"
                         { (yyval.choice).kind = CHOICE_OTHERS; }
#line 3016 "grammar83.tab.c"
    break;

  case 116: /* decl_part: %empty  */
#line 660 "grammar83.y"
                         { (yyval.decl) = NULL; }
#line 3022 "grammar83.tab.c"
    break;

  case 117: /* decl_part: decl_item_or_body_s1  */
#line 661 "grammar83.y"
                         { (yyval.decl) = (yyvsp[0].decl_list).first; }
#line 3028 "grammar83.tab.c"
    break;

  case 118: /* decl_item_s: %empty  */
#line 665 "grammar83.y"
                 { (yyval.decl) = NULL; }
#line 3034 "grammar83.tab.c"
    break;

  case 119: /* decl_item_s: decl_item_s1  */
#line 666 "grammar83.y"
                 { (yyval.decl) = (yyvsp[0].decl_list).first; }
#line 3040 "grammar83.tab.c"
    break;

  case 121: /* decl_item_s1: decl_item_s1 decl_item  */
#line 671 "grammar83.y"
                           {
        DeclList_splice(&(yyvsp[-1].decl_list), &(yyvsp[0].decl_list));
        (yyval.decl_list) = (yyvsp[-1].decl_list);
    }
#line 3049 "grammar83.tab.c"
    break;

  case 127: /* decl_item_or_body_s1: decl_item_or_body_s1 decl_item_or_body  */
#line 685 "grammar83.y"
                                           {
        DeclList_splice(&(yyvsp[-1].decl_list), &(yyvsp[0].decl_list));
        (yyval.decl_list) = (yyvsp[-1].decl_list);
    }
#line 3058 "grammar83.tab.c"
    break;

  case 128: /* decl_item_or_body: body  */
#line 691 "grammar83.y"
              {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), (yyvsp[0].decl));
    }
#line 3067 "grammar83.tab.c"
    break;

  case 130: /* body: subprog_body  */
#line 699 "grammar83.y"
                 { (yyval.decl) = &(yyvsp[0].subprogram_decl)->base; }
#line 3073 "grammar83.tab.c"
    break;

  case 131: /* body: pkg_body  */
#line 700 "grammar83.y"
                 { (yyval.decl) = &(yyvsp[0].pkg_body)->base; }
#line 3079 "grammar83.tab.c"
    break;

  case 132: /* name: identifier  */
#line 704 "grammar83.y"
               {
        memset(&(yyval.name), 0, sizeof((yyval.name)));
        (yyval.name).name = (yyvsp[0].str_token);
    }
#line 3088 "grammar83.tab.c"
    break;

  case 136: /* name: operator_symbol  */
#line 711 "grammar83.y"
                    {
        memset(&(yyval.name), 0, sizeof((yyval.name)));
        (yyval.name).name = (yyvsp[0].str_token);
        //TODO: lookup operator, determine its arity, and allocate args array
    }
#line 3098 "grammar83.tab.c"
    break;

  case 140: /* used_char: char_lit  */
#line 724 "grammar83.y"
             {
        (yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
        (yyval.expr)->u.char_lit = (yyvsp[0].c);
    }
#line 3107 "grammar83.tab.c"
    break;

  case 158: /* literal: numeric_lit  */
#line 768 "grammar83.y"
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
#line 3131 "grammar83.tab.c"
    break;

  case 170: /* expression: expression logical relation  */
#line 810 "grammar83.y"
                                      { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3137 "grammar83.tab.c"
    break;

  case 171: /* expression: expression short_circuit relation  */
#line 811 "grammar83.y"
                                      { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3143 "grammar83.tab.c"
    break;

  case 172: /* logical: AND  */
#line 815 "grammar83.y"
        { (yyval.unary_op) = OP_AND; }
#line 3149 "grammar83.tab.c"
    break;

  case 173: /* logical: OR  */
#line 816 "grammar83.y"
        { (yyval.unary_op) = OP_OR; }
#line 3155 "grammar83.tab.c"
    break;

  case 174: /* logical: XOR  */
#line 817 "grammar83.y"
        { (yyval.unary_op) = OP_XOR; }
#line 3161 "grammar83.tab.c"
    break;

  case 175: /* short_circuit: AND THEN  */
#line 821 "grammar83.y"
               { (yyval.unary_op) = OP_AND_THEN; }
#line 3167 "grammar83.tab.c"
    break;

  case 176: /* short_circuit: OR ELSE  */
#line 822 "grammar83.y"
               { (yyval.unary_op) = OP_OR_ELSE; }
#line 3173 "grammar83.tab.c"
    break;

  case 178: /* relation: simple_expression relational simple_expression  */
#line 828 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3179 "grammar83.tab.c"
    break;

  case 179: /* relation: simple_expression membership range  */
#line 829 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3185 "grammar83.tab.c"
    break;

  case 180: /* relation: simple_expression membership name  */
#line 830 "grammar83.y"
                                                   {
        Expression* right = create_expr(EXPR_NAME, (yylsp[0]));
        right->u.name = (yyvsp[0].name);
        (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), right);
    }
#line 3195 "grammar83.tab.c"
    break;

  case 181: /* relational: '='  */
#line 837 "grammar83.y"
          { (yyval.unary_op) = OP_EQ; }
#line 3201 "grammar83.tab.c"
    break;

  case 182: /* relational: NE  */
#line 838 "grammar83.y"
          { (yyval.unary_op) = OP_NEQ; }
#line 3207 "grammar83.tab.c"
    break;

  case 183: /* relational: '<'  */
#line 839 "grammar83.y"
          { (yyval.unary_op) = OP_LT; }
#line 3213 "grammar83.tab.c"
    break;

  case 184: /* relational: LT_EQ  */
#line 840 "grammar83.y"
          { (yyval.unary_op) = OP_LTE; }
#line 3219 "grammar83.tab.c"
    break;

  case 185: /* relational: '>'  */
#line 841 "grammar83.y"
          { (yyval.unary_op) = OP_GT; }
#line 3225 "grammar83.tab.c"
    break;

  case 186: /* relational: GE  */
#line 842 "grammar83.y"
          { (yyval.unary_op) = OP_GTE; }
#line 3231 "grammar83.tab.c"
    break;

  case 187: /* membership: IN  */
#line 846 "grammar83.y"
           { (yyval.unary_op) = OP_IN; }
#line 3237 "grammar83.tab.c"
    break;

  case 188: /* membership: NOT IN  */
#line 847 "grammar83.y"
           { (yyval.unary_op) = OP_NOT_IN; }
#line 3243 "grammar83.tab.c"
    break;

  case 190: /* simple_expression: unary term  */
#line 852 "grammar83.y"
                                  { (yyval.expr) = make_unary_expr((yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3249 "grammar83.tab.c"
    break;

  case 191: /* simple_expression: simple_expression adding term  */
#line 853 "grammar83.y"
                                  { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3255 "grammar83.tab.c"
    break;

  case 192: /* unary: '+'  */
#line 857 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_PLUS; }
#line 3261 "grammar83.tab.c"
    break;

  case 193: /* unary: '-'  */
#line 858 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_MINUS; }
#line 3267 "grammar83.tab.c"
    break;

  case 194: /* adding: '+'  */
#line 862 "grammar83.y"
        { (yyval.unary_op) = OP_PLUS; }
#line 3273 "grammar83.tab.c"
    break;

  case 195: /* adding: '-'  */
#line 863 "grammar83.y"
        { (yyval.unary_op) = OP_MINUS; }
#line 3279 "grammar83.tab.c"
    break;

  case 196: /* adding: '&'  */
#line 864 "grammar83.y"
        { (yyval.unary_op) = OP_AMP; }
#line 3285 "grammar83.tab.c"
    break;

  case 198: /* term: term multiplying factor  */
#line 869 "grammar83.y"
                            { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3291 "grammar83.tab.c"
    break;

  case 199: /* multiplying: '*'  */
#line 873 "grammar83.y"
        { (yyval.unary_op) = OP_MULT; }
#line 3297 "grammar83.tab.c"
    break;

  case 200: /* multiplying: '/'  */
#line 874 "grammar83.y"
        { (yyval.unary_op) = OP_DIVIDE; }
#line 3303 "grammar83.tab.c"
    break;

  case 201: /* multiplying: MOD  */
#line 875 "grammar83.y"
        { (yyval.unary_op) = OP_MOD; }
#line 3309 "grammar83.tab.c"
    break;

  case 202: /* multiplying: REM  */
#line 876 "grammar83.y"
        { (yyval.unary_op) = OP_REM; }
#line 3315 "grammar83.tab.c"
    break;

  case 204: /* factor: NOT primary  */
#line 881 "grammar83.y"
                          { (yyval.expr) = make_unary_expr(OP_NOT, (yyvsp[0].expr)); }
#line 3321 "grammar83.tab.c"
    break;

  case 205: /* factor: ABS primary  */
#line 882 "grammar83.y"
                          { (yyval.expr) = make_unary_expr(OP_ABS, (yyvsp[0].expr)); }
#line 3327 "grammar83.tab.c"
    break;

  case 206: /* factor: primary EXPON primary  */
#line 883 "grammar83.y"
                          { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_EXP, (yyvsp[0].expr)); }
#line 3333 "grammar83.tab.c"
    break;

  case 208: /* primary: name  */
#line 888 "grammar83.y"
         {
        (yyval.expr) = create_expr(EXPR_NAME, (yyloc));
        (yyval.expr)->u.name = (yyvsp[0].name);
    }
#line 3342 "grammar83.tab.c"
    break;

  case 213: /* parenthesized_primary: '(' expression ')'  */
#line 899 "grammar83.y"
                       { (yyval.expr) = (yyvsp[-1].expr); }
#line 3348 "grammar83.tab.c"
    break;

  case 217: /* statement_s: statement  */
#line 912 "grammar83.y"
                          {
        memset(&(yyval.stmt_list), 0, sizeof((yyval.stmt_list)));
        StmtList_append(&(yyval.stmt_list), (yyvsp[0].stmt));
    }
#line 3357 "grammar83.tab.c"
    break;

  case 218: /* statement_s: statement_s statement  */
#line 916 "grammar83.y"
                          {
        StmtList_append(&(yyvsp[-1].stmt_list), (yyvsp[0].stmt));
        (yyval.stmt_list) = (yyvsp[-1].stmt_list);
    }
#line 3366 "grammar83.tab.c"
    break;

  case 220: /* statement: goto_label statement  */
#line 923 "grammar83.y"
                         {
        check_for_redefinition(context, (yyvsp[-1].str_token), (yylsp[-1]));
        LabelDecl* label = create_label((yyvsp[-1].str_token), (yylsp[-1]));
        push_declaration(context, (Declaration*)label);
        (yyval.stmt) = (yyvsp[0].stmt);
    }
#line 3377 "grammar83.tab.c"
    break;

  case 237: /* null_stmt: NuLL ';'  */
#line 956 "grammar83.y"
             { (yyval.stmt) = create_stmt(STMT_NULL, (yyloc)); }
#line 3383 "grammar83.tab.c"
    break;

  case 238: /* assign_stmt: name IS_ASSIGNED expression ';'  */
#line 960 "grammar83.y"
                                    {
        (yyval.stmt) = create_stmt(STMT_ASSIGN, (yyloc));
        (yyval.stmt)->u.assign.dest.kind = EXPR_NAME;
        (yyval.stmt)->u.assign.dest.line_num = (yyloc);
        (yyval.stmt)->u.assign.dest.u.name = (yyvsp[-3].name);
        (yyval.stmt)->u.assign.expr = (yyvsp[-1].expr);
    }
#line 3395 "grammar83.tab.c"
    break;

  case 239: /* if_stmt: IF cond_clause_s else_opt END IF ';'  */
#line 969 "grammar83.y"
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
#line 981 "grammar83.y"
                                    {
        (yyval.stmt) = (yyvsp[-2].stmt);
        (yyval.stmt)->u.if_.else_ = (yyvsp[0].stmt);
    }
#line 3418 "grammar83.tab.c"
    break;

  case 242: /* cond_clause: cond_part statement_s  */
#line 987 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_IF, (yyloc));
        (yyval.stmt)->u.if_.condition = (yyvsp[-1].expr);
        (yyval.stmt)->u.if_.stmts = (yyvsp[0].stmt_list).first;
    }
#line 3428 "grammar83.tab.c"
    break;

  case 243: /* cond_part: condition THEN  */
#line 994 "grammar83.y"
                   { (yyval.expr) = (yyvsp[-1].expr); }
#line 3434 "grammar83.tab.c"
    break;

  case 245: /* else_opt: %empty  */
#line 1002 "grammar83.y"
                     { (yyval.stmt) = NULL; }
#line 3440 "grammar83.tab.c"
    break;

  case 246: /* else_opt: ELSE statement_s  */
#line 1003 "grammar83.y"
                     { (yyval.stmt) = (yyvsp[0].stmt_list).first; }
#line 3446 "grammar83.tab.c"
    break;

  case 247: /* case_stmt: case_hdr pragma_s alternative_s END CASE ';'  */
#line 1007 "grammar83.y"
                                                 {
        (yyval.stmt) = (yyvsp[-5].stmt);
        // TODO: pragmas
        (yyval.stmt)->u.case_.cases = (yyvsp[-3].case_list).first;
    }
#line 3456 "grammar83.tab.c"
    break;

  case 248: /* case_hdr: CASE expression IS  */
#line 1014 "grammar83.y"
                       {
        (yyval.stmt) = create_stmt(STMT_CASE, (yyloc));
        (yyval.stmt)->u.case_.expr = (yyvsp[-1].expr);
    }
#line 3465 "grammar83.tab.c"
    break;

  case 249: /* alternative_s: %empty  */
#line 1020 "grammar83.y"
                              { memset(&(yyval.case_list), 0, sizeof((yyval.case_list))); }
#line 3471 "grammar83.tab.c"
    break;

  case 250: /* alternative_s: alternative_s alternative  */
#line 1021 "grammar83.y"
                              {
        (yyval.case_list) = (yyvsp[-1].case_list);
        AltList_append(&(yyval.case_list), (yyvsp[0].case_));
    }
#line 3480 "grammar83.tab.c"
    break;

  case 251: /* alternative: WHEN choice_s RIGHT_SHAFT statement_s  */
#line 1027 "grammar83.y"
                                          {
        (yyval.case_) = calloc(1, sizeof(Alternative));
        (yyval.case_)->choices.choices = (yyvsp[-2].choice_array).data;
        (yyval.case_)->choices.count = ChoiceArray_size(&(yyvsp[-2].choice_array));
        (yyval.case_)->stmts = (yyvsp[0].stmt_list).first;
    }
#line 3491 "grammar83.tab.c"
    break;

  case 252: /* loop_stmt: label_opt loop_content id_opt ';'  */
#line 1036 "grammar83.y"
                                      { (yyval.stmt) = (yyvsp[-2].stmt); }
#line 3497 "grammar83.tab.c"
    break;

  case 255: /* loop_content: basic_loop  */
#line 1045 "grammar83.y"
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
#line 3512 "grammar83.tab.c"
    break;

  case 256: /* loop_content: WHILE condition basic_loop  */
#line 1055 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_WHILE;
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
        (yyval.stmt)->u.loop.u.while_.condition = (yyvsp[-1].expr);
    }
#line 3523 "grammar83.tab.c"
    break;

  case 257: /* loop_content: iter_part reverse_opt discrete_range basic_loop  */
#line 1061 "grammar83.y"
                                                    {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_FOR;
        (yyval.stmt)->u.loop.reverse = (yyvsp[-2].bool_);
        (yyval.stmt)->u.loop.u.for_.var = (yyvsp[-3].object_decl);
        (yyval.stmt)->u.loop.u.for_.range = (yyvsp[-1].expr);
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
    }
#line 3536 "grammar83.tab.c"
    break;

  case 258: /* iter_part: FOR identifier IN  */
#line 1071 "grammar83.y"
                      {
        memset(&(yyval.object_decl), 0, sizeof((yyval.object_decl)));
        (yyval.object_decl).base.kind = DECL_OBJECT;
        (yyval.object_decl).base.line_num = (yyloc);
        (yyval.object_decl).name = (yyvsp[-1].str_token);
    }
#line 3547 "grammar83.tab.c"
    break;

  case 259: /* reverse_opt: %empty  */
#line 1079 "grammar83.y"
            { (yyval.bool_) = false; }
#line 3553 "grammar83.tab.c"
    break;

  case 260: /* reverse_opt: REVERSE  */
#line 1080 "grammar83.y"
            { (yyval.bool_) = true; }
#line 3559 "grammar83.tab.c"
    break;

  case 261: /* basic_loop: LOOP statement_s END LOOP  */
#line 1084 "grammar83.y"
                              { (yyval.stmt) = (yyvsp[-2].stmt_list).first; }
#line 3565 "grammar83.tab.c"
    break;

  case 264: /* block: label_opt block_decl block_body END id_opt ';'  */
#line 1094 "grammar83.y"
                                                   {
        (yyval.stmt) = create_stmt(STMT_BLOCK, (yyloc));
        (yyval.stmt)->u.block.decls = (yyvsp[-4].decl);
        (yyval.stmt)->u.block.stmts = (yyvsp[-3].stmt);
        // Close scope if needed (i.e. if there was a declaration section)
        if((yyvsp[-4].decl)) {
            end_scope(context, (yylsp[-2]));
        }
    }
#line 3579 "grammar83.tab.c"
    break;

  case 265: /* block_decl: %empty  */
#line 1105 "grammar83.y"
                                                    { (yyval.decl) = NULL; }
#line 3585 "grammar83.tab.c"
    break;

  case 266: /* $@1: %empty  */
#line 1106 "grammar83.y"
            { begin_scope(context, (yylsp[0])); }
#line 3591 "grammar83.tab.c"
    break;

  case 267: /* block_decl: DECLARE $@1 decl_part  */
#line 1106 "grammar83.y"
                                                    { (yyval.decl) = (yyvsp[0].decl); }
#line 3597 "grammar83.tab.c"
    break;

  case 268: /* block_body: BEGiN handled_stmt_s  */
#line 1110 "grammar83.y"
                         { (yyval.stmt) = (yyvsp[0].stmt); }
#line 3603 "grammar83.tab.c"
    break;

  case 269: /* handled_stmt_s: statement_s except_handler_part_opt  */
#line 1115 "grammar83.y"
                                        { (yyval.stmt) = (yyvsp[-1].stmt_list).first; }
#line 3609 "grammar83.tab.c"
    break;

  case 272: /* exit_stmt: EXIT name_opt when_opt ';'  */
#line 1124 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_EXIT, (yyloc));
        // TODO: name_opt
        (yyval.stmt)->u.exit.condition = (yyvsp[-1].expr);
    }
#line 3619 "grammar83.tab.c"
    break;

  case 275: /* when_opt: %empty  */
#line 1136 "grammar83.y"
                   { (yyval.expr) = NULL; }
#line 3625 "grammar83.tab.c"
    break;

  case 276: /* when_opt: WHEN condition  */
#line 1137 "grammar83.y"
                   { (yyval.expr) = (yyvsp[0].expr); }
#line 3631 "grammar83.tab.c"
    break;

  case 277: /* return_stmt: RETURN ';'  */
#line 1141 "grammar83.y"
                  { (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc)); }
#line 3637 "grammar83.tab.c"
    break;

  case 278: /* return_stmt: RETURN expression ';'  */
#line 1142 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc));
        (yyval.stmt)->u.return_.expr = (yyvsp[-1].expr);
    }
#line 3646 "grammar83.tab.c"
    break;

  case 279: /* goto_stmt: GOTO name ';'  */
#line 1148 "grammar83.y"
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
#line 3673 "grammar83.tab.c"
    break;

  case 280: /* subprog_decl: subprog_spec ';'  */
#line 1172 "grammar83.y"
                          { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3679 "grammar83.tab.c"
    break;

  case 282: /* @2: %empty  */
#line 1178 "grammar83.y"
                                          {
        begin_scope(context, (yylsp[0]));
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
    }
#line 3689 "grammar83.tab.c"
    break;

  case 283: /* subprog_spec: PROCEDURE identifier @2 formal_part_opt  */
#line 1183 "grammar83.y"
                                { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3695 "grammar83.tab.c"
    break;

  case 284: /* @3: %empty  */
#line 1184 "grammar83.y"
                                         {
        begin_scope(context, (yylsp[0]));
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
    }
#line 3705 "grammar83.tab.c"
    break;

  case 285: /* subprog_spec: FUNCTION designator @3 formal_part_opt RETURN name  */
#line 1189 "grammar83.y"
                                { (yyval.subprogram_decl) = (yyvsp[-3].subprogram_decl); }
#line 3711 "grammar83.tab.c"
    break;

  case 288: /* designator: char_string  */
#line 1195 "grammar83.y"
                { (yyval.str_token) = string_pool_to_token((yyvsp[0].str)); }
#line 3717 "grammar83.tab.c"
    break;

  case 296: /* mode: %empty  */
#line 1218 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3723 "grammar83.tab.c"
    break;

  case 297: /* mode: IN  */
#line 1219 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3729 "grammar83.tab.c"
    break;

  case 298: /* mode: OUT  */
#line 1220 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_OUT; }
#line 3735 "grammar83.tab.c"
    break;

  case 299: /* mode: IN OUT  */
#line 1221 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN_OUT; }
#line 3741 "grammar83.tab.c"
    break;

  case 300: /* subprog_spec_is_push: subprog_spec IS  */
#line 1225 "grammar83.y"
                    { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3747 "grammar83.tab.c"
    break;

  case 301: /* subprog_body: subprog_spec_is_push decl_part block_body END id_opt ';'  */
#line 1229 "grammar83.y"
                                                             {
        (yyval.subprogram_decl) = (yyvsp[-5].subprogram_decl);
        (yyval.subprogram_decl)->decls = (yyvsp[-4].decl);
        (yyval.subprogram_decl)->stmts = (yyvsp[-3].stmt);
    }
#line 3757 "grammar83.tab.c"
    break;

  case 302: /* procedure_call: name ';'  */
#line 1236 "grammar83.y"
             {
        (yyval.stmt) = create_stmt(STMT_EXPR, (yyloc));
        (yyval.stmt)->u.expr.kind = EXPR_NAME;
        (yyval.stmt)->u.expr.line_num = (yyloc);
        (yyval.stmt)->u.expr.u.name = (yyvsp[-1].name);
    }
#line 3768 "grammar83.tab.c"
    break;

  case 303: /* pkg_decl: pkg_spec ';'  */
#line 1244 "grammar83.y"
                         { (yyval.pkg_spec) = (yyvsp[-1].pkg_spec); }
#line 3774 "grammar83.tab.c"
    break;

  case 305: /* pkg_spec: PACKAGE identifier IS decl_item_s private_part END identifier_opt  */
#line 1249 "grammar83.y"
                                                                      {
        (yyval.pkg_spec) = calloc(1, sizeof(PackageSpec));
        (yyval.pkg_spec)->base.kind = DECL_PKG_SPEC;
        (yyval.pkg_spec)->base.line_num = (yyloc);
        (yyval.pkg_spec)->name = (yyvsp[-5].str_token);
        (yyval.pkg_spec)->decls = (yyvsp[-3].decl);
        // TODO: private part
        // TODO: check identifier_opt matches
    }
#line 3788 "grammar83.tab.c"
    break;

  case 310: /* pkg_body: PACKAGE BODY identifier IS decl_part body_opt END identifier_opt ';'  */
#line 1270 "grammar83.y"
                                                                         {
        (yyval.pkg_body) = calloc(1, sizeof(PackageBody));
        (yyval.pkg_body)->base.kind = DECL_PKG_BODY;
        (yyval.pkg_body)->base.line_num = (yyloc);
        (yyval.pkg_body)->name = (yyvsp[-6].str_token);
        (yyval.pkg_body)->decls = (yyvsp[-4].decl);
        // TODO: body_opt
        // TODO: check identifier_opt matches
    }
#line 3802 "grammar83.tab.c"
    break;

  case 327: /* comp_unit: context_spec unit pragma_s  */
#line 1321 "grammar83.y"
                               { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3808 "grammar83.tab.c"
    break;

  case 328: /* comp_unit: unit pragma_s  */
#line 1322 "grammar83.y"
                               { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3814 "grammar83.tab.c"
    break;

  case 335: /* unit: pkg_decl  */
#line 1341 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_PACKAGE_SPEC);
        (yyval.comp_unit)->u.package_spec = (yyvsp[0].pkg_spec);
    }
#line 3823 "grammar83.tab.c"
    break;

  case 336: /* unit: pkg_body  */
#line 1345 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_PACKAGE_BODY);
        (yyval.comp_unit)->u.package_body = (yyvsp[0].pkg_body);
    }
#line 3832 "grammar83.tab.c"
    break;

  case 337: /* unit: subprog_decl  */
#line 1349 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        (yyval.comp_unit)->u.subprogram_decl = (yyvsp[0].subprogram_decl);
    }
#line 3841 "grammar83.tab.c"
    break;

  case 338: /* unit: subprog_body  */
#line 1353 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        (yyval.comp_unit)->u.subprogram_decl = (yyvsp[0].subprogram_decl);
    }
#line 3850 "grammar83.tab.c"
    break;


#line 3854 "grammar83.tab.c"

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

#line 1498 "grammar83.y"


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
