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
  YYSYMBOL_used_char = 163,                /* used_char  */
  YYSYMBOL_operator_symbol = 164,          /* operator_symbol  */
  YYSYMBOL_indexed_comp = 165,             /* indexed_comp  */
  YYSYMBOL_value_s = 166,                  /* value_s  */
  YYSYMBOL_value = 167,                    /* value  */
  YYSYMBOL_selected_comp = 168,            /* selected_comp  */
  YYSYMBOL_attribute = 169,                /* attribute  */
  YYSYMBOL_attribute_id = 170,             /* attribute_id  */
  YYSYMBOL_literal = 171,                  /* literal  */
  YYSYMBOL_aggregate = 172,                /* aggregate  */
  YYSYMBOL_value_s_2 = 173,                /* value_s_2  */
  YYSYMBOL_comp_assoc = 174,               /* comp_assoc  */
  YYSYMBOL_expression = 175,               /* expression  */
  YYSYMBOL_logical = 176,                  /* logical  */
  YYSYMBOL_short_circuit = 177,            /* short_circuit  */
  YYSYMBOL_relation = 178,                 /* relation  */
  YYSYMBOL_relational = 179,               /* relational  */
  YYSYMBOL_membership = 180,               /* membership  */
  YYSYMBOL_simple_expression = 181,        /* simple_expression  */
  YYSYMBOL_unary = 182,                    /* unary  */
  YYSYMBOL_adding = 183,                   /* adding  */
  YYSYMBOL_term = 184,                     /* term  */
  YYSYMBOL_multiplying = 185,              /* multiplying  */
  YYSYMBOL_factor = 186,                   /* factor  */
  YYSYMBOL_primary = 187,                  /* primary  */
  YYSYMBOL_parenthesized_primary = 188,    /* parenthesized_primary  */
  YYSYMBOL_qualified = 189,                /* qualified  */
  YYSYMBOL_allocator = 190,                /* allocator  */
  YYSYMBOL_statement_s = 191,              /* statement_s  */
  YYSYMBOL_statement = 192,                /* statement  */
  YYSYMBOL_unlabeled = 193,                /* unlabeled  */
  YYSYMBOL_simple_stmt = 194,              /* simple_stmt  */
  YYSYMBOL_compound_stmt = 195,            /* compound_stmt  */
  YYSYMBOL_null_stmt = 196,                /* null_stmt  */
  YYSYMBOL_assign_stmt = 197,              /* assign_stmt  */
  YYSYMBOL_if_stmt = 198,                  /* if_stmt  */
  YYSYMBOL_cond_clause_s = 199,            /* cond_clause_s  */
  YYSYMBOL_cond_clause = 200,              /* cond_clause  */
  YYSYMBOL_cond_part = 201,                /* cond_part  */
  YYSYMBOL_condition = 202,                /* condition  */
  YYSYMBOL_else_opt = 203,                 /* else_opt  */
  YYSYMBOL_case_stmt = 204,                /* case_stmt  */
  YYSYMBOL_case_hdr = 205,                 /* case_hdr  */
  YYSYMBOL_alternative_s = 206,            /* alternative_s  */
  YYSYMBOL_alternative = 207,              /* alternative  */
  YYSYMBOL_loop_stmt = 208,                /* loop_stmt  */
  YYSYMBOL_label_opt = 209,                /* label_opt  */
  YYSYMBOL_loop_content = 210,             /* loop_content  */
  YYSYMBOL_iter_part = 211,                /* iter_part  */
  YYSYMBOL_reverse_opt = 212,              /* reverse_opt  */
  YYSYMBOL_basic_loop = 213,               /* basic_loop  */
  YYSYMBOL_id_opt = 214,                   /* id_opt  */
  YYSYMBOL_block = 215,                    /* block  */
  YYSYMBOL_block_decl = 216,               /* block_decl  */
  YYSYMBOL_217_1 = 217,                    /* $@1  */
  YYSYMBOL_block_body = 218,               /* block_body  */
  YYSYMBOL_handled_stmt_s = 219,           /* handled_stmt_s  */
  YYSYMBOL_except_handler_part_opt = 220,  /* except_handler_part_opt  */
  YYSYMBOL_exit_stmt = 221,                /* exit_stmt  */
  YYSYMBOL_name_opt = 222,                 /* name_opt  */
  YYSYMBOL_when_opt = 223,                 /* when_opt  */
  YYSYMBOL_return_stmt = 224,              /* return_stmt  */
  YYSYMBOL_goto_stmt = 225,                /* goto_stmt  */
  YYSYMBOL_subprog_decl = 226,             /* subprog_decl  */
  YYSYMBOL_subprog_spec = 227,             /* subprog_spec  */
  YYSYMBOL_228_2 = 228,                    /* @2  */
  YYSYMBOL_229_3 = 229,                    /* @3  */
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
  YYSYMBOL_simple_name_opt = 242,          /* simple_name_opt  */
  YYSYMBOL_pkg_body = 243,                 /* pkg_body  */
  YYSYMBOL_body_opt = 244,                 /* body_opt  */
  YYSYMBOL_private_type = 245,             /* private_type  */
  YYSYMBOL_limited_opt = 246,              /* limited_opt  */
  YYSYMBOL_use_clause = 247,               /* use_clause  */
  YYSYMBOL_name_s = 248,                   /* name_s  */
  YYSYMBOL_rename_decl = 249,              /* rename_decl  */
  YYSYMBOL_rename_unit = 250,              /* rename_unit  */
  YYSYMBOL_renames = 251,                  /* renames  */
  YYSYMBOL_comp_unit = 252,                /* comp_unit  */
  YYSYMBOL_context_spec = 253,             /* context_spec  */
  YYSYMBOL_with_clause = 254,              /* with_clause  */
  YYSYMBOL_use_clause_opt = 255,           /* use_clause_opt  */
  YYSYMBOL_unit = 256,                     /* unit  */
  YYSYMBOL_subunit = 257,                  /* subunit  */
  YYSYMBOL_subunit_body = 258,             /* subunit_body  */
  YYSYMBOL_body_stub = 259,                /* body_stub  */
  YYSYMBOL_exception_decl = 260,           /* exception_decl  */
  YYSYMBOL_except_handler_part = 261,      /* except_handler_part  */
  YYSYMBOL_exception_handler = 262,        /* exception_handler  */
  YYSYMBOL_except_choice_s = 263,          /* except_choice_s  */
  YYSYMBOL_except_choice = 264,            /* except_choice  */
  YYSYMBOL_raise_stmt = 265,               /* raise_stmt  */
  YYSYMBOL_generic_decl = 266,             /* generic_decl  */
  YYSYMBOL_generic_formal_part = 267,      /* generic_formal_part  */
  YYSYMBOL_generic_formal = 268,           /* generic_formal  */
  YYSYMBOL_generic_discrim_part_opt = 269, /* generic_discrim_part_opt  */
  YYSYMBOL_subp_default = 270,             /* subp_default  */
  YYSYMBOL_generic_type_def = 271,         /* generic_type_def  */
  YYSYMBOL_generic_derived_type = 272,     /* generic_derived_type  */
  YYSYMBOL_generic_subp_inst = 273,        /* generic_subp_inst  */
  YYSYMBOL_generic_pkg_inst = 274,         /* generic_pkg_inst  */
  YYSYMBOL_generic_inst = 275,             /* generic_inst  */
  YYSYMBOL_rep_spec = 276,                 /* rep_spec  */
  YYSYMBOL_attrib_def = 277,               /* attrib_def  */
  YYSYMBOL_record_type_spec = 278,         /* record_type_spec  */
  YYSYMBOL_align_opt = 279,                /* align_opt  */
  YYSYMBOL_comp_loc_s = 280,               /* comp_loc_s  */
  YYSYMBOL_address_spec = 281,             /* address_spec  */
  YYSYMBOL_code_stmt = 282                 /* code_stmt  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;



/* Unqualified %code blocks.  */
#line 78 "grammar83.y"

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

#line 478 "grammar83.tab.c"

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
#define YYFINAL  35
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1401

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  95
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  188
/* YYNRULES -- Number of rules.  */
#define YYNRULES  400
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
       0,   254,   254,   258,   259,   263,   264,   268,   269,   273,
     274,   278,   279,   280,   284,   288,   292,   296,   297,   298,
     299,   300,   304,   330,   345,   349,   354,   355,   359,   360,
     364,   365,   369,   381,   382,   383,   388,   389,   393,   394,
     395,   396,   397,   398,   399,   400,   404,   421,   425,   429,
     430,   434,   438,   449,   453,   454,   458,   459,   460,   464,
     472,   476,   482,   486,   492,   496,   500,   501,   505,   509,
     510,   514,   515,   519,   523,   527,   531,   532,   536,   540,
     544,   545,   549,   550,   554,   558,   559,   563,   564,   565,
     569,   570,   574,   575,   579,   580,   584,   588,   589,   593,
     594,   598,   599,   603,   607,   608,   612,   616,   620,   626,
     630,   631,   635,   636,   640,   641,   645,   646,   650,   651,
     655,   656,   662,   663,   664,   665,   669,   670,   676,   680,
     684,   685,   689,   693,   694,   695,   696,   703,   704,   705,
     709,   713,   719,   723,   727,   728,   732,   733,   734,   735,
     739,   740,   741,   742,   746,   750,   751,   752,   753,   757,
     776,   777,   781,   782,   783,   784,   785,   789,   790,   794,
     798,   799,   800,   804,   805,   806,   810,   811,   816,   817,
     818,   819,   826,   827,   828,   829,   830,   831,   835,   836,
     840,   841,   842,   846,   847,   851,   852,   853,   857,   858,
     862,   863,   864,   865,   869,   870,   871,   872,   876,   877,
     881,   882,   883,   887,   888,   892,   896,   897,   901,   905,
     911,   912,   920,   921,   922,   926,   927,   928,   929,   930,
     931,   932,   933,   934,   938,   939,   940,   941,   945,   949,
     958,   969,   970,   976,   983,   987,   991,   992,   996,  1003,
    1009,  1010,  1016,  1025,  1029,  1030,  1034,  1044,  1050,  1060,
    1068,  1069,  1073,  1077,  1078,  1083,  1094,  1095,  1095,  1099,
    1104,  1108,  1109,  1113,  1120,  1121,  1125,  1126,  1130,  1131,
    1137,  1161,  1162,  1167,  1167,  1173,  1173,  1179,  1183,  1184,
    1188,  1189,  1193,  1197,  1198,  1202,  1203,  1207,  1208,  1209,
    1210,  1214,  1218,  1225,  1233,  1234,  1238,  1249,  1250,  1254,
    1255,  1259,  1270,  1271,  1275,  1279,  1280,  1284,  1288,  1289,
    1293,  1294,  1295,  1299,  1300,  1301,  1302,  1306,  1310,  1311,
    1315,  1316,  1317,  1321,  1325,  1326,  1330,  1334,  1338,  1342,
    1346,  1347,  1348,  1352,  1356,  1357,  1361,  1362,  1366,  1370,
    1371,  1375,  1376,  1380,  1381,  1385,  1386,  1390,  1394,  1395,
    1399,  1400,  1404,  1405,  1406,  1407,  1408,  1409,  1410,  1414,
    1415,  1416,  1420,  1421,  1422,  1426,  1427,  1428,  1429,  1430,
    1431,  1432,  1433,  1434,  1435,  1439,  1440,  1444,  1448,  1452,
    1456,  1457,  1458,  1462,  1466,  1470,  1471,  1475,  1476,  1480,
    1484
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
  "decl_item_or_body", "body", "name", "mark", "simple_name", "used_char",
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
  "pkg_decl", "pkg_spec", "private_part", "simple_name_opt", "pkg_body",
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

#define YYPACT_NINF (-544)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-353)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     808,     1,  -544,    28,    14,   110,   122,   115,  -544,   371,
    1301,  -544,  -544,    44,  -544,  -544,  -544,   686,  -544,  -544,
    -544,  -544,   160,   123,   221,  -544,  -544,  -544,     3,    14,
     356,  -544,    14,  -544,   201,  -544,   199,   195,  -544,   244,
     281,    14,    80,   308,   330,   339,   195,  -544,  -544,  -544,
    -544,   317,  -544,  -544,   373,  -544,  1199,  -544,  -544,  -544,
     402,  -544,  -544,  -544,  -544,  -544,  -544,  -544,  -544,  -544,
    -544,  -544,  -544,  -544,  -544,  -544,  -544,  -544,   328,   395,
    -544,    14,    14,   247,   326,     7,   355,   374,  -544,  -544,
    -544,  -544,   403,   453,  1145,   419,   403,   420,  -544,   436,
     195,  -544,  -544,   547,  -544,  -544,  -544,  -544,  -544,  -544,
    -544,   441,  -544,    14,   457,   435,   480,   458,   547,   209,
      53,  1130,   517,  -544,   282,   328,   395,  -544,  -544,   392,
     471,     1,    14,    14,   165,  -544,   479,  -544,  -544,    68,
     501,  -544,  1243,   188,   513,  1235,  -544,   464,  -544,  -544,
    -544,   411,  -544,   547,   655,   219,   234,   756,   219,    14,
     531,  -544,   812,   195,    66,   551,  -544,  -544,   195,   604,
       9,   286,   536,   812,   195,   195,   812,   541,   195,   775,
     546,  1130,  -544,    92,   553,   911,  -544,  -544,  -544,  -544,
    -544,  -544,  -544,  -544,  -544,  -544,   425,  -544,  -544,  -544,
    -544,  -544,  -544,  -544,  -544,     1,   555,  1279,   565,    93,
    -544,   608,   403,   615,   403,   610,  -544,    14,  -544,   297,
    -544,   195,   373,    14,  1326,   638,  -544,   282,   653,   631,
    -544,  -544,  -544,  -544,   984,   195,   984,  -544,  -544,  -544,
    -544,   679,  -544,  -544,  -544,    32,  -544,   463,    18,  -544,
     487,  -544,  -544,  -544,  -544,   208,  -544,   382,   260,   272,
    -544,   671,  -544,  -544,  -544,  -544,  -544,  -544,  -544,  -544,
    -544,  -544,  -544,  -544,   812,   672,   559,   148,   674,  -544,
    -544,  1191,   529,  -544,   693,   473,   632,   318,  -544,   639,
     564,   364,  -544,   338,   660,   547,   812,  -544,   662,   668,
     709,   691,  -544,  -544,  -544,  -544,   486,   547,   681,   586,
     473,   624,  -544,  1130,   690,  -544,   694,  -544,   238,  -544,
    -544,   812,  -544,   332,  -544,   705,  -544,  -544,   705,   395,
    -544,   685,  1130,   812,     1,   716,  -544,   373,   703,  -544,
    -544,  -544,   702,   462,   725,   742,   749,  -544,    38,    68,
    -544,   547,  -544,   760,   754,  -544,    14,  -544,  -544,   559,
    -544,  -544,   739,   717,   576,   722,   178,   812,   793,   812,
     637,  -544,  -544,   655,   731,   776,  -544,   812,   812,   812,
    -544,  -544,  -544,  -544,   767,  -544,  -544,  -544,  -544,  -544,
    -544,   812,   812,   260,   272,  -544,  -544,  -544,  -544,   260,
     984,   268,   764,  -544,  -544,   729,   737,   812,   812,  -544,
     812,  -544,  -544,  -544,  -544,   800,   104,  -544,   257,   812,
     812,  -544,   812,   195,   622,  -544,  -544,  -544,  -544,  -544,
    -544,  -544,  -544,  -544,  -544,  -544,  -544,   540,  -544,   277,
    -544,   812,   770,   812,   743,   744,  -544,   812,   748,  -544,
    1130,   812,   788,   876,  -544,  -544,  -544,   280,  -544,   481,
    -544,  -544,    41,  1301,   789,  1067,   787,   755,  -544,   812,
     805,  -544,  -544,   836,   837,   841,   195,   849,   856,  -544,
    -544,  -544,   810,   782,  -544,   195,   195,   112,   785,  -544,
    -544,    14,   809,  -544,  -544,   794,   655,  -544,   655,  -544,
     735,  -544,   473,  -544,  -544,   473,  -544,   603,    36,   796,
    -544,  -544,  -544,  -544,  -544,   509,  -544,   509,  -544,   345,
     272,  -544,  -544,  -544,   812,   162,  -544,  -544,  -544,   473,
     444,  -544,    14,  -544,   195,  -544,   407,   444,   473,  -544,
    -544,  -544,   626,  -544,   817,  -544,  -544,  -544,  -544,  -544,
     628,  -544,   633,  -544,   711,   195,   473,  -544,  -544,  -544,
    -544,  1095,  -544,   840,  -544,  -544,   798,   547,    45,  -544,
     858,   793,  -544,  -544,  -544,   843,  -544,  -544,   787,   556,
       1,   864,  -544,  -544,   819,  -544,   801,  -544,   340,   692,
    -544,   547,  -544,   814,  -544,  -544,  -544,   839,   645,   812,
     322,   844,    26,  -544,  -544,    38,  -544,   812,  -544,  -544,
    -544,   622,  -544,   164,   847,   195,  -544,   812,   504,  -544,
    -544,  -544,   821,   514,  1130,   514,   828,    49,  -544,  -544,
     829,   894,   862,  -544,   832,  -544,   528,  -544,   835,  -544,
      48,  -544,   838,   812,  -544,   444,  -544,   842,    14,   845,
     651,   889,  -544,  -544,  -544,   195,  -544,   451,  -544,  -544,
    -544,    51,   939,  -544,  -544,  1130,  -544,  -544,  -544,  -544,
     846,  -544,  -544,  -544,   466,  -544,  -544,   880,  -544,   195,
     866,   217,  -544,   395,  -544,   921,  1130,   974,   850,   812,
    -544,   395,   709,  -544,  -544,  -544,  1032,  -544,   854,   149,
     859,   395,  -544,   793,    43,  -544,  -544,    55,   905,  -544,
    -544,   860,   164,  -544,  -544
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     0,   360,     0,     0,     0,     0,     0,   338,     0,
       0,   339,   336,     0,   337,   342,     2,     0,   334,     9,
     340,   341,     0,     0,     0,   140,   289,   288,   287,     0,
       0,   283,     0,    24,     0,     1,   301,     0,   281,     0,
       0,     0,     0,     0,     0,     0,     0,   125,   122,    11,
      12,     0,    13,    14,     0,   129,     0,   126,   128,    15,
       0,   130,    16,   131,   123,    18,   322,    20,    17,    19,
     124,   390,   391,   392,   304,   332,   334,     9,   330,   329,
     296,     0,     0,     0,     0,     0,     0,     0,   368,   361,
     282,   305,   290,     0,     0,     0,   290,     0,   333,     0,
       0,   387,   142,   327,   132,   136,   133,   134,   135,   324,
      21,     0,   137,     0,   140,     0,     0,    33,   318,     0,
      26,     0,     0,   127,   301,   331,   328,   335,    10,     0,
     369,     0,     0,     0,   297,   358,     0,   362,   359,     0,
       0,   291,     0,     0,   307,     0,   120,     0,   388,   323,
     284,     0,    25,   389,     0,     0,     0,     0,     0,     0,
       0,     3,     0,     0,     0,    36,    34,   317,     0,    27,
       0,     0,     0,     0,   274,     0,     0,     0,   274,     0,
     140,     0,   224,     0,     0,     0,   218,   220,   222,   223,
     225,   226,   234,   235,     9,   236,   266,   237,   269,   227,
     228,   229,   230,   231,   232,   263,     0,     0,     0,     0,
     370,     0,   290,     0,   290,   298,   299,     0,   326,     0,
     293,     0,   312,     0,     0,     0,   121,     0,     0,     0,
     344,   345,   343,   149,     0,     0,     0,   161,   111,   141,
     159,     0,   193,   194,   113,     0,   107,   110,   209,   160,
       0,   144,   208,   213,   147,   109,   170,   178,     0,   190,
     198,   204,   212,   211,   210,   158,   157,   156,   155,   154,
     153,   150,   151,   152,     0,   395,   209,     0,   178,   138,
     139,     0,     0,     5,   132,     7,     0,    48,   100,     0,
       0,     0,    97,   315,     0,   319,     0,   348,     0,     0,
      30,    28,    29,    71,    72,   233,     0,   275,   276,     0,
     245,   246,   241,     0,     0,   238,     0,   278,     0,   255,
     221,     0,   303,     0,   400,     0,   219,   270,   272,   250,
     267,     0,     0,     0,   263,   260,   256,     0,     0,   264,
     347,   325,     0,   315,     0,     0,   372,   300,    30,     0,
     292,   286,   313,     0,     0,   308,   309,   301,   206,   216,
     217,   205,   161,     0,     0,   147,   109,     0,     0,     0,
       0,   112,   143,     0,   173,   174,   175,     0,     0,     0,
     185,   183,   187,   188,     0,   182,   184,   186,   195,   196,
     197,     0,     0,     0,   191,   202,   203,   200,   201,     0,
       0,     0,     0,   397,   393,     0,     0,     0,     0,    46,
       0,    47,    50,    49,    35,   101,     0,    96,     0,     0,
       0,   316,     0,     0,     0,    37,    44,    64,    38,    39,
      40,    66,    67,    41,    42,    43,    45,     0,    32,     0,
     321,     0,     0,     0,     0,     0,   249,     0,     0,   280,
       0,     0,     0,     0,   244,   357,   279,     0,   215,     0,
     349,   350,     0,     0,     0,     0,     0,     0,   261,     0,
       0,   302,   371,     0,     0,     0,     0,     0,     0,   381,
     382,   383,     0,     0,   384,     0,     0,     0,     0,   295,
     294,   309,     0,   310,   306,     0,     0,   163,     0,   162,
       0,   214,   169,   108,   110,   109,    53,   209,     0,    57,
     145,   176,   177,   171,   172,    56,   189,   179,   180,   181,
     192,   199,   207,   399,     0,     0,   346,     4,     6,     8,
      54,   102,     0,    98,     0,   114,     0,    54,    65,    52,
      63,    62,     0,    60,     0,   314,     9,    84,    23,    83,
       0,    76,     0,    80,   209,     0,    31,    22,   320,   277,
     273,     0,   242,     0,   239,   356,   140,   355,     0,   353,
       0,     0,   251,   268,   259,     0,   257,   253,     0,   209,
     263,   378,   380,   377,   385,   376,     0,   363,   372,     0,
     374,   373,   364,     0,   166,   167,   168,   161,     0,     0,
       0,     0,     0,    55,    51,    30,   115,     0,    69,    68,
      59,     0,    86,     0,     0,     0,    79,     0,     0,    82,
      75,    74,     0,     0,     0,     0,     0,     0,   262,   258,
       0,     0,     0,   375,     0,   367,     0,   311,     0,   164,
       0,   396,     0,     0,    99,    54,    61,     0,     0,     0,
       0,     0,     9,    90,     9,     0,    77,     0,    81,    78,
     240,     0,     0,   354,   248,     0,   265,   379,   386,   365,
       0,   165,    58,   394,     0,    70,    95,     0,     9,     0,
       0,     0,    87,    88,    73,     0,     0,     0,     0,     0,
       9,    89,    30,    85,    91,     9,     0,   366,     0,     0,
       0,    93,   398,     0,     0,   104,    94,     0,     0,   105,
       9,     0,     0,   103,   106
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -544,  -544,    -7,  -544,   530,   -69,  -544,  -544,  -544,    -6,
    -544,  -544,  -343,  -544,  -544,  -544,  -544,  -544,  -157,  -544,
    -544,  -544,  -237,  -500,  -360,  -544,  -544,   324,  -544,  -544,
    -544,  -544,  -235,  -544,  -544,  -456,  -544,   327,  -544,  -544,
    -451,  -544,  -544,   243,  -544,  -544,   265,   820,  -544,   543,
    -544,   275,  -544,   261,  -543,   593,  -364,   621,    -9,   746,
    -544,   -65,  -544,   912,  -544,    -1,  -205,    50,   811,   818,
    -544,   478,  -221,  -544,  -544,   822,  -544,  -544,  -544,   738,
     145,  -544,  -544,   365,  -544,  -544,  -127,  -544,  -544,  -239,
    -544,   584,  -211,  -263,   -37,  -544,  -311,  -168,  -544,  -544,
    -544,  -544,  -544,  -544,  -544,   533,  -544,  -290,  -544,  -544,
    -544,  -544,  -544,  -544,  -544,  -544,  -544,  -544,  -428,  -312,
    -544,  -544,  -544,  -191,  -544,  -544,  -544,   823,  -544,  -544,
    -544,    78,    24,  -544,  -544,    25,   -63,  -544,  -544,  -124,
    -544,  -544,    35,  -544,   205,   966,  -544,   499,    40,  -544,
     649,   657,    12,  -544,  -544,   213,   -23,  -544,  -544,   979,
     922,   985,  -544,  -544,  -544,  -544,  -544,   675,   383,   380,
    -544,   236,  -544,  -544,  -544,   430,  -544,  -544,  -544,  -544,
     919,  -544,  -544,  -544,  -544,  -544,  -544,  -544
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     7,   182,   282,   283,    79,    48,    49,    50,    51,
     171,   300,   444,    52,   165,   294,   425,    53,   620,   411,
     412,   426,   603,   619,   244,   428,   542,   543,   429,   430,
     431,   432,   302,   303,   304,   621,   550,   551,   442,   552,
     553,   434,   547,   651,   652,   682,   653,   166,   291,   292,
     532,   654,   704,   705,   245,   246,   247,   435,   222,   144,
     145,    55,    56,    57,    58,   276,   111,   104,   249,   105,
     106,   250,   251,   107,   108,   269,   252,   253,   364,   254,
     255,   377,   378,   256,   391,   392,   278,   258,   393,   259,
     399,   260,   261,   262,   263,   264,   185,   186,   187,   188,
     189,   190,   191,   192,   311,   312,   313,   314,   452,   193,
     194,   462,   572,   195,   196,   334,   335,   469,   336,   338,
     197,   337,   463,   122,   198,   327,   199,   308,   448,   200,
     201,    59,    60,    96,    92,   339,   140,   141,   219,    86,
     217,    10,    61,   202,    62,    13,   225,   494,    63,   353,
     436,   437,    64,   119,    65,    66,    39,    16,    17,    18,
      78,    19,    20,   232,    67,    68,   328,   460,   568,   569,
     203,    69,    22,    89,   211,   488,   483,   484,    23,    24,
     101,    70,    71,    72,   403,   525,    73,   204
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      34,    54,   453,    47,   504,   489,   286,    95,   126,   506,
      75,   371,   348,   320,   301,   220,    84,   326,   578,   394,
     363,   465,   467,   358,     9,   361,    28,   257,   627,   146,
     604,   352,   518,   150,    88,    11,   103,   609,   576,   379,
      14,     9,   367,   466,   643,   118,    85,   443,    29,    47,
     413,    27,    11,    30,    31,   624,   427,    14,   433,   665,
     458,   686,   136,  -285,   374,   710,    37,   288,    37,    80,
     289,   570,   128,   708,   369,   169,    25,    26,     8,    93,
     226,   549,    97,  -285,   184,   170,   135,    47,   297,    25,
     127,   112,    30,   115,   288,     8,   375,   342,   154,   153,
     113,   321,   370,    25,   156,   288,   208,   458,   479,   549,
     158,   571,   159,   703,   257,    35,   590,   368,   147,   128,
     183,   376,   158,    74,   159,   329,   388,   389,   390,   672,
     625,   129,   130,    84,   368,    47,   625,   127,    47,   561,
     368,    33,   146,    33,   184,   675,   470,   298,   184,   344,
     629,   346,   510,   248,   520,    25,   212,   559,   290,   146,
     707,    80,   287,   160,   374,   647,   658,   295,    33,   147,
     287,   322,   154,   307,   309,   229,   323,   307,   156,    33,
     183,    27,   213,   214,   183,   648,   230,    25,   102,   522,
      32,   231,   601,    30,   374,     1,   375,    33,   360,   684,
      47,    43,    90,   290,   215,    12,   271,   504,   223,   280,
     649,    81,   284,    15,     4,   216,    43,    47,   647,   703,
     351,   376,    12,   692,   374,   490,   375,   404,    82,    46,
      15,   147,    83,   265,   359,    33,    21,    25,   648,    33,
     248,   257,   508,   100,   266,   267,   257,   -92,   147,   270,
     500,   376,   515,    21,   374,    27,   375,   549,   506,   501,
    -146,   535,   644,    25,   517,   508,   539,   112,   630,    43,
      25,   102,   234,   354,    47,   595,   184,   596,   445,   534,
      98,   376,   131,    99,   374,   326,   375,   -92,   167,  -146,
    -146,   168,    33,   374,   268,   184,   374,   326,   132,   608,
      91,   133,   277,   299,   235,   236,   237,   285,   239,    25,
     102,   376,   183,   662,   508,   395,   375,   456,   306,   584,
     602,   310,   128,   109,   318,   375,   100,   605,   375,   698,
     396,   183,    25,   102,   239,    25,   102,   240,   374,   504,
     241,   376,   508,    84,   410,   206,   265,   523,  -209,   700,
     376,  -209,   418,   376,   687,   299,   548,   266,   267,   564,
     110,    25,   102,   419,   420,   397,   398,   248,   507,   257,
     375,   257,   248,   257,   369,   696,   349,   606,   350,   421,
     487,   422,   423,   114,    27,   379,   366,   380,  -209,   381,
     382,   519,   121,   326,   369,   376,    94,    46,   154,    99,
     120,   641,   155,  -209,   156,   116,   493,   268,    99,   134,
     290,    36,   241,   184,   117,    37,   184,   287,   424,   401,
     154,   383,   287,   374,   155,   154,   156,   384,   184,   370,
      37,   156,   207,   607,   137,  -209,  -209,  -209,  -209,  -209,
     554,   439,   124,   416,   257,   417,     1,    43,   330,   183,
      38,    37,   183,   138,   573,   375,    47,   284,   567,   331,
     374,    37,   228,   369,   183,     4,   457,   332,   579,   385,
     386,   387,   388,   389,   390,   287,   418,   613,   310,   299,
     376,    38,   374,   139,   588,   589,   591,   473,   474,   374,
     508,   508,   375,   142,   326,   248,   333,   248,   149,   248,
     369,   151,   374,   421,   227,   475,   476,   685,   659,   257,
     157,   152,   502,   505,   375,   162,   234,   376,   477,   326,
     163,   375,   689,    37,   184,   158,   446,   159,   326,   233,
     565,   154,   670,   287,   375,   155,   161,   156,   164,   376,
     234,   493,   478,    38,  -148,  -148,   376,   205,   235,   236,
     237,   209,   285,   529,   287,   530,   566,   102,   218,   376,
     183,   221,   508,   565,   536,   537,   224,   538,   372,   373,
     248,   281,   235,   236,   237,   112,   257,   238,   239,    25,
     102,   240,   112,   681,   241,   683,   544,   184,   556,    25,
     102,   293,   310,   545,   242,   243,   310,   546,   -54,   388,
     389,   390,   239,    25,   102,   240,   128,   650,   241,   691,
     406,   407,   369,   296,   657,   305,   579,   507,   242,   243,
     315,   699,   567,   183,   567,   184,   701,   154,   184,   319,
      27,   155,   324,   156,   340,   248,   154,   -54,   -54,   154,
     370,   712,   156,   323,   341,   156,    99,   415,   343,   184,
     184,   265,   450,   451,   287,   345,   233,   497,   498,   184,
     347,   183,   266,   267,   183,   449,   154,   234,   356,   600,
     155,   357,   156,    29,   128,   650,   128,   400,   287,   380,
     233,   381,   382,   154,   128,   183,   183,   370,   507,   156,
     402,   234,   128,   509,   128,   183,   540,   541,   677,   235,
     236,   237,   248,   408,   238,   128,   650,   610,   611,   614,
     615,   409,   268,   383,   616,   617,   505,   241,   443,   384,
     414,     1,     2,   235,   236,   362,   639,   373,   238,   239,
      25,   102,   240,    99,   679,   241,   233,     3,    43,   438,
       4,   440,   513,   514,   640,   242,   243,   234,   441,     5,
      37,   447,   645,   239,    25,   102,   240,   454,     6,   241,
     464,   385,   386,   387,   388,   389,   390,   618,   234,   242,
     243,   635,   636,   455,   274,   459,   155,   468,   156,   235,
     236,   597,   471,   472,   238,   485,   486,   234,   674,   487,
     491,   154,   -54,   -54,   492,   370,   495,   156,   511,   496,
     235,   236,   237,   499,   512,   234,   516,   524,   526,   239,
      25,   102,   240,   275,   531,   241,   527,   555,   563,   235,
     236,   237,   557,   558,   234,   242,   243,   560,   574,   332,
     239,    25,   102,   240,   577,   580,   241,   235,   236,   237,
     581,   582,   238,     1,     2,   583,   242,   243,   505,   239,
      25,   102,   240,   585,   317,   241,   235,   236,   237,     3,
     586,   587,     4,   545,   592,   242,   243,   239,    25,   102,
     240,     5,   405,   241,   612,   594,   599,   172,   622,   626,
       6,   623,   633,   242,   243,   628,   239,    25,   102,   240,
     631,   632,   241,   637,   655,  -254,   638,   173,   667,  -254,
     660,   642,   242,   243,  -243,  -243,  -243,   664,   666,   174,
    -254,   669,   172,   175,   176,   668,   671,   673,  -254,   680,
     690,   676,   177,   693,   678,   659,   711,   688,    43,   697,
    -254,   178,   173,   702,  -254,   646,   179,   528,   706,   713,
     172,  -271,   656,   325,   174,  -254,   694,  -254,   175,   176,
     210,   180,   102,  -254,   181,   714,   695,   177,  -254,   533,
     173,   503,  -254,    43,   480,   709,   178,   272,   123,  -351,
     355,   179,   174,  -254,   273,   172,   175,   176,   598,   365,
     279,  -254,  -254,   521,   562,   177,   180,   102,    87,   181,
     593,    43,   481,  -254,   178,   173,    76,  -254,   125,   179,
     482,   316,    77,   461,  -252,   663,   661,   174,  -254,  -351,
    -254,   175,   176,   148,   180,   102,  -254,   181,   634,     0,
     177,     0,     0,     0,     0,     0,    43,     0,   235,   178,
     237,     0,     0,   172,   179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -252,  -254,     0,     0,     0,   180,
     102,  -254,   181,   173,     0,  -254,     0,     0,   239,    25,
     102,   240,  -352,     0,   241,   174,  -254,     0,   172,   175,
     176,     0,     0,     0,  -254,     0,     0,     0,   177,     0,
       0,     0,     0,     0,    43,     0,  -254,   178,   173,     0,
    -254,     0,   179,     0,     0,     0,   172,   575,     0,     0,
     174,  -254,  -352,  -254,   175,   176,     0,   180,   102,  -254,
     181,     0,     0,   177,  -254,     0,   173,     0,  -254,    43,
       0,     0,   178,     0,     0,  -247,     0,   179,   174,  -254,
       0,   172,   175,   176,     0,     0,     0,  -254,  -254,     0,
       0,   177,   180,   102,     0,   181,    40,    43,     0,  -254,
     178,   173,     0,  -254,     0,   179,     0,     0,     0,     0,
       0,     0,     0,   174,  -254,     0,  -254,   175,   176,     0,
     180,   102,  -254,   181,     0,  -118,   177,     0,     0,    41,
       1,     2,    43,     0,     0,   178,     0,     0,     0,   100,
     179,     0,    40,     0,     0,     0,   143,    43,  -118,     4,
      40,  -254,     0,     0,     0,   180,   102,     0,   181,    44,
    -116,     0,     0,    45,    46,     0,     0,     0,  -117,     0,
      33,  -116,     0,     0,     0,    41,     1,     2,     0,  -117,
       0,     0,     0,    41,     1,     2,    40,     0,     0,     0,
       0,     0,    42,    43,    40,     4,     0,     0,     0,     0,
      42,    43,     0,     4,   405,    44,     0,     0,     0,    45,
      46,     0,  -116,    44,     0,  -119,    33,    45,    46,    41,
       1,     2,     0,  -116,    33,     0,     0,    41,     1,     2,
      40,     0,     0,     0,     0,     0,   143,    43,  -119,     4,
       0,     0,     0,     0,    42,    43,     0,     4,     0,    44,
       0,     0,    40,    45,    46,     0,     0,    44,     0,  -118,
      33,    45,    46,    41,     1,     2,     0,     0,    33,     0,
    -116,     0,     0,     0,     0,     0,     0,    40,     0,     0,
     143,    43,  -118,     4,     0,    41,     1,     2,     0,     0,
       0,     0,     0,    44,     0,     0,     0,    45,    46,     0,
       0,     0,    42,    43,    33,     4,  -118,     0,     0,     0,
      41,     1,     2,     0,     0,    44,     0,     0,     0,    45,
      46,     0,     0,     0,     0,     0,    33,   143,    43,     0,
       4,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      44,     0,     0,     0,    45,    46,     0,     0,     0,     0,
       0,    33
};

static const yytype_int16 yycheck[] =
{
       6,    10,   313,    10,   368,   348,   163,    30,    77,   369,
      17,   248,   217,   181,   171,   139,    22,   185,   469,   258,
     241,   332,   334,   234,     0,   236,     1,   154,   571,    94,
     530,   222,   392,    96,    22,     0,    37,   537,   466,     3,
       0,    17,    10,   333,    18,    46,    22,     9,    20,    56,
     287,     1,    17,     3,     4,    10,   293,    17,   293,    10,
     323,    10,    85,    60,    16,    10,    59,     1,    59,     1,
       4,    30,    79,    30,    56,    22,    75,    76,     0,    29,
     145,   441,    32,    80,   121,    32,    79,    94,    79,    75,
      78,    41,    42,    43,     1,    17,    48,     4,    80,   100,
      20,     9,    84,    75,    86,     1,   129,   370,   343,   469,
      84,    70,    86,    70,   241,     0,     4,    85,    94,   126,
     121,    73,    84,    79,    86,   194,    90,    91,    92,    81,
      85,    81,    82,   139,    85,   142,    85,   125,   145,   450,
      85,    75,   207,    75,   181,   645,   337,   170,   185,   212,
     578,   214,   373,   154,   393,    75,   131,   447,   164,   224,
     703,     1,   163,   113,    16,     1,   617,   168,    75,   145,
     171,    79,    80,   174,   175,   151,    84,   178,    86,    75,
     181,   131,   132,   133,   185,    21,   151,    75,    76,   400,
      80,   151,    30,   143,    16,    35,    48,    75,   235,   655,
     207,    52,    79,   209,    39,     0,   156,   571,    20,   159,
      46,    51,   162,     0,    54,    50,    52,   224,     1,    70,
     221,    73,    17,   679,    16,   349,    48,    79,    68,    69,
      17,   207,    72,    14,   235,    75,     0,    75,    21,    75,
     241,   368,   369,    44,    25,    26,   373,    30,   224,    15,
      72,    73,   379,    17,    16,   205,    48,   617,   618,    81,
      82,   418,   605,    75,   391,   392,   423,   217,   580,    52,
      75,    76,    12,   223,   281,   496,   313,   498,   301,    22,
      79,    73,    35,    82,    16,   453,    48,    70,    79,    81,
      82,    82,    75,    16,    75,   332,    16,   465,    51,   536,
      79,    54,   157,    17,    44,    45,    46,   162,    74,    75,
      76,    73,   313,   624,   441,    43,    48,    79,   173,   476,
     525,   176,   329,    79,   179,    48,    44,   532,    48,   689,
      58,   332,    75,    76,    74,    75,    76,    77,    16,   703,
      80,    73,   469,   349,    26,    63,    14,    79,     3,   692,
      73,     6,    14,    73,   665,    17,    79,    25,    26,    79,
      79,    75,    76,    25,    26,    93,    94,   368,   369,   496,
      48,   498,   373,   500,    56,   686,    79,   534,    81,    41,
      40,    43,    44,    75,   334,     3,   241,     5,    43,     7,
       8,   392,    19,   561,    56,    73,    40,    69,    80,    82,
      83,    79,    84,    58,    86,    75,   356,    75,    82,    83,
     416,    40,    80,   450,    75,    59,   453,   418,    80,   274,
      80,    39,   423,    16,    84,    80,    86,    45,   465,    84,
      59,    86,    40,    26,    79,    90,    91,    92,    93,    94,
     441,   296,    40,    79,   571,    81,    35,    52,    23,   450,
      79,    59,   453,    79,   463,    48,   463,   407,   459,    34,
      16,    59,    51,    56,   465,    54,   321,    42,   469,    87,
      88,    89,    90,    91,    92,   476,    14,   546,   333,    17,
      73,    79,    16,    80,   485,   486,   487,    25,    26,    16,
     617,   618,    48,    40,   662,   496,    71,   498,    79,   500,
      56,    81,    16,    41,    40,    43,    44,    56,     4,   636,
      69,    75,   367,   368,    48,    80,    12,    73,    56,   687,
      40,    48,    56,    59,   561,    84,    40,    86,   696,     1,
      49,    80,     4,   534,    48,    84,    79,    86,    80,    73,
      12,   491,    80,    79,    81,    82,    73,    30,    44,    45,
      46,    80,   407,   408,   555,   410,    75,    76,    79,    73,
     561,    60,   689,    49,   419,   420,    53,   422,    81,    82,
     571,    40,    44,    45,    46,   525,   703,    49,    74,    75,
      76,    77,   532,   652,    80,   654,    46,   624,   443,    75,
      76,    40,   447,    53,    90,    91,   451,    57,    42,    90,
      91,    92,    74,    75,    76,    77,   613,   613,    80,   678,
      81,    82,    56,     9,   615,    79,   617,   618,    90,    91,
      79,   690,   623,   624,   625,   662,   695,    80,   665,    83,
     580,    84,    79,    86,    79,   636,    80,    81,    82,    80,
      84,   710,    86,    84,    79,    86,    82,    83,    40,   686,
     687,    14,    28,    29,   655,    40,     1,    81,    82,   696,
      50,   662,    25,    26,   665,    79,    80,    12,    30,   524,
      84,    40,    86,    20,   681,   681,   683,     6,   679,     5,
       1,     7,     8,    80,   691,   686,   687,    84,   689,    86,
      18,    12,   699,    56,   701,   696,    74,    75,   648,    44,
      45,    46,   703,    10,    49,   712,   712,    81,    82,    81,
      82,    79,    75,    39,    81,    82,   571,    80,     9,    45,
      81,    35,    36,    44,    45,    46,    81,    82,    49,    74,
      75,    76,    77,    82,    83,    80,     1,    51,    52,    79,
      54,    79,   377,   378,   599,    90,    91,    12,    80,    63,
      59,    70,   607,    74,    75,    76,    77,    67,    72,    80,
      75,    87,    88,    89,    90,    91,    92,    56,    12,    90,
      91,    79,    80,    79,    18,    70,    84,    61,    86,    44,
      45,    46,    79,    81,    49,    60,    44,    12,   643,    40,
      30,    80,    81,    82,    40,    84,    57,    86,    67,    82,
      44,    45,    46,    81,    28,    12,    39,    43,    79,    74,
      75,    76,    77,    57,    14,    80,    79,    47,    30,    44,
      45,    46,    79,    79,    12,    90,    91,    79,    39,    42,
      74,    75,    76,    77,    79,    30,    80,    44,    45,    46,
       4,     4,    49,    35,    36,     4,    90,    91,   703,    74,
      75,    76,    77,     4,    79,    80,    44,    45,    46,    51,
       4,    79,    54,    53,    79,    90,    91,    74,    75,    76,
      77,    63,    63,    80,    57,    81,    80,     1,    38,    21,
      72,    83,    81,    90,    91,    42,    74,    75,    76,    77,
      26,    72,    80,    79,    47,    19,    57,    21,     4,    23,
      79,    57,    90,    91,    28,    29,    30,    79,    79,    33,
      34,    79,     1,    37,    38,    53,    81,    79,    42,    30,
      40,    79,    46,    57,    79,     4,    21,    81,    52,    79,
      19,    55,    21,    79,    23,   611,    60,   407,    79,    79,
       1,    30,   615,    32,    33,    34,   681,    71,    37,    38,
     130,    75,    76,    42,    78,   712,   681,    46,    19,   416,
      21,   368,    23,    52,   343,   704,    55,   156,    56,    30,
     224,    60,    33,    34,   156,     1,    37,    38,   500,   241,
     158,    42,    71,   399,   451,    46,    75,    76,    22,    78,
     491,    52,   343,    19,    55,    21,    17,    23,    76,    60,
     343,   178,    17,   328,    30,   625,   623,    33,    34,    70,
      71,    37,    38,    94,    75,    76,    42,    78,   588,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    52,    -1,    44,    55,
      46,    -1,    -1,     1,    60,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,    75,
      76,    19,    78,    21,    -1,    23,    -1,    -1,    74,    75,
      76,    77,    30,    -1,    80,    33,    34,    -1,     1,    37,
      38,    -1,    -1,    -1,    42,    -1,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    -1,    52,    -1,    19,    55,    21,    -1,
      23,    -1,    60,    -1,    -1,    -1,     1,    30,    -1,    -1,
      33,    34,    70,    71,    37,    38,    -1,    75,    76,    42,
      78,    -1,    -1,    46,    19,    -1,    21,    -1,    23,    52,
      -1,    -1,    55,    -1,    -1,    30,    -1,    60,    33,    34,
      -1,     1,    37,    38,    -1,    -1,    -1,    42,    71,    -1,
      -1,    46,    75,    76,    -1,    78,     1,    52,    -1,    19,
      55,    21,    -1,    23,    -1,    60,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    33,    34,    -1,    71,    37,    38,    -1,
      75,    76,    42,    78,    -1,    30,    46,    -1,    -1,    34,
      35,    36,    52,    -1,    -1,    55,    -1,    -1,    -1,    44,
      60,    -1,     1,    -1,    -1,    -1,    51,    52,    53,    54,
       1,    71,    -1,    -1,    -1,    75,    76,    -1,    78,    64,
      19,    -1,    -1,    68,    69,    -1,    -1,    -1,    19,    -1,
      75,    30,    -1,    -1,    -1,    34,    35,    36,    -1,    30,
      -1,    -1,    -1,    34,    35,    36,     1,    -1,    -1,    -1,
      -1,    -1,    51,    52,     1,    54,    -1,    -1,    -1,    -1,
      51,    52,    -1,    54,    63,    64,    -1,    -1,    -1,    68,
      69,    -1,    19,    64,    -1,    30,    75,    68,    69,    34,
      35,    36,    -1,    30,    75,    -1,    -1,    34,    35,    36,
       1,    -1,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
      -1,    -1,    -1,    -1,    51,    52,    -1,    54,    -1,    64,
      -1,    -1,     1,    68,    69,    -1,    -1,    64,    -1,    30,
      75,    68,    69,    34,    35,    36,    -1,    -1,    75,    -1,
      19,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      51,    52,    53,    54,    -1,    34,    35,    36,    -1,    -1,
      -1,    -1,    -1,    64,    -1,    -1,    -1,    68,    69,    -1,
      -1,    -1,    51,    52,    75,    54,    30,    -1,    -1,    -1,
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
       0,    35,    36,    51,    54,    63,    72,    96,   226,   227,
     236,   237,   239,   240,   243,   250,   252,   253,   254,   256,
     257,   266,   267,   273,   274,    75,    76,   162,   230,    20,
     162,   162,    80,    75,   104,     0,    40,    59,    79,   251,
       1,    34,    51,    52,    64,    68,    69,    97,   101,   102,
     103,   104,   108,   112,   153,   156,   157,   158,   159,   226,
     227,   237,   239,   243,   247,   249,   250,   259,   260,   266,
     276,   277,   278,   281,    79,    97,   254,   256,   255,   100,
       1,    51,    68,    72,   104,   227,   234,   240,   247,   268,
      79,    79,   229,   162,    40,   251,   228,   162,    79,    82,
      44,   275,    76,   160,   162,   164,   165,   168,   169,    79,
      79,   161,   162,    20,    75,   162,    75,    75,   160,   248,
      83,    19,   218,   158,    40,   255,   100,   247,    97,   162,
     162,    35,    51,    54,    83,    79,   251,    79,    79,    80,
     231,   232,    40,    51,   154,   155,   156,   227,   275,    79,
     231,    81,    75,   160,    80,    84,    86,    69,    84,    86,
     162,    79,    80,    40,    80,   109,   142,    79,    82,    22,
      32,   105,     1,    21,    33,    37,    38,    46,    55,    60,
      75,    78,    97,   160,   189,   191,   192,   193,   194,   195,
     196,   197,   198,   204,   205,   208,   209,   215,   219,   221,
     224,   225,   238,   265,   282,    30,    63,    40,   251,    80,
     142,   269,   230,   162,   162,    39,    50,   235,    79,   233,
     234,    60,   153,    20,    53,   241,   156,    40,    51,   227,
     237,   243,   258,     1,    12,    44,    45,    46,    49,    74,
      77,    80,    90,    91,   119,   149,   150,   151,   160,   163,
     166,   167,   171,   172,   174,   175,   178,   181,   182,   184,
     186,   187,   188,   189,   190,    14,    25,    26,    75,   170,
      15,   162,   163,   164,    18,    57,   160,   175,   181,   170,
     162,    40,    98,    99,   162,   175,   113,   160,     1,     4,
     104,   143,   144,    40,   110,   160,     9,    79,   251,    17,
     106,   113,   127,   128,   129,    79,   175,   160,   222,   160,
     175,   199,   200,   201,   202,    79,   222,    79,   175,    83,
     192,     9,    79,    84,    79,    32,   192,   220,   261,   100,
      23,    34,    42,    71,   210,   211,   213,   216,   214,   230,
      79,    79,     4,    40,   231,    40,   231,    50,   161,    79,
      81,   160,   218,   244,   162,   154,    30,    40,   187,   160,
     189,   187,    46,   167,   173,   174,   175,    10,    85,    56,
      84,   117,    81,    82,    16,    48,    73,   176,   177,     3,
       5,     7,     8,    39,    45,    87,    88,    89,    90,    91,
      92,   179,   180,   183,   184,    43,    58,    93,    94,   185,
       6,   175,    18,   279,    79,    63,    81,    82,    10,    79,
      26,   114,   115,   117,    81,    83,    79,    81,    14,    25,
      26,    41,    43,    44,    80,   111,   116,   117,   120,   123,
     124,   125,   126,   127,   136,   152,   245,   246,    79,   175,
      79,    80,   133,     9,   107,   251,    40,    70,   223,    79,
      28,    29,   203,   191,    67,    79,    79,   175,   188,    70,
     262,   262,   206,   217,    75,   191,   202,   214,    61,   212,
     218,    79,    81,    25,    26,    43,    44,    56,    80,   127,
     152,   245,   246,   271,   272,    60,    44,    40,   270,   107,
     234,    30,    40,   162,   242,    57,    82,    81,    82,    81,
      72,    81,   175,   150,   151,   175,   119,   160,   181,    56,
     167,    67,    28,   178,   178,   181,    39,   181,   119,   160,
     184,   186,   187,    79,    43,   280,    79,    79,    99,   175,
     175,    14,   145,   144,    22,   113,   175,   175,   175,   113,
      74,    75,   121,   122,    46,    53,    57,   137,    79,   119,
     131,   132,   134,   135,   160,    47,   175,    79,    79,   202,
      79,   191,   200,    30,    79,    49,    75,   160,   263,   264,
      30,    70,   207,   153,    39,    30,   213,    79,   135,   160,
      30,     4,     4,     4,   113,     4,     4,    79,   160,   160,
       4,   160,    79,   242,    81,   167,   167,    46,   166,    80,
     175,    30,   161,   117,   118,   161,   113,    26,   117,   118,
      81,    82,    57,   100,    81,    82,    81,    82,    56,   118,
     113,   130,    38,    83,    10,    85,    21,   149,    42,   213,
     214,    26,    72,    81,   270,    79,    80,    79,    57,    81,
     175,    79,    57,    18,   107,   175,   122,     1,    21,    46,
     104,   138,   139,   141,   146,    47,   132,   160,   135,     4,
      79,   263,   191,   264,    79,    10,    79,     4,    53,    79,
       4,    81,    81,    79,   175,   118,    79,   162,    79,    83,
      30,   100,   140,   100,   130,    56,    10,   191,    81,    56,
      40,   100,   130,    57,   141,   146,   191,    79,   119,   100,
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
     162,   163,   164,   165,   166,   166,   167,   167,   167,   167,
     168,   168,   168,   168,   169,   170,   170,   170,   170,   171,
     171,   171,   172,   172,   172,   172,   172,   173,   173,   174,
     175,   175,   175,   176,   176,   176,   177,   177,   178,   178,
     178,   178,   179,   179,   179,   179,   179,   179,   180,   180,
     181,   181,   181,   182,   182,   183,   183,   183,   184,   184,
     185,   185,   185,   185,   186,   186,   186,   186,   187,   187,
     187,   187,   187,   188,   188,   189,   190,   190,   191,   191,
     192,   192,   193,   193,   193,   194,   194,   194,   194,   194,
     194,   194,   194,   194,   195,   195,   195,   195,   196,   197,
     198,   199,   199,   200,   201,   202,   203,   203,   204,   205,
     206,   206,   207,   208,   209,   209,   210,   210,   210,   211,
     212,   212,   213,   214,   214,   215,   216,   217,   216,   218,
     219,   220,   220,   221,   222,   222,   223,   223,   224,   224,
     225,   226,   226,   228,   227,   229,   227,   227,   230,   230,
     231,   231,   232,   233,   233,   234,   234,   235,   235,   235,
     235,   236,   237,   238,   239,   239,   240,   241,   241,   242,
     242,   243,   244,   244,   245,   246,   246,   247,   248,   248,
     249,   249,   249,   250,   250,   250,   250,   251,   252,   252,
     253,   253,   253,   254,   255,   255,   256,   256,   256,   256,
     256,   256,   256,   257,   258,   258,   259,   259,   260,   261,
     261,   262,   262,   263,   263,   264,   264,   265,   266,   266,
     267,   267,   268,   268,   268,   268,   268,   268,   268,   269,
     269,   269,   270,   270,   270,   271,   271,   271,   271,   271,
     271,   271,   271,   271,   271,   272,   272,   273,   274,   275,
     276,   276,   276,   277,   278,   279,   279,   280,   280,   281,
     282
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
       1,     1,     1,     4,     1,     3,     1,     1,     1,     1,
       3,     3,     3,     3,     3,     1,     1,     1,     1,     1,
       1,     1,     3,     3,     5,     6,     4,     3,     3,     3,
       1,     3,     3,     1,     1,     1,     2,     2,     1,     3,
       3,     3,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     2,     3,     1,     1,     1,     1,     1,     1,     3,
       1,     1,     1,     1,     1,     2,     2,     3,     1,     1,
       1,     1,     1,     1,     3,     3,     2,     2,     1,     2,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     1,     2,     4,
       6,     1,     3,     2,     2,     1,     0,     2,     6,     3,
       0,     2,     4,     4,     0,     2,     1,     3,     4,     3,
       0,     1,     4,     0,     1,     6,     0,     0,     3,     2,
       2,     0,     1,     4,     0,     1,     0,     2,     2,     3,
       3,     2,     2,     0,     4,     0,     6,     2,     1,     1,
       0,     1,     3,     1,     3,     5,     1,     0,     1,     1,
       2,     2,     6,     2,     2,     2,     7,     0,     2,     0,
       1,     9,     0,     1,     2,     0,     1,     3,     1,     3,
       6,     5,     1,     4,     3,     5,     4,     2,     3,     2,
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
#line 240 "grammar83.y"
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

#line 2508 "grammar83.tab.c"

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
#line 254 "grammar83.y"
                        { context->comp_unit = (yyvsp[0].comp_unit); }
#line 2728 "grammar83.tab.c"
    break;

  case 13: /* decl: type_decl  */
#line 280 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), (yyvsp[0].decl));
    }
#line 2737 "grammar83.tab.c"
    break;

  case 14: /* decl: subtype_decl  */
#line 284 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), (yyvsp[0].decl));
    }
#line 2746 "grammar83.tab.c"
    break;

  case 15: /* decl: subprog_decl  */
#line 288 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), &(yyvsp[0].subprogram_decl)->base);
    }
#line 2755 "grammar83.tab.c"
    break;

  case 16: /* decl: pkg_decl  */
#line 292 "grammar83.y"
                 {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), &(yyvsp[0].pkg_spec)->base);
    }
#line 2764 "grammar83.tab.c"
    break;

  case 22: /* object_decl: def_id_s ':' object_qualifier_opt object_subtype_def init_opt ';'  */
#line 304 "grammar83.y"
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
#line 2793 "grammar83.tab.c"
    break;

  case 23: /* number_decl: def_id_s ':' CONSTANT IS_ASSIGNED expression ';'  */
#line 330 "grammar83.y"
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
#line 2811 "grammar83.tab.c"
    break;

  case 24: /* def_id_s: identifier  */
#line 345 "grammar83.y"
               {
        StringTokenArray_init(&(yyval.str_token_array));
        StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token));
    }
#line 2820 "grammar83.tab.c"
    break;

  case 25: /* def_id_s: def_id_s ',' identifier  */
#line 349 "grammar83.y"
                            { StringTokenArray_append(&(yyval.str_token_array), (yyvsp[0].str_token)); }
#line 2826 "grammar83.tab.c"
    break;

  case 26: /* object_qualifier_opt: %empty  */
#line 354 "grammar83.y"
             { (yyval.bool_) = false; }
#line 2832 "grammar83.tab.c"
    break;

  case 27: /* object_qualifier_opt: CONSTANT  */
#line 355 "grammar83.y"
             { (yyval.bool_) = true; }
#line 2838 "grammar83.tab.c"
    break;

  case 30: /* init_opt: %empty  */
#line 364 "grammar83.y"
                           { (yyval.expr) = NULL; }
#line 2844 "grammar83.tab.c"
    break;

  case 31: /* init_opt: IS_ASSIGNED expression  */
#line 365 "grammar83.y"
                           { (yyval.expr) = (yyvsp[0].expr); }
#line 2850 "grammar83.tab.c"
    break;

  case 32: /* type_decl: TYPE identifier discrim_part_opt type_completion ';'  */
#line 369 "grammar83.y"
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
#line 2865 "grammar83.tab.c"
    break;

  case 37: /* type_completion: IS type_def  */
#line 389 "grammar83.y"
                { (yyval.type_decl) = (yyvsp[0].type_decl); }
#line 2871 "grammar83.tab.c"
    break;

  case 46: /* subtype_decl: SUBTYPE identifier IS subtype_ind ';'  */
#line 404 "grammar83.y"
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
#line 2890 "grammar83.tab.c"
    break;

  case 47: /* subtype_ind: name constraint  */
#line 421 "grammar83.y"
                    {
        // TODO: propagate constraint somehow
        (yyval.str_token) = (yyvsp[-1].name).name;
    }
#line 2899 "grammar83.tab.c"
    break;

  case 48: /* subtype_ind: name  */
#line 425 "grammar83.y"
         { (yyval.str_token) = (yyvsp[0].name).name; }
#line 2905 "grammar83.tab.c"
    break;

  case 52: /* derived_type: NEW subtype_ind  */
#line 438 "grammar83.y"
                    {
        (yyval.type_decl) = create_type_decl(TYPE_DERIVED);
        TypeDecl* base_type = find_type_decl(context, (yyvsp[0].str_token));
        if(!base_type) {
            error_print((yyloc), "Unknown base type: %s", ST((yyvsp[0].str_token)));
            error_exit();
        }
        (yyval.type_decl)->u.subtype.base = base_type;
    }
#line 2919 "grammar83.tab.c"
    break;

  case 53: /* range_constraint: RANGE range  */
#line 449 "grammar83.y"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 2925 "grammar83.tab.c"
    break;

  case 54: /* range_constr_opt: %empty  */
#line 453 "grammar83.y"
           { (yyval.expr) = NULL; }
#line 2931 "grammar83.tab.c"
    break;

  case 56: /* range: simple_expression DOT_DOT simple_expression  */
#line 458 "grammar83.y"
                                                { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_RANGE, (yyvsp[0].expr)); }
#line 2937 "grammar83.tab.c"
    break;

  case 59: /* enumeration_type: '(' enum_id_s ')'  */
#line 464 "grammar83.y"
                      {
        (yyval.type_decl) = create_type_decl(TYPE_ENUM);
        (yyval.type_decl)->u.enum_.literals = (yyvsp[-1].expr_array).data;
        (yyval.type_decl)->u.enum_.literal_count = ExprPtrArray_size(&(yyvsp[-1].expr_array));
        // TODO: add all enum literals into symbol table scope
    }
#line 2948 "grammar83.tab.c"
    break;

  case 60: /* enum_id_s: enum_id  */
#line 472 "grammar83.y"
            {
        ExprPtrArray_init(&(yyval.expr_array));
        ExprPtrArray_append(&(yyval.expr_array), (yyvsp[0].expr));
    }
#line 2957 "grammar83.tab.c"
    break;

  case 61: /* enum_id_s: enum_id_s ',' enum_id  */
#line 476 "grammar83.y"
                          {
        (yyval.expr_array) = (yyvsp[-2].expr_array);
        ExprPtrArray_append(&(yyval.expr_array), (yyvsp[0].expr));
    }
#line 2966 "grammar83.tab.c"
    break;

  case 62: /* enum_id: identifier  */
#line 482 "grammar83.y"
               {
        (yyval.expr) = create_expr(EXPR_NAME, (yyloc));
        (yyval.expr)->u.name.name = (yyvsp[0].str_token);
    }
#line 2975 "grammar83.tab.c"
    break;

  case 63: /* enum_id: char_lit  */
#line 486 "grammar83.y"
             {
        (yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
        (yyval.expr)->u.char_lit = (yyvsp[0].c);
    }
#line 2984 "grammar83.tab.c"
    break;

  case 64: /* integer_type: range_constraint  */
#line 492 "grammar83.y"
                     {
        (yyval.type_decl) = create_type_decl(TYPE_INTEGER);
        (yyval.type_decl)->u.int_.range = (yyvsp[0].expr);
    }
#line 2993 "grammar83.tab.c"
    break;

  case 107: /* choice_s: choice  */
#line 616 "grammar83.y"
                        {
        ChoiceArray_init(&(yyval.choice_array));
        ChoiceArray_append(&(yyval.choice_array), (yyvsp[0].choice));
    }
#line 3002 "grammar83.tab.c"
    break;

  case 108: /* choice_s: choice_s '|' choice  */
#line 620 "grammar83.y"
                        {
        (yyval.choice_array) = (yyvsp[-2].choice_array);
        ChoiceArray_append(&(yyval.choice_array), (yyvsp[0].choice));
    }
#line 3011 "grammar83.tab.c"
    break;

  case 109: /* choice: expression  */
#line 626 "grammar83.y"
                         {
        (yyval.choice).kind = CHOICE_EXPR;
        (yyval.choice).u.expr = (yyvsp[0].expr);
    }
#line 3020 "grammar83.tab.c"
    break;

  case 111: /* choice: OTHERS  */
#line 631 "grammar83.y"
                         { (yyval.choice).kind = CHOICE_OTHERS; }
#line 3026 "grammar83.tab.c"
    break;

  case 116: /* decl_part: %empty  */
#line 645 "grammar83.y"
                         { (yyval.decl) = NULL; }
#line 3032 "grammar83.tab.c"
    break;

  case 117: /* decl_part: decl_item_or_body_s1  */
#line 646 "grammar83.y"
                         { (yyval.decl) = (yyvsp[0].decl_list).first; }
#line 3038 "grammar83.tab.c"
    break;

  case 118: /* decl_item_s: %empty  */
#line 650 "grammar83.y"
                 { (yyval.decl) = NULL; }
#line 3044 "grammar83.tab.c"
    break;

  case 119: /* decl_item_s: decl_item_s1  */
#line 651 "grammar83.y"
                 { (yyval.decl) = (yyvsp[0].decl_list).first; }
#line 3050 "grammar83.tab.c"
    break;

  case 121: /* decl_item_s1: decl_item_s1 decl_item  */
#line 656 "grammar83.y"
                           {
        DeclList_splice(&(yyvsp[-1].decl_list), &(yyvsp[0].decl_list));
        (yyval.decl_list) = (yyvsp[-1].decl_list);
    }
#line 3059 "grammar83.tab.c"
    break;

  case 127: /* decl_item_or_body_s1: decl_item_or_body_s1 decl_item_or_body  */
#line 670 "grammar83.y"
                                           {
        DeclList_splice(&(yyvsp[-1].decl_list), &(yyvsp[0].decl_list));
        (yyval.decl_list) = (yyvsp[-1].decl_list);
    }
#line 3068 "grammar83.tab.c"
    break;

  case 128: /* decl_item_or_body: body  */
#line 676 "grammar83.y"
              {
        memset(&(yyval.decl_list), 0, sizeof((yyval.decl_list)));
        DeclList_append(&(yyval.decl_list), (yyvsp[0].decl));
    }
#line 3077 "grammar83.tab.c"
    break;

  case 130: /* body: subprog_body  */
#line 684 "grammar83.y"
                 { (yyval.decl) = &(yyvsp[0].subprogram_decl)->base; }
#line 3083 "grammar83.tab.c"
    break;

  case 131: /* body: pkg_body  */
#line 685 "grammar83.y"
                 { (yyval.decl) = &(yyvsp[0].pkg_body)->base; }
#line 3089 "grammar83.tab.c"
    break;

  case 132: /* name: simple_name  */
#line 689 "grammar83.y"
                {
        memset(&(yyval.name), 0, sizeof((yyval.name)));
        (yyval.name).name = (yyvsp[0].str_token);
    }
#line 3098 "grammar83.tab.c"
    break;

  case 136: /* name: operator_symbol  */
#line 696 "grammar83.y"
                    {
        memset(&(yyval.name), 0, sizeof((yyval.name)));
        (yyval.name).name = (yyvsp[0].str_token);
        //TODO: lookup operator, determine its arity, and allocate args array
    }
#line 3108 "grammar83.tab.c"
    break;

  case 141: /* used_char: char_lit  */
#line 713 "grammar83.y"
             {
        (yyval.expr) = create_expr(EXPR_CHAR_LIT, (yyloc));
        (yyval.expr)->u.char_lit = (yyvsp[0].c);
    }
#line 3117 "grammar83.tab.c"
    break;

  case 159: /* literal: numeric_lit  */
#line 757 "grammar83.y"
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
#line 3141 "grammar83.tab.c"
    break;

  case 171: /* expression: expression logical relation  */
#line 799 "grammar83.y"
                                      { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3147 "grammar83.tab.c"
    break;

  case 172: /* expression: expression short_circuit relation  */
#line 800 "grammar83.y"
                                      { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3153 "grammar83.tab.c"
    break;

  case 173: /* logical: AND  */
#line 804 "grammar83.y"
        { (yyval.unary_op) = OP_AND; }
#line 3159 "grammar83.tab.c"
    break;

  case 174: /* logical: OR  */
#line 805 "grammar83.y"
        { (yyval.unary_op) = OP_OR; }
#line 3165 "grammar83.tab.c"
    break;

  case 175: /* logical: XOR  */
#line 806 "grammar83.y"
        { (yyval.unary_op) = OP_XOR; }
#line 3171 "grammar83.tab.c"
    break;

  case 176: /* short_circuit: AND THEN  */
#line 810 "grammar83.y"
               { (yyval.unary_op) = OP_AND_THEN; }
#line 3177 "grammar83.tab.c"
    break;

  case 177: /* short_circuit: OR ELSE  */
#line 811 "grammar83.y"
               { (yyval.unary_op) = OP_OR_ELSE; }
#line 3183 "grammar83.tab.c"
    break;

  case 179: /* relation: simple_expression relational simple_expression  */
#line 817 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3189 "grammar83.tab.c"
    break;

  case 180: /* relation: simple_expression membership range  */
#line 818 "grammar83.y"
                                                   { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3195 "grammar83.tab.c"
    break;

  case 181: /* relation: simple_expression membership name  */
#line 819 "grammar83.y"
                                                   {
        Expression* right = create_expr(EXPR_NAME, (yylsp[0]));
        right->u.name = (yyvsp[0].name);
        (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), right);
    }
#line 3205 "grammar83.tab.c"
    break;

  case 182: /* relational: '='  */
#line 826 "grammar83.y"
          { (yyval.unary_op) = OP_EQ; }
#line 3211 "grammar83.tab.c"
    break;

  case 183: /* relational: NE  */
#line 827 "grammar83.y"
          { (yyval.unary_op) = OP_NEQ; }
#line 3217 "grammar83.tab.c"
    break;

  case 184: /* relational: '<'  */
#line 828 "grammar83.y"
          { (yyval.unary_op) = OP_LT; }
#line 3223 "grammar83.tab.c"
    break;

  case 185: /* relational: LT_EQ  */
#line 829 "grammar83.y"
          { (yyval.unary_op) = OP_LTE; }
#line 3229 "grammar83.tab.c"
    break;

  case 186: /* relational: '>'  */
#line 830 "grammar83.y"
          { (yyval.unary_op) = OP_GT; }
#line 3235 "grammar83.tab.c"
    break;

  case 187: /* relational: GE  */
#line 831 "grammar83.y"
          { (yyval.unary_op) = OP_GTE; }
#line 3241 "grammar83.tab.c"
    break;

  case 188: /* membership: IN  */
#line 835 "grammar83.y"
           { (yyval.unary_op) = OP_IN; }
#line 3247 "grammar83.tab.c"
    break;

  case 189: /* membership: NOT IN  */
#line 836 "grammar83.y"
           { (yyval.unary_op) = OP_NOT_IN; }
#line 3253 "grammar83.tab.c"
    break;

  case 191: /* simple_expression: unary term  */
#line 841 "grammar83.y"
                                  { (yyval.expr) = make_unary_expr((yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3259 "grammar83.tab.c"
    break;

  case 192: /* simple_expression: simple_expression adding term  */
#line 842 "grammar83.y"
                                  { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3265 "grammar83.tab.c"
    break;

  case 193: /* unary: '+'  */
#line 846 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_PLUS; }
#line 3271 "grammar83.tab.c"
    break;

  case 194: /* unary: '-'  */
#line 847 "grammar83.y"
        { (yyval.unary_op) = OP_UNARY_MINUS; }
#line 3277 "grammar83.tab.c"
    break;

  case 195: /* adding: '+'  */
#line 851 "grammar83.y"
        { (yyval.unary_op) = OP_PLUS; }
#line 3283 "grammar83.tab.c"
    break;

  case 196: /* adding: '-'  */
#line 852 "grammar83.y"
        { (yyval.unary_op) = OP_MINUS; }
#line 3289 "grammar83.tab.c"
    break;

  case 197: /* adding: '&'  */
#line 853 "grammar83.y"
        { (yyval.unary_op) = OP_AMP; }
#line 3295 "grammar83.tab.c"
    break;

  case 199: /* term: term multiplying factor  */
#line 858 "grammar83.y"
                            { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), (yyvsp[-1].unary_op), (yyvsp[0].expr)); }
#line 3301 "grammar83.tab.c"
    break;

  case 200: /* multiplying: '*'  */
#line 862 "grammar83.y"
        { (yyval.unary_op) = OP_MULT; }
#line 3307 "grammar83.tab.c"
    break;

  case 201: /* multiplying: '/'  */
#line 863 "grammar83.y"
        { (yyval.unary_op) = OP_DIVIDE; }
#line 3313 "grammar83.tab.c"
    break;

  case 202: /* multiplying: MOD  */
#line 864 "grammar83.y"
        { (yyval.unary_op) = OP_MOD; }
#line 3319 "grammar83.tab.c"
    break;

  case 203: /* multiplying: REM  */
#line 865 "grammar83.y"
        { (yyval.unary_op) = OP_REM; }
#line 3325 "grammar83.tab.c"
    break;

  case 205: /* factor: NOT primary  */
#line 870 "grammar83.y"
                          { (yyval.expr) = make_unary_expr(OP_NOT, (yyvsp[0].expr)); }
#line 3331 "grammar83.tab.c"
    break;

  case 206: /* factor: ABS primary  */
#line 871 "grammar83.y"
                          { (yyval.expr) = make_unary_expr(OP_ABS, (yyvsp[0].expr)); }
#line 3337 "grammar83.tab.c"
    break;

  case 207: /* factor: primary EXPON primary  */
#line 872 "grammar83.y"
                          { (yyval.expr) = make_binary_expr((yyvsp[-2].expr), OP_EXP, (yyvsp[0].expr)); }
#line 3343 "grammar83.tab.c"
    break;

  case 209: /* primary: name  */
#line 877 "grammar83.y"
         {
        (yyval.expr) = create_expr(EXPR_NAME, (yyloc));
        (yyval.expr)->u.name = (yyvsp[0].name);
    }
#line 3352 "grammar83.tab.c"
    break;

  case 214: /* parenthesized_primary: '(' expression ')'  */
#line 888 "grammar83.y"
                       { (yyval.expr) = (yyvsp[-1].expr); }
#line 3358 "grammar83.tab.c"
    break;

  case 218: /* statement_s: statement  */
#line 901 "grammar83.y"
                          {
        memset(&(yyval.stmt_list), 0, sizeof((yyval.stmt_list)));
        StmtList_append(&(yyval.stmt_list), (yyvsp[0].stmt));
    }
#line 3367 "grammar83.tab.c"
    break;

  case 219: /* statement_s: statement_s statement  */
#line 905 "grammar83.y"
                          {
        StmtList_append(&(yyvsp[-1].stmt_list), (yyvsp[0].stmt));
        (yyval.stmt_list) = (yyvsp[-1].stmt_list);
    }
#line 3376 "grammar83.tab.c"
    break;

  case 221: /* statement: goto_label statement  */
#line 912 "grammar83.y"
                         {
        check_for_redefinition(context, (yyvsp[-1].str_token), (yylsp[-1]));
        LabelDecl* label = create_label((yyvsp[-1].str_token), (yylsp[-1]));
        push_declaration(context, (Declaration*)label);
        (yyval.stmt) = (yyvsp[0].stmt);
    }
#line 3387 "grammar83.tab.c"
    break;

  case 238: /* null_stmt: NuLL ';'  */
#line 945 "grammar83.y"
             { (yyval.stmt) = create_stmt(STMT_NULL, (yyloc)); }
#line 3393 "grammar83.tab.c"
    break;

  case 239: /* assign_stmt: name IS_ASSIGNED expression ';'  */
#line 949 "grammar83.y"
                                    {
        (yyval.stmt) = create_stmt(STMT_ASSIGN, (yyloc));
        (yyval.stmt)->u.assign.dest.kind = EXPR_NAME;
        (yyval.stmt)->u.assign.dest.line_num = (yyloc);
        (yyval.stmt)->u.assign.dest.u.name = (yyvsp[-3].name);
        (yyval.stmt)->u.assign.expr = (yyvsp[-1].expr);
    }
#line 3405 "grammar83.tab.c"
    break;

  case 240: /* if_stmt: IF cond_clause_s else_opt END IF ';'  */
#line 958 "grammar83.y"
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

  case 242: /* cond_clause_s: cond_clause_s ELSIF cond_clause  */
#line 970 "grammar83.y"
                                    {
        (yyval.stmt) = (yyvsp[-2].stmt);
        (yyval.stmt)->u.if_.else_ = (yyvsp[0].stmt);
    }
#line 3428 "grammar83.tab.c"
    break;

  case 243: /* cond_clause: cond_part statement_s  */
#line 976 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_IF, (yyloc));
        (yyval.stmt)->u.if_.condition = (yyvsp[-1].expr);
        (yyval.stmt)->u.if_.stmts = (yyvsp[0].stmt_list).first;
    }
#line 3438 "grammar83.tab.c"
    break;

  case 244: /* cond_part: condition THEN  */
#line 983 "grammar83.y"
                   { (yyval.expr) = (yyvsp[-1].expr); }
#line 3444 "grammar83.tab.c"
    break;

  case 246: /* else_opt: %empty  */
#line 991 "grammar83.y"
                     { (yyval.stmt) = NULL; }
#line 3450 "grammar83.tab.c"
    break;

  case 247: /* else_opt: ELSE statement_s  */
#line 992 "grammar83.y"
                     { (yyval.stmt) = (yyvsp[0].stmt_list).first; }
#line 3456 "grammar83.tab.c"
    break;

  case 248: /* case_stmt: case_hdr pragma_s alternative_s END CASE ';'  */
#line 996 "grammar83.y"
                                                 {
        (yyval.stmt) = (yyvsp[-5].stmt);
        // TODO: pragmas
        (yyval.stmt)->u.case_.cases = (yyvsp[-3].case_list).first;
    }
#line 3466 "grammar83.tab.c"
    break;

  case 249: /* case_hdr: CASE expression IS  */
#line 1003 "grammar83.y"
                       {
        (yyval.stmt) = create_stmt(STMT_CASE, (yyloc));
        (yyval.stmt)->u.case_.expr = (yyvsp[-1].expr);
    }
#line 3475 "grammar83.tab.c"
    break;

  case 250: /* alternative_s: %empty  */
#line 1009 "grammar83.y"
                              { memset(&(yyval.case_list), 0, sizeof((yyval.case_list))); }
#line 3481 "grammar83.tab.c"
    break;

  case 251: /* alternative_s: alternative_s alternative  */
#line 1010 "grammar83.y"
                              {
        (yyval.case_list) = (yyvsp[-1].case_list);
        AltList_append(&(yyval.case_list), (yyvsp[0].case_));
    }
#line 3490 "grammar83.tab.c"
    break;

  case 252: /* alternative: WHEN choice_s RIGHT_SHAFT statement_s  */
#line 1016 "grammar83.y"
                                          {
        (yyval.case_) = calloc(1, sizeof(Alternative));
        (yyval.case_)->choices.choices = (yyvsp[-2].choice_array).data;
        (yyval.case_)->choices.count = ChoiceArray_size(&(yyvsp[-2].choice_array));
        (yyval.case_)->stmts = (yyvsp[0].stmt_list).first;
    }
#line 3501 "grammar83.tab.c"
    break;

  case 253: /* loop_stmt: label_opt loop_content id_opt ';'  */
#line 1025 "grammar83.y"
                                      { (yyval.stmt) = (yyvsp[-2].stmt); }
#line 3507 "grammar83.tab.c"
    break;

  case 256: /* loop_content: basic_loop  */
#line 1034 "grammar83.y"
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
#line 3522 "grammar83.tab.c"
    break;

  case 257: /* loop_content: WHILE condition basic_loop  */
#line 1044 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_WHILE;
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
        (yyval.stmt)->u.loop.u.while_.condition = (yyvsp[-1].expr);
    }
#line 3533 "grammar83.tab.c"
    break;

  case 258: /* loop_content: iter_part reverse_opt discrete_range basic_loop  */
#line 1050 "grammar83.y"
                                                    {
        (yyval.stmt) = create_stmt(STMT_LOOP, (yyloc));
        (yyval.stmt)->u.loop.kind = LOOP_FOR;
        (yyval.stmt)->u.loop.reverse = (yyvsp[-2].bool_);
        (yyval.stmt)->u.loop.u.for_.var = (yyvsp[-3].object_decl);
        (yyval.stmt)->u.loop.u.for_.range = (yyvsp[-1].expr);
        (yyval.stmt)->u.loop.stmts = (yyvsp[0].stmt);
    }
#line 3546 "grammar83.tab.c"
    break;

  case 259: /* iter_part: FOR identifier IN  */
#line 1060 "grammar83.y"
                      {
        memset(&(yyval.object_decl), 0, sizeof((yyval.object_decl)));
        (yyval.object_decl).base.kind = DECL_OBJECT;
        (yyval.object_decl).base.line_num = (yyloc);
        (yyval.object_decl).name = (yyvsp[-1].str_token);
    }
#line 3557 "grammar83.tab.c"
    break;

  case 260: /* reverse_opt: %empty  */
#line 1068 "grammar83.y"
            { (yyval.bool_) = false; }
#line 3563 "grammar83.tab.c"
    break;

  case 261: /* reverse_opt: REVERSE  */
#line 1069 "grammar83.y"
            { (yyval.bool_) = true; }
#line 3569 "grammar83.tab.c"
    break;

  case 262: /* basic_loop: LOOP statement_s END LOOP  */
#line 1073 "grammar83.y"
                              { (yyval.stmt) = (yyvsp[-2].stmt_list).first; }
#line 3575 "grammar83.tab.c"
    break;

  case 265: /* block: label_opt block_decl block_body END id_opt ';'  */
#line 1083 "grammar83.y"
                                                   {
        (yyval.stmt) = create_stmt(STMT_BLOCK, (yyloc));
        (yyval.stmt)->u.block.decls = (yyvsp[-4].decl);
        (yyval.stmt)->u.block.stmts = (yyvsp[-3].stmt);
        // Close scope if needed (i.e. if there was a declaration section)
        if((yyvsp[-4].decl)) {
            end_scope(context, (yylsp[-2]));
        }
    }
#line 3589 "grammar83.tab.c"
    break;

  case 266: /* block_decl: %empty  */
#line 1094 "grammar83.y"
                                                    { (yyval.decl) = NULL; }
#line 3595 "grammar83.tab.c"
    break;

  case 267: /* $@1: %empty  */
#line 1095 "grammar83.y"
            { begin_scope(context, (yylsp[0])); }
#line 3601 "grammar83.tab.c"
    break;

  case 268: /* block_decl: DECLARE $@1 decl_part  */
#line 1095 "grammar83.y"
                                                    { (yyval.decl) = (yyvsp[0].decl); }
#line 3607 "grammar83.tab.c"
    break;

  case 269: /* block_body: BEGiN handled_stmt_s  */
#line 1099 "grammar83.y"
                         { (yyval.stmt) = (yyvsp[0].stmt); }
#line 3613 "grammar83.tab.c"
    break;

  case 270: /* handled_stmt_s: statement_s except_handler_part_opt  */
#line 1104 "grammar83.y"
                                        { (yyval.stmt) = (yyvsp[-1].stmt_list).first; }
#line 3619 "grammar83.tab.c"
    break;

  case 273: /* exit_stmt: EXIT name_opt when_opt ';'  */
#line 1113 "grammar83.y"
                               {
        (yyval.stmt) = create_stmt(STMT_EXIT, (yyloc));
        // TODO: name_opt
        (yyval.stmt)->u.exit.condition = (yyvsp[-1].expr);
    }
#line 3629 "grammar83.tab.c"
    break;

  case 276: /* when_opt: %empty  */
#line 1125 "grammar83.y"
                   { (yyval.expr) = NULL; }
#line 3635 "grammar83.tab.c"
    break;

  case 277: /* when_opt: WHEN condition  */
#line 1126 "grammar83.y"
                   { (yyval.expr) = (yyvsp[0].expr); }
#line 3641 "grammar83.tab.c"
    break;

  case 278: /* return_stmt: RETURN ';'  */
#line 1130 "grammar83.y"
                  { (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc)); }
#line 3647 "grammar83.tab.c"
    break;

  case 279: /* return_stmt: RETURN expression ';'  */
#line 1131 "grammar83.y"
                          {
        (yyval.stmt) = create_stmt(STMT_RETURN, (yyloc));
        (yyval.stmt)->u.return_.expr = (yyvsp[-1].expr);
    }
#line 3656 "grammar83.tab.c"
    break;

  case 280: /* goto_stmt: GOTO name ';'  */
#line 1137 "grammar83.y"
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
#line 3683 "grammar83.tab.c"
    break;

  case 281: /* subprog_decl: subprog_spec ';'  */
#line 1161 "grammar83.y"
                          { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3689 "grammar83.tab.c"
    break;

  case 283: /* @2: %empty  */
#line 1167 "grammar83.y"
                                           {
        begin_scope(context, (yylsp[0]));
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
    }
#line 3699 "grammar83.tab.c"
    break;

  case 284: /* subprog_spec: PROCEDURE simple_name @2 formal_part_opt  */
#line 1172 "grammar83.y"
                                { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3705 "grammar83.tab.c"
    break;

  case 285: /* @3: %empty  */
#line 1173 "grammar83.y"
                                         {
        begin_scope(context, (yylsp[0]));
        // TODO: check for name conflict
        (yyval.subprogram_decl) = create_subprogram_decl((yyvsp[0].str_token), (yylsp[0]));
    }
#line 3715 "grammar83.tab.c"
    break;

  case 286: /* subprog_spec: FUNCTION designator @3 formal_part_opt RETURN name  */
#line 1178 "grammar83.y"
                                { (yyval.subprogram_decl) = (yyvsp[-3].subprogram_decl); }
#line 3721 "grammar83.tab.c"
    break;

  case 289: /* designator: char_string  */
#line 1184 "grammar83.y"
                { (yyval.str_token) = string_pool_to_token((yyvsp[0].str)); }
#line 3727 "grammar83.tab.c"
    break;

  case 297: /* mode: %empty  */
#line 1207 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3733 "grammar83.tab.c"
    break;

  case 298: /* mode: IN  */
#line 1208 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN; }
#line 3739 "grammar83.tab.c"
    break;

  case 299: /* mode: OUT  */
#line 1209 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_OUT; }
#line 3745 "grammar83.tab.c"
    break;

  case 300: /* mode: IN OUT  */
#line 1210 "grammar83.y"
           { (yyval.param_mode) = PARAM_MODE_IN_OUT; }
#line 3751 "grammar83.tab.c"
    break;

  case 301: /* subprog_spec_is_push: subprog_spec IS  */
#line 1214 "grammar83.y"
                    { (yyval.subprogram_decl) = (yyvsp[-1].subprogram_decl); }
#line 3757 "grammar83.tab.c"
    break;

  case 302: /* subprog_body: subprog_spec_is_push decl_part block_body END id_opt ';'  */
#line 1218 "grammar83.y"
                                                             {
        (yyval.subprogram_decl) = (yyvsp[-5].subprogram_decl);
        (yyval.subprogram_decl)->decls = (yyvsp[-4].decl);
        (yyval.subprogram_decl)->stmts = (yyvsp[-3].stmt);
    }
#line 3767 "grammar83.tab.c"
    break;

  case 303: /* procedure_call: name ';'  */
#line 1225 "grammar83.y"
             {
        (yyval.stmt) = create_stmt(STMT_EXPR, (yyloc));
        (yyval.stmt)->u.expr.kind = EXPR_NAME;
        (yyval.stmt)->u.expr.line_num = (yyloc);
        (yyval.stmt)->u.expr.u.name = (yyvsp[-1].name);
    }
#line 3778 "grammar83.tab.c"
    break;

  case 304: /* pkg_decl: pkg_spec ';'  */
#line 1233 "grammar83.y"
                         { (yyval.pkg_spec) = (yyvsp[-1].pkg_spec); }
#line 3784 "grammar83.tab.c"
    break;

  case 306: /* pkg_spec: PACKAGE simple_name IS decl_item_s private_part END simple_name_opt  */
#line 1238 "grammar83.y"
                                                                        {
        (yyval.pkg_spec) = calloc(1, sizeof(PackageSpec));
        (yyval.pkg_spec)->base.kind = DECL_PKG_SPEC;
        (yyval.pkg_spec)->base.line_num = (yyloc);
        (yyval.pkg_spec)->name = (yyvsp[-5].str_token);
        (yyval.pkg_spec)->decls = (yyvsp[-3].decl);
        // TODO: private part
        // TODO: check simple_name_opt matches
    }
#line 3798 "grammar83.tab.c"
    break;

  case 311: /* pkg_body: PACKAGE BODY simple_name IS decl_part body_opt END simple_name_opt ';'  */
#line 1259 "grammar83.y"
                                                                           {
        (yyval.pkg_body) = calloc(1, sizeof(PackageBody));
        (yyval.pkg_body)->base.kind = DECL_PKG_BODY;
        (yyval.pkg_body)->base.line_num = (yyloc);
        (yyval.pkg_body)->name = (yyvsp[-6].str_token);
        (yyval.pkg_body)->decls = (yyvsp[-4].decl);
        // TODO: body_opt
        // TODO: check simple_name_opt matches
    }
#line 3812 "grammar83.tab.c"
    break;

  case 328: /* comp_unit: context_spec unit pragma_s  */
#line 1310 "grammar83.y"
                               { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3818 "grammar83.tab.c"
    break;

  case 329: /* comp_unit: unit pragma_s  */
#line 1311 "grammar83.y"
                               { (yyval.comp_unit) = (yyvsp[-1].comp_unit); }
#line 3824 "grammar83.tab.c"
    break;

  case 336: /* unit: pkg_decl  */
#line 1330 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_PACKAGE_SPEC);
        (yyval.comp_unit)->u.package_spec = (yyvsp[0].pkg_spec);
    }
#line 3833 "grammar83.tab.c"
    break;

  case 337: /* unit: pkg_body  */
#line 1334 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_PACKAGE_BODY);
        (yyval.comp_unit)->u.package_body = (yyvsp[0].pkg_body);
    }
#line 3842 "grammar83.tab.c"
    break;

  case 338: /* unit: subprog_decl  */
#line 1338 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        (yyval.comp_unit)->u.subprogram_decl = (yyvsp[0].subprogram_decl);
    }
#line 3851 "grammar83.tab.c"
    break;

  case 339: /* unit: subprog_body  */
#line 1342 "grammar83.y"
                 {
        (yyval.comp_unit) = create_comp_unit(COMP_UNIT_SUBPROGRAM);
        (yyval.comp_unit)->u.subprogram_decl = (yyvsp[0].subprogram_decl);
    }
#line 3860 "grammar83.tab.c"
    break;


#line 3864 "grammar83.tab.c"

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

#line 1487 "grammar83.y"


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
