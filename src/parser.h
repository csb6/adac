/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 27 "grammar83.y"

    #include <stdint.h>
    #include <stdbool.h>
    #include <ctype.h>
    #include "array.h"
    #include "ast.h"

    typedef uint32_t SourceLocation;

    typedef Expression* ExprPtr;
    DEFINE_ARRAY_TYPE(ExprPtr)

    DEFINE_ARRAY_TYPE(StringToken)

    #define YYLLOC_DEFAULT(Cur, Rhs, N) \
        do { \
            if(N > 0) { \
                (Cur) = YYRHSLOC(Rhs, 1); \
            } else { \
                (Cur) = YYRHSLOC(Rhs, 0); \
            } \
        } while (0);

    typedef struct {
        Declaration* first;
        Declaration* last;
    } DeclList;

    typedef struct {
        DeclList scope_stack[32];
        Declaration** symbol_table; // array of Declaration*
        uint32_t symbol_table_capacity;
        uint32_t symbol_table_size;
        uint8_t curr_scope_idx;
    } ParseContext;

#line 86 "parser.h"

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    DOT_DOT = 258,                 /* DOT_DOT  */
    BOX = 259,                     /* BOX  */
    LT_EQ = 260,                   /* LT_EQ  */
    EXPON = 261,                   /* EXPON  */
    NE = 262,                      /* NE  */
    GE = 263,                      /* GE  */
    IS_ASSIGNED = 264,             /* IS_ASSIGNED  */
    RIGHT_SHAFT = 265,             /* RIGHT_SHAFT  */
    ABORT = 266,                   /* ABORT  */
    ABS = 267,                     /* ABS  */
    ACCEPT = 268,                  /* ACCEPT  */
    ACCESS = 269,                  /* ACCESS  */
    ALL = 270,                     /* ALL  */
    AND = 271,                     /* AND  */
    ARRAY = 272,                   /* ARRAY  */
    AT = 273,                      /* AT  */
    BEGiN = 274,                   /* BEGiN  */
    BODY = 275,                    /* BODY  */
    CASE = 276,                    /* CASE  */
    CONSTANT = 277,                /* CONSTANT  */
    DECLARE = 278,                 /* DECLARE  */
    DELAY = 279,                   /* DELAY  */
    DELTA = 280,                   /* DELTA  */
    DIGITS = 281,                  /* DIGITS  */
    DO = 282,                      /* DO  */
    ELSE = 283,                    /* ELSE  */
    ELSIF = 284,                   /* ELSIF  */
    END = 285,                     /* END  */
    ENTRY = 286,                   /* ENTRY  */
    EXCEPTION = 287,               /* EXCEPTION  */
    EXIT = 288,                    /* EXIT  */
    FOR = 289,                     /* FOR  */
    FUNCTION = 290,                /* FUNCTION  */
    GENERIC = 291,                 /* GENERIC  */
    GOTO = 292,                    /* GOTO  */
    IF = 293,                      /* IF  */
    IN = 294,                      /* IN  */
    IS = 295,                      /* IS  */
    LIMITED = 296,                 /* LIMITED  */
    LOOP = 297,                    /* LOOP  */
    MOD = 298,                     /* MOD  */
    NEW = 299,                     /* NEW  */
    NOT = 300,                     /* NOT  */
    NuLL = 301,                    /* NuLL  */
    OF = 302,                      /* OF  */
    OR = 303,                      /* OR  */
    OTHERS = 304,                  /* OTHERS  */
    OUT = 305,                     /* OUT  */
    PACKAGE = 306,                 /* PACKAGE  */
    PRAGMA = 307,                  /* PRAGMA  */
    PRIVATE = 308,                 /* PRIVATE  */
    PROCEDURE = 309,               /* PROCEDURE  */
    RAISE = 310,                   /* RAISE  */
    RANGE = 311,                   /* RANGE  */
    RECORD = 312,                  /* RECORD  */
    REM = 313,                     /* REM  */
    RENAMES = 314,                 /* RENAMES  */
    RETURN = 315,                  /* RETURN  */
    REVERSE = 316,                 /* REVERSE  */
    SELECT = 317,                  /* SELECT  */
    SEPARATE = 318,                /* SEPARATE  */
    SUBTYPE = 319,                 /* SUBTYPE  */
    TASK = 320,                    /* TASK  */
    TERMINATE = 321,               /* TERMINATE  */
    THEN = 322,                    /* THEN  */
    TYPE = 323,                    /* TYPE  */
    USE = 324,                     /* USE  */
    WHEN = 325,                    /* WHEN  */
    WHILE = 326,                   /* WHILE  */
    WITH = 327,                    /* WITH  */
    XOR = 328,                     /* XOR  */
    char_lit = 329,                /* char_lit  */
    identifier = 330,              /* identifier  */
    char_string = 331,             /* char_string  */
    numeric_lit = 332,             /* numeric_lit  */
    goto_label = 333               /* goto_label  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 161 "grammar83.y"

    UnaryOperator unary_op;
    BinaryOperator binary_op;
    Expression* expr;
    Statement* stmt;
    TypeDecl* type_decl;
    SubprogramDecl* subprogram_decl;
    bool bool_;
    ParamMode param_mode;
    StringToken str_token;
    char c;
    StringView str; // Note: this StringView owns its allocated data
    Array_ExprPtr expr_array;
    Array_StringToken str_token_array;

#line 197 "parser.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
typedef SourceLocation YYLTYPE;




int yyparse (void* scanner, ParseContext* context);

/* "%code provides" blocks.  */
#line 65 "grammar83.y"

    void yyerror(YYLTYPE* yyloc, void* scanner, ParseContext* parse_ctx, const char* msg);

#line 218 "parser.h"

#endif /* !YY_YY_PARSER_H_INCLUDED  */
