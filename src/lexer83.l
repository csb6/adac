%{
/******* A "lex"-style lexer for Ada 9X ****************************/
/* Copyright (C) Intermetrics, Inc. 1994 Cambridge, MA  USA        */
/* Copying permitted if accompanied by this statement.             */
/* Derivative works are permitted if accompanied by this statement.*/
/* This lexer is known to be only approximately correct, but it is */
/* more than adequate for most uses (the lexing of apostrophe is   */
/* not as sophisticated as it needs to be to be "perfect").        */
/* As usual there is *no warranty* but we hope it is useful.       */
/*                                                                 */
/* Ada 83 lexer (adapted from Ada 9X lexer)                        */
/* Copyright 2026 Cole Blakley                                     */
/*******************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include "error.h"
#include "parser.h"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#include "keywords.c"
#pragma GCC diagnostic pop
%}

%option 8bit noyywrap nounput noinput
/* Gets rid of fileno check */
%option never-interactive
/* Use equivalence classes */
%option ecs
/* Connect with pure Bison parser */
%option reentrant bison-bridge bison-locations
/* Extra data to be used in parser actions */
/*%option extra-type="SourceFileData"*/

DIGIT           [0-9]
EXTENDED_DIGIT  [0-9a-zA-Z]
INTEGER         ({DIGIT}(_?{DIGIT})*)
EXPONENT        ([eE](\+?|-){INTEGER})
DECIMAL_LITERAL {INTEGER}(\.?{INTEGER})?{EXPONENT}?
BASE            {INTEGER}
BASED_INTEGER   {EXTENDED_DIGIT}(_?{EXTENDED_DIGIT})*
BASED_LITERAL   {BASE}#{BASED_INTEGER}(\.{BASED_INTEGER})?#{EXPONENT}?
IDENTIFIER      [a-zA-Z](_?[a-zA-Z0-9])*

%%
"."                     return '.';
"<"                     return '<';
"("                     return '(';
"+"                     return '+';
"|"                     return '|';
"&"                     return '&';
"*"                     return '*';
")"                     return ')';
";"                     return ';';
"-"                     return '-';
"/"                     return '/';
","                     return ',';
">"                     return '>';
":"                     return ':';
"="                     return '=';
"'"                     return '\'';
".."                    return DOT_DOT;
"<>"                    return BOX;
"<="                    return LT_EQ;
"**"                    return EXPON;
"/="                    return NE;
">="                    return GE;
":="                    return IS_ASSIGNED;
"=>"                    return RIGHT_SHAFT;
\<\<{IDENTIFIER}>>      {
    StringView token_text = { .value = yytext + 2, .len = yyleng - 4 }; // Exclude "<<" and ">>"
    yylval->str_token = string_pool_to_token(token_text);
    return goto_label;
}
{IDENTIFIER}            {
    const struct keyword_token* keyword = is_keyword(yytext, yyleng);
    if(keyword) {
        return keyword->kind;
    } else {
        StringView token_text = { .value = yytext, .len = yyleng };
        yylval->str_token = string_pool_to_token(token_text);
        return identifier;
    }
}
"'"."'"                 {
    yylval->c = yytext[1]; // Skip over the opening "'" character
    return char_lit;
};
\"(\"\"|[^\n\"])*\"     {
    char* str = calloc(yyleng + 1, sizeof(char));
    strcpy(str, yytext);
    yylval->str.value = str;
    yylval->str.len = yyleng;
    return char_string;
}
{DECIMAL_LITERAL}|{BASED_LITERAL}       {
    char* str = calloc(yyleng + 1, sizeof(char));
    strcpy(str, yytext);
    yylval->str.value = str;
    yylval->str.len = yyleng;
    return numeric_lit;
}
--.*\n                  (*yylloc)++;
[ \t\f]                 ;
\n                      (*yylloc)++;
<<EOF>>                 return YYEOF;
.                       {
    error_print(*yylloc, "Illegal character: %c", *yytext);
    return YYerror;
}
%%
