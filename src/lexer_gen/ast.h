/*
lexer_gen - lexer generator
Copyright (C) 2025  Cole Blakley

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/
#ifndef LEXER_GEN_AST_H
#define LEXER_GEN_AST_H

#include <stdint.h>
#include "string_view.h"

struct TypeDecl_;

typedef uint8_t TypeKind;
enum {
    TYPE_OPTION, TYPE_RANGE
};

typedef struct {
    StringView* options;
    uint8_t option_count;
} OptionType;

typedef struct {
    struct TypeDecl_* base_type;
    uint8_t start;
    uint8_t end; // inclusive
} RangeType;

typedef struct TypeDecl_ {
    StringView name;
    uint32_t line_num;
    TypeKind kind;
    union {
        OptionType option;
        RangeType range;
    } u;
    struct TypeDecl_* next;
} TypeDecl;

typedef uint8_t TransitionKind;
enum {
    TRANSITION_CHAR,
    TRANSITION_RANGE,
    TRANSITION_OTHERS
};

typedef struct {
    char start;
    char end; // inclusive
} CharRange;

typedef struct {
    uint32_t line_num;
    TransitionKind kind;
    uint8_t next_state; // Only used in generator
    union {
        char c;
        CharRange range;
    } u;
    StringView next_state_name;
} Transition;

// Intermediate state
typedef struct IState_ {
    StringView name;
    Transition* transitions;
    uint32_t line_num;
    uint8_t transition_count;
    Transition* default_transition; // Set in checker; also included in transition list
    struct IState_* next;
} IState;

typedef struct TableDecl_ {
    StringView name;
    uint32_t line_num;
    uint8_t istate_count;
    TypeDecl* domain;
    IState* istates;
    struct TableDecl_* next;
} TableDecl;

typedef struct Module_ {
    TypeDecl* types;
    TableDecl* tables;
} Module;

#endif /* LEXER_GEN_AST_H */
