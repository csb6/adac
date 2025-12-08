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
    uint32_t option_count;
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

typedef uint8_t MappingKind;
enum {
    MAPPING_CHAR,
    MAPPING_RANGE,
    MAPPING_OTHERS
};

typedef struct {
    char start;
    char end; // inclusive
} CharRange;

typedef struct {
    MappingKind kind;
    uint32_t line_num;
    union {
        char c;
        CharRange range;
    } u;
    StringView next_state;
} Mapping;

typedef struct TableEntry_ {
    StringView state;
    Mapping* mappings;
    uint32_t mapping_count;
    uint32_t line_num;
    struct TableEntry_* next;
} TableEntry;

typedef struct TableDecl_ {
    StringView name;
    uint32_t line_num;
    TypeDecl* domain;
    TableEntry* entries;
    struct TableDecl_* next;
} TableDecl;

typedef struct Module_ {
    TypeDecl* types;
    TableDecl* tables;
} Module;

#endif /* LEXER_GEN_AST_H */
