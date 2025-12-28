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
#include "checker.h"
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include "ast.h"
#include "error.h"

static void check_type_decl(const TypeDecl* type_decl);
static void check_table_decl(const TableDecl* table_decl);
static void check_overlap(const Transition* a, const Transition* b);
static void check_char_and_range(uint32_t line_num, char c, CharRange range);
static bool find_option(const TypeDecl* type_decl, const StringView* name);

void check_module(const Module* module)
{
    if(!module->types) {
        error_print(1, "Module must have at least 1 type");
        error_exit();
    }
    if(!module->tables) {
        error_print(1, "Module must have at least 1 table");
        error_exit();
    }

    for(const TypeDecl* type_decl = module->types; type_decl != NULL; type_decl = type_decl->next) {
        // Check for redefinitions
        for(const TypeDecl* other = type_decl->next; other != NULL; other = other->next) {
            if(string_view_eq(&other->name, &type_decl->name)) {
                error_print(other->line_num, "Redefinition of type '%.*s'", SV(other->name));
                error_exit();
            }
        }
        check_type_decl(type_decl);
    }

    for(const TableDecl* table_decl = module->tables; table_decl != NULL; table_decl = table_decl->next) {
        // Check for redefinitions
        for(const TableDecl* other = table_decl->next; other != NULL; other = other->next) {
            if(string_view_eq(&other->name, &table_decl->name)) {
                error_print(other->line_num, "Redefinition of table '%.*s'", SV(other->name));
                error_exit();
            }
        }
        check_table_decl(table_decl);
    }
}

static
void check_type_decl(const TypeDecl* type_decl)
{
    switch(type_decl->kind) {
        case TYPE_RANGE:
            if(type_decl->u.range.start > type_decl->u.range.end) {
                error_print(type_decl->line_num, "Start of range must be less than or equal to end of range");
                error_exit();
            }
            break;
        case TYPE_OPTION: {
            const OptionType* option_type = &type_decl->u.option;
            if(option_type->option_count == 0) {
                error_print(type_decl->line_num, "Option types must have at least 1 option");
                error_exit();
            }
            for(uint16_t i = 0; i < option_type->option_count; ++i) {
                for(uint16_t j = i + 1; j < option_type->option_count; ++j) {
                    if(string_view_eq(option_type->options + i, option_type->options + j)) {
                        error_print(type_decl->line_num, "Duplicate option: '%.*s'", SV(option_type->options[i]));
                        error_exit();
                    }
                }
            }
            break;
        }
        default:
            error_print(type_decl->line_num, "Invalid type kind");
            error_exit();
    }
}

static
void check_table_decl(const TableDecl* table_decl)
{
    // Number of tokens (i.e. final states)
    uint8_t token_count = 0;
    switch(table_decl->domain->kind) {
        case TYPE_OPTION:
            token_count = table_decl->domain->u.option.option_count;
            break;
        case TYPE_RANGE:
            token_count = table_decl->domain->u.range.end - table_decl->domain->u.range.start + 1;
            break;
        default:
            assert(false);
    }

    uint32_t state_count = table_decl->istate_count + token_count;
    if(state_count > UINT8_MAX) {
        error_print(table_decl->line_num, "Too many states for table '%.*s' (maximum supported is 255, has %u)", SV(table_decl->name), state_count);
        error_exit();
    }

    for(IState* istate = table_decl->istates; istate != NULL; istate = istate->next) {
        if(find_option(table_decl->domain, &istate->name)) {
            error_print(istate->line_num, "Intermediate state '%.*s' cannot share name of token in type '%.*s'", SV(istate->name), SV(table_decl->domain->name));
            error_exit();
        }
        // TODO: don't need 'others' case if istate has all cases specified explictly
        Transition* others = NULL;
        for(uint16_t i = 0; i < istate->transition_count; ++i) {
            if(istate->transitions[i].kind == TRANSITION_OTHERS) {
                if(others) {
                    error_print(istate->line_num, "State '%.*s' has multiple 'others' cases", SV(istate->name));
                    error_exit();
                }
                others = istate->transitions + i;
            }
        }
        if(!others) {
            error_print(istate->line_num, "State '%.*s' is missing an 'others' case", SV(istate->name));
            error_exit();
        }
        istate->default_transition = others;

        for(uint16_t i = 0; i < istate->transition_count; ++i) {
            for(uint16_t j = i + 1; j < istate->transition_count; ++j) {
                check_overlap(istate->transitions + i, istate->transitions + j);
            }
        }
    }
}

static
void check_overlap(const Transition* a, const Transition* b)
{
    if(a->kind == TRANSITION_CHAR) {
        if(b->kind == TRANSITION_CHAR) {
            if(a->u.c == b->u.c) {
                char c_str[3] = {0};
                escape_char(a->u.c, c_str);
                error_print(a->line_num, "'%s' has multiple possible transitions in this entry", c_str);
                error_exit();
            }
        } else if(b->kind == TRANSITION_RANGE) {
            check_char_and_range(a->line_num, a->u.c, b->u.range);
        }
    } else if(a->kind == TRANSITION_RANGE) {
        if(b->kind == TRANSITION_CHAR) {
            check_char_and_range(b->line_num, b->u.c, a->u.range);
        } else if(b->kind == TRANSITION_RANGE) {
            if((a->u.range.start >= b->u.range.start && a->u.range.start <= b->u.range.end)
                || (a->u.range.end >= b->u.range.start && a->u.range.end <= b->u.range.end)) {
                char range_start1[3] = {0};
                char range_end1[3] = {0};
                char range_start2[3] = {0};
                char range_end2[3] = {0};
                escape_char(a->u.range.start, range_start1);
                escape_char(a->u.range.end, range_end1);
                escape_char(b->u.range.start, range_start2);
                escape_char(b->u.range.end, range_end2);
                error_print(a->line_num, "Range '%s' .. '%s' overlaps with range '%s' .. '%s' in this entry", range_start1, range_end1, range_start2, range_end2);
                error_exit();
            }
        }
    }
}

static
void check_char_and_range(uint32_t line_num, char c, CharRange range)
{
    if(c >= range.start && c <= range.end) {
        char c_str[3] = {0};
        char range_start[3] = {0};
        char range_end[3] = {0};
        escape_char(c, c_str);
        escape_char(range.start, range_start);
        escape_char(range.end, range_end);
        error_print(line_num, "'%s' overlaps with range '%s' .. '%s' in this entry", c_str, range_start, range_end);
        error_exit();
    }
}

static
bool find_option(const TypeDecl* type_decl, const StringView* name)
{
    switch(type_decl->kind) {
        case TYPE_OPTION:
            for(uint16_t i = 0; i < type_decl->u.option.option_count; ++i) {
                if(string_view_eq(type_decl->u.option.options + i, name)) {
                    return true;
                }
            }
            break;
        case TYPE_RANGE:
            return find_option(type_decl->u.range.base_type, name);
        default:
            error_print(type_decl->line_num, "Invalid type kind");
            error_exit();
    }
    return false;
}
