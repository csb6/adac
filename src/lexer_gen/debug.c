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
#include "debug.h"
#include <stdio.h>
#include <string.h>
#include "ast.h"
#include "string_view.h"

static
void print_type_decl(const TypeDecl* type_decl)
{
    printf("type %.*s is ", SV(type_decl->name));
    switch(type_decl->kind) {
        case TYPE_OPTION: {
            const OptionType* option_type = &type_decl->u.option;
            printf("option ");
            for(uint32_t i = 0; i < option_type->option_count; ++i) {
                printf("%.*s", SV(option_type->options[i]));
                if(i < option_type->option_count - 1) {
                    printf(" | ");
                }
            }
            break;
        }
        case TYPE_RANGE: {
            const RangeType* range_type = &type_decl->u.range;
            printf("range %u .. %u", range_type->start, range_type->end);
            break;
        }
        default:
            printf("Unknown type");
    }
    printf(";\n");
}

static
void print_table_decl(const TableDecl* table_decl)
{
    printf("%.*s: [%.*s] = {\n", SV(table_decl->name), SV(table_decl->domain->name));
    for(const IState* istate = table_decl->istates; istate != NULL; istate = istate->next) {
        printf("  %.*s: {\n", SV(istate->name));
        for(uint32_t i = 0; i < istate->transition_count; ++i) {
            char c_str[3] = {0};
            char c_str1[3] = {0};
            const Transition* transition = istate->transitions + i;
            printf("    ");
            switch(transition->kind) {
                case TRANSITION_CHAR:
                    escape_char(transition->u.c, c_str);
                    printf("'%s'", c_str);
                    break;
                case TRANSITION_RANGE:
                    escape_char(transition->u.range.start, c_str);
                    escape_char(transition->u.range.end, c_str1);
                    printf("'%s'..'%s'", c_str, c_str1);
                    break;
                case TRANSITION_OTHERS:
                    printf("others");
                    break;
                default:
                    printf("Unknown transition kind");
            }
            printf(": %.*s,\n", SV(istate->transitions[i].next_state_name));
        }
        printf("  },\n");
    }
    printf("};\n");
}

void print_module(const struct Module_* module)
{
    for(const TypeDecl* type_decl = module->types; type_decl != NULL; type_decl = type_decl->next) {
        print_type_decl(type_decl);
    }

    for(const TableDecl* table_decl = module->tables; table_decl != NULL; table_decl = table_decl->next) {
        print_table_decl(table_decl);
    }
}
