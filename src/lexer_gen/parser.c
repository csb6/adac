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
#include "parser.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "error.h"
#include "lexer.h"

static struct {
    const char* input_start;
    const char* input_end;
    const char* curr;
    Token token;
    Module* module;
} ctx;

/* DECLARATIONS */
static TableDecl* parse_table_decl(void);
static TableEntry* parse_table_entry(void);
static void parse_mapping_key(Mapping* mapping);
static char parse_char_literal(void);
static TypeDecl* parse_type_decl(void);
static void parse_option_type_decl(OptionType* decl);
static void parse_range_type_decl(RangeType* decl);
/* UTILITIES */
#define print_parse_error(...) error_print2(ctx.token.line_num, __VA_ARGS__)
static void print_unexpected_token_error(const Token* token);
static uint8_t count_options(void);
static uint32_t count_mappings(void);
static void next_token(void);
static void expect_token(TokenKind kind);
static TypeDecl* find_type_decl(StringView name);
static uint16_t find_option(const StringView* options, uint8_t option_count, StringView option_name);

void parser_parse(const char* input_start, const char* input_end, struct Module_* module)
{
    memset(&ctx, 0, sizeof(ctx));
    ctx.input_start = input_start;
    ctx.input_end = input_end;
    ctx.curr = input_start;
    ctx.module = module;

    TableDecl* last_table = NULL;
    TypeDecl* last_type = NULL;
    next_token(); // Retrieve first token
    while(ctx.token.kind != TOKEN_EOF) {
        switch(ctx.token.kind) {
            case TOKEN_IDENT: {
                TableDecl* decl = parse_table_decl();
                if(last_table) {
                    last_table->next = decl;
                } else {
                    module->tables = decl;
                }
                last_table = decl;
                break;
            }
            case TOKEN_TYPE: {
                TypeDecl* decl = parse_type_decl();
                if(last_type) {
                    last_type->next = decl;
                } else {
                    module->types = decl;
                }
                last_type = decl;
                break;
            }
            default:
                print_unexpected_token_error(&ctx.token);
                error_exit();
        }
        expect_token(TOKEN_SEMICOLON);
        next_token();
    }
}

static
TableDecl* parse_table_decl(void)
{
    TableDecl* decl = calloc(1, sizeof(TableDecl));

    expect_token(TOKEN_IDENT);
    decl->name = ctx.token.text;
    decl->line_num = ctx.token.line_num;
    next_token();

    expect_token(TOKEN_COLON);
    next_token();
    expect_token(TOKEN_L_SQ_BRACKET);
    next_token();
    expect_token(TOKEN_IDENT);
    decl->domain = find_type_decl(ctx.token.text);
    if(!decl->domain) {
        print_parse_error("Unknown type: '%.*s'", SV(ctx.token.text));
        error_exit();
    }
    next_token();
    expect_token(TOKEN_R_SQ_BRACKET);
    next_token();

    expect_token(TOKEN_EQ);
    next_token();

    expect_token(TOKEN_L_BRACE);
    next_token();
    TableEntry* last_entry = NULL;
    while(ctx.token.kind != TOKEN_R_BRACE) {
        TableEntry* entry = parse_table_entry();
        if(last_entry) {
            last_entry->next = entry;
        } else {
            decl->entries = entry;
        }
        last_entry = entry;
    }
    next_token(); // Skip '}'

    return decl;
}

static
TableEntry* parse_table_entry(void)
{
    TableEntry* entry = calloc(1, sizeof(TableEntry));
    expect_token(TOKEN_IDENT);
    entry->state = ctx.token.text;
    entry->line_num = ctx.token.line_num;
    next_token();
    expect_token(TOKEN_COLON);
    next_token();
    expect_token(TOKEN_L_BRACE);
    next_token();
    entry->mapping_count = count_mappings();
    entry->mappings = calloc(entry->mapping_count, sizeof(Mapping));
    uint32_t i = 0;
    while(i < entry->mapping_count) {
        parse_mapping_key(entry->mappings + i);
        uint32_t first_key = i;
        // Parse each alternative as if it were a separate mappings with identical next_state
        while(ctx.token.kind == TOKEN_BAR) {
            next_token();
            ++i;
            parse_mapping_key(entry->mappings + i);
        }
        expect_token(TOKEN_COLON);
        next_token();
        for(uint32_t j = first_key; j <= i; ++j) {
            entry->mappings[j].next_state = ctx.token.text;
        }
        next_token();
        if(i < entry->mapping_count - 1) {
            expect_token(TOKEN_COMMA);
            next_token();
        } else if(ctx.token.kind == TOKEN_COMMA) {
            // Optional trailing comma
            next_token();
        }
        ++i;
    }
    expect_token(TOKEN_R_BRACE);
    next_token();
    if(ctx.token.kind == TOKEN_COMMA) {
        // Optional trailing comma
        next_token();
    }
    return entry;
}

static
void parse_mapping_key(Mapping* mapping)
{
    mapping->line_num = ctx.token.line_num;
    switch(ctx.token.kind) {
        case TOKEN_CHAR_LITERAL: {
            char c = parse_char_literal();
            next_token();
            if(ctx.token.kind == TOKEN_DOUBLE_DOT) {
                next_token();
                expect_token(TOKEN_CHAR_LITERAL);
                mapping->kind = MAPPING_RANGE;
                mapping->u.range.start = c;
                mapping->u.range.end = parse_char_literal();
                next_token();
            } else {
                mapping->kind = MAPPING_CHAR;
                mapping->u.c = c;
            }
            break;
        }
        case TOKEN_OTHERS:
            mapping->kind = MAPPING_OTHERS;
            next_token();
            break;
        default:
            print_unexpected_token_error(&ctx.token);
            error_exit();
    }
}

static
char parse_char_literal(void)
{
    char c;
    if(ctx.token.text.value[0] == '\\') {
        if(ctx.token.text.len != 2) {
            print_parse_error("Invalid escape sequence: '%.*s'", SV(ctx.token.text));
            error_exit();
        }
        c = unescape_char(ctx.token.text.value[1]);
        if(!c) {
            print_parse_error("Invalid escape sequence: '%.*s'", SV(ctx.token.text));
            error_exit();
        }
    } else {
        c = ctx.token.text.value[0];
    }
    return c;
}

static
TypeDecl* parse_type_decl(void)
{
    TypeDecl* decl = calloc(1, sizeof(TypeDecl));

    next_token(); // Skip 'type' keyword
    expect_token(TOKEN_IDENT);
    decl->name = ctx.token.text;
    decl->line_num = ctx.token.line_num;
    next_token();

    expect_token(TOKEN_IS);
    next_token();

    if(ctx.token.kind == TOKEN_OPTION) {
        decl->kind = TYPE_OPTION;
        parse_option_type_decl(&decl->u.option);
    } else if(ctx.token.kind == TOKEN_IDENT) {
        decl->kind = TYPE_RANGE;
        parse_range_type_decl(&decl->u.range);
    } else {
        print_unexpected_token_error(&ctx.token);
        error_exit();
    }

    return decl;
}

static
void parse_option_type_decl(OptionType* decl)
{
    next_token(); // Skip 'option' keyword
    decl->option_count = count_options();
    decl->options = calloc(decl->option_count, sizeof(StringView));
    for(uint8_t i = 0; i < decl->option_count; ++i) {
        expect_token(TOKEN_IDENT);
        decl->options[i] = ctx.token.text;
        next_token();
        if(i < decl->option_count - 1) {
            expect_token(TOKEN_BAR);
            next_token();
        }
    }
}

static
void parse_range_type_decl(RangeType* decl)
{
    TypeDecl* base_type_decl = find_type_decl(ctx.token.text);
    if(!base_type_decl) {
        print_parse_error("Unknown type '%.*s'", SV(ctx.token.text));
        error_exit();
    }
    if(base_type_decl->kind != TYPE_OPTION) {
        print_parse_error("Base type must be an option type, not another range type");
        error_exit();
    }
    decl->base_type = base_type_decl;
    const OptionType* base_option_type = &base_type_decl->u.option;
    next_token();

    expect_token(TOKEN_RANGE);
    next_token();

    expect_token(TOKEN_IDENT);
    uint16_t lower_bound = find_option(base_option_type->options, base_option_type->option_count, ctx.token.text);
    if(lower_bound == UINT16_MAX) {
        print_parse_error("Unable to find option '%.*s' under option type '%.*s'", SV(ctx.token.text), SV(base_type_decl->name));
        error_exit();
    }
    next_token();
    expect_token(TOKEN_DOUBLE_DOT);
    next_token();
    expect_token(TOKEN_IDENT);
    uint16_t upper_bound = find_option(base_option_type->options, base_option_type->option_count, ctx.token.text);
    if(upper_bound == UINT16_MAX) {
        print_parse_error("Unable to find option '%.*s' under option type '%.*s'", SV(ctx.token.text), SV(base_type_decl->name));
        error_exit();
    }
    if(upper_bound < lower_bound) {
        print_parse_error("Upper bound of range type must be greater than or equal than the lower bound");
        error_exit();
    }
    decl->start = (uint8_t)lower_bound;
    decl->end = (uint8_t)upper_bound;
    next_token();
}

static
uint8_t count_options(void)
{
    uint8_t option_count = 1;
    Token token = ctx.token;
    const char* curr = ctx.curr;
    while(token.kind != TOKEN_SEMICOLON) {
        switch(token.kind) {
            case TOKEN_BAR:
                if(option_count == UINT8_MAX) {
                    error_print(ctx.input_start, curr, "Too many options for option type (maximum supported is 255)");
                    error_exit();
                }
                ++option_count;
                break;
            case TOKEN_IDENT:
                break;
            case TOKEN_EOF:
                error_print(ctx.input_start, curr, "Unexpected end of input");
                error_exit();
                break;
            default:
                print_unexpected_token_error(&token);
                error_exit();
        }
        curr = lexer_parse_token(ctx.input_start, ctx.input_end, curr, &token);
    }
    return option_count;
}

static
uint32_t count_mappings(void)
{
    uint32_t count = 0;
    Token token = ctx.token;
    const char* curr = ctx.curr;
    while(token.kind != TOKEN_R_BRACE) {
        switch(token.kind) {
            case TOKEN_BAR:
            case TOKEN_COLON:
                ++count;
                break;
            default:
                break;
        }
        curr = lexer_parse_token(ctx.input_start, ctx.input_end, curr, &token);
    }
    return count;
}

static
void next_token(void)
{
    ctx.curr = lexer_parse_token(ctx.input_start, ctx.input_end, ctx.curr, &ctx.token);
}

static
void expect_token(TokenKind kind)
{
    if(ctx.token.kind != kind) {
        print_unexpected_token_error(&ctx.token);
        print_parse_error("Expected token: '%s'", token_kind_to_str(kind));
        error_exit();
    }
}

static
void print_unexpected_token_error(const Token* token)
{
    error_print2(token->line_num, "Unexpected token: '%.*s'", SV(token->text));
}

static
TypeDecl* find_type_decl(StringView name)
{
    for(TypeDecl* decl = ctx.module->types; decl != NULL; decl = decl->next) {
        if(string_view_eq(&decl->name, &name)) {
            return decl;
        }
    }
    return NULL;
}

static
uint16_t find_option(const StringView* options, uint8_t option_count, StringView option_name)
{
    for(uint16_t i = 0; i < option_count; ++i) {
        if(string_view_eq(options + i, &option_name)) {
            return i;
        }
    }
    return UINT16_MAX;
}
