/*
adac - Ada compiler
Copyright (C) 2026  Cole Blakley

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
#include "comp_manager.h"
#include <assert.h>
#include <dirent.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "array.h"
#include "ast.h"
#include "error.h"
#include "parser.h"
#include "lexer.h"

typedef const char* Path;
DEFINE_ARRAY_TYPE(Path)
DEFINE_ARRAY_OPS(Path)

static
char* add_extension(const char* file_stem, const char* extension);

static
char* create_input_file_path(const char* source_dir, const char* file_name);

static
const char* find_dir(const char** dir_list, uint32_t dir_count, const char* file_name);

struct CompilationManager_ {
    PathArray source_dirs;
};

CompilationManager* comp_manager_init(void)
{
    CompilationManager* comp_manager = calloc(1, sizeof(CompilationManager));
    PathArray_init(&comp_manager->source_dirs);
    return comp_manager;
}

void comp_manager_add_source_dir(CompilationManager* comp_manager, const char* source_dir)
{
    PathArray_append(&comp_manager->source_dirs, source_dir);
}

CompilationUnit* comp_manager_parse_unit(CompilationManager* comp_manager, const char* unit_name)
{
    char* unit_file_name = add_extension(unit_name, ".adb");

    const char* source_dir = find_dir(
        comp_manager->source_dirs.data, PathArray_size(&comp_manager->source_dirs), unit_file_name);
    if(!source_dir) {
        fprintf(stderr, "Error: Unable to find source file for unit '%s'\n", unit_name);
        free(unit_file_name);
        return NULL;
    }

    char* input_file_path = create_input_file_path(source_dir, unit_file_name);
    free(unit_file_name);
    FILE* input_file = fopen(input_file_path, "rb");
    if(!input_file) {
        perror(input_file_path);
        free(input_file_path);
        return NULL;
    }
    yyscan_t lexer;
    error_set_source_file_path(input_file_path);
    yylex_init(&lexer);
    yyset_in(input_file, lexer);

    ParseContext parse_ctx = {0};
    int parse_status = yyparse(lexer, &parse_ctx);
    yylex_destroy(lexer);
    fclose(input_file);
    if(parse_status != 0) {
        fprintf(stderr, "Compilation failed\n");
        free(input_file_path);
        return NULL;
    }
    assert(parse_ctx.curr_scope_idx == 0);
    free(input_file_path);
    return parse_ctx.comp_unit;
}

void yyerror(YYLTYPE* yyloc, yyscan_t scanner, ParseContext* parse_ctx, const char* msg)
{
    (void)scanner;
    (void)parse_ctx;
    error_print(*yyloc, msg);
    error_exit();
}

static
char* add_extension(const char* file_stem, const char* extension)
{
    char* file_name = calloc(strlen(file_stem) + strlen(extension) + 1, sizeof(char));
    strcpy(file_name, file_stem);
    strcat(file_name, extension);
    return file_name;
}

static
char* create_input_file_path(const char* source_dir, const char* file_name)
{
    // path = source_dir + file_name + path_separator + '\0'
    char* input_file_path = calloc(strlen(source_dir) + strlen(file_name) + 2, sizeof(char));
    strcpy(input_file_path, source_dir);
    strcat(input_file_path, "/");
    strcat(input_file_path, file_name);
    return input_file_path;
}

static
const char* find_dir(const char** dir_list, uint32_t dir_count, const char* file_name)
{
    for(uint32_t i = 0; i < dir_count; ++i) {
        DIR* directory = opendir(dir_list[i]);
        if(!directory) {
            fprintf(stderr, "Warning: Couldn't open directory: '%s'\n", dir_list[i]);
            continue;
        }
        struct dirent* entry;
        while((entry = readdir(directory))) {
            if(strcmp(entry->d_name, file_name) == 0) {
                closedir(directory);
                return dir_list[i];
            }
        }
        closedir(directory);
    }
    return NULL;
}
