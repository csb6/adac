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
#include "string_pool.h"
#include "ast.h"
#include "error.h"
#include "parser.h"
#include "lexer.h"
#include "string_view.h"

#define NAME spec_cache_map
#define KEY_TY StringToken
#define VAL_TY CompilationUnit*
#define HASH_FN vt_hash_integer
#define CMPR_FN vt_cmpr_integer
#include "verstable.h"

typedef const char* Path;
DEFINE_ARRAY_TYPE(Path)
DEFINE_ARRAY_OPS(Path)

static
char* add_extension(const char* file_stem, const char* extension);

static
char* create_input_file_path(const char* source_dir, const char* file_name);

static
const char* find_dir(const char** dir_list, uint32_t dir_count, char* file_name);

static
CompilationUnit* parse_unit(CompilationManager* comp_manager, const char* source_dir, const char* unit_file_name);


struct CompilationManager_ {
    PathArray source_dirs;
    spec_cache_map spec_cache;
};

CompilationManager* comp_manager_init(void)
{
    CompilationManager* comp_manager = calloc(1, sizeof(CompilationManager));
    PathArray_init(&comp_manager->source_dirs);
    spec_cache_map_init(&comp_manager->spec_cache);
    return comp_manager;
}

void comp_manager_add_source_dir(CompilationManager* comp_manager, const char* source_dir)
{
    PathArray_append(&comp_manager->source_dirs, source_dir);
}

CompilationUnit* comp_manager_parse_spec(
    CompilationManager* comp_manager, const char* spec_name, SourceLocation* loc)
{
    StringToken spec_name_token = string_pool_c_str_to_token(spec_name);
    spec_cache_map_itr it = spec_cache_map_get(&comp_manager->spec_cache, spec_name_token);
    if(!spec_cache_map_is_end(it)) {
        return it.data->val;
    }
    char* spec_file_name = add_extension(spec_name, ".ads");
    const char* source_dir = find_dir(
        comp_manager->source_dirs.data, PathArray_size(&comp_manager->source_dirs), spec_file_name);
    if(!source_dir) {
        if(loc) {
            error_print(*loc, "Unable to find spec file for unit '%s'", spec_name);
        } else {
            error_print_general("Unable to find spec file for unit '%s'", spec_name);
        }
        error_exit();
    }

    CompilationUnit* comp_unit = parse_unit(comp_manager, source_dir, spec_file_name);
    free(spec_file_name);
    spec_cache_map_insert(&comp_manager->spec_cache, spec_name_token, comp_unit);
    return comp_unit;
}

CompilationUnit* comp_manager_parse_unit(CompilationManager* comp_manager, const char* unit_name)
{
    char* unit_file_name = add_extension(unit_name, ".adb");
    const char* source_dir = find_dir(
        comp_manager->source_dirs.data, PathArray_size(&comp_manager->source_dirs), unit_file_name);
    if(!source_dir) {
        error_print_general("Unable to find source file for unit '%s'", unit_name);
        error_exit();
    }

    CompilationUnit* comp_unit = parse_unit(comp_manager, source_dir, unit_file_name);
    free(unit_file_name);
    return comp_unit;
}

static
CompilationUnit* parse_unit(CompilationManager* comp_manager, const char* source_dir, const char* unit_file_name)
{
    char* input_file_path = create_input_file_path(source_dir, unit_file_name);
    FILE* input_file = fopen(input_file_path, "rb");
    if(!input_file) {
        error_print_general("%s", strerror(errno));
        error_exit();
    }
    yyscan_t lexer;
    yylex_init(&lexer);
    yyset_in(input_file, lexer);

    ParseContext parse_ctx = {0};
    parse_ctx.comp_manager = comp_manager;
    parse_ctx.file_id = error_get_file_id(input_file_path);
    int parse_status = yyparse(lexer, &parse_ctx);
    yylex_destroy(lexer);
    fclose(input_file);
    if(parse_status != 0) {
        error_print_general("Compilation failed");
        error_exit();
    }
    assert(parse_ctx.curr_scope_idx == 0);
    free(input_file_path);
    return parse_ctx.comp_unit;
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
    // path = source_dir + path_separator + file_name + '\0'
    char* input_file_path = calloc(strlen(source_dir) + strlen(file_name) + 2, sizeof(char));
    strcpy(input_file_path, source_dir);
    strcat(input_file_path, "/");
    strcat(input_file_path, file_name);
    return input_file_path;
}

// Note: modifies file_name in-place to use the exact casing of the file that matches
static
const char* find_dir(const char** dir_list, uint32_t dir_count, char* file_name)
{
    for(uint32_t i = 0; i < dir_count; ++i) {
        DIR* directory = opendir(dir_list[i]);
        if(!directory) {
            fprintf(stderr, "Warning: Couldn't open directory: '%s'\n", dir_list[i]);
            continue;
        }
        struct dirent* entry;
        while((entry = readdir(directory))) {
            if(string_caseless_eq(entry->d_name, file_name)) {
                // Replace with version with exact casing
                strcpy(file_name, entry->d_name);
                closedir(directory);
                return dir_list[i];
            }
        }
        closedir(directory);
    }
    return NULL;
}
