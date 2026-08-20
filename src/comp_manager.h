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
#ifndef ADAC_COMP_MANAGER_H
#define ADAC_COMP_MANAGER_H

#include "error.h"

struct CompilationManager_;
typedef struct CompilationManager_ CompilationManager;
struct CompilationUnit_;

CompilationManager* comp_manager_init(void);

void comp_manager_add_source_dir(CompilationManager* comp_manager, const char* source_dir);

struct CompilationUnit_* comp_manager_parse_spec(
    CompilationManager* comp_manager, const char* spec_name, SourceLocation* loc);

struct CompilationUnit_* comp_manager_parse_unit(CompilationManager* comp_manager, const char* unit_name);

#endif /* ADAC_COMP_MANAGER_H */
