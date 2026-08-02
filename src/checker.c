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
#include "checker.h"
#include "parser.h"
#include "ast.h"
#include "error.h"
#include <assert.h>

static
void gather_candidates(ParseContext* context, Expression* expr);

static
void gather_candidates_name(ParseContext* context, Expression* expr);

static
void add_overload(Expression* expr, uint32_t overload_n);


void resolve_stmt(ParseContext* context, Statement* stmt)
{
    // TODO: second pass to finish resolving names
    switch(stmt->kind) {
        case STMT_NULL:
        case STMT_GOTO:
            // Nothing left to resolve
            break;
        case STMT_EXPR:
            gather_candidates(context, &stmt->u.expr);
            break;
        case STMT_ASSIGN:
            gather_candidates(context, stmt->u.assign.expr);
            break;
        case STMT_EXIT:
            gather_candidates(context, stmt->u.exit.condition);
            break;
        case STMT_RETURN:
            gather_candidates(context, stmt->u.return_.expr);
            break;
        case STMT_BLOCK:
            // TODO: decls
            for(Statement* s = stmt->u.block.stmts; s; s = s->next) {
                resolve_stmt(context, s);
            }
            break;
        case STMT_IF:
            gather_candidates(context, stmt->u.if_.condition);
            for(Statement* s = stmt->u.if_.stmts; s; s = s->next) {
                resolve_stmt(context, s);
            }
            resolve_stmt(context, stmt->u.if_.else_);
            break;
        case STMT_CASE:
            gather_candidates(context, stmt->u.case_.expr);
            for(Case* c = stmt->u.case_.cases; c; c = c->next) {
                for(uint32_t i = 0; i < c->choice.count; ++i) {
                    if(c->choice.alternatives[i].kind == ALT_EXPR) {
                        gather_candidates(context, c->choice.alternatives[i].u.expr);
                    }
                }
                for(Statement* s = c->stmts; s; s = s->next) {
                    resolve_stmt(context, s);
                }
            }
            break;
        case STMT_LOOP:
            switch(stmt->u.loop.kind) {
                case LOOP_FOR:
                    gather_candidates(context, stmt->u.loop.u.for_.range);
                    break;
                case LOOP_WHILE:
                    gather_candidates(context, stmt->u.loop.u.while_.condition);
                    break;
                default:
                    assert(false && "Unhandled loop kind");
                    break;
            }
            for(Statement* s = stmt->u.loop.stmts; s; s = s->next) {
                resolve_stmt(context, s);
            }
            break;
        default:
            assert(false && "Unhandled statement kind");
            break;
    }
}

// TODO: run in all contexts (declarations, statements, representation clauses)
static
void gather_candidates(ParseContext* context, Expression* expr)
{
    switch(expr->kind) {
        case EXPR_INT_LIT:
        case EXPR_CHAR_LIT:
        case EXPR_STRING_LIT:
            // Nothing left to resolve
            break;
        case EXPR_UNARY:
            // TODO: resolve unary operator overloads
            gather_candidates(context, expr->u.unary.right);
            break;
        case EXPR_BINARY:
            // TODO: resolve binary operator overloads
            gather_candidates(context, expr->u.binary.left);
            gather_candidates(context, expr->u.binary.right);
            break;
        case EXPR_NAME:
            gather_candidates_name(context, expr);
            break;
        default:
            assert(false && "Unhandled expression kind");
            break;
    }
}

static
void gather_candidates_name(ParseContext* context, Expression* expr)
{
    assert(expr->kind == EXPR_NAME);
    NameExpr* name_expr = &expr->u.name;
    for(uint32_t i = 0; i < name_expr->arg_count; ++i) {
        gather_candidates(context, name_expr->args[i]);
    }
    ObjectDecl* object_decl = find_object_decl(context, name_expr->name);
    if(object_decl) {
        // If an object decl can be found, it means there are no other visible overloads
        // (any entities with same name would be hidden or a redefinition error would have
        // already occurred)
        if(name_expr->arg_count == 0) {
            expr->kind = EXPR_OBJECT;
            expr->u.object = object_decl;
            name_expr = NULL; // Can't use anymore
        } else if(name_expr->arg_count == 1) {
            //  - A one-dimensional array object (being sliced or indexed into)
            //  - A one-dimensional array rvalue returned from a function (being sliced or indexed into)
            // TODO
            assert(false);
        } else {
            //  - A multidimensional array object being indexed into
            //  - An multidimensional array rvalue returned from a function being indexed into
            // TODO
            assert(false);
        }
    } else {
        Declaration** bucket = find_bucket(context, name_expr->name);
        // See if this name refers to a subprogram
        uint32_t overload_n = 0;
        for(Declaration* decl = *bucket; decl; decl = decl->next_overload) {
            // TODO: account for default parameters
            // TODO: account for named parameters
            if(decl->kind == DECL_SUBPROGRAM
                && ((SubprogramDecl*)decl)->param_count == name_expr->arg_count) {
                add_overload(expr, overload_n);
            }
            ++overload_n;
        }

        if(name_expr->arg_count == 0) {
            // See if this name refers to a enumeration literal
            // TODO
        }
        // TODO: other possible entities that can have names
        // TODO: check for more constraints on candidates? (e.g. type of operands)
        if(!name_expr->overload_candidates) {
            error_print(expr->line_num, "No viable overloads for name '%s'", ST(name_expr->name));
            error_exit();
        }
    }
}

static
void add_overload(Expression* expr, uint32_t overload_n)
{
    assert(expr->kind == EXPR_NAME);
    // Only care about there being too many overloads if we are unable
    // to represent a candidate in the bitset, so we only check here
    if(overload_n >= MAX_OVERLOAD_CANDIDATES) {
        error_print(expr->line_num,
            "Too many overload candidates to consider for name '%s' (maximum is %u)",
            ST(expr->u.name.name), MAX_OVERLOAD_CANDIDATES);
        error_exit();
    }
    expr->u.name.overload_candidates |= (1 << overload_n);
}
