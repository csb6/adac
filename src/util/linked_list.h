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
#ifndef ADA_LINKED_LIST_H
#define ADA_LINKED_LIST_H

#define DEFINE_LINKED_LIST_TYPE(T) \
    typedef struct { \
        T* first; \
        T* last; \
    } T##List;

#define DEFINE_LINKED_LIST_OPS(T) \
    void T##List_append(T##List* list, T* item) \
    { \
        if(list->last) { \
            list->last->next = item; \
        } else { \
            list->first = item; \
        } \
        list->last = item; \
    } \
    void T##List_splice(T##List* a, T##List* b) \
    { \
        if(a->last) { \
            a->last->next = b->first; \
        } else { \
            a->first = b->first; \
        } \
        a->last = b->last; \
    }

#endif /* ADA_LINKED_LIST_H */
