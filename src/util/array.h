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
#ifndef ADA_ARRAY_H
#define ADA_ARRAY_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULT_ARRAY_CAPACITY 8

#define DEFINE_ARRAY_TYPE(T) \
    typedef struct { \
        T* data; \
        T* end; \
        T* buffer_end; \
    } Array_##T;

#define DEFINE_ARRAY_OPS(T) \
    void array_##T##_init(Array_##T* array) \
    { \
        array->data = calloc(DEFAULT_ARRAY_CAPACITY, sizeof(T)); \
        array->end = array->data; \
        array->buffer_end = array->data + DEFAULT_ARRAY_CAPACITY; \
    } \
    void array_##T##_append(Array_##T* array, T value) \
    { \
        if(array->end >= array->buffer_end) { \
            uint32_t old_size = array->end - array->data; \
            uint32_t new_size = old_size * 2; \
            array->data = realloc(array->data, new_size * sizeof(T)); \
            array->buffer_end = array->data + new_size; \
            array->end = array->data + old_size; \
        } \
        *array->end++ = value; \
    } \
    uint32_t array_##T##_size(const Array_##T* array) \
    { \
       return array->end - array->data; \
    }

#endif /* ADA_ARRAY_H */
