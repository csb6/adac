/*
adac - Ada compiler
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
#include "string_pool.h"
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include "string_view.h"

// We call the hash function ourselves, so no need to hash again
static inline
uint64_t hash_identity(uint64_t h)
{
    return h;
}

#define NAME token_map
#define KEY_TY uint64_t
#define VAL_TY StringToken
#define HASH_FN hash_identity
#define CMPR_FN vt_cmpr_integer
#include "verstable.h"

#define DEFAULT_STRING_POOL_CAPACITY 512 // Must be a power of 2

static char* string_pool;
static uint32_t string_pool_size;
static uint32_t string_pool_capacity;
static token_map tokens;

void string_pool_init(void)
{
    string_pool = calloc(DEFAULT_STRING_POOL_CAPACITY, sizeof(char));
    string_pool_capacity = DEFAULT_STRING_POOL_CAPACITY;
    string_pool_size = 1; // Reserves StringToken 0 to represent empty bucket in hash table

    token_map_init(&tokens);
}

static
uint32_t next_largest_pow_2(uint32_t v)
{
    v |= (v >> 1);
    v |= (v >> 2);
    v |= (v >> 4);
    v |= (v >> 8);
    v |= (v >> 16);
    return v + 1;
}

static
StringToken append(StringView s)
{
    if(string_pool_size + s.len + 1 > string_pool_capacity) {
        string_pool_capacity = next_largest_pow_2(string_pool_size + s.len + 1);
        string_pool = realloc(string_pool, string_pool_capacity);
    }
    StringToken token = string_pool_size;
    memcpy(string_pool + token, s.value, s.len);
    string_pool[string_pool_size + s.len] = '\0';
    string_pool_size += s.len + 1;
    return token;
}

// FNV-1 hash (32-bit variant)
static
uint32_t hash_fnv(StringView s)
{
    uint32_t hash = 2166136261;
    const char* end = s.value + s.len;
    for(const char* c = s.value; c != end; ++c) {
        hash *= 16777619;
        hash ^= downcase_table[(uint8_t)(*c)];
    }
    return hash;
}

StringToken string_pool_c_str_to_token(const char* s)
{
    StringView sv = { .value = s, .len = strlen(s) };
    return string_pool_to_token(sv);
}

StringToken string_pool_to_token(StringView s)
{
    uint64_t h = hash_fnv(s);
    token_map_itr it = token_map_get(&tokens, h);
    if(token_map_is_end(it)) {
        StringToken t = append(s);
        token_map_insert(&tokens, h, t);
        return t;
    }
    return it.data->val;
}

const char* string_pool_to_str(StringToken token)
{
    if(token >= string_pool_size) {
        assert(false && "Invalid token");
        return "";
    }
    return string_pool + token;
}
