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

#define DEFAULT_TABLE_CAPACITY 64
#define TABLE_GROWTH_FACTOR 2
#define DEFAULT_STRING_POOL_CAPACITY 512 // Must be a power of 2

static char* string_pool;
static uint32_t string_pool_size;
static uint32_t string_pool_capacity;
static StringToken* tokens; // Each token is an index into string_pool (denoting the start of an interned string)
static uint32_t table_size;
static uint32_t table_capacity;

void string_pool_init(void)
{
    string_pool = calloc(DEFAULT_STRING_POOL_CAPACITY, sizeof(char));
    string_pool_capacity = DEFAULT_STRING_POOL_CAPACITY;
    string_pool_size = 1; // Reserves StringToken 0 to represent empty bucket in hash table

    tokens = calloc(DEFAULT_TABLE_CAPACITY, sizeof(*tokens));
    table_capacity = DEFAULT_TABLE_CAPACITY;
    table_size = 0;
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

static
uint32_t hash_fnv_c_str(const char* s)
{
    uint32_t hash = 2166136261;
    for(const char* c = s; *c != '\0'; ++c) {
        hash *= 16777619;
        hash ^= downcase_table[(uint8_t)(*c)];
    }
    return hash;
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

static
bool insert(StringView s, StringToken* token)
{
    uint32_t hash = hash_fnv(s);
    uint32_t i = hash % table_capacity;
    while(tokens[i]) {
        if(string_view_equal(s, string_pool + tokens[i])) {
            // String is already present in table
            *token = tokens[i];
            return false;
        }
        // Collision
        ++i; // Linear probing
        i %= table_capacity;
    }
    // New string, no collision
    tokens[i] = append(s);
    *token = tokens[i];
    ++table_size;
    return true;
}

static
void reinsert(StringToken token)
{
    uint32_t hash = hash_fnv_c_str(string_pool + token);
    uint32_t i = hash % table_capacity;
    while(tokens[i]) {
        // Collision
        ++i; // Linear probing
        i %= table_capacity;
    }
    tokens[i] = token;
}

static
void grow_table(void)
{
    uint32_t old_capacity = table_capacity;
    StringToken* old_tokens = tokens;

    table_capacity *= TABLE_GROWTH_FACTOR;
    tokens = calloc(table_capacity, sizeof(StringToken));
    for(uint32_t i = 0; i < old_capacity; ++i) {
        if(old_tokens[i]) {
            reinsert(old_tokens[i]);
        }
    }
    free(old_tokens);
}

StringToken string_pool_c_str_to_token(const char* s)
{
    StringView sv = { .value = s, .len = strlen(s) };
    return string_pool_to_token(sv);
}

StringToken string_pool_to_token(StringView s)
{
    StringToken token = 0;
    if(insert(s, &token) && table_size * 10 / table_capacity >= 7) {
        // Grow if table is at least 70% full
        grow_table();
    }
    return token;
}

const char* string_pool_to_str(StringToken token)
{
    if(token >= string_pool_size) {
        assert(false && "Invalid token");
        return "";
    }
    return string_pool + token;
}
