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

#define DEFAULT_TABLE_CAPACITY 64
#define DEFAULT_STRING_POOL_CAPACITY 512
#define TABLE_GROWTH_FACTOR 2
#define STRING_POOL_GROWTH_FACTOR 2

static const uint8_t downcase_table[256] = {
    0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,
    15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
    30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,
    45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,
    60,  61,  62,  63,  64,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106,
    107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
    122,  91,  92,  93,  94,  95,  96,  97,  98,  99, 100, 101, 102, 103, 104,
    105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
    120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134,
    135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
    150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164,
    165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
    180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194,
    195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
    210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224,
    225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
    240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254,
    255
};

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
bool string_equal(StringView a, const char* b)
{
    const char* ai = a.value;
    const char* bi = b;
    const char* a_end = a.value + a.len;
    while(ai < a_end && *bi != '\0') {
        if(downcase_table[(uint8_t)*ai] != downcase_table[(uint8_t)*bi]) {
            return false;
        }
        ++ai;
        ++bi;
    }
    return ai == a_end && *bi == '\0';
}

static
StringToken append(StringView s)
{
    if(string_pool_size + s.len + 1 > string_pool_capacity) {
        string_pool_capacity *= STRING_POOL_GROWTH_FACTOR;
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
        if(string_equal(s, string_pool + tokens[i])) {
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
