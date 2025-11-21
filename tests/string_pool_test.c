#include <assert.h>
#include <string.h>
#include <stdint.h>
#include "string_pool.h"
#include "string_view.h"

static const char* strings[] = {
    "abort",
    "declare",
    "generic",
    "of",
    "select",
    "abs",
    "delay",
    "goto",
    "or",
    "separate",
    "accept",
    "delta",
    "others",
    "subtype",
    "access",
    "digits",
    "if",
    "out",
    "all",
    "do",
    "in",
    "task",
    "and",
    "is",
    "package",
    "terminate",
    "array",
    "pragma",
    "then",
    "at",
    "else",
    "private",
    "type",
    "elsif",
    "limited",
    "procedure",
    "end",
    "loop",
    "begin",
    "entry",
    "raise",
    "use",
    "body",
    "exception",
    "range",
    "exit",
    "mod",
    "record",
    "when",
    "rem",
    "while",
    "new",
    "renames",
    "with",
    "case",
    "for",
    "not",
    "return",
    "constant",
    "function",
    "null",
    "reverse",
    "xor",
    "foo",
    "bar",
    "why is this a string",
    "another one"
};

int main(void)
{
    string_pool_init();

    for(uint32_t i = 0; i < sizeof(strings) / sizeof(strings[0]); ++i) {
        StringView s = { .value = strings[i], .len = strlen(strings[i]) };
        StringToken t1 = string_pool_to_token(s);
        StringToken t2 = string_pool_to_token(s);
        assert(t1 == t2);
        assert(strcmp(string_pool_to_str(t1), string_pool_to_str(t2)) == 0);
        assert(strcmp(string_pool_to_str(t1), strings[i]) == 0);
        assert(strcmp(string_pool_to_str(t2), strings[i]) == 0);
    }

    return 0;
}
