#ifndef ADA_STRING_VIEW_H
#define ADA_STRING_VIEW_H

#include <stdint.h>
#include "token.h"

typedef struct {
    const char* value;
    uint32_t len;
} StringView;

void string_view_create(const char* text, const Token* token, StringView* string_view);

#endif /* ADA_STRING_VIEW_H */
