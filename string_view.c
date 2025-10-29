#include "string_view.h"

void string_view_create(const char* text, const Token* token, StringView* string_view)
{
    string_view->value = text + token->start;
    string_view->len = token->len;
}
