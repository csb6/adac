#ifndef ADA_LEXER_H
#define ADA_LEXER_H

#include "token.h"

const char* lexer_parse_token(const char* input_start, const char* input_end, const char* curr, Token* token);

#endif /*ADA_LEXER_H*/
