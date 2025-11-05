#ifndef ADA_STRING_VIEW_H
#define ADA_STRING_VIEW_H

#include <stdint.h>

typedef struct {
    const char* value;
    uint32_t len;
} StringView;

// Enables succinct printing of a StringView using printf("%.*s")
#define SV(sv) (sv).len, (sv).value

#endif /* ADA_STRING_VIEW_H */
