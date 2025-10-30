#ifndef ADA_STRING_VIEW_H
#define ADA_STRING_VIEW_H

#include <stdint.h>

typedef struct {
    const char* value;
    uint32_t len;
} StringView;

#endif /* ADA_STRING_VIEW_H */
