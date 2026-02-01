#include <assert.h>
#include "array.h"

DEFINE_ARRAY_TYPE(int)
DEFINE_ARRAY_OPS(int)

int main(void)
{
    Array_int int_array;
    array_int_init(&int_array);
    for(int i = 0; i < 1000; ++i) {
        array_int_append(&int_array, i);
    }
    assert(array_int_size(&int_array) == 1000);
    for(int i = 0; i < 1000; ++i) {
        assert(int_array.data[i] == i);
    }
    free(int_array.data);

    return 0;
}
