#include <assert.h>
#include "array.h"

DEFINE_ARRAY_TYPE(int)
DEFINE_ARRAY_OPS(int)

int main(void)
{
    intArray int_array;
    intArray_init(&int_array);
    for(int i = 0; i < 1000; ++i) {
        intArray_append(&int_array, i);
    }
    assert(intArray_size(&int_array) == 1000);
    for(int i = 0; i < 1000; ++i) {
        assert(int_array.data[i] == i);
    }
    free(int_array.data);

    return 0;
}
