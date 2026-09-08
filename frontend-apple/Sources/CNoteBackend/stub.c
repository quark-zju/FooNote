#include "notebackend.h"

void notebackend_stack_push_str_safe(const char *value, size_t size) {
    static const uint8_t empty = 0;
    const uint8_t *bytes = size == 0 ? &empty : (const uint8_t *)value;
    notebackend_stack_push_str(bytes, size);
}
